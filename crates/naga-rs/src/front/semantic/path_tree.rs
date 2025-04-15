//! # Visibility
//!
//! Some [`PathValue`]s contain `vis_scope` field. That field indicates a node
//! index of a module that the value is visible. See an example below.
//! ```ignore
//! mod a {
//!     mod b {
//!         struct A;            // vis_scope: Node index to 'mod b'
//!         pub(super) struct B; // vis_scope: Node index to 'mod a'
//!     }
//! }
//! ```
//! We use node index instead of path id becuase a node can have only one
//! module, plus, it makes visibility check easy.

use crate::{
    Result,
    front::{
        syntax::{
            common::{IdentifySyn, SynId},
            file::SmFile,
        },
        util,
    },
};
use smallvec::SmallVec;
use std::{
    borrow::Cow,
    cell::Cell,
    cmp,
    collections::HashSet,
    fmt,
    iter::{self, FusedIterator},
    mem,
    ops::{self, Deref, DerefMut, Index, IndexMut},
    path::{Path as StdPath, PathBuf},
    result::Result as StdResult,
};

pub struct PathTree {
    nodes: Vec<PathNode>,
}

impl PathTree {
    pub const ROOT: NodeIndex = NodeIndex(0);

    pub fn new() -> Self {
        // Starts with the root node.
        let nodes = vec![PathNode {
            leaf: SmallVec::new(),
            parent: Self::ROOT,
            children: Vec::new(),
        }];
        Self { nodes }
    }

    pub fn iter(&self) -> PathIter<'_> {
        PathIter::new(self)
    }

    pub fn values(&self) -> PathValues<'_> {
        PathValues::new(self)
    }

    pub fn get_node(&self, index: NodeIndex) -> &PathNode {
        &self.nodes[index]
    }

    pub fn get_mut_node(&mut self, index: NodeIndex) -> &mut PathNode {
        &mut self.nodes[index]
    }

    pub fn get_value(&self, id: PathId) -> &PathValue {
        let node = self.get_node(id.ni);
        &node.leaf[id.vi]
    }

    pub fn get_mut_value(&mut self, id: PathId) -> &mut PathValue {
        let node = self.get_mut_node(id.ni);
        &mut node.leaf[id.vi]
    }

    pub fn get_values(&self, index: NodeIndex) -> &[PathValue] {
        let node = self.get_node(index);
        &node.leaf
    }

    pub fn get_glob_values(&self, index: NodeIndex) -> &[PathValue] {
        let node = self.get_node(index);
        if let Some(glob) = node.children.iter().find(|(k, _)| k == "*") {
            let glob_node = self.get_node(glob.1);
            &glob_node.leaf
        } else {
            &[]
        }
    }

    pub fn search_values<'a, I>(&self, key: I) -> &[PathValue]
    where
        I: Iterator<Item = &'a str>,
    {
        let Some(ni) = self.search_from(Self::ROOT, key) else {
            return &[];
        };
        self.nodes[ni].leaf.as_ref()
    }

    pub fn search_mut_values<'a, I>(&mut self, key: I) -> &mut [PathValue]
    where
        I: Iterator<Item = &'a str>,
    {
        let Some(ni) = self.search_from(Self::ROOT, key) else {
            return &mut [];
        };
        self.nodes[ni].leaf.as_mut()
    }

    pub fn search<'a, I>(&self, key: I) -> Option<NodeIndex>
    where
        I: Iterator<Item = &'a str>,
    {
        self.search_from(Self::ROOT, key)
    }

    pub fn search_from<'a, I>(&self, from: NodeIndex, key: I) -> Option<NodeIndex>
    where
        I: Iterator<Item = &'a str>,
    {
        let mut cur = from;
        for seg in key {
            if let Some(next) = self.nodes[cur].children.iter().find(|(k, _)| k == seg) {
                cur = next.1;
            } else {
                return None;
            }
        }
        Some(cur)
    }

    /// Ok: node index to the target and advance count
    /// Err: node index to a node in the middle of search and advance count
    pub fn search_best_from<'a, I>(
        &self,
        from: NodeIndex,
        key: I,
    ) -> StdResult<(NodeIndex, usize), (NodeIndex, usize)>
    where
        I: Iterator<Item = &'a str>,
    {
        let mut cur = from;
        let mut advance = 0;
        for seg in key {
            if let Some(next) = self.nodes[cur].children.iter().find(|(k, _)| k == seg) {
                cur = next.1;
                advance += 1;
            } else {
                return Err((cur, advance));
            }
        }
        Ok((cur, advance))
    }

    pub fn is_in_block<'a, I>(&self, key: I) -> bool
    where
        I: Iterator<Item = &'a str>,
    {
        let value = self.search_nearest_value(key, |value| {
            matches!(
                value,
                PathValue::Block(_) | PathValue::Mod(_) | PathValue::ModRaw(_)
            )
        });

        matches!(value, Some(PathValue::Block(_)))
    }

    pub fn search_nearest_value<'a, I, F>(&self, key: I, mut cmp: F) -> Option<&PathValue>
    where
        I: Iterator<Item = &'a str>,
        F: FnMut(&PathValue) -> bool,
    {
        let mut cur = Self::ROOT;
        let mut nearest = None;
        for seg in key {
            for value in &self.nodes[cur].leaf {
                if cmp(value) {
                    nearest = Some(value);
                }
            }
            if let Some(next) = self.nodes[cur].children.iter().find(|(k, _)| k == seg) {
                cur = next.1;
            } else {
                break;
            }
        }
        for value in &self.nodes[cur].leaf {
            if cmp(value) {
                nearest = Some(value);
            }
        }
        nearest
    }

    /// Visits all values matched by the given condition, `from` and `ext`.
    /// Traverse also can jump over resolved 'use'. then calls the given
    /// function on the matched value.
    ///
    /// If the given function returns Some value, then it is returned and
    /// traverse stops.
    ///
    /// # Caution
    ///
    /// This method ignores unresolved 'use'.
    pub fn traverse_from<'a, I, F, R>(&self, mut from: NodeIndex, ext: I, mut f: F) -> Option<R>
    where
        I: Iterator<Item = &'a str> + Clone,
        F: FnMut(PathId) -> Option<R>,
    {
        // Normalizes the given path.
        let mut ext_off = 0;
        for seg in ext.clone() {
            if seg == "crate" {
                from = self.search(iter::once("crate")).unwrap();
                ext_off += 1;
            } else if seg == "super" {
                from = self.parent_module(from).ni;
                ext_off += 1;
            } else {
                break;
            }
        }

        let mut stack = Vec::<(NodeIndex, usize)>::new();
        let mut visited = HashSet::<NodeIndex>::new();
        stack.push((from, ext_off));
        while let Some((from, ext_off)) = stack.pop() {
            match self.search_best_from(from, ext.clone().skip(ext_off)) {
                Ok((dst, advance)) => {
                    if !visited.insert(dst) {
                        continue;
                    }

                    // Tries to jump over through 'use'. Note that this method
                    // assumes all `PathValue`s are resolved.
                    for value in self.get_values(dst).iter().chain(self.get_glob_values(dst)) {
                        if let PathValue::Use(PathUse { dst, .. }) = value {
                            stack.push((*dst, ext_off + advance));
                        }
                    }

                    for vi in 0..self.get_values(dst).len() {
                        let ret = f(PathId::new(dst, vi));
                        if ret.is_some() {
                            return ret;
                        }
                    }
                }
                Err((mid, advance)) => {
                    if !visited.insert(mid) {
                        continue;
                    }

                    // Tries to jump over through 'use'. Note that this method
                    // assumes all `PathValue`s are resolved.
                    for value in self.get_values(mid).iter().chain(self.get_glob_values(mid)) {
                        if let PathValue::Use(PathUse { dst, .. }) = value {
                            stack.push((*dst, ext_off + advance));
                        }
                    }

                    // Gives extern matching a try.
                    stack.push((Self::ROOT, ext_off + advance));
                }
            }
        }
        None
    }

    /// Returns first matched value's id.
    ///
    /// # Caution
    ///
    /// Do not call this method until all 'use's are resolved.
    pub fn reach<'a, I>(&self, from: NodeIndex, ext: I) -> PathId
    where
        I: Iterator<Item = &'a str> + Clone,
    {
        self.traverse_from(from, ext.clone(), |pid| {
            let v = self.get_value(pid);
            if let PathValue::Use(_) = v {
                None
            } else {
                Some(pid)
            }
        })
        .unwrap_or_else(|| {
            panic!(
                "couldn't find path node: `{}` + `{}`",
                self.named_path(from),
                ext.collect::<String>()
            )
        })
    }

    pub fn named_path(&self, index: NodeIndex) -> NamedPath {
        let mut cur = index;
        let mut segments = Vec::new();
        while cur != Self::ROOT {
            let c = self.get_node(cur);
            let p = self.get_node(c.parent);
            let (k, _) = p.children.iter().find(|(_, h)| *h == cur).unwrap();
            segments.push(k.0.clone());
            cur = c.parent;
        }
        segments.reverse();
        segments.join("::").into()
    }

    pub fn is_descendant(&self, descendant: NodeIndex, ancestor: NodeIndex) -> bool {
        let mut cur = descendant;
        while cur != ancestor {
            if cur == Self::ROOT {
                return false;
            }
            cur = self.get_node(cur).parent;
        }
        true
    }

    /// Returns id of the right above parent module.
    pub fn parent_module(&self, index: NodeIndex) -> PathId {
        let parent = self.get_node(index).parent;
        self.nearest_module(parent)
    }

    /// Returns the nearest module's id.
    ///
    /// If the node at the given index contains module, then returns its id.
    /// Note that root node is also considered a module.
    pub fn nearest_module(&self, index: NodeIndex) -> PathId {
        let mut cur = index;
        while cur != PathTree::ROOT {
            let node = self.get_node(cur);
            for (vi, value) in node.leaf.iter().enumerate() {
                if matches!(value, PathValue::Mod { .. } | PathValue::ModRaw { .. }) {
                    return PathId::new(cur, vi);
                }
            }
            cur = node.parent;
        }

        PathId::new(Self::ROOT, 0)
    }

    pub fn append_value<'a, I>(&mut self, from: NodeIndex, key: I, value: PathValue) -> PathId
    where
        I: Iterator<Item = &'a str>,
    {
        self.insert_node_then(from, key, |ni, values| {
            values.push(value);
            PathId::new(ni, values.len() - 1)
        })
    }

    pub fn insert_node_then<'a, I, F, R>(&mut self, from: NodeIndex, key: I, then: F) -> R
    where
        I: Iterator<Item = &'a str>,
        F: FnOnce(NodeIndex, &mut SmallVec<[PathValue; 1]>) -> R,
    {
        let mut cur = from;
        for seg in key {
            if let Some(next) = self.nodes[cur].children.iter().find(|(k, _)| k == seg) {
                cur = next.1;
            } else {
                let ni = self.new_node(cur);
                let seg = seg.to_owned().into();
                self.nodes[cur].children.push((seg, ni));
                cur = ni;
            }
        }
        then(cur, &mut self.nodes[cur].leaf)
    }

    fn new_node(&mut self, parent: NodeIndex) -> NodeIndex {
        self.nodes.push(PathNode {
            leaf: SmallVec::new(),
            parent,
            children: Vec::new(),
        });
        NodeIndex(self.nodes.len() - 1)
    }
}

impl Index<PathId> for PathTree {
    type Output = PathValue;

    fn index(&self, id: PathId) -> &Self::Output {
        self.get_value(id)
    }
}

impl IndexMut<PathId> for PathTree {
    fn index_mut(&mut self, id: PathId) -> &mut Self::Output {
        self.get_mut_value(id)
    }
}

impl Index<NodeIndex> for PathTree {
    type Output = PathNode;

    fn index(&self, index: NodeIndex) -> &Self::Output {
        self.get_node(index)
    }
}

impl IndexMut<NodeIndex> for PathTree {
    fn index_mut(&mut self, index: NodeIndex) -> &mut Self::Output {
        self.get_mut_node(index)
    }
}

impl fmt::Debug for PathTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        thread_local! {
            static NEED_SEP: Cell<bool> = Cell::new(false);
        }

        fn dfs(
            this: &PathTree,
            ni: NodeIndex,
            key: &mut String,
            f: &mut fmt::Formatter<'_>,
        ) -> fmt::Result {
            for (seg, child) in &this[ni].children {
                let org_len = key.len();
                util::push_colon_path(key, seg);
                for value in &this[*child].leaf {
                    if NEED_SEP.get() {
                        write!(f, "\n")?;
                    }
                    write!(f, "[{child}] {key} -> {value:?}")?;
                    NEED_SEP.set(true);
                }
                dfs(this, *child, key, f)?;
                key.truncate(org_len);
            }
            Ok(())
        }

        let mut key = String::new();
        let res = dfs(self, Self::ROOT, &mut key, f);
        NEED_SEP.set(false);
        res
    }
}

pub struct PathIter<'a> {
    nodes: &'a [PathNode],
    ni: NodeIndex,
    vi: ValueIndex,
}

impl<'a> PathIter<'a> {
    fn new(tree: &'a PathTree) -> Self {
        Self {
            nodes: &tree.nodes,
            ni: NodeIndex(0),
            vi: ValueIndex(0),
        }
    }
}

impl<'a> Iterator for PathIter<'a> {
    type Item = (PathId, &'a PathValue);

    fn next(&mut self) -> Option<Self::Item> {
        while self.ni < self.nodes.len() {
            let node = &self.nodes[self.ni];
            while self.vi < node.leaf.len() {
                if let Some(value) = node.leaf.get(self.vi.0) {
                    let id = PathId {
                        ni: self.ni,
                        vi: self.vi,
                    };
                    self.vi += 1;
                    return Some((id, value));
                }
                self.vi += 1;
            }
            self.ni += 1;
            self.vi = ValueIndex(0);
        }
        None
    }
}

impl<'a> FusedIterator for PathIter<'a> {}

pub struct PathValues<'a> {
    nodes: &'a [PathNode],
    ni: usize,
    vi: usize,
}

impl<'a> PathValues<'a> {
    fn new(tree: &'a PathTree) -> Self {
        Self {
            nodes: &tree.nodes,
            ni: 0,
            vi: 0,
        }
    }
}

impl<'a> Iterator for PathValues<'a> {
    type Item = &'a PathValue;

    fn next(&mut self) -> Option<Self::Item> {
        while self.ni < self.nodes.len() {
            let node = &self.nodes[self.ni];
            while self.vi < node.leaf.len() {
                let value = node.leaf.get(self.vi);
                self.vi += 1;
                if value.is_some() {
                    return value;
                }
            }
            self.ni += 1;
            self.vi = 0;
        }
        None
    }
}

impl FusedIterator for PathValues<'_> {}

#[derive(Debug)]
pub struct PathNode {
    leaf: SmallVec<[PathValue; 1]>,
    parent: NodeIndex,
    children: Vec<(NamedPath, NodeIndex)>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct PathId {
    pub ni: NodeIndex,
    pub vi: ValueIndex,
}

impl PathId {
    pub fn new<I, J>(ni: I, vi: J) -> Self
    where
        I: Into<NodeIndex>,
        J: Into<ValueIndex>,
    {
        Self {
            ni: ni.into(),
            vi: vi.into(),
        }
    }
}

impl fmt::Display for PathId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ni)
    }
}

impl fmt::Debug for PathId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ni)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeIndex(pub usize);

impl NodeIndex {
    pub const fn to_path_id(self, vi: usize) -> PathId {
        PathId {
            ni: self,
            vi: ValueIndex(vi),
        }
    }
}

impl From<usize> for NodeIndex {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl fmt::Display for NodeIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Debug for NodeIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PartialEq<usize> for NodeIndex {
    fn eq(&self, other: &usize) -> bool {
        self.0.eq(other)
    }
}

impl PartialOrd<usize> for NodeIndex {
    fn partial_cmp(&self, other: &usize) -> Option<cmp::Ordering> {
        self.0.partial_cmp(other)
    }
}

impl ops::Add<usize> for NodeIndex {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        Self(self.0 + rhs)
    }
}

impl ops::AddAssign<usize> for NodeIndex {
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs;
    }
}

impl Index<NodeIndex> for [PathNode] {
    type Output = PathNode;

    fn index(&self, index: NodeIndex) -> &Self::Output {
        &self[index.0]
    }
}

impl IndexMut<NodeIndex> for [PathNode] {
    fn index_mut(&mut self, index: NodeIndex) -> &mut Self::Output {
        &mut self[index.0]
    }
}

impl Index<NodeIndex> for Vec<PathNode> {
    type Output = PathNode;

    fn index(&self, index: NodeIndex) -> &Self::Output {
        &self.as_slice()[index]
    }
}

impl IndexMut<NodeIndex> for Vec<PathNode> {
    fn index_mut(&mut self, index: NodeIndex) -> &mut Self::Output {
        &mut self.as_mut_slice()[index]
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueIndex(pub usize);

impl From<usize> for ValueIndex {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl fmt::Display for ValueIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Debug for ValueIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PartialEq<usize> for ValueIndex {
    fn eq(&self, other: &usize) -> bool {
        self.0.eq(other)
    }
}

impl PartialOrd<usize> for ValueIndex {
    fn partial_cmp(&self, other: &usize) -> Option<cmp::Ordering> {
        self.0.partial_cmp(other)
    }
}

impl ops::Add<usize> for ValueIndex {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        Self(self.0 + rhs)
    }
}

impl ops::AddAssign<usize> for ValueIndex {
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs;
    }
}

impl Index<ValueIndex> for [PathValue] {
    type Output = PathValue;

    fn index(&self, index: ValueIndex) -> &Self::Output {
        &self[index.0]
    }
}

impl IndexMut<ValueIndex> for [PathValue] {
    fn index_mut(&mut self, index: ValueIndex) -> &mut Self::Output {
        &mut self[index.0]
    }
}

impl Index<ValueIndex> for SmallVec<[PathValue; 1]> {
    type Output = PathValue;

    fn index(&self, index: ValueIndex) -> &Self::Output {
        &self[index.0]
    }
}

impl IndexMut<ValueIndex> for SmallVec<[PathValue; 1]> {
    fn index_mut(&mut self, index: ValueIndex) -> &mut Self::Output {
        &mut self[index.0]
    }
}

pub enum PathValue {
    Block(PathBlock),
    Const(PathConst),
    ConstRaw(PathConstRaw),
    Extern(PathExtern),
    Field(PathField),
    FieldRaw(PathFieldRaw),
    Fn(PathFn),
    FnRaw(PathFnRaw),
    Mod(PathMod),
    ModRaw(PathModRaw),
    Struct(PathStruct),
    StructRaw(PathStructRaw),
    TypeAlias(PathTypeAlias),
    TypeAliasRaw(PathTypeAliasRaw),
    Use(PathUse),
    UseRaw(PathUseRaw),
}

impl PathValue {
    pub fn as_const(&self) -> &PathConst {
        let Self::Const(v) = self else {
            unreachable!("invalid variant at `PathValue::as_const`")
        };
        v
    }

    pub fn as_const_raw(&self) -> &PathConstRaw {
        let Self::ConstRaw(v) = self else {
            unreachable!("invalid variant at `PathValue::as_const_raw`");
        };
        v
    }

    pub fn as_field_raw(&self) -> &PathFieldRaw {
        let Self::FieldRaw(v) = self else {
            unreachable!("invalid variant at `PathValue::as_field_raw`")
        };
        v
    }

    pub fn as_fn(&self) -> &PathFn {
        let Self::Fn(v) = self else {
            unreachable!("invalid variant at `PathValue::as_fn`")
        };
        v
    }

    pub fn as_fn_raw(&self) -> &PathFnRaw {
        let Self::FnRaw(v) = self else {
            unreachable!("invalid variant at `PathValue::as_fn_raw`")
        };
        v
    }

    pub fn as_mod(&self) -> &PathMod {
        let Self::Mod(v) = self else {
            unreachable!("invalid variant at `PathValue::as_mod`")
        };
        v
    }

    pub fn as_mod_mut(&mut self) -> &mut PathMod {
        let Self::Mod(v) = self else {
            unreachable!("invalid variant at `PathValue::as_mod_mut`")
        };
        v
    }

    pub fn as_mod_raw(&self) -> &PathModRaw {
        let Self::ModRaw(v) = self else {
            unreachable!("invalid variant at `PathValue::as_mod_raw`")
        };
        v
    }

    pub fn as_struct(&self) -> &PathStruct {
        let Self::Struct(v) = self else {
            unreachable!("invalid variant at `PathValue::as_struct`")
        };
        v
    }

    pub fn as_struct_raw(&self) -> &PathStructRaw {
        let Self::StructRaw(v) = self else {
            unreachable!("invalid variant at `PathValue::as_struct_raw`")
        };
        v
    }

    pub fn as_type_alias_raw(&self) -> &PathTypeAliasRaw {
        let Self::TypeAliasRaw(v) = self else {
            unreachable!("invalid variant at `PathValue::as_as_type_alias_raw`")
        };
        v
    }

    pub fn as_use_raw(&self) -> &PathUseRaw {
        let Self::UseRaw(v) = self else {
            unreachable!("invalid variant at `PathValue::as_use_raw`")
        };
        v
    }

    pub fn resolve_const(&mut self, vis_scope: NodeIndex, elem_ty: PathId) {
        let Self::ConstRaw(v) = self else {
            unreachable!("invalid variant at `PathValue::resolve_const`")
        };
        *self = PathValue::Const(PathConst {
            syn: v.syn,
            vis_scope,
            elem_ty,
        });
    }

    pub fn resolve_fn(&mut self, vis_scope: NodeIndex) {
        let Self::FnRaw(v) = self else {
            unreachable!("invalid variant at `PathValue::resolve_fn`")
        };
        *self = PathValue::Fn(PathFn {
            syn: v.syn,
            vis_scope,
        })
    }

    pub fn resolve_mod(&mut self, vis_scope: NodeIndex) {
        let Self::ModRaw(v) = self else {
            unreachable!("invalid variant at `PathValue::resolve_mod`")
        };
        *self = PathValue::Mod(PathMod {
            syn: v.syn,
            syn_file: v.syn_file,
            vis_scope,
            fpath: mem::take(&mut v.fpath),
            mod_rs: v.mod_rs,
        });
    }

    pub fn resolve_struct(&mut self, vis_scope: NodeIndex) {
        let Self::StructRaw(v) = self else {
            unreachable!("invalid variant at `PathValue::resolve_struct`")
        };
        *self = PathValue::Struct(PathStruct {
            syn: v.syn,
            vis_scope,
        });
    }

    pub fn resolve_field(&mut self, elem_ty: PathId) {
        let Self::FieldRaw(v) = self else {
            unreachable!("invalid variant at `PathValue::resolve_field`")
        };
        *self = PathValue::Field(PathField {
            syn: v.syn,
            elem_ty,
        });
    }

    pub fn resolve_type_alias(&mut self, vis_scope: NodeIndex) {
        let Self::TypeAliasRaw(v) = self else {
            unreachable!("invalid variant at `PathValue::resolve_type_alias`")
        };
        *self = PathValue::TypeAlias(PathTypeAlias {
            syn: v.syn,
            vis_scope,
        })
    }

    pub fn resolve_use(&mut self, vis_scope: NodeIndex, dst: NodeIndex) {
        let Self::UseRaw(v) = self else {
            unreachable!("invalid variant at `PathValue::resolve_use`")
        };
        *self = PathValue::Use(PathUse {
            syn: v.syn,
            detail: v.detail,
            vis_scope,
            dst,
        });
    }

    pub fn syn_id(&self) -> SynId {
        match self {
            Self::Block(v) => v.syn_id(),
            Self::Const(v) => v.syn_id(),
            Self::ConstRaw(v) => v.syn_id(),
            Self::Extern(v) => v.syn_id(),
            Self::Field(v) => v.syn_id(),
            Self::FieldRaw(v) => v.syn_id(),
            Self::Fn(v) => v.syn_id(),
            Self::FnRaw(v) => v.syn_id(),
            Self::Mod(v) => v.syn_id(),
            Self::ModRaw(v) => v.syn_id(),
            Self::Struct(v) => v.syn_id(),
            Self::StructRaw(v) => v.syn_id(),
            Self::TypeAlias(v) => v.syn_id(),
            Self::TypeAliasRaw(v) => v.syn_id(),
            Self::Use(v) => v.syn_id(),
            Self::UseRaw(v) => v.syn_id(),
        }
    }

    pub fn vis_scope(&self) -> Option<NodeIndex> {
        match self {
            Self::Block(v) => {
                panic!(
                    "{}: not available visibility for block",
                    v.syn_id().content()
                );
            }
            Self::Const(v) => Some(v.vis_scope),
            Self::ConstRaw(_) => None,
            Self::Extern(v) => {
                // TODO
                panic!(
                    "{}: not available visibility for extern",
                    v.syn_id().content()
                );
            }
            Self::Field(v) => {
                // TODO
                panic!(
                    "{}: not available visibility for field",
                    v.syn_id().content()
                );
            }
            Self::FieldRaw(v) => {
                // TODO
                panic!(
                    "{}: not available visibility for field",
                    v.syn_id().content()
                );
            }
            Self::Fn(v) => Some(v.vis_scope),
            Self::FnRaw(_) => None,
            Self::Mod(v) => Some(v.vis_scope),
            Self::ModRaw(_) => None,
            Self::Struct(v) => Some(v.vis_scope),
            Self::StructRaw(_) => None,
            Self::TypeAlias(v) => Some(v.vis_scope),
            Self::TypeAliasRaw(_) => None,
            Self::Use(v) => Some(v.vis_scope),
            Self::UseRaw(_) => None,
        }
    }

    pub fn is_type(&self) -> bool {
        matches!(self, Self::Extern(_) | Self::Struct(_) | Self::StructRaw(_))
    }
}

impl fmt::Debug for PathValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Block(v) => v.fmt(f),
            Self::Const(v) => v.fmt(f),
            Self::ConstRaw(v) => v.fmt(f),
            Self::Extern(v) => v.fmt(f),
            Self::Field(v) => v.fmt(f),
            Self::FieldRaw(v) => v.fmt(f),
            Self::Fn(v) => v.fmt(f),
            Self::FnRaw(v) => v.fmt(f),
            Self::Mod(v) => v.fmt(f),
            Self::ModRaw(v) => v.fmt(f),
            Self::Struct(v) => v.fmt(f),
            Self::StructRaw(v) => v.fmt(f),
            Self::TypeAlias(v) => v.fmt(f),
            Self::TypeAliasRaw(v) => v.fmt(f),
            Self::Use(v) => v.fmt(f),
            Self::UseRaw(v) => v.fmt(f),
        }
    }
}

#[derive(Debug)]
pub struct PathBlock {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::Block,
}

impl PathBlock {
    pub fn as_syn<'o>(&self) -> &'o syn::Block {
        unsafe { self.syn.as_ref().expect("PathBlock contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }
}

#[derive(Debug)]
pub struct PathConst {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemConst,
    pub vis_scope: NodeIndex,
    pub elem_ty: PathId,
}

impl PathConst {
    pub fn as_syn<'o>(&self) -> &'o syn::ItemConst {
        unsafe { self.syn.as_ref().expect("PathConst contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }
}

#[derive(Debug)]
pub struct PathConstRaw {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemConst,
}

impl PathConstRaw {
    pub fn as_syn<'o>(&self) -> &'o syn::ItemConst {
        unsafe { self.syn.as_ref().expect("PathConstRaw contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }

    pub fn visibility(&self) -> PathVis {
        PathVis::new(&self.as_syn().vis)
    }
}

#[derive(Debug)]
pub struct PathExtern {
    /// Read-only pointer to a syntax tree node that never changes.
    ///
    /// Note, however, that this identifier doesn't mean unique syntax node that
    /// corresponds to this extern. Many syntax nodes can be related with this
    /// extern. For example, multiple syntax nodes of "i32" will share this
    /// extern.
    ///
    /// Also, extern node would be a part of a non-extern node like below.
    /// - [`syn::ItemConst::ty`]
    /// - [`syn::Field::ty`]
    /// - [`syn::ItemUse::tree`]
    /// - and so on.
    pub sid: SynId,
}

impl PathExtern {
    pub const fn syn_id(&self) -> SynId {
        self.sid
    }
}

#[derive(Debug)]
pub struct PathField {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::Field,
    pub elem_ty: PathId,
}

impl PathField {
    pub fn as_syn<'o>(&self) -> &'o syn::Field {
        unsafe { self.syn.as_ref().expect("PathField contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }
}

#[derive(Debug)]
pub struct PathFieldRaw {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::Field,
}

impl PathFieldRaw {
    pub fn as_syn<'o>(&self) -> &'o syn::Field {
        unsafe { self.syn.as_ref().expect("PathFieldRaw contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }
}

#[derive(Debug)]
pub struct PathFn {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemFn,

    /// Refer to the [module documentation](self).
    pub vis_scope: NodeIndex,
}

impl PathFn {
    pub fn as_syn<'o>(&self) -> &'o syn::ItemFn {
        unsafe { self.syn.as_ref().expect("PathFn contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }
}

#[derive(Debug)]
pub struct PathFnRaw {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemFn,
}

impl PathFnRaw {
    pub fn as_syn<'o>(&self) -> &'o syn::ItemFn {
        unsafe { self.syn.as_ref().expect("PathFnRaw contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }

    pub fn visibility(&self) -> PathVis {
        PathVis::new(&self.as_syn().vis)
    }
}

#[derive(Debug)]
pub struct PathMod {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemMod,

    pub syn_file: Option<*const SmFile>,

    /// Refer to the [module documentation](self).
    pub vis_scope: NodeIndex,

    /// Absolute file path that represent this module.
    ///
    /// ```ignore
    /// // /a.rs
    /// mod b {           // fpath: /a/b
    ///     mod c;        // fpath: /a/b/c.rs or /a/b/c/mod.rs
    /// }
    /// #[path = "d.rs"]
    /// mod d;            // fpath: /d.rs
    /// mod e;            // fpath: /a/e.rs or /a/e/mod.rs
    /// ```
    pub fpath: PathBuf,

    /// - True if the file is one of "mod.rs", "main.rs", or "lib.rs".
    /// - True if the file is determined by "path" attribute, plus the module is
    ///   inline (e.g. #[path = "a.rs"] mod foo;)
    /// - False otherwise.
    pub mod_rs: bool,
}

impl PathMod {
    pub fn as_syn<'o>(&self) -> &'o syn::ItemMod {
        unsafe { self.syn.as_ref().expect("PathMod contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }

    pub fn is_inline(&self) -> bool {
        if !self.syn.is_null() {
            self.as_syn().content.is_some()
        } else {
            false
        }
    }
}

#[derive(Debug)]
pub struct PathModRaw {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemMod,
    pub syn_file: Option<*const SmFile>,
    pub fpath: PathBuf,
    pub mod_rs: bool,
}

impl PathModRaw {
    pub fn as_syn<'o>(&self) -> &'o syn::ItemMod {
        unsafe { self.syn.as_ref().expect("PathModRaw contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }

    pub fn is_inline(&self) -> bool {
        if !self.syn.is_null() {
            self.as_syn().content.is_some()
        } else {
            false
        }
    }

    pub fn visibility(&self) -> PathVis {
        if !self.syn.is_null() {
            PathVis::new(&self.as_syn().vis)
        } else {
            PathVis::Private
        }
    }
}

#[derive(Debug)]
pub struct PathStruct {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemStruct,

    /// Refer to the [module documentation](self).
    pub vis_scope: NodeIndex,
}

impl PathStruct {
    pub fn as_syn<'o>(&self) -> &'o syn::ItemStruct {
        unsafe { self.syn.as_ref().expect("PathStruct contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }
}

#[derive(Debug)]
pub struct PathStructRaw {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemStruct,
}

impl PathStructRaw {
    pub fn as_syn<'o>(&self) -> &'o syn::ItemStruct {
        unsafe { self.syn.as_ref().expect("PathStructRaw contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }

    pub fn visibility(&self) -> PathVis {
        PathVis::new(&self.as_syn().vis)
    }
}

#[derive(Debug)]
pub struct PathTypeAlias {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemType,

    /// Refer to the [module documentation](self).
    pub vis_scope: NodeIndex,
}

impl PathTypeAlias {
    pub fn as_syn<'o>(&self) -> &'o syn::ItemType {
        unsafe { self.syn.as_ref().expect("PathTypeAlias contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }
}

#[derive(Debug)]
pub struct PathTypeAliasRaw {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemType,
}

impl PathTypeAliasRaw {
    pub fn as_syn<'o>(&self) -> &'o syn::ItemType {
        unsafe {
            self.syn
                .as_ref()
                .expect("PathTypeAliasRaw contains nullptr")
        }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }

    pub fn visibility(&self) -> PathVis {
        PathVis::new(&self.as_syn().vis)
    }
}

#[derive(Debug)]
pub struct PathUse {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemUse,

    /// Pointer to a specific item in the [`ItemUse::tree`].
    ///
    /// For instance, if [`Self::syn`] points to 'use a::{b, c, *}', this field
    /// points to 'b', 'c', or '*'.
    pub detail: SynId,

    /// Refer to the [module documentation](self).
    pub vis_scope: NodeIndex,
    pub dst: NodeIndex,
}

impl PathUse {
    pub fn as_syn<'o>(&self) -> &'o syn::ItemUse {
        unsafe { self.syn.as_ref().expect("PathUse contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }
}

#[derive(Debug)]
pub struct PathUseRaw {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemUse,

    /// Pointer to a specific item in the [`ItemUse::tree`].
    ///
    /// For instance, if [`Self::syn`] points to 'use a::{b, c, *}', this field
    /// points to 'b', 'c', or '*'.
    pub detail: SynId,

    pub npath: NamedPath,
}

impl PathUseRaw {
    pub fn as_syn<'o>(&self) -> &'o syn::ItemUse {
        unsafe { self.syn.as_ref().expect("PathUseRaw contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }

    pub fn visibility(&self) -> PathVis {
        PathVis::new(&self.as_syn().vis)
    }
}

// === NamedPath ===

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NamedPath(pub String);

impl NamedPath {
    pub const EMPTY: Self = Self(String::new());

    pub fn new<'a, T>(s: T) -> Self
    where
        T: Into<Cow<'a, str>>,
    {
        let s: Cow<'_, str> = s.into();
        match s {
            Cow::Owned(s) => Self(s),
            Cow::Borrowed(s) => Self(s.to_owned()),
        }
    }

    pub fn from_relative_file_path(fpath: &StdPath) -> Result<Self> {
        let mut this = Self::EMPTY;
        let mut need_colon = false;
        for segment in fpath {
            if need_colon {
                this.push_str("::");
            } else {
                need_colon = true;
            }
            this.push_str(util::os_str_to_str(segment)?);
        }
        Ok(this)
    }

    pub fn from_relative_file_path_str<P>(s: &str) -> Self {
        let s = s.trim_end_matches(".rs");
        let mut buf = String::with_capacity(s.len() * 2 + "crate::".len());
        buf.push_str("crate::");
        for c in s.chars() {
            if c == '/' {
                buf.push_str("::");
            } else {
                buf.push(c);
            }
        }
        Self(buf)
    }

    pub fn from_path<P>(path: P) -> Result<Self>
    where
        P: AsRef<StdPath>,
    {
        let path = path.as_ref();
        let rel_path = util::to_relative_path(path)?;

        let mut buf = String::with_capacity(rel_path.as_os_str().len() + "crate::".len());
        buf.push_str("crate::");
        for segment in rel_path.components() {
            let segment = util::os_str_to_str(segment.as_os_str())?;
            let segment = segment.trim_end_matches(".rs");
            buf.push_str(segment);
            buf.push_str("::");
        }
        buf.pop(); // Removes "::".
        buf.pop();
        Ok(Self(buf))
    }

    pub fn segments(&self) -> ModPathSegIter<'_> {
        ModPathSegIter {
            path: self.as_str(),
        }
    }
}

impl Default for NamedPath {
    fn default() -> Self {
        Self(String::new())
    }
}

impl From<String> for NamedPath {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl fmt::Display for NamedPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Deref for NamedPath {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for NamedPath {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl PartialEq<str> for NamedPath {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl AsRef<str> for NamedPath {
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}

// === ModPathSegIter ===

#[derive(Clone)]
pub struct ModPathSegIter<'a> {
    path: &'a str,
}

impl<'a> Iterator for ModPathSegIter<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((head, tail)) = self.path.split_once("::") {
            self.path = tail;
            Some(head)
        } else if !self.path.is_empty() {
            let head = self.path;
            self.path = "";
            Some(head)
        } else {
            None
        }
    }
}

// === PathVis ===

#[derive(Debug, Clone)]
pub enum PathVis {
    Pub,
    PubCrate,
    PubSuper,
    PubPath(NamedPath),
    Private,
}

impl PathVis {
    pub fn new(vis: &syn::Visibility) -> Self {
        match vis {
            syn::Visibility::Public(_) => PathVis::Pub,
            syn::Visibility::Restricted(syn::VisRestricted { path, .. }) => {
                let path = util::path_to_string(&path);
                match path.as_str() {
                    "crate" => PathVis::PubCrate,
                    "super" => PathVis::PubSuper,
                    "self" => PathVis::Private,
                    _ => PathVis::PubPath(path.into()),
                }
            }
            syn::Visibility::Inherited => PathVis::Private,
        }
    }
}

impl Default for PathVis {
    fn default() -> Self {
        Self::Private
    }
}
