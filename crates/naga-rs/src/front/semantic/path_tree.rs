//! # Visibility
//!
//! Some [`PathValue`]s contain `vis_node` field. That field indicates a node
//! index of a module that the value is visible. See an example below.
//! ```ignore
//! mod a {
//!     mod b {
//!         struct A;            // vis_node: Node index to 'mod b'
//!         pub(super) struct B; // vis_node: Node index to 'mod a'
//!     }
//! }
//! ```
//! We use node index instead of path id becuase a node can have only one
//! module, plus, it makes visibility check easy.

use super::known;
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
use indexmap::IndexSet;
use smallvec::SmallVec;
use std::{
    borrow::Cow,
    cell::{Cell, UnsafeCell},
    cmp,
    collections::HashSet,
    fmt,
    hash::Hash,
    iter::{self, FusedIterator, Skip},
    mem, ops,
    path::{Path as StdPath, PathBuf},
    result::Result as StdResult,
};
use syn_locator::Locate;

pub struct PathTree {
    nodes: Vec<PathNode>,
    types: UnsafeCell<IndexSet<RefType>>,
}

impl PathTree {
    pub const ROOT: NodeIndex = NodeIndex(0);

    pub fn new() -> Self {
        // Starts with the root node.
        let nodes = vec![PathNode {
            values: SmallVec::new(),
            parent: Self::ROOT,
            children: Vec::new(),
        }];

        Self {
            nodes,
            types: UnsafeCell::new(IndexSet::new()),
        }
    }

    pub fn num_nodes(&self) -> usize {
        self.nodes.len()
    }

    pub fn iter(&self) -> PathIter<'_> {
        PathIter::new(self)
    }

    pub fn iter_mut(&mut self) -> PathIterMut<'_> {
        PathIterMut::new(self)
    }

    pub fn get_type(&self, id: RefTypeId) -> &RefType {
        let types = unsafe { self.types.get().as_ref().unwrap_unchecked() };
        &types[id.0]
    }

    pub fn get_type_id_of(&self, ty: &RefType) -> Option<RefTypeId> {
        let types = unsafe { self.types.get().as_ref().unwrap_unchecked() };
        types.get_index_of(ty).map(|index| RefTypeId(index))
    }

    pub fn insert_type(&self, ty: RefType) -> RefTypeId {
        let types = unsafe { self.types.get().as_mut().unwrap_unchecked() };
        let (index, _) = types.insert_full(ty);
        RefTypeId(index)
    }

    pub fn normalize_search_condition<'a, I>(
        &self,
        mut from: NodeIndex,
        ext: I,
    ) -> (NodeIndex, Skip<I>)
    where
        I: Iterator<Item = &'a str> + Clone,
    {
        let mut ext_off = 0;
        for seg in ext.clone() {
            if seg == "crate" {
                from = self.search(PathTree::ROOT, iter::once("crate")).unwrap();
                ext_off += 1;
            } else if seg == "super" {
                from = self.parent_container(from, [ContainerKind::Mod]).ni;
                ext_off += 1;
            } else {
                break;
            }
        }

        (from, ext.clone().skip(ext_off))
    }

    pub fn search_values<'a, I>(&self, key: I) -> PathNodeIter
    where
        I: IntoIterator<Item = &'a str>,
    {
        let Some(ni) = self.search(Self::ROOT, key) else {
            return PathNodeIter::empty();
        };
        self[ni].iter()
    }

    pub fn search_mut_values<'a, I>(&mut self, key: I) -> PathNodeIterMut
    where
        I: IntoIterator<Item = &'a str>,
    {
        let Some(ni) = self.search(Self::ROOT, key) else {
            return PathNodeIterMut::empty();
        };
        self[ni].iter_mut()
    }

    pub fn search<'a, I>(&self, from: NodeIndex, key: I) -> Option<NodeIndex>
    where
        I: IntoIterator<Item = &'a str>,
    {
        let mut cur = from;
        for seg in key {
            if let Some(next) = self[cur].children.iter().find(|(k, _)| k == seg) {
                cur = next.1;
            } else {
                return None;
            }
        }
        Some(cur)
    }

    /// Ok: node index to the target and advance count
    /// Err: node index to a node in the middle of search and advance count
    pub fn search_best<'a, I>(
        &self,
        from: NodeIndex,
        key: I,
    ) -> StdResult<(NodeIndex, usize), (NodeIndex, usize)>
    where
        I: IntoIterator<Item = &'a str>,
    {
        let mut cur = from;
        let mut advance = 0;
        for seg in key {
            if let Some(next) = self[cur].children.iter().find(|(k, _)| k == seg) {
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
        I: IntoIterator<Item = &'a str>,
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
        I: IntoIterator<Item = &'a str>,
        F: FnMut(&PathValue) -> bool,
    {
        let mut cur = Self::ROOT;
        let mut nearest = None;
        for seg in key {
            for (_, value) in self[cur].iter() {
                if cmp(value) {
                    nearest = Some(value);
                }
            }
            if let Some(next) = self[cur].children.iter().find(|(k, _)| k == seg) {
                cur = next.1;
            } else {
                break;
            }
        }
        for (_, value) in self[cur].iter() {
            if cmp(value) {
                nearest = Some(value);
            }
        }
        nearest
    }

    /// Visits all values matched by the given condition, `from` and `ext`.
    ///
    /// If the given function returns Some value, then it is returned
    /// immediately without further traversing.
    ///
    /// # Caution
    ///
    /// This method ignores unresolved `use` and 'type'.
    pub fn traverse<'a, I, II, F, R>(&self, from: NodeIndex, ext: I, mut f: F) -> Option<R>
    where
        I: IntoIterator<IntoIter = II>,
        II: Iterator<Item = &'a str> + Clone,
        F: FnMut(PathId, &PathValue) -> Option<R>,
    {
        let ext = ext.into_iter();
        let (from, ext) = self.normalize_search_condition(from, ext);

        let mut stack = Vec::<(NodeIndex, usize)>::new();
        stack.push((from, 0));

        let mut visited = HashSet::<NodeIndex>::new();

        let jump_if_possible =
            |stack: &mut Vec<(NodeIndex, usize)>, mid: NodeIndex, advance: usize| {
                for (_, value) in self[mid].iter() {
                    match value {
                        PathValue::Use(PathUse { dst, .. }) => {
                            stack.push((dst.ni, advance));
                        }
                        PathValue::TypeAlias(PathTypeAlias { tid, .. }) => {
                            if let RefType::Path(dst) = self.get_type(*tid) {
                                stack.push((dst.ni, advance));
                            }
                        }
                        _ => {}
                    }
                }
            };

        while let Some((from, ext_off)) = stack.pop() {
            match self.search_best(from, ext.clone().into_iter().skip(ext_off)) {
                Ok((dst, advance)) => {
                    if !visited.insert(dst) {
                        continue;
                    }

                    jump_if_possible(&mut stack, dst, advance);

                    for (vi, value) in self[dst].iter() {
                        let ret = f(dst.to_path_id(vi), value);
                        if ret.is_some() {
                            return ret;
                        }
                    }
                }
                Err((mid, advance)) => {
                    if !visited.insert(mid) {
                        continue;
                    }

                    jump_if_possible(&mut stack, mid, advance);

                    // Gives extern matching a try.
                    stack.push((Self::ROOT, ext_off + advance));
                }
            }
        }
        None
    }

    /// Finds a type from the given condition, `from` and `ext`.
    ///
    /// This method may look up the given `ext` with other conditions inherited
    /// from the given `from`. See example below.
    ///
    /// ```ignore
    /// // Let `from` is `a::b::foo::{0}::bar::{0}`
    /// // a::b::foo::{0}::bar::{0} // Searches in the block
    /// // a::b::foo::{0}           // Searches in the block
    /// // a::b                     // Searches in the module
    /// mod a {
    ///     mod b {            // Maybe `T` is defined or imported here
    ///         fn foo() {     // Maybe `T` is defined or imported here
    ///             fn bar() { // Maybe `T` is defined or imported here
    ///                 T
    ///             }
    ///         }
    ///     }
    /// }
    /// ```
    ///
    /// # Caution
    ///
    /// This method ignores unresolved 'use'.
    pub fn search_type<'a, I, II>(&self, from: NodeIndex, ext: I) -> Option<RefTypeId>
    where
        I: IntoIterator<IntoIter = II>,
        II: Iterator<Item = &'a str> + Clone,
    {
        let ext = ext.into_iter();

        // Determines where we are, 'mod' or 'block'.

        enum Context {
            Mod,
            Block,
        }

        let mut cx = None;

        for (_, value) in self[from].iter() {
            match value {
                PathValue::Mod(..) | PathValue::ModRaw(..) => {
                    cx = Some(Context::Mod);
                    break;
                }
                PathValue::Block(..) | PathValue::Fn(..) | PathValue::FnRaw(..) => {
                    cx = Some(Context::Block);
                    break;
                }
                _ => {}
            }
        }

        let cx = if let Some(cx) = cx {
            cx
        } else {
            let cont_pid = self.parent_container(
                from,
                [ContainerKind::Block, ContainerKind::Fn, ContainerKind::Mod],
            );
            match self[cont_pid] {
                PathValue::Mod(_) | PathValue::ModRaw(_) => Context::Mod,
                _ => Context::Block,
            }
        };

        let from = Cell::new(from);

        let cb = |pid: PathId, value: &PathValue| match value {
            PathValue::Extern => {
                let tid = self.insert_type(RefType::Path(pid));
                Some(tid)
            }
            PathValue::Struct(v) => {
                if self.is_descendant(from.get(), v.vis_node) {
                    let tid = self.insert_type(RefType::Path(pid));
                    Some(tid)
                } else {
                    None
                }
            }
            PathValue::StructRaw(v) => {
                if let Some(vis) = v.vis_node {
                    if self.is_descendant(from.get(), vis) {
                        let tid = self.insert_type(RefType::Path(pid));
                        return Some(tid);
                    }
                }
                None
            }
            PathValue::TypeAlias(v) => self.is_descendant(from.get(), v.vis_node).then_some(v.tid),
            PathValue::TypeAliasRaw(v) => {
                if let Some(vis) = v.vis_node {
                    if self.is_descendant(from.get(), vis) {
                        let tid = self.insert_type(RefType::Path(pid));
                        return Some(tid);
                    }
                }
                None
            }
            PathValue::Field(v) => Some(v.tid),
            _ => None,
        };

        // In a module context, we cannot see items defined in parent blocks.
        // Therefore, we don't have to search on other conditions.
        if matches!(cx, Context::Mod) {
            self.traverse(from.get(), ext, cb)
        }
        // In a block context, we can also see items defined in parant blocks
        // without explicit prefix like 'super' or something like that. So, we
        // have to consider parent blocks as well.
        else {
            'outer: loop {
                let res = self.traverse(from.get(), ext.clone(), cb);
                if res.is_some() {
                    return res;
                }

                for (_, value) in self[from.get()].iter() {
                    match value {
                        PathValue::Block(..) | PathValue::Fn(..) | PathValue::FnRaw(..) => {
                            let parent = self[from.get()].parent;
                            from.set(parent);
                            break;
                        }
                        PathValue::Mod(..) | PathValue::ModRaw(..) => break 'outer,
                        _ => {}
                    }
                }
            }

            None
        }
    }

    /// Finds the nearest value from the given condition, `from`, `ext` and
    /// `pivot`. The nearest value will be chosen, so be careful of cases like
    /// below.
    ///
    /// ```ignore
    /// let x = ...;
    /// let x = x ...; // Pivot should be less than lvalue 'x', not rvalue 'x'.
    /// ```
    ///
    /// Plus, this method looks up the given `ext` with other conditions
    /// inherited from the given `from`. See example below.
    ///
    /// ```ignore
    /// // Let `from` is `a::b::foo::{0}::bar`
    /// // a::b::foo::{0}::bar::{0} // Searches in the block
    /// // a::b::foo::{0}::bar      // Searches in the function arguments
    /// // a::b                     // Searches in the module
    /// mod a {
    ///     mod b {
    ///         fn foo() {
    ///             fn bar() {
    ///                 // Searches here
    ///                 pivot
    ///                 // Does not search here
    ///             }
    ///         }
    ///     }
    /// }
    /// ```
    ///
    /// # Caution
    ///
    /// This method ignores unresolved `use`.
    pub fn search_value<'a, I, II>(
        &self,
        mut from: NodeIndex,
        ext: I,
        pivot: usize,
    ) -> Option<PathId>
    where
        I: IntoIterator<IntoIter = II>,
        II: Iterator<Item = &'a str> + Clone,
    {
        let ext = ext.into_iter();

        let found = Cell::new(None);
        let mut least_diff = usize::MAX;

        'outer: loop {
            self.traverse(from, ext.clone(), |pid, value| {
                const CONTINUE_TRAVERSING: Option<()> = None;
                const STOP_TRAVERSING: Option<()> = Some(());

                let try1 = |vis: NodeIndex| {
                    if self.is_descendant(from, vis) {
                        found.set(Some(pid));
                        return STOP_TRAVERSING;
                    }
                    CONTINUE_TRAVERSING
                };

                let try2 = |vis: Option<NodeIndex>| {
                    if let Some(vis) = vis {
                        return try1(vis);
                    }
                    CONTINUE_TRAVERSING
                };

                match value {
                    PathValue::Const(v) => try1(v.vis_node),
                    PathValue::ConstRaw(v) => try2(v.vis_node),
                    PathValue::Fn(v) => try1(v.vis_node),
                    PathValue::FnRaw(v) => try2(v.vis_node),
                    // Struct constructs are in value namespace.
                    PathValue::Struct(v) => try1(v.vis_node),
                    PathValue::StructRaw(v) => try2(v.vis_node),
                    PathValue::Local(v) => {
                        let end = v.as_syn().location().end;
                        if end < pivot && (pivot - end) < least_diff {
                            least_diff = pivot - end;
                            found.set(Some(pid));
                        }
                        CONTINUE_TRAVERSING
                    }
                    PathValue::LocalRaw(v) => {
                        let end = v.as_syn().location().end;
                        if end < pivot && (pivot - end) < least_diff {
                            least_diff = pivot - end;
                            found.set(Some(pid));
                        }
                        CONTINUE_TRAVERSING
                    }
                    _ => CONTINUE_TRAVERSING,
                }
            });

            if found.get().is_some() {
                break;
            }

            for (_, value) in self[from].iter() {
                match value {
                    PathValue::Block(..) => {
                        from = self[from].parent;
                        break;
                    }
                    PathValue::Fn(..) | PathValue::FnRaw(..) => {
                        from = self.parent_container(from, [ContainerKind::Mod]).ni;
                        break;
                    }
                    PathValue::Mod(..) | PathValue::ModRaw(..) => break 'outer,
                    _ => {}
                }
            }
        }

        found.get()
    }

    pub fn named_path(&self, index: NodeIndex) -> NamedPath {
        let mut cur = index;
        let mut segments = Vec::new();
        while cur != Self::ROOT {
            let c = &self[cur];
            let p = &self[c.parent];
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
            cur = self[cur].parent;
        }
        true
    }

    /// Returns the nearest module's id.
    ///
    /// If the node at the given index contains module, then returns its id.
    pub fn nearest_module(&self, index: NodeIndex) -> PathId {
        // If input is a module, then returns it.
        for (vi, value) in self[index].iter() {
            if matches!(value, PathValue::Mod(..) | PathValue::ModRaw(..)) {
                return index.to_path_id(vi);
            }
        }

        // Otherwise, returns parent module.
        self.parent_container(index, [ContainerKind::Mod])
    }

    pub fn parent_container<I, II>(&self, mut index: NodeIndex, filters: I) -> PathId
    where
        I: IntoIterator<IntoIter = II>,
        II: Iterator<Item = ContainerKind> + Clone,
    {
        let filters = filters.into_iter();

        while index != Self::ROOT {
            let cont = self._parent_container(index);
            if filters.clone().into_iter().any(|kind| kind == cont.kind) {
                return cont.id;
            }
            index = cont.id.ni;
        }
        index.to_path_id(0) // Invalid id
    }

    fn _parent_container(&self, index: NodeIndex) -> Container {
        let mut parent = self[index].parent;
        let mut values = self[parent].iter();

        // There are no values between 'crate' and the entry module. In
        // that case, goes up to the 'crate'.
        while parent != PathTree::ROOT && values.is_empty() {
            parent = self[parent].parent;
            values = self[parent].iter();
        }

        for (vi, value) in self[parent].iter() {
            match value {
                PathValue::Mod(..) | PathValue::ModRaw(..) => {
                    return Container {
                        kind: ContainerKind::Mod,
                        id: parent.to_path_id(vi),
                    };
                }
                PathValue::Block(..) => {
                    return Container {
                        kind: ContainerKind::Block,
                        id: parent.to_path_id(vi),
                    };
                }
                PathValue::Fn(..) | PathValue::FnRaw(..) => {
                    return Container {
                        kind: ContainerKind::Fn,
                        id: parent.to_path_id(vi),
                    };
                }
                PathValue::Struct(..) | PathValue::StructRaw(..) => {
                    return Container {
                        kind: ContainerKind::Struct,
                        id: parent.to_path_id(vi),
                    };
                }
                _ => {}
            }
        }

        debug_assert_eq!(parent, PathTree::ROOT);
        Container {
            kind: ContainerKind::Root,
            id: parent.to_path_id(0), // Invalid id
        }
    }

    pub fn append_value<'a, I>(&mut self, from: NodeIndex, key: I, value: PathValue) -> PathId
    where
        I: IntoIterator<Item = &'a str>,
    {
        self.insert_node_then(from, key, |ni, node| {
            let vi = node.push(value);
            ni.to_path_id(vi)
        })
    }

    fn insert_node_then<'a, I, F, R>(&mut self, from: NodeIndex, key: I, then: F) -> R
    where
        I: IntoIterator<Item = &'a str>,
        F: FnOnce(NodeIndex, &mut PathNode) -> R,
    {
        let mut cur = from;
        for seg in key {
            if let Some(next) = self[cur].children.iter().find(|(k, _)| k == seg) {
                cur = next.1;
            } else {
                let ni = self.new_node(cur);
                let seg = seg.to_owned().into();
                self[cur].children.push((seg, ni));
                cur = ni;
            }
        }
        then(cur, &mut self[cur])
    }

    /// # Panics
    ///
    /// Panics if `id` is out of bounds.
    pub fn take_value(&mut self, id: PathId) -> PathValue {
        self[id.ni].take(id.vi)
    }

    fn new_node(&mut self, parent: NodeIndex) -> NodeIndex {
        self.nodes.push(PathNode {
            values: SmallVec::new(),
            parent,
            children: Vec::new(),
        });
        NodeIndex(self.nodes.len() - 1)
    }
}

impl Default for PathTree {
    fn default() -> Self {
        Self::new()
    }
}

impl ops::Index<PathId> for PathTree {
    type Output = PathValue;

    fn index(&self, id: PathId) -> &Self::Output {
        let node = &self[id.ni];
        &node.values[id.vi]
    }
}

impl ops::IndexMut<PathId> for PathTree {
    fn index_mut(&mut self, id: PathId) -> &mut Self::Output {
        let node = &mut self[id.ni];
        &mut node.values[id.vi]
    }
}

impl ops::Index<NodeIndex> for PathTree {
    type Output = PathNode;

    fn index(&self, index: NodeIndex) -> &Self::Output {
        &self.nodes[index]
    }
}

impl ops::IndexMut<NodeIndex> for PathTree {
    fn index_mut(&mut self, index: NodeIndex) -> &mut Self::Output {
        &mut self.nodes[index]
    }
}

impl fmt::Debug for PathTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Known libraries such as "std" or primitives like "u32" are not
        // printed out except things referenced from client code.
        let referenced_known = referenced_known(self);

        thread_local! {
            static NEED_SEP: Cell<bool> = Cell::new(false);
        }

        let mut key = NamedPath::EMPTY;
        let res = write_(self, Self::ROOT, &mut key, &referenced_known, f);
        NEED_SEP.set(false);
        return res;

        // === Internal helper functions ===

        fn referenced_known(this: &PathTree) -> HashSet<PathId> {
            let mut referenced_known = HashSet::new();
            for (pid, subject_value) in this.iter() {
                let subject = this.named_path(pid.ni);
                if known::is_known_lib(subject.segments().next().unwrap()) {
                    continue;
                }
                match subject_value {
                    PathValue::Const(v) => {
                        insert_referenced_primitive(this, v.tid, &mut referenced_known);
                    }
                    PathValue::Field(v) => {
                        insert_referenced_primitive(this, v.tid, &mut referenced_known);
                    }
                    PathValue::Fn(v) => {
                        insert_referenced_primitive(this, v.ret_tid, &mut referenced_known);
                    }
                    PathValue::Local(v) => {
                        insert_referenced_primitive(this, v.tid, &mut referenced_known);
                    }
                    PathValue::TypeAlias(v) => {
                        insert_referenced_primitive(this, v.tid, &mut referenced_known);
                    }
                    PathValue::Use(v) => {
                        let object = this.named_path(v.dst.ni);
                        if known::is_known_lib(object.segments().next().unwrap()) {
                            referenced_known.insert(v.dst);
                        }
                    }
                    _ => {}
                }
            }
            referenced_known
        }

        fn insert_referenced_primitive(
            this: &PathTree,
            tid: RefTypeId,
            referenced_known: &mut HashSet<PathId>,
        ) {
            match this.get_type(tid) {
                RefType::Path(pid) => {
                    let npath = this.named_path(pid.ni);
                    if known::is_known_primitive(&npath) {
                        referenced_known.insert(*pid);
                    }
                }
                RefType::Tuple(tuple) => {
                    for elem in tuple {
                        insert_referenced_primitive(this, *elem, referenced_known);
                    }
                }
                RefType::Array { elem } => {
                    insert_referenced_primitive(this, *elem, referenced_known);
                }
                RefType::Slice { elem } => {
                    insert_referenced_primitive(this, *elem, referenced_known);
                }
                RefType::Reference { elem, .. } => {
                    insert_referenced_primitive(this, *elem, referenced_known);
                }
                RefType::InferredInt | RefType::InferredFloat | RefType::Default => {}
            }
        }

        fn write_(
            this: &PathTree,
            ni: NodeIndex,
            key: &mut NamedPath,
            referenced_known: &HashSet<PathId>,
            f: &mut fmt::Formatter<'_>,
        ) -> fmt::Result {
            for (seg, child) in &this[ni].children {
                let org_len = key.len();
                util::push_colon_path(key, seg);

                for (vi, value) in this[*child].iter() {
                    let pid = child.to_path_id(vi);

                    // Unreferenced known libraries are skipped.
                    if known::is_known_lib(key.segments().next().unwrap())
                        && !referenced_known.contains(&pid)
                    {
                        // !!! TODO: Uncomment me
                        // continue;
                    }

                    // Unreferenced known primivites are also skipped.
                    if known::is_known_primitive(key) && !referenced_known.contains(&pid) {
                        // !!! TODO: Uncomment me
                        // continue;
                    }

                    if NEED_SEP.get() {
                        write!(f, "\n")?;
                    }
                    write!(f, "{pid} {key} -> ")?;
                    value.write_debug(this, f)?;
                    NEED_SEP.set(true);
                }
                write_(this, *child, key, referenced_known, f)?;

                key.truncate(org_len);
            }
            Ok(())
        }
    }
}

#[derive(Debug)]
pub struct PathNode {
    pub values: SmallVec<[PathValue; 1]>,
    pub parent: NodeIndex,
    pub children: Vec<(NamedPath, NodeIndex)>,
}

impl PathNode {
    pub fn iter(&self) -> PathNodeIter {
        PathNodeIter::new(self)
    }

    pub fn iter_mut(&mut self) -> PathNodeIterMut {
        PathNodeIterMut::new(self)
    }

    pub fn push(&mut self, value: PathValue) -> ValueIndex {
        self.values.push(value);
        ValueIndex(self.values.len() - 1)
    }

    /// # Panics
    ///
    /// Panics if the given index is out of bounds.
    pub fn take(&mut self, vi: ValueIndex) -> PathValue {
        mem::take(&mut self.values[vi.0])
    }
}

/// An iterator traversing the whole tree and yielding pairs of [`PathId`] and
/// reference to a [`PathValue`].
#[derive(Clone)]
pub struct PathIter<'a> {
    nodes: &'a [PathNode],
    node_iter: PathNodeIter<'a>,
    ni: NodeIndex,
}

impl<'a> PathIter<'a> {
    fn new(tree: &'a PathTree) -> Self {
        Self {
            nodes: &tree.nodes,
            node_iter: PathNodeIter::empty(),
            ni: NodeIndex(0),
        }
    }
}

impl<'a> Iterator for PathIter<'a> {
    type Item = (PathId, &'a PathValue);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some((vi, value)) = self.node_iter.next() {
                let ni = self.ni - 1;
                return Some((ni.to_path_id(vi), value));
            }

            if self.ni < self.nodes.len() {
                self.node_iter = self.nodes[self.ni].iter();
                self.ni += 1;
            } else {
                return None;
            }
        }
    }
}

impl FusedIterator for PathIter<'_> {}

/// An iterator traversing the whole tree and yielding pairs of [`PathId`] and
/// mutable reference to a [`PathValue`].
pub struct PathIterMut<'a> {
    nodes: &'a mut [PathNode],
    node_iter: PathNodeIterMut<'a>,
    ni: NodeIndex,
}

impl<'a> PathIterMut<'a> {
    fn new(tree: &'a mut PathTree) -> Self {
        Self {
            nodes: &mut tree.nodes,
            node_iter: PathNodeIterMut::empty(),
            ni: NodeIndex(0),
        }
    }
}

impl<'a> Iterator for PathIterMut<'a> {
    type Item = (PathId, &'a mut PathValue);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some((vi, value)) = self.node_iter.next() {
                let ni = self.ni - 1;
                return Some((ni.to_path_id(vi), value));
            }

            match mem::take(&mut self.nodes) {
                [] => return None,
                [head, tail @ ..] => {
                    self.nodes = tail;
                    self.node_iter = head.iter_mut();
                    self.ni += 1;
                }
            }
        }
    }
}

impl FusedIterator for PathIterMut<'_> {}

/// An iterator traversing a node's values and yielding pairs of value index and
/// reference to a [`PathValue`].
#[derive(Clone)]
pub struct PathNodeIter<'a> {
    values: &'a [PathValue],
    vi: usize,
}

impl<'a> PathNodeIter<'a> {
    fn new(node: &'a PathNode) -> Self {
        Self {
            values: &node.values,
            vi: 0,
        }
    }

    fn empty() -> Self {
        Self { values: &[], vi: 0 }
    }

    pub fn is_empty(&self) -> bool {
        self.clone().count() == 0
    }
}

impl<'a> Iterator for PathNodeIter<'a> {
    type Item = (ValueIndex, &'a PathValue);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(PathValue::None) = self.values.get(self.vi) {
            self.vi += 1;
        }

        match self.values.get(self.vi) {
            Some(PathValue::None) | None => None,
            Some(value) => {
                let item = (ValueIndex(self.vi), value);
                self.vi += 1;
                Some(item)
            }
        }
    }
}

impl FusedIterator for PathNodeIter<'_> {}

/// An iterator traversing a node's values and yielding pairs of value index and
/// mutable reference to a [`PathValue`].
pub struct PathNodeIterMut<'a> {
    values: &'a mut [PathValue],
    vi: usize,
}

impl<'a> PathNodeIterMut<'a> {
    fn new(node: &'a mut PathNode) -> Self {
        Self {
            values: &mut node.values,
            vi: 0,
        }
    }

    fn empty() -> Self {
        Self {
            values: &mut [],
            vi: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        // `self.value` gets shorten as we call `next()`.
        self.values.iter().all(|value| value == &PathValue::None)
    }
}

impl<'a> Iterator for PathNodeIterMut<'a> {
    type Item = (ValueIndex, &'a mut PathValue);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match mem::take(&mut self.values) {
                [] => return None,
                [PathValue::None, tail @ ..] => {
                    self.values = tail;
                    self.vi += 1;
                    continue;
                }
                [head, tail @ ..] => {
                    let item = (ValueIndex(self.vi), head);

                    self.values = tail;
                    self.vi += 1;

                    return Some(item);
                }
            }
        }
    }
}

impl FusedIterator for PathNodeIterMut<'_> {}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
        write!(f, "({},{})", self.ni, self.vi)
    }
}

impl fmt::Debug for PathId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({},{})", self.ni, self.vi)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeIndex(pub usize);

impl NodeIndex {
    pub fn to_path_id<I: Into<ValueIndex>>(self, vi: I) -> PathId {
        PathId {
            ni: self,
            vi: vi.into(),
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

impl ops::Sub<usize> for NodeIndex {
    type Output = Self;

    fn sub(self, rhs: usize) -> Self::Output {
        Self(self.0 - rhs)
    }
}

impl ops::Index<NodeIndex> for [PathNode] {
    type Output = PathNode;

    fn index(&self, index: NodeIndex) -> &Self::Output {
        &self[index.0]
    }
}

impl ops::IndexMut<NodeIndex> for [PathNode] {
    fn index_mut(&mut self, index: NodeIndex) -> &mut Self::Output {
        &mut self[index.0]
    }
}

impl ops::Index<NodeIndex> for Vec<PathNode> {
    type Output = PathNode;

    fn index(&self, index: NodeIndex) -> &Self::Output {
        &self.as_slice()[index]
    }
}

impl ops::IndexMut<NodeIndex> for Vec<PathNode> {
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

impl ops::Index<ValueIndex> for [PathValue] {
    type Output = PathValue;

    fn index(&self, index: ValueIndex) -> &Self::Output {
        &self[index.0]
    }
}

impl ops::IndexMut<ValueIndex> for [PathValue] {
    fn index_mut(&mut self, index: ValueIndex) -> &mut Self::Output {
        &mut self[index.0]
    }
}

impl ops::Index<ValueIndex> for SmallVec<[PathValue; 1]> {
    type Output = PathValue;

    fn index(&self, index: ValueIndex) -> &Self::Output {
        &self.as_slice()[index]
    }
}

impl ops::IndexMut<ValueIndex> for SmallVec<[PathValue; 1]> {
    fn index_mut(&mut self, index: ValueIndex) -> &mut Self::Output {
        &mut self.as_mut_slice()[index]
    }
}

#[derive(PartialEq, Eq)]
pub enum PathValue {
    Block(PathBlock),
    Const(PathConst),
    ConstRaw(PathConstRaw),
    Extern,
    Field(PathField),
    FieldRaw(PathFieldRaw),
    Fn(PathFn),
    FnRaw(PathFnRaw),
    Local(PathLocal),
    LocalRaw(PathLocalRaw),
    Mod(PathMod),
    ModRaw(PathModRaw),
    Struct(PathStruct),
    StructRaw(PathStructRaw),
    Trait(PathTrait),
    TraitRaw(PathTraitRaw),
    TypeAlias(PathTypeAlias),
    TypeAliasRaw(PathTypeAliasRaw),
    Use(PathUse),
    UseRaw(PathUseRaw),
    None,
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

    pub fn as_local(&self) -> &PathLocal {
        let Self::Local(v) = self else {
            unreachable!("invalid variant at `PathValue::as_local`")
        };
        v
    }

    pub fn as_mut_local(&mut self) -> &mut PathLocal {
        let Self::Local(v) = self else {
            unreachable!("invalid variant at `PathValue::as_mut_local`")
        };
        v
    }

    pub fn as_local_raw(&self) -> &PathLocalRaw {
        let Self::LocalRaw(v) = self else {
            unreachable!("invalid variant at `PathValue::as_local_raw`")
        };
        v
    }

    pub fn as_mut_local_raw(&mut self) -> &mut PathLocalRaw {
        let Self::LocalRaw(v) = self else {
            unreachable!("invalid variant at `PathValue::as_mut_local_raw`")
        };
        v
    }

    pub fn as_mod(&self) -> &PathMod {
        let Self::Mod(v) = self else {
            unreachable!("invalid variant at `PathValue::as_mod`")
        };
        v
    }

    pub fn as_mut_mod(&mut self) -> &mut PathMod {
        let Self::Mod(v) = self else {
            unreachable!("invalid variant at `PathValue::as_mut_mod`")
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

    pub fn as_type_alias(&self) -> &PathTypeAlias {
        let Self::TypeAlias(v) = self else {
            unreachable!("invalid variant at `PathValue::as_type_alias`")
        };
        v
    }

    pub fn as_mut_type_alias(&mut self) -> &mut PathTypeAlias {
        let Self::TypeAlias(v) = self else {
            unreachable!("invalid variant at `PathValue::as_mut_type_alias`")
        };
        v
    }

    pub fn as_type_alias_raw(&self) -> &PathTypeAliasRaw {
        let Self::TypeAliasRaw(v) = self else {
            unreachable!("invalid variant at `PathValue::as_type_alias_raw`")
        };
        v
    }

    pub fn as_mut_use(&mut self) -> &mut PathUse {
        let Self::Use(v) = self else {
            unreachable!("invalid variant at `PathValue::as_mut_use`")
        };
        v
    }

    pub fn as_use(&self) -> &PathUse {
        let Self::Use(v) = self else {
            unreachable!("invalid variant at `PathValue::as_use`")
        };
        v
    }

    pub fn as_use_raw(&self) -> &PathUseRaw {
        let Self::UseRaw(v) = self else {
            unreachable!("invalid variant at `PathValue::as_use_raw`")
        };
        v
    }

    pub fn as_mut_use_raw(&mut self) -> &mut PathUseRaw {
        let Self::UseRaw(v) = self else {
            unreachable!("invalid variant at `PathValue::as_mut_use_raw`")
        };
        v
    }

    pub fn vis_node(&self) -> Option<NodeIndex> {
        match self {
            Self::Block(v) => {
                panic!(
                    "{}: not available visibility for block",
                    v.syn_id().content()
                );
            }
            Self::Const(v) => Some(v.vis_node),
            Self::ConstRaw(v) => v.vis_node,
            Self::Extern => None,
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
            Self::Fn(v) => Some(v.vis_node),
            Self::FnRaw(v) => v.vis_node,
            Self::Local(..) => None,
            Self::LocalRaw(..) => None,
            Self::Mod(v) => Some(v.vis_node),
            Self::ModRaw(v) => v.vis_node,
            Self::Struct(v) => Some(v.vis_node),
            Self::StructRaw(v) => v.vis_node,
            Self::Trait(v) => Some(v.vis_node),
            Self::TraitRaw(v) => v.vis_node,
            Self::TypeAlias(v) => Some(v.vis_node),
            Self::TypeAliasRaw(v) => v.vis_node,
            Self::Use(v) => Some(v.vis_node),
            Self::UseRaw(v) => v.vis_node,
            Self::None => None,
        }
    }

    fn write_debug<T: fmt::Write>(&self, ptree: &PathTree, f: &mut T) -> fmt::Result {
        match self {
            Self::Block(v) => v.write_debug(ptree, f),
            Self::Const(v) => v.write_debug(ptree, f),
            Self::ConstRaw(v) => v.write_debug(ptree, f),
            Self::Extern => write!(f, "Extern"),
            Self::Field(v) => v.write_debug(ptree, f),
            Self::FieldRaw(v) => v.write_debug(ptree, f),
            Self::Fn(v) => v.write_debug(ptree, f),
            Self::FnRaw(v) => v.write_debug(ptree, f),
            Self::Local(v) => v.write_debug(ptree, f),
            Self::LocalRaw(v) => v.write_debug(ptree, f),
            Self::Mod(v) => v.write_debug(ptree, f),
            Self::ModRaw(v) => v.write_debug(ptree, f),
            Self::Struct(v) => v.write_debug(ptree, f),
            Self::StructRaw(v) => v.write_debug(ptree, f),
            Self::Trait(v) => v.write_debug(ptree, f),
            Self::TraitRaw(v) => v.write_debug(ptree, f),
            Self::TypeAlias(v) => v.write_debug(ptree, f),
            Self::TypeAliasRaw(v) => v.write_debug(ptree, f),
            Self::Use(v) => v.write_debug(ptree, f),
            Self::UseRaw(v) => v.write_debug(ptree, f),
            Self::None => Ok(()),
        }
    }
}

impl Default for PathValue {
    fn default() -> Self {
        Self::None
    }
}

impl fmt::Debug for PathValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Block(v) => v.fmt(f),
            Self::Const(v) => v.fmt(f),
            Self::ConstRaw(v) => v.fmt(f),
            Self::Extern => write!(f, "Extern"),
            Self::Field(v) => v.fmt(f),
            Self::FieldRaw(v) => v.fmt(f),
            Self::Fn(v) => v.fmt(f),
            Self::FnRaw(v) => v.fmt(f),
            Self::Local(v) => v.fmt(f),
            Self::LocalRaw(v) => v.fmt(f),
            Self::Mod(v) => v.fmt(f),
            Self::ModRaw(v) => v.fmt(f),
            Self::Struct(v) => v.fmt(f),
            Self::StructRaw(v) => v.fmt(f),
            Self::Trait(v) => v.fmt(f),
            Self::TraitRaw(v) => v.fmt(f),
            Self::TypeAlias(v) => v.fmt(f),
            Self::TypeAliasRaw(v) => v.fmt(f),
            Self::Use(v) => v.fmt(f),
            Self::UseRaw(v) => v.fmt(f),
            Self::None => Ok(()),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
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

    fn write_debug<T: fmt::Write>(&self, _: &PathTree, f: &mut T) -> fmt::Result {
        write!(f, "Block {{ syn: {:?} }}", self.syn)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PathConst {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemConst,

    /// Refer to the [module documentation](self).
    pub vis_node: NodeIndex,

    /// Type of this value.
    pub tid: RefTypeId,
}

impl PathConst {
    pub fn as_syn<'o>(&self) -> &'o syn::ItemConst {
        unsafe { self.syn.as_ref().expect("PathConst contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }

    fn write_debug<T: fmt::Write>(&self, ptree: &PathTree, f: &mut T) -> fmt::Result {
        write!(
            f,
            "Const {{ syn: {:?}, vis_node: {:?}, ty: ",
            self.syn, self.vis_node,
        )?;
        ptree.get_type(self.tid).write_debug(ptree, f)?;
        write!(f, " }}")
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PathConstRaw {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemConst,

    /// Refer to the [module documentation](self).
    pub vis_node: Option<NodeIndex>,
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

    fn write_debug<T: fmt::Write>(&self, _: &PathTree, f: &mut T) -> fmt::Result {
        write!(
            f,
            "ConstRaw {{ syn: {:?}, vis_node: {:?} }}",
            self.syn, self.vis_node
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PathField {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::Field,

    /// Refer to the [module documentation](self).
    pub vis_node: NodeIndex,

    /// Type of this value.
    pub tid: RefTypeId,
}

impl PathField {
    pub fn as_syn<'o>(&self) -> &'o syn::Field {
        unsafe { self.syn.as_ref().expect("PathField contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }

    fn write_debug<T: fmt::Write>(&self, ptree: &PathTree, f: &mut T) -> fmt::Result {
        write!(
            f,
            "Field {{ syn: {:?}, vis_node: {:?}, ty: ",
            self.syn, self.vis_node
        )?;
        ptree.get_type(self.tid).write_debug(ptree, f)?;
        write!(f, " }}")
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PathFieldRaw {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::Field,

    /// Refer to the [module documentation](self).
    pub vis_node: Option<NodeIndex>,
}

impl PathFieldRaw {
    pub fn as_syn<'o>(&self) -> &'o syn::Field {
        unsafe { self.syn.as_ref().expect("PathFieldRaw contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }

    pub fn visibility(&self) -> PathVis {
        PathVis::new(&self.as_syn().vis)
    }

    fn write_debug<T: fmt::Write>(&self, _: &PathTree, f: &mut T) -> fmt::Result {
        write!(
            f,
            "FieldRaw {{ syn: {:?}, vis_node: {:?} }}",
            self.syn, self.vis_node
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PathFn {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemFn,

    /// Refer to the [module documentation](self).
    pub vis_node: NodeIndex,

    /// Return type.
    pub ret_tid: RefTypeId,
}

impl PathFn {
    pub fn as_syn<'o>(&self) -> &'o syn::ItemFn {
        unsafe { self.syn.as_ref().expect("PathFn contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }

    fn write_debug<T: fmt::Write>(&self, ptree: &PathTree, f: &mut T) -> fmt::Result {
        write!(
            f,
            "Fn {{ syn: {:?}, vis_node: {:?}, ret_ty: ",
            self.syn, self.vis_node
        )?;
        ptree.get_type(self.ret_tid).write_debug(ptree, f)?;
        write!(f, " }}")
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PathFnRaw {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemFn,

    /// Refer to the [module documentation](self).
    pub vis_node: Option<NodeIndex>,
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

    fn write_debug<T: fmt::Write>(&self, _: &PathTree, f: &mut T) -> fmt::Result {
        write!(
            f,
            "FnRaw {{ syn: {:?}, vis_node: {:?} }}",
            self.syn, self.vis_node
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PathLocal {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::PatIdent,
    pub mut_: bool,

    /// Type of this value.
    pub tid: RefTypeId,
}

impl PathLocal {
    pub fn as_syn<'o>(&self) -> &'o syn::PatIdent {
        unsafe { self.syn.as_ref().expect("PathLocal contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }

    fn write_debug<T: fmt::Write>(&self, ptree: &PathTree, f: &mut T) -> fmt::Result {
        write!(
            f,
            "Local {{ syn: {:?}, mut: {:?}, ty: ",
            self.syn, self.mut_
        )?;
        ptree.get_type(self.tid).write_debug(ptree, f)?;
        write!(f, " }}")
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PathLocalRaw {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::PatIdent,
    pub mut_: bool,
}

impl PathLocalRaw {
    pub fn as_syn<'o>(&self) -> &'o syn::PatIdent {
        unsafe { self.syn.as_ref().expect("PathLocalRaw contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }

    fn write_debug<T: fmt::Write>(&self, _: &PathTree, f: &mut T) -> fmt::Result {
        write!(
            f,
            "LocalRaw {{ syn: {:?}, mut: {:?} }}",
            self.syn, self.mut_
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PathMod {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemMod,

    pub syn_file: Option<*const SmFile>,

    /// Refer to the [module documentation](self).
    pub vis_node: NodeIndex,

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
    /// - True if the file is determined by "path" attribute and the module is
    ///   not inline (e.g. #[path = "a.rs"] mod foo;)
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

    fn write_debug<T: fmt::Write>(&self, _: &PathTree, f: &mut T) -> fmt::Result {
        write!(
            f,
            "Mod {{ syn: {:?}, syn_file: {:?}, vis_node: {:?}, fpath: {:?}, mod_rs: {:?} }}",
            self.syn, self.syn_file, self.vis_node, self.fpath, self.mod_rs
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PathModRaw {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemMod,
    pub syn_file: Option<*const SmFile>,

    /// Refer to the [module documentation](self).
    pub vis_node: Option<NodeIndex>,
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

    fn write_debug<T: fmt::Write>(&self, _: &PathTree, f: &mut T) -> fmt::Result {
        write!(
            f,
            "ModRaw {{ syn: {:?}, syn_file: {:?}, vis_node: {:?}, fpath: {:?}, mod_rs: {:?} }}",
            self.syn, self.syn_file, self.vis_node, self.fpath, self.mod_rs
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PathStruct {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemStruct,

    /// Refer to the [module documentation](self).
    pub vis_node: NodeIndex,
}

impl PathStruct {
    pub fn as_syn<'o>(&self) -> &'o syn::ItemStruct {
        unsafe { self.syn.as_ref().expect("PathStruct contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }

    fn write_debug<T: fmt::Write>(&self, _: &PathTree, f: &mut T) -> fmt::Result {
        write!(
            f,
            "Struct {{ syn: {:?}, vis_node: {:?} }}",
            self.syn, self.vis_node
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PathStructRaw {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemStruct,

    /// Refer to the [module documentation](self).
    pub vis_node: Option<NodeIndex>,
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

    fn write_debug<T: fmt::Write>(&self, _: &PathTree, f: &mut T) -> fmt::Result {
        write!(
            f,
            "StructRaw {{ syn: {:?}, vis_node: {:?} }}",
            self.syn, self.vis_node
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PathTrait {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemTrait,

    /// Refer to the [module documentation](self).
    pub vis_node: NodeIndex,
}

impl PathTrait {
    fn write_debug<T: fmt::Write>(&self, ptree: &PathTree, f: &mut T) -> fmt::Result {
        write!(
            f,
            "Trait {{ syn: {:?}, vis_node: {:?} }}",
            self.syn, self.vis_node
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PathTraitRaw {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemTrait,

    /// Refer to the [module documentation](self).
    pub vis_node: Option<NodeIndex>,
}

impl PathTraitRaw {
    pub fn as_syn<'o>(&self) -> &'o syn::ItemTrait {
        unsafe { self.syn.as_ref().expect("PathTraitRaw contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }

    pub fn visibility(&self) -> PathVis {
        PathVis::new(&self.as_syn().vis)
    }

    fn write_debug<T: fmt::Write>(&self, _: &PathTree, f: &mut T) -> fmt::Result {
        write!(
            f,
            "TraitRaw {{ syn: {:?}, vis_node: {:?} }}",
            self.syn, self.vis_node
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PathTypeAlias {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemType,

    /// Refer to the [module documentation](self).
    pub vis_node: NodeIndex,

    /// Type of this value.
    pub tid: RefTypeId,
}

impl PathTypeAlias {
    pub fn as_syn<'o>(&self) -> &'o syn::ItemType {
        unsafe { self.syn.as_ref().expect("PathTypeAlias contains nullptr") }
    }

    pub fn syn_id(&self) -> SynId {
        self.as_syn().syn_id()
    }

    fn write_debug<T: fmt::Write>(&self, ptree: &PathTree, f: &mut T) -> fmt::Result {
        write!(
            f,
            "TypeAlias {{ syn: {:?}, vis_node: {:?}, ty: ",
            self.syn, self.vis_node
        )?;
        ptree.get_type(self.tid).write_debug(ptree, f)?;
        write!(f, " }}")
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PathTypeAliasRaw {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemType,

    /// Refer to the [module documentation](self).
    pub vis_node: Option<NodeIndex>,
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

    fn write_debug<T: fmt::Write>(&self, _: &PathTree, f: &mut T) -> fmt::Result {
        write!(
            f,
            "TypeAliasRaw {{ syn: {:?}, vis_node: {:?} }}",
            self.syn, self.vis_node
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PathUse {
    /// Refer to the [module documentation](self).
    pub vis_node: NodeIndex,
    pub dst: PathId,
}

impl PathUse {
    fn write_debug<T: fmt::Write>(&self, _: &PathTree, f: &mut T) -> fmt::Result {
        write!(
            f,
            "Use {{ vis_node: {:?}, dst: {:?} }}",
            self.vis_node, self.dst
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PathUseRaw {
    /// Read-only pointer to a syntax tree node that never changes.
    pub syn: *const syn::ItemUse,

    /// Pointer to a specific item in the [`ItemUse::tree`].
    ///
    /// For instance, if [`Self::syn`] points to 'use a::{b, c, \*}', this field
    /// points to 'b', 'c', or '\*'.
    pub detail: SynId,

    /// Refer to the [module documentation](self).
    pub vis_node: Option<NodeIndex>,
    pub npath: NamedPath,
    pub dst_node: Option<NodeIndex>,
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

    fn write_debug<T: fmt::Write>(&self, _: &PathTree, f: &mut T) -> fmt::Result {
        write!(
            f,
            "UseRaw {{ syn: {:?}, detail: {:?}, vis_node: {:?}, npath: {:?}, dst_node: {:?} }}",
            self.syn, self.detail, self.vis_node, self.npath, self.dst_node
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RefTypeId(usize);

// === RefType ===

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RefType {
    Path(PathId),
    Tuple(Vec<RefTypeId>),
    Array { elem: RefTypeId },
    Slice { elem: RefTypeId },
    Reference { elem: RefTypeId, mut_: bool },
    InferredInt,
    InferredFloat,
    Default,
}

impl RefType {
    pub(crate) fn write_debug<T: fmt::Write>(&self, ptree: &PathTree, f: &mut T) -> fmt::Result {
        match self {
            Self::Path(pid) => write!(f, "{pid:?}"),
            Self::Tuple(tuple) => {
                write!(f, "Tuple(")?;
                for (i, elem) in tuple.iter().enumerate() {
                    ptree.get_type(*elem).write_debug(ptree, f)?;
                    if i < tuple.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            Self::Array { elem } => {
                write!(f, "Array {{ elem: ")?;
                ptree.get_type(*elem).write_debug(ptree, f)?;
                write!(f, " }}")
            }
            Self::Slice { elem } => {
                write!(f, "Slice {{ elem: ")?;
                ptree.get_type(*elem).write_debug(ptree, f)?;
                write!(f, " }}")
            }
            Self::Reference { elem, mut_ } => {
                if *mut_ {
                    write!(f, "&mut ")?;
                } else {
                    write!(f, "& ")?;
                }
                ptree.get_type(*elem).write_debug(ptree, f)?;
                write!(f, " }}")
            }
            Self::InferredInt => write!(f, "InferredInt"),
            Self::InferredFloat => write!(f, "InferredFloat"),
            Self::Default => write!(f, "Default"),
        }
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

impl ops::Deref for NamedPath {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ops::DerefMut for NamedPath {
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

#[derive(Clone, Copy)]
pub struct Container {
    kind: ContainerKind,
    id: PathId,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ContainerKind {
    Root,
    Mod,
    Block,
    Fn,
    Struct,
}
