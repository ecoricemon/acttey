use super::{path_tree::*, queue::*};
use crate::{
    Result,
    front::{
        syntax::{
            common::{AttributeHelper, IdentifySyn, SynId},
            file::SmFile,
        },
        util,
    },
};
use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    fmt::{self, Write},
    iter,
    path::{Path as StdPath, PathBuf},
    pin::Pin,
    ptr,
    rc::Rc,
};

macro_rules! err {
    ($($t:tt)*) => {
        Err(
            format!($($t)*).into()
        )
    };
}

pub struct PathProcessor {
    /// Syntax tree nodes never change after they are constructed.
    pub stree: HashMap<PathBuf, Pin<Box<SmFile>>>,
    pub ptree: PathTree,
    pub s2p: SynToPath,
    tasks: TaskQueue<PathTask>,
}

impl PathProcessor {
    pub fn new(entry: &StdPath) -> Result<Self> {
        let mut ptree = PathTree::new();

        // Inserts 'crate' as a module to be distinguished from externs.
        let dummy = PathValue::Mod(PathMod {
            syn: ptr::null(),
            syn_file: None,
            vis_scope: PathTree::ROOT,
            fpath: entry.to_path_buf(),
            mod_rs: false,
        });
        let pid = ptree.append_value(PathTree::ROOT, iter::once("crate"), dummy);
        let node = ptree.get_mut_value(pid);
        node.as_mod_mut().vis_scope = pid.ni;

        Ok(Self {
            stree: HashMap::new(),
            ptree,
            s2p: SynToPath::new(),
            tasks: TaskQueue::new(),
        })
    }

    pub fn import<I, II>(&mut self, fpaths: I) -> Result<()>
    where
        I: Iterator<Item = II>,
        II: AsRef<StdPath>,
    {
        for fpath in fpaths {
            let fpath = fpath.as_ref();
            let npath = NamedPath::from_path(&fpath)?;
            self.tasks.push_front(PathTask::Read {
                fpath: fpath.to_path_buf(),
                npath,
            });
        }
        Ok(())
    }

    pub fn process(&mut self) -> Result<()> {
        while let Some(task) = self.tasks.pop() {
            let task = task.into_value();
            util::assert_finite_loop(task.id(), "constructing path tree");

            match task {
                PathTask::Read { fpath, npath } => self.read_file(&fpath, npath)?,
                PathTask::Parse { fpath, npath } => self.parse_file(fpath, npath)?,
                PathTask::JoinPathTree { fpath, npath } => self.join_path_tree(&fpath, npath)?,
                PathTask::Resolve(pid) => self.resolve(pid)?,
                PathTask::ResolvePath(data) => self.resolve_path(data),
            }
        }

        println!("@@@ === constructed path tree ===");
        println!("{:?}", self.ptree);
        Ok(())
    }

    fn read_file<P>(&mut self, fpath: P, npath: NamedPath) -> Result<()>
    where
        P: AsRef<StdPath>,
    {
        let fpath = if fpath.as_ref().is_absolute() {
            fpath.as_ref().to_path_buf()
        } else {
            util::to_absolute_path(fpath)?
        };
        self.tasks.push_front(PathTask::Parse { fpath, npath });
        Ok(())
    }

    fn parse_file(&mut self, fpath: PathBuf, npath: NamedPath) -> Result<()> {
        if self.stree.contains_key(&fpath) {
            return Ok(());
        }

        let file = SmFile::from_path(fpath.clone())?;
        self.stree.insert(fpath.clone(), file);
        self.tasks
            .push_front(PathTask::JoinPathTree { fpath, npath });
        Ok(())
    }

    fn join_path_tree<P>(&mut self, fpath: P, npath: NamedPath) -> Result<()>
    where
        P: AsRef<StdPath>,
    {
        let mut cx = PathCx {
            ptree: &mut self.ptree,
            tasks: &mut self.tasks,
            s2p: &mut self.s2p,
            path: npath,
        };
        let file = self.stree.get(fpath.as_ref()).unwrap();
        file_to_path_tree(file, &mut cx)?;
        Ok(())
    }

    fn resolve(&mut self, pid: PathId) -> Result<()> {
        match self.ptree.get_value(pid) {
            PathValue::ConstRaw(_) => self.resolve_const(pid),
            PathValue::FnRaw(_) => self.resolve_fn(pid),
            PathValue::ModRaw(_) => self.resolve_module(pid),
            PathValue::StructRaw(_) => self.resolve_struct(pid),
            PathValue::FieldRaw(_) => self.resolve_field(pid),
            PathValue::TypeAliasRaw(_) => self.resolve_type_alias(pid),
            PathValue::UseRaw(_) => self.resolve_import(pid),
            _ => panic!("unsupported resolve: {}", self.ptree.named_path(pid.ni)),
        }
    }

    fn resolve_const(&mut self, input: PathId) -> Result<()> {
        // Resolves visibility.
        let value = self.ptree.get_value(input).as_const_raw();
        let vis = value.visibility();
        let Some(vis_scope) = self.resolve_visibility(input.ni, &vis) else {
            self.tasks.push_back(PathTask::Resolve(input));
            return Ok(());
        };

        // Resolves the type of this constant.
        let syn = self.ptree.get_value(input).as_const_raw().as_syn();
        resolve_ref_type(
            &self.ptree,
            &mut self.tasks,
            input,
            &syn.ty,
            move |ty_pid, value| {
                value.resolve_const(vis_scope, ty_pid);
            },
        )
    }

    fn resolve_fn(&mut self, input: PathId) -> Result<()> {
        // Resolves visibility.
        let value = self.ptree.get_value(input).as_fn_raw();
        let vis = value.visibility();
        let Some(vis_scope) = self.resolve_visibility(input.ni, &vis) else {
            self.tasks.push_back(PathTask::Resolve(input));
            return Ok(());
        };
        self.ptree.get_mut_value(input).resolve_fn(vis_scope);

        // Resolves the types of input and output.
        let syn = self.ptree.get_value(input).as_fn().as_syn();
        let mod_ni = self.ptree.nearest_module(input.ni).ni;
        for arg in &syn.sig.inputs {
            match arg {
                syn::FnArg::Receiver(_v) => {
                    todo!("resolve_fn() got receiver")
                }
                syn::FnArg::Typed(syn::PatType { ty, .. }) => {
                    resolve_ref_type_as_extern(&mut self.tasks, mod_ni, ty)?;
                }
            }
        }
        if let syn::ReturnType::Type(_, ty) = &syn.sig.output {
            resolve_ref_type_as_extern(&mut self.tasks, mod_ni, ty)?;
        }

        Ok(())
    }

    fn resolve_module(&mut self, input: PathId) -> Result<()> {
        // Resolves visibility.
        let value = self.ptree.get_value(input).as_mod_raw();
        let vis = value.visibility();
        let Some(vis_scope) = self.resolve_visibility(input.ni, &vis) else {
            self.tasks.push_back(PathTask::Resolve(input));
            return Ok(());
        };
        self.ptree.get_mut_value(input).resolve_mod(vis_scope);

        // Schedules 'Read' task if needed.
        let value = self.ptree.get_value(input).as_mod();
        if !value.is_inline() && !self.stree.contains_key(&value.fpath) {
            let npath = self.ptree.named_path(input.ni);
            self.tasks.push_front(PathTask::Read {
                fpath: value.fpath.clone(),
                npath,
            });
        }

        Ok(())
    }

    fn resolve_struct(&mut self, input: PathId) -> Result<()> {
        // Resolves visibility.
        let value = self.ptree.get_value(input).as_struct_raw();
        let vis = value.visibility();
        let Some(vis_scope) = self.resolve_visibility(input.ni, &vis) else {
            self.tasks.push_back(PathTask::Resolve(input));
            return Ok(());
        };
        self.ptree.get_mut_value(input).resolve_struct(vis_scope);
        Ok(())
    }

    fn resolve_field(&mut self, input: PathId) -> Result<()> {
        // Resolves the type of this field.
        let syn = self.ptree.get_value(input).as_field_raw().as_syn();
        resolve_ref_type(
            &self.ptree,
            &mut self.tasks,
            input,
            &syn.ty,
            |ty_pid, value| {
                value.resolve_field(ty_pid);
            },
        )
    }

    fn resolve_type_alias(&mut self, input: PathId) -> Result<()> {
        // Resolves visibility.
        let value = self.ptree.get_value(input).as_type_alias_raw();
        let vis = value.visibility();
        let Some(vis_scope) = self.resolve_visibility(input.ni, &vis) else {
            self.tasks.push_back(PathTask::Resolve(input));
            return Ok(());
        };
        self.ptree
            .get_mut_value(input)
            .resolve_type_alias(vis_scope);
        Ok(())
    }

    fn resolve_import(&mut self, input: PathId) -> Result<()> {
        // Resolves visibility.
        let value = self.ptree.get_value(input).as_use_raw();
        let vis = value.visibility();
        let ext = &value.npath;
        let Some(vis_scope) = self.resolve_visibility(input.ni, &vis) else {
            self.tasks.push_back(PathTask::Resolve(input));
            return Ok(());
        };

        let on_success = move |this: &mut Self, data: &ResolvePathData| {
            let value = this.ptree.get_mut_value(input);
            value.resolve_use(vis_scope, data.best);
            true
        };

        let on_fail = move |this: &mut Self, data: &ResolvePathData| {
            let dst = if matches!(data.ext.segments().nth(data.ext_off),
                Some(segment) if segment == "*"
            ) {
                let ext = data.ext.segments().take(data.ext_off);
                this.ptree
                    .traverse_from(data.base, ext, |pid| Some(pid))
                    .unwrap()
            } else {
                data.join_path_tree_as_extern(&mut this.ptree, &mut this.s2p)
            };

            let value = this.ptree.get_mut_value(input);
            value.resolve_use(vis_scope, dst.ni);
        };

        let base = self.ptree.parent_module(input.ni).ni;
        let sid = value.detail;
        let data = ResolvePathData::new(base, ext.clone(), sid, on_success, on_fail);

        self.tasks.push_front(PathTask::ResolvePath(data));
        Ok(())
    }

    fn resolve_path(&mut self, mut data: ResolvePathData) {
        // Returns if the data has been expired. The data gets expired when its
        // handler function is called.
        if data.expired.get() {
            return;
        }

        // Normalizes the given path.
        let mut base = data.best;
        for segment in data.ext.segments().skip(data.ext_off) {
            if segment == "crate" {
                base = self.ptree.search(iter::once("crate")).unwrap();
                data.ext_off += 1;
            } else if segment == "super" {
                base = self.ptree.parent_module(base).ni;
                data.ext_off += 1;
            } else {
                break;
            }
        }

        let prv = data.best;
        let key = data.ext.segments().skip(data.ext_off);
        match self.ptree.search_best_from(base, key) {
            Ok((dst, advance)) => {
                data.best = dst;
                data.ext_off += advance;

                jump_then(self, dst, data, |this, data| {
                    if (*data.on_success.borrow_mut())(this, &data) {
                        data.expired.set(true);
                    }
                });
            }
            Err((mid, advance)) if mid != prv => {
                // Couldn't reach, but there was progress. Retry.
                data.best = mid;
                data.ext_off += advance;
                self.tasks.push_back(PathTask::ResolvePath(data));
            }
            Err((mid, advance)) => {
                data.best = mid;
                data.ext_off += advance;

                jump_then(self, mid, data, |this, data| {
                    (*data.on_fail.borrow_mut())(this, &data);
                    data.expired.set(true);
                });
            }
        }

        // === Internal helper functions ===

        fn jump_then<F>(this: &mut PathProcessor, ni: NodeIndex, data: ResolvePathData, then: F)
        where
            F: FnOnce(&mut PathProcessor, ResolvePathData),
        {
            // Reschedules if unresolved import found.
            let values = this
                .ptree
                .get_values(ni)
                .iter()
                .chain(this.ptree.get_glob_values(ni));
            if values.clone().any(|v| matches!(v, PathValue::UseRaw(_))) {
                this.tasks.push_back(PathTask::ResolvePath(data));
                return;
            }

            let latter = this.tasks.push_back(PathTask::ResolvePath(data.clone()));

            // Schedules child tasks for the 'use' jumps.
            let prv_len = this.tasks.len();
            for value in values {
                if let PathValue::Use(PathUse { dst, .. }) = value {
                    let new_base = *dst;
                    let new_best = *dst;
                    let cloned = data.make_clone(new_base, new_best, data.ext_off);
                    this.tasks
                        .push_front_with_dependency(PathTask::ResolvePath(cloned), latter);
                }
            }

            // Not being able to jump means we reached an end. Calls 'then'
            // function.
            if this.tasks.len() == prv_len {
                then(this, data);
            }
        }
    }

    fn resolve_visibility(&self, ni: NodeIndex, vis: &PathVis) -> Option<NodeIndex> {
        match vis {
            PathVis::Pub => Some(PathTree::ROOT),
            PathVis::PubCrate => self.ptree.search(iter::once("crate")),
            PathVis::PubSuper => Some(self.ptree.parent_module(ni).ni),
            PathVis::PubPath(path) => self.ptree.search(path.segments()),
            PathVis::Private => Some(self.ptree.nearest_module(ni).ni),
        }
    }
}

#[derive(Debug)]
pub struct SynToPath(HashMap<SynId, PathId>);

impl SynToPath {
    fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn to_path_id(&self, sid: SynId) -> PathId {
        self.get_path_id(sid)
            .expect(&format!("couldn't find PathId for SynId `{sid:?}`"))
    }

    fn add_syn_to_path(&mut self, sid: SynId, pid: PathId) {
        if let Some(old_pid) = self.0.insert(sid, pid) {
            if old_pid != pid {
                panic!(
                    "Id conflicts: sid: {}({}), pid: {}, old_pid: {}",
                    sid,
                    sid.content(),
                    pid,
                    old_pid,
                );
            }
        }
    }

    fn get_path_id(&self, sid: SynId) -> Option<PathId> {
        self.0.get(&sid).cloned()
    }
}

#[derive(Debug)]
enum PathTask {
    Read { fpath: PathBuf, npath: NamedPath },
    Parse { fpath: PathBuf, npath: NamedPath },
    JoinPathTree { fpath: PathBuf, npath: NamedPath },
    Resolve(PathId),
    ResolvePath(ResolvePathData),
}

impl PathTask {
    fn id(&self) -> PathTaskId {
        match self {
            Self::Read { fpath, npath } => PathTaskId::Read {
                fpath: fpath.clone(),
                npath: npath.clone(),
            },
            Self::Parse { fpath, npath } => PathTaskId::Parse {
                fpath: fpath.clone(),
                npath: npath.clone(),
            },
            Self::JoinPathTree { fpath, npath } => PathTaskId::JoinPathTree {
                fpath: fpath.clone(),
                npath: npath.clone(),
            },
            Self::Resolve(pid) => PathTaskId::Resolve(*pid),
            Self::ResolvePath(data) => PathTaskId::ResolvePath {
                base: data.base,
                ext: data.ext.clone(),
                best: data.best,
                ext_off: data.ext_off,
                sid: data.sid,
                on_success: data.on_success.as_ptr().cast_const().cast::<()>(),
                on_fail: data.on_fail.as_ptr().cast_const().cast::<()>(),
                expired: data.expired.as_ptr().cast_const().cast::<()>(),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum PathTaskId {
    Read {
        fpath: PathBuf,
        npath: NamedPath,
    },
    Parse {
        fpath: PathBuf,
        npath: NamedPath,
    },
    JoinPathTree {
        fpath: PathBuf,
        npath: NamedPath,
    },
    Resolve(PathId),
    ResolvePath {
        base: NodeIndex,
        ext: NamedPath,
        best: NodeIndex,
        ext_off: usize,
        sid: SynId,
        on_success: *const (),
        on_fail: *const (),
        expired: *const (),
    },
}

/// ```ignore
/// mod a {
///     mod b {
///         // resolve c::C
///     }
/// }
/// ```
#[derive(Clone)]
struct ResolvePathData {
    /// Node index to "a::b".
    base: NodeIndex,
    /// "c::C".
    ext: NamedPath,
    /// Node index of the nearest node to the "a::b::c::C" that we've found
    /// until now.
    best: NodeIndex,
    /// Number of segments of `ext` to get the `best` from `base`.
    ext_off: usize,
    /// Id of a corresponding syntax tree node.
    sid: SynId,
    /// Handler that will be called when the resolve process succeeds.
    on_success: Rc<RefCell<dyn FnMut(&mut PathProcessor, &ResolvePathData) -> bool>>,
    /// Handler that will be called when the resolve process fails.
    on_fail: Rc<RefCell<dyn FnMut(&mut PathProcessor, &ResolvePathData)>>,
    /// Whether the resolve process completed or not.
    expired: Rc<Cell<bool>>,
}

impl ResolvePathData {
    fn new<S, F>(base: NodeIndex, ext: NamedPath, sid: SynId, on_success: S, on_fail: F) -> Self
    where
        S: FnMut(&mut PathProcessor, &ResolvePathData) -> bool + 'static,
        F: FnMut(&mut PathProcessor, &ResolvePathData) + 'static,
    {
        Self {
            base,
            ext,
            best: base,
            ext_off: 0,
            sid,
            on_success: Rc::new(RefCell::new(on_success)),
            on_fail: Rc::new(RefCell::new(on_fail)),
            expired: Rc::new(Cell::new(false)),
        }
    }

    fn make_clone(&self, base: NodeIndex, best: NodeIndex, ext_off: usize) -> Self {
        Self {
            base,
            ext: self.ext.clone(),
            best,
            ext_off,
            sid: self.sid,
            on_success: Rc::clone(&self.on_success),
            on_fail: Rc::clone(&self.on_fail),
            expired: Rc::clone(&self.expired),
        }
    }

    fn join_path_tree_as_extern(&self, ptree: &mut PathTree, s2p: &mut SynToPath) -> PathId {
        // `ext_off > 0` means that front part of `ext` is included to the
        // `best`. Therefore, `ext` must be a descendant of the `best`.
        let from = if self.ext_off > 0 {
            self.best
        }
        // `ext_off == 0` means that `ext` is totally different one from the
        // `base`. Therefore, `ext` must start from the root node.
        else {
            PathTree::ROOT
        };

        // Appends new extern if no existing entry found.
        let pid = ptree.insert_node_then(
            from,
            self.ext.segments().skip(self.ext_off),
            |ni, values| {
                for (vi, value) in values.iter().enumerate() {
                    if matches!(value, PathValue::Extern(_)) {
                        return PathId::new(ni, vi);
                    }
                }
                values.push(PathValue::Extern(PathExtern { sid: self.sid }));
                PathId::new(ni, values.len() - 1)
            },
        );

        s2p.add_syn_to_path(self.sid, pid);

        pid
    }

    fn verbose<'a, 'b>(&'a self, ptree: &'b PathTree) -> VerboseResolvePathData<'a, 'b> {
        VerboseResolvePathData { data: self, ptree }
    }
}

impl fmt::Debug for ResolvePathData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ResolvePathData")
            .field("base", &self.base)
            .field("ext", &self.ext.as_str())
            .field("best", &self.best)
            .field("ext_off", &self.ext_off)
            .field("sid", &self.sid)
            .field("expired", &self.expired.get())
            .finish_non_exhaustive()
    }
}

struct VerboseResolvePathData<'a, 'b> {
    data: &'a ResolvePathData,
    ptree: &'b PathTree,
}

impl fmt::Debug for VerboseResolvePathData<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let base = self.ptree.named_path(self.data.base);
        let best = self.ptree.named_path(self.data.best);
        let syn_content = self.data.sid.content();

        f.debug_struct("ResolvePathData")
            .field("base", &base.as_str())
            .field("ext", &self.data.ext.as_str())
            .field("best", &best.as_str())
            .field("ext_off", &self.data.ext_off)
            .field("sid", &syn_content)
            .field("expired", &self.data.expired.get())
            .finish_non_exhaustive()
    }
}

pub struct PathCx<'a> {
    ptree: &'a mut PathTree,
    tasks: &'a mut TaskQueue<PathTask>,
    s2p: &'a mut SynToPath,
    path: NamedPath,
}

fn file_to_path_tree(file: &SmFile, cx: &mut PathCx) -> Result<()> {
    if !matches!(file.abs_path.extension(), Some(ext) if ext == "rs") {
        return Err("not a rust file".into());
    }

    let sid = file.syn_id();
    let key = cx.path.segments();
    let ni = cx.ptree.search(key.clone());
    let values = cx.ptree.search_mut_values(key.clone());

    // If the file was loaded by 'mod' from another file, we just set the
    // file pointer.
    for (vi, value) in values.iter_mut().enumerate() {
        match value {
            PathValue::Mod(v) => {
                v.syn_file = Some(file as *const _);
                cx.s2p.add_syn_to_path(sid, ni.unwrap().to_path_id(vi));
            }
            PathValue::ModRaw(v) => {
                v.syn_file = Some(file as *const _);
                cx.s2p.add_syn_to_path(sid, ni.unwrap().to_path_id(vi));
            }
            _ => {}
        }
    }

    // If the file was loaded as entry file, we need to append new module
    // for this file.
    if values.is_empty() {
        let value = PathValue::ModRaw(PathModRaw {
            syn: ptr::null(),
            syn_file: Some(file as *const _),
            fpath: file.abs_path.clone(),
            mod_rs: util::is_mod_rs(&file.abs_path),
        });
        let pid = cx.ptree.append_value(PathTree::ROOT, key, value);
        cx.s2p.add_syn_to_path(sid, pid);
        cx.tasks.push_front(PathTask::Resolve(pid));
    }

    // Regardless of how the file was loaded, inserts items in the path
    // tree.
    for item in &file.file.items {
        item_to_path_tree(item, cx)?;
    }

    Ok(())
}

fn item_to_path_tree(item: &syn::Item, cx: &mut PathCx) -> Result<()> {
    // Tracks only when the path has been initialzied by tracking a file.
    if cx.path.is_empty() {
        return Ok(());
    }

    // Items that make 'new' paths need to be inserted to the path tree.
    match item {
        syn::Item::Const(v) => item_const_to_path_tree(v, cx),
        syn::Item::Enum(_) => Ok(()),
        syn::Item::ExternCrate(_) => Ok(()),
        syn::Item::Fn(v) => item_fn_to_path_tree(v, cx),
        syn::Item::ForeignMod(_) => Ok(()),
        syn::Item::Impl(_) => Ok(()),
        syn::Item::Macro(_) => Ok(()),
        syn::Item::Mod(v) => item_mod_to_path_tree(v, cx),
        syn::Item::Static(_) => Ok(()),
        syn::Item::Struct(v) => item_struct_to_path_tree(v, cx),
        syn::Item::Trait(_) => Ok(()),
        syn::Item::TraitAlias(_) => Ok(()),
        syn::Item::Type(v) => item_type_to_path_tree(v, cx),
        syn::Item::Union(_) => Ok(()),
        syn::Item::Use(v) => item_use_to_path_tree(v, cx),
        syn::Item::Verbatim(_) => Ok(()),
        _ => Ok(()), // Non-exhaustive
    }
}

fn item_const_to_path_tree(item_const: &syn::ItemConst, cx: &mut PathCx) -> Result<()> {
    let org_len = cx.path.len();
    util::push_colon_path(&mut cx.path, &item_const.ident);

    let key = cx.path.segments();
    let pid = cx.ptree.append_value(
        PathTree::ROOT,
        key,
        PathValue::ConstRaw(PathConstRaw {
            syn: item_const as *const _,
        }),
    );
    let sid = item_const.syn_id();
    cx.s2p.add_syn_to_path(sid, pid);
    cx.tasks.push_front(PathTask::Resolve(pid));

    cx.path.truncate(org_len);
    Ok(())
}

fn item_mod_to_path_tree(item_mod: &syn::ItemMod, cx: &mut PathCx) -> Result<()> {
    // Terminology
    // * inline - Module is inline when it contains its content within {}.
    // * fpath - File path of the module.
    //   - "/home/a.rs" if non-inline module.
    //   - "/home/a" if inline module.
    // * mod_rs - Whether the module is a `mod-rs` or not.
    //   - `mod-rs` if the file is one of "mod.rs", "main.rs", or "lib.rs".
    //   - `mod-rs` if the file is determined by "path" attribute, plus the
    //     module is inline (e.g. #[path = "a.rs"] mod foo;)
    //   - `non-mod-rs` otherwise.

    // Retrieves parent module info.
    let ni = cx.ptree.search(cx.path.segments()).unwrap();
    let parent_pid = cx.ptree.nearest_module(ni);
    let (parent_fpath, parent_mod_rs) = match cx.ptree.get_value(parent_pid) {
        PathValue::Mod(PathMod { fpath, mod_rs, .. }) => (fpath, *mod_rs),
        PathValue::ModRaw(PathModRaw { fpath, mod_rs, .. }) => (fpath, *mod_rs),
        _ => unreachable!(),
    };

    // Directory containing a file that declares this module with or without
    // content.
    let dir = parent_fpath.parent().unwrap();

    let inline = item_mod.content.is_some();

    // Determines this module's file path and "mod-rs".
    let fpath;
    let mod_rs;
    if let Some(path_attr) = item_mod.get_attribute_value("path") {
        fpath = dir.join(path_attr.trim_matches('"'));
        mod_rs = !inline;
    } else {
        let base_buf;
        let base = if parent_mod_rs {
            dir
        } else {
            base_buf = parent_fpath.with_extension("");
            base_buf.as_ref()
        };

        if inline {
            fpath = base.join(format!("{}", item_mod.ident));
            mod_rs = false;
        } else {
            let fpath_a = base.join(format!("{}.rs", item_mod.ident));
            let fpath_b = base.join(format!("{}/mod.rs", item_mod.ident));
            match (fpath_a.exists(), fpath_b.exists()) {
                (true, false) => {
                    fpath = fpath_a;
                    mod_rs = false;
                }
                (false, true) => {
                    fpath = fpath_b;
                    mod_rs = true;
                }
                (true, true) => {
                    return err!("`{fpath_a:?}` and `{fpath_b:?}` conflict.");
                }
                (false, false) => {
                    return err!("expected `{fpath_a:?}` or `{fpath_b:?}`");
                }
            }
        }
    };

    // Enters the scope.
    let org_len = cx.path.len();
    util::push_colon_path(&mut cx.path, &item_mod.ident);

    let key = cx.path.segments();
    let pid = cx.ptree.append_value(
        PathTree::ROOT,
        key,
        PathValue::ModRaw(PathModRaw {
            syn: item_mod as *const _,
            syn_file: None,
            fpath,
            mod_rs,
        }),
    );
    let sid = item_mod.syn_id();
    cx.s2p.add_syn_to_path(sid, pid);
    cx.tasks.push_front(PathTask::Resolve(pid));

    // Internal items.
    if let Some((_, items)) = &item_mod.content {
        for item in items.iter() {
            item_to_path_tree(item, cx)?;
        }
    }

    // Exits the scope.
    cx.path.truncate(org_len);
    Ok(())
}

fn item_struct_to_path_tree(item_struct: &syn::ItemStruct, cx: &mut PathCx) -> Result<()> {
    let org_len = cx.path.len();
    util::push_colon_path(&mut cx.path, &item_struct.ident);

    let key = cx.path.segments();
    let pid = cx.ptree.append_value(
        PathTree::ROOT,
        key,
        PathValue::StructRaw(PathStructRaw {
            syn: item_struct as *const _,
        }),
    );
    let sid = item_struct.syn_id();
    cx.s2p.add_syn_to_path(sid, pid);
    cx.tasks.push_front(PathTask::Resolve(pid));

    // Fields
    for (nth, field) in item_struct.fields.iter().enumerate() {
        field_to_path_tree(field, cx, nth as u32)?;
    }

    cx.path.truncate(org_len);
    Ok(())
}

fn field_to_path_tree(field: &syn::Field, cx: &mut PathCx, nth: u32) -> Result<()> {
    let org_len = cx.path.len();
    if let Some(ident) = &field.ident {
        util::push_colon_path(&mut cx.path, ident);
    } else {
        util::push_colon_path(&mut cx.path, nth);
    }

    let key = cx.path.segments();
    let pid = cx.ptree.append_value(
        PathTree::ROOT,
        key,
        PathValue::FieldRaw(PathFieldRaw {
            syn: field as *const _,
        }),
    );
    let sid = field.syn_id();
    cx.s2p.add_syn_to_path(sid, pid);
    cx.tasks.push_front(PathTask::Resolve(pid));

    cx.path.truncate(org_len);
    Ok(())
}

fn item_fn_to_path_tree(item_fn: &syn::ItemFn, cx: &mut PathCx) -> Result<()> {
    let org_len = cx.path.len();
    util::push_colon_path(&mut cx.path, &item_fn.sig.ident);

    let key = cx.path.segments();
    let pid = cx.ptree.append_value(
        PathTree::ROOT,
        key,
        PathValue::FnRaw(PathFnRaw {
            syn: item_fn as *const _,
        }),
    );
    let sid = item_fn.syn_id();
    cx.s2p.add_syn_to_path(sid, pid);
    cx.tasks.push_front(PathTask::Resolve(pid));

    // Block
    block_to_path_tree(&item_fn.block, cx, BlockState { nth: 0 })?;

    cx.path.truncate(org_len);
    Ok(())
}

fn block_to_path_tree(block: &syn::Block, cx: &mut PathCx, mut state: BlockState) -> Result<()> {
    let org_len = cx.path.len();
    util::push_colon_path(&mut cx.path, format!("{{{}}}", state.nth));
    state.nth += 1;

    let key = cx.path.segments();
    let pid = cx.ptree.append_value(
        PathTree::ROOT,
        key,
        PathValue::Block(PathBlock {
            syn: block as *const _,
        }),
    );
    let sid = block.syn_id();
    cx.s2p.add_syn_to_path(sid, pid);

    // Statements
    for stmt in &block.stmts {
        stmt_to_path_tree(stmt, cx, state)?;
    }

    cx.path.truncate(org_len);
    Ok(())
}

fn stmt_to_path_tree(stmt: &syn::Stmt, cx: &mut PathCx, state: BlockState) -> Result<()> {
    match stmt {
        syn::Stmt::Local(v) => local_to_path_tree(v, cx),
        syn::Stmt::Item(v) => item_to_path_tree(v, cx),
        syn::Stmt::Expr(v, _) => expr_to_path_tree(v, cx, state),
        syn::Stmt::Macro(..) => Ok(()),
    }
}

// If this let binding has explicit type, adds the type to the path tree.
fn local_to_path_tree(local: &syn::Local, cx: &mut PathCx) -> Result<()> {
    let syn::Pat::Type(syn::PatType { ty, .. }) = &local.pat else {
        return Ok(());
    };

    let ni = cx.ptree.search(cx.path.segments()).unwrap();
    let mod_ni = cx.ptree.nearest_module(ni).ni;
    resolve_ref_type_as_extern(&mut cx.tasks, mod_ni, ty)
}

fn expr_to_path_tree(expr: &syn::Expr, cx: &mut PathCx, state: BlockState) -> Result<()> {
    match expr {
        syn::Expr::Block(syn::ExprBlock { block, .. }) => block_to_path_tree(block, cx, state),
        _ => Ok(()),
    }
}

fn item_type_to_path_tree(item_type: &syn::ItemType, cx: &mut PathCx) -> Result<()> {
    let org_len = cx.path.len();
    util::push_colon_path(&mut cx.path, &item_type.ident);

    let key = cx.path.segments();
    let pid = cx.ptree.append_value(
        PathTree::ROOT,
        key,
        PathValue::TypeAliasRaw(PathTypeAliasRaw {
            syn: item_type as *const _,
        }),
    );
    let sid = item_type.syn_id();
    cx.s2p.add_syn_to_path(sid, pid);
    cx.tasks.push_front(PathTask::Resolve(pid));

    cx.path.truncate(org_len);
    Ok(())
}

fn item_use_to_path_tree(item_use: &syn::ItemUse, cx: &mut PathCx) -> Result<()> {
    fn dfs(
        item: &syn::ItemUse,
        node: &syn::UseTree,
        vis: PathVis,
        buf: &mut String,
        cx: &mut PathCx,
    ) {
        match node {
            syn::UseTree::Path(v) => {
                let org_len = buf.len();
                write!(buf, "{}::", v.ident).unwrap();
                dfs(item, &v.tree, vis, buf, cx);
                buf.truncate(org_len);
            }
            syn::UseTree::Name(v) => {
                let org_len = cx.path.len();
                util::push_colon_path(&mut cx.path, &v.ident);

                let detail = v.syn_id();
                add_path_value(cx, item, detail, format!("{buf}{}", v.ident));

                cx.path.truncate(org_len);
            }
            syn::UseTree::Rename(v) => {
                let org_len = cx.path.len();
                util::push_colon_path(&mut cx.path, &v.rename);

                let detail = v.syn_id();
                add_path_value(cx, item, detail, format!("{buf}{}", v.ident));

                cx.path.truncate(org_len);
            }
            syn::UseTree::Glob(v) => {
                let org_len = cx.path.len();
                cx.path.push_str("::*");

                let detail = v.syn_id();
                add_path_value(cx, item, detail, format!("{buf}*"));

                cx.path.truncate(org_len);
            }
            syn::UseTree::Group(v) => {
                for node in &v.items {
                    dfs(item, node, vis.clone(), buf, cx);
                }
            }
        }
    }

    fn add_path_value(cx: &mut PathCx, item: &syn::ItemUse, detail: SynId, npath: String) {
        let key = cx.path.segments();
        let pid = cx.ptree.append_value(
            PathTree::ROOT,
            key,
            PathValue::UseRaw(PathUseRaw {
                syn: item as *const _,
                detail,
                npath: npath.into(),
            }),
        );
        cx.s2p.add_syn_to_path(detail, pid);
        cx.tasks.push_front(PathTask::Resolve(pid));
    }

    let mut buf = String::new();
    dfs(
        item_use,
        &item_use.tree,
        PathVis::new(&item_use.vis),
        &mut buf,
        cx,
    );
    Ok(())
}

/// Resolves a reference type like `const C: ty` and field `a: ty`.
///
/// * ty - Type that is needed to be resolved.
/// * resolve - Resolve function that receives path id of the resolved type
///   and path value of the `input`. If the given type is array or slice,
///   element type of the array or slice is resolved.
fn resolve_ref_type<F>(
    ptree: &PathTree,
    tasks: &mut TaskQueue<PathTask>,
    input: PathId,
    ty: &syn::Type,
    mut resolve: F,
) -> Result<()>
where
    F: FnMut(PathId, &mut PathValue) + Clone + 'static,
{
    let mut c_resolve = resolve.clone();

    let on_success = move |this: &mut PathProcessor, data: &ResolvePathData| {
        let mut vi = None;
        let mut need_retry = false;
        for (i, v) in this
            .ptree
            .get_values(data.best)
            .iter()
            .enumerate()
            .filter(|(_, v)| v.is_type())
        {
            if let Some(found_ni) = v.vis_scope() {
                if this.ptree.is_descendant(input.ni, found_ni) {
                    vi = Some(i);
                    break;
                }
            } else {
                need_retry = true;
            }
        }

        if let Some(vi) = vi {
            let elem_ty_pid = data.best.to_path_id(vi);
            this.s2p.add_syn_to_path(data.sid, elem_ty_pid);
            let value = this.ptree.get_mut_value(input);
            c_resolve(elem_ty_pid, value);
            true
        } else if need_retry {
            this.tasks.push_back(PathTask::Resolve(input));
            true
        } else {
            false
        }
    };

    // If failed to resolve the type, assumes it as an extern.
    let on_fail = move |this: &mut PathProcessor, data: &ResolvePathData| {
        let elem_ty_pid = data.join_path_tree_as_extern(&mut this.ptree, &mut this.s2p);
        let value = this.ptree.get_mut_value(input);
        resolve(elem_ty_pid, value);
    };

    let base = ptree.nearest_module(input.ni).ni;
    let elem = util::element_type_path_string(ty)?;
    let elem_sid = util::element_type(ty)?.syn_id();
    let data = ResolvePathData::new(base, elem.into(), elem_sid, on_success, on_fail);
    tasks.push_front(PathTask::ResolvePath(data));
    Ok(())
}

/// Resolves a reference type like `const C: ty` and field `a: ty` as an extern
/// when the resolve process failed.
///
/// * ty - Type that is needed to be resolved.
fn resolve_ref_type_as_extern(
    tasks: &mut TaskQueue<PathTask>,
    mod_ni: NodeIndex,
    ty: &syn::Type,
) -> Result<()> {
    // If failed to resolve the type, assumes it as an extern.
    let on_fail = move |this: &mut PathProcessor, data: &ResolvePathData| {
        data.join_path_tree_as_extern(&mut this.ptree, &mut this.s2p);
    };

    let elem = util::element_type_path_string(ty)?;
    let elem_sid = util::element_type(ty)?.syn_id();
    let data = ResolvePathData::new(mod_ni, elem.into(), elem_sid, |_, _| true, on_fail);
    tasks.push_front(PathTask::ResolvePath(data));
    Ok(())
}

#[derive(Clone, Copy)]
struct BlockState {
    nth: u32,
}
