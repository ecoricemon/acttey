use super::{
    file::{ImportFilePaths, SmFile},
    item::{SmField, SmItem, SmItemConst, SmItemMod, SmItemStruct},
    known::*,
    task::*,
    tree::*,
};
use crate::{
    traits::{IdentifySyn, SmAttributeHelper, SynId},
    util::{self, call_site},
};
use naga::{
    front::Typifier, proc::{Alignment, ConstantEvaluator, Emitter, ExpressionKindTracker, Layouter}, Arena, ArraySize, BinaryOperator, Block, Constant, Expression, FastIndexMap, Function, FunctionArgument, FunctionResult, Handle, Literal, LocalVariable, Module, ScalarKind, Span, Statement, StructMember, Type, TypeInner
};
use quote::ToTokens;
use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    fmt,
    fmt::Write,
    iter,
    num::NonZeroU32,
    path::{Path as StdPath, PathBuf},
    pin::Pin,
    ptr,
    rc::Rc,
    result::Result as StdResult,
};
use syn::{Error, ItemFn, ItemType, ItemUse, Lit, Result, UseTree};

macro_rules! unwrap_tri {
    ($($t:tt)*) => {
        match $($t)* {
            TriResult::Ok(t) => t,
            TriResult::None => return TriResult::None,
            TriResult::Err(e) => return TriResult::Err(e),
        }
    };
}

macro_rules! tri_err {
    ($($t:tt)*) => {
        TriResult::Err(Error::new(call_site(), format!($($t)*)))
    };
}

pub struct SemanticAnalyzer {
    pub path_proc: PathProcessor,

    pub naga_proc: NagaProcessor,
}

impl SemanticAnalyzer {
    pub fn new<P>(entry: P) -> Result<Self>
    where
        P: AsRef<StdPath>,
    {
        Ok(Self {
            path_proc: PathProcessor::new(entry.as_ref())?,
            naga_proc: NagaProcessor::new(),
        })
    }

    pub fn import(&mut self, fpaths: &ImportFilePaths) -> Result<()> {
        self.path_proc.import(fpaths)
    }

    pub fn process(&mut self) -> Result<()> {
        self.path_proc.process()?;
        self.naga_proc.process(&self.path_proc)?;
        Ok(())
    }
}

pub struct PathProcessor {
    /// Syntax tree nodes never change after they are constructed.
    pub stree: HashMap<PathBuf, Pin<Box<SmFile>>>,
    pub ptree: PathTree,
    tasks: TaskQueue<PathTask>,
    s2p: SynToPath,
}

impl PathProcessor {
    fn new(entry: &StdPath) -> Result<Self> {
        let mut ptree = PathTree::new();

        // Inserts 'crate' as a module to be distinguished from externs.
        let vfpath = util::fpath_to_vfpath(entry)?;
        let dummy = PathValue::Mod(PathMod {
            syn: ptr::null(),
            syn_file: None,
            vis_scope: PathTree::ROOT,
            vfpath,
        });
        let pid = ptree.append_value(PathTree::ROOT, iter::once("crate"), dummy);
        let node = ptree.get_mut_value(pid);
        node.as_mod_mut().vis_scope = pid.ni;

        Ok(Self {
            stree: HashMap::new(),
            ptree,
            tasks: TaskQueue::new(),
            s2p: SynToPath::new(),
        })
    }

    fn import(&mut self, fpaths: &ImportFilePaths) -> Result<()> {
        for fpath in fpaths.iter() {
            let npath = NamedPath::from_path(&fpath)?;
            self.tasks.push_front(PathTask::Read { fpath, npath });
        }
        Ok(())
    }

    fn process(&mut self) -> Result<()> {
        while let Some(task) = self.tasks.pop() {
            let task = task.into_value();
            util::assert_finite_loop(task.id(), "constructing path tree");

            match task {
                PathTask::Read { fpath, npath } => self.read_file(&fpath, npath)?,
                PathTask::Parse { fpath, npath } => self.parse_file(fpath, npath)?,
                PathTask::JoinPathTree { fpath, npath } => self.join_path_tree(&fpath, npath)?,
                PathTask::Resolve(pid) => self.resolve(pid),
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
        self.stree.insert(fpath.clone(), Box::pin(file));
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
        file.join_path_tree(&mut cx, ())?;
        Ok(())
    }

    fn resolve(&mut self, pid: PathId) {
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

    fn resolve_const(&mut self, input: PathId) {
        // Resolves visibility.
        let value = self.ptree.get_value(input).as_const_raw();
        let vis = value.visibility();
        let Some(vis_scope) = self.resolve_visibility(input.ni, &vis) else {
            self.tasks.push_back(PathTask::Resolve(input));
            return;
        };

        // Resolves the type of this constant.
        let syn = self.ptree.get_value(input).as_const_raw().as_syn();
        self.resolve_ref_type(input, &syn.ty, move |pid, value| {
            value.resolve_const(vis_scope, pid);
        });
    }

    fn resolve_fn(&mut self, input: PathId) {
        // Resolves visibility.
        let value = self.ptree.get_value(input).as_fn_raw();
        let vis = value.visibility();
        let Some(vis_scope) = self.resolve_visibility(input.ni, &vis) else {
            self.tasks.push_back(PathTask::Resolve(input));
            return;
        };
        self.ptree.get_mut_value(input).resolve_fn(vis_scope);

        // Resolves the types of input and output.
        let syn = self.ptree.get_value(input).as_fn().as_syn();
        for arg in &syn.sig.inputs {
            match arg {
                syn::FnArg::Receiver(v) => {
                    todo!("resolve_fn() got receiver")
                }
                syn::FnArg::Typed(v) => {
                    self.resolve_ref_type(input, &v.ty, |_, _| {});
                }
            }
        }
    }

    fn resolve_module(&mut self, input: PathId) {
        // Resolves visibility.
        let value = self.ptree.get_value(input).as_mod_raw();
        let vis = value.visibility();
        let Some(vis_scope) = self.resolve_visibility(input.ni, &vis) else {
            self.tasks.push_back(PathTask::Resolve(input));
            return;
        };
        self.ptree.get_mut_value(input).resolve_mod(vis_scope);

        // Schedules 'Read' task if needed.
        let m = self.ptree.get_value(input).as_mod();
        if m.is_inline() {
            return;
        }
        let (fpath_a, fpath_b) = util::vfpath_to_fpath(&m.vfpath);
        if self.stree.contains_key(&fpath_a) || self.stree.contains_key(&fpath_b) {
            return;
        }
        let fpath = match (fpath_a.exists(), fpath_b.exists()) {
            (true, false) => fpath_a,
            (false, true) => fpath_b,
            (true, true) => {
                panic!("`{fpath_a:?}` and `{fpath_b:?}` conflict.");
            }
            (false, false) => {
                panic!("expected `{fpath_a:?}` or `{fpath_b:?}`");
            }
        };
        let npath = self.ptree.named_path(input.ni);
        self.tasks.push_front(PathTask::Read { fpath, npath });
    }

    fn resolve_struct(&mut self, input: PathId) {
        // Resolves visibility.
        let value = self.ptree.get_value(input).as_struct_raw();
        let vis = value.visibility();
        let Some(vis_scope) = self.resolve_visibility(input.ni, &vis) else {
            self.tasks.push_back(PathTask::Resolve(input));
            return;
        };
        self.ptree.get_mut_value(input).resolve_struct(vis_scope);
    }

    fn resolve_field(&mut self, input: PathId) {
        // Resolves the type of this field.
        let syn = self.ptree.get_value(input).as_field_raw().as_syn();
        self.resolve_ref_type(input, &syn.ty, |pid, value| {
            value.resolve_field(pid);
        });
    }

    fn resolve_type_alias(&mut self, input: PathId) {
        // Resolves visibility.
        let value = self.ptree.get_value(input).as_type_alias_raw();
        let vis = value.visibility();
        let Some(vis_scope) = self.resolve_visibility(input.ni, &vis) else {
            self.tasks.push_back(PathTask::Resolve(input));
            return;
        };
        self.ptree
            .get_mut_value(input)
            .resolve_type_alias(vis_scope);
    }

    fn resolve_import(&mut self, input: PathId) {
        // Resolves visibility.
        let value = self.ptree.get_value(input).as_use_raw();
        let vis = value.visibility();
        let ext = &value.npath;
        let Some(vis_scope) = self.resolve_visibility(input.ni, &vis) else {
            self.tasks.push_back(PathTask::Resolve(input));
            return;
        };

        let on_success = move |this: &mut Self, data: &ResolvePathData| {
            let value = this.ptree.get_mut_value(input);
            value.resolve_use(vis_scope, data.best);
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
                    (*data.on_success.borrow_mut())(this, &data);
                    data.expired.set(true);
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

            // Schedules child tasks caused by imports.
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
            PathVis::Private => Some(ni),
        }
    }

    /// Resolves referencing type like `const C: ty` and field `a: ty`.
    fn resolve_ref_type<F>(&mut self, input: PathId, ty: &syn::Type, mut resolve: F)
    where
        F: FnMut(PathId, &mut PathValue) + Clone + 'static,
    {
        let mut c_resolve = resolve.clone();

        let on_success = move |this: &mut PathProcessor, data: &ResolvePathData| {
            if let Some((vi, _)) = this
                .ptree
                .get_values(data.best)
                .iter()
                .enumerate()
                .find(|(_, v)| v.is_type())
            {
                let elem_ty = data.best.to_path_id(vi);
                this.s2p.add_syn_to_path(data.sid, elem_ty);
                let value = this.ptree.get_mut_value(input);
                c_resolve(elem_ty, value);
            } else {
                this.tasks.push_back(PathTask::Resolve(input));
            }
        };

        // If failed to resolve the type, assumes it as an extern.
        let on_fail = move |this: &mut PathProcessor, data: &ResolvePathData| {
            let elem_ty = data.join_path_tree_as_extern(&mut this.ptree, &mut this.s2p);
            let value = this.ptree.get_mut_value(input);
            resolve(elem_ty, value);
        };

        let base = self.ptree.nearest_module(input.ni).ni;
        let elem = util::element_type_path_string(ty).unwrap();
        let elem_sid = util::element_type(ty).unwrap().syn_id();
        let data = ResolvePathData::new(base, elem.into(), elem_sid, on_success, on_fail);
        self.tasks.push_front(PathTask::ResolvePath(data));
    }
}

#[derive(Debug)]
struct SynToPath(HashMap<SynId, PathId>);

impl SynToPath {
    fn new() -> Self {
        Self(HashMap::new())
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

    fn to_path_id(&self, sid: SynId) -> PathId {
        self.get_path_id(sid)
            .expect(&format!("couldn't find PathId for SynId `{sid:?}`"))
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
    on_success: Rc<RefCell<dyn FnMut(&mut PathProcessor, &ResolvePathData)>>,
    /// Handler that will be called when the resolve process fails.
    on_fail: Rc<RefCell<dyn FnMut(&mut PathProcessor, &ResolvePathData)>>,
    /// Whether the resolve process completed or not.
    expired: Rc<Cell<bool>>,
}

impl ResolvePathData {
    fn new<S, F>(base: NodeIndex, ext: NamedPath, sid: SynId, on_success: S, on_fail: F) -> Self
    where
        S: FnMut(&mut PathProcessor, &ResolvePathData) + 'static,
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

impl<'a> PathCx<'a> {
    fn join_to_current_vfpath<P>(&self, ext: P) -> PathBuf
    where
        P: AsRef<StdPath>,
    {
        for cur in self.ptree.search_values(self.path.segments()) {
            match cur {
                PathValue::Mod(PathMod { vfpath, .. }) => {
                    return vfpath.join(ext.as_ref().with_extension(""));
                }
                PathValue::ModRaw(PathModRaw { vfpath, .. }) => {
                    return vfpath.join(ext.as_ref().with_extension(""));
                }
                _ => {}
            };
        }
        unreachable!()
    }

    fn join_to_parent_vfpath<P>(&self, ext: P) -> PathBuf
    where
        P: AsRef<StdPath>,
    {
        let cur_ni = self.ptree.search(self.path.segments()).unwrap();
        let parent_pid = self.ptree.parent_module(cur_ni);
        let parent = self.ptree.get_value(parent_pid);
        let parent_vfpath = match parent {
            PathValue::Mod(PathMod { vfpath, .. }) => vfpath.as_path(),
            PathValue::ModRaw(PathModRaw { vfpath, .. }) => vfpath.as_path(),
            _ => unreachable!(),
        };
        parent_vfpath.join(ext.as_ref().with_extension(""))
    }
}

trait JoinPathTree {
    type Arg;

    fn join_path_tree(&self, cx: &mut PathCx<'_>, arg: Self::Arg) -> Result<()>;
}

impl JoinPathTree for SmFile {
    type Arg = ();

    fn join_path_tree(&self, cx: &mut PathCx<'_>, _: ()) -> Result<()> {
        if !matches!(self.abs_path.extension(), Some(ext) if ext == "rs") {
            return Err(Error::new(call_site(), "not a rust file"));
        }

        let sid = self.syn_id();
        let key = cx.path.segments();
        let ni = cx.ptree.search(key.clone());
        let values = cx.ptree.search_mut_values(key.clone());

        // If the file was loaded by 'mod' from another file, we just set the
        // file pointer.
        for (vi, value) in values.iter_mut().enumerate() {
            match value {
                PathValue::Mod(v) => {
                    v.syn_file = Some(self as *const _);
                    cx.s2p.add_syn_to_path(sid, ni.unwrap().to_path_id(vi));
                }
                PathValue::ModRaw(v) => {
                    v.syn_file = Some(self as *const _);
                    cx.s2p.add_syn_to_path(sid, ni.unwrap().to_path_id(vi));
                }
                _ => {}
            }
        }

        // If the file was loaded as entry file, we need to append new module
        // for this file.
        if values.is_empty() {
            let vfpath = util::fpath_to_vfpath(&self.abs_path)?;
            let value = PathValue::ModRaw(PathModRaw {
                syn: ptr::null(),
                syn_file: Some(self as *const _),
                vfpath,
            });
            let pid = cx.ptree.append_value(PathTree::ROOT, key, value);
            cx.s2p.add_syn_to_path(sid, pid);
            cx.tasks.push_front(PathTask::Resolve(pid));
        }

        // Regardless of how the file was loaded, inserts items in the path
        // tree.
        for item in &self.items {
            item.join_path_tree(cx, ())?;
        }

        Ok(())
    }
}

impl JoinPathTree for SmItem {
    type Arg = ();

    fn join_path_tree(&self, cx: &mut PathCx<'_>, _: Self::Arg) -> Result<()> {
        // Tracks only when the path has been initialzied by tracking a file.
        if cx.path.is_empty() {
            return Ok(());
        }

        // Items that make 'new' paths need to be inserted to the path tree.
        match self {
            SmItem::Const(v) => v.join_path_tree(cx, ()),
            SmItem::Fn(v) => v.join_path_tree(cx, ()),
            SmItem::Mod(v) => v.join_path_tree(cx, ()),
            SmItem::Struct(v) => v.join_path_tree(cx, ()),
            SmItem::Type(v) => v.join_path_tree(cx, ()),
            SmItem::Use(v) => v.join_path_tree(cx, ()),
            SmItem::Other(_) => Ok(()),
        }
    }
}

impl JoinPathTree for SmItemConst {
    type Arg = ();

    fn join_path_tree(&self, cx: &mut PathCx<'_>, _: Self::Arg) -> Result<()> {
        let org_len = cx.path.len();
        util::push_colon_path(&mut cx.path, &self.ident);

        let key = cx.path.segments();
        let pid = cx.ptree.append_value(
            PathTree::ROOT,
            key,
            PathValue::ConstRaw(PathConstRaw {
                syn: self as *const _,
            }),
        );
        let sid = self.syn_id();
        cx.s2p.add_syn_to_path(sid, pid);
        cx.tasks.push_front(PathTask::Resolve(pid));

        cx.path.truncate(org_len);
        Ok(())
    }
}

impl JoinPathTree for SmItemMod {
    type Arg = ();

    fn join_path_tree(&self, cx: &mut PathCx<'_>, _: Self::Arg) -> Result<()> {
        let vfpath = if let Some(path) = self.get_attribute_value("path") {
            cx.join_to_parent_vfpath(path.trim_matches('"'))
        } else {
            cx.join_to_current_vfpath(self.ident.to_string())
        };

        // Enters the scope.
        let org_len = cx.path.len();
        util::push_colon_path(&mut cx.path, &self.ident);

        let key = cx.path.segments();
        let pid = cx.ptree.append_value(
            PathTree::ROOT,
            key,
            PathValue::ModRaw(PathModRaw {
                syn: self as *const _,
                syn_file: None,
                vfpath,
            }),
        );
        let sid = self.syn_id();
        cx.s2p.add_syn_to_path(sid, pid);
        cx.tasks.push_front(PathTask::Resolve(pid));

        // Internal items.
        if let Some((_, items)) = &self.content {
            for item in items.iter() {
                item.join_path_tree(cx, ())?;
            }
        }

        // Exits the scope.
        cx.path.truncate(org_len);
        Ok(())
    }
}

impl JoinPathTree for SmItemStruct {
    type Arg = ();

    fn join_path_tree(&self, cx: &mut PathCx<'_>, _: Self::Arg) -> Result<()> {
        let org_len = cx.path.len();
        util::push_colon_path(&mut cx.path, &self.ident);

        let key = cx.path.segments();
        let pid = cx.ptree.append_value(
            PathTree::ROOT,
            key,
            PathValue::StructRaw(PathStructRaw {
                syn: self as *const _,
            }),
        );
        let sid = self.syn_id();
        cx.s2p.add_syn_to_path(sid, pid);
        cx.tasks.push_front(PathTask::Resolve(pid));

        // Fields
        for (i, field) in self.fields.iter().enumerate() {
            field.join_path_tree(cx, i)?;
        }

        cx.path.truncate(org_len);
        Ok(())
    }
}

impl JoinPathTree for SmField {
    type Arg = usize;

    fn join_path_tree(&self, cx: &mut PathCx<'_>, nth: Self::Arg) -> Result<()> {
        let org_len = cx.path.len();
        if let Some(ident) = &self.ident {
            util::push_colon_path(&mut cx.path, ident);
        } else {
            util::push_colon_path(&mut cx.path, nth);
        }

        let key = cx.path.segments();
        let pid = cx.ptree.append_value(
            PathTree::ROOT,
            key,
            PathValue::FieldRaw(PathFieldRaw {
                syn: self as *const _,
            }),
        );
        let sid = self.syn_id();
        cx.s2p.add_syn_to_path(sid, pid);
        cx.tasks.push_front(PathTask::Resolve(pid));

        cx.path.truncate(org_len);
        Ok(())
    }
}

impl JoinPathTree for ItemFn {
    type Arg = ();

    fn join_path_tree(&self, cx: &mut PathCx<'_>, _: Self::Arg) -> Result<()> {
        let org_len = cx.path.len();
        util::push_colon_path(&mut cx.path, &self.sig.ident);

        let key = cx.path.segments();
        let pid = cx.ptree.append_value(
            PathTree::ROOT,
            key,
            PathValue::FnRaw(PathFnRaw {
                syn: self as *const _,
            }),
        );
        let sid = self.syn_id();
        cx.s2p.add_syn_to_path(sid, pid);
        cx.tasks.push_front(PathTask::Resolve(pid));

        cx.path.truncate(org_len);
        Ok(())
    }
}

impl JoinPathTree for ItemType {
    type Arg = ();

    fn join_path_tree(&self, cx: &mut PathCx<'_>, _: Self::Arg) -> Result<()> {
        let org_len = cx.path.len();
        util::push_colon_path(&mut cx.path, &self.ident);

        let key = cx.path.segments();
        let pid = cx.ptree.append_value(
            PathTree::ROOT,
            key,
            PathValue::TypeAliasRaw(PathTypeAliasRaw {
                syn: self as *const _,
            }),
        );
        let sid = self.syn_id();
        cx.s2p.add_syn_to_path(sid, pid);
        cx.tasks.push_front(PathTask::Resolve(pid));

        cx.path.truncate(org_len);
        Ok(())
    }
}

impl JoinPathTree for ItemUse {
    type Arg = ();

    fn join_path_tree(&self, cx: &mut PathCx<'_>, _: Self::Arg) -> Result<()> {
        fn dfs(
            item: &ItemUse,
            node: &UseTree,
            vis: PathVis,
            buf: &mut String,
            cx: &mut PathCx<'_>,
        ) {
            match node {
                UseTree::Path(v) => {
                    let org_len = buf.len();
                    write!(buf, "{}::", v.ident).unwrap();
                    dfs(item, &v.tree, vis, buf, cx);
                    buf.truncate(org_len);
                }
                UseTree::Name(v) => {
                    let org_len = cx.path.len();
                    util::push_colon_path(&mut cx.path, &v.ident);

                    let detail = v.syn_id();
                    add_path_value(cx, item, detail, format!("{buf}{}", v.ident));

                    cx.path.truncate(org_len);
                }
                UseTree::Rename(v) => {
                    let org_len = cx.path.len();
                    util::push_colon_path(&mut cx.path, &v.rename);

                    let detail = v.syn_id();
                    add_path_value(cx, item, detail, format!("{buf}{}", v.ident));

                    cx.path.truncate(org_len);
                }
                UseTree::Glob(v) => {
                    let org_len = cx.path.len();
                    cx.path.push_str("::*");

                    let detail = v.syn_id();
                    add_path_value(cx, item, detail, format!("{buf}*"));

                    cx.path.truncate(org_len);
                }
                UseTree::Group(v) => {
                    for node in &v.items {
                        dfs(item, node, vis.clone(), buf, cx);
                    }
                }
            }
        }

        fn add_path_value(cx: &mut PathCx<'_>, item: &ItemUse, detail: SynId, npath: String) {
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
        dfs(self, &self.tree, PathVis::new(&self.vis), &mut buf, cx);
        Ok(())
    }
}

pub struct NagaProcessor {
    pub module: Module,
    layouter: Layouter,
    typifier: Typifier,
    global_expr_tracker: ExpressionKindTracker,
    work_fs: FastIndexMap<SynId, WorkingFn>,
    tasks: TaskQueue<NagaTask>,
    s2n: SynToNaga,
}

impl NagaProcessor {
    fn new() -> Self {
        Self {
            module: Module::default(),
            layouter: Layouter::default(),
            typifier: Typifier::new(),
            global_expr_tracker: ExpressionKindTracker::new(),
            work_fs: FastIndexMap::default(),
            tasks: TaskQueue::new(),
            s2n: SynToNaga::new(),
        }
    }

    fn process(&mut self, path_proc: &PathProcessor) -> Result<()> {
        // Constructs naga module.
        for file in path_proc.stree.values() {
            self.tasks.push_back(NagaTask {
                kind: NagaTaskKind::File,
                sid: file.syn_id(),
                state: NagaCxState {
                    local: None,
                    base: PathTree::ROOT,
                },
            });
        }

        while let Some(task) = self.tasks.pop() {
            let task = task.into_value();
            util::assert_finite_loop(task, "constructing naga module");

            let sid = task.sid;
            let mut cx = NagaCx {
                ptree: &path_proc.ptree,
                s2p: &path_proc.s2p,

                module: &mut self.module,
                layouter: &mut self.layouter,
                typifier: &mut self.typifier,
                global_expr_tracker: &mut self.global_expr_tracker,
                work_fs: &mut self.work_fs,
                tasks: &mut self.tasks,
                s2n: &mut self.s2n,

                state: task.state,
            };

            match task.kind {
                NagaTaskKind::Const => {
                    sid.downcast::<SmItemConst>().join_naga(&mut cx, ())?;
                }
                NagaTaskKind::File => {
                    sid.downcast::<SmFile>().join_naga(&mut cx, ())?;
                }
                NagaTaskKind::Fn => {
                    sid.downcast::<ItemFn>().join_naga(&mut cx, ())?;
                }
                NagaTaskKind::Struct => {
                    sid.downcast::<SmItemStruct>().join_naga(&mut cx, ())?;
                }
                NagaTaskKind::Block(input) => {
                    sid.downcast::<syn::Block>().join_naga(&mut cx, input)?;
                }
            }
        }

        for (_, WorkingFn { f, .. }) in self.work_fs.drain(..) {
            self.module.functions.append(f, Span::UNDEFINED);
        }

        Ok(())
    }
}

#[derive(Debug)]
struct SynToNaga(HashMap<SynId, NagaHandle>);

impl SynToNaga {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn add_syn_to_naga(&mut self, sid: SynId, handle: NagaHandle) {
        if let Some(old_handle) = self.0.insert(sid, handle.clone()) {
            if handle != old_handle {
                panic!(
                    "Id conflicts: sid: {}({}), handle: {:?}, old_handle: {:?}",
                    sid,
                    sid.content(),
                    handle,
                    old_handle
                );
            }
        }
    }

    fn to_naga_handle(&self, sid: SynId) -> NagaHandle {
        self.get_naga_handle(sid)
            .expect(&format!("couldn't find NagaHandle for SynId `{sid:?}`"))
    }

    fn get_naga_handle(&self, sid: SynId) -> Option<NagaHandle> {
        self.0.get(&sid).cloned()
    }
}

struct WorkingFn {
    f: Function,
    expr_tracker: ExpressionKindTracker,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct NagaTask {
    kind: NagaTaskKind,
    sid: SynId,
    state: NagaCxState,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum NagaTaskKind {
    Const,
    File,
    Fn,
    Struct,
    Block(BlockTaskInput),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct BlockTaskInput {
    stmt_start_idx: usize,
    h_block_ty: Option<Handle<Type>>,
}

pub struct NagaCx<'a> {
    ptree: &'a PathTree,
    s2p: &'a SynToPath,

    module: &'a mut Module,
    layouter: &'a mut Layouter,
    typifier: &'a mut Typifier,
    global_expr_tracker: &'a mut ExpressionKindTracker,
    work_fs: &'a mut FastIndexMap<SynId, WorkingFn>,
    tasks: &'a mut TaskQueue<NagaTask>,
    s2n: &'a mut SynToNaga,

    state: NagaCxState,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct NagaCxState {
    local: Option<SynId>,
    base: NodeIndex,
}

impl<'a> NagaCx<'a> {
    /// Inserts a [`naga::Constant`] in the naga module.
    ///
    /// If the naga module already contains value the given function generates,
    /// the generated value is dropped and handle to the existing naga value is
    /// returned.
    fn insert_const<F, E>(&mut self, sid: SynId, f: F) -> TriResult<Handle<Constant>, E>
    where
        F: FnOnce(&mut Self) -> TriResult<Constant, E>,
    {
        if let Some(v) = self.s2n.get_naga_handle(sid) {
            return TriResult::Ok(v.to_const_handle());
        }

        let value = unwrap_tri! { f(self) };
        let handle = self.module.constants.append(value, Span::UNDEFINED);
        self.s2n.add_syn_to_naga(sid, NagaHandle::Const(handle));
        TriResult::Ok(handle)
    }

    /// Inserts a [`naga::Expression`] in the naga module.
    ///
    /// If the naga module already contains value the given function generates,
    /// the generated value is dropped and handle to the existing naga value is
    /// returned.
    fn insert_expr<F, E>(&mut self, sid: SynId, f: F) -> TriResult<Handle<Expression>, E>
    where
        F: FnOnce(&mut Self) -> TriResult<Expression, E>,
    {
        if let Some(v) = self.s2n.get_naga_handle(sid) {
            return TriResult::Ok(v.to_expr_handle());
        }

        let value = unwrap_tri! { f(self) };
        let handle = self.get_mut_expressions().append(value, Span::UNDEFINED);
        self.s2n.add_syn_to_naga(sid, NagaHandle::Expr(handle));
        TriResult::Ok(handle)
    }

    fn insert_working_fn<F, E>(&mut self, sid: SynId, f: F) -> TriResult<(), E>
    where
        F: FnOnce(&mut Self) -> TriResult<Function, E>,
    {
        if self.work_fs.contains_key(&sid) {
            return TriResult::Ok(());
        }

        let value = unwrap_tri! { f(self) };
        self.work_fs.insert(
            sid,
            WorkingFn {
                f: value,
                expr_tracker: ExpressionKindTracker::new(),
            },
        );
        TriResult::Ok(())
    }

    /// Inserts a [`naga::Type`] in the naga module.
    ///
    /// If the naga module already contains value the given function generates,
    /// the generated value is dropped and handle to the existing naga value is
    /// returned.
    fn insert_type<F>(&mut self, sid: SynId, f: F) -> Handle<Type>
    where
        F: FnOnce(&mut Self) -> Type,
    {
        if let Some(v) = self.s2n.get_naga_handle(sid) {
            return v.to_type_handle();
        }

        let value = f(self);
        let handle = self.module.types.insert(value, Span::UNDEFINED);
        self.layouter.update(self.module.to_ctx()).unwrap();
        self.s2n.add_syn_to_naga(sid, NagaHandle::Type(handle));
        handle
    }

    fn insert_type_by_syn(&mut self, ty: &syn::Type) -> TriResult<Handle<Type>, Error> {
        match ty {
            syn::Type::Path(v) => {
                let ext: NamedPath = util::path_to_string(&v.path).into();
                self._search_or_insert_type(&ext)
            }
            syn::Type::Array(v) => {
                // Element type
                let h_elem = unwrap_tri! { self.insert_type_by_syn(&v.elem) };

                // Size
                let h_expr = unwrap_tri! { v.len.to_naga(self, Some(h_elem)) };
                let expr = &self.get_expressions()[h_expr];
                let expr_value = match expr_to_u32(expr) {
                    Ok(v) => v,
                    Err(e) => return TriResult::Err(e),
                };
                let Some(size) = NonZeroU32::new(expr_value as _) else {
                    return tri_err!("array length must be greater than zero");
                };

                let handle = self.insert_type(v.syn_id(), |cx| Type {
                    name: None,
                    inner: TypeInner::Array {
                        base: h_elem,
                        size: ArraySize::Constant(size),
                        stride: cx.layouter[h_elem].to_stride(),
                    },
                });
                TriResult::Ok(handle)
            }
            syn::Type::Slice(v) => {
                // Element type
                let h_elem = unwrap_tri! { self.insert_type_by_syn(&v.elem) };

                let handle = self.insert_type(v.syn_id(), |cx| Type {
                    name: None,
                    inner: TypeInner::Array {
                        base: h_elem,
                        size: ArraySize::Dynamic,
                        stride: cx.layouter[h_elem].to_stride(),
                    },
                });
                TriResult::Ok(handle)
            }
            _ => {
                panic!("`{}` is not a supported type", ty.content());
            }
        }
    }

    fn _search_or_insert_type(&mut self, ext: &NamedPath) -> TriResult<Handle<Type>, Error> {
        let mut res = TriResult::None;

        self.ptree
            .traverse_from(self.state.base, ext.segments(), |pid| {
                const CONTINUE_TRAVERSING: Option<()> = None;
                const STOP_TRAVERSING: Option<()> = Some(());

                let sid = self.ptree.get_value(pid).syn_id();
                match self.s2n.get_naga_handle(sid) {
                    Some(NagaHandle::Type(handle)) => {
                        res = TriResult::Ok(handle);
                        STOP_TRAVERSING
                    }
                    Some(_) => CONTINUE_TRAVERSING,
                    None => match self._insert_known_path_type(pid) {
                        TriResult::Ok(handle) => {
                            res = TriResult::Ok(handle);
                            STOP_TRAVERSING
                        }
                        TriResult::None => CONTINUE_TRAVERSING,
                        TriResult::Err(e) => {
                            res = TriResult::Err(e);
                            STOP_TRAVERSING
                        }
                    },
                }
            });

        res
    }

    fn _insert_known_path_type(&mut self, pid: PathId) -> TriResult<Handle<Type>, Error> {
        let value = self.ptree.get_value(pid);
        match value {
            PathValue::Extern(v) => {
                let sid = v.syn_id();
                let npath = self.ptree.named_path(pid.ni);
                if let Some(ty) = known(&npath) {
                    let handle = self.insert_type(sid, |_| ty);
                    TriResult::Ok(handle)
                } else {
                    tri_err!("unknown type: `{}`", value.syn_id().content())
                }
            }
            _ => TriResult::None,
        }
    }

    fn get_expressions(&self) -> &Arena<Expression> {
        if let Some(local) = &self.state.local {
            let work_fn = self.work_fs.get(local).unwrap();
            &work_fn.f.expressions
        } else {
            &self.module.global_expressions
        }
    }

    fn get_mut_expressions(&mut self) -> &mut Arena<Expression> {
        if let Some(local) = &self.state.local {
            let work_fn = self.work_fs.get_mut(local).unwrap();
            &mut work_fn.f.expressions
        } else {
            &mut self.module.global_expressions
        }
    }

    fn get_function(&self) -> &Function {
        let sid = self
            .state
            .local
            .as_ref()
            .expect("not found working function");
        let work_fn = self.work_fs.get(sid).unwrap();
        &work_fn.f
    }

    fn get_mut_function(&mut self) -> &mut Function {
        let sid = self
            .state
            .local
            .as_ref()
            .expect("not found working function");
        let work_fn = self.work_fs.get_mut(sid).unwrap();
        &mut work_fn.f
    }

    fn try_eval_expr_and_append(
        &mut self,
        expr: Expression,
    ) -> TriResult<Handle<Expression>, Error> {
        let mut emitter = Emitter::default();

        let mut evaluator = if let Some(local) = &self.state.local {
            let work_fn = self.work_fs.get_mut(local).unwrap();
            let expressions = &mut work_fn.f.expressions;
            let block = &mut work_fn.f.body;
            let expr_tracker = &mut work_fn.expr_tracker;
            let is_const = false; // TODO

            ConstantEvaluator::for_wgsl_function(
                self.module,
                expressions,
                expr_tracker,
                &mut emitter,
                block,
                is_const,
            )
        } else {
            let is_override_ctx = false; // TODO
            ConstantEvaluator::for_wgsl_module(
                self.module,
                self.global_expr_tracker,
                is_override_ctx,
            )
        };

        match evaluator.try_eval_and_append(expr, Span::UNDEFINED) {
            Ok(handle) => TriResult::Ok(handle),
            Err(e) => TriResult::Err(Error::new(call_site(), e)),
        }
    }

    fn destructure_signature(
        &mut self,
        sig: &syn::Signature,
    ) -> TriResult<(String, Vec<FunctionArgument>, Option<FunctionResult>), Error> {
        let name = sig.ident.to_string();

        let mut inputs = Vec::with_capacity(sig.inputs.len());
        for input in &sig.inputs {
            match input {
                syn::FnArg::Receiver(v) => {
                    todo!("receiver argument is not implemented yet");
                }
                syn::FnArg::Typed(v) => {
                    let name = if let syn::Pat::Ident(ident) = v.pat.as_ref() {
                        ident.ident.to_string()
                    } else {
                        return tri_err!("not an ident in an argument position");
                    };
                    let h_ty = unwrap_tri! { self.insert_type_by_syn(&v.ty) };
                    inputs.push(FunctionArgument {
                        name: Some(name),
                        ty: h_ty,
                        binding: None,
                    });
                }
            }
        }

        let output = match &sig.output {
            syn::ReturnType::Default => None,
            syn::ReturnType::Type(_, ty) => {
                println!("destructure_signature inserting type begins");
                let h_ty = unwrap_tri! { self.insert_type_by_syn(ty) };
                println!("destructure_signature inserting type ends");
                Some(FunctionResult {
                    ty: h_ty,
                    binding: None,
                })
            }
        };

        TriResult::Ok((name, inputs, output))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NagaHandle {
    Const(Handle<Constant>),
    Expr(Handle<Expression>),
    Fn(Handle<Function>),
    Type(Handle<Type>),
}

impl NagaHandle {
    fn to_const_handle(self) -> Handle<Constant> {
        let Self::Const(v) = self else {
            unreachable!("invalid variant at `NagaHandle::to_const_handle`")
        };
        v
    }

    fn to_expr_handle(self) -> Handle<Expression> {
        let Self::Expr(v) = self else {
            unreachable!("invalid variant at `NagaHandle::to_expr_handle`")
        };
        v
    }

    fn to_fn_handle(self) -> Handle<Function> {
        let Self::Fn(v) = self else {
            unreachable!("invalid variant at `NagaHandle::to_fn_handle`")
        };
        v
    }

    fn to_type_handle(self) -> Handle<Type> {
        let Self::Type(v) = self else {
            unreachable!("invalid variant at `NagaHandle::to_type_handle`")
        };
        v
    }
}

/// Unlike [`ToNaga`], implementations of this trait are responsible for
/// rescheduling when they cannot complete tasks.
trait JoinNaga {
    type In;
    type Out;

    fn join_naga(&self, cx: &mut NagaCx<'_>, input: Self::In) -> Result<Self::Out>;
}

trait ToNaga {
    type In;
    type Out;

    fn to_naga(&self, cx: &mut NagaCx<'_>, input: Self::In) -> TriResult<Self::Out, Error>;
}

impl JoinNaga for SmFile {
    type In = ();
    type Out = ();

    fn join_naga(&self, cx: &mut NagaCx<'_>, input: Self::In) -> Result<Self::Out> {
        // Replaces the scope.
        cx.state.base = cx.s2p.to_path_id(self.syn_id()).ni;

        for item in &self.items {
            item.join_naga(cx, ())?;
        }

        Ok(())
    }
}

impl JoinNaga for SmItemStruct {
    type In = ();
    type Out = ();

    fn join_naga(&self, cx: &mut NagaCx<'_>, _: Self::In) -> Result<Self::Out> {
        let sid = self.syn_id();
        if cx.s2n.get_naga_handle(sid).is_some() {
            return Ok(());
        }

        let mut offset = 0;
        let mut max_align = 1;
        let mut members = Vec::new();

        for field in &self.fields {
            let member = match field.to_naga(cx, offset) {
                TriResult::Ok(member) => member,
                TriResult::None => {
                    cx.tasks.push_back(NagaTask {
                        kind: NagaTaskKind::Struct,
                        sid,
                        state: cx.state,
                    });
                    return Ok(());
                }
                TriResult::Err(e) => return Err(e),
            };

            let member_layout = cx.layouter[member.ty];
            offset = member.offset + member_layout.size;
            max_align = max_align.max(member_layout.alignment * 1);

            members.push(member);
        }

        let layout = Alignment::new(max_align).unwrap();
        let span = layout.round_up(offset);
        cx.insert_type(sid, |_| Type {
            name: Some(self.ident.to_string()),
            inner: TypeInner::Struct { members, span },
        });

        Ok(())
    }
}

impl JoinNaga for SmItem {
    type In = ();
    type Out = ();

    fn join_naga(&self, cx: &mut NagaCx<'_>, _: Self::In) -> Result<Self::Out> {
        match self {
            Self::Const(v) => v.join_naga(cx, ()),
            Self::Fn(v) => v.join_naga(cx, ()),
            Self::Mod(v) => {
                Ok(()) // @@@ TODO
            }
            Self::Struct(v) => v.join_naga(cx, ()),
            Self::Type(v) => {
                Ok(()) // @@@ TODO
            }
            Self::Use(v) => {
                Ok(()) // @@@ TODO
            }
            Self::Other(v) => {
                Ok(()) // @@@ TODO
            }
        }
    }
}

impl JoinNaga for SmItemConst {
    type In = ();
    type Out = ();

    fn join_naga(&self, cx: &mut NagaCx<'_>, _: Self::In) -> Result<Self::Out> {
        // Inserts this constant.
        let sid = self.syn_id();
        let res = cx.insert_const(sid, |cx| {
            // Inserts type of this constant.
            let h_ty = unwrap_tri! { cx.insert_type_by_syn(&self.ty) };

            // Inserts expression of this constant.
            let h_expr = unwrap_tri! { self.expr.to_naga(cx, Some(h_ty)) };

            // Overwrites expression's type with the explicit constant type if
            // possible.
            let _ = try_overwrite_expr_type(
                &mut cx.module.global_expressions[h_expr],
                &cx.module.types[h_ty],
            );

            TriResult::Ok(Constant {
                name: Some(self.ident.to_string()),
                ty: h_ty,
                init: h_expr,
            })
        });

        match res {
            TriResult::Ok(_) => Ok(()),
            TriResult::None => {
                cx.tasks.push_back(NagaTask {
                    kind: NagaTaskKind::Const,
                    sid,
                    state: cx.state,
                });
                Ok(())
            }
            TriResult::Err(e) => Err(e),
        }
    }
}

impl JoinNaga for ItemFn {
    type In = ();
    type Out = ();

    fn join_naga(&self, cx: &mut NagaCx<'_>, _: Self::In) -> Result<Self::Out> {
        let sid = self.syn_id();

        // Signature
        let (name, arguments, result) = match cx.destructure_signature(&self.sig) {
            TriResult::Ok(v) => v,
            TriResult::None => {
                cx.tasks.push_back(NagaTask {
                    kind: NagaTaskKind::Fn,
                    sid,
                    state: cx.state,
                });
                return Ok(());
            }
            TriResult::Err(e) => return Err(e),
        };
        let h_block_ty = result.as_ref().map(|res| res.ty);

        // Function with signature information
        let c_arguments = arguments.clone();
        match cx.insert_working_fn(sid, move |cx| {
            TriResult::Ok(Function {
                name: Some(name),
                arguments: c_arguments,
                result,
                local_variables: Arena::new(),
                expressions: Arena::new(),
                named_expressions: FastIndexMap::default(),
                body: Block::new(),
                diagnostic_filter_leaf: None,
            })
        }) {
            TriResult::Ok(_) => {}
            TriResult::None => {
                cx.tasks.push_back(NagaTask {
                    kind: NagaTaskKind::Fn,
                    sid,
                    state: cx.state,
                });
                return Ok(());
            }
            TriResult::Err(e) => return Err(e),
        }

        // Update the state.
        cx.state.local = Some(sid);

        // Adds arguments as named expressions.
        for (i, arg) in arguments.into_iter().enumerate() {
            let expr = Expression::FunctionArgument(i as u32);
            let h_expr = match cx.try_eval_expr_and_append(expr) {
                TriResult::Ok(handle) => handle,
                _ => unreachable!(),
            };
            if let Some(name) = arg.name {
                let f = cx.get_mut_function();
                f.named_expressions.insert(h_expr, name);
            }
        }

        // Block
        self.block.join_naga(
            cx,
            BlockTaskInput {
                stmt_start_idx: 0,
                h_block_ty,
            },
        )?;

        // Reverts the state.
        cx.state.local = None;
        Ok(())
    }
}

impl JoinNaga for syn::Block {
    type In = BlockTaskInput;
    type Out = ();

    fn join_naga(&self, cx: &mut NagaCx<'_>, input: Self::In) -> Result<Self::Out> {
        let BlockTaskInput {
            stmt_start_idx,
            h_block_ty,
        } = input;
        let sid = self.syn_id();

        for (i, stmt) in self.stmts.iter().enumerate().skip(stmt_start_idx) {
            match stmt.to_naga(cx, h_block_ty) {
                TriResult::Ok(stmt) => {
                    if let Some(stmt) = stmt {
                        // TODO: If not a function context?
                        cx.get_mut_function().body.push(stmt, Span::UNDEFINED);
                    }
                }
                TriResult::None => {
                    cx.tasks.push_back(NagaTask {
                        kind: NagaTaskKind::Block(BlockTaskInput {
                            stmt_start_idx: i,
                            h_block_ty: input.h_block_ty,
                        }),
                        sid,
                        state: cx.state,
                    });
                    return Ok(());
                }
                TriResult::Err(e) => return Err(Error::new(call_site(), e)),
            }
        }

        // Puts empty return statement if needed.
        let f = cx.get_mut_function(); // TODO: If not a function context?
        let mut need_return = true;
        if let Some(last_stmt) = f.body.last() {
            need_return = !matches!(last_stmt, Statement::Return { .. });
        }
        if need_return {
            f.body.push(Statement::Return { value: None }, Span::UNDEFINED);
        }

        Ok(())
    }
}

impl ToNaga for syn::Stmt {
    type In = Option<Handle<Type>>;
    type Out = Option<Statement>;

    fn to_naga(&self, cx: &mut NagaCx<'_>, input: Self::In) -> TriResult<Self::Out, Error> {
        let h_block_ty = input;

        match self {
            syn::Stmt::Local(local) => local.to_naga(cx, ()),
            syn::Stmt::Expr(expr, semi) => {
                let h_expr = unwrap_tri! { expr.to_naga(cx, h_block_ty) };
                let stmt = if semi.is_none() {
                    Some(Statement::Return {
                        value: Some(h_expr),
                    })
                } else {
                    todo!("expr -> stmt")
                };
                TriResult::Ok(stmt)
            }
            o => todo!("Stmt::to_naga, {o:?}"),
        }
    }
}

impl ToNaga for syn::Local {
    type In = ();
    type Out = Option<Statement>;

    fn to_naga(&self, cx: &mut NagaCx<'_>, _: Self::In) -> TriResult<Self::Out, Error> {
        let (mutability, ident, ty) = unwrap_tri! { helper(&self.pat) };
        let Some(ty) = ty else {
            let this = self.to_token_stream();
            return tri_err!("`{this}`: inference is not implemented yet");
        };

        let Some(init) = &self.init else {
            let this = self.to_token_stream();
            return tri_err!("`{this}`: inference is not implemented yet");
        };

        let h_ty = unwrap_tri! { cx.insert_type_by_syn(ty) };
        let h_expr = unwrap_tri! { init.expr.to_naga(cx, Some(h_ty)) };

        if !mutability {
            let f = cx.get_mut_function();
            f.named_expressions.insert(h_expr, ident);
        } else {
            let value = LocalVariable {
                name: Some(ident),
                ty: h_ty,
                init: Some(h_expr),
            };

            let f = cx.get_mut_function();
            f.local_variables.append(value, Span::UNDEFINED);
        }

        // `let` binding generates named expression or local variable instead of
        // statement.
        return TriResult::Ok(None);

        // === Internal helper functions ===

        fn helper(pat: &syn::Pat) -> TriResult<(bool, String, Option<&syn::Type>), Error> {
            match pat {
                syn::Pat::Ident(v) => {
                    let mutability = v.mutability.is_some();
                    let ident = v.ident.to_string();
                    let ty = None;
                    TriResult::Ok((mutability, ident, ty))
                }
                syn::Pat::Type(v) => {
                    let (mutability, ident, _) = unwrap_tri! { helper(&v.pat) };
                    TriResult::Ok((mutability, ident, Some(&v.ty)))
                }
                _ => {
                    tri_err!("`{}` is not supported yet", pat.to_token_stream())
                }
            }
        }
    }
}

impl ToNaga for syn::Expr {
    type In = Option<Handle<Type>>;
    type Out = Handle<Expression>;

    fn to_naga(&self, cx: &mut NagaCx<'_>, ty: Self::In) -> TriResult<Self::Out, Error> {
        if let Some(handle) = cx.s2n.get_naga_handle(self.syn_id()) {
            return TriResult::Ok(handle.to_expr_handle());
        }

        match self {
            syn::Expr::Array(v) => v.to_naga(cx, ty),
            syn::Expr::Binary(v) => v.to_naga(cx, ty),
            syn::Expr::Lit(v) => v.to_naga(cx, ()),
            syn::Expr::Path(v) => v.to_naga(cx, ()),
            o => todo!("syn::Expr::ToNaga, {o:?}"),
        }
    }
}

impl ToNaga for syn::ExprArray {
    type In = Option<Handle<Type>>;
    type Out = Handle<Expression>;

    fn to_naga(&self, cx: &mut NagaCx<'_>, ty: Self::In) -> TriResult<Self::Out, Error> {
        let mut components = Vec::with_capacity(self.elems.len());
        for elem in &self.elems {
            let h_elem = unwrap_tri! { elem.to_naga(cx, ty) };
            components.push(h_elem);
        }

        cx.try_eval_expr_and_append(Expression::Compose {
            ty: ty.expect("expected handle to a type for naga array"),
            components,
        })
        .into()
    }
}

impl ToNaga for syn::ExprBinary {
    type In = Option<Handle<Type>>;
    type Out = Handle<Expression>;

    fn to_naga(&self, cx: &mut NagaCx<'_>, ty: Self::In) -> TriResult<Self::Out, Error> {
        let op = unwrap_tri! { self.op.to_naga(cx, ()) };
        let h_left = unwrap_tri! { self.left.to_naga(cx, ty) };
        let h_right = unwrap_tri! { self.right.to_naga(cx, ty) };

        cx.try_eval_expr_and_append(Expression::Binary {
            op,
            left: h_left,
            right: h_right,
        })
        .into()
    }
}

impl ToNaga for syn::ExprLit {
    type In = ();
    type Out = Handle<Expression>;

    fn to_naga(&self, cx: &mut NagaCx<'_>, _: Self::In) -> TriResult<Self::Out, Error> {
        let lit = unwrap_tri! { self.lit.to_naga(cx, ()) };
        cx.try_eval_expr_and_append(Expression::Literal(lit)).into()
    }
}

impl ToNaga for syn::ExprPath {
    type In = ();
    type Out = Handle<Expression>;

    fn to_naga(&self, cx: &mut NagaCx<'_>, _: Self::In) -> TriResult<Self::Out, Error> {
        let sid = self.syn_id();
        if let Some(handle) = cx.s2n.get_naga_handle(sid) {
            return TriResult::Ok(handle.to_expr_handle());
        }

        let ext: NamedPath = util::path_to_string(&self.path).into();
        let pid = cx.ptree.reach(cx.state.base, ext.segments());
        let value = cx.ptree.get_value(pid);
        match value {
            PathValue::Const(c) => {
                let Some(handle) = cx.s2n.get_naga_handle(c.syn_id()) else {
                    return TriResult::None;
                };
                let expr = Expression::Constant(handle.to_const_handle());
                cx.try_eval_expr_and_append(expr.clone()).into()
            }
            _ => todo!("@@@ syn::ExprPath::to_naga"),
        }
    }
}

impl ToNaga for syn::BinOp {
    type In = ();
    type Out = BinaryOperator;

    fn to_naga(&self, _: &mut NagaCx<'_>, _: Self::In) -> TriResult<Self::Out, Error> {
        TriResult::Ok(match self {
            syn::BinOp::Add(_) => BinaryOperator::Add,
            syn::BinOp::Sub(_) => BinaryOperator::Subtract,
            syn::BinOp::Mul(_) => BinaryOperator::Multiply,
            syn::BinOp::Div(_) => BinaryOperator::Divide,
            syn::BinOp::Rem(_) => BinaryOperator::Modulo,
            syn::BinOp::Eq(_) => BinaryOperator::Equal,
            syn::BinOp::Ne(_) => BinaryOperator::NotEqual,
            syn::BinOp::Lt(_) => BinaryOperator::Less,
            syn::BinOp::Le(_) => BinaryOperator::LessEqual,
            syn::BinOp::Gt(_) => BinaryOperator::Greater,
            syn::BinOp::Ge(_) => BinaryOperator::GreaterEqual,
            syn::BinOp::BitAnd(_) => BinaryOperator::And,
            syn::BinOp::BitXor(_) => BinaryOperator::ExclusiveOr,
            syn::BinOp::BitOr(_) => BinaryOperator::InclusiveOr,
            syn::BinOp::And(_) => BinaryOperator::LogicalAnd,
            syn::BinOp::Or(_) => BinaryOperator::LogicalOr,
            syn::BinOp::Shl(_) => BinaryOperator::ShiftLeft,
            syn::BinOp::Shr(_) => BinaryOperator::ShiftRight,
            _ => {
                return TriResult::Err(Error::new(
                    call_site(),
                    &format!("unsupported operator: `{:?}`", self),
                ));
            }
        })
    }
}

impl ToNaga for syn::Lit {
    type In = ();
    type Out = Literal;

    fn to_naga(&self, _: &mut NagaCx<'_>, _: Self::In) -> TriResult<Self::Out, Error> {
        macro_rules! unwrap {
            ($($t:tt)*) => {
                match $($t)* {
                    Ok(t) => t,
                    Err(e) => return TriResult::Err(e),
                }
            };
        }

        let lit = match self {
            Lit::Int(v) => match v.suffix() {
                "i8" | "i16" | "i32" => Literal::I32(unwrap!(v.base10_parse())),
                "u8" | "u16" | "u32" => Literal::U32(unwrap!(v.base10_parse())),
                "i64" => Literal::I64(unwrap!(v.base10_parse())),
                "u64" => Literal::U64(unwrap!(v.base10_parse())),
                "i128" | "u128" => {
                    return TriResult::Err(Error::new(call_site(), "128-bits is not allowed"));
                }
                // isize, usize, and no suffix
                _ => Literal::AbstractInt(unwrap!(v.base10_parse())),
            },
            Lit::Float(v) => match v.suffix() {
                "f32" => Literal::F32(unwrap!(v.base10_parse())),
                "f64" => Literal::F64(unwrap!(v.base10_parse())),
                // no suffix
                _ => Literal::AbstractFloat(unwrap!(v.base10_parse())),
            },
            Lit::Bool(v) => Literal::Bool(v.value()),
            _ => {
                return TriResult::Err(Error::new(
                    call_site(),
                    &format!("unsupported literal: `{}`", self.content()),
                ));
            }
        };
        TriResult::Ok(lit)
    }
}

impl ToNaga for SmField {
    type In = u32;
    type Out = StructMember;

    fn to_naga(&self, cx: &mut NagaCx<'_>, mut offset: Self::In) -> TriResult<Self::Out, Error> {
        // Finds naga handle of this field.
        cx.insert_type_by_syn(&self.ty).map(|h_ty| {
            // Constructs naga struct member using the type handle.
            let layout = cx.layouter[h_ty];
            offset = layout.alignment.round_up(offset);
            StructMember {
                name: self.ident.as_ref().map(ToString::to_string),
                ty: h_ty,
                binding: None,
                offset,
            }
        })
    }
}

#[derive(Debug)]
enum TriResult<T, E> {
    Ok(T),
    None,
    Err(E),
}

impl<T, E> TriResult<T, E> {
    fn map<F, R>(self, f: F) -> TriResult<R, E>
    where
        F: FnOnce(T) -> R,
    {
        match self {
            Self::Ok(t) => TriResult::Ok(f(t)),
            Self::None => TriResult::None,
            Self::Err(e) => TriResult::Err(e),
        }
    }

    fn map_err<F, R>(self, f: F) -> TriResult<T, R>
    where
        F: FnOnce(E) -> R,
    {
        match self {
            Self::Ok(t) => TriResult::Ok(t),
            Self::None => TriResult::None,
            Self::Err(e) => TriResult::Err(f(e)),
        }
    }

    fn into_res(self) -> StdResult<(), E> {
        self.into()
    }
}

impl<T, E> From<TriResult<T, E>> for StdResult<(), E> {
    fn from(value: TriResult<T, E>) -> Self {
        match value {
            TriResult::Ok(_) | TriResult::None => Ok(()),
            TriResult::Err(e) => Err(e),
        }
    }
}

impl<T> From<Option<T>> for TriResult<T, Error> {
    fn from(value: Option<T>) -> Self {
        if let Some(v) = value {
            TriResult::Ok(v)
        } else {
            TriResult::None
        }
    }
}

impl<T, E> From<StdResult<T, E>> for TriResult<T, E> {
    fn from(value: StdResult<T, E>) -> Self {
        match value {
            Ok(t) => TriResult::Ok(t),
            Err(e) => TriResult::Err(e),
        }
    }
}

/// Overwrites the type of `expr` with the given `ty`.
/// 
/// It only works if `expr` is a literal for now.
#[rustfmt::skip]
fn try_overwrite_expr_type(expr: &mut Expression, ty: &Type) -> StdResult<(), ()> {
    // Overwriting expression type is only possible on scalar types
    let TypeInner::Scalar(ty) = &ty.inner else {
        return Err(());
    };

    // All bit options we have are only 32 and 64.
    macro_rules! decl_literal_cast_map {
        ($id:ident, $ty:ty) => {
            const $id: [fn(&mut Literal, $ty); 12] = [
                |lit, v| { *lit = Literal::I32(v as _) }, /* Sint(0)*2 + 7/8 */
                |lit, v| { *lit = Literal::I64(v as _) }, /* Sint(0)*2 + 8/8 */
                |lit, v| { *lit = Literal::U32(v as _) }, /* Uint(1)*2 + 7/8 */
                |lit, v| { *lit = Literal::U64(v as _) }, /* Uint(1)*2 + 8/8 */
                |lit, v| { *lit = Literal::F32(v as _) }, /* Float(2)*2 + 7/8 */
                |lit, v| { *lit = Literal::F64(v as _) }, /* Float(2)*2 + 8/8 */
                |_, _| {}, /* Bool(3)*2 + 7/8 */
                |_, _| {}, /* Bool(3)*2 + 8/8 */
                |_, _| {}, /* AbsI(4)*2 + 7/8 */
                |lit, v| { *lit = Literal::AbstractInt(v as _) }, /* AbsI(4)*2 + 8/8 */
                |_, _| {}, /* AbsF(5)*2 + 7/8 */
                |lit, v| { *lit = Literal::AbstractFloat(v as _) }, /* AbsF(5)*2 + 8/8 */
            ];
        };
    }

    // Assertions
    const { 
        assert!(ScalarKind::Sint as usize == 0);
        assert!(ScalarKind::Uint as usize == 1);
        assert!(ScalarKind::Float as usize == 2);
        assert!(ScalarKind::AbstractInt as usize == 4);
        assert!(ScalarKind::AbstractFloat as usize == 5);
    };
    debug_assert!(ty.width <= 8);

    decl_literal_cast_map!(LIT_I32_TO_TARGET, i32);
    decl_literal_cast_map!(LIT_U32_TO_TARGET, u32);
    decl_literal_cast_map!(LIT_I64_TO_TARGET, i64);
    decl_literal_cast_map!(LIT_U64_TO_TARGET, u64);
    decl_literal_cast_map!(LIT_F32_TO_TARGET, f32);
    decl_literal_cast_map!(LIT_F64_TO_TARGET, f64);

    let target = (ty.kind as u8 * 2 + ty.width / 8) as usize;

    if let Expression::Literal(lit) = expr {
        match *lit {
            Literal::Bool(_) => {
                if ty.kind != ScalarKind::Bool {
                    return Err(());
                }
            }
            Literal::I32(v) => LIT_I32_TO_TARGET[target](lit, v),
            Literal::I64(v) => LIT_I64_TO_TARGET[target](lit, v),
            Literal::U32(v) => LIT_U32_TO_TARGET[target](lit, v),
            Literal::U64(v) => LIT_U64_TO_TARGET[target](lit, v),
            Literal::F32(v) => LIT_F32_TO_TARGET[target](lit, v),
            Literal::F64(v) => LIT_F64_TO_TARGET[target](lit, v),
            Literal::AbstractInt(v) => LIT_I64_TO_TARGET[target](lit, v),
            Literal::AbstractFloat(v) => LIT_F64_TO_TARGET[target](lit, v),
        }
    }

    Ok(())
}

fn expr_to_u32(expr: &Expression) -> Result<u32> {
    let Expression::Literal(lit) = expr else {
        return Err(Error::new(call_site(), format!("`{expr:?}` cannot be u32")));
    };

    Ok(match lit {
        Literal::Bool(v) => *v as _,
        Literal::I32(v) => *v as _,
        Literal::I64(v) => *v as _,
        Literal::U32(v) => *v as _,
        Literal::U64(v) => *v as _,
        Literal::F32(v) => *v as _,
        Literal::F64(v) => *v as _,
        Literal::AbstractInt(v) => *v as _,
        Literal::AbstractFloat(v) => *v as _,
    })
}
