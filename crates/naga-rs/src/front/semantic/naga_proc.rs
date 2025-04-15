use super::{known::*, path_proc::*, path_tree::*};
use crate::{
    Error, Result,
    front::{
        syntax::{
            common::{IdentifySyn, SynId},
            file::SmFile,
        },
        util,
    },
};
use naga::{
    Arena, ArraySize, BinaryOperator, Block, Constant, Expression, FastIndexMap, Function,
    FunctionArgument, FunctionResult, Handle, Literal, LocalVariable, Module, ScalarKind, Span,
    Statement, StructMember, Type, TypeInner,
    front::{SymbolTable, Typifier},
    proc::{Alignment, ConstantEvaluator, Emitter, ExpressionKindTracker, Layouter},
};
use quote::ToTokens;
use std::{
    collections::{HashMap, VecDeque},
    num::NonZeroU32,
    result::Result as StdResult,
};
use syn_locator::Locate;

macro_rules! terr {
    ($id:ident) => {
        Err(Fail::Hard(
            format!("{}", $id).into()
        ))
    };
    ($($t:tt)*) => {
        Err(Fail::Hard(
            format!($($t)*).into()
        ))
    };
}

pub struct NagaProcessor {
    pub module: Module,
}

impl NagaProcessor {
    pub fn new() -> Self {
        Self {
            module: Module::default(),
        }
    }

    pub fn process(&mut self, path_proc: &PathProcessor) -> Result<()> {
        let mut tasks = NagaTaskQueue::new();
        let mut s2n = SynToNaga::new();

        let mut layouter = Layouter::default();
        let mut typifier = Typifier::default();
        let mut namer = Namer::new();
        let mut global_expr_tracker = ExpressionKindTracker::new();
        let mut work_fs = FastIndexMap::default();

        // Constructs naga module.
        for file in path_proc.stree.values() {
            tasks.push_back(NagaTask {
                kind: NagaTaskKind::File,
                sid: file.syn_id(),
                state: NagaCxState {
                    local: None,
                    base: PathTree::ROOT,
                },
            });
        }

        while let Some(task) = tasks.pop() {
            util::assert_finite_loop(task, "constructing naga module");

            let sid = task.sid;
            let mut cx = NagaCx {
                // From path processor.
                ptree: &path_proc.ptree,
                s2p: &path_proc.s2p,

                // From naga processor or handler.
                module: &mut self.module,
                tasks: &mut tasks,
                s2n: &mut s2n,

                // naga helper or temporary storage.
                layouter: &mut layouter,
                typifier: &mut typifier,
                namer: &mut namer,
                global_expr_tracker: &mut global_expr_tracker,
                work_fs: &mut work_fs,

                // Own data.
                state: task.state,
            };

            macro_rules! unwrap {
                ($($t:tt)*) => {
                    match $($t)* {
                        Ok(()) | Err(Fail::Soft) => {}
                        Err(Fail::Hard(e)) => return Err(e)
                    }
                };
            }

            match task.kind {
                NagaTaskKind::Const => unwrap! {
                    item_const_to_naga(sid.downcast::<syn::ItemConst>(), &mut cx)
                },
                NagaTaskKind::File => unwrap! {
                    file_to_naga(sid.downcast::<SmFile>(), &mut cx)
                },
                NagaTaskKind::Fn => unwrap! {
                    item_fn_to_naga(sid.downcast::<syn::ItemFn>(), &mut cx)
                },
                NagaTaskKind::Struct => unwrap! {
                    item_struct_to_naga(sid.downcast::<syn::ItemStruct>(), &mut cx)
                },
                NagaTaskKind::Block {
                    stmt_start_idx,
                    h_block_ty,
                } => unwrap! {
                    block_to_naga(sid.downcast::<syn::Block>(), &mut cx, stmt_start_idx, h_block_ty)
                },
            }
        }

        for (_, WorkingFn { f, .. }) in work_fs.drain(..) {
            self.module.functions.append(f, Span::UNDEFINED);
        }

        Ok(())
    }
}

struct Namer {
    map: HashMap<SynId, String>,
    inner: naga::proc::Namer,
}

impl Namer {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
            inner: naga::proc::Namer::default(),
        }
    }

    /// Turns the given ident into globally unique ident.
    fn unique_ident(&mut self, sid: SynId, ident: &str) -> &str {
        self.map.entry(sid).or_insert(self.inner.call(ident))
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

struct NagaTaskQueue {
    queue: VecDeque<NagaTask>,
    re_slot: Option<(SynId, NagaTask)>,
}

impl NagaTaskQueue {
    const fn new() -> Self {
        Self {
            queue: VecDeque::new(),
            re_slot: None,
        }
    }

    fn push_back(&mut self, task: NagaTask) {
        self.queue.push_back(task);
    }

    fn reserve_resched(&mut self, task: NagaTask) {
        if self.re_slot.is_none() {
            self.re_slot = Some((task.sid, task));
        }
    }

    fn cancel_resched(&mut self, sid: SynId) {
        if matches!(self.re_slot.as_ref(), Some((re_sid, _)) if *re_sid == sid) {
            self.re_slot.take();
        }
    }

    fn pop(&mut self) -> Option<NagaTask> {
        if let Some((_, re_task)) = self.re_slot.take() {
            self.queue.push_back(re_task);
        }
        self.queue.pop_front()
    }
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
    Block {
        stmt_start_idx: usize,
        h_block_ty: Option<Handle<Type>>,
    },
}

pub struct NagaCx<'a> {
    // From path processor.
    ptree: &'a PathTree,
    s2p: &'a SynToPath,

    // From naga processor or handler.
    module: &'a mut Module,
    tasks: &'a mut NagaTaskQueue,
    s2n: &'a mut SynToNaga,

    // naga helper or temporary storage.
    layouter: &'a mut Layouter,
    typifier: &'a mut Typifier,
    namer: &'a mut Namer,
    global_expr_tracker: &'a mut ExpressionKindTracker,
    work_fs: &'a mut FastIndexMap<SynId, WorkingFn>,

    // Own data.
    state: NagaCxState,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct NagaCxState {
    local: Option<SynId>,
    base: NodeIndex,
}

impl<'a> NagaCx<'a> {
    fn reserve_resched(&mut self, kind: NagaTaskKind, sid: SynId) {
        self.tasks.reserve_resched(NagaTask {
            kind,
            sid,
            state: self.state,
        });
    }

    fn cancel_resched(&mut self, sid: SynId) {
        self.tasks.cancel_resched(sid);
    }

    /// Inserts a [`naga::Constant`] in the naga module.
    ///
    /// If the naga module already contains value the given function generates,
    /// the generated value is dropped and handle to the existing naga value is
    /// returned.
    fn insert_const<F>(&mut self, sid: SynId, f: F) -> TriResult<Handle<Constant>>
    where
        F: FnOnce(&mut Self) -> TriResult<Constant>,
    {
        if let Some(v) = self.s2n.get_naga_handle(sid) {
            return Ok(v.to_const_handle());
        }

        let value = f(self)?;
        let handle = self.module.constants.append(value, Span::UNDEFINED);
        self.s2n.add_syn_to_naga(sid, NagaHandle::Const(handle));
        Ok(handle)
    }

    /// Inserts a [`naga::Expression`] in the naga module.
    ///
    /// If the naga module already contains value the given function generates,
    /// the generated value is dropped and handle to the existing naga value is
    /// returned.
    fn insert_expr<F>(&mut self, sid: SynId, f: F) -> TriResult<Handle<Expression>>
    where
        F: FnOnce(&mut Self) -> TriResult<Expression>,
    {
        if let Some(v) = self.s2n.get_naga_handle(sid) {
            return Ok(v.to_expr_handle());
        }

        let value = f(self)?;
        let handle = self.get_mut_expressions().append(value, Span::UNDEFINED);
        self.s2n.add_syn_to_naga(sid, NagaHandle::Expr(handle));
        Ok(handle)
    }

    fn insert_working_fn<F>(&mut self, sid: SynId, f: F) -> TriResult<()>
    where
        F: FnOnce(&mut Self) -> TriResult<Function>,
    {
        if self.work_fs.contains_key(&sid) {
            return Ok(());
        }

        let value = f(self)?;
        self.work_fs.insert(
            sid,
            WorkingFn {
                f: value,
                expr_tracker: ExpressionKindTracker::new(),
            },
        );
        Ok(())
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

    fn insert_type_by_syn(&mut self, ty: &syn::Type) -> TriResult<Handle<Type>> {
        match ty {
            syn::Type::Path(v) => {
                let ext: NamedPath = util::path_to_string(&v.path).into();
                self._search_or_insert_type(&ext)
            }
            syn::Type::Array(v) => {
                // Element type
                let h_elem = self.insert_type_by_syn(&v.elem)?;

                // Size
                let h_expr = expr_to_naga(&v.len, self, Some(h_elem))?;
                let expr = &self.get_expressions()[h_expr];
                let expr_value = match expr_to_u32(expr) {
                    Ok(v) => v,
                    Err(e) => return Err(Fail::Hard(e)),
                };
                let Some(size) = NonZeroU32::new(expr_value as _) else {
                    return terr!(
                        "{}: array length must be greater than zero",
                        v.location_message()
                    );
                };

                let h_ty = self.insert_type(v.syn_id(), |cx| Type {
                    name: None,
                    inner: TypeInner::Array {
                        base: h_elem,
                        size: ArraySize::Constant(size),
                        stride: cx.layouter[h_elem].to_stride(),
                    },
                });
                Ok(h_ty)
            }
            syn::Type::Slice(v) => {
                // Element type
                let h_elem = self.insert_type_by_syn(&v.elem)?;

                let h_ty = self.insert_type(v.syn_id(), |cx| Type {
                    name: None,
                    inner: TypeInner::Array {
                        base: h_elem,
                        size: ArraySize::Dynamic,
                        stride: cx.layouter[h_elem].to_stride(),
                    },
                });
                Ok(h_ty)
            }
            _ => {
                terr!("`{}` is not a supported type", ty.location_message())
            }
        }
    }

    fn _search_or_insert_type(&mut self, ext: &NamedPath) -> TriResult<Handle<Type>> {
        let mut res = Err(Fail::Soft);

        self.ptree
            .traverse_from(self.state.base, ext.segments(), |pid| {
                const CONTINUE_TRAVERSING: Option<()> = None;
                const STOP_TRAVERSING: Option<()> = Some(());

                let sid = self.ptree.get_value(pid).syn_id();
                match self.s2n.get_naga_handle(sid) {
                    Some(handle) => {
                        res = Ok(handle.to_type_handle());
                        STOP_TRAVERSING
                    }
                    None => match self._insert_known_path_type(pid) {
                        ok @ Ok(..) => {
                            res = ok;
                            STOP_TRAVERSING
                        }
                        err @ Err(Fail::Hard(..)) => {
                            res = err;
                            STOP_TRAVERSING
                        }
                        Err(Fail::Soft) => CONTINUE_TRAVERSING,
                    },
                }
            });

        res
    }

    fn _insert_known_path_type(&mut self, pid: PathId) -> TriResult<Handle<Type>> {
        let value = self.ptree.get_value(pid);
        match value {
            PathValue::Extern(v) => {
                let sid = v.syn_id();
                let npath = self.ptree.named_path(pid.ni);
                if let Some(ty) = known(&npath) {
                    let handle = self.insert_type(sid, |_| ty);
                    Ok(handle)
                } else {
                    terr!("{}: unknown type", value.syn_id().content())
                }
            }
            _ => Err(Fail::Soft),
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

    fn try_eval_expr_and_append(&mut self, expr: Expression) -> TriResult<Handle<Expression>> {
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
            Err(e) => terr!(e),
        }
    }

    fn destructure_signature(
        &mut self,
        sig: &syn::Signature,
    ) -> TriResult<(String, Vec<FunctionArgument>, Option<FunctionResult>)> {
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
                        return terr!("not an ident in an argument position");
                    };
                    let h_ty = self.insert_type_by_syn(&v.ty)?;
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
                let h_ty = self.insert_type_by_syn(ty)?;
                Some(FunctionResult {
                    ty: h_ty,
                    binding: None,
                })
            }
        };

        Ok((name, inputs, output))
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

type TriResult<T> = StdResult<T, Fail>;

enum Fail {
    /// Failed, but can be resolved by rescheduling or something else.
    Soft,
    Hard(Error),
}

fn file_to_naga(file: &SmFile, cx: &mut NagaCx) -> TriResult<()> {
    // Replaces the scope.
    cx.state.base = cx.s2p.to_path_id(file.syn_id()).ni;

    for item in &file.file.items {
        item_to_naga(item, cx)?;
    }

    Ok(())
}

/// For a struct directly declared in a module,
/// - Creates [`naga::Type`].
/// - Inserts the naga value in the naga module.
fn item_struct_to_naga(st: &syn::ItemStruct, cx: &mut NagaCx) -> TriResult<()> {
    let sid = st.syn_id();
    if cx.s2n.get_naga_handle(sid).is_some() {
        return Ok(());
    }
    cx.reserve_resched(NagaTaskKind::Struct, sid);

    let mut offset = 0;
    let mut max_align = 1;
    let mut members = Vec::new();

    for field in &st.fields {
        let member = field_to_naga(field, cx, offset)?;
        let member_layout = cx.layouter[member.ty];
        offset = member.offset + member_layout.size;
        max_align = max_align.max(member_layout.alignment * 1);

        members.push(member);
    }

    let layout = Alignment::new(max_align).unwrap();
    let span = layout.round_up(offset);
    cx.insert_type(sid, |_| Type {
        name: Some(st.ident.to_string()),
        inner: TypeInner::Struct { members, span },
    });

    cx.cancel_resched(sid);
    Ok(())
}

/// For an item directly declared in a module,
/// - Creates corresponding naga value.
/// - Inserts the naga value in the naga module.
fn item_to_naga(item: &syn::Item, cx: &mut NagaCx) -> TriResult<()> {
    match item {
        syn::Item::Const(v) => item_const_to_naga(v, cx),
        syn::Item::Enum(_) => Ok(()),
        syn::Item::ExternCrate(_) => Ok(()),
        syn::Item::Fn(v) => item_fn_to_naga(v, cx),
        syn::Item::ForeignMod(_) => Ok(()),
        syn::Item::Impl(_) => Ok(()),
        syn::Item::Macro(_) => Ok(()),
        syn::Item::Mod(_) => Ok(()),
        syn::Item::Static(_) => Ok(()),
        syn::Item::Struct(v) => item_struct_to_naga(v, cx),
        syn::Item::Trait(_) => Ok(()),
        syn::Item::TraitAlias(_) => Ok(()),
        syn::Item::Type(_) => Ok(()),
        syn::Item::Union(_) => Ok(()),
        syn::Item::Use(_) => Ok(()),
        syn::Item::Verbatim(_) => Ok(()),
        _ => Ok(()),
    }
}

/// For a const directly declared in a module,
/// - Creates [`naga::Constant`].
/// - Inserts the naga value in the naga module.
fn item_const_to_naga(item_const: &syn::ItemConst, cx: &mut NagaCx) -> TriResult<()> {
    let sid = item_const.syn_id();
    cx.reserve_resched(NagaTaskKind::Const, sid);

    // Const is hoisted up to global scope.
    let local = cx.state.local.take();

    // Inserts this constant.
    cx.insert_const(sid, |cx| {
        // Inserts type of this constant.
        let h_ty = cx.insert_type_by_syn(&item_const.ty)?;

        // Inserts expression of this constant.
        let h_expr = expr_to_naga(&item_const.expr, cx, Some(h_ty))?;

        // Overwrites expression's type with the explicit constant type if
        // possible.
        let _ = try_overwrite_expr_type(
            &mut cx.module.global_expressions[h_expr],
            &cx.module.types[h_ty],
        );

        let ident = item_const.ident.to_string();
        let ident = cx.namer.unique_ident(sid, &ident).to_owned();

        Ok(Constant {
            name: Some(ident),
            ty: h_ty,
            init: h_expr,
        })
    })?;

    // Restores the state.
    cx.state.local = local;

    cx.cancel_resched(sid);
    Ok(())
}

/// For a free standing function directly declared in a module,
/// - Creates [`naga::Function`].
/// - Inserts the naga value in the naga module.
fn item_fn_to_naga(item_fn: &syn::ItemFn, cx: &mut NagaCx) -> TriResult<()> {
    let sid = item_fn.syn_id();
    cx.reserve_resched(NagaTaskKind::Fn, sid);

    // Signature
    let (name, arguments, result) = cx.destructure_signature(&item_fn.sig)?;
    let h_block_ty = result.as_ref().map(|res| res.ty);

    // Function with signature information
    let c_arguments = arguments.clone();
    cx.insert_working_fn(sid, move |cx| {
        Ok(Function {
            name: Some(name),
            arguments: c_arguments,
            result,
            local_variables: Arena::new(),
            expressions: Arena::new(),
            named_expressions: FastIndexMap::default(),
            body: Block::new(),
            diagnostic_filter_leaf: None,
        })
    })?;

    // Update the state.
    cx.state.local = Some(sid);

    // Adds arguments as named expressions.
    for (i, arg) in arguments.into_iter().enumerate() {
        let expr = Expression::FunctionArgument(i as u32);
        let h_expr = match cx.try_eval_expr_and_append(expr) {
            Ok(h_expr) => h_expr,
            _ => unreachable!(),
        };
        if let Some(name) = arg.name {
            let f = cx.get_mut_function();
            f.named_expressions.insert(h_expr, name);
        }
    }

    // Block
    let stmt_start_idx = 0;
    block_to_naga(&item_fn.block, cx, stmt_start_idx, h_block_ty)?;

    // Restores the state.
    cx.state.local = None;

    cx.cancel_resched(sid);
    Ok(())
}

fn block_to_naga(
    block: &syn::Block,
    cx: &mut NagaCx,
    stmt_start_idx: usize,
    h_block_ty: Option<Handle<Type>>,
) -> TriResult<()> {
    let sid = block.syn_id();

    for (i, stmt) in block.stmts.iter().enumerate().skip(stmt_start_idx) {
        cx.reserve_resched(
            NagaTaskKind::Block {
                stmt_start_idx: i,
                h_block_ty,
            },
            sid,
        );

        if let Some(stmt) = stmt_to_naga(stmt, cx, h_block_ty)? {
            // TODO: If not a function context?
            cx.get_mut_function().body.push(stmt, Span::UNDEFINED);
        }

        cx.cancel_resched(sid);
    }

    // Puts empty return statement if needed.
    let f = cx.get_mut_function(); // TODO: If not a function context?
    if !matches!(f.body.last(), Some(stmt) if matches!(stmt, Statement::Return { .. })) {
        f.body
            .push(Statement::Return { value: None }, Span::UNDEFINED);
    }

    Ok(())
}

fn stmt_to_naga(
    stmt: &syn::Stmt,
    cx: &mut NagaCx,
    h_block_ty: Option<Handle<Type>>,
) -> TriResult<Option<Statement>> {
    match stmt {
        syn::Stmt::Local(local) => {
            local_to_naga(local, cx)?;
            Ok(None)
        }
        syn::Stmt::Item(item) => {
            item_to_naga(item, cx)?;
            Ok(None)
        }
        syn::Stmt::Expr(expr, semi) => {
            let h_expr = expr_to_naga(expr, cx, h_block_ty)?;
            let stmt = if semi.is_none() {
                Some(Statement::Return {
                    value: Some(h_expr),
                })
            } else {
                todo!("expr -> stmt")
            };
            Ok(stmt)
        }
        syn::Stmt::Macro(mac) => todo!("SmStmt::to_naga, {mac:?}"),
    }
}

fn local_to_naga(local: &syn::Local, cx: &mut NagaCx) -> TriResult<()> {
    let (mutability, ident, ty) = helper(&local.pat)?;
    let Some(ty) = ty else {
        return terr!(
            "{}: inference is not implemented yet",
            local.location_message()
        );
    };

    let Some(init) = &local.init else {
        return terr!(
            "{}: inference is not implemented yet",
            local.location_message()
        );
    };

    let h_ty = cx.insert_type_by_syn(ty)?;
    let h_expr = expr_to_naga(&init.expr, cx, Some(h_ty))?;

    if !mutability {
        let f = cx.get_mut_function();
        f.named_expressions.insert(h_expr, ident);
    } else {
        let f = cx.get_mut_function();
        let h_local_var = f.local_variables.append(
            LocalVariable {
                name: Some(ident),
                ty: h_ty,
                init: Some(h_expr),
            },
            Span::UNDEFINED,
        );
        cx.try_eval_expr_and_append(Expression::LocalVariable(h_local_var))?;
    }

    return Ok(());

    // === Internal helper functions ===

    fn helper(pat: &syn::Pat) -> TriResult<(bool, String, Option<&syn::Type>)> {
        match pat {
            syn::Pat::Ident(v) => {
                let mutability = v.mutability.is_some();
                let ident = v.ident.to_string();
                let ty = None;
                Ok((mutability, ident, ty))
            }
            syn::Pat::Type(v) => {
                let (mutability, ident, _) = helper(&v.pat)?;
                Ok((mutability, ident, Some(&v.ty)))
            }
            _ => {
                terr!("`{}` is not supported yet", pat.to_token_stream())
            }
        }
    }
}

fn expr_to_naga(
    expr: &syn::Expr,
    cx: &mut NagaCx,
    ty: Option<Handle<Type>>,
) -> TriResult<Handle<Expression>> {
    if let Some(handle) = cx.s2n.get_naga_handle(expr.syn_id()) {
        return Ok(handle.to_expr_handle());
    }

    match expr {
        syn::Expr::Array(v) => expr_array_to_naga(v, cx, ty),
        syn::Expr::Binary(v) => expr_binary_to_naga(v, cx, ty),
        syn::Expr::Lit(v) => expr_lit_to_naga(v, cx),
        syn::Expr::Path(v) => expr_path_to_naga(v, cx),
        o => todo!("syn::Expr::ToNaga, {o:?}"),
    }
}

fn expr_array_to_naga(
    expr_array: &syn::ExprArray,
    cx: &mut NagaCx,
    ty: Option<Handle<Type>>,
) -> TriResult<Handle<Expression>> {
    let mut components = Vec::with_capacity(expr_array.elems.len());
    for elem in &expr_array.elems {
        let h_elem = expr_to_naga(elem, cx, ty)?;
        components.push(h_elem);
    }

    cx.try_eval_expr_and_append(Expression::Compose {
        ty: ty.expect("expected handle to a type for naga array"),
        components,
    })
}

fn expr_binary_to_naga(
    expr_binary: &syn::ExprBinary,
    cx: &mut NagaCx,
    ty: Option<Handle<Type>>,
) -> TriResult<Handle<Expression>> {
    let op = bin_op_to_naga(&expr_binary.op)?;
    let h_left = expr_to_naga(&expr_binary.left, cx, ty)?;
    let h_right = expr_to_naga(&expr_binary.right, cx, ty)?;

    cx.try_eval_expr_and_append(Expression::Binary {
        op,
        left: h_left,
        right: h_right,
    })
}

fn expr_lit_to_naga(expr_lit: &syn::ExprLit, cx: &mut NagaCx) -> TriResult<Handle<Expression>> {
    let lit = lit_to_naga(&expr_lit.lit)?;
    cx.try_eval_expr_and_append(Expression::Literal(lit))
}

fn expr_path_to_naga(expr_path: &syn::ExprPath, cx: &mut NagaCx) -> TriResult<Handle<Expression>> {
    let sid = expr_path.syn_id();
    if let Some(handle) = cx.s2n.get_naga_handle(sid) {
        return Ok(handle.to_expr_handle());
    }

    // TODO: Need more general approach

    let ext: NamedPath = util::path_to_string(&expr_path.path).into();
    let pid = cx.ptree.reach(cx.state.base, ext.segments());
    let value = cx.ptree.get_value(pid);
    match value {
        PathValue::Const(c) => {
            let Some(handle) = cx.s2n.get_naga_handle(c.syn_id()) else {
                return Err(Fail::Soft);
            };
            let expr = Expression::Constant(handle.to_const_handle());
            cx.try_eval_expr_and_append(expr.clone())
        }
        _ => todo!("@@@ syn::ExprPath::to_naga"),
    }
}

fn bin_op_to_naga(bin_op: &syn::BinOp) -> TriResult<BinaryOperator> {
    Ok(match bin_op {
        syn::BinOp::Add(_) => BinaryOperator::Add,
        syn::BinOp::Sub(_) => BinaryOperator::Subtract,
        syn::BinOp::Mul(_) => BinaryOperator::Multiply,
        syn::BinOp::Div(_) => BinaryOperator::Divide,
        syn::BinOp::Rem(_) => BinaryOperator::Modulo,
        syn::BinOp::And(_) => BinaryOperator::LogicalAnd,
        syn::BinOp::Or(_) => BinaryOperator::LogicalOr,
        syn::BinOp::BitXor(_) => BinaryOperator::ExclusiveOr,
        syn::BinOp::BitAnd(_) => BinaryOperator::And,
        syn::BinOp::BitOr(_) => BinaryOperator::InclusiveOr,
        syn::BinOp::Shl(_) => BinaryOperator::ShiftLeft,
        syn::BinOp::Shr(_) => BinaryOperator::ShiftRight,
        syn::BinOp::Eq(_) => BinaryOperator::Equal,
        syn::BinOp::Lt(_) => BinaryOperator::Less,
        syn::BinOp::Le(_) => BinaryOperator::LessEqual,
        syn::BinOp::Ne(_) => BinaryOperator::NotEqual,
        syn::BinOp::Ge(_) => BinaryOperator::GreaterEqual,
        syn::BinOp::Gt(_) => BinaryOperator::Greater,
        // These operators must be replaced with alternative operators.
        syn::BinOp::AddAssign(_)
        | syn::BinOp::SubAssign(_)
        | syn::BinOp::MulAssign(_)
        | syn::BinOp::DivAssign(_)
        | syn::BinOp::RemAssign(_)
        | syn::BinOp::BitXorAssign(_)
        | syn::BinOp::BitAndAssign(_)
        | syn::BinOp::BitOrAssign(_)
        | syn::BinOp::ShlAssign(_)
        | syn::BinOp::ShrAssign(_) => {
            unreachable!("{}: unhandled operator", bin_op.location_message())
        }
        o => return terr!("yet unsupported operator: `{o:?}`"), /* Non-exhaustive */
    })
}

fn lit_to_naga(lit: &syn::Lit) -> TriResult<Literal> {
    macro_rules! unwrap {
        ($($t:tt)*) => {
            match $($t)* {
                Ok(t) => t,
                Err(e) => return terr!(e),
            }
        };
    }

    Ok(match lit {
        syn::Lit::Int(v) => match v.suffix() {
            "i8" | "i16" | "i32" => Literal::I32(unwrap!(v.base10_parse())),
            "u8" | "u16" | "u32" => Literal::U32(unwrap!(v.base10_parse())),
            "i64" => Literal::I64(unwrap!(v.base10_parse())),
            "u64" => Literal::U64(unwrap!(v.base10_parse())),
            "i128" | "u128" => return terr!("128-bits is not allowed"),
            "isize" | "usize" | "" | _ => Literal::AbstractInt(unwrap!(v.base10_parse())),
        },
        syn::Lit::Float(v) => match v.suffix() {
            "f32" => Literal::F32(unwrap!(v.base10_parse())),
            "f64" => Literal::F64(unwrap!(v.base10_parse())),
            "" | _ => Literal::AbstractFloat(unwrap!(v.base10_parse())),
        },
        syn::Lit::Bool(v) => Literal::Bool(v.value()),
        _ => return terr!("{}: unsupported literal", lit.content()),
    })
}

fn field_to_naga(field: &syn::Field, cx: &mut NagaCx, mut offset: u32) -> TriResult<StructMember> {
    // Finds naga handle of this field.
    cx.insert_type_by_syn(&field.ty).map(|h_ty| {
        // Constructs naga struct member using the type handle.
        let layout = cx.layouter[h_ty];
        offset = layout.alignment.round_up(offset);
        StructMember {
            name: field.ident.as_ref().map(ToString::to_string),
            ty: h_ty,
            binding: None,
            offset,
        }
    })
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
        return Err(format!("`{expr:?}` cannot be u32").into());
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
