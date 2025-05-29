use super::{known, path_tree::*};
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
use indexmap::IndexSet;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::{self, Write},
    iter, mem,
    path::{Path as StdPath, PathBuf},
    pin::Pin,
    ptr,
};
use syn_locator::Locate;

#[cfg(test)]
thread_local! {
    pub(crate) static TEST_FILES: std::cell::RefCell<HashMap<PathBuf, &'static str>> = std::cell::RefCell::new(HashMap::new());
}

pub struct PathProcessor {
    /// Syntax tree nodes never change after they are constructed.
    pub stree: HashMap<PathBuf, Pin<Box<SmFile>>>,
    pub ptree: PathTree,
    pub s2p: SynToPath,
    pub trait_impl: TraitImplTable,
    construct_tasks: VecDeque<ConstructTask>,
    impl_tasks: VecDeque<ResolveImplTask>,
}

impl PathProcessor {
    pub fn new(entry: &StdPath) -> Result<Self> {
        let mut ptree = PathTree::new();

        // Inserts 'crate' as a module to be distinguished from externs.
        let dummy = PathValue::Mod(PathMod {
            syn: ptr::null(),
            syn_file: None,
            vis_node: PathTree::ROOT,
            fpath: entry.to_path_buf(),
            mod_rs: false,
        });
        let pid = ptree.append_value(PathTree::ROOT, iter::once("crate"), dummy);
        ptree[pid].as_mut_mod().vis_node = pid.ni;

        Ok(Self {
            stree: HashMap::new(),
            ptree,
            s2p: SynToPath::new(),
            trait_impl: TraitImplTable::new(),
            construct_tasks: VecDeque::new(),
            impl_tasks: VecDeque::new(),
        })
    }

    pub fn import<I, II>(&mut self, fpaths: I) -> Result<()>
    where
        I: IntoIterator<Item = II>,
        II: AsRef<StdPath>,
    {
        for fpath in fpaths {
            let fpath = fpath.as_ref();
            let npath = NamedPath::from_path(&fpath)?;
            self.construct_tasks.push_back(ConstructTask {
                fpath: fpath.to_path_buf(),
                npath,
            });
        }
        Ok(())
    }

    pub fn process(&mut self) -> Result<()> {
        // Constructs path tree.
        let mut c = Constructor {
            stree: mem::take(&mut self.stree),
            ptree: mem::take(&mut self.ptree),
            s2p: mem::take(&mut self.s2p),
            construct_tasks: mem::take(&mut self.construct_tasks),
            impl_tasks: mem::take(&mut self.impl_tasks),
        };
        c.construct()?;
        let Constructor {
            stree,
            ptree,
            s2p,
            impl_tasks,
            ..
        } = c;

        // Resolves some path values.
        let mut r = Resolver {
            stree,
            ptree,
            s2p,
            trait_impl: TraitImplTable::new(),
            impl_tasks,
        };
        r.resolve()?;
        let Resolver {
            stree, ptree, s2p, ..
        } = r;

        self.stree = stree;
        self.ptree = ptree;
        self.s2p = s2p;

        Ok(())
    }
}

#[derive(Debug, Default)]
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

struct Constructor {
    stree: HashMap<PathBuf, Pin<Box<SmFile>>>,
    ptree: PathTree,
    s2p: SynToPath,
    construct_tasks: VecDeque<ConstructTask>,
    impl_tasks: VecDeque<ResolveImplTask>,
}

impl Constructor {
    fn construct(&mut self) -> Result<()> {
        // std is basically included.
        add_known_file(
            &mut self.stree,
            &mut self.ptree,
            &mut self.s2p,
            &mut self.impl_tasks,
            "std.rs",
        )?;

        while let Some(task) = self.construct_tasks.pop_front() {
            let ConstructTask { fpath, npath } = task;

            let fpath = if fpath.is_absolute() {
                fpath.to_path_buf()
            } else {
                util::to_absolute_path(fpath)?
            };

            // Reads the file then parses it.
            if !self.stree.contains_key(&fpath) {
                #[cfg(not(test))]
                let code = std::fs::read_to_string(&fpath)?;

                #[cfg(test)]
                let code = TEST_FILES.with_borrow(|map| (*map.get(&fpath).unwrap()).to_owned());

                let file = SmFile::new(fpath.clone(), code)?;
                self.stree.insert(fpath.clone(), file);
            }

            // Expands the path tree according to the parsed file.
            let mut cx = ConstructCx {
                stree: &self.stree,
                ptree: &mut self.ptree,
                s2p: &mut self.s2p,
                construct_tasks: &mut self.construct_tasks,
                impl_tasks: &mut self.impl_tasks,
                path: npath,
            };
            let file = self.stree.get(&fpath).unwrap();
            cx.add_file(file)?;
        }
        Ok(())
    }
}

struct ConstructCx<'a> {
    stree: &'a HashMap<PathBuf, Pin<Box<SmFile>>>,
    ptree: &'a mut PathTree,
    s2p: &'a mut SynToPath,
    construct_tasks: &'a mut VecDeque<ConstructTask>,
    impl_tasks: &'a mut VecDeque<ResolveImplTask>,
    path: NamedPath,
}

impl ConstructCx<'_> {
    fn add_file(&mut self, file: &SmFile) -> Result<()> {
        let sid = file.syn_id();
        let key = self.path.segments();
        let ni = self.ptree.search(PathTree::ROOT, key.clone());
        let values = self.ptree.search_mut_values(key.clone());
        let empty_values = values.is_empty();

        // If the file was loaded by 'mod' from another file, we just set the
        // file pointer.
        for (vi, value) in values {
            match value {
                PathValue::Mod(v) => {
                    v.syn_file = Some(file as *const _);
                    self.s2p.add_syn_to_path(sid, ni.unwrap().to_path_id(vi));
                }
                PathValue::ModRaw(v) => {
                    v.syn_file = Some(file as *const _);
                    self.s2p.add_syn_to_path(sid, ni.unwrap().to_path_id(vi));
                }
                _ => {}
            }
        }

        // If the file was loaded as entry file, we need to append new module
        // for this file.
        if empty_values {
            let value = PathValue::ModRaw(PathModRaw {
                syn: ptr::null(),
                syn_file: Some(file as *const _),
                vis_node: None,
                fpath: file.abs_path.clone(),
                mod_rs: util::is_mod_rs(&file.abs_path),
            });
            let pid = self.ptree.append_value(PathTree::ROOT, key, value);
            self.s2p.add_syn_to_path(sid, pid);
        }

        // Regardless of how the file was loaded, inserts items in the path
        // tree.
        for item in &file.file.items {
            self.add_item(item)?;
        }

        Ok(())
    }

    fn add_item(&mut self, item: &syn::Item) -> Result<()> {
        // Tracks only when the path has been initialzied by tracking a file.
        if self.path.is_empty() {
            return Ok(());
        }

        // Items that make 'new' paths need to be inserted to the path tree.
        match item {
            syn::Item::Const(v) => self.add_item_const(v),
            syn::Item::Enum(_) => Ok(()),
            syn::Item::ExternCrate(_) => Ok(()),
            syn::Item::Fn(v) => self.add_item_fn(v),
            syn::Item::ForeignMod(_) => Ok(()),
            syn::Item::Impl(v) => Ok(self.add_item_impl(v)),
            syn::Item::Macro(_) => Ok(()),
            syn::Item::Mod(v) => self.add_item_mod(v),
            syn::Item::Static(_) => Ok(()),
            syn::Item::Struct(v) => self.add_item_struct(v),
            syn::Item::Trait(v) => self.add_item_trait(v),
            syn::Item::TraitAlias(_) => Ok(()),
            syn::Item::Type(v) => self.add_item_type(v),
            syn::Item::Union(_) => Ok(()),
            syn::Item::Use(v) => self.add_item_use(v),
            syn::Item::Verbatim(_) => Ok(()),
            _ => Ok(()), // Non-exhaustive
        }
    }

    fn add_item_const(&mut self, item_const: &syn::ItemConst) -> Result<()> {
        let org_len = self.path.len();
        util::push_colon_path(&mut self.path, &item_const.ident);

        let key = self.path.segments();
        let pid = self.ptree.append_value(
            PathTree::ROOT,
            key,
            PathValue::ConstRaw(PathConstRaw {
                syn: item_const as *const _,
                vis_node: None,
            }),
        );
        let sid = item_const.syn_id();
        self.s2p.add_syn_to_path(sid, pid);

        // Appends extern if needed.
        self.try_add_extern_by_type(&item_const.ty);

        self.path.truncate(org_len);
        Ok(())
    }

    fn add_item_fn(&mut self, item_fn: &syn::ItemFn) -> Result<()> {
        let org_len = self.path.len();
        util::push_colon_path(&mut self.path, &item_fn.sig.ident);

        let key = self.path.segments();
        let pid = self.ptree.append_value(
            PathTree::ROOT,
            key,
            PathValue::FnRaw(PathFnRaw {
                syn: item_fn as *const _,
                vis_node: None,
            }),
        );
        let sid = item_fn.syn_id();
        self.s2p.add_syn_to_path(sid, pid);

        // Signature & Block
        self.add_signature(&item_fn.sig)?;
        let mut state = BlockState { nth: 0 };
        self.add_block(&item_fn.block, &mut state)?;

        self.path.truncate(org_len);
        Ok(())
    }

    fn add_item_impl(&mut self, item_impl: &syn::ItemImpl) {
        let key = self.path.segments();
        let base = self.ptree.search(PathTree::ROOT, key).unwrap();
        self.impl_tasks.push_back(ResolveImplTask {
            syn: item_impl as *const _,
            base,
        });

        // Appends self type if needed.
        self.try_add_extern_by_type(&item_impl.self_ty);
    }

    fn add_item_mod(&mut self, item_mod: &syn::ItemMod) -> Result<()> {
        // Terminology
        // * inline - Module is inline when it contains its content within {}.
        // * fpath - File path of the module.
        //   - "/home/a.rs" if non-inline module.
        //   - "/home/a" if inline module.
        // * mod_rs - Whether the module is a `mod-rs` or not.
        //   - `mod-rs` if the file is one of "mod.rs", "main.rs", or "lib.rs".
        //   - `mod-rs` if the file is determined by "path" attribute and the
        //     module is not inline (e.g. #[path = "a.rs"] mod foo;)
        //   - `non-mod-rs` otherwise.
        // ref: https://doc.rust-lang.org/reference/items/modules.html

        // Retrieves parent module info.
        let ni = self
            .ptree
            .search(PathTree::ROOT, self.path.segments())
            .unwrap();
        let parent_pid = self.ptree.nearest_module(ni);
        let (parent_fpath, parent_mod_rs, parent_inline) = match &self.ptree[parent_pid] {
            PathValue::Mod(v) => (&v.fpath, v.mod_rs, v.is_inline()),
            PathValue::ModRaw(v) => (&v.fpath, v.mod_rs, v.is_inline()),
            _ => unreachable!(),
        };

        // Directory containing a file that declares this module with or without
        // content.
        let dir = parent_fpath.parent().unwrap();

        let inline = item_mod.content.is_some();

        // Determines this module's file path and `mod-rs`.
        let fpath;
        let mod_rs;
        if let Some(path_attr) = item_mod.get_attribute_value("path") {
            fpath = if !parent_inline {
                dir.join(path_attr.trim_matches('"'))
            } else {
                parent_fpath
                    .with_extension("")
                    .join(path_attr.trim_matches('"'))
            };
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

                #[cfg(not(test))]
                let (exist_a, exist_b) = (fpath_a.exists(), fpath_b.exists());

                #[cfg(test)]
                let (exist_a, exist_b) = TEST_FILES
                    .with_borrow(|map| (map.contains_key(&fpath_a), map.contains_key(&fpath_b)));

                match (exist_a, exist_b) {
                    (true, false) => {
                        fpath = fpath_a;
                        mod_rs = false;
                    }
                    (false, true) => {
                        fpath = fpath_b;
                        mod_rs = true;
                    }
                    (true, true) => {
                        return Err(format!("`{fpath_a:?}` and `{fpath_b:?}` conflict.").into());
                    }
                    (false, false) => {
                        return Err(format!("expected `{fpath_a:?}` or `{fpath_b:?}`").into());
                    }
                }
            }
        };

        // Enters the scope.
        let org_len = self.path.len();
        util::push_colon_path(&mut self.path, &item_mod.ident);

        let key = self.path.segments();
        let pid = self.ptree.append_value(
            PathTree::ROOT,
            key,
            PathValue::ModRaw(PathModRaw {
                syn: item_mod as *const _,
                syn_file: None,
                vis_node: None,
                fpath,
                mod_rs,
            }),
        );
        let sid = item_mod.syn_id();
        self.s2p.add_syn_to_path(sid, pid);

        // Internal items.
        if let Some((_, items)) = &item_mod.content {
            for item in items.iter() {
                self.add_item(item)?;
            }
        }

        // Exits the scope.
        self.path.truncate(org_len);

        // Appends a task for the new file if necessary.
        let value = self.ptree[pid].as_mod_raw();
        if !inline && !self.stree.contains_key(&value.fpath) {
            self.construct_tasks.push_back(ConstructTask {
                fpath: value.fpath.clone(),
                npath: self.ptree.named_path(pid.ni),
            });
        }

        Ok(())
    }

    fn add_item_struct(&mut self, item_struct: &syn::ItemStruct) -> Result<()> {
        let org_len = self.path.len();
        util::push_colon_path(&mut self.path, &item_struct.ident);

        let key = self.path.segments();
        let pid = self.ptree.append_value(
            PathTree::ROOT,
            key,
            PathValue::StructRaw(PathStructRaw {
                syn: item_struct as *const _,
                vis_node: None,
            }),
        );
        let sid = item_struct.syn_id();
        self.s2p.add_syn_to_path(sid, pid);

        // Fields
        for (nth, field) in item_struct.fields.iter().enumerate() {
            self.add_field(field, nth as u32)?;
        }

        self.path.truncate(org_len);
        Ok(())
    }

    fn add_item_trait(&mut self, item_trait: &syn::ItemTrait) -> Result<()> {
        let org_len = self.path.len();
        util::push_colon_path(&mut self.path, &item_trait.ident);

        let key = self.path.segments();
        let pid = self.ptree.append_value(
            PathTree::ROOT,
            key,
            PathValue::TraitRaw(PathTraitRaw {
                syn: item_trait as *const _,
                vis_node: None,
            }),
        );
        let sid = item_trait.syn_id();
        self.s2p.add_syn_to_path(sid, pid);

        self.path.truncate(org_len);
        Ok(())
    }

    fn add_item_type(&mut self, item_type: &syn::ItemType) -> Result<()> {
        let org_len = self.path.len();
        util::push_colon_path(&mut self.path, &item_type.ident);

        let key = self.path.segments();
        let pid = self.ptree.append_value(
            PathTree::ROOT,
            key,
            PathValue::TypeAliasRaw(PathTypeAliasRaw {
                syn: item_type as *const _,
                vis_node: None,
            }),
        );
        let sid = item_type.syn_id();
        self.s2p.add_syn_to_path(sid, pid);

        // Appends extern if needed.
        self.try_add_extern_by_type(&item_type.ty);

        self.path.truncate(org_len);
        Ok(())
    }

    fn add_item_use(&mut self, item_use: &syn::ItemUse) -> Result<()> {
        fn dfs(
            this: &mut ConstructCx,
            item: &syn::ItemUse,
            node: &syn::UseTree,
            vis: PathVis,
            buf: &mut String,
        ) {
            match node {
                syn::UseTree::Path(v) => {
                    let org_len = buf.len();
                    write!(buf, "{}::", v.ident).unwrap();
                    dfs(this, item, &v.tree, vis, buf);
                    buf.truncate(org_len);
                }
                syn::UseTree::Name(v) => {
                    let org_len = this.path.len();
                    util::push_colon_path(&mut this.path, &v.ident);

                    let detail = v.syn_id();
                    add_path_value(this, item, detail, format!("{buf}{}", v.ident));

                    this.path.truncate(org_len);
                }
                syn::UseTree::Rename(v) => {
                    let org_len = this.path.len();
                    util::push_colon_path(&mut this.path, &v.rename);

                    let detail = v.syn_id();
                    add_path_value(this, item, detail, format!("{buf}{}", v.ident));

                    this.path.truncate(org_len);
                }
                syn::UseTree::Glob(v) => {
                    let org_len = this.path.len();
                    this.path.push_str("::*");

                    let detail = v.syn_id();
                    add_path_value(this, item, detail, format!("{buf}*"));

                    this.path.truncate(org_len);
                }
                syn::UseTree::Group(v) => {
                    for node in &v.items {
                        dfs(this, item, node, vis.clone(), buf);
                    }
                }
            }
        }

        fn add_path_value(
            this: &mut ConstructCx,
            item: &syn::ItemUse,
            detail: SynId,
            npath: String,
        ) {
            let key = this.path.segments();
            let pid = this.ptree.append_value(
                PathTree::ROOT,
                key,
                PathValue::UseRaw(PathUseRaw {
                    syn: item as *const _,
                    detail,
                    vis_node: None,
                    npath: npath.into(),
                    dst_node: None,
                }),
            );
            this.s2p.add_syn_to_path(detail, pid);
        }

        let mut buf = String::new();
        dfs(
            self,
            item_use,
            &item_use.tree,
            PathVis::new(&item_use.vis),
            &mut buf,
        );
        Ok(())
    }

    fn add_field(&mut self, field: &syn::Field, nth: u32) -> Result<()> {
        let org_len = self.path.len();
        if let Some(ident) = &field.ident {
            util::push_colon_path(&mut self.path, ident);
        } else {
            util::push_colon_path(&mut self.path, nth);
        }

        let key = self.path.segments();
        let pid = self.ptree.append_value(
            PathTree::ROOT,
            key,
            PathValue::FieldRaw(PathFieldRaw {
                syn: field as *const _,
                vis_node: None,
            }),
        );
        let sid = field.syn_id();
        self.s2p.add_syn_to_path(sid, pid);

        // Appends extern if needed.
        self.try_add_extern_by_type(&field.ty);

        self.path.truncate(org_len);
        Ok(())
    }

    fn add_signature(&mut self, sig: &syn::Signature) -> Result<()> {
        for input in &sig.inputs {
            match input {
                syn::FnArg::Receiver(..) => todo!(),
                syn::FnArg::Typed(v) => self.add_local_pat_type(v),
            }
        }

        // Appends output type if needed.
        if let syn::ReturnType::Type(_, ty) = &sig.output {
            self.try_add_extern_by_type(ty);
        }

        Ok(())
    }

    fn add_block(&mut self, block: &syn::Block, state: &mut BlockState) -> Result<()> {
        let org_len = self.path.len();
        util::push_colon_path(&mut self.path, format!("{{{}}}", state.nth));
        state.nth += 1;

        let key = self.path.segments();
        let pid = self.ptree.append_value(
            PathTree::ROOT,
            key,
            PathValue::Block(PathBlock {
                syn: block as *const _,
            }),
        );
        let sid = block.syn_id();
        self.s2p.add_syn_to_path(sid, pid);

        // Statements
        let mut state = BlockState { nth: 0 };
        for stmt in &block.stmts {
            self.add_stmt(stmt, &mut state)?;
        }

        self.path.truncate(org_len);
        Ok(())
    }

    fn add_stmt(&mut self, stmt: &syn::Stmt, state: &mut BlockState) -> Result<()> {
        return match stmt {
            syn::Stmt::Local(v) => {
                self.add_local_pat(&v.pat);

                // Appends extern if needed.
                if let Some(init) = &v.init {
                    self.try_add_extern_by_expr(&init.expr)?;
                }
                Ok(())
            }
            syn::Stmt::Item(v) => self.add_item(v),
            syn::Stmt::Expr(v, _) => self.add_expr(v, state),
            syn::Stmt::Macro(..) => Ok(()),
        };
    }

    fn add_expr(&mut self, expr: &syn::Expr, state: &mut BlockState) -> Result<()> {
        match expr {
            syn::Expr::Block(syn::ExprBlock { block, .. }) => self.add_block(block, state),
            syn::Expr::Assign(v) => self.add_expr_assign(v),
            _ => Ok(()),
        }
    }

    fn add_expr_assign(&mut self, expr_assign: &syn::ExprAssign) -> Result<()> {
        // Appends extern if needed.
        self.try_add_extern_by_expr(&expr_assign.right)
    }

    fn add_local_pat(&mut self, pat: &syn::Pat) {
        match pat {
            syn::Pat::Ident(v) => self.add_local_pat_ident(v),
            syn::Pat::Struct(v) => self.add_local_pat_struct(v),
            syn::Pat::Tuple(v) => self.add_local_pat_tuple(v),
            syn::Pat::Type(v) => self.add_local_pat_type(v),
            _ => {}
        }
    }

    fn add_local_pat_struct(&mut self, pat_struct: &syn::PatStruct) {
        for field in &pat_struct.fields {
            self.add_local_pat(&field.pat);
        }
    }

    fn add_local_pat_tuple(&mut self, pat_tuple: &syn::PatTuple) {
        for elem in &pat_tuple.elems {
            self.add_local_pat(elem);
        }
    }

    fn add_local_pat_type(&mut self, pat_type: &syn::PatType) {
        // Appends extern if needed.
        self.try_add_extern_by_type(&pat_type.ty);

        match &*pat_type.ty {
            syn::Type::Path(_)
            | syn::Type::Array(_)
            | syn::Type::Slice(_)
            | syn::Type::Reference(_) => self.add_local_pat(&pat_type.pat),
            syn::Type::Tuple(..) => match &*pat_type.pat {
                syn::Pat::Tuple(pat_tuple) => {
                    for elem in &pat_tuple.elems {
                        self.add_local_pat(elem);
                    }
                }
                syn::Pat::Ident(pat_ident) => self.add_local_pat_ident(pat_ident),
                o => unreachable!("{o:?}"),
            },
            _ => {}
        }
    }

    fn add_local_pat_ident(&mut self, pat_ident: &syn::PatIdent) {
        let org_len = self.path.len();
        util::push_colon_path(&mut self.path, &pat_ident.ident);

        let key = self.path.segments();
        let pid = self.ptree.append_value(
            PathTree::ROOT,
            key,
            PathValue::LocalRaw(PathLocalRaw {
                syn: pat_ident as *const _,
                mut_: pat_ident.mutability.is_some(),
            }),
        );
        let sid = pat_ident.syn_id();
        self.s2p.add_syn_to_path(sid, pid);

        self.path.truncate(org_len);
    }

    /// Adds a known extern type to the path tree if necessary.
    fn try_add_extern_by_type(&mut self, ty: &syn::Type) {
        match ty {
            syn::Type::Path(v) => {
                let ext: NamedPath = util::path_to_string(&v.path).into();
                self.try_add_extern_by_path(&ext);
            }
            syn::Type::Array(v) => self.try_add_extern_by_type(&v.elem),
            syn::Type::Reference(v) => self.try_add_extern_by_type(&v.elem),
            syn::Type::Slice(v) => self.try_add_extern_by_type(&v.elem),
            syn::Type::Tuple(v) => {
                for elem in &v.elems {
                    self.try_add_extern_by_type(elem);
                }
            }
            _ => {} // Not a known extern type
        }
    }

    fn try_add_extern_by_expr(&mut self, expr: &syn::Expr) -> Result<()> {
        match expr {
            syn::Expr::Binary(v) => {
                self.try_add_extern_by_expr(&v.left)?;
                self.try_add_extern_by_expr(&v.right)?;
            }
            syn::Expr::Cast(v) => self.try_add_extern_by_type(&v.ty),
            syn::Expr::Lit(v) => match &v.lit {
                syn::Lit::Int(v) => {
                    let path: NamedPath = v.suffix().to_owned().into();
                    self.try_add_extern_by_path(&path);
                }
                syn::Lit::Float(v) => {
                    let path: NamedPath = v.suffix().to_owned().into();
                    self.try_add_extern_by_path(&path);
                }
                syn::Lit::Bool(v) => todo!("{v:?}"),
                o => unreachable!("{o:?}"),
            },
            _ => {}
        };

        Ok(())
    }

    fn try_add_extern_by_path(&mut self, ext: &NamedPath) {
        if known::is_known_primitive(&ext)
            && self.ptree.search(PathTree::ROOT, ext.segments()).is_none()
        {
            self.ptree
                .append_value(PathTree::ROOT, ext.segments(), PathValue::Extern);
        }
    }
}

#[derive(Debug)]
struct ConstructTask {
    fpath: PathBuf,
    npath: NamedPath,
}

#[derive(Debug)]
struct ResolveImplTask {
    syn: *const syn::ItemImpl,
    base: NodeIndex,
}

#[derive(Clone, Copy)]
struct BlockState {
    nth: u32,
}

struct Resolver {
    stree: HashMap<PathBuf, Pin<Box<SmFile>>>,
    ptree: PathTree,
    s2p: SynToPath,
    trait_impl: TraitImplTable,
    impl_tasks: VecDeque<ResolveImplTask>,
}

impl Resolver {
    fn resolve(&mut self) -> Result<()> {
        loop {
            self.resolve_visibility();
            self.resolve_use()?;
            let changed = self.add_known_lib_file()?;
            if !changed {
                break;
            }
        }

        self.resolve_type();
        self.resolve_impl()?;

        // Resolves local variables.
        let mut l = LocalResolver {
            ptree: mem::take(&mut self.ptree),
            s2p: mem::take(&mut self.s2p),
        };
        l.resolve()?;
        let LocalResolver { ptree, s2p } = l;

        self.ptree = ptree;
        self.s2p = s2p;

        Ok(())
    }

    /// Resolves visibility and completes path values below.
    /// - ModRaw -> Mod
    /// - StructRaw -> Struct
    /// - TraitRaw -> Trait
    fn resolve_visibility(&mut self) {
        let mut targets = Vec::new();
        for (pid, value) in self.ptree.iter() {
            match value {
                // Fully resolve
                PathValue::ModRaw(v) => {
                    targets.push((pid, find_vis_node(&self.ptree, pid.ni, &v.visibility())))
                }
                PathValue::StructRaw(v) => {
                    targets.push((pid, find_vis_node(&self.ptree, pid.ni, &v.visibility())))
                }
                PathValue::TraitRaw(v) => {
                    targets.push((pid, find_vis_node(&self.ptree, pid.ni, &v.visibility())))
                }
                // Visibility only
                PathValue::ConstRaw(v) => {
                    targets.push((pid, find_vis_node(&self.ptree, pid.ni, &v.visibility())))
                }
                PathValue::FieldRaw(v) => {
                    targets.push((pid, find_vis_node(&self.ptree, pid.ni, &v.visibility())))
                }
                PathValue::FnRaw(v) => {
                    targets.push((pid, find_vis_node(&self.ptree, pid.ni, &v.visibility())))
                }
                PathValue::TypeAliasRaw(v) => {
                    targets.push((pid, find_vis_node(&self.ptree, pid.ni, &v.visibility())))
                }
                PathValue::UseRaw(v) => {
                    targets.push((pid, find_vis_node(&self.ptree, pid.ni, &v.visibility())))
                }
                _ => {}
            }
        }

        for (pid, vis_node) in targets {
            let value = &mut self.ptree[pid];
            match value {
                // Fully resolve
                PathValue::ModRaw(v) => {
                    *value = PathValue::Mod(PathMod {
                        syn: v.syn,
                        syn_file: v.syn_file,
                        vis_node,
                        fpath: mem::take(&mut v.fpath),
                        mod_rs: v.mod_rs,
                    });
                }
                PathValue::StructRaw(v) => {
                    *value = PathValue::Struct(PathStruct {
                        syn: v.syn,
                        vis_node,
                    });
                }
                PathValue::TraitRaw(v) => {
                    *value = PathValue::Trait(PathTrait {
                        syn: v.syn,
                        vis_node,
                    });
                }
                // Visibility only
                PathValue::ConstRaw(v) => v.vis_node = Some(vis_node),
                PathValue::FieldRaw(v) => v.vis_node = Some(vis_node),
                PathValue::FnRaw(v) => v.vis_node = Some(vis_node),
                PathValue::TypeAliasRaw(v) => v.vis_node = Some(vis_node),
                PathValue::UseRaw(v) => v.vis_node = Some(vis_node),
                _ => unreachable!(),
            }
        }

        // === Internal helper functions ===

        fn find_vis_node(ptree: &PathTree, ni: NodeIndex, vis: &PathVis) -> NodeIndex {
            enum Wrapper {
                Root,
                Mod,
                Block,
                Fn,
                Struct,
            }

            let climb = |ni: NodeIndex| {
                let mut parent = ptree[ni].parent;
                let mut values = ptree[parent].iter();

                // There are no values between 'crate' and the entry module. In
                // that case, goes up to the 'crate'.
                while parent != PathTree::ROOT && values.is_empty() {
                    parent = ptree[parent].parent;
                    values = ptree[parent].iter();
                }

                for (_, value) in ptree[parent].iter() {
                    match value {
                        PathValue::Mod(_) | PathValue::ModRaw(_) => {
                            return (Wrapper::Mod, parent);
                        }
                        PathValue::Block(_) => return (Wrapper::Block, parent),
                        PathValue::Fn(_) | PathValue::FnRaw(_) => return (Wrapper::Fn, parent),
                        PathValue::Struct(_) | PathValue::StructRaw(_) => {
                            return (Wrapper::Struct, parent);
                        }
                        _ => {}
                    }
                }

                debug_assert_eq!(parent, PathTree::ROOT);
                (Wrapper::Root, parent)
            };

            // If we meet a block during climbing, then stops.
            let climb_to = |mut ni: NodeIndex, dst: NodeIndex| {
                while ni != dst {
                    let (w, p) = climb(ni);
                    ni = p;
                    if matches!(w, Wrapper::Block) {
                        break;
                    }
                }
                ni
            };

            match vis {
                PathVis::Pub => climb_to(ni, PathTree::ROOT),
                PathVis::PubCrate => {
                    let dst = ptree.search(PathTree::ROOT, iter::once("crate")).unwrap();
                    climb_to(ni, dst)
                }
                PathVis::PubSuper => {
                    let cont = ptree.parent_container(ni, [ContainerKind::Mod]).ni;
                    let dst = ptree.parent_container(cont, [ContainerKind::Mod]).ni;
                    climb_to(ni, dst)
                }
                PathVis::PubPath(path) => {
                    let cont = ptree.parent_container(ni, [ContainerKind::Mod]).ni;
                    let (base, ext) = ptree.normalize_search_condition(cont, path.segments());
                    let dst = ptree.search(base, ext).unwrap();
                    climb_to(ni, dst)
                }
                PathVis::Private => {
                    let dst = ptree.parent_container(ni, [ContainerKind::Mod]).ni;
                    climb_to(ni, dst)
                }
            }
        }
    }

    fn resolve_use(&mut self) -> Result<()> {
        // Gathers glob and non-glob `UseRaw`s.
        let mut globs = VecDeque::new();
        let mut non_globs = VecDeque::new();
        for (pid, value) in self.ptree.iter() {
            if matches!(value, PathValue::UseRaw(_)) {
                if matches!(value.as_use_raw().npath.segments().last(), Some("*")) {
                    globs.push_back(pid);
                } else {
                    non_globs.push_back(pid);
                }
            }
        }

        let mut changed = true;
        while changed && (!globs.is_empty() || !non_globs.is_empty()) {
            changed = false;

            // Tries to expand all globs.
            let n = globs.len();
            for _ in 0..n {
                let pid = globs.pop_front().unwrap();

                let mut visit = HashSet::new();
                if !expand_glob(&mut self.ptree, pid, &mut visit, &mut non_globs) {
                    globs.push_back(pid);
                    continue;
                }

                self.ptree.take_value(pid);
                changed = true;
            }

            // Tries to search destinations of `UseRaw`s.
            for pid in &non_globs {
                if self.ptree[*pid].as_use_raw().dst_node.is_some() {
                    continue;
                }

                let dst = search_dst_of_use_raw(&self.ptree, *pid);
                let raw = self.ptree[*pid].as_mut_use_raw();
                changed |= raw.dst_node != dst;
                raw.dst_node = dst;
            }

            // Tries to turn `UseRaw`s into `Use`s.
            let n = non_globs.len();
            for _ in 0..n {
                let pid = non_globs.pop_front().unwrap();

                let mut visit = HashSet::new();
                let mut terminals = Vec::new();
                if !find_terminal_of_use_raw(&self.ptree, pid, &mut visit, &mut terminals) {
                    non_globs.push_back(pid);
                    continue;
                }

                let old = self.ptree.take_value(pid);

                for terminal in terminals {
                    if terminal.ni == pid.ni {
                        continue;
                    }
                    let resolved = PathValue::Use(PathUse {
                        vis_node: old.as_use_raw().vis_node.unwrap(),
                        dst: terminal,
                    });
                    self.ptree[pid.ni].push(resolved);
                }
                changed = true;
            }
        }

        remove_duplicate(&mut self.ptree);

        return Ok(());

        // === Internal helper functions ===

        fn expand_glob(
            ptree: &mut PathTree,
            src: PathId,
            visit: &mut HashSet<PathId>,
            non_globs: &mut VecDeque<PathId>,
        ) -> bool {
            if !visit.insert(src) {
                return true;
            }

            let dst = search_dst_of_use_raw(ptree, src);
            let Some(dst) = dst else {
                return false;
            };

            // 1st traverse: Expands globs in the destination module first.
            let num_children = ptree[dst].children.len();
            for child in 0..num_children {
                let (path, node) = &ptree[dst].children[child];
                if path != "*" {
                    continue;
                }
                let node = *node;

                let num_values = ptree[node].values.len();
                for vi in 0..num_values {
                    let pid = node.to_path_id(vi);
                    let value = &ptree[pid];
                    if value == &PathValue::None {
                        continue;
                    }
                    let Some(vis_node) = value.vis_node() else {
                        continue;
                    };
                    if !ptree.is_descendant(pid.ni, vis_node) {
                        continue;
                    }
                    if !expand_glob(ptree, pid, visit, non_globs) {
                        return false;
                    }
                }
            }

            // 2nd traverse
            // - Visits non-glob items of the destination module including
            //   expanded items by the 1st traverse.
            // - Expands the path tree for the items.
            // - The expansion is not recursive, which means it brings direct
            //   children only.
            let mut entries = Vec::new();
            for (path, dst_ni) in &ptree[dst].children {
                if path == "*" {
                    continue;
                }

                for (_, value) in ptree[*dst_ni].iter() {
                    let Some(vis_node) = value.vis_node() else {
                        continue;
                    };
                    if !ptree.is_descendant(src.ni, vis_node) {
                        continue;
                    }

                    let raw = ptree[src].as_use_raw();
                    entries.push((
                        path.clone(),
                        PathValue::UseRaw(PathUseRaw {
                            syn: raw.syn,
                            detail: raw.detail,
                            vis_node: raw.vis_node,
                            npath: NamedPath::EMPTY, // Fine because it's not used anymore
                            dst_node: Some(*dst_ni),
                        }),
                    ));
                }
            }

            let base = ptree[src.ni].parent;
            for entry in entries {
                let (path, new_value) = entry;

                let new_pid = ptree.append_value(base, path.segments(), new_value);
                non_globs.push_back(new_pid);
            }

            true
        }

        /// Traverses `UseRaw` chain then finds terminal values.
        ///
        /// * src - Path id to a `UseRaw` value.
        fn find_terminal_of_use_raw(
            ptree: &PathTree,
            src: PathId,
            visit: &mut HashSet<PathId>,
            terminals: &mut Vec<PathId>,
        ) -> bool {
            if !visit.insert(src) {
                return true;
            }

            let raw = ptree[src].as_use_raw();
            let Some(dst_ni) = raw.dst_node else {
                return false;
            };

            for (dst_vi, dst_value) in ptree[dst_ni].iter() {
                let dst_pid = dst_ni.to_path_id(dst_vi);
                match dst_value {
                    PathValue::Use(_) => {
                        if !find_terminal_of_use(ptree, dst_pid, visit, terminals) {
                            return false;
                        }
                    }
                    PathValue::UseRaw(_) => {
                        if !find_terminal_of_use_raw(ptree, dst_pid, visit, terminals) {
                            return false;
                        }
                    }
                    _ => terminals.push(dst_pid),
                }
            }
            true
        }

        /// * src - Path id to a `UseRaw` value.
        fn find_terminal_of_use(
            ptree: &PathTree,
            src: PathId,
            visit: &mut HashSet<PathId>,
            terminals: &mut Vec<PathId>,
        ) -> bool {
            if !visit.insert(src) {
                return true;
            }

            let dst_pid = ptree[src].as_use().dst;
            match &ptree[dst_pid] {
                PathValue::Use(_) => find_terminal_of_use(ptree, dst_pid, visit, terminals),
                PathValue::UseRaw(_) => find_terminal_of_use_raw(ptree, dst_pid, visit, terminals),
                _ => {
                    terminals.push(dst_pid);
                    true
                }
            }
        }

        /// Removes duplicate 'Use'.
        ///
        /// Duplication can occur due to recursive imports. By that, a single
        /// node can have multiple same 'Use's, or 'Use' can points to something
        /// in the same node. Also, glob and normal imports can be duplicate.
        fn remove_duplicate(ptree: &mut PathTree) {
            let num_nodes = ptree.num_nodes();
            for ni in 0..num_nodes {
                let ni = NodeIndex(ni);

                let mut values = &mut ptree[ni].values;
                let num_values = values.len();
                for ri in (0..num_values).rev() {
                    let PathValue::Use(right) = &values[ri] else {
                        break;
                    };

                    // Destination of a 'Use' pointing to the same node is
                    // redundant.
                    if right.dst.ni == ni {
                        values.remove(ri);
                        continue;
                    }

                    // If we find the same 'Use's in one node, removes right one
                    // and keep more general visibility.
                    for li in 0..ri {
                        let PathValue::Use(left) = &values[li] else {
                            continue;
                        };

                        if right.dst != left.dst {
                            continue;
                        }

                        let left_vis = left.vis_node;
                        let right_vis = right.vis_node;
                        if ptree.is_descendant(left_vis, right_vis) {
                            let pid = ni.to_path_id(li);
                            ptree[pid].as_mut_use().vis_node = right_vis;
                        }

                        // Assigns again due to the borrow rule.
                        values = &mut ptree[ni].values;
                        values.remove(ri);
                        break;
                    }
                }
            }
        }
    }

    fn add_known_lib_file(&mut self) -> Result<bool> {
        let mut changed = false;

        let mut raw_uses = Vec::new();
        for (pid, value) in self.ptree.iter() {
            if matches!(value, PathValue::UseRaw(_)) {
                raw_uses.push(pid);
            }
        }

        for pid in raw_uses {
            let raw = self.ptree[pid].as_use_raw();
            let first_seg = raw.npath.segments().next().unwrap();
            let fname = format!("{first_seg}.rs");
            if !known::is_known_lib(&fname) {
                continue;
            }

            add_known_file(
                &mut self.stree,
                &mut self.ptree,
                &mut self.s2p,
                &mut self.impl_tasks,
                &fname,
            )?;

            changed = true;
        }

        Ok(changed)
    }

    fn resolve_type(&mut self) {
        resolve_raw(&mut self.ptree);

        shorten_chain(&mut self.ptree);

        // === Internal helper function ===

        fn resolve_raw(ptree: &mut PathTree) {
            let mut resolved = Vec::new();
            for (pid, value) in ptree.iter() {
                match value {
                    PathValue::ConstRaw(v) => {
                        let resolved_value = PathValue::Const(PathConst {
                            syn: v.syn,
                            vis_node: v.vis_node.unwrap(),
                            tid: syn_to_ref_type_id(&v.as_syn().ty, ptree, pid),
                        });
                        resolved.push((pid, resolved_value));
                    }
                    PathValue::FieldRaw(v) => {
                        let resolved_value = PathValue::Field(PathField {
                            syn: v.syn,
                            vis_node: v.vis_node.unwrap(),
                            tid: syn_to_ref_type_id(&v.as_syn().ty, ptree, pid),
                        });
                        resolved.push((pid, resolved_value));
                    }
                    PathValue::FnRaw(v) => {
                        let ret_tid = match &v.as_syn().sig.output {
                            syn::ReturnType::Default => ptree.insert_type(RefType::Default),
                            syn::ReturnType::Type(_, ty) => syn_to_ref_type_id(ty, ptree, pid),
                        };
                        let resolved_value = PathValue::Fn(PathFn {
                            syn: v.syn,
                            vis_node: v.vis_node.unwrap(),
                            ret_tid,
                        });
                        resolved.push((pid, resolved_value));
                    }
                    PathValue::TypeAliasRaw(v) => {
                        let resolved_value = PathValue::TypeAlias(PathTypeAlias {
                            syn: v.syn,
                            vis_node: v.vis_node.unwrap(),
                            tid: syn_to_ref_type_id(&v.as_syn().ty, ptree, pid),
                        });
                        resolved.push((pid, resolved_value));
                    }
                    _ => {}
                }
            }

            for (pid, resolved_value) in resolved {
                ptree[pid] = resolved_value;
            }
        }

        fn syn_to_ref_type_id(ty: &syn::Type, ptree: &PathTree, target: PathId) -> RefTypeId {
            let base = ptree
                .parent_container(target.ni, [ContainerKind::Mod, ContainerKind::Block])
                .ni;
            InferType::from_syn_type(ty, ptree, base).into_ref_type_id(ptree)
        }

        fn shorten_chain(ptree: &mut PathTree) {
            let num_nodes = ptree.num_nodes();
            for ni in 0..num_nodes {
                let ni = NodeIndex(ni);

                let num_values = ptree[ni].values.len();
                for vi in 0..num_values {
                    let value = &ptree[ni].values[vi];
                    if value == &PathValue::None {
                        continue;
                    }

                    if matches!(value, PathValue::Use(..) | PathValue::TypeAlias(..)) {
                        dfs(
                            ptree,
                            PathId {
                                ni,
                                vi: ValueIndex(vi),
                            },
                        );
                    }
                }
            }

            fn dfs(ptree: &mut PathTree, input: PathId) -> RefTypeId {
                match &ptree[input] {
                    PathValue::Use(v) => {
                        let fin = dfs(ptree, v.dst);
                        if let RefType::Path(pid) = ptree.get_type(fin) {
                            let pid = *pid;
                            ptree[input].as_mut_use().dst = pid;
                        }
                        fin
                    }
                    PathValue::TypeAlias(alias) => {
                        let fin = dfs_type_alias(ptree, alias.tid);
                        ptree[input].as_mut_type_alias().tid = fin.clone();
                        fin
                    }
                    _ => ptree.insert_type(RefType::Path(input)),
                }
            }

            fn dfs_type_alias(ptree: &mut PathTree, tid: RefTypeId) -> RefTypeId {
                let ty = ptree.get_type(tid).clone();
                match ty {
                    RefType::Path(dst) => dfs(ptree, dst),
                    RefType::Tuple(tuple) => {
                        let ty = RefType::Tuple(
                            tuple
                                .into_iter()
                                .map(|tid| dfs_type_alias(ptree, tid))
                                .collect(),
                        );
                        ptree.insert_type(ty)
                    }
                    RefType::Array { elem } => {
                        let ty = RefType::Array {
                            elem: dfs_type_alias(ptree, elem),
                        };
                        ptree.insert_type(ty)
                    }
                    RefType::Slice { elem } => {
                        let ty = RefType::Slice {
                            elem: dfs_type_alias(ptree, elem),
                        };
                        ptree.insert_type(ty)
                    }
                    RefType::Reference { elem, mut_ } => {
                        let ty = RefType::Reference {
                            elem: dfs_type_alias(ptree, elem),
                            mut_,
                        };
                        ptree.insert_type(ty)
                    }
                    RefType::InferredInt | RefType::InferredFloat | RefType::Default => tid,
                }
            }
        }
    }

    fn resolve_impl(&mut self) -> Result<()> {
        while let Some(task) = self.impl_tasks.pop_front() {
            let item_impl = unsafe { task.syn.as_ref().unwrap_unchecked() };
            insert_impl(&self.ptree, item_impl, task.base, &mut self.trait_impl)?;
        }

        return Ok(());

        // === Internal helper functions ===

        fn insert_impl(
            ptree: &PathTree,
            item_impl: &syn::ItemImpl,
            base: NodeIndex,
            trait_impl: &mut TraitImplTable,
        ) -> Result<()> {
            let self_tid =
                InferType::from_syn_type(&item_impl.self_ty, ptree, base).into_ref_type_id(ptree);

            if let Some((_, trait_path, _)) = &item_impl.trait_ {
                let ext: NamedPath = util::path_to_string(trait_path).into();
                let Some(node) = ptree.search(base, ext.segments()) else {
                    let msg = format!("failed to find `{}`", trait_path.location_message());
                    return Err(msg.into());
                };

                let mut decl_pid = None;
                for (vi, value) in ptree[node].iter() {
                    if !matches!(value, PathValue::TraitRaw(_)) {
                        decl_pid = Some(node.to_path_id(vi));
                        break;
                    }
                }

                trait_impl.insert(TraitImpl {
                    decl_pid: decl_pid.unwrap(),
                    self_tid,
                    syn: item_impl as *const _,
                });
            }

            Ok(())
        }
    }
}

fn search_dst_of_use_raw(ptree: &PathTree, pid: PathId) -> Option<NodeIndex> {
    let base = ptree.parent_container(pid.ni, [ContainerKind::Mod]).ni;
    let ext = ptree[pid].as_use_raw().npath.segments();

    if matches!(ext.clone().last(), Some("*")) {
        let ext_len = ext.clone().count();
        let ext = ext.clone().take(ext_len - 1);
        ptree.traverse(base, ext, |pid, _value| Some(pid.ni))
    } else {
        ptree.traverse(base, ext, |pid, _value| Some(pid.ni))
    }
}

#[derive(Debug, Default)]
pub struct TraitImplTable {
    values: IndexSet<TraitImpl>,

    /// A path id to a trait declaration -> An index to `values`.
    decl_map: HashMap<PathId, Vec<usize>>,

    /// A self type of a trait implementation -> An index to `values`.
    self_ty_map: HashMap<RefTypeId, Vec<usize>>,
}

impl TraitImplTable {
    fn new() -> Self {
        Self {
            values: IndexSet::new(),
            decl_map: HashMap::new(),
            self_ty_map: HashMap::new(),
        }
    }

    fn insert(&mut self, value: TraitImpl) {
        let (index, is_new) = self.values.insert_full(value);
        if !is_new {
            return;
        }

        self.decl_map
            .entry(value.decl_pid)
            .and_modify(|indices| indices.push(index))
            .or_insert(vec![index]);

        self.self_ty_map
            .entry(value.self_tid)
            .and_modify(|indices| indices.push(index))
            .or_insert(vec![index]);
    }

    fn find_by_self_type(&self, key: &RefTypeId, output: &mut HashSet<TraitImpl>) -> bool {
        let Some(indices) = self.self_ty_map.get(key) else {
            return false;
        };

        for index in indices {
            let value = self.values.get_index(*index).unwrap();
            output.insert(*value);
        }
        true
    }

    fn find_by_trait_decl(&self, key: &PathId, output: &mut HashSet<TraitImpl>) -> bool {
        let Some(indices) = self.decl_map.get(key) else {
            return false;
        };

        for index in indices {
            let value = self.values.get_index(*index).unwrap();
            output.insert(*value);
        }
        true
    }

    fn write_debug<T: fmt::Write>(&self, ptree: &PathTree, f: &mut T) -> fmt::Result {
        write!(f, "TraitImplTable {{ ")?;
        for (i, value) in self.values.iter().enumerate() {
            value.write_debug(ptree, f)?;
            if i < self.values.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, " }}")?;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct TraitImpl {
    decl_pid: PathId,
    self_tid: RefTypeId,
    syn: *const syn::ItemImpl,
}

impl TraitImpl {
    fn write_debug<T: fmt::Write>(&self, ptree: &PathTree, f: &mut T) -> fmt::Result {
        write!(f, "TraitImpl {{ decl_pid: {:?}, self_ty: ", self.decl_pid)?;
        ptree.get_type(self.self_tid).write_debug(ptree, f)?;
        write!(f, " }}")
    }
}

struct LocalResolver {
    ptree: PathTree,
    s2p: SynToPath,
}

impl LocalResolver {
    fn resolve(&mut self) -> Result<()> {
        let mut tasks = Vec::new();
        let mut seen = HashSet::new();

        for (pid, value) in self.ptree.iter() {
            let PathValue::LocalRaw(v) = value else {
                continue;
            };
            let syn = v.as_syn();
            let loc = syn.location();

            let parent = self.ptree[pid.ni].parent;
            for (_, value) in self.ptree[parent].iter() {
                match value {
                    // Local variable in a function input
                    PathValue::Fn(v) => {
                        for input in &v.as_syn().sig.inputs {
                            match input {
                                syn::FnArg::Receiver(..) => todo!(),
                                syn::FnArg::Typed(v) => {
                                    if !v.contains(syn as *const _) {
                                        continue;
                                    }
                                }
                            }

                            tasks.push((
                                loc,
                                LocalResolveTask::FnArg {
                                    arg: input as *const _,
                                    base: parent,
                                },
                            ));
                            break;
                        }
                        break;
                    }
                    // Local variables in a block
                    PathValue::Block(v) => {
                        let ptr_block = v.as_syn() as *const _;
                        if !seen.insert(ptr_block) {
                            continue;
                        }

                        tasks.push((
                            loc,
                            LocalResolveTask::Block {
                                block: v.as_syn() as *const _,
                            },
                        ));
                        break;
                    }
                    _ => continue,
                }
            }
        }

        tasks.sort_unstable_by_key(|(loc, _)| *loc);

        let mut helper = LocalHelper {
            ptree: &self.ptree,
            s2p: &self.s2p,
            base: PathTree::ROOT,
            pivot: None,
            i2p: HashMap::new(),
            belonging: HashMap::new(),
            cur_stmts: Vec::new(),
        };

        for (_, task) in &tasks {
            match task {
                LocalResolveTask::FnArg { arg, base } => {
                    let arg = unsafe { arg.as_ref().unwrap_unchecked() };
                    helper.add_fn_arg(arg, *base);
                }
                LocalResolveTask::Block { block } => {
                    let block = unsafe { block.as_ref().unwrap_unchecked() };
                    helper.add_block(block);
                }
            }
        }

        let LocalHelper { i2p, belonging, .. } = helper;

        let mut targets: VecDeque<PathId> = belonging.keys().cloned().collect();
        targets
            .make_contiguous()
            .sort_unstable_by_key(|pid| self.ptree[*pid].as_local_raw().as_syn().location());

        let dummy = HashSet::new();
        let mut lcx = LocalCx {
            ptree: &mut self.ptree,
            s2p: &self.s2p,
            i2p: &i2p,
            belonging: &dummy,
            base: PathTree::ROOT,
            s2ty: HashMap::new(),
            updated: Vec::new(),
        };

        while let Some(pid) = targets.pop_front() {
            lcx.belonging = belonging.get(&pid).unwrap();

            for (_, task) in &tasks {
                match task {
                    LocalResolveTask::FnArg { arg, base } => {
                        let arg = unsafe { arg.as_ref().unwrap_unchecked() };
                        lcx.resolve_fn_arg(arg, *base);
                    }
                    LocalResolveTask::Block { block } => {
                        let block = unsafe { block.as_ref().unwrap_unchecked() };
                        lcx.resolve_block(block)?;
                    }
                }
            }

            for pid in lcx.updated.drain(..) {
                targets.push_back(pid);
            }
        }

        let LocalCx { s2ty, .. } = lcx;

        for (sid, ty) in s2ty {
            let Some(pid) = self.s2p.get_path_id(sid) else {
                continue;
            };

            let tid = ty.into_ref_type_id(&self.ptree);
            let value = &mut self.ptree[pid];
            let raw = value.as_local_raw();
            *value = PathValue::Local(PathLocal {
                syn: raw.syn,
                mut_: raw.mut_,
                tid,
            });
        }

        Ok(())
    }
}

struct LocalHelper<'a> {
    ptree: &'a PathTree,
    s2p: &'a SynToPath,

    base: NodeIndex,
    pivot: Option<usize>,

    /// Mapping from an ident to an unresolved local value.
    i2p: HashMap<*const syn::Ident, PathId>,

    /// Maaping from an unresolved local value to statments that include the
    /// local value.
    belonging: HashMap<PathId, HashSet<*const syn::Stmt>>,

    /// Nested statements we have passed in a syntax tree.
    cur_stmts: Vec<*const syn::Stmt>,
}

impl<'a> LocalHelper<'a> {
    fn add_fn_arg(&mut self, fn_arg: &syn::FnArg, base: NodeIndex) {
        let old_base = self.base;
        self.base = base;

        match fn_arg {
            syn::FnArg::Receiver(_) => todo!(),
            syn::FnArg::Typed(v) => self.add_pat_type(v),
        }

        self.base = old_base;
    }

    fn add_block(&mut self, block: &syn::Block) {
        let old_base = self.base;
        self.base = self.s2p.get_path_id(block.syn_id()).unwrap().ni;

        for stmt in &block.stmts {
            self.cur_stmts.push(stmt as *const _);

            match stmt {
                syn::Stmt::Local(v) => self.add_local(v),
                syn::Stmt::Item(v) => self.add_item(v),
                syn::Stmt::Expr(v, _) => self.add_expr(v),
                syn::Stmt::Macro(_) => {}
            }

            self.cur_stmts.pop();
        }

        self.base = old_base;
    }

    fn add_local(&mut self, local: &syn::Local) {
        // Let binding may have the same ident on both left and right hands.
        // They are different symbols, so we adjust pivot here in order to find
        // correct path value.
        let old_pivot = self.pivot;

        self.pivot = Some(local.pat.location().end + 1);
        self.add_pat(&local.pat);

        if let Some(init) = &local.init {
            self.pivot = Some(local.location().start);
            self.add_local_init(init);
        }

        self.pivot = old_pivot;
    }

    fn add_local_init(&mut self, local_init: &syn::LocalInit) {
        self.add_expr(&local_init.expr);
    }

    fn add_item(&mut self, _item: &syn::Item) {
        // Not identified yet
    }

    fn add_expr(&mut self, expr: &syn::Expr) {
        match expr {
            syn::Expr::Assign(v) => self.add_expr_assign(v),
            syn::Expr::Binary(v) => self.add_expr_binary(v),
            syn::Expr::Block(v) => self.add_expr_block(v),
            syn::Expr::Cast(v) => self.add_expr_cast(v),
            syn::Expr::Path(v) => self.add_expr_path(v),
            syn::Expr::Repeat(v) => self.add_expr_repeat(v),
            syn::Expr::Struct(v) => todo!("{v:?}"),
            syn::Expr::Tuple(v) => self.add_expr_tuple(v),
            _ => {}
        }
    }

    fn add_expr_assign(&mut self, expr_assign: &syn::ExprAssign) {
        self.add_expr(&expr_assign.left);
        self.add_expr(&expr_assign.right);
    }

    fn add_expr_binary(&mut self, expr_binary: &syn::ExprBinary) {
        self.add_expr(&expr_binary.left);
        self.add_expr(&expr_binary.right);
    }

    fn add_expr_block(&mut self, expr_block: &syn::ExprBlock) {
        self.add_block(&expr_block.block);
    }

    fn add_expr_cast(&mut self, expr_cast: &syn::ExprCast) {
        self.add_expr(&expr_cast.expr);
    }

    fn add_expr_path(&mut self, expr_path: &syn::ExprPath) {
        self.add_path(&expr_path.path);
    }

    fn add_expr_repeat(&mut self, expr_repeat: &syn::ExprRepeat) {
        self.add_expr(&expr_repeat.expr);
    }

    fn add_expr_tuple(&mut self, expr_tuple: &syn::ExprTuple) {
        for elem in &expr_tuple.elems {
            self.add_expr(elem);
        }
    }

    fn add_pat(&mut self, pat: &syn::Pat) {
        match pat {
            syn::Pat::Ident(v) => self.add_pat_ident(v),
            syn::Pat::Struct(v) => self.add_pat_struct(v),
            syn::Pat::Tuple(v) => self.add_pat_tuple(v),
            syn::Pat::Type(v) => self.add_pat_type(v),
            _ => {}
        }
    }

    fn add_pat_ident(&mut self, pat_ident: &syn::PatIdent) {
        self.add_ident(&pat_ident.ident);
    }

    fn add_pat_struct(&mut self, pat_struct: &syn::PatStruct) {
        for field in &pat_struct.fields {
            // Do not use 'field.member' because
            // - We are creating pointer map.
            // - 'field.member' and 'field.pat' have similar idents, but their
            //   addresses are different.
            // - We have used 'field.pat' during path tree construction.
            self.add_pat(&field.pat);
        }
    }

    fn add_pat_tuple(&mut self, pat_tuple: &syn::PatTuple) {
        for elem in &pat_tuple.elems {
            self.add_pat(elem);
        }
    }

    fn add_pat_type(&mut self, pat_type: &syn::PatType) {
        self.add_pat(&pat_type.pat);
    }

    fn add_path(&mut self, path: &syn::Path) {
        if let Some(ident) = path.get_ident() {
            self.add_ident(ident);
        }
    }

    fn add_ident(&mut self, ident: &syn::Ident) {
        let ext: NamedPath = ident.to_string().into();
        let pivot = self.cur_pivot_or(|| ident.location().end + 1);
        self.add(ext.segments(), pivot, ident);
    }

    fn add<'b, I, II>(&mut self, ext: I, pivot: usize, ident: &syn::Ident)
    where
        I: IntoIterator<IntoIter = II>,
        II: Iterator<Item = &'b str> + Clone,
    {
        let pid = self.ptree.search_value(self.base, ext, pivot).unwrap();

        self.i2p.insert(ident as *const _, pid);
        self.belonging
            .entry(pid)
            .and_modify(|set| {
                for ptr_stmt in &self.cur_stmts {
                    set.insert(*ptr_stmt);
                }
            })
            .or_insert(self.cur_stmts.iter().cloned().collect());
    }

    fn cur_pivot_or<F: FnOnce() -> usize>(&self, or: F) -> usize {
        if let Some(pivot) = self.pivot {
            pivot
        } else {
            or()
        }
    }
}

struct LocalCx<'a> {
    ptree: &'a mut PathTree,
    s2p: &'a SynToPath,
    i2p: &'a HashMap<*const syn::Ident, PathId>,
    belonging: &'a HashSet<*const syn::Stmt>,

    base: NodeIndex,

    /// Syn id of raw local -> inferred type
    s2ty: HashMap<SynId, InferType>,
    updated: Vec<PathId>,
}

impl<'a> LocalCx<'a> {
    fn resolve_fn_arg(&mut self, fn_arg: &syn::FnArg, base: NodeIndex) {
        let old_base = self.base;
        self.base = base;

        match fn_arg {
            syn::FnArg::Receiver(..) => todo!(),
            syn::FnArg::Typed(v) => {
                self.resolve_pat_type(v);
            }
        }

        self.base = old_base;
    }

    fn resolve_block(&mut self, block: &syn::Block) -> Result<()> {
        let old_base = self.base;
        self.base = self.s2p.get_path_id(block.syn_id()).unwrap().ni;

        for stmt in &block.stmts {
            if !self.belonging.contains(&(stmt as *const _)) {
                continue;
            }

            match stmt {
                syn::Stmt::Local(v) => self.resolve_local(v)?,
                syn::Stmt::Item(..) => {}
                syn::Stmt::Expr(v, _) => self.resolve_expr(v, InferType::Default)?,
                syn::Stmt::Macro(..) => {}
            }
        }

        self.base = old_base;
        Ok(())
    }

    fn resolve_local(&mut self, local: &syn::Local) -> Result<()> {
        let l_ty = self.resolve_pat(&local.pat, InferType::Default);

        let Some(init) = &local.init else {
            return Ok(());
        };

        let pivot = local.location().start;
        let r_ty = InferCx { lcx: self, pivot }.infer_expr(&init.expr)?;

        if l_ty.is_overwritable(&r_ty) {
            self.resolve_pat(&local.pat, r_ty.clone());
            self.resolve_expr(&init.expr, r_ty)?;
        } else if r_ty.is_overwritable(&l_ty) {
            self.resolve_expr(&init.expr, l_ty)?;
        }

        Ok(())
    }

    fn resolve_expr(&mut self, expr: &syn::Expr, ty: InferType) -> Result<()> {
        match expr {
            syn::Expr::Assign(v) => self.resolve_expr_assign(v),
            syn::Expr::Binary(v) => self.resolve_expr_binary(v, ty),
            syn::Expr::Block(v) => self.resolve_block(&v.block),
            syn::Expr::Path(v) => {
                // TODO: is unwrap() right?
                let ident = v.path.get_ident().unwrap();
                self.resolve_ident(ident, ty);
                Ok(())
            }
            syn::Expr::Struct(v) => todo!("{v:?}"),
            syn::Expr::Tuple(v) => self.resolve_expr_tuple(v, ty),
            _ => Ok(()),
        }
    }

    fn resolve_expr_assign(&mut self, expr_assign: &syn::ExprAssign) -> Result<()> {
        let pivot = expr_assign.location().start;
        let ty = InferCx { lcx: self, pivot }.infer_expr(&expr_assign.right)?;
        self.resolve_expr(&expr_assign.left, ty)
    }

    fn resolve_expr_binary(&mut self, expr_binary: &syn::ExprBinary, ty: InferType) -> Result<()> {
        // TODO: Infer l & r type each then resolve them using l_ty, r_ty, and ty.
        self.resolve_expr(&expr_binary.left, ty.clone())?;
        self.resolve_expr(&expr_binary.right, ty)
    }

    fn resolve_expr_tuple(&mut self, expr_tuple: &syn::ExprTuple, ty: InferType) -> Result<()> {
        match ty {
            InferType::Complete(tid) => {
                let RefType::Tuple(tuple) = self.ptree.get_type(tid) else {
                    unreachable!()
                };
                assert_eq!(tuple.len(), expr_tuple.elems.len());

                let tuple = tuple.clone();
                for (elem, elem_tid) in expr_tuple.elems.iter().zip(tuple) {
                    self.resolve_expr(elem, InferType::Complete(elem_tid))?;
                }
            }
            InferType::Tuple(tuple) => {
                assert_eq!(tuple.len(), expr_tuple.elems.len());

                for (elem, elem_ty) in expr_tuple.elems.iter().zip(tuple) {
                    self.resolve_expr(elem, elem_ty)?;
                }
            }
            InferType::Default => {
                for elem in &expr_tuple.elems {
                    self.resolve_expr(elem, InferType::Default)?;
                }
            }
            o => unreachable!("{o:?}"),
        }
        Ok(())
    }

    fn resolve_pat(&mut self, pat: &syn::Pat, ty: InferType) -> InferType {
        match pat {
            syn::Pat::Ident(v) => self.resolve_ident(&v.ident, ty),
            syn::Pat::Struct(v) => self.resolve_pat_struct(v),
            syn::Pat::Tuple(v) => self.resolve_pat_tuple(v, ty),
            syn::Pat::Type(v) => self.resolve_pat_type(v),
            _ => InferType::Default,
        }
    }

    fn resolve_pat_struct(&mut self, pat_struct: &syn::PatStruct) -> InferType {
        let ext: NamedPath = util::path_to_string(&pat_struct.path).into();
        let ty = self.ptree.search_type(self.base, ext.segments()).unwrap();
        let ty = InferType::Complete(ty);

        for field in &pat_struct.fields {
            let mut ext = ext.clone();
            match &field.member {
                syn::Member::Named(v) => util::push_colon_path(&mut ext, v),
                syn::Member::Unnamed(v) => util::push_colon_path(&mut ext, v.index),
            }

            let ty = self.ptree.search_type(self.base, ext.segments()).unwrap();
            self.resolve_pat(&field.pat, InferType::Complete(ty));
        }

        ty
    }

    fn resolve_pat_tuple(&mut self, pat_tuple: &syn::PatTuple, ty: InferType) -> InferType {
        match ty {
            InferType::Complete(_ty) => todo!(),
            InferType::Tuple(types) => {
                assert_eq!(pat_tuple.elems.len(), types.len());

                let mut elems = Vec::new();
                for (pat, ty) in pat_tuple.elems.iter().zip(types) {
                    elems.push(self.resolve_pat(pat, ty));
                }
                InferType::Tuple(elems)
            }
            o => unreachable!("{o:?}"),
        }
    }

    fn resolve_pat_type(&mut self, pat_type: &syn::PatType) -> InferType {
        let ty = InferType::from_syn_type(&pat_type.ty, &self.ptree, self.base);
        self.resolve_pat(&pat_type.pat, ty.clone());
        ty
    }

    fn resolve_ident(&mut self, ident: &syn::Ident, ty: InferType) -> InferType {
        let pid = *self.i2p.get(&(ident as *const _)).unwrap();

        let PathValue::LocalRaw(v) = &self.ptree[pid] else {
            unreachable!();
        };
        let sid = v.syn_id();

        // Overwrites.
        if let Some(old) = self.s2ty.get_mut(&sid) {
            if old.is_overwritable(&ty) {
                *old = ty;
                if !self.updated.contains(&pid) {
                    self.updated.push(pid);
                }
            }
            old.clone()
        } else {
            self.s2ty.insert(sid, ty.clone());
            if !self.updated.contains(&pid) {
                self.updated.push(pid);
            }
            ty
        }
    }
}

trait ContainsPatIdent {
    fn contains(&self, target: *const syn::PatIdent) -> bool;
}

impl ContainsPatIdent for syn::Pat {
    fn contains(&self, target: *const syn::PatIdent) -> bool {
        match self {
            syn::Pat::Ident(v) => v.contains(target),
            syn::Pat::Struct(v) => v.contains(target),
            syn::Pat::Tuple(v) => v.contains(target),
            syn::Pat::Type(v) => v.contains(target),
            _ => false,
        }
    }
}

impl ContainsPatIdent for syn::PatIdent {
    fn contains(&self, target: *const syn::PatIdent) -> bool {
        self as *const _ == target
    }
}

impl ContainsPatIdent for syn::PatStruct {
    fn contains(&self, target: *const syn::PatIdent) -> bool {
        self.fields.iter().any(|field| field.pat.contains(target))
    }
}

impl ContainsPatIdent for syn::PatTuple {
    fn contains(&self, target: *const syn::PatIdent) -> bool {
        self.elems.iter().any(|elem| elem.contains(target))
    }
}

impl ContainsPatIdent for syn::PatType {
    fn contains(&self, target: *const syn::PatIdent) -> bool {
        self.pat.contains(target)
    }
}

#[derive(Debug)]
enum LocalResolveTask {
    FnArg {
        arg: *const syn::FnArg,
        base: NodeIndex,
    },
    Block {
        block: *const syn::Block,
    },
}

struct InferCx<'a, 'b: 'a> {
    lcx: &'a mut LocalCx<'b>,
    pivot: usize,
}

impl InferCx<'_, '_> {
    fn infer_expr(&mut self, expr: &syn::Expr) -> Result<InferType> {
        let sid = expr.syn_id();
        if let Some(ty) = self.lcx.s2ty.get(&sid) {
            if ty.is_complete() {
                return Ok(ty.clone());
            }
        }

        let ty = match expr {
            syn::Expr::Binary(v) => self.infer_expr_binary(v)?,
            syn::Expr::Cast(v) => self.infer_expr_cast(v),
            syn::Expr::Field(v) => self.infer_expr_field(v)?,
            syn::Expr::Lit(v) => Self::infer_expr_lit(&self.lcx.ptree, v)?,
            syn::Expr::Path(v) => self.infer_expr_path(v),
            syn::Expr::Reference(v) => self.infer_expr_reference(v)?,
            syn::Expr::Repeat(v) => self.infer_expr_repeat(v)?,
            syn::Expr::Struct(v) => self.infer_expr_struct(v),
            syn::Expr::Tuple(v) => self.infer_expr_tuple(v)?,
            _ => InferType::Default,
        };

        // We don't have to check that overwrite is valid because inferred type
        // must be overwritable.
        self.lcx.s2ty.insert(sid, ty.clone());

        Ok(ty)
    }

    fn infer_expr_binary(&mut self, expr_binary: &syn::ExprBinary) -> Result<InferType> {
        let l = self.infer_expr(&expr_binary.left)?;
        let r = self.infer_expr(&expr_binary.right)?;
        let ty = Self::bin_op_output_type(&self.lcx.ptree, expr_binary.op, &l, &r);
        Ok(ty)
    }

    fn bin_op_output_type(
        ptree: &PathTree,
        _op: syn::BinOp,
        l: &InferType,
        r: &InferType,
    ) -> InferType {
        // TODO: Think of more general approach.
        match (l, r) {
            (InferType::InferredInt, InferType::InferredInt) => {
                return InferType::InferredInt;
            }
            (InferType::InferredFloat, InferType::InferredFloat) => {
                return InferType::InferredFloat;
            }
            (InferType::InferredInt | InferType::InferredFloat, InferType::Complete(v)) => {
                return InferType::Complete(v.clone());
            }
            (InferType::Complete(v), InferType::InferredInt | InferType::InferredFloat) => {
                return InferType::Complete(v.clone());
            }
            (InferType::Complete(l_tid), InferType::Complete(r_tid)) => {
                let l_ty = ptree.get_type(*l_tid);
                let r_ty = ptree.get_type(*r_tid);
                if let (RefType::Path(l_pid), RefType::Path(r_pid)) = (l_ty, r_ty) {
                    if l_pid == r_pid && matches!(ptree[*l_pid], PathValue::Extern) {
                        return InferType::Complete(*l_tid);
                    }
                }
            }
            _ => {}
        }

        todo!("not ready to process implementations of std::ops")
    }

    fn infer_expr_cast(&mut self, expr_cast: &syn::ExprCast) -> InferType {
        InferType::from_syn_type(&expr_cast.ty, &self.lcx.ptree, self.lcx.base)
    }

    fn infer_expr_field(&mut self, expr_field: &syn::ExprField) -> Result<InferType> {
        let base_ty = self.infer_expr(&expr_field.base)?;

        let ty = match &expr_field.member {
            syn::Member::Named(member) => {
                // Struct
                let InferType::Complete(tid) = base_ty else {
                    unreachable!()
                };
                let RefType::Path(base_pid) = self.lcx.ptree.get_type(tid) else {
                    unreachable!()
                };

                let ext = member.to_string();
                let member_ty = self
                    .lcx
                    .ptree
                    .search_type(base_pid.ni, [ext.as_str()])
                    .unwrap();
                InferType::Complete(member_ty)
            }
            syn::Member::Unnamed(_member) => {
                // Tuple
                todo!()
            }
        };

        Ok(ty)
    }

    fn infer_expr_lit(ptree: &PathTree, expr_lit: &syn::ExprLit) -> Result<InferType> {
        match &expr_lit.lit {
            syn::Lit::Int(v) => {
                if v.suffix().is_empty() {
                    return Ok(InferType::InferredInt);
                }

                let path: NamedPath = v.suffix().to_owned().into();

                if let Some(node) = ptree.search(PathTree::ROOT, path.segments()) {
                    // The node contains extern integer value only.
                    let ty = RefType::Path(node.to_path_id(0));
                    let tid = ptree.insert_type(ty);
                    Ok(InferType::Complete(tid))
                } else {
                    panic!("couldn't find {path} in the path tree");
                }
            }
            syn::Lit::Float(v) => {
                if v.suffix().is_empty() {
                    return Ok(InferType::InferredFloat);
                }

                let path: NamedPath = v.suffix().to_owned().into();

                if let Some(node) = ptree.search(PathTree::ROOT, path.segments()) {
                    // The node contains extern float value only.
                    let ty = RefType::Path(node.to_path_id(0));
                    let tid = ptree.insert_type(ty);
                    Ok(InferType::Complete(tid))
                } else {
                    panic!("couldn't find {path} in the path tree");
                }
            }
            syn::Lit::Bool(v) => todo!("{}: not yet allowed type", v.location_message()),
            o => Err(format!("{}: not allowed type", o.location_message()).into()),
        }
    }

    fn infer_expr_path(&mut self, expr_path: &syn::ExprPath) -> InferType {
        self.infer_path(&expr_path.path)
    }

    fn infer_expr_reference(&mut self, expr_reference: &syn::ExprReference) -> Result<InferType> {
        let elem = self.infer_expr(&expr_reference.expr)?;
        Ok(InferType::Reference {
            elem: Box::new(elem),
            mut_: expr_reference.mutability.is_some(),
        })
    }

    fn infer_expr_repeat(&mut self, expr_repeat: &syn::ExprRepeat) -> Result<InferType> {
        let elem = self.infer_expr(&expr_repeat.expr)?;
        Ok(InferType::Array {
            elem: Box::new(elem),
        })
    }

    fn infer_expr_struct(&mut self, expr_struct: &syn::ExprStruct) -> InferType {
        self.infer_path(&expr_struct.path)
    }

    fn infer_expr_tuple(&mut self, expr_tuple: &syn::ExprTuple) -> Result<InferType> {
        let mut v = Vec::with_capacity(expr_tuple.elems.len());
        for elem in &expr_tuple.elems {
            v.push(self.infer_expr(elem)?);
        }
        Ok(InferType::Tuple(v))
    }

    /// * path - A path in expression position.
    fn infer_path(&mut self, path: &syn::Path) -> InferType {
        let ext: NamedPath = util::path_to_string(path).into();
        let pid = self
            .lcx
            .ptree
            .search_value(self.lcx.base, ext.segments(), self.pivot)
            .unwrap();

        match &self.lcx.ptree[pid] {
            PathValue::Const(v) => InferType::Complete(v.tid),
            PathValue::Local(v) => InferType::Complete(v.tid),
            PathValue::LocalRaw(v) => {
                let ty = self.lcx.s2ty.get(&v.syn_id()).unwrap();
                ty.clone()
            }
            PathValue::Struct(_) | PathValue::StructRaw(_) => {
                let ty = RefType::Path(pid);
                let tid = self.lcx.ptree.insert_type(ty);
                InferType::Complete(tid)
            }
            o => unreachable!("{o:?}"),
        }
    }
}

// This is similar to `RefType`, but may be expanded in the future.
#[derive(Debug, Clone, PartialEq, Eq)]
enum InferType {
    Complete(RefTypeId),
    Tuple(Vec<InferType>),
    Array { elem: Box<InferType> },
    Slice { elem: Box<InferType> },
    Reference { elem: Box<InferType>, mut_: bool },
    InferredInt,
    InferredFloat,
    Default,
}

impl InferType {
    fn from_syn_type(ty: &syn::Type, ptree: &PathTree, base: NodeIndex) -> Self {
        match ty {
            syn::Type::Path(v) => {
                let ext: NamedPath = util::path_to_string(&v.path).into();
                let ty = ptree.search_type(base, ext.segments()).unwrap();
                Self::Complete(ty)
            }
            syn::Type::Tuple(v) => {
                let elems = v
                    .elems
                    .iter()
                    .map(|e| Self::from_syn_type(e, ptree, base))
                    .collect();
                Self::Tuple(elems)
            }
            syn::Type::Array(v) => Self::Array {
                elem: Box::new(Self::from_syn_type(&v.elem, ptree, base)),
            },
            syn::Type::Slice(v) => Self::Slice {
                elem: Box::new(Self::from_syn_type(&v.elem, ptree, base)),
            },
            syn::Type::Reference(v) => Self::Reference {
                elem: Box::new(Self::from_syn_type(&v.elem, ptree, base)),
                mut_: v.mutability.is_some(),
            },
            o => todo!("{o:?}"),
        }
    }

    fn into_ref_type_id(self, ptree: &PathTree) -> RefTypeId {
        match self {
            Self::Complete(tid) => tid,
            Self::Tuple(tuple) => {
                let elems = tuple
                    .into_iter()
                    .map(|elem| elem.into_ref_type_id(ptree))
                    .collect();
                let ty = RefType::Tuple(elems);
                ptree.insert_type(ty)
            }
            Self::Array { elem } => {
                let ty = RefType::Array {
                    elem: elem.into_ref_type_id(ptree),
                };
                ptree.insert_type(ty)
            }
            Self::Slice { elem } => {
                let ty = RefType::Slice {
                    elem: elem.into_ref_type_id(ptree),
                };
                ptree.insert_type(ty)
            }
            Self::Reference { elem, mut_ } => {
                let ty = RefType::Reference {
                    elem: elem.into_ref_type_id(ptree),
                    mut_,
                };
                ptree.insert_type(ty)
            }
            Self::InferredInt => ptree.insert_type(RefType::InferredInt),
            Self::InferredFloat => ptree.insert_type(RefType::InferredFloat),
            Self::Default => ptree.insert_type(RefType::Default),
        }
    }

    fn is_complete(&self) -> bool {
        match self {
            Self::Complete(_) => true,
            Self::Tuple(tuple) => tuple.iter().all(Self::is_complete),
            Self::Array { elem } => elem.is_complete(),
            Self::Slice { elem } => elem.is_complete(),
            Self::Reference { elem, mut_: _ } => elem.is_complete(),
            Self::InferredInt | Self::InferredFloat | Self::Default => false,
        }
    }

    fn is_overwritable(&self, rhs: &Self) -> bool {
        match self {
            Self::Complete(_) => false,
            Self::Tuple(lhs_tuple) => {
                let Self::Tuple(rhs_tuple) = rhs else {
                    return false;
                };
                if lhs_tuple.len() != rhs_tuple.len() {
                    return false;
                }
                lhs_tuple
                    .iter()
                    .zip(rhs_tuple)
                    .all(|(l, r)| l.is_overwritable(r))
            }
            Self::Array { elem: lhs_elem } => {
                let Self::Array { elem: rhs_elem } = rhs else {
                    return false;
                };
                lhs_elem.is_overwritable(&rhs_elem)
            }
            Self::Slice { elem: lhs_elem } => {
                let Self::Slice { elem: rhs_elem } = rhs else {
                    return false;
                };
                lhs_elem.is_overwritable(&rhs_elem)
            }
            Self::Reference {
                elem: lhs_elem,
                mut_: _,
            } => {
                let Self::Reference {
                    elem: rhs_elem,
                    mut_: _,
                } = rhs
                else {
                    return false;
                };
                lhs_elem.is_overwritable(&rhs_elem)
            }
            Self::InferredInt | Self::InferredFloat => {
                !matches!(rhs, Self::InferredInt | Self::InferredFloat | Self::Default)
            }
            Self::Default => !matches!(rhs, Self::Default),
        }
    }
}

// * fname - A known file name like "std.rs".
fn add_known_file(
    stree: &mut HashMap<PathBuf, Pin<Box<SmFile>>>,
    ptree: &mut PathTree,
    s2p: &mut SynToPath,
    impl_tasks: &mut VecDeque<ResolveImplTask>,
    fname: &str,
) -> Result<()> {
    if stree.contains_key(StdPath::new(fname)) {
        return Ok(());
    }

    let fpath: PathBuf = fname.into();
    let code = known::known_lib_code(fname);
    let file = SmFile::new(fpath.clone(), code.to_owned())?;
    stree.insert(fpath.clone(), file);

    let mut dummy = VecDeque::new();
    let mut cx = ConstructCx {
        stree,
        ptree,
        s2p,
        construct_tasks: &mut dummy,
        impl_tasks,
        path: NamedPath::new(fname.strip_suffix(".rs").unwrap()), // e.g. "std"
    };

    let file = stree.get(&fpath).unwrap();
    cx.add_file(file)?;
    assert!(dummy.is_empty());

    Ok(())
}

#[cfg(test)]
#[rustfmt::skip]
mod tests {
    use super::*;
    use std::env;

    /// Tests if the processor can find connected files then constructs path
    /// tree according to the files.
    #[test]
    fn test_read_file() -> Result<()> {
        let dir = env::current_dir()?;

        let path_a1 = dir.join("a1.rs");
        let code_a1 = r#"
        // Entry file
        // Logical path: ~::a1
        // Physical path: ~/a1

        // Logical path: ~::a1::b1
        // Physical path: ~/a1/b1.rs or ~/a1/b1/mod.rs. We have the first one.
        mod b1;

        // Logical path: ~::a1::c1
        // Physical path: ~/c1.rs
        #[path = "c1.rs"]
        mod c1;

        // Logical path: ~::a1::dx
        // Physical path: ~/d1
        #[path = "d1"]
        mod dx {
            // Logical path: ~::a1::dx::d2
            // Physical path: ~/d1/d2.rs or ~d1/d2/mod.rs. We have the first one.
            mod d2;
        }

        // Logical path: ~::a1::e1
        // Physical path: ~/a1/e1
        mod e1 {
            // Logical path: ~::a1::e1::e2
            // Physical path: ~/a1/e1/e2.rs or ~/a1/e1/e2/mod.rs. We have the first one.
            mod e2;

            // Logical path: ~::a1::e1::e3
            // Physical path: ~/a1/e1/e4.rs
            #[path = "e4.rs"]
            mod e3;
        }
        "#;

        let path_b1 = dir.join("a1/b1.rs");
        let code_b1 = r#"
        // From a1.rs
        // Logical path: ~::a1::b1
        // Physical path: ~/a1/b1.rs

        // Logical path: ~::a1::b1::b2
        // Physical path: ~/a1/b1/b2.rs or ~/a1/b1/b2/mod.rs. We have the first one.
        mod b2;
        "#;

        let path_b2 = dir.join("a1/b1/b2.rs");
        let code_b2 = r#"
        // From a1/b1.rs
        // Logical path: ~::a1::b1::b2
        // Physical path: ~/a1/b1/b2.rs
        "#;

        let path_c1 = dir.join("c1.rs");
        let code_c1 = r#"
        // From a1.rs
        // Logical path: ~::a1::c1
        // Physical path: ~/c1.rs
        "#;

        let path_d2 = dir.join("d1/d2.rs");
        let code_d2 = r#"
        // From a1.rs
        // Logical path: ~::a1::dx::d2
        // Physical path: ~/d1/d2.rs
        "#;

        let path_e2 = dir.join("a1/e1/e2.rs");
        let code_e2 = r#"
        // From a1.rs
        // Logical path: ~::a1::e1::e2
        // Physical path: ~/a1/e1/e2.rs
        "#;

        let path_e4 = dir.join("a1/e1/e4.rs");
        let code_e4 = r#"
        // From a1.rs
        // Logical path: ~::a1::e1::e3
        // Physical path: ~/a1/e1/e4.rs
        "#;

        // Gets ready for the test.
        TEST_FILES.with_borrow_mut(|map| {
            map.insert(path_a1.clone(), code_a1);
            map.insert(path_b1.clone(), code_b1);
            map.insert(path_b2.clone(), code_b2);
            map.insert(path_c1.clone(), code_c1);
            map.insert(path_d2.clone(), code_d2);
            map.insert(path_e2.clone(), code_e2);
            map.insert(path_e4.clone(), code_e4);
        });
        let entry = dir.join("main.rs");
        let mut proc = PathProcessor::new(&entry)?;
        proc.import([&path_a1])?;
        proc.process()?;
        let ptree = &proc.ptree;

        let test = |key: &str, expect_fpath: &StdPath| {
            let full_key = ["crate"]
                .into_iter()
                .chain(key.split("::"));
            let node = ptree.search(PathTree::ROOT, full_key).unwrap();
            let (_, value) = ptree[node].iter().next().unwrap();
            if !matches!(value, PathValue::Mod(PathMod { fpath, ..}) if fpath == expect_fpath) {
                panic!("failed at key: `{key}`");
            }
        };

        // From the "a1.rs"
        test("a1", &dir.join("a1.rs"));
        test("a1::b1", &dir.join("a1/b1.rs"));
        test("a1::c1", &dir.join("c1.rs"));
        test("a1::dx", &dir.join("d1"));
        test("a1::dx::d2", &dir.join("d1/d2.rs"));
        test("a1::e1", &dir.join("a1/e1"));
        test("a1::e1::e2", &dir.join("a1/e1/e2.rs"));
        test("a1::e1::e3", &dir.join("a1/e1/e4.rs"));

        // From the "a1/b1.rs"
        test("a1::b1::b2", &dir.join("a1/b1/b2.rs"));

        Ok(())
    }

    #[test]
    fn test_cascading_import() -> Result<()> {
        let proc = prepare("mod.rs", r#"
        mod a {
            pub mod b {
                pub struct C;
            }
        }

        use b::C;
        use a::b;
        "#)?;
        let ptree = &proc.ptree;

        let node_b = ptree.search(PathTree::ROOT, ["crate","mod","a","b"]).unwrap();
        let node_c = ptree.search(PathTree::ROOT, ["crate","mod","a","b","C"]).unwrap();
        let pid_b = node_b.to_path_id(0);
        let pid_c = node_c.to_path_id(0);

        let b = ptree.search(PathTree::ROOT, ["crate","mod","b"]).unwrap();
        assert!(matches!(
            ptree[b].iter().next().unwrap().1,
            PathValue::Use(PathUse { dst, .. })
            if *dst == pid_b
        ));

        let c = ptree.search(PathTree::ROOT, ["crate","mod","C"]).unwrap();
        assert!(matches!(
            ptree[c].iter().next().unwrap().1,
            PathValue::Use(PathUse { dst, .. })
            if *dst == pid_c
        ));

        Ok(())
    }

    #[test]
    fn test_recursive_import() -> Result<()> {
        let proc = prepare("mod.rs", r#"
        mod a {
            pub struct A;
            struct AA;

            pub use super::b::*;
        }

        mod b {
            pub struct B;
            struct BB;

            pub use super::c::*;
        }

        mod c {
            pub struct C;
            struct CC;

            pub use super::a::*;
        }
        "#)?;
        let ptree = &proc.ptree;

        let node_a = ptree.search(PathTree::ROOT, ["crate","mod","a","A"]).unwrap();
        let node_b = ptree.search(PathTree::ROOT, ["crate","mod","b","B"]).unwrap();
        let node_c = ptree.search(PathTree::ROOT, ["crate","mod","c","C"]).unwrap();

        assert_eq!(ptree[node_a].iter().count(), 1);
        assert_eq!(ptree[node_b].iter().count(), 1);
        assert_eq!(ptree[node_c].iter().count(), 1);

        let pid_a = node_a.to_path_id(0);
        let pid_b = node_b.to_path_id(0);
        let pid_c = node_c.to_path_id(0);

        let test = |key: &str, expect_dst: PathId| {
            let full_key = ["crate","mod"]
                .into_iter()
                .chain(key.split("::"));
            let node = ptree.search(PathTree::ROOT, full_key).unwrap();
            let mut values = ptree[node].iter();

            if values.clone().count() != 1 {
                panic!("error at key: `{key}`");
            }

            let (_, value) = values.next().unwrap();
            if !matches!(value, PathValue::Use(PathUse { dst, .. }) if *dst == expect_dst) {
                panic!("error at key: `{key}`");
            }
        };

        test("a::B", pid_b);
        test("a::C", pid_c);
        test("b::A", pid_a);
        test("b::C", pid_c);
        test("c::A", pid_a);
        test("c::B", pid_b);

        assert!(ptree.search(PathTree::ROOT, ["crate","mod","a","BB"]).is_none());
        assert!(ptree.search(PathTree::ROOT, ["crate","mod","a","CC"]).is_none());
        assert!(ptree.search(PathTree::ROOT, ["crate","mod","b","AA"]).is_none());
        assert!(ptree.search(PathTree::ROOT, ["crate","mod","b","CC"]).is_none());
        assert!(ptree.search(PathTree::ROOT, ["crate","mod","c","AA"]).is_none());
        assert!(ptree.search(PathTree::ROOT, ["crate","mod","c","BB"]).is_none());

        Ok(())
    }

    #[test]
    fn test_import_multiple_namespaces() -> Result<()> {
        let proc = prepare("mod.rs", r#"
        mod a {
            pub mod x {}  // Type namespace
            pub fn x() {} // Value namespace
        }

        mod b {
            pub mod x {}  // Type namespace
            pub fn x() {} // Value namespace
        }

        use a::x;     // brings 'mod' and 'fn' inside 'a'
        pub use a::*; // same as above
        use b::*;     // brings 'mod' and 'fn' inside 'b'
        "#)?;
        let ptree = &proc.ptree;

        let vis_node = ptree.search(PathTree::ROOT, ["crate","mod"]).unwrap();
        let node_a_x = ptree.search(PathTree::ROOT, ["crate","mod","a","x"]).unwrap();
        let node_b_x = ptree.search(PathTree::ROOT, ["crate","mod","b","x"]).unwrap();

        let mut expect = vec![
            (NodeIndex(0), node_a_x.to_path_id(0)),
            (NodeIndex(0), node_a_x.to_path_id(1)),
            (vis_node, node_b_x.to_path_id(0)),
            (vis_node, node_b_x.to_path_id(1))
        ];
        expect.sort_unstable();

        let node_x = ptree.search(PathTree::ROOT, ["crate","mod","x"]).unwrap();
        let mut use_values = ptree[node_x].iter().map(|(_, value)| {
            let PathValue::Use(v) = value else {
                panic!();
            };
            (v.vis_node, v.dst)
        }).collect::<Vec<_>>();
        use_values.sort_unstable();

        assert_eq!(use_values, expect);

        Ok(())
    }

    #[test]
    fn test_import_known_library() -> Result<()> {
        let proc = prepare("mod.rs", r#"
        use ops::Add;
        use std::ops;
        "#)?;
        let ptree = &proc.ptree;

        let node_add = ptree.search(PathTree::ROOT, ["core","ops","Add"]).unwrap();
        let node_ops = ptree.search(PathTree::ROOT, ["core","ops"]).unwrap();
        let pid_add = node_add.to_path_id(0);
        let pid_ops = node_ops.to_path_id(0);

        let add = ptree.search(PathTree::ROOT, ["crate","mod","Add"]).unwrap();
        assert!(matches!(
            ptree[add].iter().next().unwrap().1,
            PathValue::Use(PathUse { dst, .. })
            if *dst == pid_add
        ));

        let ops = ptree.search(PathTree::ROOT, ["crate","mod","ops"]).unwrap();
        assert!(matches!(
            ptree[ops].iter().next().unwrap().1,
            PathValue::Use(PathUse { dst, .. })
            if *dst == pid_ops
        ));


        Ok(())
    }

    #[test]
    fn test_various_pub_visibility() -> Result<()> {
        let proc = prepare("mod.rs", r#"
        mod a {
            mod b {
                pub mod c {
                    pub struct T0; // Visible at root
                    pub(crate) struct T1; // Visible at crate
                    pub(in super::super) struct T2; // Visible at 'a'
                    pub(super) struct T3; // Visible at 'b'
                    struct T4; // Visible at 'c'
                }
            }
        }
        "#)?;
        let ptree = &proc.ptree;

        let node_root = ptree.search(PathTree::ROOT, []).unwrap();
        let node_crate = ptree.search(PathTree::ROOT, ["crate"]).unwrap();
        let node_a = ptree.search(PathTree::ROOT, ["crate","mod","a"]).unwrap();
        let node_b = ptree.search(PathTree::ROOT, ["crate","mod","a","b"]).unwrap();
        let node_c = ptree.search(PathTree::ROOT, ["crate","mod","a","b","c"]).unwrap();

        let test = |key: &str, expect: NodeIndex| {
            let Some(node) = ptree.search(PathTree::ROOT, key.split("::")) else {
                panic!("error at key: `{key}`");
            };

            if !matches!(
                ptree[node].iter().next(),
                Some((_vi, PathValue::Struct(PathStruct { vis_node, .. })))
                if *vis_node == expect
            ) {
                panic!("error at key: `{key}`");
            }
        };

        test("crate::mod::a::b::c::T0", node_root);
        test("crate::mod::a::b::c::T1", node_crate);
        test("crate::mod::a::b::c::T2", node_a);
        test("crate::mod::a::b::c::T3", node_b);
        test("crate::mod::a::b::c::T4", node_c);

        Ok(())
    }

    #[test]
    fn test_choosing_visible_item() -> Result<()> {
        let proc = prepare("mod.rs", r#"
        mod a {
            struct A;
        }

        mod b {
            pub struct A;
        }

        mod c {
            struct A;
        }

        mod d {
            use super::{a::*, b::*, c::*};

            struct T {
                a: A, // b::A
            }
        }
        "#)?;
        let ptree = &proc.ptree;

        let node_b = ptree.search(PathTree::ROOT, ["crate","mod","b","A"]).unwrap();
        let pid_b = node_b.to_path_id(0);

        let node_field = ptree.search(PathTree::ROOT, ["crate","mod","d","T","a"]).unwrap();
        assert!(matches!(
            ptree[node_field].iter().next().unwrap().1,
            PathValue::Field(PathField { tid, .. })
            if ptree.get_type(*tid) == &RefType::Path(pid_b)
        ));

        Ok(())
    }

    #[test]
    fn test_shorten_type_alias_chain() -> Result<()> {
        let proc = prepare("mod.rs", r#"
        mod a {
            pub struct A;
        }

        mod b {
            pub type B = super::a::A;
        }

        mod c {
            pub type C = (super::a::A, super::b::B);
        }

        mod d {
            pub type D = [super::c::C; 1];
        }
        "#)?;
        let ptree = &proc.ptree;

        let node_a = ptree.search(PathTree::ROOT, ["crate","mod","a","A"]).unwrap();
        let pid_a = node_a.to_path_id(0);

        let node_b = ptree.search(PathTree::ROOT, ["crate","mod","b","B"]).unwrap();
        let tid_b = ptree.get_type_id_of(&RefType::Path(pid_a)).unwrap();
        assert!(matches!(
            ptree[node_b].iter().next().unwrap().1,
            PathValue::TypeAlias(PathTypeAlias { tid, .. })
            if *tid == tid_b
        ));

        let node_c = ptree.search(PathTree::ROOT, ["crate","mod","c","C"]).unwrap();
        let tid_c = ptree.get_type_id_of(&RefType::Tuple(vec![tid_b, tid_b])).unwrap();
        assert!(matches!(
            ptree[node_c].iter().next().unwrap().1,
            PathValue::TypeAlias(PathTypeAlias { tid, .. })
            if *tid == tid_c
        ));

        let node_d = ptree.search(PathTree::ROOT, ["crate","mod","d","D"]).unwrap();
        let tid_d = ptree.get_type_id_of(&RefType::Array {
            elem: tid_c
        }).unwrap();
        assert!(matches!(
            ptree[node_d].iter().next().unwrap().1,
            PathValue::TypeAlias(PathTypeAlias { tid, .. })
            if *tid == tid_d
        ));

        Ok(())
    }

    #[test]
    fn test_shorten_mixed_use_type_alias_chain() -> Result<()> {
        let proc = prepare("mod.rs", r#"
        mod a {
            pub struct A;
        }

        mod b {
            pub type B = super::a::A;
        }

        mod c {
            pub use super::b::B as C;
        }

        mod d {
            pub type D = super::c::C;
        }

        mod e {
            pub use super::d::D;
        }
        "#)?;
        let ptree = &proc.ptree;

        let node_a = ptree.search(PathTree::ROOT, ["crate","mod","a","A"]).unwrap();
        let pid_a = node_a.to_path_id(0);

        let node_b = ptree.search(PathTree::ROOT, ["crate","mod","b","B"]).unwrap();
        assert!(matches!(
            ptree[node_b].iter().next().unwrap().1,
            PathValue::TypeAlias(PathTypeAlias { tid, .. })
            if ptree.get_type(*tid) == &RefType::Path(pid_a)
        ));

        let node_c = ptree.search(PathTree::ROOT, ["crate","mod","c","C"]).unwrap();
        assert!(matches!(
            ptree[node_c].iter().next().unwrap().1,
            PathValue::Use(PathUse { dst, .. })
            if *dst == pid_a
        ));

        let node_d = ptree.search(PathTree::ROOT, ["crate","mod","d","D"]).unwrap();
        assert!(matches!(
            ptree[node_d].iter().next().unwrap().1,
            PathValue::TypeAlias(PathTypeAlias { tid, .. })
            if ptree.get_type(*tid) == &RefType::Path(pid_a)
        ));

        let node_e = ptree.search(PathTree::ROOT, ["crate","mod","e","D"]).unwrap();
        assert!(matches!(
            ptree[node_e].iter().next().unwrap().1,
            PathValue::Use(PathUse { dst, .. })
            if *dst == pid_a
        ));

        Ok(())
    }

    /// Tests if the processor can correctly resolve function parameters.
    #[test]
    fn test_fn_input() -> Result<()> {
        let proc = prepare("mod.rs", r#"
        struct T {
            a: f32,
            b: f64,
        }

        fn f0(a: i32, b: i64) {}

        fn f1(a: [i32; 2]) {}

        fn f2(T { a, b }: T) {}

        fn f3(a: (i32, i64)) {}
        "#)?;
        let ptree = &proc.ptree;

        let pid_i32 = ptree.search(PathTree::ROOT, ["i32"]).unwrap().to_path_id(0);
        let pid_i64 = ptree.search(PathTree::ROOT, ["i64"]).unwrap().to_path_id(0);
        let pid_f32 = ptree.search(PathTree::ROOT, ["f32"]).unwrap().to_path_id(0);
        let pid_f64 = ptree.search(PathTree::ROOT, ["f64"]).unwrap().to_path_id(0);

        let f0_a = ptree.search(PathTree::ROOT, ["crate","mod","f0","a"]).unwrap();
        let tid_i32 = ptree.get_type_id_of(&RefType::Path(pid_i32)).unwrap();
        assert!(matches!(
            ptree[f0_a].iter().next().unwrap().1,
            PathValue::Local(PathLocal { tid, .. })
            if *tid == tid_i32
        ));
        
        let f0_b = ptree.search(PathTree::ROOT, ["crate","mod","f0","b"]).unwrap();
        let tid_i64 = ptree.get_type_id_of(&RefType::Path(pid_i64)).unwrap();
        assert!(matches!(
            ptree[f0_b].iter().next().unwrap().1,
            PathValue::Local(PathLocal { tid, .. })
            if *tid == tid_i64
        ));

        let f1_a = ptree.search(PathTree::ROOT, ["crate","mod","f1","a"]).unwrap();
        let exp = ptree.get_type_id_of(&RefType::Array { elem: tid_i32 }).unwrap();
        assert!(matches!(
            ptree[f1_a].iter().next().unwrap().1,
            PathValue::Local(PathLocal { tid, .. })
            if *tid == exp
        ));

        let f2_a = ptree.search(PathTree::ROOT, ["crate","mod","f2","a"]).unwrap();
        let tid_f32 = ptree.get_type_id_of(&RefType::Path(pid_f32)).unwrap();
        assert!(matches!(
            ptree[f2_a].iter().next().unwrap().1,
            PathValue::Local(PathLocal { tid, .. })
            if *tid == tid_f32
        ));

        let f2_b = ptree.search(PathTree::ROOT, ["crate","mod","f2","b"]).unwrap();
        let tid_f64 = ptree.get_type_id_of(&RefType::Path(pid_f64)).unwrap();
        assert!(matches!(
            ptree[f2_b].iter().next().unwrap().1,
            PathValue::Local(PathLocal { tid, .. })
            if *tid == tid_f64
        ));

        let f3_a = ptree.search(PathTree::ROOT, ["crate","mod","f3","a"]).unwrap();
        let exp = ptree.get_type_id_of(&RefType::Tuple(vec![tid_i32, tid_i64])).unwrap();
        assert!(matches!(
            ptree[f3_a].iter().next().unwrap().1,
            PathValue::Local(PathLocal { tid, .. })
            if *tid == exp
        ));

        Ok(())
    }

    #[test]
    fn test_local() -> Result<()> {
        let proc = prepare("mod.rs", r#"
        fn f1() {
            let a = 0;          // InferredInt
            let b = 0_i32;      // i32
            let c: i32 = 0;     // i32
            let d = [0; 1];     // Array (InferredInt)
            let e = (0_i32, 1); // Tuple (i32, InferredInt)
            let mut f = 1;
            f = 1_i32;          // i32

            let g = 1;          // u32
            let h = g + 1;      // u32
            let i = h + 1_u32;  // u32
            {
                let g = g;      // u32
            }
        }
        "#)?;
        let ptree = &proc.ptree;

        let pid_i32 = ptree.search(PathTree::ROOT, ["i32"]).unwrap().to_path_id(0);
        let pid_u32 = ptree.search(PathTree::ROOT, ["u32"]).unwrap().to_path_id(0);

        let f1_a = ptree.search(PathTree::ROOT, ["crate","mod","f1","{0}","a"]).unwrap();
        let tid_int = ptree.get_type_id_of(&RefType::InferredInt).unwrap();
        assert!(matches!(
            ptree[f1_a].iter().next().unwrap().1,
            PathValue::Local(PathLocal { tid, .. })
            if *tid == tid_int
        ));

        let f1_b = ptree.search(PathTree::ROOT, ["crate","mod","f1","{0}","b"]).unwrap();
        let tid_i32 = ptree.get_type_id_of(&RefType::Path(pid_i32)).unwrap();
        assert!(matches!(
            ptree[f1_b].iter().next().unwrap().1,
            PathValue::Local(PathLocal { tid, .. })
            if *tid == tid_i32
        ));

        let f1_c = ptree.search(PathTree::ROOT, ["crate","mod","f1","{0}","c"]).unwrap();
        assert!(matches!(
            ptree[f1_c].iter().next().unwrap().1,
            PathValue::Local(PathLocal { tid, .. })
            if *tid == tid_i32
        ));

        let f1_d = ptree.search(PathTree::ROOT, ["crate","mod","f1","{0}","d"]).unwrap();
        let exp = ptree.get_type_id_of(&RefType::Array { elem: tid_int }).unwrap();
        assert!(matches!(
            ptree[f1_d].iter().next().unwrap().1,
            PathValue::Local(PathLocal { tid, .. })
            if *tid == exp
        ));

        let f1_e = ptree.search(PathTree::ROOT, ["crate","mod","f1","{0}","e"]).unwrap();
        let exp = ptree.get_type_id_of(&RefType::Tuple(vec![tid_i32, tid_int])).unwrap();
        assert!(matches!(
            ptree[f1_e].iter().next().unwrap().1,
            PathValue::Local(PathLocal { tid, .. })
            if *tid == exp
        ));

        let f1_f = ptree.search(PathTree::ROOT, ["crate","mod","f1","{0}","f"]).unwrap();
        assert!(matches!(
            ptree[f1_f].iter().next().unwrap().1,
            PathValue::Local(PathLocal { tid, .. })
            if *tid == tid_i32
        ));

        let f1_g = ptree.search(PathTree::ROOT, ["crate","mod","f1","{0}","g"]).unwrap();
        let tid_u32 = ptree.get_type_id_of(&RefType::Path(pid_u32)).unwrap();
        assert!(matches!(
            ptree[f1_g].iter().next().unwrap().1,
            PathValue::Local(PathLocal { tid, .. })
            if *tid == tid_u32
        ));

        let f1_h = ptree.search(PathTree::ROOT, ["crate","mod","f1","{0}","h"]).unwrap();
        assert!(matches!(
            ptree[f1_h].iter().next().unwrap().1,
            PathValue::Local(PathLocal { tid, .. })
            if *tid == tid_u32
        ));

        let f1_i = ptree.search(PathTree::ROOT, ["crate","mod","f1","{0}","i"]).unwrap();
        assert!(matches!(
            ptree[f1_i].iter().next().unwrap().1,
            PathValue::Local(PathLocal { tid, .. })
            if *tid == tid_u32
        ));

        let f1_g = ptree.search(PathTree::ROOT, ["crate","mod","f1","{0}","{0}","g"]).unwrap();
        assert!(matches!(
            ptree[f1_g].iter().next().unwrap().1,
            PathValue::Local(PathLocal { tid, .. })
            if *tid == tid_u32
        ));

        Ok(())
    }

    fn prepare(fname: &str, code: &'static str) -> Result<PathProcessor> {
        let dir = env::current_dir()?;
        let path = dir.join(fname);

        // Puts the given code in the test suite.
        TEST_FILES.with_borrow_mut(|map| {
            map.insert(path.clone(), code);
        });

        let entry = dir.join("main.rs");
        let mut proc = PathProcessor::new(&entry)?;
        proc.import([&path])?;
        proc.process()?;
        Ok(proc)
    }
}
