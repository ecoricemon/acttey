use super::{
    attr::Attribute,
    function::{CompoundStatement, WgslFn},
    structs::{BeWgslStruct, StructMember, WgslStruct},
    to_code::{ConstructPrettyCode, ConstructWgslCode},
    util,
    var::WgslVarDecl,
};

pub trait BeWgslModule {
    fn be_module() -> WgslModule;
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct WgslModule {
    /// Each entry such as struct, global variable, and function.
    pub entries: Vec<WgslEntry>,
}

impl WgslModule {
    pub const fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    pub fn of<T: BeWgslModule>() -> Self {
        T::be_module()
    }

    pub fn merge(&mut self, other: Self) {
        self.entries.extend(other.entries)
    }

    /// Generates WGSL code.
    pub fn build(&self) -> String {
        let mut buf = String::new();
        for entry in self.entries.iter() {
            entry.write_wgsl_string(&mut buf);
        }
        buf
    }

    /// Generates WGSL code with white space.
    pub fn build_pretty(&self) -> String {
        let mut buf = String::new();
        for entry in self
            .entries
            .iter()
            .take(self.entries.len().saturating_sub(1))
        {
            entry.write_pretty_code(&mut buf);
            buf.push('\n');
        }
        if let Some(last_entry) = self.entries.last() {
            last_entry.write_pretty_code(&mut buf);
        }

        #[cfg(debug_assertions)]
        {
            let mut non_pretty = self.build();
            let mut pretty = buf.clone();
            non_pretty.retain(|c| !c.is_whitespace());
            pretty.retain(|c| !c.is_whitespace());
            debug_assert_eq!(non_pretty, pretty, "internal bug detected");
        }

        buf
    }

    /// Retrieves the index of the struct that has the given name.
    pub fn find_struct(&self, ident: &str) -> Option<usize> {
        util::find_index(self.entries.iter(), ident, |entry| {
            entry.as_struct().map(|st| st.ident.as_str())
        })
    }

    /// Determines if the struct exists.
    pub fn contains_struct(&self, ident: &str) -> bool {
        self.find_struct(ident).is_some()
    }

    /// Retrieves the struct that has the given name.
    pub fn get_struct(&self, ident: &str) -> Option<&WgslStruct> {
        // Safety: `i` points to valid `ShaderEntry::Struct`.
        self.find_struct(ident).map(|i| unsafe {
            self.entries
                .get(i)
                .unwrap_unchecked()
                .as_struct()
                .unwrap_unchecked()
        })
    }

    /// Retrieves the struct that has the given name.
    pub fn get_struct_mut(&mut self, ident: &str) -> Option<&mut WgslStruct> {
        // Safety: `i` points to valid `ShaderEntry::Struct`.
        self.find_struct(ident).map(|i| unsafe {
            self.entries
                .get_mut(i)
                .unwrap_unchecked()
                .as_struct_mut()
                .unwrap_unchecked()
        })
    }

    /// Appends the given struct.
    pub fn push_struct_of<T: BeWgslStruct>(&mut self) {
        self.push_struct(T::be_struct())
    }

    /// Appends the given struct.
    pub fn push_struct(&mut self, st: WgslStruct) {
        self.entries.push(WgslEntry::Struct(st));
    }

    /// Tries to remove the struct that has the given name.
    /// If succeeded, returns removed one.
    pub fn remove_struct(&mut self, ident: &str) -> Option<WgslStruct> {
        self.find_struct(ident)
            .map(|i| match self.entries.remove(i) {
                WgslEntry::Struct(st) => st,
                _ => unreachable!(),
            })
    }

    /// Tries to remove the struct member that has the given name.
    /// If succeeded, returns removed one.
    pub fn remove_struct_member(&mut self, st: &str, member: &str) -> Option<StructMember> {
        if let Some(st) = self.get_struct_mut(st) {
            st.remove_member(member)
        } else {
            None
        }
    }

    /// Inserts a new struct member attribute.
    /// If there was same attribute variant already,
    /// Its inner value is changed with the given `inner`.
    pub fn insert_struct_member_attribute(
        &mut self,
        struct_ident: &str,
        member_ident: &str,
        attr_outer: &str,
        attr_inner: Option<&str>,
    ) {
        if let Some(st) = self.get_struct_mut(struct_ident) {
            if let Some(member) = st.get_member_mut(member_ident) {
                member.insert_attribute(attr_outer, attr_inner);
            }
        }
    }

    pub fn retain_struct_members<'a>(
        &mut self,
        struct_ident: &str,
        member_idents: impl Iterator<Item = &'a str> + Clone,
    ) {
        if let Some(st) = self.get_struct_mut(struct_ident) {
            st.retain_members(member_idents);
        }
    }

    pub fn reorder_struct_members<'a>(
        &mut self,
        struct_ident: &str,
        order: impl Iterator<Item = &'a str>,
    ) {
        if let Some(st) = self.get_struct_mut(struct_ident) {
            st.reorder_members(order);
        }
    }

    /// Retrieves the index of the global variable that has the given name.
    pub fn find_global_variable(&self, ident: &str) -> Option<usize> {
        util::find_index(self.entries.iter(), ident, |entry| {
            entry.as_global_variable().map(|var| var.ident.as_str())
        })
    }

    /// Determines if the global variable exists.
    pub fn contains_global_variable(&self, ident: &str) -> bool {
        self.find_global_variable(ident).is_some()
    }

    /// Retrieves the `my_wgsl::GlobalVariable` that has the given name.
    pub fn get_global_variable(&self, ident: &str) -> Option<&WgslVarDecl> {
        // Safety: `i` points to valid `ShaderEntry::GlobalVariable`.
        self.find_global_variable(ident).map(|i| unsafe {
            self.entries
                .get(i)
                .unwrap_unchecked()
                .as_global_variable()
                .unwrap_unchecked()
        })
    }

    /// Retrieves the `my_wgsl::GlobalVariable` that has the given name.
    pub fn get_global_variable_mut(&mut self, ident: &str) -> Option<&mut WgslVarDecl> {
        // Safety: `i` points to valid `ShaderEntry::GlobalVariable`.
        self.find_global_variable(ident).map(|i| unsafe {
            self.entries
                .get_mut(i)
                .unwrap_unchecked()
                .as_global_variable_mut()
                .unwrap_unchecked()
        })
    }

    /// Appends the given global variable.
    pub fn push_global_variable(&mut self, var: WgslVarDecl) {
        self.entries.push(WgslEntry::GlobalVariable(var));
    }

    /// Tries to remove the global variable that has the given name.
    /// If succeeded, returns removed one.
    pub fn remove_global_variable(&mut self, ident: &str) -> Option<WgslVarDecl> {
        self.find_global_variable(ident)
            .map(|i| match self.entries.remove(i) {
                WgslEntry::GlobalVariable(var) => var,
                _ => unreachable!(),
            })
    }

    /// Retrieves the index of the function that has the given name.
    pub fn find_function(&self, ident: &str) -> Option<usize> {
        util::find_index(self.entries.iter(), ident, |entry| {
            entry.as_function().map(|f| f.ident.as_str())
        })
    }

    /// Determines if the function exists.
    pub fn contains_function(&self, ident: &str) -> bool {
        self.find_function(ident).is_some()
    }

    /// Retrieves the `my_wgsl::Function` that has the given name.
    pub fn get_function(&self, ident: &str) -> Option<&WgslFn> {
        // Safety: `i` points to valid `ShaderEntry::Function`.
        self.find_function(ident).map(|i| unsafe {
            self.entries
                .get(i)
                .unwrap_unchecked()
                .as_function()
                .unwrap_unchecked()
        })
    }

    /// Retrieves the `my_wgsl::Function` that has the given name.
    pub fn get_function_mut(&mut self, ident: &str) -> Option<&mut WgslFn> {
        // Safety: `i` points to valid `ShaderEntry::Function`.
        self.find_function(ident).map(|i| unsafe {
            self.entries
                .get_mut(i)
                .unwrap_unchecked()
                .as_function_mut()
                .unwrap_unchecked()
        })
    }

    /// Appends the given function.
    pub fn push_function(&mut self, f: WgslFn) {
        self.entries.push(WgslEntry::Function(f))
    }

    /// Tries to remove the function that has the given name.
    /// If succeeded, returns removed one.
    pub fn remove_function(&mut self, ident: &str) -> Option<WgslFn> {
        self.find_function(ident)
            .map(|i| match self.entries.remove(i) {
                WgslEntry::Function(f) => f,
                _ => unreachable!(),
            })
    }

    pub fn remove_function_statement(
        &mut self,
        fn_ident: &str,
        attr_outer: &str,
        attr_inner: Option<&str>,
    ) -> Vec<CompoundStatement> {
        if let Some(f) = self.get_function_mut(fn_ident) {
            f.remove_statement(attr_outer, attr_inner)
        } else {
            Vec::new()
        }
    }

    pub fn make_bare_function_statement(
        &mut self,
        fn_ident: &str,
        attr_outer: &str,
        attr_inner: Option<&str>,
    ) {
        if let Some(f) = self.get_function_mut(fn_ident) {
            f.stmt.make_bare_statements_recur(attr_outer, attr_inner);
        }
    }

    /// Returns the first function that has `vertex` attribute.
    pub fn get_vertex_stage_ident(&self) -> Option<&str> {
        self.entries.iter().find_map(|entry| {
            if let WgslEntry::Function(f) = entry {
                if f.attrs.contains_attribute(&Attribute::Vertex) {
                    return Some(f.ident.as_str());
                }
            }
            None
        })
    }

    /// Returns the first function that has `fragment` attribute.
    pub fn get_fragment_stage_ident(&self) -> Option<&str> {
        self.entries.iter().find_map(|entry| {
            if let WgslEntry::Function(f) = entry {
                if f.attrs.contains_attribute(&Attribute::Fragment) {
                    return Some(f.ident.as_str());
                }
            }
            None
        })
    }

    /// Returns the first function that has `compute` attribute.
    pub fn get_compute_stage_ident(&self) -> Option<&str> {
        self.entries.iter().find_map(|entry| {
            if let WgslEntry::Function(f) = entry {
                if f.attrs.contains_attribute(&Attribute::Compute) {
                    return Some(f.ident.as_str());
                }
            }
            None
        })
    }
}

macro_rules! impl_from_tuple_for_wgsl_module {
    ($($i:expr),*) => {
        paste::paste! {
            #[allow(unused_parens)]
            impl<$([<A $i>]: BeWgslModule),*> From<( $([<A $i>]),* )> for WgslModule {
                fn from(_: ( $([<A $i>]),* )) -> Self {
                    let mut total = WgslModule::new();
                    $(
                        total.merge([<A $i>]::be_module());
                    )*
                    total
                }
            }
        }
    };
}

impl_from_tuple_for_wgsl_module!(0);
impl_from_tuple_for_wgsl_module!(0, 1);
impl_from_tuple_for_wgsl_module!(0, 1, 2);
impl_from_tuple_for_wgsl_module!(0, 1, 2, 3);
impl_from_tuple_for_wgsl_module!(0, 1, 2, 3, 4);
impl_from_tuple_for_wgsl_module!(0, 1, 2, 3, 4, 5);
impl_from_tuple_for_wgsl_module!(0, 1, 2, 3, 4, 5, 6);
impl_from_tuple_for_wgsl_module!(0, 1, 2, 3, 4, 5, 6, 7);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum WgslEntry {
    Struct(WgslStruct),
    GlobalVariable(WgslVarDecl),
    Function(WgslFn),
}

macro_rules! impl_shader_entry_matcher {
    ($fn:ident, $kind:ident, $ret:ty) => {
        pub fn $fn(&self) -> Option<&$ret> {
            if let Self::$kind(inner) = self {
                Some(inner)
            } else {
                None
            }
        }

        paste::paste! {
            pub fn [<$fn _mut>](&mut self) -> Option<&mut $ret> {
                if let Self::$kind(inner) = self {
                    Some(inner)
                } else {
                    None
                }
            }
        }
    };
}

impl WgslEntry {
    impl_shader_entry_matcher!(as_struct, Struct, WgslStruct);
    impl_shader_entry_matcher!(as_global_variable, GlobalVariable, WgslVarDecl);
    impl_shader_entry_matcher!(as_function, Function, WgslFn);
}

impl ConstructWgslCode for WgslEntry {
    fn write_wgsl_string(&self, buf: &mut String) {
        match self {
            Self::Struct(st) => st.write_wgsl_string(buf),
            Self::GlobalVariable(global_variable) => global_variable.write_wgsl_string(buf),
            Self::Function(function) => function.write_wgsl_string(buf),
        }
    }
}

impl ConstructPrettyCode for WgslEntry {
    fn write_pretty_code(&self, buf: &mut String) {
        match self {
            Self::Struct(st) => st.write_pretty_code(buf),
            Self::GlobalVariable(global_variable) => global_variable.write_pretty_code(buf),
            Self::Function(function) => function.write_pretty_code(buf),
        }
    }
}
