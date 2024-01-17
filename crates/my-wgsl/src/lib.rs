//! # A crate to generate WGSL text from rust code.
//!
//! ## When to use
//!
//! - When you want to compose multiple WGSL pieces into one WGSL text,
//!   so that you can generate shader module from it.
//! - When you want to modify WGSL piece such as deletion.
//!
//! ## Example
//!
//! ```
//! // ref: https://www.w3.org/TR/WGSL/
//! use my_wgsl::*;
//!
//! #[wgsl_decl_struct]
//! struct PointLight {
//!     position: vec3f,
//!     color: vec3f,
//! }
//!
//! #[wgsl_decl_struct]
//! struct LightStorage {
//!     pointCount: u32,
//!     point: array<PointLight>,
//! }
//!
//! let mut builder = Builder::new();
//!
//! wgsl_structs![builder, PointLight, LightStorage];
//!
//! wgsl_bind!(
//!     builder, group(0) binding(0) var<storage> lights : LightStorage
//! );
//! wgsl_bind!(
//!     builder, group(1) binding(0) var baseColorSampler : sampler
//! );
//! wgsl_bind!(
//!     builder, group(1) binding(1) var baseColorTexture : texture_2d<f32>
//! );
//!
//! wgsl_fn!(builder,
//!     #[fragment]
//!     fn fragmentMain(#[location(0)] worldPos : vec3f,
//!                     #[location(1)] normal : vec3f,
//!                     #[location(2)] uv : vec2f) -> #[location(0)] vec4f {
//!         // Sample the base color of the surface from a texture.
//!         let baseColor = textureSample(baseColorTexture, baseColorSampler, uv);
//!
//!         let N = normalize(normal);
//!         var surfaceColor = vec3f(0);
//!
//!         // Loop over the scene point lights.
//!         for (var i = 0u; i < lights.pointCount; ++i) {
//!             let worldToLight = lights.point[i].position - worldPos;
//!             let dist = length(worldToLight);
//!             let dir = normalize(worldToLight);
//!
//!             // Determine the contribution of this light to the surface color.
//!             let radiance = lights.point[i].color * (1 / pow(dist, 2));
//!             let nDotL = max(dot(N, dir), 0);
//!
//!             // Accumulate light contribution to the surface color.
//!             surfaceColor += baseColor.rgb * radiance * nDotL;
//!         }
//!
//!         // Return the accumulated surface color.
//!         return vec4(surfaceColor, baseColor.a);
//!         
//!     }
//! );
//!
//! let wgsl: String = builder.build();
//! // You can use `wgsl` to make shader module.
//! ```
#![allow(non_camel_case_types)]

use paste::paste;
use smallvec::SmallVec;
use std::marker::PhantomData;

pub use my_wgsl_macros::*;
use util::*;

/// Re-export smallvec.
pub use smallvec;

const TAB_SIZE: usize = 2;

pub trait ToIdent {
    fn maybe_ident() -> Option<&'static str> {
        None
    }

    fn to_ident() -> String;
}

pub trait ToIdentPretty: ToIdent {
    fn maybe_ident_pretty() -> Option<&'static str> {
        Self::maybe_ident()
    }

    fn to_ident_pretty() -> String {
        Self::to_ident()
    }
}

pub trait PutStr {
    fn put_ident(&self, buf: &mut String);

    fn put_str(&self, buf: &mut String);

    // `to_string()` can conflict with ToString, so use another name.
    fn to_str(&self) -> String {
        let mut buf = String::new();
        self.put_str(&mut buf);
        buf
    }
}

// Temporary pretty style printer.
pub trait PutStrPretty: PutStr {
    fn put_ident_pretty(&self, buf: &mut String) {
        self.put_ident(buf)
    }

    fn put_str_pretty(&self, buf: &mut String) {
        self.put_str(buf)
    }

    fn to_str_pretty(&self) -> String {
        let mut buf = String::new();
        self.put_str_pretty(&mut buf);
        buf
    }
}

impl PutStr for String {
    fn put_ident(&self, buf: &mut String) {
        buf.push_str(self);
    }

    fn put_str(&self, buf: &mut String) {
        buf.push_str(self);
    }
}

impl PutStrPretty for String {}

pub trait AsStructure: ToIdent {
    fn as_structure() -> Structure;
}

impl<T: AsStructure> PutStr for T {
    fn put_ident(&self, buf: &mut String) {
        buf.push_str(&Self::to_ident())
    }

    fn put_str(&self, buf: &mut String) {
        T::as_structure().put_str(buf)
    }
}

#[derive(Default, Debug)]
pub struct Builder {
    /// Each shader entry such as struct, global variable, and function.
    pub entries: Vec<ShaderEntry>,
}

impl Builder {
    pub fn new() -> Self {
        Self::default()
    }

    /// Generates WGSL text.
    pub fn build(&self) -> String {
        let mut res = String::new();
        for entry in self.entries.iter() {
            entry.put_str(&mut res);
        }
        res
    }

    /// Generates WGSL text with white space.
    pub fn build_pretty(&self) -> String {
        let mut res = String::new();
        for entry in self
            .entries
            .iter()
            .take(self.entries.len().saturating_sub(1))
        {
            entry.put_str_pretty(&mut res);
            res.push('\n');
        }
        if let Some(last_entry) = self.entries.last() {
            last_entry.put_str_pretty(&mut res);
        }

        #[cfg(debug_assertions)]
        {
            let mut non_pretty = self.build();
            let mut pretty = res.clone();
            non_pretty.retain(|c| !c.is_whitespace());
            pretty.retain(|c| !c.is_whitespace());
            debug_assert_eq!(non_pretty, pretty, "internal bug detected");
        }

        res
    }

    /// Retrieves the index of the structure that has the given name.
    pub fn find_structure(&self, ident: &str) -> Option<usize> {
        find_index(self.entries.iter(), ident, |entry| {
            entry.as_structure().map(|structure| structure.ident)
        })
    }

    /// Retrieves the `my_wgsl::Structure` that has the given name.
    pub fn get_structure(&self, ident: &str) -> Option<&Structure> {
        // Safety: `i` points to valid `ShaderEntry::Structure`.
        self.find_structure(ident).map(|i| unsafe {
            self.entries
                .get(i)
                .unwrap_unchecked()
                .as_structure()
                .unwrap_unchecked()
        })
    }

    /// Retrieves the `my_wgsl::Structure` that has the given name.
    pub fn get_structure_mut(&mut self, ident: &str) -> Option<&mut Structure> {
        // Safety: `i` points to valid `ShaderEntry::Structure`.
        self.find_structure(ident).map(|i| unsafe {
            self.entries
                .get_mut(i)
                .unwrap_unchecked()
                .as_structure_mut()
                .unwrap_unchecked()
        })
    }

    /// Appends the given structure.
    pub fn push_structure(&mut self, structure: Structure) {
        self.entries.push(ShaderEntry::Structure(structure));
    }

    /// Tries to remove the structure that has the given name.
    /// If succeeded, returns removed one.
    pub fn remove_structure(&mut self, ident: &str) -> Option<Structure> {
        self.find_structure(ident)
            .map(|i| match self.entries.remove(i) {
                ShaderEntry::Structure(structure) => structure,
                _ => unreachable!(),
            })
    }

    /// Tries to remove the structure member that has the given name.
    /// If succeeded, returns removed one.
    pub fn remove_structure_member(&mut self, st: &str, member: &str) -> Option<StructureMember> {
        if let Some(st) = self.get_structure_mut(st) {
            st.remove_member(member)
        } else {
            None
        }
    }

    /// Inserts a new structure member attribure.
    /// If there was same attribute variant already,
    /// Its inner value is changed with the given `inner`.
    pub fn insert_structure_member_attribute(
        &mut self,
        struct_ident: &str,
        member_ident: &str,
        attr_outer: &str,
        attr_inner: Option<&str>,
    ) {
        if let Some(st) = self.get_structure_mut(struct_ident) {
            if let Some(member) = st.get_member_mut(member_ident) {
                if let Some(i) = member.attrs.find_attribute_partial(attr_outer, attr_inner) {
                    member.attrs.0[i].set_inner(attr_inner.unwrap_or_default());
                } else {
                    member.attrs.0.push(Attribute::from((
                        attr_outer,
                        attr_inner.unwrap_or_default(),
                    )));
                }
            }
        }
    }

    /// Retains only the specified members by the `member_idents` in the struct.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_wgsl::*;
    ///
    /// #[wgsl_decl_struct]
    /// struct VertexInput {
    ///     #[builtin(vertex_index)] vertexIndex: u32,
    ///     #[builtin(instance_index)] instanceIndex: u32,
    ///     #[location(0)] position: vec3<f32>,
    ///     #[location(1)] normal: vec3<f32>
    /// }
    ///
    /// let mut builder = Builder::new();
    ///
    /// // It has initially four members.
    /// wgsl_structs!(builder, VertexInput);
    /// assert_eq!(4, builder.get_structure("VertexInput").unwrap().members.len());
    ///
    /// // Retains only two members.
    /// builder.retain_structure_members("VertexInput", &["position", "normal"]);
    /// assert_eq!(2, builder.get_structure("VertexInput").unwrap().members.len());
    /// ```
    pub fn retain_structure_members<'a>(
        &mut self,
        struct_ident: &str,
        member_idents: impl Iterator<Item = &'a str>,
    ) {
        if let Some(st) = self.get_structure_mut(struct_ident) {
            st.retain_members(member_idents);
        }
    }

    /// Reorders structure's members along the given `order`.
    /// Caller should guarantee that all idents in `order` appear only once
    /// and match with the structure's members.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_wgsl::*;
    ///
    /// #[wgsl_decl_struct]
    /// struct VertexInput {
    ///     #[location(0)] position: vec3<f32>,
    ///     #[location(1)] normal: vec3<f32>,
    ///     #[location(2)] color: vec3<f32>
    /// }
    ///
    /// let mut builder = Builder::new();
    ///
    /// wgsl_structs!(builder, VertexInput);
    ///
    /// builder.reorder_structure_members("VertexInput", &["normal", "color", "position"]);
    ///
    /// let st = builder.get_structure("VertexInput").unwrap();
    /// assert_eq!("normal", st.members[0].ident);
    /// assert_eq!("color", st.members[1].ident);
    /// assert_eq!("position", st.members[2].ident);
    /// ```
    pub fn reorder_structure_members<'a>(
        &mut self,
        struct_ident: &str,
        order: impl Iterator<Item = &'a str>,
    ) {
        if let Some(st) = self.get_structure_mut(struct_ident) {
            st.reorder_members(order);
        }
    }

    /// Retrieves the index of the global variable that has the given name.
    pub fn find_global_variable(&self, ident: &str) -> Option<usize> {
        find_index(self.entries.iter(), ident, |entry| {
            entry.as_global_variable().map(|var| var.ident)
        })
    }

    /// Retrieves the `my_wgsl::GlobalVariable` that has the given name.
    pub fn get_global_variable(&self, ident: &str) -> Option<&GlobalVariable> {
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
    pub fn get_global_variable_mut(&mut self, ident: &str) -> Option<&mut GlobalVariable> {
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
    pub fn push_global_variable(&mut self, var: GlobalVariable) {
        self.entries.push(ShaderEntry::GlobalVariable(var));
    }

    /// Tries to remove the global variable that has the given name.
    /// If succeeded, returns removed one.
    pub fn remove_global_variable(&mut self, ident: &str) -> Option<GlobalVariable> {
        self.find_global_variable(ident)
            .map(|i| match self.entries.remove(i) {
                ShaderEntry::GlobalVariable(var) => var,
                _ => unreachable!(),
            })
    }

    /// Retrieves the index of the function that has the given name.
    pub fn find_function(&self, ident: &str) -> Option<usize> {
        find_index(self.entries.iter(), ident, |entry| {
            entry.as_function().map(|f| f.ident)
        })
    }

    /// Retrieves the `my_wgsl::Function` that has the given name.
    pub fn get_function(&self, ident: &str) -> Option<&Function> {
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
    pub fn get_function_mut(&mut self, ident: &str) -> Option<&mut Function> {
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
    pub fn push_function(&mut self, f: Function) {
        self.entries.push(ShaderEntry::Function(f))
    }

    /// Tries to remove the function that has the given name.
    /// If succeeded, returns removed one.
    pub fn remove_function(&mut self, ident: &str) -> Option<Function> {
        self.find_function(ident)
            .map(|i| match self.entries.remove(i) {
                ShaderEntry::Function(f) => f,
                _ => unreachable!(),
            })
    }

    /// Removes matched statements in the function with the given `fn_ident` name.
    /// Returns removed statements.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_wgsl::*;
    ///
    /// let mut builder = Builder::new();
    /// wgsl_fn!(builder,
    ///     fn foo() {
    ///         #[ID(0)] {
    ///             return vec4f(0.0, 0.0, 0.0, 1.0);
    ///         }
    ///         #[ID(1)] {
    ///             return vec4f(1.0, 1.0, 1.0, 1.0);
    ///         }
    ///     }
    /// );
    ///
    /// assert_eq!(2, builder.get_function("foo").unwrap().stmt.stmts.len());
    ///
    /// // Removes statements that have attribute #[ID(0)]
    /// builder.remove_function_statement("foo", "ID", Some("0"));
    /// assert_eq!(1, builder.get_function("foo").unwrap().stmt.stmts.len());
    ///
    /// // Removes statements that have attribute #[ID(anything)]
    /// builder.remove_function_statement("foo", "ID", None);
    /// assert_eq!(0, builder.get_function("foo").unwrap().stmt.stmts.len());
    /// ```
    pub fn remove_function_statement(
        &mut self,
        fn_ident: &str,
        attr_outer: &str,
        attr_inner: Option<&str>,
    ) -> SmallVec<[CompoundStatement; 4]> {
        if let Some(f) = self.get_function_mut(fn_ident) {
            f.stmt
                .remove_statement_recur_partial(attr_outer, attr_inner)
        } else {
            smallvec::smallvec![]
        }
    }

    /// Makes the specified statements bare statements, which don't output curly braces.
    /// It finds all matching statements recursively even if the inside of matched statement.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_wgsl::*;
    ///
    /// let mut builder = Builder::new();
    /// wgsl_fn!(builder,
    ///     fn foo() {
    ///         var color = #[ID(0)] { 0.0; } #[ID(1)] { 1.0; }
    ///     }
    /// );
    ///
    /// // Removes ID(0) statement and unwrap ID(1) statement.
    /// builder.remove_function_statement("foo", "ID", Some("0"));
    /// builder.into_bare_function_statement("foo", "ID", Some("1"));
    /// assert_eq!(
    ///     "fn foo(){var color=1.0;}",
    ///     &builder.build()
    /// );
    /// ```
    pub fn into_bare_function_statement(
        &mut self,
        fn_ident: &str,
        attr_outer: &str,
        attr_inner: Option<&str>,
    ) {
        if let Some(f) = self.get_function_mut(fn_ident) {
            f.stmt.into_bare_statements_recur(attr_outer, attr_inner);
        }
    }

    /// finds out the first function that has vertex attribute.
    /// if found something, returns its ident, otherwise returns None.
    pub fn get_vertex_stage_ident(&self) -> Option<&'static str> {
        self.entries.iter().find_map(|entry| {
            if let ShaderEntry::Function(f) = entry {
                if f.attrs.has_attribute(&Attribute::Vertex) {
                    return Some(f.ident);
                }
            }
            None
        })
    }

    /// finds out the first function that has fragment attribute.
    /// if found something, returns its ident, otherwise returns None.
    pub fn get_fragment_stage_ident(&self) -> Option<&'static str> {
        self.entries.iter().find_map(|entry| {
            if let ShaderEntry::Function(f) = entry {
                if f.attrs.has_attribute(&Attribute::Fragment) {
                    return Some(f.ident);
                }
            }
            None
        })
    }
}

#[derive(Debug)]
pub enum ShaderEntry {
    Structure(Structure),
    GlobalVariable(GlobalVariable),
    Function(Function),
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

        paste! {
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

impl ShaderEntry {
    impl_shader_entry_matcher!(as_structure, Structure, Structure);
    impl_shader_entry_matcher!(as_global_variable, GlobalVariable, GlobalVariable);
    impl_shader_entry_matcher!(as_function, Function, Function);
}

impl PutStr for ShaderEntry {
    fn put_ident(&self, buf: &mut String) {
        match self {
            Self::Structure(structure) => structure.put_ident(buf),
            Self::GlobalVariable(global_variable) => global_variable.put_ident(buf),
            Self::Function(function) => function.put_ident(buf),
        }
    }

    fn put_str(&self, buf: &mut String) {
        match self {
            Self::Structure(structure) => structure.put_str(buf),
            Self::GlobalVariable(global_variable) => global_variable.put_str(buf),
            Self::Function(function) => function.put_str(buf),
        }
    }
}

impl PutStrPretty for ShaderEntry {
    fn put_str_pretty(&self, buf: &mut String) {
        match self {
            Self::Structure(structure) => structure.put_str_pretty(buf),
            Self::GlobalVariable(global_variable) => global_variable.put_str_pretty(buf),
            Self::Function(function) => function.put_str_pretty(buf),
        }
    }
}

// 6.2.10. Structure Types
// https://www.w3.org/TR/WGSL/#struct-types
#[derive(Debug)]
pub struct Structure {
    /// Name of the struct.
    ///
    /// e.g. struct **Light** { ... }
    pub ident: &'static str,
    /// Structure members.
    ///
    /// e.g. struct Light { **pos : vec3f**, **color : vec3f** }
    pub members: SmallVec<[StructureMember; 4]>,
}

impl Structure {
    /// Retrieves the index of the structure member that has the given name.
    pub fn find_member(&self, ident: &str) -> Option<usize> {
        find_index(self.members.iter(), ident, |member| Some(member.ident))
    }

    /// Tests if there's the specified member.
    pub fn has_member(&self, ident: &str) -> bool {
        self.find_member(ident).is_some()
    }

    /// Retrieves the `my_wgsl::StructureMember` that has the given name.
    pub fn get_member(&self, ident: &str) -> Option<&StructureMember> {
        // Safety: `i` is valid.
        self.find_member(ident)
            .map(|i| unsafe { self.members.get(i).unwrap_unchecked() })
    }

    /// Retrieves the `my_wgsl::StructureMember` that has the given name.
    pub fn get_member_mut(&mut self, ident: &str) -> Option<&mut StructureMember> {
        // Safety: `i` is valid.
        self.find_member(ident)
            .map(|i| unsafe { self.members.get_mut(i).unwrap_unchecked() })
    }

    /// Tries to remove the structure member that has the given name.
    /// If succeeded, returns removed one.
    pub fn remove_member(&mut self, ident: &str) -> Option<StructureMember> {
        self.find_member(ident).map(|i| self.members.remove(i))
    }

    /// Retains only the members in the given `idents`.
    pub fn retain_members<'a>(&mut self, mut idents: impl Iterator<Item = &'a str>) {
        for i in (0..self.members.len()).rev() {
            if idents.all(|retain| retain != self.members[i].ident) {
                self.members.remove(i);
            }
        }
    }

    /// Reorders members along the given `order`.
    /// Caller should guarantee that all idents in `order` appear only once
    /// and match with this members.
    pub fn reorder_members<'a>(&mut self, order: impl Iterator<Item = &'a str>) {
        let mut new_members = SmallVec::with_capacity(self.members.len());
        for ident in order {
            let i = self.find_member(ident).unwrap();
            new_members.push(self.members[i].clone());
        }
        self.members = new_members;
    }

    /// Merges with the given structure.
    /// Copies all members in the `rhs` and overwrite to this structure.
    /// Copied and overwritten members are located at the end of this structure members,
    /// and follow the order shown in `rhs`.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_wgsl::*;
    ///
    /// #[wgsl_decl_struct]
    /// struct A {
    ///     a: f32,
    ///     b: u32,
    ///     c: bool,
    /// }
    ///
    /// #[wgsl_decl_struct]
    /// struct B {
    ///     b: f32,
    ///     a: u32,
    ///     d: f16,
    /// }
    ///
    /// let mut st_a = A::as_structure();
    /// st_a.merge(&B::as_structure());
    /// assert_eq!(4, st_a.members.len());
    /// # assert_eq!("c", st_a.members[0].ident);
    /// # assert_eq!("bool", st_a.members[0].ty);
    /// # assert_eq!("b", st_a.members[1].ident);
    /// # assert_eq!("f32", st_a.members[1].ty);
    /// # assert_eq!("a", st_a.members[2].ident);
    /// # assert_eq!("u32", st_a.members[2].ty);
    /// # assert_eq!("d", st_a.members[3].ident);
    /// # assert_eq!("f16", st_a.members[3].ty);
    /// ```
    pub fn merge(&mut self, rhs: &Structure) {
        let num = self
            .members
            .iter()
            .filter(|m| !rhs.has_member(m.ident))
            .count()
            + rhs.members.len();
        let mut merged = SmallVec::with_capacity(num);
        for member in self.members.iter() {
            if !rhs.has_member(member.ident) {
                merged.push(member.clone());
            }
        }
        merged.extend(rhs.members.iter().cloned());
        self.members = merged;
    }
}

impl PutStr for Structure {
    fn put_ident(&self, buf: &mut String) {
        buf.push_str(self.ident);
    }

    fn put_str(&self, buf: &mut String) {
        buf.push_str("struct ");
        self.put_ident(buf);
        buf.push('{');
        put_str_join(self.members.iter(), buf, "", ",", "");
        buf.push('}');
    }
}

impl PutStrPretty for Structure {
    fn put_str_pretty(&self, buf: &mut String) {
        buf.push_str("struct ");
        self.put_ident(buf);
        buf.push_str(" {\n");
        let tab_str = " ".repeat(TAB_SIZE);
        put_str_join(self.members.iter(), buf, &tab_str, ",\n", "\n");
        buf.push('}');
    }
}

#[derive(Debug, Clone)]
pub struct StructureMember {
    /// Attributes of the structure member.
    ///
    /// e.g. struct VertexInput { **@location(0)** pos : vec3f }
    pub attrs: Attributes,
    /// Name of the structure member.
    ///
    /// e.g. struct VertexInput { **pos** : vec3f }
    pub ident: &'static str,
    /// Type of the structure member.
    ///
    /// e.g. struct VertexInput { pos : **vec3f** }
    pub ty: &'static str,
}

impl PutStr for StructureMember {
    fn put_ident(&self, buf: &mut String) {
        buf.push_str(self.ident);
    }

    fn put_str(&self, buf: &mut String) {
        put_attrs(self.attrs.0.iter(), buf);
        self.put_ident(buf);
        buf.push(':');
        buf.push_str(self.ty);
    }
}

#[derive(Debug)]
pub struct GlobalVariable {
    /// Attributes of the global variable.
    ///
    /// e.g. **group(0)** **binding(0)** var<storage> light : LightStorage.
    pub attrs: Attributes,
    /// Templates of the global variable.
    ///
    /// e.g. group(0) binding(0) var<**storage**> light : LightStorage.
    pub templates: SmallVec<[String; 2]>,
    /// Name of the global variable.
    ///
    /// e.g. group(0) binding(0) var<storage> **light** : LightStorage.
    pub ident: &'static str,
    /// Type of the global variable.
    ///
    /// e.g. group(0) binding(0) var<storage> light : **LightStorage**.
    pub ty: Option<String>,
    /// Expression of the global variable.
    pub expr: Option<String>,
}

impl GlobalVariable {
    /// Generates a new `my_wgsl::GlobalVariable`.
    /// Use `wgsl_global_var` macro for usual cases.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_wgsl::*;
    ///
    /// // Generates @group(0) @binding(0) var<storage> lights : LightStorage;
    /// let var = GlobalVariable::new(
    ///     [("group", 0).into(), ("binding", 0).into()].into_iter(),
    ///     std::iter::once("storage"),
    ///     "lights",
    ///     Some("LightStorage"),
    ///     None,
    /// );
    ///
    /// let mut builder = Builder::new();
    /// builder.push_global_variable(var);
    /// ```
    pub fn new<'a>(
        attrs: impl Iterator<Item = Attribute>,
        templates: impl Iterator<Item = &'a str>,
        ident: &'static str,
        ty: Option<&str>,
        expr: Option<&str>,
    ) -> Self {
        Self {
            attrs: Attributes(attrs.collect()),
            templates: templates.map(|v| v.to_owned()).collect(),
            ident,
            ty: ty.map(|v| v.to_owned()),
            expr: expr.map(|v| v.to_owned()),
        }
    }

    /// Retrives the index of the template that has the given name.
    pub fn find_template(&self, template: &str) -> Option<usize> {
        find_index(self.templates.iter(), template, |v| Some(v))
    }

    /// Appends the given template.
    pub fn push_template(&mut self, template: &str) {
        self.templates.push(template.to_owned())
    }

    /// Tries to remove the template that has the given name.
    pub fn remove_template(&mut self, template: &str) -> Option<String> {
        self.find_template(template)
            .map(|i| self.templates.remove(i))
    }
}

impl PutStr for GlobalVariable {
    fn put_ident(&self, buf: &mut String) {
        buf.push_str(self.ident)
    }

    fn put_str(&self, buf: &mut String) {
        put_attrs(self.attrs.0.iter(), buf);
        buf.push_str("var");
        if !self.templates.is_empty() {
            buf.push('<');
            put_str_join(self.templates.iter(), buf, "", ",", "");
            buf.push('>');
        } else {
            buf.push(' ');
        }
        self.put_ident(buf);
        if let Some(ty) = &self.ty {
            buf.push(':');
            buf.push_str(ty);
        }
        if let Some(expr) = &self.expr {
            buf.push('=');
            buf.push_str(expr);
        }
        buf.push(';');
    }
}

impl PutStrPretty for GlobalVariable {
    fn put_str_pretty(&self, buf: &mut String) {
        put_attrs_pretty(self.attrs.0.iter(), buf);
        buf.push_str("var");
        if !self.templates.is_empty() {
            buf.push('<');
            put_str_join(self.templates.iter(), buf, "", ", ", "");
            buf.push('>');
        }
        buf.push(' ');
        self.put_ident(buf);
        if let Some(ty) = &self.ty {
            buf.push_str(" : ");
            buf.push_str(ty);
        }
        if let Some(expr) = &self.expr {
            buf.push_str(" = ");
            buf.push_str(expr);
        }
        buf.push(';');
    }
}

#[derive(Debug)]
pub struct Function {
    /// Attributes of the function.
    ///
    /// e.g. **@fragment** fn fragmentMain(...) {...}
    pub attrs: Attributes,
    /// Name of the function.
    ///
    /// e.g. fn **foo**(...) {...}
    pub ident: &'static str,
    /// Inputs of the function.
    ///
    /// e.g. fn foo(**input : Input**) {...}
    pub inputs: SmallVec<[FunctionParameter; 4]>,
    /// Output of the function.
    ///
    /// e.g. fn foo(...) -> **Output** {...}
    pub output: Option<FunctionParameter>,
    /// Statements of the function.
    ///
    /// e.g. fn foo(...) { **statement; { statement } ...** }
    pub stmt: CompoundStatement,
}

impl PutStr for Function {
    fn put_ident(&self, buf: &mut String) {
        buf.push_str(self.ident);
    }

    fn put_str(&self, buf: &mut String) {
        put_attrs(self.attrs.0.iter(), buf);
        buf.push_str("fn ");
        self.put_ident(buf);
        buf.push('(');
        put_str_join(self.inputs.iter(), buf, "", ",", "");
        buf.push(')');
        if let Some(output) = &self.output {
            buf.push_str("->");
            output.put_str(buf);
        }
        self.stmt.put_str(buf);
    }
}

impl PutStrPretty for Function {
    fn put_str_pretty(&self, buf: &mut String) {
        // Writes attributes.
        put_attrs_pretty(self.attrs.0.iter(), buf);
        buf.push('\n');

        // Writes from `fn` to the first input.
        let prev = buf.len();
        buf.push_str("fn ");
        self.put_ident(buf);
        buf.push('(');
        let input_tab = " ".repeat(buf.len() - prev);
        if let Some(first_input) = self.inputs.first() {
            first_input.put_str_pretty(buf);
            if self.inputs.len() > 1 {
                buf.push_str(",\n");
            }
        }

        // Writes other inputs.
        put_str_pretty_join(self.inputs.iter().skip(1), buf, &input_tab, ",\n", "");
        buf.push(')');

        // Writes output.
        if let Some(output) = &self.output {
            buf.push_str(" -> ");
            output.put_str_pretty(buf);
        }

        // Writes statement.
        buf.push('\n');
        pushn(buf, ' ', TAB_SIZE);
        self.stmt.put_str_pretty(buf);
    }
}

#[derive(Debug)]
pub struct FunctionParameter {
    /// Attributes of the function parameter.
    ///
    /// e.g. fn @fragement fragementMain(**@location(0)** pos : vec3f, ...) -> **@location(0)** vec4f {...}
    pub attrs: Attributes,
    /// Name of the function parameter.
    ///
    /// e.g. fn foo(**name** : vec3f) {...}
    pub ident: Option<&'static str>,
    /// type of the function parameter.
    ///
    /// e.g. fn foo(name : **vec3f**) -> **vec4f** {...}
    pub ty: &'static str,
}

impl PutStr for FunctionParameter {
    fn put_ident(&self, buf: &mut String) {
        if let Some(ident) = &self.ident {
            buf.push_str(ident)
        }
    }

    fn put_str(&self, buf: &mut String) {
        put_attrs(self.attrs.0.iter(), buf);
        if let Some(ident) = &self.ident {
            buf.push_str(ident);
            buf.push(':');
        }
        buf.push_str(self.ty);
    }
}

impl PutStrPretty for FunctionParameter {
    fn put_str_pretty(&self, buf: &mut String) {
        put_attrs_pretty(self.attrs.0.iter(), buf);
        if let Some(ident) = &self.ident {
            buf.push_str(ident);
            buf.push_str(" : ");
        }
        buf.push_str(self.ty);
    }
}

// 9.1. Compound Statement
// https://www.w3.org/TR/WGSL/#compound-statement-section
/// Compound statement is a set of statement wrapped with braces, {...}.
#[derive(Debug, Clone)]
pub struct CompoundStatement {
    /// Attributes of the statement.
    pub attrs: Attributes,
    /// Statements inside this block.
    pub stmts: Vec<Statement>,
}

impl CompoundStatement {
    /// Retrieve statements that have given attribute using recursive search.
    pub fn get_statement_recur(&self, attr: &Attribute) -> SmallVec<[&CompoundStatement; 4]> {
        self.get_statement_recur_partial(attr.outer(), attr.inner().as_deref())
    }

    /// Searches partially matched attribute.
    /// If `inner` is Some, it tries to find exactly matched one.
    /// Otherwise, it compares outer only.
    pub fn get_statement_recur_partial(
        &self,
        outer: &str,
        inner: Option<&str>,
    ) -> SmallVec<[&CompoundStatement; 4]> {
        let mut res = SmallVec::new();
        for stmt in self.stmts.iter() {
            if let Statement::Compound(comp_stmt) = stmt {
                if comp_stmt.attrs.has_attribute_partial(outer, inner) {
                    // Captures only the most outer one.
                    res.push(comp_stmt);
                } else {
                    res.extend(comp_stmt.get_statement_recur_partial(outer, inner));
                }
            }
        }
        res
    }

    /// Retrieve statements that have given attribute using recursive search.
    pub fn get_statement_recur_mut(
        &mut self,
        attr: &Attribute,
    ) -> SmallVec<[&mut CompoundStatement; 4]> {
        self.get_statement_recur_partial_mut(attr.outer(), attr.inner().as_deref())
    }

    /// Searches partially matched attribute.
    /// If `inner` is Some, it tries to find exactly matched one.
    /// Otherwise, it compares outer only.
    pub fn get_statement_recur_partial_mut(
        &mut self,
        outer: &str,
        inner: Option<&str>,
    ) -> SmallVec<[&mut CompoundStatement; 4]> {
        let mut res = SmallVec::new();
        for stmt in self.stmts.iter_mut() {
            if let Statement::Compound(comp_stmt) = stmt {
                if comp_stmt.attrs.has_attribute_partial(outer, inner) {
                    // Captures only the most outer one.
                    res.push(comp_stmt);
                } else {
                    res.extend(comp_stmt.get_statement_recur_partial_mut(outer, inner));
                }
            }
        }
        res
    }

    /// Removes statements that have given attribute using recursive search.
    /// Then returns removed statements.
    pub fn remove_statement_recur(&mut self, attr: &Attribute) -> SmallVec<[CompoundStatement; 4]> {
        self.remove_statement_recur_partial(attr.outer(), attr.inner().as_deref())
    }

    /// Searches partially matched attribute.
    /// If `inner` is Some, it tries to find exactly matched one.
    /// Otherwise, it compares outer only.
    pub fn remove_statement_recur_partial(
        &mut self,
        outer: &str,
        inner: Option<&str>,
    ) -> SmallVec<[CompoundStatement; 4]> {
        let mut removed = SmallVec::new();
        for i in (0..self.stmts.len()).rev() {
            if let Statement::Compound(comp_stmt) = &mut self.stmts[i] {
                if comp_stmt.attrs.has_attribute_partial(outer, inner) {
                    if let Statement::Compound(removed_stat) = self.stmts.remove(i) {
                        removed.push(removed_stat);
                    }
                } else {
                    removed.extend(comp_stmt.remove_statement_recur_partial(outer, inner));
                }
            }
        }
        removed
    }

    /// Makes the specified statements bare statements.
    /// It finds all matching statements recursively even if the inside of matched statement.
    pub fn into_bare_statements_recur(&mut self, outer: &str, inner: Option<&str>) {
        for stmt in self.stmts.iter_mut() {
            match stmt {
                Statement::Compound(comp_stmt) | Statement::BareCompound(comp_stmt) => {
                    if comp_stmt.attrs.has_attribute_partial(outer, inner) {
                        comp_stmt.into_bare_statements_recur(outer, inner);
                        let mut bare = Statement::BareCompound(comp_stmt.clone());
                        std::mem::swap(stmt, &mut bare);
                    } else {
                        comp_stmt.into_bare_statements_recur(outer, inner);
                    }
                }
                _ => (),
            }
        }
    }

    /// Writes only inner statements without attributes and braces.
    pub fn put_str_inner(&self, buf: &mut String) {
        put_str_join(self.stmts.iter(), buf, "", "", "");
    }

    /// Writes only inner statements without attributes and braces in pretty style.
    pub fn put_str_pretty_inner(&self, buf: &mut String) {
        let non_ws = get_last_whitespaces(buf);
        match buf.chars().nth_back(non_ws) {
            Some('=') | Some(',') => {
                popn(buf, non_ws);
                let indent = get_last_indent(buf);
                let indent = " ".repeat(indent);
                put_str_pretty_join(self.stmts.iter().take(1), buf, " ", "", "\n");
                put_str_pretty_join(self.stmts.iter().skip(1), buf, &indent, "", "");
            }
            _ => {
                let indent = get_last_indent(buf);
                let indent = " ".repeat(indent);
                put_str_pretty_join(self.stmts.iter().take(1), buf, "", "", "\n");
                put_str_pretty_join(self.stmts.iter().skip(1), buf, &indent, "", "");
            }
        }
    }
}

impl PutStr for CompoundStatement {
    fn put_ident(&self, _buf: &mut String) {}

    fn put_str(&self, buf: &mut String) {
        put_str_join(self.attrs.0.iter(), buf, "", "", "");
        buf.push('{');
        put_str_join(self.stmts.iter(), buf, "", "", "");
        buf.push('}');
    }
}

impl PutStrPretty for CompoundStatement {
    fn put_str_pretty(&self, buf: &mut String) {
        // Note that caller must have put a tab for this compound.
        let tab = get_last_spaces(buf);
        popn(buf, tab.min(TAB_SIZE));

        put_attrs_pretty(self.attrs.0.iter().filter(|attr| !attr.is_my_attr()), buf);
        buf.push_str("{\n");
        let tab_str = " ".repeat(tab);
        put_str_pretty_join(self.stmts.iter(), buf, &tab_str, "\n", "\n");
        pushn(buf, ' ', tab.saturating_sub(TAB_SIZE));
        buf.push('}');
    }
}

// 9.7. Statements Grammar Summary
// https://www.w3.org/TR/WGSL/#statements-summary
#[derive(Debug, Clone)]
pub enum Statement {
    // Not fully implemented.
    /// Compoud statement is a set of statements wrapped with braces.
    Compound(CompoundStatement),
    /// Compound statement, but doesn't output attributes and braces.
    /// It's useful when you assign conditional compound statement to a variable.
    BareCompound(CompoundStatement),
    /// Currently, all statements without braces belong to this.
    Other(Vec<String>),
}

impl PutStr for Statement {
    fn put_ident(&self, _buf: &mut String) {}

    fn put_str(&self, buf: &mut String) {
        match self {
            Self::Compound(comp_stmt) => comp_stmt.put_str(buf),
            Self::BareCompound(comp_stmt) => comp_stmt.put_str_inner(buf),
            Self::Other(others) => {
                let mut prev = &String::from(".");
                for cur in others.iter() {
                    if cur.is_empty() {
                        continue;
                    }

                    let mut stick = false;
                    match unsafe { prev.as_str().chars().next().unwrap_unchecked() } {
                        '.' | ';' | '[' | '(' | '=' | '+' | '-' | '*' | '/' | ',' => {
                            stick = true;
                        }
                        _ => (),
                    }
                    match unsafe { cur.as_str().chars().next().unwrap_unchecked() } {
                        '.' | ';' | '[' | '(' | '=' | '+' | '-' | '*' | '/' | ',' => {
                            stick = true;
                        }
                        _ => {}
                    }
                    if !stick {
                        buf.push(' ');
                    }

                    buf.push_str(cur);
                    prev = cur;
                }
            }
        }
    }
}

impl PutStrPretty for Statement {
    fn put_str_pretty(&self, buf: &mut String) {
        match self {
            Self::Compound(comp_stmt) => {
                pushn(buf, ' ', TAB_SIZE);
                comp_stmt.put_str_pretty(buf);
            }
            Self::BareCompound(comp_stmt) => comp_stmt.put_str_pretty_inner(buf),
            Self::Other(others) => {
                let mut prev = &String::from(".");
                for cur in others.iter() {
                    if cur.is_empty() {
                        continue;
                    }

                    let mut stick = false;
                    let (p_first, c_first) = unsafe {
                        (
                            prev.as_str().chars().next().unwrap_unchecked(),
                            cur.as_str().chars().next().unwrap_unchecked(),
                        )
                    };
                    match p_first {
                        '.' | ';' | '[' | '(' | ',' => {
                            stick = true;
                        }
                        _ => (),
                    }
                    match c_first {
                        '.' | ';' | '[' | '(' | ',' => {
                            stick = true;
                        }
                        _ => {}
                    }
                    if c_first == '=' && ['+', '-', '*', '/', '='].iter().any(|ch| *ch == p_first) {
                        stick = true;
                    }
                    if c_first == '(' && ['+', '-', '*', '/'].iter().any(|ch| *ch == p_first) {
                        stick = false;
                    }
                    if !stick {
                        buf.push(' ');
                    }

                    buf.push_str(cur);
                    prev = cur;
                }
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Attributes(pub SmallVec<[Attribute; 2]>);

impl Attributes {
    pub fn new() -> Self {
        Self::default()
    }

    /// Retrieves the index of the exactly matched attribute.
    pub fn find_attribute(&self, attr: &Attribute) -> Option<usize> {
        find_index(self.0.iter(), attr, Some)
    }

    /// Searches partially matched attribute.
    /// If `inner` is Some, it tries to find exactly matched one.
    /// Otherwise, it compares outer only.
    pub fn find_attribute_partial(&self, outer: &str, inner: Option<&str>) -> Option<usize> {
        if let Some(inner) = inner {
            self.find_attribute(&Attribute::from((outer, inner)))
        } else {
            self.0
                .iter()
                .enumerate()
                .find_map(|(i, attr)| attr.is_same_outer2(outer).then_some(i))
        }
    }

    /// Tests if there's an attribute that exactly matches with the given attribute.
    pub fn has_attribute(&self, attr: &Attribute) -> bool {
        self.find_attribute(attr).is_some()
    }

    /// Searches partially matched attribute.
    /// If `inner` is Some, it tries to find exactly matched one.
    /// Otherwise, it compares outer only.
    pub fn has_attribute_partial(&self, outer: &str, inner: Option<&str>) -> bool {
        self.find_attribute_partial(outer, inner).is_some()
    }

    /// Tries to remove the attribute that matches with the given attribute.
    pub fn remove_attribute(&mut self, attr: &Attribute) -> Option<Attribute> {
        self.find_attribute(attr).map(|i| self.0.remove(i))
    }
}

// 11. Attributes
// https://www.w3.org/TR/WGSL/#attributes
#[derive(Debug, PartialEq, Clone)]
pub enum Attribute {
    Align(u32),
    Binding(u32),
    Builtin(BuiltinValue),
    Const,
    // Diagnostic,
    Group(u32),
    Id(u32),
    // Interpolate,
    Invariant,
    Location(u32),
    MustUse,
    Size(u32),
    // WorkgroupSize,
    Vertex,
    Fragment,
    Compute,
    MyId(String),
}

impl Attribute {
    /// Tests if the given attribute has the same variant.
    pub fn is_same_outer(&self, rhs: &Attribute) -> bool {
        use std::mem::discriminant;
        discriminant(self) == discriminant(rhs)
    }

    /// Tests if this instance is a variant equivalnt to the given string.
    pub fn is_same_outer2(&self, rhs: &str) -> bool {
        self.is_same_outer(&Attribute::from(rhs))
    }

    /// Tests if this instance is custom attribute like MyId.
    pub fn is_my_attr(&self) -> bool {
        matches!(self, Self::MyId(..))
    }

    /// Returns a string corresponding to this variant.
    pub fn outer(&self) -> &str {
        match self {
            Self::Align(..) => "align",
            Self::Binding(..) => "binding",
            Self::Builtin(..) => "builtin",
            Self::Const => "const",
            // Self::Diagnostic => unimplemented!()
            Self::Group(..) => "group",
            Self::Id(..) => "id",
            // Self::Interpolate => unimplemented!()
            Self::Invariant => "invariant",
            Self::Location(..) => "location",
            Self::MustUse => "must_use",
            Self::Size(..) => "size",
            // Self::WorkgroupSize => unimplemented!()
            Self::Vertex => "vertex",
            Self::Fragment => "fragment",
            Self::Compute => "compute",
            Self::MyId(..) => "ID",
        }
    }

    /// Returns a string converted from inner value.
    pub fn inner(&self) -> Option<String> {
        match self {
            Self::Align(v) => Some(v.to_string()),
            Self::Binding(v) => Some(v.to_string()),
            Self::Builtin(v) => Some(v.to_str()),
            Self::Const => None,
            // Self::Diagnostic => unimplemented!()
            Self::Group(v) => Some(v.to_string()),
            Self::Id(v) => Some(v.to_string()),
            // Self::Interpolate => unimplemented!()
            Self::Invariant => None,
            Self::Location(v) => Some(v.to_string()),
            Self::MustUse => None,
            Self::Size(v) => Some(v.to_string()),
            // Self::WorkgroupSize => unimplemented!()
            Self::Vertex => None,
            Self::Fragment => None,
            Self::Compute => None,
            Self::MyId(v) => Some(v.clone()),
        }
    }

    /// Sets inner value.
    pub fn set_inner(&mut self, inner: &str) {
        match self {
            Self::Align(v) => *v = inner.parse().unwrap(),
            Self::Binding(v) => *v = inner.parse().unwrap(),
            Self::Builtin(v) => *v = BuiltinValue::from(inner),
            Self::Const => (),
            // Self::Diagnostic => unimplemented!()
            Self::Group(v) => *v = inner.parse().unwrap(),
            Self::Id(v) => *v = inner.parse().unwrap(),
            // Self::Interpolate => unimplemented!()
            Self::Invariant => (),
            Self::Location(v) => *v = inner.parse().unwrap(),
            Self::MustUse => (),
            Self::Size(v) => *v = inner.parse().unwrap(),
            // Self::WorkgroupSize => unimplemented!()
            Self::Vertex => (),
            Self::Fragment => (),
            Self::Compute => (),
            Self::MyId(v) => *v = String::from(inner),
        }
    }
}

impl PutStr for Attribute {
    fn put_ident(&self, buf: &mut String) {
        if !self.is_my_attr() {
            buf.push_str(self.outer());
        }
    }

    fn put_str(&self, buf: &mut String) {
        match self {
            Self::Align(v)
            | Self::Binding(v)
            | Self::Group(v)
            | Self::Id(v)
            | Self::Location(v)
            | Self::Size(v) => {
                buf.push('@');
                self.put_ident(buf);
                buf.push('(');
                buf.push_str(&v.to_string());
                buf.push(')');
            }
            Self::Builtin(v) => {
                buf.push('@');
                self.put_ident(buf);
                buf.push('(');
                v.put_str(buf);
                buf.push(')');
            }
            Self::Const
            | Self::Invariant
            | Self::MustUse
            | Self::Vertex
            | Self::Fragment
            | Self::Compute => {
                buf.push('@');
                self.put_ident(buf);
            }
            Self::MyId(..) => {} // Self::Diagnostic => unimplemented!()
                                 // Self::Interpolate => unimplemented!()
                                 // Self::WorkgroupSize => unimplemented!()
        };
    }
}

impl From<(&str, &str)> for Attribute {
    fn from(value: (&str, &str)) -> Self {
        match value {
            ("align", v) => Self::Align(v.parse().unwrap()),
            ("binding", v) => Self::Binding(v.parse().unwrap()),
            ("builtin", v) => Self::Builtin(BuiltinValue::from(v)),
            ("Const", _) => Self::Const, // Conflict with 'const'
            // ("diagnostic", _) => unimplemented!()
            ("group", v) => Self::Group(v.parse().unwrap()),
            ("id", v) => Self::Id(v.parse().unwrap()),
            // ("interpolate", _) => unimplemented!()
            ("invariant", _) => Self::Invariant,
            ("location", v) => Self::Location(v.parse().unwrap()),
            ("must_use", _) => Self::MustUse,
            ("size", v) => Self::Size(v.parse().unwrap()),
            // ("workgroup_size", _) => unimplemented!()
            ("vertex", _) => Self::Vertex,
            ("fragment", _) => Self::Fragment,
            ("compute", _) => Self::Compute,
            ("ID", v) => Self::MyId(v.to_owned()),
            _ => panic!("{:?} can't be Attribute", value),
        }
    }
}

impl From<(&str, u32)> for Attribute {
    fn from(value: (&str, u32)) -> Self {
        match value {
            ("align", v) => Self::Align(v),
            ("binding", v) => Self::Binding(v),
            ("builtin", v) => Self::Builtin(BuiltinValue::from(v.to_string().as_str())),
            ("Const", _) => Self::Const,
            // ("diagnostic", _) => unimplemented!()
            ("group", v) => Self::Group(v),
            ("id", v) => Self::Id(v),
            // ("interpolate", _) => unimplemented!()
            ("invariant", _) => Self::Invariant,
            ("location", v) => Self::Location(v),
            ("must_use", _) => Self::MustUse,
            ("size", v) => Self::Size(v),
            // ("workgroup_size", _) => unimplemented!()
            ("vertex", _) => Self::Vertex,
            ("fragment", _) => Self::Fragment,
            ("compute", _) => Self::Compute,
            ("ID", v) => Self::MyId(v.to_string()),
            _ => panic!("{:?} can't be Attribute", value),
        }
    }
}

impl From<&str> for Attribute {
    fn from(value: &str) -> Self {
        match value {
            "align" => Self::Align(Default::default()),
            "binding" => Self::Binding(Default::default()),
            "builtin" => Self::Builtin(Default::default()),
            "Const" => Self::Const,
            //("diagnostic" => unimplemented!()
            "group" => Self::Group(Default::default()),
            "id" => Self::Id(Default::default()),
            // "interpolate" => unimplemented!()
            "invariant" => Self::Invariant,
            "location" => Self::Location(Default::default()),
            "must_use" => Self::MustUse,
            "size" => Self::Size(Default::default()),
            // "workgroup_size" => unimplemented!()
            "vertex" => Self::Vertex,
            "fragment" => Self::Fragment,
            "compute" => Self::Compute,
            "ID" => Self::MyId(Default::default()),
            _ => panic!("{value} can't be Attribute"),
        }
    }
}

// 12.3.1.1. Built-in Inputs and Outputs
// https://www.w3.org/TR/WGSL/#builtin-inputs-outputs
#[derive(Debug, PartialEq, Default, Clone)]
pub enum BuiltinValue {
    #[default]
    VertexIndex, // Vertex input
    InstanceIndex,        // Vertex input
    Position,             // Vertex output & Fragment input
    FrontFacing,          // Fragment input
    FragDepth,            // Fragment output
    SampleIndex,          // Fragment input
    SampleMask,           // Fragment input & output
    LocalInvocationId,    // Compute input
    LocalInvocationIndex, // Compute input
    GlobalInvocationId,   // Compute input
    WorkgroupId,          // Compute input
    NumWorkgroups,        // Compute input
}

impl PutStr for BuiltinValue {
    fn put_ident(&self, buf: &mut String) {
        match self {
            Self::VertexIndex => buf.push_str("vertex_index"),
            Self::InstanceIndex => buf.push_str("instance_index"),
            Self::Position => buf.push_str("position"),
            Self::FrontFacing => buf.push_str("front_facing"),
            Self::FragDepth => buf.push_str("frag_depth"),
            Self::SampleIndex => buf.push_str("sample_index"),
            Self::SampleMask => buf.push_str("sample_mask"),
            Self::LocalInvocationId => buf.push_str("local_invocation_id"),
            Self::LocalInvocationIndex => buf.push_str("local_invocation_index"),
            Self::GlobalInvocationId => buf.push_str("global_invocation_id"),
            Self::WorkgroupId => buf.push_str("workgroup_id"),
            Self::NumWorkgroups => buf.push_str("num_workgroups"),
        }
    }

    fn put_str(&self, buf: &mut String) {
        self.put_ident(buf)
    }
}

impl From<&str> for BuiltinValue {
    fn from(value: &str) -> Self {
        match value {
            "vertex_index" => Self::VertexIndex,
            "instance_index" => Self::InstanceIndex,
            "position" => Self::Position,
            "front_facing" => Self::FrontFacing,
            "frag_depth" => Self::FragDepth,
            "sample_index" => Self::SampleIndex,
            "sample_mask" => Self::SampleMask,
            "local_invocation_id" => Self::LocalInvocationId,
            "local_invocation_index" => Self::LocalInvocationIndex,
            "global_invocation_id" => Self::GlobalInvocationId,
            "workgroup_id" => Self::WorkgroupId,
            "num_workgroups" => Self::NumWorkgroups,
            _ => panic!("{value} can't be BuiltinValue"),
        }
    }
}

macro_rules! impl_str {
    ($id:ident) => {
        impl ToIdent for $id {
            fn maybe_ident() -> Option<&'static str> {
                Some(stringify!($id))
            }

            fn to_ident() -> String {
                // Safety: Infallible.
                unsafe { Self::maybe_ident().unwrap_unchecked().to_owned() }
            }
        }

        impl PutStr for $id {
            fn put_ident(&self, buf: &mut String) {
                // Safety: Infallible.
                unsafe { buf.push_str(Self::maybe_ident().unwrap_unchecked()) }
            }

            fn put_str(&self, buf: &mut String) {
                // Safety: Infallible.
                unsafe { buf.push_str(Self::maybe_ident().unwrap_unchecked()) }
            }
        }
    };
    ($id:ident<$t:ident>) => {
        impl<$t: ToIdent> ToIdent for $id<$t> {
            fn maybe_ident() -> Option<&'static str> {
                ($t::maybe_ident() == Some(stringify!($t))).then_some(concat!(
                    stringify!($id),
                    '<',
                    stringify!($t),
                    '>'
                ))
            }

            fn to_ident() -> String {
                let mut res = stringify!($id).to_owned();
                res.push('<');
                res.push_str(&$t::to_ident());
                res.push('>');
                res
            }
        }

        impl<$t: ToIdent> PutStr for $id<$t> {
            fn put_ident(&self, buf: &mut String) {
                buf.push_str(&Self::to_ident());
            }

            fn put_str(&self, buf: &mut String) {
                buf.push_str(&Self::to_ident());
            }
        }
    };
    ($id:ident<$t:ident, N>) => {
        impl<$t: ToIdent, const N: usize> ToIdent for $id<$t, N> {
            // We can't implement maybe_ident()?

            fn to_ident() -> String {
                let mut res = stringify!($id).to_owned();
                res.push('<');
                res.push_str(&$t::to_ident());
                if (N > 0) {
                    res.push(',');
                    res.push_str(&N.to_string());
                }
                res.push('>');
                res
            }
        }

        impl<$t: ToIdent, const N: usize> PutStr for $id<$t, N> {
            fn put_ident(&self, buf: &mut String) {
                buf.push_str(&Self::to_ident());
            }

            fn put_str(&self, buf: &mut String) {
                buf.push_str(&Self::to_ident());
            }
        }
    };
}

// 6.2.2. Boolean Type
// https://www.w3.org/TR/WGSL/#bool-type
impl_str!(bool);

// 6.2.3. Integer Types
// https://www.w3.org/TR/WGSL/#integer-types
impl_str!(i32);
impl_str!(u32);

// 6.2.4. Floating Point Types
// https://www.w3.org/TR/WGSL/#floating-point-types
impl_str!(f32);
pub struct f16;
impl_str!(f16);

// 6.2.6. Vector Types
// https://www.w3.org/TR/WGSL/#vector-types
pub struct vec2<T>(PhantomData<T>);
impl_str!(vec2<T>);
pub struct vec3<T>(PhantomData<T>);
impl_str!(vec3<T>);
pub struct vec4<T>(PhantomData<T>);
impl_str!(vec4<T>);
// Predeclared vecN<T> aliases
pub type vec2i = vec2<i32>;
pub type vec3i = vec3<i32>;
pub type vec4i = vec4<i32>;
pub type vec2u = vec2<u32>;
pub type vec3u = vec3<u32>;
pub type vec4u = vec4<u32>;
pub type vec2f = vec2<f32>;
pub type vec3f = vec3<f32>;
pub type vec4f = vec4<f32>;
pub type vec2h = vec2<f16>;
pub type vec3h = vec3<f16>;
pub type vec4h = vec4<f16>;

// 6.2.7. Matrix Types
// https://www.w3.org/TR/WGSL/#matrix-types
pub struct mat2x2<T>(PhantomData<T>);
impl_str!(mat2x2<T>);
pub struct mat2x3<T>(PhantomData<T>);
impl_str!(mat2x3<T>);
pub struct mat2x4<T>(PhantomData<T>);
impl_str!(mat2x4<T>);
pub struct mat3x2<T>(PhantomData<T>);
impl_str!(mat3x2<T>);
pub struct mat3x3<T>(PhantomData<T>);
impl_str!(mat3x3<T>);
pub struct mat3x4<T>(PhantomData<T>);
impl_str!(mat3x4<T>);
pub struct mat4x2<T>(PhantomData<T>);
impl_str!(mat4x2<T>);
pub struct mat4x3<T>(PhantomData<T>);
impl_str!(mat4x3<T>);
pub struct mat4x4<T>(PhantomData<T>);
impl_str!(mat4x4<T>);
// Predeclared matCxR<T> aliases
pub type mat2x2f = mat2x2<f32>;
pub type mat2x3f = mat2x3<f32>;
pub type mat2x4f = mat2x4<f32>;
pub type mat3x2f = mat3x2<f32>;
pub type mat3x3f = mat3x3<f32>;
pub type mat3x4f = mat3x4<f32>;
pub type mat4x2f = mat4x2<f32>;
pub type mat4x3f = mat4x3<f32>;
pub type mat4x4f = mat4x4<f32>;
pub type mat2x2h = mat2x2<f16>;
pub type mat2x3h = mat2x3<f16>;
pub type mat2x4h = mat2x4<f16>;
pub type mat3x2h = mat3x2<f16>;
pub type mat3x3h = mat3x3<f16>;
pub type mat3x4h = mat3x4<f16>;
pub type mat4x2h = mat4x2<f16>;
pub type mat4x3h = mat4x3<f16>;
pub type mat4x4h = mat4x4<f16>;

// 6.5.2. Sampled Texture Types
// https://www.w3.org/TR/WGSL/#sampled-texture-type
pub struct texture_1d<T>(PhantomData<T>);
impl_str!(texture_1d<T>);
pub struct texture_2d<T>(PhantomData<T>);
impl_str!(texture_2d<T>);
pub struct texture_2d_array<T>(PhantomData<T>);
impl_str!(texture_2d_array<T>);
pub struct texture_3d<T>(PhantomData<T>);
impl_str!(texture_3d<T>);
pub struct texture_cube<T>(PhantomData<T>);
impl_str!(texture_cube<T>);
pub struct texture_cube_array<T>(PhantomData<T>);
impl_str!(texture_cube_array<T>);

// 6.5.7. Sampler Type
// https://www.w3.org/TR/WGSL/#sampler-type
pub struct sampler;
impl_str!(sampler);
pub struct sampler_comparison;
impl_str!(sampler_comparison);

/// Helps you to push structs into builder.
///
/// # Examples
///
/// ```
/// use my_wgsl::*;
///
/// #[wgsl_decl_struct]
/// struct PointLight {
///     position: vec3f,
///     color: vec3f,
/// }
///
/// #[wgsl_decl_struct]
/// struct LightStorage {
///     pointCount: u32,
///     point: array<PointLight>,
/// }
///
/// let mut builder = Builder::new();
/// wgsl_structs![builder, PointLight, LightStorage];
/// ```
#[macro_export]
macro_rules! wgsl_structs {
    ($builder:ident, $($st:ty),+) => {
        $( $builder.push_structure(<$st as AsStructure>::as_structure()); )+
    };
}

/// Helps you to create a global variable.
///
/// # Examples
///
/// ```
/// use my_wgsl::*;
/// # #[wgsl_decl_struct]
/// # struct LightStorage;
///
/// let var_a = wgsl_global_var!(0, 0, <storage>, lights, LightStorage);
/// let var_b = wgsl_global_var!(1, 0, <>, baseColorSampler, sampler);
/// let var_c = wgsl_global_var!(1, 1, <>, baseColorTexture, texture_2d<f32>);
///
/// let mut builder = Builder::new();
/// builder.push_global_variable(var_a);
/// builder.push_global_variable(var_b);
/// builder.push_global_variable(var_c);
/// ```
#[macro_export]
macro_rules! wgsl_global_var {
    ($gi:expr, $bi:expr, $id:ident) => {
        my_wgsl::GlobalVariable::new(
            [("group", $gi).into(), ("binding", $bi).into()].into_iter(),
            std::iter::empty(),
            stringify!($id),
            None,
            None,
        )
    };
    ($gi:expr, $bi:expr, <$($temp:ident),*>, $id:ident) => {
        {
            #[allow(unused_mut)]
            let mut var = my_wgsl::wgsl_global_var!($gi, $bi, $id);
            $(
                var.push_template(stringify!($temp));
            )*
            var
        }
    };
    ($gi:expr, $bi:expr, $(<$($temp:ident),*>,)? $id:ident, $ty:ty) => {
        {
            let mut var = my_wgsl::wgsl_global_var!($gi, $bi, <$($($temp),*)?>, $id);
            var.ty = Some(<$ty as ToIdent>::to_ident());
            var
        }
    };
    ($gi:expr, $bi:expr, $(<$($temp:ident),*>,)? $id:ident, $ty:ty, $expr:tt) => {
        {
            let mut var = my_wgsl::wgsl_global_var!($gi, $bi, <$($($temp),*)?>, $id, $ty);
            var.expr = Some(stringify!($expr).to_owned());
            var
        }
    };
}

/// Helps you to push a global variable into builder.
///
/// # Examples
///
/// ```
/// use my_wgsl::*;
/// # #[wgsl_decl_struct]
/// # struct LightStorage;
///
/// let mut builder = Builder::new();
/// wgsl_bind!(builder, group(0) binding(0) var<storage> lights : LightStorage);
/// wgsl_bind!(builder, group(1) binding(0) var baseColorSampler : sampler);
/// wgsl_bind!(builder, group(1) binding(1) var baseColorTexture : texture_2d<f32>);
/// ```
#[macro_export]
macro_rules! wgsl_bind {
    ($builder:ident, group($gi:expr) binding($bi:expr) var$(<$($temp:ident),*>)? $id:ident $(: $ty:ty)? $(= $expr:tt)?) => {
        $builder.push_global_variable(
            my_wgsl::wgsl_global_var!($gi, $bi, <$($($temp),*)?>, $id $(, $ty)? $(, $expr)?)
        )
    };
}

/// Helps you to push a function into builder.
///
/// # Examples
///
/// ```
/// use my_wgsl::*;
///
/// let mut builder = Builder::new();
/// wgsl_fn!(builder,
///     #[fragment]
///     fn fragementMain() -> #[location(0)] vec4f {
///         return vec4f(0.0, 0.0, 0.0, 1.0);
///     }
/// );
/// ```
#[macro_export]
macro_rules! wgsl_fn {
    ($builder:ident, $($tt:tt)*) => {
        {
            let f = my_wgsl::wgsl_decl_fn!($($tt)*);
            $builder.push_function(f);
        }
    };
}

// 6.2.9. Array Types
// https://www.w3.org/TR/WGSL/#array-types
pub struct array<E, const N: usize = 0>(PhantomData<E>);
impl_str!(array<E, N>);

// Utility
mod util {
    pub(super) fn find_index<I, II, T, F>(iter: I, target: T, mut map: F) -> Option<usize>
    where
        I: Iterator<Item = II>,
        T: PartialEq,
        F: FnMut(II) -> Option<T>,
    {
        iter.enumerate().find_map(|(i, ii)| {
            if let Some(ii) = map(ii) {
                (ii == target).then_some(i)
            } else {
                None
            }
        })
    }

    pub(super) fn _put_str_join<'a, I, II, F>(
        iter: I,
        buf: &mut String,
        pre: &str,
        sep: &str,
        last_punct: &str,
        mut put_fn: F,
    ) where
        I: Iterator<Item = &'a II>,
        II: 'a,
        F: FnMut(&II, &mut String),
    {
        let prev = buf.len();
        for item in iter {
            buf.push_str(pre);
            put_fn(item, buf);
            buf.push_str(sep);
        }
        if buf.len() > prev {
            for _ in 0..sep.len() {
                buf.pop();
            }
            buf.push_str(last_punct);
        }
    }

    pub(super) fn put_str_join<'a, I, II>(
        iter: I,
        buf: &mut String,
        pre: &str,
        sep: &str,
        last_punct: &str,
    ) where
        I: Iterator<Item = &'a II>,
        II: super::PutStr + 'a,
    {
        _put_str_join(iter, buf, pre, sep, last_punct, super::PutStr::put_str);
    }

    pub(super) fn put_str_pretty_join<'a, I, II>(
        iter: I,
        buf: &mut String,
        pre: &str,
        sep: &str,
        last_punct: &str,
    ) where
        I: Iterator<Item = &'a II>,
        II: super::PutStrPretty + 'a,
    {
        _put_str_join(
            iter,
            buf,
            pre,
            sep,
            last_punct,
            super::PutStrPretty::put_str_pretty,
        );
    }

    pub(super) fn put_attrs<'a, I, II>(iter: I, buf: &mut String)
    where
        I: Iterator<Item = &'a II>,
        II: super::PutStr + 'a,
    {
        let prev = buf.len();
        put_str_join(iter, buf, "", "", "");
        if buf.len() > prev {
            // Safety: `buf` is not empty.
            unsafe {
                if buf.chars().last().unwrap_unchecked() != ')' {
                    buf.push(' ');
                }
            }
        }
    }

    pub(super) fn put_attrs_pretty<'a, I, II>(iter: I, buf: &mut String)
    where
        I: Iterator<Item = &'a II>,
        II: super::PutStr + 'a,
    {
        let prev = buf.len();
        put_str_join(iter, buf, "", " ", "");
        if buf.len() > prev {
            buf.push(' ');
        }
    }

    pub(super) fn get_last_spaces(buf: &str) -> usize {
        buf.chars()
            .rev()
            .enumerate()
            .find_map(|(i, c)| (c != ' ').then_some(i))
            .unwrap_or_default()
    }

    pub(super) fn get_last_whitespaces(buf: &str) -> usize {
        buf.chars()
            .rev()
            .enumerate()
            .find_map(|(i, c)| (!c.is_whitespace()).then_some(i))
            .unwrap_or_default()
    }

    pub(super) fn get_last_indent(buf: &str) -> usize {
        let mut ns: isize = -1;
        for (i, ch) in buf.chars().rev().enumerate() {
            if ch == '\n' {
                return i - (ns + 1) as usize;
            }
            if ch != ' ' {
                ns = i as isize;
            }
        }
        0
    }

    pub(super) fn pushn(buf: &mut String, ch: char, n: usize) {
        for _ in 0..n {
            buf.push(ch);
        }
    }

    pub(super) fn popn(buf: &mut String, n: usize) {
        for _ in 0..n {
            buf.pop();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate as my_wgsl;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_build() {
        let mut builder = Builder::new();

        // ref: https://www.w3.org/TR/WGSL/

        #[wgsl_decl_struct]
        struct PointLight {
            position: vec3f,
            color: vec3f,
        }

        #[wgsl_decl_struct]
        struct LightStorage {
            pointCount: u32,
            point: array<PointLight>,
        }

        wgsl_structs![builder, PointLight, LightStorage];

        wgsl_bind!(
            builder, group(0) binding(0) var<storage> lights : LightStorage
        );
        wgsl_bind!(
            builder, group(1) binding(0) var baseColorSampler : sampler
        );
        wgsl_bind!(
            builder, group(1) binding(1) var baseColorTexture : texture_2d<f32>
        );

        wgsl_fn!(builder,
            #[fragment]
            fn fragmentMain(#[location(0)] worldPos : vec3f,
                            #[location(1)] normal : vec3f,
                            #[location(2)] uv : vec2f) -> #[location(0)] vec4f {
                // Sample the base color of the surface from a texture.
                let baseColor = textureSample(baseColorTexture, baseColorSampler, uv);

                let N = normalize(normal);
                var surfaceColor = vec3f(0);

                // Loop over the scene point lights.
                for (var i = 0u; i < lights.pointCount; ++i) {
                    let worldToLight = lights.point[i].position - worldPos;
                    let dist = length(worldToLight);
                    let dir = normalize(worldToLight);

                    // Determine the contribution of this light to the surface color.
                    let radiance = lights.point[i].color * (1 / pow(dist, 2));
                    let nDotL = max(dot(N, dir), 0);

                    // Accumulate light contribution to the surface color.
                    surfaceColor += baseColor.rgb * radiance * nDotL;
                }

                // Return the accumulated surface color.
                return vec4(surfaceColor, baseColor.a);
            }
        );

        let mut non_pretty = builder.build();
        let mut pretty = builder.build_pretty();
        non_pretty.retain(|c| !c.is_whitespace());
        pretty.retain(|c| !c.is_whitespace());
        assert_eq!(non_pretty, pretty);
    }
}
