use super::{
    attr::Attributes,
    to_code::{ConstructPrettyCode, ConstructWgslCode},
    util,
};
use std::{cell::UnsafeCell, fmt, marker::PhantomData, ops::Deref};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WgslVarDecl {
    /// Attributes of the value or variable declaration.
    ///
    /// ```text
    /// e.g. *group(0)* *binding(0)* var<storage> buf: array<f32>
    /// ```
    pub attrs: Attributes,

    /// Kind of the value or variable declaration.
    ///
    /// ```text
    /// e.g. group(0) binding(0) *var*<storage> buf: array<f32>
    /// ```
    pub kind: VarKind,

    /// Templates of the value or variable declaration.
    ///
    /// ```text
    /// e.g. group(0) binding(0) var<*storage*> buf: array<f32>
    /// ```
    pub templates: Vec<String>,

    /// Name of the value or variable declaration.
    ///
    /// ```text
    /// e.g. group(0) binding(0) var<storage> *buf*: array<f32>
    /// ```
    pub ident: String,

    /// Type of the value or variable declaration.
    ///
    /// ```text
    /// e.g. group(0) binding(0) var<storage> buf : *array<f32>*
    /// ```
    pub ty: Option<String>,

    /// Expression of the value or variable declaration.
    ///
    /// ```text
    /// e.g. const x: i32 = *0*
    /// ```
    pub expr: Option<String>,
}

impl WgslVarDecl {
    pub const fn new() -> Self {
        Self {
            attrs: Attributes::new(),
            kind: VarKind::Var,
            templates: Vec::new(),
            ident: String::new(),
            ty: None,
            expr: None,
        }
    }

    /// Retrieves the index of the template that has the given name.
    pub fn find_template(&self, template: &str) -> Option<usize> {
        util::find_index(self.templates.iter(), template, |v| Some(v))
    }

    /// Appends the given template.
    pub fn push_template(&mut self, template: String) {
        self.templates.push(template)
    }

    /// Tries to remove the template that has the given name.
    pub fn remove_template(&mut self, template: &str) -> Option<String> {
        self.find_template(template)
            .map(|i| self.templates.remove(i))
    }
}

impl Default for WgslVarDecl {
    fn default() -> Self {
        Self::new()
    }
}

impl ConstructWgslCode for WgslVarDecl {
    fn write_wgsl_string(&self, buf: &mut String) {
        util::put_attrs(self.attrs.iter(), buf);
        self.kind.write_wgsl_string(buf);
        if !self.templates.is_empty() {
            buf.push('<');
            util::put_str_join(self.templates.iter(), buf, "", ",", "");
            buf.push('>');
        } else {
            buf.push(' ');
        }
        buf.push_str(self.ident.as_str());
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

impl ConstructPrettyCode for WgslVarDecl {
    fn write_pretty_code(&self, buf: &mut String) {
        util::put_attrs_pretty(self.attrs.iter(), buf);
        self.kind.write_pretty_code(buf);
        if !self.templates.is_empty() {
            buf.push('<');
            util::put_str_join(self.templates.iter(), buf, "", ", ", "");
            buf.push('>');
        }
        buf.push(' ');
        buf.push_str(self.ident.as_str());
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VarKind {
    Const,
    Override,
    Let,
    Var,
}

impl ConstructWgslCode for VarKind {
    fn write_wgsl_string(&self, buf: &mut String) {
        match self {
            Self::Const => buf.push_str("const"),
            Self::Override => buf.push_str("override"),
            Self::Let => buf.push_str("let"),
            Self::Var => buf.push_str("var"),
        }
    }
}

impl ConstructPrettyCode for VarKind {}

/// `const foo: Override<T>` corresponds to `override foo: T`.
#[repr(transparent)]
pub struct Override<T>(PhantomData<T>);

/// `static foo: Private<T>` corresponds to `var<private> foo: T`.
#[repr(transparent)]
pub struct Private<T>(Var<T>);

/// `static foo: Workgroup<T>` corresponds to `var<workgroup> foo: T`.
#[repr(transparent)]
pub struct Workgroup<T>(Var<T>);

/// `static foo: Uniform<T, G, B>` corresponds to `@group(G) @binding(B)
/// var<uniform> foo: T`.
#[repr(transparent)]
pub struct Uniform<T, const G: usize, const B: usize>(Var<T>);

/// `static foo: Storage<T, G, B, RW>` corresponds to `@group(G) @binding(B)
/// var<storage> foo: T`.
#[repr(transparent)]
pub struct Storage<T, const G: usize, const B: usize, const RW: bool>(Var<T>);

#[repr(transparent)]
pub struct Var<T>(UnsafeCell<T>);

// For usage as static variable.
unsafe impl<T> Send for Var<T> {}
unsafe impl<T> Sync for Var<T> {}

impl<T> Var<T> {
    pub const fn new(value: T) -> Self {
        Self(UnsafeCell::new(value))
    }

    pub fn set_field<V>(&self, offset: usize, value: V) {
        unsafe {
            let dst = self.field_ptr::<V>(offset).as_mut().unwrap_unchecked();
            *dst = value;
        }
    }

    pub fn set_field_elem<V>(&self, offset: usize, index: usize, value: V) {
        unsafe {
            let ptr = self.field_ptr::<V>(offset);
            let dst = ptr.add(index).as_mut().unwrap_unchecked();
            *dst = value;
        }
    }

    unsafe fn field_ptr<V>(&self, offset: usize) -> *mut V {
        let ptr = self.0.get().cast::<V>();
        let ptr = unsafe { ptr.byte_add(offset) };
        assert!(ptr.is_aligned());
        ptr
    }
}

impl<T> Deref for Var<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.get().as_ref().unwrap_unchecked() }
    }
}

impl<T: fmt::Debug> fmt::Debug for Var<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}
