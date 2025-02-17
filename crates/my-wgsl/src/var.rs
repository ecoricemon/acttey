use super::{
    attr::{Attribute, Attributes},
    to_code::{PutStr, PutStrPretty},
    util,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GlobalVariable {
    /// Attributes of the global variable.
    ///
    /// ```text
    /// e.g. **group(0)** **binding(0)** var<storage> light : LightStorage.
    /// ```
    pub attrs: Attributes,

    /// Templates of the global variable.
    ///
    /// ```text
    /// e.g. group(0) binding(0) var<**storage**> light : LightStorage.
    /// ```
    pub templates: Vec<String>,

    /// Name of the global variable.
    ///
    /// ```text
    /// e.g. group(0) binding(0) var<storage> **light** : LightStorage.
    /// ```
    pub ident: String,

    /// Type of the global variable.
    ///
    /// ```text
    /// e.g. group(0) binding(0) var<storage> light : **LightStorage**.
    /// ```
    pub ty: Option<String>,

    /// Expression of the global variable.
    pub expr: Option<String>,
}

impl GlobalVariable {
    pub fn new<'a>(
        attrs: impl Iterator<Item = Attribute>,
        templates: impl Iterator<Item = &'a str>,
        ident: String,
        ty: Option<String>,
        expr: Option<String>,
    ) -> Self {
        Self {
            attrs: Attributes(attrs.collect()),
            templates: templates.map(|v| v.to_owned()).collect(),
            ident,
            ty,
            expr,
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

impl PutStr for GlobalVariable {
    fn put_ident(&self, buf: &mut String) {
        buf.push_str(self.ident.as_str())
    }

    fn put_str(&self, buf: &mut String) {
        util::put_attrs(self.attrs.iter(), buf);
        buf.push_str("var");
        if !self.templates.is_empty() {
            buf.push('<');
            util::put_str_join(self.templates.iter(), buf, "", ",", "");
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
        util::put_attrs_pretty(self.attrs.iter(), buf);
        buf.push_str("var");
        if !self.templates.is_empty() {
            buf.push('<');
            util::put_str_join(self.templates.iter(), buf, "", ", ", "");
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
