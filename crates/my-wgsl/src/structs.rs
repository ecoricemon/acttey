use super::{
    attr::{Attribute, Attributes},
    to_code::{ConstructPrettyCode, ConstructWgslCode, TAB_SIZE},
    util,
};

pub trait BeWgslStruct {
    fn be_struct() -> WgslStruct;
    fn as_bytes(&self) -> &[u8];
    fn as_mut_bytes(&mut self) -> &mut [u8];
}

// 6.2.10. Struct Types
// https://www.w3.org/TR/WGSL/#struct-types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WgslStruct {
    /// Name of the struct.
    ///
    /// e.g. struct **Light** { ... }
    pub ident: String,

    /// Struct members.
    ///
    /// e.g. struct Light { **pos : vec3f**, **color : vec3f** }
    pub members: Vec<StructMember>,
}

impl WgslStruct {
    pub fn of<T: BeWgslStruct>() -> Self {
        T::be_struct()
    }

    /// Retrieves the index of the struct member that has the given name.
    pub fn find_member(&self, ident: &str) -> Option<usize> {
        util::find_index(self.members.iter(), ident, |member| {
            Some(member.ident.as_str())
        })
    }

    /// Tests if there's the specified member.
    pub fn contains_member(&self, ident: &str) -> bool {
        self.find_member(ident).is_some()
    }

    /// Retrieves the struct member that has the given name.
    pub fn get_member(&self, ident: &str) -> Option<&StructMember> {
        // Safety: `i` is valid.
        self.find_member(ident)
            .map(|i| unsafe { self.members.get(i).unwrap_unchecked() })
    }

    /// Retrieves the struct member that has the given name.
    pub fn get_member_mut(&mut self, ident: &str) -> Option<&mut StructMember> {
        // Safety: `i` is valid.
        self.find_member(ident)
            .map(|i| unsafe { self.members.get_mut(i).unwrap_unchecked() })
    }

    /// Tries to remove the struct member that has the given name.
    /// If succeeded, returns removed one.
    pub fn remove_member(&mut self, ident: &str) -> Option<StructMember> {
        self.find_member(ident).map(|i| self.members.remove(i))
    }

    /// Retains only the members in the given `idents`.
    pub fn retain_members<'a>(&mut self, idents: impl Iterator<Item = &'a str> + Clone) {
        for i in (0..self.members.len()).rev() {
            let mut iter = idents.clone();
            if iter.all(|retain| *retain != self.members[i].ident) {
                self.members.remove(i);
            }
        }
    }

    /// Reorders members of the [`Struct`] along the given ident iterator.
    ///
    /// Caller must guarantee that all idents in the iterator appear only once
    /// and match with the members of the `Struct`.
    pub fn reorder_members<'a>(&mut self, order: impl Iterator<Item = &'a str>) {
        let mut new_members = Vec::with_capacity(self.members.len());
        for ident in order {
            let i = self.find_member(ident).unwrap();
            new_members.push(self.members[i].clone());
        }
        self.members = new_members;
    }

    pub fn merge(&mut self, rhs: &WgslStruct) {
        let num = self
            .members
            .iter()
            .filter(|member| !rhs.contains_member(member.ident.as_str()))
            .count()
            + rhs.members.len();
        let mut merged = Vec::with_capacity(num);
        for member in self.members.iter() {
            if !rhs.contains_member(member.ident.as_str()) {
                merged.push(member.clone());
            }
        }
        merged.extend(rhs.members.iter().cloned());
        self.members = merged;
    }
}

impl ConstructWgslCode for WgslStruct {
    fn write_wgsl_string(&self, buf: &mut String) {
        buf.push_str("struct ");
        buf.push_str(self.ident.as_str());
        buf.push('{');
        util::put_str_join(self.members.iter(), buf, "", ",", "");
        buf.push('}');
    }
}

impl ConstructPrettyCode for WgslStruct {
    fn write_pretty_code(&self, buf: &mut String) {
        buf.push_str("struct ");
        buf.push_str(self.ident.as_str());
        buf.push_str(" {\n");
        let tab_str = " ".repeat(TAB_SIZE);
        util::put_str_pretty_join(self.members.iter(), buf, &tab_str, ",\n", "\n");
        buf.push('}');
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructMember {
    /// Attributes of the struct member.
    ///
    /// e.g. struct VertexInput { **@location(0)** pos : vec3f }
    pub attrs: Attributes,

    /// Name of the struct member.
    ///
    /// e.g. struct VertexInput { **pos** : vec3f }
    pub ident: String,

    /// Type of the struct member.
    ///
    /// e.g. struct VertexInput { pos : **vec3f** }
    pub ty: String,
}

impl StructMember {
    /// Creates a new struct member.
    pub fn new(ident: String, ty: String) -> Self {
        Self {
            attrs: Attributes::new(),
            ident,
            ty,
        }
    }

    /// Inserts a new struct member attribute.
    /// If there was same attribute variant already,
    /// Its inner value is changed with the given `inner`.
    pub fn insert_attribute(&mut self, outer: &str, inner: Option<&str>) {
        if let Some(i) = self.attrs.find_attribute_partial(outer, inner) {
            self.attrs[i].set_inner(inner.unwrap_or_default());
        } else {
            self.attrs
                .push(Attribute::from((outer, inner.unwrap_or_default())));
        }
    }
}

impl ConstructWgslCode for StructMember {
    fn write_wgsl_string(&self, buf: &mut String) {
        util::put_attrs(self.attrs.iter(), buf);
        buf.push_str(self.ident.as_str());
        buf.push(':');
        buf.push_str(&self.ty);
    }
}

impl ConstructPrettyCode for StructMember {
    fn write_pretty_code(&self, buf: &mut String) {
        util::put_attrs_pretty(self.attrs.iter(), buf);
        buf.push_str(self.ident.as_str());
        buf.push_str(" : ");
        buf.push_str(&self.ty);
    }
}
