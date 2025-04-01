use super::front::attr::{SmAttributes, SmMeta};
use proc_macro2::Span;
use quote::ToTokens;
use std::{
    any::{Any, TypeId},
    fmt::{self, Write},
};
use syn::{
    Attribute, Error, Expr, ExprLit, Field, Item, ItemConst, ItemMod, ItemStruct, ItemType,
    ItemUse, Lit, Meta, Result, Type, TypeArray, TypePath, TypeSlice, UseGlob, UseName, UseRename,
    UseTree, spanned::Spanned,
};

// === FromSyn ====

pub trait FromSyn<Input>: Sized {
    fn from_syn(input: Input) -> Result<Self>;
}

// === SmAttributeHelper ===

pub trait SmAttributeHelper {
    fn get_attributes(&self) -> Option<&SmAttributes>;
    fn get_mut_attributes(&mut self) -> Option<&mut SmAttributes>;

    fn contains_attribute(&self, path: &str) -> bool {
        let Some(attrs) = self.get_attributes() else {
            return false;
        };
        attrs.iter().any(|attr| attr.is_path(path))
    }

    fn remove_attribute(&mut self, path: &str) {
        let Some(attrs) = self.get_mut_attributes() else {
            return;
        };
        attrs.retain(|attr| !attr.is_path(path))
    }

    /// #[path(inner)]
    fn get_attribute_inner(&self, path: &str) -> Option<String> {
        let attr = self
            .get_attributes()?
            .iter()
            .find(|attr| attr.is_path(path))?;
        if let SmMeta::Other(Meta::List(l)) = &attr.meta {
            Some(l.tokens.to_string())
        } else {
            None
        }
    }

    /// #[path = value]
    fn get_attribute_value(&self, path: &str) -> Option<String> {
        let attr = self
            .get_attributes()?
            .iter()
            .find(|attr| attr.is_path(path))?;
        if let SmMeta::Other(Meta::NameValue(nv)) = &attr.meta {
            Some(nv.value.to_token_stream().to_string())
        } else {
            None
        }
    }
}

// === AttributeHelper ===

pub trait AttributeHelper {
    fn get_attributes(&self) -> Option<&Vec<Attribute>>;
    fn get_mut_attributes(&mut self) -> Option<&mut Vec<Attribute>>;

    fn contains_attribute(&self, path: &str) -> bool {
        let Some(attrs) = self.get_attributes() else {
            return false;
        };
        attrs.iter().any(|attr| attr.path().is_ident(path))
    }

    fn remove_attribute(&mut self, path: &str) {
        let Some(attrs) = self.get_mut_attributes() else {
            return;
        };
        attrs.retain(|attr| !attr.path().is_ident(path))
    }

    /// #[path(inner)]
    fn get_attribute_inner(&self, path: &str) -> Option<String> {
        let attr = self
            .get_attributes()?
            .iter()
            .find(|attr| attr.path().is_ident(path))?;
        if let Meta::List(l) = &attr.meta {
            Some(l.tokens.to_string())
        } else {
            None
        }
    }

    /// #[path = value]
    fn get_attribute_value(&self, path: &str) -> Option<String> {
        let attr = self
            .get_attributes()?
            .iter()
            .find(|attr| attr.path().is_ident(path))?;
        if let Meta::NameValue(nv) = &attr.meta {
            Some(nv.value.to_token_stream().to_string())
        } else {
            None
        }
    }
}

impl AttributeHelper for Field {
    fn get_attributes(&self) -> Option<&Vec<Attribute>> {
        Some(&self.attrs)
    }

    fn get_mut_attributes(&mut self) -> Option<&mut Vec<Attribute>> {
        Some(&mut self.attrs)
    }
}

impl AttributeHelper for Item {
    fn get_attributes(&self) -> Option<&Vec<Attribute>> {
        match self {
            Item::Const(v) => v.get_attributes(),
            Item::Mod(v) => v.get_attributes(),
            Item::Struct(v) => v.get_attributes(),
            _ => None,
        }
    }

    fn get_mut_attributes(&mut self) -> Option<&mut Vec<Attribute>> {
        match self {
            Item::Const(v) => v.get_mut_attributes(),
            Item::Mod(v) => v.get_mut_attributes(),
            Item::Struct(v) => v.get_mut_attributes(),
            _ => None,
        }
    }
}

impl AttributeHelper for ItemConst {
    fn get_attributes(&self) -> Option<&Vec<Attribute>> {
        Some(&self.attrs)
    }

    fn get_mut_attributes(&mut self) -> Option<&mut Vec<Attribute>> {
        Some(&mut self.attrs)
    }
}

impl AttributeHelper for ItemMod {
    fn get_attributes(&self) -> Option<&Vec<Attribute>> {
        Some(&self.attrs)
    }

    fn get_mut_attributes(&mut self) -> Option<&mut Vec<Attribute>> {
        Some(&mut self.attrs)
    }
}

impl AttributeHelper for ItemStruct {
    fn get_attributes(&self) -> Option<&Vec<Attribute>> {
        Some(&self.attrs)
    }

    fn get_mut_attributes(&mut self) -> Option<&mut Vec<Attribute>> {
        Some(&mut self.attrs)
    }
}

// === IdentifySyn ===

pub trait IdentifySyn: ToTokens + Any {
    fn syn_id(&self) -> SynId
    where
        Self: Sized,
    {
        SynId(self as *const Self as *const dyn IdentifySyn)
    }

    fn content(&self) -> String {
        self.to_token_stream().to_string()
    }

    fn as_any(&self) -> &dyn Any;
}

macro_rules! impl_identify_syn {
    ($ty:ty) => {
        impl IdentifySyn for $ty {
            fn as_any(&self) -> &dyn Any {
                self
            }
        }
    };
}

impl_identify_syn!(crate::front::file::SmFile);
impl_identify_syn!(crate::front::item::SmItemConst);
impl_identify_syn!(crate::front::item::SmItemMod);
impl_identify_syn!(crate::front::item::SmItemStruct);
impl_identify_syn!(crate::front::item::SmField);
impl_identify_syn!(syn::Block);
impl_identify_syn!(syn::Expr);
impl_identify_syn!(syn::ExprBinary);
impl_identify_syn!(syn::ExprLit);
impl_identify_syn!(syn::ExprPath);
impl_identify_syn!(syn::FnArg);
impl_identify_syn!(syn::ItemFn);
impl_identify_syn!(syn::ItemType);
impl_identify_syn!(syn::ItemUse);
impl_identify_syn!(syn::Lit);
impl_identify_syn!(syn::Stmt);
impl_identify_syn!(syn::Type);
impl_identify_syn!(syn::TypeArray);
impl_identify_syn!(syn::TypeSlice);
impl_identify_syn!(syn::UseGlob);
impl_identify_syn!(syn::UseName);
impl_identify_syn!(syn::UseRename);
impl_identify_syn!(syn::UseTree);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct SynId(*const dyn IdentifySyn);

impl SynId {
    pub fn content(&self) -> String {
        unsafe { self.0.as_ref().unwrap().content() }
    }

    pub fn downcast<T: Any>(&self) -> &T {
        let r = unsafe { self.0.as_ref().unwrap() };
        r.as_any().downcast_ref().unwrap()
    }
}

impl fmt::Debug for SynId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Display for SynId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}
