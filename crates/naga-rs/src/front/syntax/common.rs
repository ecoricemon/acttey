use quote::ToTokens;
use std::{any::Any, fmt, mem};
use syn_locator::Locate;

// === IdentifySyn ===

pub trait IdentifySyn: Any + Locate {
    fn as_any(&self) -> &dyn Any;

    fn syn_id(&self) -> SynId
    where
        Self: Sized,
    {
        SynId(self as *const Self as *const dyn IdentifySyn)
    }

    fn content(&self) -> String {
        Locate::code(self)
    }
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

impl_identify_syn!(crate::front::syntax::file::SmFile);
impl_identify_syn!(syn::Block);
impl_identify_syn!(syn::Expr);
impl_identify_syn!(syn::ExprBinary);
impl_identify_syn!(syn::ExprLit);
impl_identify_syn!(syn::ExprPath);
impl_identify_syn!(syn::Field);
impl_identify_syn!(syn::FnArg);
impl_identify_syn!(syn::ItemConst);
impl_identify_syn!(syn::ItemFn);
impl_identify_syn!(syn::ItemMod);
impl_identify_syn!(syn::ItemStruct);
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
    pub fn downcast<T: Any>(&self) -> &T {
        let r = unsafe { self.0.as_ref().unwrap() };
        r.as_any().downcast_ref().unwrap()
    }

    pub fn content(&self) -> String {
        unsafe { self.0.as_ref().unwrap_unchecked() }.content()
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

// === AttributeHelper ===

pub trait AttributeHelper {
    fn get_attributes(&self) -> Option<&Vec<syn::Attribute>>;
    fn get_mut_attributes(&mut self) -> Option<&mut Vec<syn::Attribute>>;

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

    fn replace_attributes(&mut self, new: Vec<syn::Attribute>) -> Vec<syn::Attribute> {
        let Some(old) = self.get_mut_attributes() else {
            return Vec::new();
        };
        mem::replace(old, new)
    }

    /// Expands this vector by attaching the given value to the front of this
    /// vector.
    fn insert_front(&mut self, mut front: Vec<syn::Attribute>) {
        let Some(this) = self.get_mut_attributes() else {
            return;
        };
        front.append(this);
        let _ = mem::replace(this, front);
    }

    /// #[path(inner)]
    fn get_attribute_inner(&self, path: &str) -> Option<String> {
        let attr = self
            .get_attributes()?
            .iter()
            .find(|attr| attr.path().is_ident(path))?;
        if let syn::Meta::List(l) = &attr.meta {
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
        if let syn::Meta::NameValue(nv) = &attr.meta {
            Some(nv.value.to_token_stream().to_string())
        } else {
            None
        }
    }
}

macro_rules! impl_attribute_helper_for_simple {
    ($ty:ty) => {
        impl AttributeHelper for $ty {
            fn get_attributes(&self) -> Option<&Vec<syn::Attribute>> {
                Some(&self.attrs)
            }

            fn get_mut_attributes(&mut self) -> Option<&mut Vec<syn::Attribute>> {
                Some(&mut self.attrs)
            }
        }
    };
}

impl_attribute_helper_for_simple!(syn::ExprArray);
impl_attribute_helper_for_simple!(syn::ExprAssign);
impl_attribute_helper_for_simple!(syn::ExprAsync);
impl_attribute_helper_for_simple!(syn::ExprAwait);
impl_attribute_helper_for_simple!(syn::ExprBinary);
impl_attribute_helper_for_simple!(syn::ExprBlock);
impl_attribute_helper_for_simple!(syn::ExprBreak);
impl_attribute_helper_for_simple!(syn::ExprCall);
impl_attribute_helper_for_simple!(syn::ExprCast);
impl_attribute_helper_for_simple!(syn::ExprClosure);
impl_attribute_helper_for_simple!(syn::ExprConst);
impl_attribute_helper_for_simple!(syn::ExprContinue);
impl_attribute_helper_for_simple!(syn::ExprField);
impl_attribute_helper_for_simple!(syn::ExprForLoop);
impl_attribute_helper_for_simple!(syn::ExprGroup);
impl_attribute_helper_for_simple!(syn::ExprIf);
impl_attribute_helper_for_simple!(syn::ExprIndex);
impl_attribute_helper_for_simple!(syn::ExprInfer);
impl_attribute_helper_for_simple!(syn::ExprLet);
impl_attribute_helper_for_simple!(syn::ExprLit);
impl_attribute_helper_for_simple!(syn::ExprLoop);
impl_attribute_helper_for_simple!(syn::ExprMacro);
impl_attribute_helper_for_simple!(syn::ExprMatch);
impl_attribute_helper_for_simple!(syn::ExprMethodCall);
impl_attribute_helper_for_simple!(syn::ExprParen);
impl_attribute_helper_for_simple!(syn::ExprPath);
impl_attribute_helper_for_simple!(syn::ExprRange);
impl_attribute_helper_for_simple!(syn::ExprRawAddr);
impl_attribute_helper_for_simple!(syn::ExprReference);
impl_attribute_helper_for_simple!(syn::ExprRepeat);
impl_attribute_helper_for_simple!(syn::ExprReturn);
impl_attribute_helper_for_simple!(syn::ExprStruct);
impl_attribute_helper_for_simple!(syn::ExprTry);
impl_attribute_helper_for_simple!(syn::ExprTryBlock);
impl_attribute_helper_for_simple!(syn::ExprTuple);
impl_attribute_helper_for_simple!(syn::ExprUnary);
impl_attribute_helper_for_simple!(syn::ExprUnsafe);
impl_attribute_helper_for_simple!(syn::ExprWhile);
impl_attribute_helper_for_simple!(syn::ExprYield);
impl_attribute_helper_for_simple!(syn::Field);
impl_attribute_helper_for_simple!(syn::ItemConst);
impl_attribute_helper_for_simple!(syn::ItemMod);
impl_attribute_helper_for_simple!(syn::ItemStruct);

impl AttributeHelper for syn::Item {
    fn get_attributes(&self) -> Option<&Vec<syn::Attribute>> {
        match self {
            syn::Item::Const(v) => v.get_attributes(),
            syn::Item::Mod(v) => v.get_attributes(),
            syn::Item::Struct(v) => v.get_attributes(),
            _ => None,
        }
    }

    fn get_mut_attributes(&mut self) -> Option<&mut Vec<syn::Attribute>> {
        match self {
            syn::Item::Const(v) => v.get_mut_attributes(),
            syn::Item::Mod(v) => v.get_mut_attributes(),
            syn::Item::Struct(v) => v.get_mut_attributes(),
            _ => None,
        }
    }
}

impl AttributeHelper for syn::Expr {
    fn get_attributes(&self) -> Option<&Vec<syn::Attribute>> {
        match self {
            Self::Array(v) => v.get_attributes(),
            Self::Assign(v) => v.get_attributes(),
            Self::Async(v) => v.get_attributes(),
            Self::Await(v) => v.get_attributes(),
            Self::Binary(v) => v.get_attributes(),
            Self::Block(v) => v.get_attributes(),
            Self::Break(v) => v.get_attributes(),
            Self::Call(v) => v.get_attributes(),
            Self::Cast(v) => v.get_attributes(),
            Self::Closure(v) => v.get_attributes(),
            Self::Const(v) => v.get_attributes(),
            Self::Continue(v) => v.get_attributes(),
            Self::Field(v) => v.get_attributes(),
            Self::ForLoop(v) => v.get_attributes(),
            Self::Group(v) => v.get_attributes(),
            Self::If(v) => v.get_attributes(),
            Self::Index(v) => v.get_attributes(),
            Self::Infer(v) => v.get_attributes(),
            Self::Let(v) => v.get_attributes(),
            Self::Lit(v) => v.get_attributes(),
            Self::Loop(v) => v.get_attributes(),
            Self::Macro(v) => v.get_attributes(),
            Self::Match(v) => v.get_attributes(),
            Self::MethodCall(v) => v.get_attributes(),
            Self::Paren(v) => v.get_attributes(),
            Self::Path(v) => v.get_attributes(),
            Self::Range(v) => v.get_attributes(),
            Self::RawAddr(v) => v.get_attributes(),
            Self::Reference(v) => v.get_attributes(),
            Self::Repeat(v) => v.get_attributes(),
            Self::Return(v) => v.get_attributes(),
            Self::Struct(v) => v.get_attributes(),
            Self::Try(v) => v.get_attributes(),
            Self::TryBlock(v) => v.get_attributes(),
            Self::Tuple(v) => v.get_attributes(),
            Self::Unary(v) => v.get_attributes(),
            Self::Unsafe(v) => v.get_attributes(),
            Self::Verbatim(_) => None,
            Self::While(v) => v.get_attributes(),
            Self::Yield(v) => v.get_attributes(),
            _ => unreachable!("non-exhaustive"),
        }
    }

    fn get_mut_attributes(&mut self) -> Option<&mut Vec<syn::Attribute>> {
        match self {
            Self::Array(v) => v.get_mut_attributes(),
            Self::Assign(v) => v.get_mut_attributes(),
            Self::Async(v) => v.get_mut_attributes(),
            Self::Await(v) => v.get_mut_attributes(),
            Self::Binary(v) => v.get_mut_attributes(),
            Self::Block(v) => v.get_mut_attributes(),
            Self::Break(v) => v.get_mut_attributes(),
            Self::Call(v) => v.get_mut_attributes(),
            Self::Cast(v) => v.get_mut_attributes(),
            Self::Closure(v) => v.get_mut_attributes(),
            Self::Const(v) => v.get_mut_attributes(),
            Self::Continue(v) => v.get_mut_attributes(),
            Self::Field(v) => v.get_mut_attributes(),
            Self::ForLoop(v) => v.get_mut_attributes(),
            Self::Group(v) => v.get_mut_attributes(),
            Self::If(v) => v.get_mut_attributes(),
            Self::Index(v) => v.get_mut_attributes(),
            Self::Infer(v) => v.get_mut_attributes(),
            Self::Let(v) => v.get_mut_attributes(),
            Self::Lit(v) => v.get_mut_attributes(),
            Self::Loop(v) => v.get_mut_attributes(),
            Self::Macro(v) => v.get_mut_attributes(),
            Self::Match(v) => v.get_mut_attributes(),
            Self::MethodCall(v) => v.get_mut_attributes(),
            Self::Paren(v) => v.get_mut_attributes(),
            Self::Path(v) => v.get_mut_attributes(),
            Self::Range(v) => v.get_mut_attributes(),
            Self::RawAddr(v) => v.get_mut_attributes(),
            Self::Reference(v) => v.get_mut_attributes(),
            Self::Repeat(v) => v.get_mut_attributes(),
            Self::Return(v) => v.get_mut_attributes(),
            Self::Struct(v) => v.get_mut_attributes(),
            Self::Try(v) => v.get_mut_attributes(),
            Self::TryBlock(v) => v.get_mut_attributes(),
            Self::Tuple(v) => v.get_mut_attributes(),
            Self::Unary(v) => v.get_mut_attributes(),
            Self::Unsafe(v) => v.get_mut_attributes(),
            Self::Verbatim(_) => None,
            Self::While(v) => v.get_mut_attributes(),
            Self::Yield(v) => v.get_mut_attributes(),
            _ => unreachable!("non-exhaustive"),
        }
    }
}
