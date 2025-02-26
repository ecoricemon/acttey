use super::{path::*, util::*};
use proc_macro2::TokenStream as TokenStream2;
use quote::ToTokens;
use syn::{
    spanned::Spanned, Attribute, BinOp, Error, Expr, ExprArray, ExprBinary, ExprCall, ExprLit,
    ExprPath, Field, Ident, Item, ItemConst, ItemStruct, Meta, Path, Result, Type, TypeArray,
    TypePath, TypeSlice,
};

pub(crate) trait RuntimeWgslToken {
    fn runtime_tokens(&self) -> TokenStream2;
}

// === AsIdent ===

pub(crate) trait AsIdent {
    fn as_ident(&self) -> Result<&Ident>;
}

impl AsIdent for ExprPath {
    fn as_ident(&self) -> Result<&Ident> {
        self.path.as_ident()
    }
}

impl AsIdent for Path {
    fn as_ident(&self) -> Result<&Ident> {
        self.segments
            .last()
            .map(|seg| &seg.ident)
            .ok_or(Error::new(self.span(), "expected non-empty path"))
    }
}

// === ToWgslString ===

pub(crate) trait ToWgslString {
    fn write_wgsl_string(&self, buf: &mut String);

    fn wgsl_string(&self) -> String {
        let mut buf = String::new();
        self.write_wgsl_string(&mut buf);
        buf
    }
}

// TODO: New AST for Expr? seems to be hard work. String match would be
// sufficient for now.
impl ToWgslString for Expr {
    fn write_wgsl_string(&self, buf: &mut String) {
        match self {
            Expr::Array(expr_array) => expr_array.write_wgsl_string(buf),
            Expr::Binary(expr_binary) => expr_binary.write_wgsl_string(buf),
            Expr::Lit(expr_lit) => expr_lit.write_wgsl_string(buf),
            Expr::Path(expr_path) => expr_path.write_wgsl_string(buf),
            Expr::Call(expr_call) => expr_call.write_wgsl_string(buf),
            _ => unimplemented!(""),
        }
    }
}

impl ToWgslString for ExprArray {
    fn write_wgsl_string(&self, buf: &mut String) {
        buf.push_str("array(");
        let len = self.elems.len();
        for (i, elem) in self.elems.iter().enumerate() {
            elem.write_wgsl_string(buf);
            if i < len - 1 {
                buf.push(',');
            }
        }
        buf.push(')');
    }
}

impl ToWgslString for ExprBinary {
    fn write_wgsl_string(&self, buf: &mut String) {
        self.left.write_wgsl_string(buf);
        buf.push_str(&self.op.to_token_stream().to_string());
        self.right.write_wgsl_string(buf);
    }
}

#[rustfmt::skip]
impl ToWgslString for ExprCall {
    fn write_wgsl_string(&self, buf: &mut String) {
        self.func.write_wgsl_string(buf);
        buf.push('(');
        let len = self.args.len();
        for (i, arg) in self.args.iter().enumerate() {
            arg.write_wgsl_string(buf);
            if i < len - 1 {
                buf.push(',');
            }
        }
        buf.push(')');
    }
}

impl ToWgslString for ExprLit {
    fn write_wgsl_string(&self, buf: &mut String) {
        buf.push_str(&self.wgsl_string())
    }

    fn wgsl_string(&self) -> String {
        self.lit.to_token_stream().to_string()
    }
}

impl ToWgslString for ExprPath {
    fn write_wgsl_string(&self, buf: &mut String) {
        self.path.write_wgsl_string(buf);
    }
}

impl ToWgslString for Path {
    fn write_wgsl_string(&self, buf: &mut String) {
        for seg in &self.segments {
            let seg_ident = seg.ident.to_string();

            // Unknown segments are discarded.
            if let Some(seg_ident) = to_wgsl_path(&seg_ident) {
                buf.push_str(seg_ident);
            }
        }
    }
}

impl ToWgslString for Type {
    fn write_wgsl_string(&self, buf: &mut String) {
        match self {
            Type::Path(ty) => ty.write_wgsl_string(buf),
            // [T; N] => array<T,N>
            Type::Array(ty) => ty.write_wgsl_string(buf),
            // [T] => array<T>
            Type::Slice(ty) => ty.write_wgsl_string(buf),
            o => buf.push_str(&o.to_token_stream().to_string()),
        }
    }
}

impl ToWgslString for TypePath {
    fn write_wgsl_string(&self, buf: &mut String) {
        self.path.write_wgsl_string(buf);
    }
}

impl ToWgslString for TypeArray {
    fn write_wgsl_string(&self, buf: &mut String) {
        buf.push_str("array<");
        self.elem.write_wgsl_string(buf);
        buf.push(',');
        self.len.write_wgsl_string(buf);
        buf.push('>');
    }
}

impl ToWgslString for TypeSlice {
    fn write_wgsl_string(&self, buf: &mut String) {
        buf.push_str("array<");
        self.elem.write_wgsl_string(buf);
        buf.push('>');
    }
}

// === IsAbstractType ===

pub(crate) trait IsAbstractType {
    fn is_abstract_type(&self) -> bool;
}

impl IsAbstractType for Type {
    fn is_abstract_type(&self) -> bool {
        match self {
            Type::Path(type_path) => type_path.is_abstract_type(),
            Type::Array(type_array) => type_array.elem.is_abstract_type(),
            Type::Slice(type_slice) => type_slice.elem.is_abstract_type(),
            _ => false,
        }
    }
}

impl IsAbstractType for TypePath {
    fn is_abstract_type(&self) -> bool {
        self.path.is_ident("i8")
            || self.path.is_ident("u8")
            || self.path.is_ident("i64")
            || self.path.is_ident("u64")
            || self.path.is_ident("isize")
            || self.path.is_ident("usize")
            || self.path.is_ident("f64")
    }
}

// === GetAttributeValue ===

pub(crate) trait GetAttributeValue {
    fn get_attribute_value(&self, outer: &str) -> Option<String>;
}

impl GetAttributeValue for Field {
    fn get_attribute_value(&self, outer: &str) -> Option<String> {
        let a = self.attrs.iter().find(|a| a.path().is_ident(outer))?;
        if let Meta::List(l) = &a.meta {
            Some(l.tokens.to_string())
        } else {
            None
        }
    }
}

// === AttributeHelper ===

pub(crate) trait AttributeHelper {
    fn get_attributes(&self) -> Option<&Vec<Attribute>>;
    fn get_mut_attributes(&mut self) -> Option<&mut Vec<Attribute>>;

    fn contains_attribute(&self, outer: &str) -> bool {
        let Some(attrs) = self.get_attributes() else {
            return false;
        };
        attrs.iter().any(|attr| attr.path().is_ident(outer))
    }

    fn remove_attribute(&mut self, outer: &str) {
        let Some(attrs) = self.get_mut_attributes() else {
            return;
        };
        attrs.retain(|attr| !attr.path().is_ident(outer))
    }
}

impl AttributeHelper for Item {
    fn get_attributes(&self) -> Option<&Vec<Attribute>> {
        match self {
            Item::Struct(st) => st.get_attributes(),
            Item::Const(c) => c.get_attributes(),
            _ => None,
        }
    }

    fn get_mut_attributes(&mut self) -> Option<&mut Vec<Attribute>> {
        match self {
            Item::Struct(st) => st.get_mut_attributes(),
            Item::Const(c) => c.get_mut_attributes(),
            _ => None,
        }
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

impl AttributeHelper for ItemConst {
    fn get_attributes(&self) -> Option<&Vec<Attribute>> {
        Some(&self.attrs)
    }

    fn get_mut_attributes(&mut self) -> Option<&mut Vec<Attribute>> {
        Some(&mut self.attrs)
    }
}

// === ToUppercase ===

pub(crate) trait ToUppercase {
    fn to_uppercase(&self) -> Self;
}

impl ToUppercase for Ident {
    fn to_uppercase(&self) -> Self {
        Ident::new(&self.to_string().to_uppercase(), self.span())
    }
}

// === FromSyn ====

pub(crate) trait FromSyn<Input>: Sized {
    fn from_syn(input: Input) -> Result<Self>;
}

// === Evaluate ===

pub(crate) trait Evaluate<Out> {
    fn evaluate<F>(&self, find: &mut F) -> Result<Out>
    where
        F: FnMut(&Ident) -> Result<Out>;
}

macro_rules! impl_evaluate_for_wgsl_expr {
    ($ty:ty) => {
        impl Evaluate<$ty> for Expr {
            fn evaluate<F>(&self, find: &mut F) -> Result<$ty>
            where
                F: FnMut(&Ident) -> Result<$ty>,
            {
                Ok(match self {
                    Expr::Binary(e) => {
                        let l = e.left.evaluate(find)?;
                        let r = e.right.evaluate(find)?;
                        let res = (e.op, l, r).evaluate(find)?;
                        if res as u64 <= i64::MAX as u64 {
                            res
                        } else {
                            return Err(Error::new(self.span(), "cannot exceed i64::MAX"));
                        }
                    }
                    Expr::Lit(e) => lit_to_number(&e.lit)?,
                    Expr::Path(e) => find(e.as_ident()?)?,
                    _ => return Err(Error::new(self.span(), "expected integer expression")),
                })
            }
        }
    };
}

macro_rules! impl_evaluate_for_int {
    ($ty:ty) => {
        impl Evaluate<$ty> for (BinOp, $ty, $ty) {
            fn evaluate<F>(&self, _find: &mut F) -> Result<$ty>
            where
                F: FnMut(&Ident) -> Result<$ty>,
            {
                let (op, l, r) = self;

                Ok(match op {
                    BinOp::Add(_) => l + r,
                    BinOp::Sub(_) => l - r,
                    BinOp::Mul(_) => l * r,
                    BinOp::Div(_) => l / r,
                    BinOp::Rem(_) => l % r,
                    BinOp::BitXor(_) => l ^ r,
                    BinOp::BitAnd(_) => l & r,
                    BinOp::BitOr(_) => l | r,
                    BinOp::Shl(_) => l << r,
                    BinOp::Shr(_) => l >> r,
                    _ => return Err(Error::new(op.span(), "not supported operator")),
                })
            }
        }
    };
}

macro_rules! impl_evaluate_for_float {
    ($ty:ty) => {
        impl Evaluate<$ty> for (BinOp, $ty, $ty) {
            fn evaluate<F>(&self, _find: &mut F) -> Result<$ty>
            where
                F: FnMut(&Ident) -> Result<$ty>,
            {
                let (op, l, r) = self;

                Ok(match op {
                    BinOp::Add(_) => l + r,
                    BinOp::Sub(_) => l - r,
                    BinOp::Mul(_) => l * r,
                    BinOp::Div(_) => l / r,
                    BinOp::Rem(_) => l % r,
                    _ => return Err(Error::new(op.span(), "not supported operator")),
                })
            }
        }
    };
}

// Evaluation of abstract-int: 64-bit signed integer
impl_evaluate_for_wgsl_expr!(i64);
impl_evaluate_for_int!(i64);

// Evaluation of abstract-float: 64-bit floating number
impl_evaluate_for_wgsl_expr!(f64);
impl_evaluate_for_float!(f64);

// Evaluation of usize: For Rust usages
impl_evaluate_for_wgsl_expr!(usize);
impl_evaluate_for_int!(usize);
