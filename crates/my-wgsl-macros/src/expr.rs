use super::{attr::*, path::*, traits::*};
use proc_macro2::{Delimiter, Group, Punct, Spacing, TokenStream as TokenStream2};
use quote::{ToTokens, TokenStreamExt};
use std::ops::{Deref, DerefMut};
use syn::{
    punctuated::Punctuated, BinOp, Expr, ExprArray, ExprBinary, ExprCall, ExprLit, ExprPath, Ident,
    Lit, Result, Token,
};

// === CommaSepWgslExprs ===

#[derive(Debug)]
pub(crate) struct CommaSepWgslExprs(Punctuated<WgslExpr, Token![,]>);

impl FromSyn<Punctuated<Expr, Token![,]>> for CommaSepWgslExprs {
    fn from_syn(input: Punctuated<Expr, Token![,]>) -> Result<Self> {
        let mut exprs = Punctuated::new();
        for expr in input {
            exprs.push(WgslExpr::from_syn(expr)?);
        }
        Ok(Self(exprs))
    }
}

impl Deref for CommaSepWgslExprs {
    type Target = Punctuated<WgslExpr, Token![,]>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for CommaSepWgslExprs {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl ToTokens for CommaSepWgslExprs {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let len = self.len();
        for (i, expr) in self.iter().enumerate() {
            expr.to_tokens(tokens);
            if i < len - 1 {
                tokens.append(Punct::new(',', Spacing::Alone));
            }
        }
    }
}

impl ToWgslString for CommaSepWgslExprs {
    fn write_wgsl_string(&self, buf: &mut String) {
        let len = self.len();
        for (i, expr) in self.iter().enumerate() {
            expr.write_wgsl_string(buf);
            if i < len - 1 {
                buf.push(',');
            }
        }
    }
}

// === WgslExpr ===

#[derive(Debug)]
pub(crate) enum WgslExpr {
    Array(WgslExprArray),
    Binary(WgslExprBinary),
    Call(WgslExprCall),
    Lit(WgslExprLit),
    Path(WgslExprPath),
    Other(Expr),
}

impl FromSyn<Expr> for WgslExpr {
    fn from_syn(input: Expr) -> Result<Self> {
        Ok(match input {
            Expr::Array(e) => Self::Array(FromSyn::from_syn(e)?),
            Expr::Binary(e) => Self::Binary(FromSyn::from_syn(e)?),
            Expr::Call(e) => Self::Call(FromSyn::from_syn(e)?),
            Expr::Lit(e) => Self::Lit(FromSyn::from_syn(e)?),
            Expr::Path(e) => Self::Path(FromSyn::from_syn(e)?),
            o => Self::Other(o),
        })
    }
}

impl ToTokens for WgslExpr {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Self::Array(e) => e.to_tokens(tokens),
            Self::Binary(e) => e.to_tokens(tokens),
            Self::Call(e) => e.to_tokens(tokens),
            Self::Lit(e) => e.to_tokens(tokens),
            Self::Path(e) => e.to_tokens(tokens),
            Self::Other(o) => o.to_tokens(tokens),
        }
    }
}

impl ToWgslString for WgslExpr {
    fn write_wgsl_string(&self, buf: &mut String) {
        match self {
            Self::Array(e) => e.write_wgsl_string(buf),
            Self::Binary(e) => e.write_wgsl_string(buf),
            Self::Call(e) => e.write_wgsl_string(buf),
            Self::Lit(e) => e.write_wgsl_string(buf),
            Self::Path(e) => e.write_wgsl_string(buf),
            Self::Other(o) => o.write_wgsl_string(buf),
        }
    }
}

// === WgslExprArray ===

#[derive(Debug)]
pub(crate) struct WgslExprArray {
    pub(crate) attrs: WgslAttributes,
    pub(crate) elems: CommaSepWgslExprs,
}

impl FromSyn<ExprArray> for WgslExprArray {
    fn from_syn(input: ExprArray) -> Result<Self> {
        let attrs = WgslAttributes::from_syn(input.attrs)?;
        let elems = CommaSepWgslExprs::from_syn(input.elems)?;

        Ok(Self { attrs, elems })
    }
}

impl ToTokens for WgslExprArray {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.attrs.to_tokens(tokens);
        Group::new(Delimiter::Bracket, self.elems.to_token_stream()).to_tokens(tokens);
    }
}

impl ToWgslString for WgslExprArray {
    fn write_wgsl_string(&self, buf: &mut String) {
        self.attrs.write_wgsl_string(buf);
        buf.push_str("array(");
        self.elems.write_wgsl_string(buf);
        buf.push(')');
    }
}

// === WgslExprBinary ===

#[derive(Debug)]
pub(crate) struct WgslExprBinary {
    pub(crate) attrs: WgslAttributes,
    pub(crate) left: Box<WgslExpr>,
    pub(crate) op: BinOp,
    pub(crate) right: Box<WgslExpr>,
}

impl FromSyn<ExprBinary> for WgslExprBinary {
    fn from_syn(input: ExprBinary) -> Result<Self> {
        let attrs = WgslAttributes::from_syn(input.attrs)?;
        let left = Box::new(WgslExpr::from_syn(*input.left)?);
        let op = input.op;
        let right = Box::new(WgslExpr::from_syn(*input.right)?);

        Ok(Self {
            attrs,
            left,
            op,
            right,
        })
    }
}

impl ToTokens for WgslExprBinary {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.attrs.to_tokens(tokens);
        self.left.to_tokens(tokens);
        self.op.to_tokens(tokens);
        self.right.to_tokens(tokens);
    }
}

impl ToWgslString for WgslExprBinary {
    fn write_wgsl_string(&self, buf: &mut String) {
        self.attrs.write_wgsl_string(buf);
        self.left.write_wgsl_string(buf);
        buf.push_str(&self.op.to_token_stream().to_string());
        self.right.write_wgsl_string(buf);
    }
}

// === WgslExprCall ===

#[derive(Debug)]
pub(crate) struct WgslExprCall {
    pub(crate) attrs: WgslAttributes,
    pub(crate) func: Box<WgslExpr>,
    pub(crate) args: CommaSepWgslExprs,
}

impl FromSyn<ExprCall> for WgslExprCall {
    fn from_syn(input: ExprCall) -> Result<Self> {
        let attrs = FromSyn::from_syn(input.attrs)?;
        let func = Box::new(WgslExpr::from_syn(*input.func)?);
        let args = FromSyn::from_syn(input.args)?;

        Ok(Self { attrs, func, args })
    }
}

impl ToTokens for WgslExprCall {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.attrs.to_tokens(tokens);
        self.func.to_tokens(tokens);
        Group::new(Delimiter::Parenthesis, self.args.to_token_stream()).to_tokens(tokens);
    }
}

impl ToWgslString for WgslExprCall {
    fn write_wgsl_string(&self, buf: &mut String) {
        self.attrs.write_wgsl_string(buf);
        self.func.write_wgsl_string(buf);
        buf.push('(');
        self.args.write_wgsl_string(buf);
        buf.push(')');
    }
}

// === WgslExprLit ===

#[derive(Debug)]
pub(crate) struct WgslExprLit {
    pub(crate) attrs: WgslAttributes,
    pub(crate) lit: Lit,
}

impl FromSyn<ExprLit> for WgslExprLit {
    fn from_syn(input: ExprLit) -> Result<Self> {
        let attrs = WgslAttributes::from_syn(input.attrs)?;
        let lit = input.lit;

        Ok(Self { attrs, lit })
    }
}

impl ToTokens for WgslExprLit {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.attrs.to_tokens(tokens);
        self.lit.to_tokens(tokens);
    }
}

impl ToWgslString for WgslExprLit {
    fn write_wgsl_string(&self, buf: &mut String) {
        self.attrs.write_wgsl_string(buf);
        buf.push_str(&self.lit.to_token_stream().to_string());
    }
}

// === WgslExprPath ===

#[derive(Debug)]
pub(crate) struct WgslExprPath {
    pub(crate) attrs: WgslAttributes,
    pub(crate) path: WgslPath,
}

impl AsIdent for WgslExprPath {
    fn as_ident(&self) -> Result<&Ident> {
        self.path.as_ident()
    }
}

impl FromSyn<ExprPath> for WgslExprPath {
    fn from_syn(input: ExprPath) -> Result<Self> {
        let attrs = FromSyn::from_syn(input.attrs)?;
        let path = WgslPath::from_syn(input.path)?;

        Ok(Self { attrs, path })
    }
}

impl ToTokens for WgslExprPath {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.attrs.to_tokens(tokens);
        self.path.to_tokens(tokens);
    }
}

impl ToWgslString for WgslExprPath {
    fn write_wgsl_string(&self, buf: &mut String) {
        self.attrs.write_wgsl_string(buf);
        self.path.write_wgsl_string(buf);
    }
}
