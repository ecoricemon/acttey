use crate::traits::FromSyn;
use proc_macro2::TokenStream as TokenStream2;
use quote::ToTokens;
use std::ops::{Deref, DerefMut};
use syn::{
    AttrStyle, Attribute, Expr, Ident, Meta, Result, Token, bracketed, parenthesized,
    parse::{Parse, ParseStream},
    spanned::Spanned,
    token::{Bracket, Paren},
};

// === SmAttributes ===

#[derive(Clone, Debug)]
pub struct SmAttributes(pub Vec<SmAttribute>);

impl SmAttributes {
    pub const fn new() -> Self {
        Self(Vec::new())
    }

    pub fn contains_custom(&self) -> bool {
        self.iter().any(SmAttribute::is_custom)
    }

    pub fn parse_outer(input: ParseStream) -> Result<Self> {
        let mut attrs = Vec::new();
        while input.peek(Token![#]) {
            attrs.push(input.call(SmAttribute::parse_outer)?);
        }
        let this = Self(attrs);
        this.validate()?;
        Ok(this)
    }

    pub fn parse_inner(input: ParseStream) -> Result<Self> {
        let mut attrs = Vec::new();
        while input.peek(Token![#]) && input.peek2(Token![!]) {
            attrs.push(input.call(SmAttribute::parse_inner)?);
        }
        let this = Self(attrs);
        this.validate()?;
        Ok(this)
    }

    pub fn extend_by_parsing_inner(&mut self, input: ParseStream) -> Result<()> {
        while input.peek(Token![#]) && input.peek2(Token![!]) {
            self.push(input.call(SmAttribute::parse_inner)?);
        }
        Ok(())
    }

    pub fn outer(&self) -> impl Iterator<Item = &SmAttribute> {
        self.iter().filter(|attr| attr.is_outer())
    }

    pub fn inner(&self) -> impl Iterator<Item = &SmAttribute> {
        self.iter().filter(|attr| !attr.is_outer())
    }

    fn validate(&self) -> Result<()> {
        if self.iter().filter(|attr| attr.is_branch()).count() > 1 {
            Err(syn::Error::new(
                self.iter().next().unwrap().span(),
                format!(
                    "`{}`, `{}`, or `{}` conflict",
                    SmAttribute::IF,
                    SmAttribute::ELIF,
                    SmAttribute::ELSE
                ),
            ))
        } else {
            Ok(())
        }
    }
}

impl Deref for SmAttributes {
    type Target = Vec<SmAttribute>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for SmAttributes {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl FromSyn<Vec<Attribute>> for SmAttributes {
    fn from_syn(input: Vec<Attribute>) -> Result<Self> {
        let mut attrs = Vec::new();
        for attr in input {
            attrs.push(FromSyn::from_syn(attr)?);
        }
        Ok(Self(attrs))
    }
}

// === SmAttribute ===

#[derive(Clone, Debug)]
pub struct SmAttribute {
    pub pound_token: Token![#],
    pub style: AttrStyle,
    pub bracket_token: Bracket,
    pub meta: SmMeta,
    pub rust_show: bool,
    pub wgsl_show: bool,
}

impl SmAttribute {
    const IF: &str = "IF";
    const ELIF: &str = "ELIF";
    const ELSE: &str = "ELSE";

    pub fn is_custom(&self) -> bool {
        self.is_branch()
    }

    pub fn is_branch(&self) -> bool {
        matches!(
            self.meta,
            SmMeta::If { .. } | SmMeta::ElIf { .. } | SmMeta::Else { .. }
        )
    }

    pub fn is_path(&self, path: &str) -> bool {
        match &self.meta {
            SmMeta::If { .. } => path == Self::IF,
            SmMeta::ElIf { .. } => path == Self::ELIF,
            SmMeta::Else { .. } => path == Self::ELSE,
            SmMeta::Other(o) => o.path().is_ident(path),
        }
    }

    pub fn path_string(&self) -> String {
        match &self.meta {
            SmMeta::If { .. } => Self::IF.to_owned(),
            SmMeta::ElIf { .. } => Self::ELIF.to_owned(),
            SmMeta::Else { .. } => Self::ELSE.to_owned(),
            SmMeta::Other(o) => o.path().to_token_stream().to_string(),
        }
    }

    pub fn is_outer(&self) -> bool {
        self.style == AttrStyle::Outer
    }

    pub fn parse_outer(input: ParseStream) -> Result<Self> {
        let content;
        let mut this = Self {
            pound_token: input.parse()?,
            style: AttrStyle::Outer,
            bracket_token: bracketed!(content in input),
            meta: content.parse()?,
            rust_show: false,
            wgsl_show: false,
        };
        this.complete_attribute();
        Ok(this)
    }

    pub fn parse_inner(input: ParseStream) -> Result<Self> {
        let content;
        let mut this = Self {
            pound_token: input.parse()?,
            style: AttrStyle::Inner(input.parse()?),
            bracket_token: bracketed!(content in input),
            meta: content.parse()?,
            rust_show: false,
            wgsl_show: false,
        };
        this.complete_attribute();
        Ok(this)
    }

    #[rustfmt::skip]
    fn complete_attribute(&mut self) {
        const WGSL_ONLY: [&str; 16] = [
            "align", "binding", "blend_src", "builtin", "const", "diagnostic",
            "group", "id", "interpolate", "invariant", "location", "size",
            "workgroup_size", "vectex", "fragment", "compute"
        ];
        const BOTH: [&str; 1] = ["must_use"];

        if WGSL_ONLY.iter().any(|s| self.is_path(s)) {
            self.rust_show = false;
            self.wgsl_show = true;
        } else if BOTH.iter().any(|s| self.is_path(s)) {
            self.rust_show = true;
            self.wgsl_show = true;
        } else {
            self.rust_show = true;
            self.wgsl_show = false;
        };
    }
}

impl FromSyn<Attribute> for SmAttribute {
    fn from_syn(input: Attribute) -> Result<Self> {
        let mut this = SmAttribute {
            pound_token: input.pound_token,
            style: input.style,
            bracket_token: input.bracket_token,
            meta: FromSyn::from_syn(input.meta)?,
            rust_show: false,
            wgsl_show: false,
        };
        this.complete_attribute();
        Ok(this)
    }
}

impl ToTokens for SmAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        if !self.rust_show {
            return;
        }

        self.pound_token.to_tokens(tokens);
        if let AttrStyle::Inner(not_token) = &self.style {
            not_token.to_tokens(tokens);
        }
        self.bracket_token.surround(tokens, |tokens| {
            self.meta.to_tokens(tokens);
        });
    }
}

// === SmMeta ===

#[derive(Clone, Debug)]
pub enum SmMeta {
    If {
        if_token: Ident,
        paren_token: Paren,
        cond: Expr,
    },
    ElIf {
        elif_token: Ident,
        paren_token: Paren,
        cond: Expr,
    },
    Else {
        else_token: Ident,
    },
    Other(Meta),
}

impl FromSyn<Meta> for SmMeta {
    fn from_syn(input: Meta) -> Result<Self> {
        Ok(Self::Other(input))
    }
}

impl Parse for SmMeta {
    fn parse(input: ParseStream) -> Result<Self> {
        let ahead = input.fork();
        if let Ok(ident) = ahead.parse::<Ident>() {
            if ident == SmAttribute::IF {
                let content;
                return Ok(Self::If {
                    if_token: input.parse()?,
                    paren_token: parenthesized!(content in input),
                    cond: content.parse()?,
                });
            } else if ident == SmAttribute::ELIF {
                let content;
                return Ok(Self::ElIf {
                    elif_token: input.parse()?,
                    paren_token: parenthesized!(content in input),
                    cond: content.parse()?,
                });
            } else if ident == SmAttribute::ELSE {
                return Ok(Self::Else {
                    else_token: input.parse()?,
                });
            }
        }

        Ok(Self::Other(input.parse()?))
    }
}

impl ToTokens for SmMeta {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        if let Self::Other(o) = self {
            o.to_tokens(tokens);
        }
    }
}
