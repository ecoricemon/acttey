use super::{expr::*, traits::*};
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, quote};
use std::ops::{Deref, DerefMut};
use syn::{
    AttrStyle, Attribute, Expr, Meta, Result, Token, bracketed,
    parse::{Parse, ParseStream},
    token::Bracket,
};

pub(crate) const ATTR_HIDE: &str = "hide";

/// Attribute to be compatible with uniform address space.
pub(crate) const ATTR_UNIFORM: &str = "uniform";

// === WgslAttributes ===

#[derive(Debug)]
pub(crate) struct WgslAttributes(Vec<WgslAttribute>);

impl WgslAttributes {
    pub(crate) const fn new() -> Self {
        Self(Vec::new())
    }

    pub(crate) fn parse_outer(input: ParseStream) -> Result<Self> {
        let mut attrs = Vec::new();
        while input.peek(Token![#]) {
            attrs.push(input.call(WgslAttribute::parse_outer)?);
        }
        Ok(Self(attrs))
    }

    #[allow(dead_code)]
    pub(crate) fn parse_inner(input: ParseStream) -> Result<Self> {
        let mut attrs = Vec::new();
        while input.peek(Token![#]) && input.peek2(Token![!]) {
            attrs.push(input.call(WgslAttribute::parse_inner)?);
        }
        Ok(Self(attrs))
    }

    #[allow(dead_code)]
    pub(crate) fn extend_by_parsing_outer(&mut self, input: ParseStream) -> Result<()> {
        while input.peek(Token![#]) {
            self.push(input.call(WgslAttribute::parse_outer)?);
        }
        Ok(())
    }

    pub(crate) fn extend_by_parsing_inner(&mut self, input: ParseStream) -> Result<()> {
        while input.peek(Token![#]) && input.peek2(Token![!]) {
            self.push(input.call(WgslAttribute::parse_inner)?);
        }
        Ok(())
    }

    pub(crate) fn outer(&self) -> impl Iterator<Item = &WgslAttribute> {
        self.iter().filter(|attr| attr.is_outer())
    }

    pub(crate) fn inner(&self) -> impl Iterator<Item = &WgslAttribute> {
        self.iter().filter(|attr| !attr.is_outer())
    }
}

impl Deref for WgslAttributes {
    type Target = Vec<WgslAttribute>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for WgslAttributes {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl FromSyn<Vec<Attribute>> for WgslAttributes {
    fn from_syn(input: Vec<Attribute>) -> Result<Self> {
        let mut attrs = Vec::new();
        for attr in input {
            attrs.push(WgslAttribute::from_syn(attr)?);
        }
        Ok(Self(attrs))
    }
}

impl ToWgslString for WgslAttributes {
    fn write_wgsl_string(&self, buf: &mut String) {
        for attr in self.iter() {
            attr.write_wgsl_string(buf);
        }
        if !self.is_empty() && !matches!(buf.chars().last(), Some(')')) {
            buf.push(' '); // Something must follow the attributes.
        }
    }
}

// === WgslAttribute ===

#[derive(Debug)]
pub(crate) struct WgslAttribute {
    pound_token: Token![#],
    style: AttrStyle,
    bracket_token: Bracket,
    pub(crate) meta: WgslMeta,
    pub(crate) rust_show: bool,
    pub(crate) wgsl_show: bool,
}

impl WgslAttribute {
    pub(crate) fn is_outer(&self) -> bool {
        self.style == AttrStyle::Outer
    }

    pub(crate) fn parse_outer(input: ParseStream) -> Result<Self> {
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

    pub(crate) fn parse_inner(input: ParseStream) -> Result<Self> {
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

        if WGSL_ONLY.iter().any(|s| self.meta.is_path(s)) {
            self.rust_show = false;
            self.wgsl_show = true;
        } else if BOTH.iter().any(|s| self.meta.is_path(s)) {
            self.rust_show = true;
            self.wgsl_show = true;
        } else {
            self.rust_show = true;
            self.wgsl_show = false;
        };
    }
}

impl FromSyn<Attribute> for WgslAttribute {
    fn from_syn(input: Attribute) -> Result<Self> {
        let mut this = WgslAttribute {
            pound_token: input.pound_token,
            style: input.style,
            bracket_token: input.bracket_token,
            meta: WgslMeta::from_syn(input.meta)?,
            rust_show: false,
            wgsl_show: false,
        };
        this.complete_attribute();
        Ok(this)
    }
}

impl ToTokens for WgslAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        if !self.rust_show {
            return;
        }

        // #[if expr] { .. } => if expr { .. }
        if self.meta.is_branch() {
            self.meta.to_tokens(tokens);
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

impl ToWgslString for WgslAttribute {
    fn write_wgsl_string(&self, buf: &mut String) {
        if !self.wgsl_show {
            return;
        }

        buf.push('@');
        buf.push_str(&self.meta.path_string());
        self.meta.write_wgsl_string(buf);
    }
}

impl RuntimeWgslToken for WgslAttribute {
    fn runtime_tokens(&self) -> TokenStream2 {
        if !self.wgsl_show {
            return TokenStream2::new();
        }

        let outer = self.meta.path_string();
        let inner = self.meta.runtime_tokens();

        quote! {
            my_wgsl::Attribute::from( ( #outer, #inner ) )
        }
    }
}

// === WgslMeta ===

#[derive(Debug)]
pub(crate) enum WgslMeta {
    If {
        if_token: Token![if],
        cond: WgslExpr,
    },
    ElseIf {
        else_token: Token![else],
        if_token: Token![if],
        cond: WgslExpr,
    },
    Else {
        else_token: Token![else],
    },
    Other(Meta),
}

impl WgslMeta {
    const IF: &str = "if";
    const ELSE_IF: &str = "else if";
    const ELSE: &str = "else";

    pub(crate) fn is_branch(&self) -> bool {
        matches!(
            self,
            Self::If { .. } | Self::ElseIf { .. } | Self::Else { .. }
        )
    }

    pub(crate) fn is_path(&self, path: &str) -> bool {
        match self {
            Self::If { .. } => path == Self::IF,
            Self::ElseIf { .. } => path == Self::ELSE_IF,
            Self::Else { .. } => path == Self::ELSE,
            Self::Other(o) => o.path().is_ident(path),
        }
    }

    pub(crate) fn path_string(&self) -> String {
        match self {
            Self::If { .. } => Self::IF.to_owned(),
            Self::ElseIf { .. } => Self::ELSE_IF.to_owned(),
            Self::Else { .. } => Self::ELSE.to_owned(),
            Self::Other(o) => o.path().to_token_stream().to_string(),
        }
    }
}

impl Parse for WgslMeta {
    fn parse(input: ParseStream) -> Result<Self> {
        let this = if input.peek(Token![if]) {
            Self::If {
                if_token: input.parse()?,
                cond: FromSyn::from_syn(input.parse::<Expr>()?)?,
            }
        } else if input.peek(Token![else]) && input.peek2(Token![if]) {
            Self::ElseIf {
                else_token: input.parse()?,
                if_token: input.parse()?,
                cond: FromSyn::from_syn(input.parse::<Expr>()?)?,
            }
        } else if input.peek(Token![else]) {
            Self::Else {
                else_token: input.parse()?,
            }
        } else {
            Self::Other(input.parse()?)
        };
        Ok(this)
    }
}

impl FromSyn<Meta> for WgslMeta {
    fn from_syn(input: Meta) -> Result<Self> {
        Ok(Self::Other(input))
    }
}

impl ToTokens for WgslMeta {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Self::If { if_token, cond } => {
                if_token.to_tokens(tokens);
                cond.to_tokens(tokens);
            }
            Self::ElseIf {
                else_token,
                if_token,
                cond,
            } => {
                else_token.to_tokens(tokens);
                if_token.to_tokens(tokens);
                cond.to_tokens(tokens);
            }
            Self::Else { else_token } => {
                else_token.to_tokens(tokens);
            }
            Self::Other(o) => o.to_tokens(tokens),
        }
    }
}

impl ToWgslString for WgslMeta {
    fn write_wgsl_string(&self, buf: &mut String) {
        if let Self::Other(Meta::List(l)) = self {
            buf.push('(');
            buf.push_str(&l.tokens.to_string());
            buf.push(')');
        }
    }
}

impl RuntimeWgslToken for WgslMeta {
    fn runtime_tokens(&self) -> TokenStream2 {
        match self {
            Self::Other(Meta::List(l)) => {
                let inner = &l.tokens;
                quote! { stringify!(#inner) }
            }
            _ => quote! { "" },
        }
    }
}
