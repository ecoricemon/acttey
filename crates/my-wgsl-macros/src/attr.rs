use super::traits::*;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens, TokenStreamExt};
use std::ops::{Deref, DerefMut};
use syn::{Attribute, Meta, Result};

/// Attribute to be compatible with uniform address space.
pub(crate) const ATTR_HIDE: &str = "hide";
pub(crate) const ATTR_UNIFORM: &str = "uniform";

// === WgslAttributes ===

#[derive(Debug)]
pub(crate) struct WgslAttributes(Vec<WgslAttribute>);

impl WgslAttributes {
    pub(crate) const fn new() -> Self {
        Self(Vec::new())
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

impl ToTokens for WgslAttributes {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.append_all(&self.0);
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
    pub(crate) meta: Meta,
    pub(crate) rust_show: bool,
    pub(crate) wgsl_show: bool,
}

impl FromSyn<Attribute> for WgslAttribute {
    #[rustfmt::skip]
    fn from_syn(input: Attribute) -> Result<Self> {
        const WGSL_ONLY: [&str; 16] = [
            "align", "binding", "blend_src", "builtin", "const", "diagnostic",
            "group", "id", "interpolate", "invariant", "location", "size",
            "workgroup_size", "vectex", "fragment", "compute"
        ];
        const BOTH: [&str; 1] = ["must_use"];

        let (rust_show, wgsl_show) = if WGSL_ONLY.iter().any(|s| input.path().is_ident(s)) {
            (false, true)
        } else if BOTH.iter().any(|s| input.path().is_ident(s)) {
            (true, true)
        } else {
            (true, false)
        };

        Ok(Self {
            meta: input.meta,
            rust_show,
            wgsl_show
        })
    }
}

impl ToTokens for WgslAttribute {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        if !self.rust_show {
            return;
        }

        let meta = &self.meta;

        tokens.append_all(quote! {
            #[#meta]
        });
    }
}

impl ToWgslString for WgslAttribute {
    fn write_wgsl_string(&self, buf: &mut String) {
        if !self.wgsl_show {
            return;
        }

        buf.push('@');
        buf.push_str(&self.meta.path().to_token_stream().to_string());

        if let Meta::List(l) = &self.meta {
            buf.push('(');
            buf.push_str(&l.tokens.to_string());
            buf.push(')');
        }
    }
}

impl RuntimeWgslToken for WgslAttribute {
    fn runtime_tokens(&self) -> TokenStream2 {
        if !self.wgsl_show {
            return TokenStream2::new();
        }

        let outer = self.meta.path();
        let inner = match &self.meta {
            Meta::List(l) => {
                let inner = &l.tokens;
                quote! { stringify!(#inner) }
            }
            _ => quote! { "" },
        };

        quote! {
            my_wgsl::Attribute::from( ( stringify!(#outer), #inner ) )
        }
    }
}
