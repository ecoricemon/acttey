use super::{path::*, structs::LayoutExt, traits::*};
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, TokenStreamExt, quote};
use syn::{
    Error, Expr, LitInt, Path, Result, Token,
    parse::{Parse, ParseStream},
    parse_macro_input,
    spanned::Spanned,
};

// Must be the same as the function name.
pub(crate) const EXTERN_TYPE: &str = "extern_type";

pub(crate) fn extern_type(item: TokenStream) -> TokenStream {
    parse_macro_input!(item as ExternType)
        .into_token_stream()
        .into()
}

// Must be the same as the function name.
pub(crate) const EXTERN_CONST: &str = "extern_const";

pub(crate) fn extern_const(item: TokenStream) -> TokenStream {
    parse_macro_input!(item as ExternConst)
        .into_token_stream()
        .into()
}

#[derive(Debug)]
pub(crate) struct ExternType {
    pub(crate) path: WgslPath,
    _comma_token1: Token![,],
    pub(crate) size: usize,
    _comma_token2: Token![,],
    pub(crate) align: usize,
}

impl ExternType {
    pub(crate) fn layout(&self) -> LayoutExt {
        const IS_SIZED: bool = true;

        LayoutExt::new(self.size, self.align, IS_SIZED).unwrap()
    }
}

impl AsIdent for ExternType {
    fn as_ident(&self) -> Result<&syn::Ident> {
        self.path.as_ident()
    }
}

impl Parse for ExternType {
    fn parse(input: ParseStream) -> Result<Self> {
        let path: Path = input.parse()?;
        let path = WgslPath::from_syn(path)?;
        let _comma_token1: Token![,] = input.parse()?;
        let size_lit: LitInt = input.parse()?;
        let _comma_token2: Token![,] = input.parse()?;
        let align_lit: LitInt = input.parse()?;

        let size = size_lit.base10_parse::<usize>()?;
        let align = align_lit.base10_parse::<usize>()?;

        let _ =
            LayoutExt::new(size, align, true).ok_or(Error::new(path.span(), "invalid layout"))?;

        // Records new path.
        insert_wgsl_path(path.to_string());

        Ok(Self {
            path,
            _comma_token1,
            size,
            _comma_token2,
            align,
        })
    }
}

impl ToTokens for ExternType {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self {
            path, size, align, ..
        } = self;

        let size_errmsg = format!("size of `{path}` is not {size}");
        let align_errmsg = format!("alignment of `{path}` is not {align}");

        tokens.append_all(quote! {
            const _: () = assert!(size_of::<#path>() == #size, #size_errmsg);
            const _: () = assert!(align_of::<#path>() == #align, #align_errmsg);
        });
    }
}

#[derive(Debug)]
pub(crate) struct ExternConst {
    pub(crate) path: WgslPath,
    _comma_token: Token![,],
    pub(crate) value: Expr,
}

impl AsIdent for ExternConst {
    fn as_ident(&self) -> Result<&syn::Ident> {
        self.path.as_ident()
    }
}

impl Parse for ExternConst {
    fn parse(input: ParseStream) -> Result<Self> {
        let path: Path = input.parse()?;
        let path = WgslPath::from_syn(path)?;
        let _comma_token: Token![,] = input.parse()?;
        let value: Expr = input.parse()?;

        // Records new path.
        insert_wgsl_path(path.to_string());

        Ok(Self {
            path,
            _comma_token,
            value,
        })
    }
}

impl ToTokens for ExternConst {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self { path, value, .. } = self;

        let value_str = value.to_token_stream().to_string();
        let errmsg = format!("`{path}` is not {value_str}");

        tokens.append_all(quote! {
            const _: () = assert!(#path == #value, #errmsg);
        });
    }
}
