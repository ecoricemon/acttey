use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    DeriveInput, Ident, LitInt, Result, Token,
};

// TODO: Getting proper path programmatically, not hard codining
// because path is varing as where we are accessing from.

/// Derive macro generating an impl of the trait `Component`.
///
/// # Examples
///
/// ```
/// # use acttey_ecs_macros::Component;
///
/// #[derive(Component, Default)]
/// struct CompA;
///
/// #[derive(Component, Default)]
/// struct CompB(u8);
///
/// #[derive(Component, Default)]
/// struct CompC {
///     vel: (f32, f32, f32),
///     acc: (f32, f32, f32),
/// }
/// ```
#[proc_macro_derive(Component)]
pub fn derive_component(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let ident = ast.ident.clone();

    quote! {
        // Implements the trait `Component`.
        impl acttey::ecs::component::Component for #ident {}
    }
    .into()
}

#[proc_macro_derive(Resource)]
pub fn derive_resource(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let ident = ast.ident.clone();

    quote! {
        // Implements the trait `Resource`.
        impl acttey::ecs::resource::Resource for #ident {}
    }
    .into()
}

#[proc_macro]
pub fn nth(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Nth);
    let identifiers = input.identifiers.into_iter().collect::<Vec<_>>();
    if input.n < identifiers.len() {
        let ident = &identifiers[input.n];
        quote! { #ident }.into()
    } else {
        panic!("Index out of bounds");
    }
}

struct Nth {
    n: usize,
    _comma: Token![,],
    identifiers: Punctuated<Ident, Token![,]>,
}

impl Parse for Nth {
    fn parse(input: ParseStream) -> Result<Self> {
        let n: LitInt = input.parse()?;
        Ok(Nth {
            n: n.base10_parse()?,
            _comma: input.parse()?,
            identifiers: input.parse_terminated(Ident::parse, Token![,])?,
        })
    }
}

#[proc_macro]
pub fn repeat(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Repeat);
    let job = input.job;
    let jobs = (0..input.n).map(|i| {
        quote! { #job!(#i); }
    });

    quote! { #(#jobs)* }.into()
}

struct Repeat {
    n: usize,
    _comma: Token![,],
    job: Ident,
}

impl Parse for Repeat {
    fn parse(input: ParseStream) -> Result<Self> {
        let n: LitInt = input.parse()?;
        Ok(Repeat {
            n: n.base10_parse()?,
            _comma: input.parse()?,
            job: input.parse()?,
        })
    }
}
