use crate::common::*;
use proc_macro::TokenStream;
use quote::quote;

/// Generates code looks like FnTable::new().with::<A>().with::<B>().
/// Users can use this at the constructor of generic trait implementations.
pub fn generate_fn_table(input: TokenStream) -> TokenStream {
    // Generates function table builder's ident.
    let input = input.to_string();
    let mut split = input.trim().split(',');
    let generic_name = split
        .next()
        .expect("Must put in the name of generic trait.");

    let generic_ident = gen_ident(generic_name);
    let builder_ident = clone_ident_with_suffix(&generic_ident, "FnTable");

    // Generates with() chain.
    let withs = split.map(|ty| {
        let ident = gen_ident(ty.trim());
        quote! { .with::<#ident>() }
    });

    quote! {
        #builder_ident::new()
        #(#withs)*
    }
    .into()
}
