use crate::common::*;
use proc_macro::TokenStream;
use quote::quote;

/// Generates code looks like instance.add::<A>().add::<B>().
pub fn add_fn_table(input: TokenStream) -> TokenStream {
    // Generates instance ident.
    let input = input.to_string();
    let mut split = input.trim().split(',');
    let instance_name = split.next().expect("Must put in the name of instance.");
    let instance_ident = gen_ident(instance_name);

    // Generates add() chain.
    let adds = split.map(|ty| {
        let ident = gen_ident(ty.trim());
        quote! { .add::<#ident>() }
    });

    quote! {
        #instance_ident.fn_table
        #(#adds)*
    }
    .into()
}
