use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Component)]
pub fn derive_component(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let ident = ast.ident.clone();
    
    // TODO: Getting full path programitically, not hard coding
    let output = quote! {
        impl acttey::acttey_ecs::traits::Component for #ident {
            type Item = Self;
        }
    };

    output.into()
}