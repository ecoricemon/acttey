use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Fields, ItemEnum};

#[proc_macro_derive(EnumObjectKey)]
pub fn derive_enum_object_key(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as ItemEnum);

    // Validates.
    assert_non_generic(&input);
    assert_unit_field_only(&input);

    // Implements `EnumObjectKey`.
    let ident = &input.ident;
    let var_idents = input.variants.iter().map(|variant| &variant.ident);
    let impl_enum_object_key = quote! {
        impl acttey::util::key::EnumObjectKey for #ident {
            fn set_to_global() -> Result<(), ()> {
                let mut guard = acttey::util::key::ObjectKeyMap::write().map_err(|_| ())?;
                #(
                    guard.insert(
                        #ident::#var_idents as u32,
                        std::sync::Arc::from(
                            concat!(stringify!(#ident), "::", stringify!(#var_idents))
                        )
                    );
                )*
                Ok(())
            }
        }
    };

    // Implements `From` for `u32`, then `IntoObjectKey` will also be implemented.
    let impl_from_for_u32 = quote! {
        impl From<#ident> for u32 {
            fn from(value: #ident) -> u32 {
                value as u32
            }
        }
    };

    TokenStream::from(quote! {
        #impl_enum_object_key
        #impl_from_for_u32
    })
}

fn assert_non_generic(item: &ItemEnum) {
    assert!(item.generics.params.is_empty(), "generic is now allowed");
}

fn assert_unit_field_only(item: &ItemEnum) {
    for var in item.variants.iter() {
        if var.fields != Fields::Unit {
            panic!("'{}' is not unit", var.ident);
        }
    }
}
