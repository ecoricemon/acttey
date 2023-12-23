use crate::common::*;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{parse_macro_input, ItemTrait};
use syn::{Signature, TraitItem, TraitItemFn};

/// Generates a new trait without generic parameters.
/// Then implements input trait for the new trait object.
pub fn erase_generic(attr: TokenStream, item: TokenStream) -> TokenStream {
    let erased_name = attr.to_string();
    let src_trait = parse_macro_input!(item as ItemTrait);
    let mut erased_trait = src_trait.clone();

    // Makes new trait with the name of `erased_trait_name`.
    into_erased_generic(&mut erased_trait, erased_name.as_str());

    // Makes impl of dyn erased generic trait.
    let generic_for_dyn_erased = impl_generic_for_dyn_erased(&src_trait, &erased_trait);

    quote! {
        #src_trait
        #erased_trait
        #generic_for_dyn_erased
    }
    .into()
}

/// Makes generic methods become non-generic.
fn into_erased_generic(ast: &mut ItemTrait, new_name: &str) {
    // Modifies the trait name.
    modify_ident(&mut ast.ident, new_name);

    // Gets signatures.
    let sigs = get_signatures(ast);

    // Tries to change signatures.
    for sig in sigs {
        modify_signature_to_erased(sig);
    }
}

/// Gets `Signature`s from the `ItemTrait`.
fn get_signatures(ast: &mut ItemTrait) -> Vec<&mut Signature> {
    ast.items
        .iter_mut()
        .filter_map(|it| match it {
            TraitItem::Fn(TraitItemFn { sig, .. }) => Some(sig),
            _ => None,
        })
        .collect()
}

/// impl generic for dyn erased.
fn impl_generic_for_dyn_erased(src: &ItemTrait, erased: &ItemTrait) -> TokenStream2 {
    // Gets source trait name.
    let src_trait_ident = &src.ident;

    // Gets erased trait name.
    let erased_trait_ident = &erased.ident;

    // Gets source and erased trait method signatures.
    let mut src = src.clone();
    let mut src_temp = src.clone();
    let mut erased = erased.clone();
    let src_sigs = get_signatures(&mut src);
    let erased_sigs = get_signatures(&mut erased);

    // Gets erased method names.
    let erased_method_idents = erased_sigs.iter().map(|sig| &sig.ident);

    // Temporary source signatures now contain &TypeId parameters for easy comparison.
    let mut src_temp_sigs = get_signatures(&mut src_temp);
    for sig in src_temp_sigs.iter_mut() {
        if is_generic(sig) {
            inject_type_id(sig);
        }
    }

    // Makes method blocks.
    let mut preprocs = Vec::new();
    let mut args = Vec::new();
    let mut postprocs = Vec::new();
    for (src_sig, erased_sig) in src_temp_sigs.iter().zip(erased_sigs.iter()) {
        let (preproc, arg, postproc) = gen_block(src_sig, erased_sig);
        preprocs.push(preproc);
        args.push(arg);
        postprocs.push(postproc);
    }

    quote! {
        impl #src_trait_ident for dyn #erased_trait_ident {
            #(
                #[inline]
                #src_sigs {
                    #preprocs
                    self. #erased_method_idents ( #args )
                    #postprocs
                }
            )*
        }
    }
}

/// Generates preproc, args, and postproc codes in dyn erased method.
fn gen_block(
    src_sig: &Signature,
    erased_sig: &Signature,
) -> (TokenStream2, TokenStream2, TokenStream2) {
    if is_generic(src_sig) {
        gen_block_generic(src_sig, erased_sig)
    } else {
        gen_block_non_generic(src_sig)
    }
}

/// Generates preproc, args, and postproc codes in dyn erased generic method.
fn gen_block_generic(
    src_sig: &Signature,
    erased_sig: &Signature,
) -> (TokenStream2, TokenStream2, TokenStream2) {
    // let mut preprocs = Vec::new();
    let mut args = Vec::new();
    // let mut postprocs = Vec::new();

    // Assumes that there's only one generic symbol.
    let generic_ident = &src_sig.generics.type_params().next().unwrap().ident;

    for (src_arg, erased_arg) in src_sig.inputs.iter().zip(erased_sig.inputs.iter()).skip(1) {
        let src_arg_ident = get_ident(src_arg);
        let src_arg_str = src_arg_ident.to_string();

        // Injected &TypeId? => Adds TypeId::of::<T>() argument.
        if src_arg_str == "__type_id__" {
            args.push(quote! { &std::any::TypeId::of::<#generic_ident>() });
        }
        // Generic?
        else if src_arg != erased_arg {
            let (_, ty, mutability) = parse_arg(src_arg);
            match (is_ref(ty), mutability) {
                // Generic mutable reference? => Adds casted argument.
                (true, Some(_)) => args.push(quote! { #src_arg_ident as &mut dyn std::any::Any }),
                // Generic reference? => Adds casted argument.
                (true, None) => args.push(quote! { #src_arg_ident as &dyn std::any::Any }),
                // Moved generic? => Looks not good to implement.
                _ => {
                    unimplemented!()
                }
            }
        }
        // Not a generic argument? => No manipulation.
        else {
            args.push(quote! { #src_arg_ident });
        }
    }

    (quote! {}, quote! { #(#args),* }, quote! {})
}

/// Generates preproc, args, and postproc codes in dyn erased non generic method.
fn gen_block_non_generic(src_sig: &Signature) -> (TokenStream2, TokenStream2, TokenStream2) {
    let arg_idents = src_sig.inputs.iter().skip(1).map(get_ident);

    (quote! {}, quote! { #(#arg_idents),* }, quote! {})
}
