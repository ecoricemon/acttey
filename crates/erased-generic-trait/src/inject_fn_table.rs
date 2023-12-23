use crate::common::*;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::parse::Parser;
use syn::{
    parse_macro_input, parse_quote, Block, Field, Fields, FieldsNamed, Ident, ItemStruct,
    Signature, TraitItemFn, Type,
};

/// Injects function table fields for the generic methods,
/// which name is something like `fn_table_foo`, into the struct.
/// Also implements erased generic for the struct.
pub fn inject_fn_table(attr: TokenStream, item: TokenStream) -> TokenStream {
    // Makes each `TokenStream` corrensponding to generic method signatures.
    let attr = attr.to_string();
    let erased_name = attr
        .trim()
        .split(';')
        .next()
        .expect("Must put in the name of erased generic trait.");
    let attr_tokens = attr
        .split(';')
        .skip(1)
        .filter(|line| !line.is_empty())
        .map(|line| {
            let mut line = line.to_owned();
            line.push(';');
            line.parse::<TokenStream>().unwrap()
        })
        .collect::<Vec<_>>();

    // Generates function table field for each generic method.
    let mut st = parse_macro_input!(item as ItemStruct);
    let mut sigs = Vec::new();
    let mut builder_field_idents = Vec::new();
    let mut table_type_idents = Vec::new();
    let mut table_type_defines = Vec::new();
    for t in attr_tokens {
        let ast = parse_macro_input!(t as TraitItemFn);
        sigs.push(ast.sig.clone());
        if let Some((field_ident, table_type_ident, table_type_define)) = gen_field(ast, &st.ident)
        {
            builder_field_idents.push(field_ident);
            table_type_idents.push(table_type_ident);
            table_type_defines.push(table_type_define);
        }
    }

    // Implements erased generic for the struct.
    let erased_for_st = impl_erased_for_st(erased_name, &st.ident, &sigs);

    // Inserts new `fn_table` field into the struct.
    let st_fields = match &mut st.fields {
        Fields::Named(FieldsNamed { named, .. }) => named,
        _ => unimplemented!(),
    };
    let builder_ident = clone_ident_with_suffix(&st.ident, "FnTable");
    let fn_table_field = Field::parse_named
        .parse2(quote! {
            fn_table: #builder_ident
        })
        .unwrap();
    st_fields.push(fn_table_field);

    // Defines and implements `fn_table` builder.
    let fn_table_builder = impl_fn_table_builder(
        &st,
        &builder_ident,
        &builder_field_idents,
        &table_type_idents,
        &sigs,
    );

    quote! {
        #st
        #(#table_type_defines)*
        #erased_for_st
        #fn_table_builder
    }
    .into()
}

/// Generates function table field.
fn gen_field(ast: TraitItemFn, st_ident: &Ident) -> Option<(Ident, Ident, TokenStream2)> {
    // Nothing for non-generic method.
    is_generic(&ast.sig).then_some(0)?;

    // Gathers input and output types.
    let mut input_types: Vec<Type> = Vec::new();
    for arg in ast.sig.inputs {
        let (ident, _, mutability) = parse_arg(&arg);
        match (ident.to_string().as_str(), mutability) {
            ("Self", None) => input_types.push(parse_quote! { &#st_ident }),
            ("Self", Some(..)) => input_types.push(parse_quote! { &mut #st_ident }),
            _ => {
                let symbols = get_generic_symbols(&ast.sig.generics);
                let mut arg = arg.clone();
                change_arg_to_any(&mut arg, symbols.iter().map(|s| s.as_str()));
                let (_, ty, _) = parse_arg(&arg);
                input_types.push(ty.clone());
            }
        }
    }
    let output_type = ast.sig.output;

    // Makes a type alias for a function table field to be injected.
    let table_name = format!("fn_table_{}", ast.sig.ident);
    let table_name = camel_case(&table_name);
    let table_type_ident = gen_ident(&table_name);
    let table_type_define = quote! {
        type #table_type_ident = std::collections::HashMap<
            std::any::TypeId,
            std::boxed::Box<
                dyn std::ops::Fn(
                    #(#input_types),*
                ) #output_type
            >,
            ahash::RandomState
        >;
    };

    // TODO: Combinations of TypeId for multiple generics.
    let field_ident = ast.sig.ident.clone();

    Some((field_ident, table_type_ident, table_type_define))
}

/// Implements erased generic for the struct.
fn impl_erased_for_st(erased_name: &str, st_ident: &Ident, sigs: &[Signature]) -> TokenStream2 {
    let erased_ident = clone_ident_with_name(st_ident, erased_name.trim());

    let mut erased_sigs = sigs.to_owned();
    let mut is_generics = Vec::new();
    for sig in erased_sigs.iter_mut() {
        is_generics.push(is_generic(sig));
        modify_signature_to_erased(sig);
    }

    let mut blocks = Vec::new();
    for (i, &is_generic) in is_generics.iter().enumerate() {
        let sig = sigs.get(i).unwrap();
        let esig = erased_sigs.get(i).unwrap();
        let arg_idents = get_idents(&esig.inputs);

        // TODO: Combinations of TypeId for multiple generics.
        let sig_ident = &sig.ident;
        let block: Block = if is_generic {
            // Skips self and __type_id__.
            let arg_idents = arg_idents.iter().skip(2);
            parse_quote! {{
                let fn_table = self
                    .fn_table
                    .#sig_ident
                    .take()
                    .expect("fn_table must be filled.");
                let delegator = fn_table
                    .get(__type_id__)
                    .expect("fn_table doesn't have appropriate entry.");
                let ret = (delegator)(self, #(#arg_idents),*);
                self.fn_table.#sig_ident = Some(fn_table);
                ret
            }}
        } else {
            // Skips self.
            let arg_idents = arg_idents.iter().skip(1);
            parse_quote! {{
                self.#sig_ident(#(#arg_idents),*)
            }}
        };
        blocks.push(block);
    }

    quote! {
        impl #erased_ident for #st_ident {
            #(
                #[inline]
                #erased_sigs
                #blocks
            )*
        }
    }
}

/// Implements a function table builder for the struct.
fn impl_fn_table_builder(
    st: &ItemStruct,
    ident: &Ident,
    field_idents: &[Ident],
    field_type_idents: &[Ident],
    sigs: &[Signature],
) -> TokenStream2 {
    // Defines a function table builder.
    let vis = &st.vis;
    let builder = quote! {
        #vis struct #ident {
            #(
                #field_idents: std::option::Option<#field_type_idents>
            ),*
        }
    };

    // TODO: Currently, assumes that there must be only one kind of trait bound.
    // So, what's the first generic param?
    let common_generic = sigs
        .iter()
        .filter_map(|sig| get_nth_generic(sig, 0))
        .next()
        .expect("Expects only one generic parameter for now.");
    let common_generic_ident = gen_ident(&get_generic_symbol(common_generic));

    // Makes an iterator generating code of inserting entries into the table.
    let st_ident = &st.ident;
    let insert_blocks = sigs.iter().filter(|sig| is_generic(sig)).map(|sig| {
        let ident = &sig.ident;

        // Assumes that the first arg is &mut self.
        let symbols = get_generic_symbols(&sig.generics);
        let mut args = sig.inputs.clone();
        change_args_to_anys(args.iter_mut(), symbols.iter().map(|s| s.as_str()));
        let args = args.iter().skip(1);

        // Casts arguments with &dyn Any or &dyn mut Any types.
        let casted = args.clone().map(|arg| {
            let (ident, _, mutability) = parse_arg(arg);
            if is_any(arg) {
                if mutability.is_some() {
                    quote! { #ident.downcast_mut::<#common_generic_ident>().unwrap() }
                } else {
                    quote! { #ident.downcast_ref::<#common_generic_ident>().unwrap() }
                }
            } else {
                quote! { #ident }
            }
        });

        quote! {
            if let Some(map) = self.#ident.as_mut() {
                map.insert(
                    std::any::TypeId::of::<#common_generic_ident>(),
                    std::boxed::Box::new(|s: &mut #st_ident, #(#args),*| {
                        s.#ident::<#common_generic_ident>(#(#casted),*)
                    })
                );
            }
        }
    });

    // Implements the builder.
    let impl_builder = quote! {
        impl #ident {
            fn new() -> Self {
                Self {
                    #(
                        #field_idents: std::option::Option::Some(
                            #field_type_idents::default()
                        )
                    ),*
                }
            }

            fn with <#common_generic> (mut self) -> Self {
                self.add::<#common_generic_ident>();
                self
            }

            fn add <#common_generic> (&mut self) -> &mut Self {
                #(#insert_blocks)*
                self
            }
        }
    };

    quote! {
        #builder
        #impl_builder
    }
}
