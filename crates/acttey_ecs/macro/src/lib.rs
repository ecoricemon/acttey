use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

// TODO: Getting proper path programmatically, not hard codining
// because path is varing as where we are accessing from.

enum Kind {
    NormalStruct,
    TupleStruct,
}

/// Derive macro generating an impl of the trait `Component`.
///
/// # Examples
///
/// ```
/// # use acttey_ecs_macro::Component;
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
        // Implements the trait Component.
        impl acttey_ecs_trait::Component for #ident {}
    }
    .into()
}

/// Derive macro generating an impl of the trait `Entity`.
/// You can't put types in that are not deriving `Component`.
///
/// # Examples
///
/// ```
/// # use acttey_ecs_macro::{Component, Entity};
/// # use acttey_struct;
///
/// #[derive(Component, Default)]
/// struct CompA;
///
/// #[derive(Component, Default)]
/// struct CompB;
///
/// #[derive(Entity, Default)]
/// struct EntA(CompA, CompB);
/// ```
///
/// ```compile_fail
/// # // `Entity` with not a `Component` shouldn't be compiled.
/// # use acttey_ecs_macro::Entity;
///
/// # struct CompA;
///
/// # #[derive(Entity)]
/// # struct EntA(CompA);
/// ```
#[proc_macro_derive(Entity)]
pub fn derive_entity(input: TokenStream) -> TokenStream {
    impl_entity(input, true)
}

// TODO: doctest
// Derive macro generating an impl of the both trait `Component` and trait `Entity`.
//
// # Examples
//
// ```
// # use acttey_ecs_macro::ComponentEntity;
//
// #[derive(ComponentEntity)]
// struct EntA;
//
// #[derive(ComponentEntity)]
// struct EntB(u8);
//
// #[derive(ComponentEntity)]
// struct EntC {
//     vel: (f32, f32, f32),
//     acc: (f32, f32, f32),
// }
// ```
#[proc_macro_derive(ComponentEntity)]
pub fn derive_component_entity(input: TokenStream) -> TokenStream {
    let mut output = derive_component(input.clone());
    output.extend(impl_entity(input, false));
    unimplemented!();
}

/// Implements the trait `Entity` and other traits.
/// Set check to true when you want to check if the struct has `Component`s only.
fn impl_entity(input: TokenStream, check: bool) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let ident = ast.ident.clone();
    let kind = get_kind(&ast);
    let fields = get_fields(&ast);
    let field_idents = unwrap_idents(get_idents(&fields));
    let field_types = get_types(&fields)
        .iter()
        .flat_map(unwrap_type)
        .collect::<Vec<_>>();

    // Generates TokenStream checking out each field's type
    // if they are implementing Component.
    let validate = field_types.iter().map(|ty| {
        quote! {
            let x: Option<#ty> = std::option::Option::None;
            let _: &dyn acttey_ecs_trait::Component = &(x.unwrap()) as &dyn acttey_ecs_trait::Component;
        }
    });

    // Wraps the validate token with option.
    let optional_validate = check.then(|| {
        quote! {
            // Analyzer will let us know when it's not implementing Component.
            #(#validate)*
            panic!("You can't call me.");
        }
    });

    // Generates TokenStream informing Component types.
    let collect_types = field_types.iter().map(|ty| {
        quote! {
            collector.collect_type::<#ty>();
        }
    });

    // Generates TokenStream destructuring from normal struct.
    let optional_destructure_normal_struct = match kind {
        Kind::NormalStruct => Some(quote! { let #ident { #(#field_idents),* } = self }),
        _ => None,
    };

    // Generates TokenStream destructuring from tuple struct.
    let optional_destructure_tuple_struct = match kind {
        Kind::TupleStruct => Some(quote! { let #ident ( #(#field_idents),* ) = self}),
        _ => None,
    };

    // Generates TokenStream to move Components out at a specific position.
    let collect_items = field_types
        .iter()
        .zip(field_idents.iter())
        .map(|(ty, ident)| {
            quote! {
                collector.collect_item::<#ty>(key, #ident);
            }
        });

    quote! {
        // Implements the trait Entity.
        impl acttey_ecs_trait::Entity for #ident {
            fn validate() {
                #optional_validate
            }
        }

        // Implements the trait acttey_struct::traits::Notify.
        impl acttey_struct::traits::Notify for #ident {
            fn notify_types(
                &self,
                collector: &mut impl acttey_struct::traits::Collect
            ) {
                collector.begin_collect_type();

                // *Expand example*
                // collector.collect_type<CompA>();
                // collector.collect_type<CompB>();
                #(#collect_types)*

                collector.end_collect_type();
            }

            fn moves(
                self,
                collector: &mut impl acttey_struct::traits::Collect,
                key: usize
            ) {
                collector.begin_collect_item(key);

                // *Expand example*
                // let EntA { a, b } = self;
                #optional_destructure_normal_struct;
                #optional_destructure_tuple_struct;

                // *Expand example*
                // collector.collect_item<CompA>(a, key);
                // collector.collect_item<CompB>(b, key);
                #(#collect_items)*

                collector.end_collect_item(key);
            }
        }
    }
    .into()
}

/// Returns a constant corresponding the input.
/// - KIND_NORMAL_STRUCT when the input is a normal struct.
/// - KINE_TUPLE_STRUCT when the input is a tuple struct.
fn get_kind(ast: &DeriveInput) -> Kind {
    match ast {
        // normal struct
        syn::DeriveInput {
            data:
                syn::Data::Struct(
                    syn::DataStruct {
                        fields: syn::Fields::Named(..),
                        ..
                    },
                    ..,
                ),
            ..
        } => Kind::NormalStruct,
        // tuple struct
        syn::DeriveInput {
            data:
                syn::Data::Struct(
                    syn::DataStruct {
                        fields: syn::Fields::Unnamed(..),
                        ..
                    },
                    ..,
                ),
            ..
        } => Kind::TupleStruct,
        _ => panic!("get_kind() can't process this type for now."),
    }
}

/// Extracts `Field`s out from the `DeriveInput`.
fn get_fields(ast: &DeriveInput) -> Vec<syn::Field> {
    let punctuated = match ast {
        // struct
        syn::DeriveInput {
            data:
                syn::Data::Struct(
                    syn::DataStruct {
                        fields:
                            // Named (normal struct)
                            syn::Fields::Named(syn::FieldsNamed { named: fields, .. })
                            |
                            // Unnamed (tuple struct)
                            syn::Fields::Unnamed(syn::FieldsUnnamed {
                                unnamed: fields, ..
                            })
                            ,
                        ..
                    },
                    ..,
                ),
            ..
        } => fields,
        // enum
        syn::DeriveInput {
            data: syn::Data::Enum(
                syn::DataEnum {
                    ..
                }
            ),
            ..
        } => unimplemented!("enum is not supported for now."),
        _ => {
            return vec![];
        },
    };
    punctuated.iter().cloned().collect()
}

/// Extracts `Ident`s out from the `Option<Ident>`s.
fn get_idents(fields: &[syn::Field]) -> Vec<Option<syn::Ident>> {
    fields.iter().map(|field| field.ident.clone()).collect()
}

/// Tries to unwrap `Option<Ident>`s or put increasing letters starting from '_0'.
fn unwrap_idents(idents: Vec<Option<syn::Ident>>) -> Vec<syn::Ident> {
    let mut num = 0_u32;
    idents
        .into_iter()
        .map(|id| {
            id.unwrap_or_else(|| {
                let mut id = String::from('_');
                id.push_str(num.to_string().as_str());
                num += 1;
                syn::Ident::new(
                    id.as_str(),
                    proc_macro2::Span::call_site(), /* No meanings */
                )
            })
        })
        .collect()
}

/// Extracts `Type`s out from the `Field`s.
fn get_types(fields: &[syn::Field]) -> Vec<syn::Type> {
    fields.iter().map(|field| field.ty.clone()).collect()
}

/// Tries to unwrap `Type`s recursively and returns unwrapped `Type`s.
/// Unwrap targets are
/// - tuple
/// - `Option`
fn unwrap_type(ty: &syn::Type) -> Vec<syn::Type> {
    let mut res = vec![];

    // Unwrap tuple
    res.extend(unwrap_type_tuple(ty));

    // Unwrap option
    res.extend(unwrap_type_option(ty));

    if res.is_empty() {
        res.push(ty.clone())
    }
    res
}

/// Unwrap a tuple of `Type`s recursively and returns unwrapped `Type`s.
/// Returns empty vector when the input `Type` is not a tuple.
fn unwrap_type_tuple(ty: &syn::Type) -> Vec<syn::Type> {
    let mut res = vec![];
    if let syn::Type::Tuple(syn::TypeTuple { elems, .. }) = ty {
        for inner_ty in elems {
            res.extend(unwrap_type(inner_ty));
        }
    }
    res
}

/// Unwrap an `Option` of `Type` recursively and returns unwrapped `Type`s.
/// Returns empty vector when the input `Type` is not an `Option`.
fn unwrap_type_option(ty: &syn::Type) -> Vec<syn::Type> {
    let mut res = vec![];

    let segments = match ty {
        syn::Type::Path(syn::TypePath {
            path: syn::Path { segments, .. },
            ..
        }) if segments.len() == 1 => segments,
        // Option starts from Path, so return immediately if it doesn't
        _ => {
            return res;
        }
    };

    let args = match &segments[0] {
        syn::PathSegment {
            ident,
            arguments:
                syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, .. }),
        } if ident == "Option" => args,
        // Not an option? Return
        _ => {
            return res;
        }
    };

    match &args[0] {
        syn::GenericArgument::Type(inner_ty) => {
            res.extend(unwrap_type(inner_ty));
        }
        // Option must have generic argumnet within it
        _ => panic!("Bug! This shouldn't be called."),
    }

    res
}
