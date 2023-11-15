use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, DeriveInput, Data, DataStruct, Fields, Field, FieldsNamed, FieldsUnnamed, DataEnum, Ident, Type, TypeTuple, TypePath, Path, PathSegment, PathArguments, AngleBracketedGenericArguments, GenericArgument, Token,
    punctuated::Punctuated,
    Result,
    parse::{Parse, ParseStream},
    LitInt,
};

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
        impl acttey::ecs::entity::Component for #ident {}
    }
    .into()
}

/// Derive macro generating an impl of the trait `Entity`.
/// You can't put types in that are not deriving `Component`.
///
/// # Examples
///
/// ```
/// # use acttey_ecs_macros::{Component, Entity};
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
/// # use acttey_ecs_macros::Entity;
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
// # use acttey_ecs_macros::ComponentEntity;
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
            let _: &dyn acttey::ecs::entity::Component = 
                &(x.unwrap()) as &dyn acttey::ecs::entity::Component;
        }
    });

    // Wraps the validate token with option.
    let optional_validate = check.then(|| quote! {
            // Analyzer will let us know when it's not implementing Component.
            #(#validate)*
            unreachable!("You can't call me.");
        });

    // Generates TokenStream informing Component types.
    let collect_types = field_types
        .iter()
        .map(|ty| quote! {
            collector.collect_type::<#ty>();
        });
    
    // Generates TokenStream inserting into `downcasters` field of the `EntityStorage`.
    let insert_downcasters = field_types
        .iter()
        .map(|ty| quote! {
            storage.downcasters.insert(
                acttey::ecs::ComponentKey::new(
                    std::any::TypeId::of::<#ty>(),
                    acttey::ecs::EntityKey::new(
                        std::any::TypeId::of::<#ident>()                       
                    )
                ),
                // Safety: function pointer is not null.
                unsafe {
                    std::ptr::NonNull::new_unchecked(T::downcast::<#ty> as *mut ())
                }
            );
        });

    // Generates TokenStream inserting into `borrows` field of the `EntityStorage`.
    let insert_borrows = field_types
        .iter()
        .map(|ty| quote! {
            storage.borrow_states.insert(
                acttey::ecs::ComponentKey::new(
                    std::any::TypeId::of::<#ty>(),
                    acttey::ecs::EntityKey::new(
                        std::any::TypeId::of::<#ident>()                       
                    )
                ),
                acttey::ecs::storage::BorrowState::Available
            );
        });

    // Generates TokenStream destructuring from normal struct.
    let optional_destructure_normal_struct = match kind {
        Kind::NormalStruct => Some(quote! { let #ident { #(mut #field_idents),* } = self }),
        _ => None,
    };

    // Generates TokenStream destructuring from tuple struct.
    let optional_destructure_tuple_struct = match kind {
        Kind::TupleStruct => Some(quote! { let #ident ( #(mut #field_idents),* ) = self}),
        _ => None,
    };

    // Generates TokenStream to move Components out at a specific position.
    let collect_items = field_types
        .iter()
        .zip(field_idents.iter())
        .map(|(ty, ident)| {
            quote! {
                collector.collect::<#ty>(key, &mut #ident);
            }
        });

    quote! {
        // Implements the trait Entity.
        impl acttey::ecs::entity::Entity for #ident {
            fn validate() {
                #optional_validate
            }

            fn notify_types<T>(
                collector: &mut T,
                storage: &mut acttey::ecs::storage::Storage,
            )
            where
                T: acttey::ecs::entity::CollectGeneric + 
                   acttey::ecs::entity::Downcast
            {
                // *Expand example*
                // collector.collect_type<CompA>();
                // collector.collect_type<CompB>();
                #(#collect_types)*
                
                #(#insert_downcasters)*
                
                #(#insert_borrows)*
            }

            fn moves(
                self,
                collector: &mut Box<dyn acttey::ecs::entity::Collect>,
                key: usize
            ) {
                collector.begin_collect(key);

                // *Expand example*
                // let EntA { a, b } = self;
                #optional_destructure_normal_struct;
                #optional_destructure_tuple_struct;

                // *Expand example*
                // collector.collect_item<CompA>(a, key);
                // collector.collect_item<CompB>(b, key);
                #(#collect_items)*

                collector.end_collect(key);
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
        DeriveInput {
            data:
                Data::Struct(
                    DataStruct {
                        fields: Fields::Named(..),
                        ..
                    },
                    ..,
                ),
            ..
        } => Kind::NormalStruct,
        // tuple struct
        DeriveInput {
            data:
                Data::Struct(
                    DataStruct {
                        fields: Fields::Unnamed(..),
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
fn get_fields(ast: &DeriveInput) -> Vec<Field> {
    let punctuated = match ast {
        // struct
        DeriveInput {
            data:
                Data::Struct(
                    DataStruct {
                        fields:
                            // Named (normal struct)
                            Fields::Named(FieldsNamed { named: fields, .. })
                            |
                            // Unnamed (tuple struct)
                            Fields::Unnamed(FieldsUnnamed {
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
        DeriveInput {
            data: Data::Enum(
                DataEnum {
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
fn get_idents(fields: &[Field]) -> Vec<Option<Ident>> {
    fields.iter().map(|field| field.ident.clone()).collect()
}

/// Tries to unwrap `Option<Ident>`s or put increasing letters starting from '_0'.
fn unwrap_idents(idents: Vec<Option<Ident>>) -> Vec<Ident> {
    let mut num = 0_u32;
    idents
        .into_iter()
        .map(|id| {
            id.unwrap_or_else(|| {
                let mut id = String::from('_');
                id.push_str(num.to_string().as_str());
                num += 1;
                Ident::new(
                    id.as_str(),
                    proc_macro2::Span::call_site(), /* No meanings */
                )
            })
        })
        .collect()
}

/// Extracts `Type`s out from the `Field`s.
fn get_types(fields: &[Field]) -> Vec<Type> {
    fields.iter().map(|field| field.ty.clone()).collect()
}

/// Tries to unwrap `Type`s recursively and returns unwrapped `Type`s.
/// Unwrap targets are
/// - tuple
/// - `Option`
fn unwrap_type(ty: &Type) -> Vec<Type> {
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
fn unwrap_type_tuple(ty: &Type) -> Vec<Type> {
    let mut res = vec![];
    if let Type::Tuple(TypeTuple { elems, .. }) = ty {
        for inner_ty in elems {
            res.extend(unwrap_type(inner_ty));
        }
    }
    res
}

/// Unwrap an `Option` of `Type` recursively and returns unwrapped `Type`s.
/// Returns empty vector when the input `Type` is not an `Option`.
fn unwrap_type_option(ty: &Type) -> Vec<Type> {
    let mut res = vec![];

    let segments = match ty {
        Type::Path(TypePath {
            path: Path { segments, .. },
            ..
        }) if segments.len() == 1 => segments,
        // Option starts from Path, so return immediately if it doesn't
        _ => {
            return res;
        }
    };

    let args = match &segments[0] {
        PathSegment {
            ident,
            arguments:
                PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }),
        } if ident == "Option" => args,
        // Not an option? Return
        _ => {
            return res;
        }
    };

    match &args[0] {
        GenericArgument::Type(inner_ty) => {
            res.extend(unwrap_type(inner_ty));
        }
        // Option must have generic argumnet within it
        _ => panic!("Bug! This shouldn't be called."),
    }

    res
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
    let jobs = (0..input.n).into_iter()
        .map(|i| {
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
