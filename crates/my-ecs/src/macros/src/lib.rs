use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    Data, DeriveInput, Ident, LitInt, Path, Result, Token,
};

// TODO: Getting proper path programmatically, not hard codining
// because path is varing as where we are accessing from.

/// Derive macro generating an impl of the trait `Component`.
///
/// # Examples
///
/// ```
/// # use my_ecs_macros::Component;
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

    TokenStream::from(quote! {
        // Implements `Component` trait.
        impl my_ecs::ecs::component::Component for #ident {}
    })
}

/// # Examples
///
/// ```
/// # use my_ecs_macros::{Component, Entity};
///
/// #[derive(Component)]
/// struct CompA;
///
/// #[derive(Entity)]
/// #[entity_hasher(std::hash::RandomState)]
/// struct EntA {
///     a: CompA,
/// }
/// ```
#[proc_macro_derive(Entity, attributes(entity_hasher))]
pub fn derive_entity(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = input.ident.clone();
    let ident_str = ident.to_string();

    let (field_idents, field_types): (Vec<_>, Vec<_>) = match input.data {
        Data::Struct(data_struct) => data_struct
            .fields
            .iter()
            .map(|field| (field.ident.clone(), field.ty.clone()))
            .unzip(),
        _ => panic!("only struct is allowed for this macro"),
    };

    // Validates that all fields implement `Compoenent` trait.
    let validate_impl_component = quote! {
        const _: () = {
            const fn validate<T: my_ecs::ecs::component::Component>() {}
            #(
                validate::<#field_types>();
            )*
        };
    };

    // Determines hasher for `EntityDesc`.
    let hasher = input
        .attrs
        .iter()
        .filter_map(|attr| {
            if attr.path().is_ident("entity_hasher") {
                let ty: Path = attr.parse_args().unwrap();
                Some(quote! { #ty })
            } else {
                None
            }
        })
        .next()
        .unwrap_or(quote! { std::hash::RandomState });

    // Implements `AsEntityDesc` trait.
    let impl_as_entity_desc = quote! {
        impl my_ecs::ecs::entity::AsEntityDesc for #ident {
            fn as_entity_descriptor() -> my_ecs::ecs::entity::EntityDesc {
                let mut desc =
                    my_ecs::ecs::entity::EntityDesc::new_with_default_container::<#hasher>(
                    #ident_str.into(),
                    Some(my_ecs::ecs::entity::EntityKeyType::new(std::any::TypeId::of::<#ident>())),
                );
                #(
                    desc.add_component(my_ecs::tinfo!(#field_types));
                )*
                desc
            }
        }
    };

    // Implements `Entity` trait.
    let num_fields = field_types.len();
    let col_indices = 0..num_fields;
    let impl_as_entity = quote! {
        impl my_ecs::ecs::entity::Entity for #ident {
            fn move_to<T: my_ecs::ecs::entity::AddEntity + ?Sized>(mut self, cont: &mut T) -> usize {
                cont.begin_add_row();

                #(
                    // Safety: Infallible.
                    unsafe {
                        cont.add_value(
                            #col_indices,
                            std::ptr::NonNull::new_unchecked(
                                (&mut self.#field_idents as *mut _ as *mut u8)
                            )
                        );
                    }
                )*

                std::mem::forget(self);

                cont.end_add_row()
            }
        }
    };

    TokenStream::from(quote! {
        #validate_impl_component
        #impl_as_entity_desc
        #impl_as_entity
    })
}

#[proc_macro_derive(Resource)]
pub fn derive_resource(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let ident = ast.ident.clone();

    TokenStream::from(quote! {
        // Implements the trait `Resource`.
        impl my_ecs::ecs::resource::Resource for #ident {}
    })
}

#[proc_macro]
pub fn nth(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Nth);
    let identifiers = input.identifiers.into_iter().collect::<Vec<_>>();
    if input.n < identifiers.len() {
        let ident = &identifiers[input.n];
        TokenStream::from(quote! { #ident })
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

    TokenStream::from(quote! { #(#jobs)* })
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
