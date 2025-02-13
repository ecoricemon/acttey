use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use proc_macro2::TokenTree as TokenTree2;
use quote::{quote, ToTokens, TokenStreamExt};
use std::iter;
use syn::token::Comma;
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    spanned::Spanned,
    token, Data, DeriveInput, Error, Expr, ExprRange, Ident, Index, Lit, LitInt, Path, RangeLimits,
    Result, Token, Type, TypePath, Visibility,
};

/// Implements [`Component`] for the type.
///
/// # Examples
///
/// ```ignore
/// use my_ecs::prelude::*;
///
/// // Zero-sized marker component.
/// #[derive(Component)]
/// struct Ca;
///
/// #[derive(Component)]
/// struct Cb(u8);
///
/// #[derive(Component)]
/// struct Cc {
///     vel: (f32, f32, f32),
///     acc: (f32, f32, f32),
/// }
/// ```
///
/// [`Component`]: ./trait.Component.html
//
// TEST: my-ecs/src/lib.rs: tests::test_my_ecs_macros_doc_derive_component
#[proc_macro_derive(Component)]
pub fn derive_component(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let ident = ast.ident.clone();

    TokenStream::from(quote! {
        // Implements `Component` trait.
        impl my_ecs::prelude::Component for #ident {
            const IS_DEFAULT: bool
                = my_ecs::ds::TypeHelper::<#ident>::IS_DEFAULT;
            const FN_DEFAULT: my_ecs::ds::FnDefaultRaw
                = my_ecs::ds::TypeHelper::<#ident>::FN_DEFAULT;
            const IS_CLONE: bool
                = my_ecs::ds::TypeHelper::<#ident>::IS_CLONE;
            const FN_CLONE: my_ecs::ds::FnCloneRaw
                = my_ecs::ds::TypeHelper::<#ident>::FN_CLONE;
        }
    })
}

/// Impelments [`Entity`] for the type.
///
/// Actually, you don't have to define entity type explicitly, but by doing so,
/// the crate becomes to be able to provide easy-to-use functions for you.
///
/// You can designate which container type you use as well by attributes
/// `container` and `random_state`.
///
/// `container` means which container type you use for the entity. You can use
/// your own types or choose one of built-in types shown below.
/// * [`SparseSet`] - Default
/// * [`ChunkSparseSet`]
///
/// `random_state` means a state to make a hasher. [`std::hash::RandomState`] is
/// default.
///
/// # Examples
///
/// ```ignore
/// use my_ecs::prelude::*;
///
/// #[derive(Component)]
/// struct Ca;
///
/// #[derive(Entity)]
/// struct Ea {
///     a: Ca,
/// }
///
/// // Or, you can customize entity container.
/// #[derive(Entity)]
/// #[container(ChunkSparseSet)]
/// #[random_state(std::hash::RandomState)]
/// struct Eb {
///     a: Ca,
/// }
/// ```
///
/// [`Entity`]: ./trait.Entity.html
/// [`SparseSet`]: ./struct.SparseSet.html
/// [`ChunkSparseSet`]: ./struct.ChunkSparseSet.html
//
// TEST: my-ecs/src/lib.rs: tests::test_my_ecs_macros_doc_derive_entity
#[proc_macro_derive(Entity, attributes(container, random_state))]
pub fn derive_entity(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let vis = input.vis.clone();
    let ident = input.ident.clone();
    let ident_str = ident.to_string();

    let (field_idents, field_types): (Vec<_>, Vec<_>) = match input.data {
        Data::Struct(data_struct) => data_struct
            .fields
            .iter()
            .map(|field| (field.ident.clone().unwrap(), field.ty.clone()))
            .unzip(),
        _ => panic!("only struct is allowed for this macro"),
    };
    let field_ident_strs = field_idents
        .iter()
        .map(|ident| ident.to_string())
        .collect::<Vec<_>>();

    // Validates that all fields implement `Component` trait.
    let validate_impl_component = quote! {
        const _: () = {
            const fn validate<T: my_ecs::prelude::Component>() {}
            #(
                validate::<#field_types>();
            )*
        };
    };

    // Determines container and hasher builder for `EntityReg`.
    let container = input
        .attrs
        .iter()
        .filter_map(|attr| {
            if attr.path().is_ident("container") {
                let ty: Path = attr.parse_args().unwrap();
                Some(quote! { #ty })
            } else {
                None
            }
        })
        .next()
        .unwrap_or(quote! { SparseSet });
    let random_state = input
        .attrs
        .iter()
        .filter_map(|attr| {
            if attr.path().is_ident("random_state") {
                let ty: Path = attr.parse_args().unwrap();
                Some(quote! { #ty })
            } else {
                None
            }
        })
        .next()
        .unwrap_or(quote! { std::hash::RandomState });

    // Implements `AsEntityReg` trait.
    let impl_as_entity_ref = quote! {
        impl my_ecs::prelude::AsEntityReg for #ident {
            fn entity_descriptor() -> my_ecs::prelude::EntityReg {
                let name = my_ecs::prelude::EntityName::new(
                    #ident_str.into()
                );
                let cont = Box::new(
                    my_ecs::prelude::#container::<#random_state>::new()
                );
                let mut desc = my_ecs::prelude::EntityReg::new(
                    Some(name), cont
                );
                #(
                    desc.add_component(my_ecs::tinfo!(#field_types));
                )*
                desc
            }
        }
    };

    // Implements `Components` trait.
    let num_fields = field_types.len();
    let impl_components = quote! {
        impl my_ecs::prelude::Components for #ident {
            type Keys = [my_ecs::prelude::ComponentKey; #num_fields];
            type Infos = [my_ecs::ds::TypeInfo; #num_fields];

            const LEN: usize = #num_fields;

            fn keys() -> Self::Keys {
                [#(
                    <#field_types as my_ecs::prelude::Component>::key()
                ),*]
            }

            fn infos() -> Self::Infos {
                [#(
                    <#field_types as my_ecs::prelude::Component>::type_info()
                ),*]
            }

            fn sorted_keys() -> Self::Keys {
                let mut keys = [#(
                    <#field_types as my_ecs::prelude::Component>::key()
                ),*];
                keys.sort_unstable();
                keys
            }
        }
    };

    // Declares `Entity::Ref` type.
    let ref_ident = Ident::new(&format!("{}__Ref", ident_str), ident.span());
    let decl_entity_ref = quote! {
        #[allow(non_camel_case_types)]
        #vis struct #ref_ident<'cont> {
            #(
                #vis #field_idents: &'cont #field_types
            ),*
        }
    };

    // Declares `Entity::Mut` type.
    let mut_ident = Ident::new(&format!("{}__Mut", ident_str), ident.span());
    let decl_entity_mut = quote! {
        #[allow(non_camel_case_types)]
        #vis struct #mut_ident<'cont> {
            #(
                #vis #field_idents: &'cont mut #field_types
            ),*
        }
    };

    // Implements `Debug` for `Entity::Ref` or `Entity::Mut`.
    fn create_entity_ref_or_mut_impl_debug<'a>(
        ident_str: &str,
        ref_ident: &Ident,
        field_idents: &'a [Ident],
        field_types: &'a [Type],
        field_ident_strs: &'a [String],
    ) -> TokenStream2 {
        quote! {
            impl<'cont> std::fmt::Debug for #ref_ident<'cont> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                    let mut s = f.debug_struct(#ident_str);
                    let mut is_full = true;

                    #(
                        if my_ecs::ds::TypeHelper::<#field_types>::IS_DEBUG {
                            let helper = my_ecs::ds::DebugHelper {
                                f: my_ecs::ds::TypeHelper::<#field_types>::FN_FMT,
                                ptr: self.#field_idents as *const #field_types as *const u8,
                            };
                            s.field(#field_ident_strs, &helper);
                        } else {
                            is_full = false;
                        }
                    )*

                    if is_full {
                        s.finish()
                    } else {
                        s.finish_non_exhaustive()
                    }
                }
            }
        }
    }
    let impl_debug_for_entity_ref = create_entity_ref_or_mut_impl_debug(
        &ident_str,
        &ref_ident,
        &field_idents,
        &field_types,
        &field_ident_strs,
    );
    let impl_debug_for_entity_mut = create_entity_ref_or_mut_impl_debug(
        &ident_str,
        &mut_ident,
        &field_idents,
        &field_types,
        &field_ident_strs,
    );

    // Implements `Entity` trait.
    let col_idxs = (0..field_idents.len()).collect::<Vec<_>>();
    let impl_entity = quote! {
        impl my_ecs::prelude::Entity for #ident {
            type Ref<'cont> = #ref_ident<'cont>;
            type Mut<'cont> = #mut_ident<'cont>;

            const OFFSETS_BY_FIELD_INDEX: &'static [usize] = &[
                #(
                    std::mem::offset_of!(#ident, #field_idents)
                ),*
            ];

            fn field_to_column_index(fi: usize) -> usize {
                use std::{sync::OnceLock, any::TypeId};

                static MAP: OnceLock<[usize; #num_fields]> = OnceLock::new();

                let map = MAP.get_or_init(|| {
                    let mut map = [0; #num_fields];

                    let decl = [ #( TypeId::of::<#field_types>() ),* ];
                    let mut sorted = decl.clone();
                    sorted.sort_unstable();

                    for i in 0..#num_fields {
                        for j in 0..#num_fields {
                            if decl[i] == sorted[j] {
                                map[i] = j;
                                break;
                            }
                        }
                    }

                    map
                });

                map[fi]
            }

            fn get_ref_from<Cont: my_ecs::prelude::ContainEntity + ?Sized>(
                cont: &Cont, vi: usize
            ) -> Self::Ref<'_> {
                unsafe { #ref_ident {
                    #(
                        #field_idents:
                            // NonNull<u8>
                            cont.value_ptr_by_value_index(
                                Self::field_to_column_index(#col_idxs),
                                vi
                            ).unwrap()
                            // NonNull<u8> -> NonNull<field_type>
                            .cast::<#field_types>()
                            // NonNull<field_type> -> &field_type
                            .as_ref()
                    ),*
                } }
            }

            fn get_mut_from<Cont: my_ecs::prelude::ContainEntity + ?Sized>(
                cont: &mut Cont, vi: usize
            ) -> Self::Mut<'_> {
                unsafe { #mut_ident {
                    #(
                        #field_idents:
                            // NonNull<u8>
                            cont.value_ptr_by_value_index(
                                Self::field_to_column_index(#col_idxs),
                                vi
                            ).unwrap()
                            // NonNull<u8> -> NonNull<field_type>
                            .cast::<#field_types>()
                            // NonNull<field_type> -> &mut field_type
                            .as_mut()
                    ),*
                } }
            }
        }

        #decl_entity_ref
        #decl_entity_mut
        #impl_debug_for_entity_ref
        #impl_debug_for_entity_mut
    };

    TokenStream::from(quote! {
        #validate_impl_component
        #impl_as_entity_ref
        #impl_components
        #impl_entity
    })
}

/// Implements [`Resource`] for the type.
///
/// # Examples
///
/// ```ignore
/// use my_ecs::prelude::*;
///
/// #[derive(Resource)]
/// struct R(i32);
/// ```
///
/// [`Resource`]: ./trait.Resource.html
//
// TEST: my-ecs/src/lib.rs: tests::test_my_ecs_macros_doc_derive_resource
#[proc_macro_derive(Resource)]
pub fn derive_resource(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let ident = ast.ident.clone();

    TokenStream::from(quote! {
        // Implements the trait `Resource`.
        impl my_ecs::prelude::Resource for #ident {}
    })
}

/// Implements [`Filter`] for the type, and implements [`Select`] optionally
/// if `Target` is defined.
///
/// Types implementing `Filter` only can be used in [`EntWrite`] only. Types
/// implementing both `Filter` and `Select`, on the other hand, also can be used
/// in [`Read`] and [`Write`] as well. Because `Read` and `Write` mean
/// requesting read or write access to a specific *target* component.
///
/// See [`Filter`] and [`Select`] for more details.
///
/// # Examples
///
/// ```ignore
/// use my_ecs::prelude::*;
///
/// #[derive(Component)] struct Ca;
/// #[derive(Component)] struct Cb;
/// #[derive(Component)] struct Cc;
/// #[derive(Component)] struct Cd;
/// #[derive(Component)] struct Ce;
///
/// // Declares `Fa` with an implemenation of `Filter`.
/// filter!(Fa, All = Ca);
///
/// // Declares `Fb` with an implemenation of `Filter`.
/// filter!(Fb, All = Ca, Any = Cb, None = Cc);
///
/// // Declares `Fc` with an implementation of `Filter` and `Select`.
/// filter!(Fc, Target = Ca);
///
/// // Declares `Fd` with an implementation of `Filter` and `Select`.
/// filter!(Fd, Target = Ca, All = Cb, Any = Cc, None = (Cd, Ce));
///
/// // All types implement `Filter` which means they can be used in
/// // `EntWrite`.
/// fn system_a(ew: EntWrite<(Fa, Fb, Fc, Fd)>) { /* ... */ }
///
/// // On the other hand, `Fc` and `Fd` can be used in `Read` and `Write`
/// // because they implement `Select` too.
/// fn system_b(r: Read<Fc>, w: Write<Fd>) { /* ... */ }
/// ```
///
/// [`Filter`]: ./trait.Filter.html
/// [`Select`]: ./trait.Select.html
/// [`EntWrite`]: ./struct.EntWrite.html
/// [`Read`]: ./struct.Read.html
/// [`Write`]: ./struct.Write.html
//
// TEST: my-ecs/src/lib.rs: tests::test_my_ecs_macros_doc_filter
#[proc_macro]
pub fn filter(input: TokenStream) -> TokenStream {
    let sel = parse_macro_input!(input as Select);

    // Validates if the Filter types implement `Component`.
    let empty = Punctuated::<TypePath, Token![,]>::new();
    let all = get_iter(&sel.filter.all, &empty);
    let any = get_iter(&sel.filter.any, &empty);
    let none = get_iter(&sel.filter.none, &empty);

    // Validates that `Target`, `All` and `Any` doesn't overlap `None`.
    let validate_non_overlap = if let Some(target) = &sel.target {
        let pos = iter::once(&target.ty).chain(all.clone()).chain(any.clone());
        validate_non_overlap_tokens(pos, none.clone())
    } else {
        let pos = all.clone().chain(any.clone());
        validate_non_overlap_tokens(pos, none.clone())
    };

    // The same purpose of code above.
    // This gives more specific position where the error occurs.
    // However, this cannot detect something like as follows
    // Target = Ca, None = crate::Ca
    if let Some(target) = &sel.target {
        let mut pos = iter::once(&target.ty).chain(all).chain(any);
        if let Some(conflict) = none.clone().find(|&n| pos.any(|p| p == n)) {
            let err = Error::new(conflict.span(), "conflicts").into_compile_error();
            return TokenStream::from(err);
        }
    } else {
        let mut pos = all.chain(any);
        if let Some(conflict) = none.clone().find(|&n| pos.any(|p| p == n)) {
            let err = Error::new(conflict.span(), "conflicts").into_compile_error();
            return TokenStream::from(err);
        }
    }

    return TokenStream::from(quote! {
        #validate_non_overlap
        #sel
    });

    // === Internal helper functions ===

    fn get_iter<'a>(
        x: &'a Option<(Token![,], Ident, Token![=], Types)>,
        empty: &'a Punctuated<TypePath, Token![,]>,
    ) -> syn::punctuated::Iter<'a, TypePath> {
        if let Some((_, _, _, list)) = x {
            list.types.iter()
        } else {
            empty.iter()
        }
    }

    fn validate_non_overlap_tokens<'a, 'b, Ia, Ib>(ia: Ia, ib: Ib) -> TokenStream2
    where
        Ia: Iterator<Item = &'a TypePath> + Clone,
        Ib: Iterator<Item = &'b TypePath> + Clone,
    {
        let pairs = ia.flat_map(|a| ib.clone().map(move |b| (a, b)));
        let pair_as = pairs.clone().map(|(a, _)| a);
        let pair_bs = pairs.map(|(_, b)| b);

        quote! {
            const _: () = {#(
                assert!(
                    !my_ecs::ds::TypeHelper::<(#pair_as, #pair_bs)>::IS_EQUAL_TYPE,
                    "Types in `Target`, `All`, and `Any` must not be included in `None`",
                );
            )*};
        }
    }
}

/// Implements [`Request`] for the type.
///
/// Functions implement the `Request` by the crate internally, but others such
/// as struct or enum don't. You must implement the `Request` yourself if you
/// want it to act as a system. This macro helps you write just a little bit of
/// code for that.
///
/// # Examples
///
/// ```ignore
/// use my_ecs::prelude::*;
///
/// #[derive(Component)] struct Ca;
/// #[derive(Component)] struct Cb;
/// #[derive(Resource)] struct Ra(i32);
/// #[derive(Resource)] struct Rb(i32);
///
/// filter!(Fa, Target = Ca);
/// filter!(Fb, Target = Cb);
/// filter!(Fc, All = (Ca, Cb));
///
/// // Declares `Req` with an implementation of `Request`.
/// // You can omit Read, Write, ResRead, ResWrite, or EntWrite.
/// request!(Req, Read = Fa, Write = Fb, ResRead = Ra, ResWrite = Rb, EntWrite = Fc);
///
/// struct Sys {
///     data: String,
/// }
///
/// impl System for Sys {
///     type Request = Req;
///     fn run(&mut self, resp: Response<'_, Self::Request>) { /* ... */ }
/// }
/// ```
///
/// [`Request`]: ./trait.Request.html
//
// TEST: my-ecs/src/lib.rs: tests::test_my_ecs_macros_doc_request
#[proc_macro]
pub fn request(input: TokenStream) -> TokenStream {
    let req = parse_macro_input!(input as Request);

    TokenStream::from(quote! { #req })
}

#[derive(Debug)]
struct Request {
    vis: Visibility,
    ident: Ident,
    read: Option<(Token![,], Ident, Token![=], Types)>,
    write: Option<(Token![,], Ident, Token![=], Types)>,
    res_read: Option<(Token![,], Ident, Token![=], Types)>,
    res_write: Option<(Token![,], Ident, Token![=], Types)>,
    ent_write: Option<(Token![,], Ident, Token![=], Types)>,
}

impl Parse for Request {
    fn parse(input: ParseStream) -> Result<Self> {
        let vis = input.parse()?;
        let ident = input.parse()?;

        let mut read = None;
        let mut write = None;
        let mut res_read = None;
        let mut res_write = None;
        let mut ent_write = None;
        while input.peek(Token![,]) || input.peek(Ident) {
            let comma: Token![,] = if input.peek(Token![,]) {
                input.parse()?
            } else {
                Comma::default()
            };
            let ident: Ident = input.parse()?;
            let ident_str = ident.to_string();
            match ident_str.to_ascii_lowercase().as_str() {
                "read" => {
                    if read.is_some() {
                        return Err(Error::new(ident.span(), "duplicate `Read`"));
                    }
                    let eq: Token![=] = input.parse()?;
                    let types: Types = input.parse()?;
                    read = Some((comma, ident, eq, types));
                }
                "write" => {
                    if write.is_some() {
                        return Err(Error::new(ident.span(), "duplicate `Write`"));
                    }
                    let eq: Token![=] = input.parse()?;
                    let types: Types = input.parse()?;
                    write = Some((comma, ident, eq, types));
                }
                "resread" => {
                    if res_read.is_some() {
                        return Err(Error::new(ident.span(), "duplicate `ResRead`"));
                    }
                    let eq: Token![=] = input.parse()?;
                    let types: Types = input.parse()?;
                    res_read = Some((comma, ident, eq, types));
                }
                "reswrite" => {
                    if res_write.is_some() {
                        return Err(Error::new(ident.span(), "duplicate `ResWrite`"));
                    }
                    let eq: Token![=] = input.parse()?;
                    let types: Types = input.parse()?;
                    res_write = Some((comma, ident, eq, types));
                }
                "entwrite" => {
                    if ent_write.is_some() {
                        return Err(Error::new(ident.span(), "duplicate `EntWrite`"));
                    }
                    let eq: Token![=] = input.parse()?;
                    let types: Types = input.parse()?;
                    ent_write = Some((comma, ident, eq, types));
                }
                _ => {
                    return Err(Error::new(
                        ident.span(),
                        "expected `Read`, `Write`, `ResRead`, `ResWrite`, or `EntWrite`",
                    ));
                }
            }
        }

        Ok(Self {
            vis,
            ident,
            read,
            write,
            res_read,
            res_write,
            ent_write,
        })
    }
}

impl ToTokens for Request {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let vis = &self.vis;
        let ident = &self.ident;
        let read = helper(&self.read);
        let write = helper(&self.write);
        let res_read = helper(&self.res_read);
        let res_write = helper(&self.res_write);
        let ent_write = helper(&self.ent_write);

        // Declares the struct.
        tokens.append_all(quote! {
            #vis struct #ident;
        });

        // Implements `Request` for the struct.
        tokens.append_all(quote! {
            impl my_ecs::prelude::Request for #ident {
                type Read = #read;
                type Write = #write;
                type ResRead = #res_read;
                type ResWrite = #res_write;
                type EntWrite = #ent_write;
            }
        });

        // === Internal helper functions ===

        fn helper(x: &Option<(Token![,], Ident, Token![=], Types)>) -> TokenStream2 {
            if let Some((_, _, _, types)) = x {
                let types = &types.types;
                if types.len() == 1 {
                    quote! { #types }
                } else {
                    quote! {( #types )}
                }
            } else {
                quote! {()}
            }
        }
    }
}

#[derive(Debug)]
struct Select {
    vis: Visibility,
    ident: Ident,
    _comma: Token![,],
    target: Option<SelectTarget>,
    filter: Filter,
}

impl Parse for Select {
    fn parse(input: ParseStream) -> Result<Self> {
        let vis = input.parse()?;
        let ident = input.parse()?;
        let _comma = input.parse()?;

        let contains_target = input
            .step(|cursor| {
                if let Some((tt, next)) = cursor.token_tree() {
                    match &tt {
                        TokenTree2::Ident(ident) if &ident.to_string() == "Target" => {
                            Ok(((), next))
                        }
                        _ => Err(cursor.error("")),
                    }
                } else {
                    Err(cursor.error(""))
                }
            })
            .is_ok();

        let (target, filter) = if contains_target {
            let target = input.parse().ok();
            let filter = input.parse()?;
            (target, filter)
        } else {
            let filter = input.parse()?;
            (None, filter)
        };

        Ok(Self {
            vis,
            ident,
            _comma,
            target,
            filter,
        })
    }
}

impl ToTokens for Select {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let vis = &self.vis;
        let ident = &self.ident;
        let all = helper(&self.filter.all);
        let any = helper(&self.filter.any);
        let none = helper(&self.filter.none);
        let exact = helper(&self.filter.exact);

        // Declares the struct.
        tokens.append_all(quote! {
            #vis struct #ident;
        });

        // Implements `Filter` for the struct.
        tokens.append_all(quote! {
            impl my_ecs::prelude::Filter for #ident {
                type All = #all;
                type Any = #any;
                type None = #none;
                type Exact = #exact;
            }
        });

        // Implements `Select` for the struct if needed.
        if let Some(target) = &self.target {
            tokens.append_all(quote! {
                impl my_ecs::prelude::Select for #ident {
                    type Target = #target;
                    type Filter = #ident;
                }
            });
        }

        // === Internal helper functions ===

        fn helper(x: &Option<(Token![,], Ident, Token![=], Types)>) -> TokenStream2 {
            if let Some((_, _, _, types)) = x {
                let types = &types.types;
                if types.len() == 1 {
                    quote! { #types }
                } else {
                    quote! {( #types )}
                }
            } else {
                quote! {()}
            }
        }
    }
}

#[derive(Debug)]
struct SelectTarget {
    _eq: Token![=],
    ty: TypePath,
}

impl Parse for SelectTarget {
    fn parse(input: ParseStream) -> Result<Self> {
        let eq: Token![=] = input.parse()?;
        let ty: TypePath = input.parse()?;

        Ok(Self { _eq: eq, ty })
    }
}

impl ToTokens for SelectTarget {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let ty = &self.ty;
        let ty = quote! { #ty };
        tokens.append_all(ty);
    }
}

#[derive(Debug)]
struct Filter {
    all: Option<(Token![,], Ident, Token![=], Types)>,
    any: Option<(Token![,], Ident, Token![=], Types)>,
    none: Option<(Token![,], Ident, Token![=], Types)>,
    exact: Option<(Token![,], Ident, Token![=], Types)>,
}

impl Parse for Filter {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut all = None;
        let mut any = None;
        let mut none = None;
        let mut exact = None;
        while input.peek(Token![,]) || input.peek(Ident) {
            let comma: Token![,] = if input.peek(Token![,]) {
                input.parse()?
            } else {
                Comma::default()
            };
            let ident: Ident = input.parse()?;
            let ident_str = ident.to_string();
            match ident_str.to_ascii_lowercase().as_str() {
                "all" => {
                    if all.is_some() {
                        return Err(Error::new(ident.span(), "duplicate `All`"));
                    }
                    let eq: Token![=] = input.parse()?;
                    let types: Types = input.parse()?;
                    all = Some((comma, ident, eq, types));
                }
                "any" => {
                    if any.is_some() {
                        return Err(Error::new(ident.span(), "duplicate `Any`"));
                    }
                    let eq: Token![=] = input.parse()?;
                    let types: Types = input.parse()?;
                    any = Some((comma, ident, eq, types));
                }
                "none" => {
                    if none.is_some() {
                        return Err(Error::new(ident.span(), "duplicate `None`"));
                    }
                    let eq: Token![=] = input.parse()?;
                    let types: Types = input.parse()?;
                    none = Some((comma, ident, eq, types));
                }
                "exact" => {
                    if exact.is_some() {
                        return Err(Error::new(ident.span(), "duplicate `Exact`"));
                    }
                    let eq: Token![=] = input.parse()?;
                    let types: Types = input.parse()?;
                    exact = Some((comma, ident, eq, types));
                }
                _ => {
                    return Err(Error::new(
                        ident.span(),
                        "expected `All`, `Any`, `None`, or `Exact`",
                    ));
                }
            }
        }

        if exact.is_some() && (all.is_some() || any.is_some() || none.is_some()) {
            return Err(Error::new(
                input.span(),
                "`Exact` cannot be with `All`, `Any`, or `None`",
            ));
        }

        Ok(Self {
            all,
            any,
            none,
            exact,
        })
    }
}

#[derive(Debug)]
struct Types {
    _paren: Option<token::Paren>,
    types: Punctuated<TypePath, Token![,]>,
}

impl Parse for Types {
    fn parse(input: ParseStream) -> Result<Self> {
        let (paren, types) = if input.peek(token::Paren) {
            let content;
            let paren = Some(parenthesized!(content in input));
            let types = content.parse_terminated(TypePath::parse, Token![,])?;
            (paren, types)
        } else {
            let paren = None;
            let ty: TypePath = input.parse()?;
            let types: Punctuated<TypePath, Token![,]> = iter::once(ty).collect();
            (paren, types)
        };

        Ok(Self {
            _paren: paren,
            types,
        })
    }
}

impl ToTokens for Types {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let types = &self.types;
        if types.len() == 1 {
            tokens.append_all(quote! { #types });
        } else {
            tokens.append_all(quote! {( #types )});
        }
    }
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

/// Repeats a certain macro.
///
/// # Examples
///
/// ```
/// # use my_ecs_macros::repeat_macro;
///
/// macro_rules! foo {
///     ($n:expr, $($i:expr),*) => {
///         // Something
///     };
/// }
///
/// foo!(0,);
/// foo!(1, 0);
/// foo!(2, 0, 1);
/// foo!(3, 0, 1, 2);
///
/// // Four lines above can be replaced with this.
/// repeat_macro!(foo, ..4);
/// ```
#[proc_macro]
pub fn repeat_macro(input: TokenStream) -> TokenStream {
    let RepeatMacro { id, start, end, .. } = parse_macro_input!(input as RepeatMacro);

    let repeats = (start..end).map(|n| {
        match n {
            0 => quote! { #id!(0,); }, // "0,", not "0"
            n => {
                let mut list = Punctuated::<Index, Token![,]>::new();
                for i in 0..n {
                    let i = Index::from(i); // Removes "usize" suffix.
                    list.push(i);
                }
                let n = Index::from(n); // Removes "usize" suffix.
                quote! { #id!(#n, #list); }
            }
        }
    });

    TokenStream::from(quote! {
        #( #repeats )*
    })
}

struct RepeatMacro {
    id: Ident,
    _comma: Token![,],
    start: usize,
    end: usize,
}

impl RepeatMacro {
    fn parse_range(expr_range: ExprRange) -> Result<(usize, usize)> {
        const RNG_ERR: &str = "expected integer literal";

        let start = if let Some(start) = &expr_range.start {
            match start.as_ref() {
                Expr::Lit(expr_lit) => {
                    if let Lit::Int(lit_int) = &expr_lit.lit {
                        lit_int.base10_parse()?
                    } else {
                        return Err(Error::new(expr_lit.span(), RNG_ERR));
                    }
                }
                _ => return Err(Error::new(start.span(), RNG_ERR)),
            }
        } else {
            0
        };

        let end = if let Some(end) = &expr_range.end {
            match end.as_ref() {
                Expr::Lit(expr_lit) => {
                    if let Lit::Int(lit_int) = &expr_lit.lit {
                        let parsed = lit_int.base10_parse()?;
                        match expr_range.limits {
                            RangeLimits::HalfOpen(_) => parsed,
                            RangeLimits::Closed(_) => parsed + 1,
                        }
                    } else {
                        return Err(Error::new(expr_lit.span(), RNG_ERR));
                    }
                }
                _ => return Err(Error::new(end.span(), RNG_ERR)),
            }
        } else {
            usize::MAX
        };

        if start > end {
            return Err(Error::new(
                expr_range.span(),
                "`end` must be greater than or equal to `start`",
            ));
        }

        Ok((start, end))
    }
}

impl Parse for RepeatMacro {
    fn parse(input: ParseStream) -> Result<Self> {
        let id: Ident = input.parse()?;
        let _comma = input.parse()?;
        let expr_range: ExprRange = input.parse()?;
        let (start, end) = Self::parse_range(expr_range)?;

        Ok(Self {
            id,
            _comma,
            start,
            end,
        })
    }
}
