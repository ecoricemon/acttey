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

/// Derive macro generating an impl of the trait `Component`.
///
/// # Examples
///
/// ```ignore
/// # use my_ecs_macros::Component;
///
/// #[derive(Component)]
/// struct CompA;
///
/// #[derive(Component)]
/// struct CompB(u8);
///
/// #[derive(Component)]
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
        impl my_ecs::ecs::ent::component::Component for #ident {
            const IS_DEFAULT: bool
                = my_ecs::ds::types::TypeHelper::<#ident>::IS_DEFAULT;
            const FN_DEFAULT: my_ecs::ds::types::FnDefaultRaw
                = my_ecs::ds::types::TypeHelper::<#ident>::FN_DEFAULT;
            const IS_CLONE: bool
                = my_ecs::ds::types::TypeHelper::<#ident>::IS_CLONE;
            const FN_CLONE: my_ecs::ds::types::FnCloneRaw
                = my_ecs::ds::types::TypeHelper::<#ident>::FN_CLONE;
        }
    })
}

/// # Examples
///
/// ```ignore
/// # use my_ecs_macros::{Component, Entity};
///
/// #[derive(Component)]
/// struct CompA;
///
/// #[derive(Entity)]
/// #[container(ChunkSparseSet)]
/// #[hasher(ahash::RandomState)]
/// struct EntA {
///     a: CompA,
/// }
/// ```
#[proc_macro_derive(Entity, attributes(container, hasher))]
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

    // Validates that all fields implement `Compoenent` trait.
    let validate_impl_component = quote! {
        const _: () = {
            const fn validate<T: my_ecs::ecs::ent::component::Component>() {}
            #(
                validate::<#field_types>();
            )*
        };
    };

    // Determines container and hasher for `EntityReg`.
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
    let hasher = input
        .attrs
        .iter()
        .filter_map(|attr| {
            if attr.path().is_ident("hasher") {
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
        impl my_ecs::ecs::ent::storage::AsEntityReg for #ident {
            fn as_entity_descriptor() -> my_ecs::ecs::ent::storage::EntityReg {
                let name = my_ecs::ecs::ent::entity::EntityName::new(
                    #ident_str.into()
                );
                let cont = Box::new(
                    my_ecs::default::ent_cont::#container::<#hasher>::new()
                );
                let mut desc = my_ecs::ecs::ent::storage::EntityReg::new(
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
        impl my_ecs::ecs::ent::component::Components for #ident {
            type Keys = [my_ecs::ecs::ent::component::ComponentKey; #num_fields];
            type Infos = [my_ecs::ds::types::TypeInfo; #num_fields];

            const LEN: usize = #num_fields;

            fn keys() -> Self::Keys {
                [#(
                    <#field_types as my_ecs::ecs::ent::component::Component>::key()
                ),*]
            }

            fn infos() -> Self::Infos {
                [#(
                    <#field_types as my_ecs::ecs::ent::component::Component>::type_info()
                ),*]
            }

            fn sorted_keys() -> Self::Keys {
                let mut keys = [#(
                    <#field_types as my_ecs::ecs::ent::component::Component>::key()
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
                        if my_ecs::ds::types::TypeHelper::<#field_types>::IS_DEBUG {
                            let helper = my_ecs::ds::types::DebugHelper {
                                f: my_ecs::ds::types::TypeHelper::<#field_types>::FN_FMT,
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
        impl my_ecs::ecs::ent::entity::Entity for #ident {
            type Ref<'cont> = #ref_ident<'cont>;
            type Mut<'cont> = #mut_ident<'cont>;

            const OFFSETS_BY_FIELD_INDEX: &'static [usize] = &[
                #(
                    std::mem::offset_of!(#ident, #field_idents)
                ),*
            ];

            fn to_column_index(fi: usize) -> usize {
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

            fn get_ref_from<Cont: my_ecs::ecs::ent::entity::ContainEntity + ?Sized>(
                cont: &Cont, vi: usize
            ) -> Self::Ref<'_> {
                unsafe { #ref_ident {
                    #(
                        #field_idents:
                            // NonNull<u8>
                            cont.value_ptr_by_value_index(
                                Self::to_column_index(#col_idxs),
                                vi
                            ).unwrap()
                            // NonNull<u8> -> NonNull<field_type>
                            .cast::<#field_types>()
                            // NonNull<field_type> -> &field_type
                            .as_ref()
                    ),*
                } }
            }

            fn get_mut_from<Cont: my_ecs::ecs::ent::entity::ContainEntity + ?Sized>(
                cont: &mut Cont, vi: usize
            ) -> Self::Mut<'_> {
                unsafe { #mut_ident {
                    #(
                        #field_idents:
                            // NonNull<u8>
                            cont.value_ptr_by_value_index(
                                Self::to_column_index(#col_idxs),
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
pub fn filter(input: TokenStream) -> TokenStream {
    let sel = parse_macro_input!(input as Select);

    // Validates if the Select::Target implement Component.
    let validate_impl_comp_for_target = if let Some(target) = &sel.target {
        quote! {
            const _: () = {
                const fn validate<T: my_ecs::ecs::ent::component::Component>() {}
                validate::<#target>();
            };
        }
    } else {
        TokenStream2::new()
    };

    // Validates if the Filter types implement Component.
    let empty = Punctuated::<TypePath, Token![,]>::new();
    let all = get_iter(&sel.filter.all, &empty);
    let any = get_iter(&sel.filter.any, &empty);
    let none = get_iter(&sel.filter.none, &empty);
    let all_clone = all.clone();
    let any_clone = any.clone();
    let none_clone = none.clone();
    let validate_impl_comp_for_filter = quote! {
        const _: () = {
            const fn validate<T: my_ecs::ecs::ent::component::Component>() {}
            #(validate::<#all_clone>();)*
            #(validate::<#any_clone>();)*
            #(validate::<#none_clone>();)*
        };
    };

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
        #validate_impl_comp_for_target
        #validate_impl_comp_for_filter
        #validate_non_overlap
        #sel
    });

    // === Internal helper functions ===

    fn get_iter<'a>(
        x: &'a Option<(Token![,], Ident, FilterList)>,
        empty: &'a Punctuated<TypePath, Token![,]>,
    ) -> syn::punctuated::Iter<'a, TypePath> {
        if let Some((_, _, list)) = x {
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
                    !my_ecs::ds::types::TypeHelper::<(#pair_as, #pair_bs)>::IS_EQUAL_TYPE,
                    "Types in `Target`, `All`, and `Any` must not be included in `None`",
                );
            )*};
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
        let all = from_list(&self.filter.all);
        let any = from_list(&self.filter.any);
        let none = from_list(&self.filter.none);
        let exact = from_list(&self.filter.exact);

        let decl_struct = quote! {
            #vis struct #ident;
        };

        let impl_filter = quote! {
            impl my_ecs::ecs::sys::select::Filter for #ident {
                type All = #all;
                type Any = #any;
                type None = #none;
                type Exact = #exact;
            }
        };

        let impl_select = if let Some(target) = &self.target {
            quote! {
                impl my_ecs::ecs::sys::select::Select for #ident {
                    type Target = #target;
                    type Filter = #ident;
                }
            }
        } else {
            TokenStream2::new()
        };

        tokens.append_all(decl_struct);
        tokens.append_all(impl_select);
        tokens.append_all(impl_filter);

        // === Internal helper functions ===

        fn from_list(x: &Option<(Token![,], Ident, FilterList)>) -> TokenStream2 {
            if let Some((_, _, list)) = x.as_ref() {
                let types = &list.types;
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
        let ty: syn::TypePath = input.parse()?;

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
    all: Option<(Token![,], Ident, FilterList)>,
    any: Option<(Token![,], Ident, FilterList)>,
    none: Option<(Token![,], Ident, FilterList)>,
    exact: Option<(Token![,], Ident, FilterList)>,
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
            match ident_str.as_str() {
                "All" => {
                    if all.is_some() {
                        return Err(Error::new(ident.span(), "duplicate `All`"));
                    }
                    let list: FilterList = input.parse()?;
                    all = Some((comma, ident, list));
                }
                "Any" => {
                    if any.is_some() {
                        return Err(Error::new(ident.span(), "duplicate `Any`"));
                    }
                    let list: FilterList = input.parse()?;
                    any = Some((comma, ident, list));
                }
                "None" => {
                    if none.is_some() {
                        return Err(Error::new(ident.span(), "duplicate `None`"));
                    }
                    let list: FilterList = input.parse()?;
                    none = Some((comma, ident, list));
                }
                "Exact" => {
                    if exact.is_some() {
                        return Err(Error::new(ident.span(), "duplicate `Exact`"));
                    }
                    let list: FilterList = input.parse()?;
                    exact = Some((comma, ident, list));
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
struct FilterList {
    _eq: Token![=],
    _paren: Option<token::Paren>,
    types: Punctuated<TypePath, Token![,]>,
}

impl Parse for FilterList {
    fn parse(input: ParseStream) -> Result<Self> {
        let eq: Token![=] = input.parse()?;

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
            _eq: eq,
            _paren: paren,
            types,
        })
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
