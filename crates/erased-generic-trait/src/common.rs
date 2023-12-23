use proc_macro2::Span;
use syn::{
    parse_quote, punctuated, token, FnArg, GenericParam, Generics, Ident, Pat, PatIdent, PatType,
    Path, PathSegment, Receiver, Signature, Token, TraitBound, Type, TypeParam, TypeParamBound,
    TypePath, TypeReference, TypeTraitObject,
};

/// Modifies `Ident` name with the given `new_name`.
#[allow(dead_code)]
pub fn modify_ident(ident: &mut Ident, new_name: &str) {
    *ident = Ident::new(new_name, ident.span());
}

/// Clones `ident` with the given `new_name`.
#[allow(dead_code)]
pub fn clone_ident_with_name(ident: &Ident, new_name: &str) -> Ident {
    Ident::new(new_name, ident.span())
}

/// Clones `ident` with the given `prefix`.
#[allow(dead_code)]
pub fn clone_ident_with_prefix(ident: &Ident, prefix: &str) -> Ident {
    let new_name: String = prefix.chars().chain(ident.to_string().chars()).collect();
    clone_ident_with_name(ident, &new_name)
}

/// Clones `ident` with the given `suffix`.
#[allow(dead_code)]
pub fn clone_ident_with_suffix(ident: &Ident, suffix: &str) -> Ident {
    let mut new_name = ident.to_string();
    new_name.push_str(suffix);
    clone_ident_with_name(ident, &new_name)
}

/// Generates a new `Ident` with the given `name` and dummy `Span`.
#[allow(dead_code)]
pub fn gen_ident(name: &str) -> Ident {
    Ident::new(name, Span::call_site())
}

/// Gets ident and mutability of `FnArg`.
#[allow(dead_code)]
pub fn parse_arg(arg: &FnArg) -> (&Ident, &Type, Option<&token::Mut>) {
    match arg {
        FnArg::Typed(PatType { ty, pat, .. }) => {
            let ident = match pat.as_ref() {
                Pat::Ident(PatIdent { ident, .. }) => ident,
                _ => unimplemented!(),
            };
            let mutability = match ty.as_ref() {
                // & or &mut
                Type::Reference(TypeReference { mutability, .. }) => mutability,
                // Not a reference
                _ => &None,
            }
            .as_ref();
            (ident, ty.as_ref(), mutability)
        }
        FnArg::Receiver(Receiver { ty, mutability, .. }) => {
            let ident = match ty.as_ref() {
                // & or &mut
                Type::Reference(TypeReference { elem, .. }) => match elem.as_ref() {
                    Type::Path(TypePath { path, .. }) => &path.segments[0].ident,
                    _ => unimplemented!(),
                },
                // Not a reference
                _ => unimplemented!(),
            };
            (ident, ty, mutability.as_ref())
        }
    }
}

/// Determines that the given `Type` is a & or &mut.
#[allow(dead_code)]
pub fn is_ref(ty: &Type) -> bool {
    matches!(ty, Type::Reference(..))
}

/// Gets generic symbols like *T* from the `Generics`.
#[allow(dead_code)]
pub fn get_generic_symbols(generics: &Generics) -> Vec<String> {
    generics.params.iter().map(get_generic_symbol).collect()
}

/// Gets generic symbol like *T* from the `GenericParam`.
#[allow(dead_code)]
pub fn get_generic_symbol(param: &GenericParam) -> String {
    match param {
        GenericParam::Type(TypeParam { ident, .. }) => ident.to_string(),
        _ => unimplemented!(),
    }
}

/// Changes generic reference parameters to `dyn Any` refererences.
#[allow(dead_code)]
pub fn change_arg_to_any<'a>(arg: &mut FnArg, mut targets: impl Iterator<Item = &'a str>) {
    // Gets into `PatType`
    let ty_dest = match arg {
        FnArg::Typed(PatType { ty, .. }) => ty,
        // Doesn't handle `Receiver`.
        FnArg::Receiver(..) => return,
    };

    // Gets into `TypeReference`
    let (mutability, ty) = match ty_dest.as_mut() {
        // &T or &mut T
        Type::Reference(TypeReference {
            mutability, elem, ..
        }) => (mutability, elem),
        // Doesn't support other types for now.
        _ => return,
    };

    // Skips not a generic reference.
    let first_seg = match ty.as_mut() {
        Type::Path(TypePath {
            path: Path { segments, .. },
            ..
        }) => &mut segments[0],
        _ => unimplemented!(),
    };
    let ty = first_seg.ident.to_string();
    if targets.all(|t| ty.as_str() != t) {
        return;
    }

    // Changes the `ty_dest` with `&mut dyn Any` or `&dyn Any`.
    let new_type: TypeReference = match mutability {
        // &mut T
        Some(_) => parse_quote! { &mut dyn std::any::Any },
        // &
        None => parse_quote! { &dyn std::any::Any },
    };
    *ty_dest = Box::new(Type::Reference(new_type));
}

/// Changes generic reference parameters to `dyn Any` refererences.
#[allow(dead_code)]
pub fn change_args_to_anys<'a>(
    args: impl Iterator<Item = &'a mut FnArg>,
    targets: impl Iterator<Item = &'a str> + Clone,
) {
    for arg in args {
        change_arg_to_any(arg, targets.clone());
    }
}

/// Gets `Ident` if `FnArg` has the name of `target` as its type's `Ident`.
#[allow(dead_code)]
pub fn get_matched_ident<'a>(arg: &'a FnArg, target: &str) -> Option<&'a Ident> {
    let first_seg = get_nth_pathseg(arg, 0)?;
    let ty = first_seg.ident.to_string();
    if ty.as_str() == target {
        let (ident, _, _) = parse_arg(arg);
        Some(ident)
    } else {
        None
    }
}

/// Gets `PathSegment`s from the given `FnArg(PatType->TypeReference->TypePath)`.
#[allow(dead_code)]
pub fn get_path_segments(arg: &FnArg) -> Option<&punctuated::Punctuated<PathSegment, Token![::]>> {
    match unwrap_pattype_typereference(arg)? {
        Type::Path(TypePath {
            path: Path { segments, .. },
            ..
        }) => Some(segments),
        _ => None,
    }
}

/// Gets `TypeParamBound`s from the given `FnArg(PatType->TypeReference->TypeTraitObject)`.
#[allow(dead_code)]
pub fn get_trait_bounds(arg: &FnArg) -> Option<&punctuated::Punctuated<TypeParamBound, Token![+]>> {
    match unwrap_pattype_typereference(arg)? {
        Type::TraitObject(TypeTraitObject { bounds, .. }) => Some(bounds),
        _ => None,
    }
}

/// Unwraps `PatType` and `TypeReference`.
#[allow(dead_code)]
pub fn unwrap_pattype_typereference(arg: &FnArg) -> Option<&Type> {
    // Gets into `PatType`
    let ty = match arg {
        FnArg::Typed(PatType { ty, .. }) => ty,
        // Doesn't handle `Receiver`.
        FnArg::Receiver(..) => return None,
    };

    // Gets into `TypeReference`
    let ty = match ty.as_ref() {
        // &T or &mut T
        Type::Reference(TypeReference { elem, .. }) => elem,
        // Doesn't support other types for now.
        _ => return None,
    };

    Some(ty.as_ref())
}

/// Gets nth `PathSegment` from the given `FnArg(PatType->TypeReference->TypePath)`.
#[allow(dead_code)]
pub fn get_nth_pathseg(arg: &FnArg, n: usize) -> Option<&PathSegment> {
    get_path_segments(arg)?.iter().nth(n)
}

/// Makes a generic method become non-generic.
#[allow(dead_code)]
pub fn modify_signature_to_erased(sig: &mut Signature) {
    // Modifies fn names.
    let new_name = format!("erased_{}", sig.ident.to_string().as_str());
    modify_ident(&mut sig.ident, new_name.as_str());

    // Skips if non-generic.
    if !is_generic(sig) {
        return;
    }

    // Gets generic symbols.
    let symbols = get_generic_symbols(&sig.generics);

    // Remove the `Generics` now.
    remove_generics(&mut sig.generics);

    // Change generic symbols in parameters into `dyn Any`.
    for arg in sig.inputs.iter_mut() {
        change_arg_to_any(arg, symbols.iter().map(|s| s.as_str()));
    }

    // Injects `__type_id__: &TypeId` as the second parameter.
    inject_type_id(sig);
}

/// Injects `__type_id__: &TypeId` as the second parameter.
#[allow(dead_code)]
pub fn inject_type_id(sig: &mut Signature) {
    let param: FnArg = parse_quote! { __type_id__: &std::any::TypeId };
    sig.inputs.insert(1, param);
}

/// Removes `Generics`.
#[allow(dead_code)]
pub fn remove_generics(generics: &mut Generics) {
    *generics = Generics {
        lt_token: None,
        params: punctuated::Punctuated::new(),
        gt_token: None,
        where_clause: None,
    }
}

/// Gets `Ident`s from the list of `FnArg`.
#[allow(dead_code)]
pub fn get_idents(args: &punctuated::Punctuated<FnArg, Token![,]>) -> Vec<Ident> {
    args.iter().map(|arg| get_ident(arg).clone()).collect()
}

/// Gets a `Ident` from the `FnArg`.
#[allow(dead_code)]
pub fn get_ident(arg: &FnArg) -> &Ident {
    let (ident, _, _) = parse_arg(arg);
    ident
}

/// Gets matched `Ident`s with the given nth generic parameter from the list of `FnArg`.
#[allow(dead_code)]
pub fn get_nth_ident(sig: &Signature, n: usize) -> Vec<Ident> {
    let symbols = get_generic_symbols(&sig.generics);
    let symbol = symbols.into_iter().nth(n);
    if let Some(symbol) = symbol {
        sig.inputs
            .iter()
            .filter_map(|arg| get_matched_ident(arg, symbol.as_str()).cloned())
            .collect()
    } else {
        Vec::new()
    }
}

/// Generates camel case string from the given snake case str.
#[allow(dead_code)]
pub fn camel_case(snake_case: &str) -> String {
    let mut res = String::new();
    let mut capital = false;
    for c in "_".chars().chain(snake_case.chars()) {
        if c == '_' {
            capital = true;
        } else if capital {
            res.push(c.to_ascii_uppercase());
            capital = false;
        } else {
            res.push(c);
        }
    }
    res
}

/// Gets the nth `GenericParam` from the given `Signature`s.
#[allow(dead_code)]
pub fn get_nth_generic(sig: &Signature, n: usize) -> Option<&GenericParam> {
    sig.generics.params.iter().nth(n)
}

/// Determines that the given `Signature` is generic.
#[allow(dead_code)]
pub fn is_generic(sig: &Signature) -> bool {
    get_nth_generic(sig, 0).is_some()
}

/// Determines that the given `FnArg` is a reference of Any.
#[allow(dead_code)]
pub fn is_any(arg: &FnArg) -> bool {
    let bounds = get_trait_bounds(arg);
    if let Some(bounds) = bounds {
        if let Some(bound) = bounds.iter().next() {
            let path = match bound {
                TypeParamBound::Trait(TraitBound { path, .. }) => path,
                _ => unimplemented!(),
            };
            let last_ident = path.segments.last().unwrap().ident.to_string();
            &last_ident == "Any"
        } else {
            false
        }
    } else {
        false
    }
}

/// Determines that the given `Signature` is associated function.
#[allow(dead_code)]
pub fn is_associated_function(sig: Signature) -> bool {
    matches!(sig.inputs.first(), Some(&FnArg::Receiver(..)))
}
