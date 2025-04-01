mod back;
mod front;
mod traits;
mod util;

use front::{
    file::{ImportFilePaths, SmFile},
    item::SmItemMod,
    sem::SemanticAnalyzer,
};
use naga::valid::Capabilities;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, TokenStreamExt};
use syn::{AttrStyle, Attribute, Error, ItemMod, parse_macro_input, spanned::Spanned};
use util::call_site;

const NAGA_CAPABILITIES: [(&'static str, Capabilities); 24] = [
    ("PUSH_CONSTANT", Capabilities::PUSH_CONSTANT),
    ("FLOAT64", Capabilities::FLOAT64),
    ("PRIMITIVE_INDEX", Capabilities::PRIMITIVE_INDEX),
    (
        "SAMPLED_TEXTURE_AND_STORAGE_BUFFER_ARRAY_NON_UNIFORM_INDEXING",
        Capabilities::SAMPLED_TEXTURE_AND_STORAGE_BUFFER_ARRAY_NON_UNIFORM_INDEXING,
    ),
    (
        "UNIFORM_BUFFER_AND_STORAGE_TEXTURE_ARRAY_NON_UNIFORM_INDEXING",
        Capabilities::UNIFORM_BUFFER_AND_STORAGE_TEXTURE_ARRAY_NON_UNIFORM_INDEXING,
    ),
    (
        "SAMPLER_NON_UNIFORM_INDEXING",
        Capabilities::SAMPLER_NON_UNIFORM_INDEXING,
    ),
    ("CLIP_DISTANCE", Capabilities::CLIP_DISTANCE),
    ("CULL_DISTANCE", Capabilities::CULL_DISTANCE),
    (
        "STORAGE_TEXTURE_16BIT_NORM_FORMATS",
        Capabilities::STORAGE_TEXTURE_16BIT_NORM_FORMATS,
    ),
    ("MULTIVIEW", Capabilities::MULTIVIEW),
    ("EARLY_DEPTH_TEST", Capabilities::EARLY_DEPTH_TEST),
    ("MULTISAMPLED_SHADING", Capabilities::MULTISAMPLED_SHADING),
    ("RAY_QUERY", Capabilities::RAY_QUERY),
    ("DUAL_SOURCE_BLENDING", Capabilities::DUAL_SOURCE_BLENDING),
    ("CUBE_ARRAY_TEXTURES", Capabilities::CUBE_ARRAY_TEXTURES),
    ("SHADER_INT64", Capabilities::SHADER_INT64),
    ("SUBGROUP", Capabilities::SUBGROUP),
    ("SUBGROUP_BARRIER", Capabilities::SUBGROUP_BARRIER),
    ("SUBGROUP_VERTEX_STAGE", Capabilities::SUBGROUP_VERTEX_STAGE),
    (
        "SHADER_INT64_ATOMIC_MIN_MAX",
        Capabilities::SHADER_INT64_ATOMIC_MIN_MAX,
    ),
    (
        "SHADER_INT64_ATOMIC_ALL_OPS",
        Capabilities::SHADER_INT64_ATOMIC_ALL_OPS,
    ),
    ("SHADER_FLOAT32_ATOMIC", Capabilities::SHADER_FLOAT32_ATOMIC),
    ("TEXTURE_ATOMIC", Capabilities::TEXTURE_ATOMIC),
    ("TEXTURE_INT64_ATOMIC", Capabilities::TEXTURE_INT64_ATOMIC),
];

#[proc_macro_attribute]
pub fn export(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let module = parse_macro_input!(item as SmItemMod);
    module.to_token_stream().into()
}

#[proc_macro_attribute]
pub fn import(attr: TokenStream, item: TokenStream) -> TokenStream {
    // item
    let module = parse_macro_input!(item as ItemMod);
    let outer_attrs = module
        .attrs
        .iter()
        .filter(|attr| attr.style == AttrStyle::Outer);
    let inner_attrs = module
        .attrs
        .iter()
        .filter(|attr| attr.style != AttrStyle::Outer);
    let filtered_outer_attrs = outer_attrs.clone().filter(|attr| {
        !NAGA_CAPABILITIES
            .iter()
            .any(|(ident, _)| attr.path().is_ident(ident))
    });

    // attr
    let path = parse_macro_input!(attr as ImportFilePaths);
    let mut capability = Capabilities::empty();
    for attr in outer_attrs.clone() {
        for (ident, cap) in NAGA_CAPABILITIES {
            if attr.path().is_ident(ident) {
                capability |= cap;
            }
        }
    }
    let import_content = translate(&path, capability);

    // to tokens
    let mut tokens = TokenStream2::new();
    tokens.append_all(filtered_outer_attrs);
    module.vis.to_tokens(&mut tokens);
    module.mod_token.to_tokens(&mut tokens);
    module.ident.to_tokens(&mut tokens);
    if let Some((brace, items)) = &module.content.as_ref() {
        brace.surround(&mut tokens, |tokens| {
            tokens.append_all(inner_attrs);
            tokens.append_all(items);
            import_content.to_tokens(tokens);
        });
    } else {
        return Error::new(module.span(), "expected curly braces")
            .into_compile_error()
            .into();
    }
    tokens.into()
}

fn translate(paths: &ImportFilePaths, capability: Capabilities) -> TokenStream2 {
    // Validates entry file path.
    let mut entry = None;
    for path in paths.iter() {
        match util::entry_path(&path) {
            Ok(path) => match &entry {
                Some(entry) if entry == &path => {}
                Some(entry) => {
                    let reason =
                        format!("detected multiple entry files: `{entry:?}` and `{path:?}`");
                    return Error::new(call_site(), reason).into_compile_error();
                }
                None => entry = Some(path),
            },
            Err(e) => return e.into_compile_error(),
        }
    }
    let Some(entry) = entry else {
        return Error::new(call_site(), "failed to find entry file").into_compile_error();
    };

    let mut sem = match SemanticAnalyzer::new(&entry) {
        Ok(sem) => sem,
        Err(e) => return e.into_compile_error(),
    };
    sem.import(paths).unwrap();
    if let Err(e) = sem.process() {
        return e.into_compile_error().into();
    }

    // @@@ TODO: Remove me
    match back::wgsl::write_string(&mut sem.naga_proc.module, capability) {
        Ok(()) => {}
        Err(e) => return e.into_compile_error(),
    };

    // TODO
    TokenStream2::new()
}
