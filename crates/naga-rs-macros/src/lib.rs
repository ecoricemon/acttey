use naga::valid::Capabilities;
use naga_rs;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, TokenStreamExt};
use std::path::{Path as StdPath, PathBuf};
use syn::{
    AttrStyle, Error, ItemMod, LitStr, Result, Token,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
};

#[proc_macro_attribute]
pub fn export(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let module = parse_macro_input!(item as syn::ItemMod);
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
    let _ = naga_rs::translate_to_wgsl(path.iter(), capability);

    // TODO: Not yet implemented. Previous commits include little impl.
    TokenStream::new()
}

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

#[derive(Debug)]
struct ImportFilePaths {
    pub paths: Punctuated<LitStr, Token![,]>,
}

impl ImportFilePaths {
    pub fn iter(&self) -> impl Iterator<Item = PathBuf> + Clone {
        self.paths
            .iter()
            .map(|path| StdPath::new(&path.value()).to_path_buf())
    }
}

impl Parse for ImportFilePaths {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            paths: Punctuated::parse_separated_nonempty(input)?,
        })
    }
}
