mod attr;
mod expr;
mod externs;
mod module;
mod path;
mod structs;
mod traits;
mod util;
mod var;

use module::WgslMod;
use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{parse_macro_input, ItemMod};

#[proc_macro_attribute]
pub fn wgsl_mod(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let module = parse_macro_input!(item as ItemMod);
    let wgsl_module = match WgslMod::new(module) {
        Ok(wgsl_module) => wgsl_module,
        Err(e) => return e.into_compile_error().into(),
    };
    wgsl_module.into_token_stream().into()
}

#[proc_macro]
pub fn extern_type(item: TokenStream) -> TokenStream {
    externs::extern_type(item)
}

#[proc_macro]
pub fn extern_const(item: TokenStream) -> TokenStream {
    externs::extern_const(item)
}
