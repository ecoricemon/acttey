mod module;
mod structs;
mod traits;
mod util;

use module::WgslMod;
use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use structs::ExternLayout;
use syn::{parse_macro_input, ItemMod};

#[proc_macro_attribute]
pub fn wgsl_mod(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let module = parse_macro_input!(item as ItemMod);
    let wgsl_module = match WgslMod::new(&module) {
        Ok(wgsl_module) => wgsl_module,
        Err(e) => return e.into_compile_error().into(),
    };
    wgsl_module.into_token_stream().into()
}

#[proc_macro]
pub fn layout(item: TokenStream) -> TokenStream {
    let layout = parse_macro_input!(item as ExternLayout);
    layout.into_token_stream().into()
}

// === Share the same plain types with outer crate ===

#[allow(unused)]
#[repr(align(4))]
pub(crate) struct Bool(bool);

#[proc_macro]
pub fn decl_bool(item: TokenStream) -> TokenStream {
    assert!(item.is_empty());
    quote! {
        #[repr(align(4))]
        #[derive(Clone, Copy, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
        pub struct Bool(bool);

        impl std::fmt::Debug for Bool {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                self.0.fmt(f)
            }
        }
    }
    .into()
}

#[rustfmt::skip]
macro_rules! help_decl_plain_type {
    (int, $outer:ident, $inner:ty, $fname:ident) => {
        #[allow(unused)]
        pub(crate) struct $outer($inner);

        impl $outer {
            pub(crate) const fn ident() -> &'static str { stringify!($outer) }
        }

        #[proc_macro]
        pub fn $fname(item: TokenStream) -> TokenStream {
            assert!(item.is_empty());
            quote! {
                #[repr(transparent)]
                #[derive(Clone, Copy, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
                pub struct $outer(pub $inner);

                impl std::fmt::Debug for $outer {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) 
                        -> Result<(), std::fmt::Error> 
                    { self.0.fmt(f) }
                }
            }.into()
        }
    };
    (float, $outer:ident, $inner:ty, $fname:ident) => {
        #[allow(unused)]
        pub(crate) struct $outer($inner);

        impl $outer {
            #[allow(unused)]
            pub(crate) const fn ident() -> &'static str { stringify!($outer) }
        }

        #[proc_macro]
        pub fn $fname(item: TokenStream) -> TokenStream {
            assert!(item.is_empty());
            quote! {
                #[repr(transparent)]
                #[derive(Clone, Copy, Default, PartialEq, PartialOrd)]
                pub struct $outer(pub $inner);

                impl std::fmt::Debug for $outer {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) 
                        -> Result<(), std::fmt::Error> 
                    { self.0.fmt(f) }
                }
            }.into()
        }
    };
}

#[rustfmt::skip]
macro_rules! help_decl_wide_type {
    (int, $outer:ident, $inner:ty, $align:expr, $fname:ident) => {
        #[allow(unused)]
        #[repr(align($align))]
        pub(crate) struct $outer($inner);

        impl $outer {
            pub(crate) const fn ident() -> &'static str { stringify!($outer) }
        }

        #[proc_macro]
        pub fn $fname(item: TokenStream) -> TokenStream {
            assert!(item.is_empty());
            quote! {
                #[allow(unused)]
                #[repr(align($align))]
                #[derive(Clone, Copy, Default, PartialEq, PartialOrd, Eq, Ord, Hash)]
                pub struct $outer(pub $inner);

                impl std::fmt::Debug for $outer {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) 
                        -> Result<(), std::fmt::Error>
                    { self.0.fmt(f) }
                }
            }.into()
        }
    };
    (float, $outer:ident, $inner:ty, $align:expr, $fname:ident) => {
        #[allow(unused)]
        #[repr(align($align))]
        pub(crate) struct $outer($inner);

        impl $outer {
            pub(crate) const fn ident() -> &'static str { stringify!($outer) }
        }

        #[proc_macro]
        pub fn $fname(item: TokenStream) -> TokenStream {
            assert!(item.is_empty());
            quote! {
                #[allow(unused)]
                #[repr(align($align))]
                #[derive(Clone, Copy, Default, PartialEq, PartialOrd)]
                pub struct $outer(pub $inner);

                impl std::fmt::Debug for $outer {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) 
                        -> Result<(), std::fmt::Error> 
                    { self.0.fmt(f) }
                }
            }.into()
        }
    };
}

// Vec
help_decl_plain_type!(int, Vec2i, [i32; 2], decl_vec2i);
help_decl_plain_type!(int, Vec3i, [i32; 3], decl_vec3i);
help_decl_plain_type!(int, Vec4i, [i32; 4], decl_vec4i);
help_decl_plain_type!(int, Vec2u, [u32; 2], decl_vec2u);
help_decl_plain_type!(int, Vec3u, [u32; 3], decl_vec3u);
help_decl_plain_type!(int, Vec4u, [u32; 4], decl_vec4u);
help_decl_plain_type!(float, Vec2f, [f32; 2], decl_vec2f);
help_decl_plain_type!(float, Vec3f, [f32; 3], decl_vec3f);
help_decl_plain_type!(float, Vec4f, [f32; 4], decl_vec4f);

// WideVec
//
// Why we need WideVec?
//
// * TLDR; Use Vec in structs. Use WideVec in arrays.
//
// - Clients can use Vec inside structs. This macro will append required pad
//   fields after Vec fields to be compatible with WGSL.
// - But, clients are disallowed to use Vec inside arrays. This macro cannot
//   insert required pad siliently into the array elements.
// - WideVec is forcefully aligned for WGSL, so that it can be used in arrays.
// - Then, can clients use WideVec only? WGSL defines Vec3 to have 12 bytes
//   size and 16 bytes alignment. So 4 bytes types can follow it and be packed
//   with it for compact size. Unfortunately, Rust doesn't allow 12/16 layout
//   like WGSL. Vec3 in Rust has 12/4 layout so that it can be packed with 4
//   bytes types.
// - Consequently, the macro will put pad fields in structs when it needed.
//   But, the macro cannot do that in arrays, so clients need to use WideVec
//   instead.
help_decl_wide_type!(int, WideVec2i, Vec2i, 8, decl_wide_vec2i);
help_decl_wide_type!(int, WideVec3i, Vec3i, 16, decl_wide_vec3i);
help_decl_wide_type!(int, WideVec4i, Vec4i, 16, decl_wide_vec4i);
help_decl_wide_type!(int, WideVec2u, Vec2u, 8, decl_wide_vec2u);
help_decl_wide_type!(int, WideVec3u, Vec3u, 16, decl_wide_vec3u);
help_decl_wide_type!(int, WideVec4u, Vec4u, 16, decl_wide_vec4u);
help_decl_wide_type!(float, WideVec2f, Vec2f, 8, decl_wide_vec2f);
help_decl_wide_type!(float, WideVec3f, Vec3f, 16, decl_wide_vec3f);
help_decl_wide_type!(float, WideVec4f, Vec4f, 16, decl_wide_vec4f);

// Mat
help_decl_plain_type!(float, Mat2x2f, [WideVec2f; 2], decl_mat2x2f);
help_decl_plain_type!(float, Mat2x3f, [WideVec3f; 2], decl_mat2x3f);
help_decl_plain_type!(float, Mat2x4f, [WideVec4f; 2], decl_mat2x4f);
help_decl_plain_type!(float, Mat3x2f, [WideVec2f; 3], decl_mat3x2f);
help_decl_plain_type!(float, Mat3x3f, [WideVec3f; 3], decl_mat3x3f);
help_decl_plain_type!(float, Mat3x4f, [WideVec4f; 3], decl_mat3x4f);
help_decl_plain_type!(float, Mat4x2f, [WideVec2f; 4], decl_mat4x2f);
help_decl_plain_type!(float, Mat4x3f, [WideVec3f; 4], decl_mat4x3f);
help_decl_plain_type!(float, Mat4x4f, [WideVec4f; 4], decl_mat4x4f);
