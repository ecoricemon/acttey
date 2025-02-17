#![doc = include_str!("../README.md")]
#![allow(non_camel_case_types)]

mod attr;
mod builtin_functions;
mod builtin_types;
mod function;
mod module;
mod structs;
mod to_code;
mod util;
mod var;

pub use module::{BeWgslModule, WgslEntry, WgslModule};
pub use my_wgsl_macros::wgsl_mod;
pub use structs::{BeWgslStruct, StructMember, WgslStruct};
pub use to_code::Identify;

pub mod builtin {
    pub use super::builtin_functions::*;
    pub use super::builtin_types::*;
}
