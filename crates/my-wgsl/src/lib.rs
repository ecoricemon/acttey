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

pub use builtin_functions::*;
pub use builtin_types::*;
pub use module::{BeWgslModule, WgslEntry, WgslModule};
pub use my_wgsl_macros::{layout, wgsl_mod};
pub use structs::{BeWgslStruct, StructMember, WgslStruct};
