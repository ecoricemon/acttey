#![doc = include_str!("../README.md")]
#![allow(non_camel_case_types)]

mod attr;
mod function;
mod module;
mod structs;
mod to_code;
mod util;
mod var;

pub use attr::{Attribute, Attributes};
pub use module::{BeWgslModule, WgslEntry, WgslModule};
pub use my_wgsl_macros::{extern_const, extern_type, wgsl_mod};
pub use structs::{BeWgslStruct, StructMember, WgslStruct};
pub use var::{Override, Private, Storage, Uniform, Var, VarKind, WgslVarDecl, Workgroup};
pub use wgsl_builtin::prelude::*;
