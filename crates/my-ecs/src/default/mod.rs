//! Default implementations for my-ecs traits.

pub mod ent_cont;
pub mod resource;
pub mod worker;

/// Imports types, traits, functions, and macros of the module that you will
/// need at once.
pub mod prelude {
    pub use super::{ent_cont::*, worker::*};
}
