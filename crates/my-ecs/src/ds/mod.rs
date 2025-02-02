//! Crate-agnostic general data structures.
//!
//! This module provides general data structures you can use regardless of ECS.
//! The data structures might not be used directly on client code, but it may be
//! useful for general need. For instance, [`AnyVec`] in this module is a vector
//! which is able to contain type-erased values. That is useful when you need
//! to store vectors of various types in a single variable.

mod arr;
mod borrow;
mod fut;
mod list;
mod map;
mod ptr;
mod queue;
mod raw;
mod signal;
mod types;
mod vec;

pub use arr::*;
pub use borrow::*;
pub use fut::*;
pub use list::*;
pub use map::*;
pub use ptr::*;
pub use queue::*;
pub use raw::*;
pub use signal::*;
pub use types::*;
pub use vec::*;

/// Imports things that is likely needed in ECS functinos.
pub mod prelude {
    pub use super::types::*;
}
