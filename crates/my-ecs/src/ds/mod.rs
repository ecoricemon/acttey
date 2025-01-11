//! # application agnostic data structures.
//!
//! TODO

pub mod arr;
pub mod borrow;
pub mod fut;
pub mod list;
pub mod map;
pub mod ptr;
pub mod queue;
pub mod raw;
pub mod signal;
pub mod types;
pub mod vec;

pub mod prelude {
    pub use super::{
        arr::*, borrow::*, fut::*, list::*, map::*, ptr::*, queue::*, raw::*, signal::*, types::*,
        vec::*,
    };
}
