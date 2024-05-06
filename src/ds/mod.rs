pub mod borrow;
pub mod buf;
pub mod common;
pub mod dep_graph;
pub mod generational;
pub mod get;
pub mod graph;
pub mod map;
pub mod refs;
pub mod set_list;
pub mod sparse_set;
pub mod vec;

pub mod prelude {
    pub use super::DsError;
}

use thiserror::Error;

#[derive(Error, Debug)]
pub enum DsError {
    #[error("failed to upgrade weak")]
    WeakUpgradeFail,
}
