pub mod dep_graph;
pub mod generational;
pub mod refs;
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
