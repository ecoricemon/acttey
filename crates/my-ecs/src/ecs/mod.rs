pub mod cache;
pub mod ent;
pub mod manager;
pub mod resource;
pub mod sys;
pub mod wait;
pub mod worker;

pub mod prelude {
    use super::*;

    pub use ent::prelude::*;
    pub use sys::prelude::*;

    pub use super::{EcsError, EcsResult};
    pub use manager::{sched::Cycle, EcsManager};
    pub use resource::{MaybeOwned, Resource, ResourceKey, ResourceKind};
    pub use worker::{BuildWorker, Job, Work};

    pub use my_ecs_macros::{Component, Entity};
}

pub type EcsResult<T> = Result<T, EcsError>;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum EcsError {
    #[error("unknown entity `{0}`")]
    UnknownEntity(String),
    #[error("unknown system")]
    UnknownSystem,
    #[error("unknown resourve `{0}`")]
    UnknownResource(String),
    #[error("invalid request `{0}`")]
    InvalidRequest(String),
}
