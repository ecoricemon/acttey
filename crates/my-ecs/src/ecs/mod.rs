pub mod component;
pub mod entity;
pub mod filter;
pub mod manager;
pub mod query;
pub mod request;
pub mod resource;
pub mod sparse_set;
pub mod system;
pub mod wait;
pub mod worker;

pub mod prelude {
    pub use super::{
        component::Component,
        entity::{Entity, EntityKey},
        filter::Filter,
        manager::EcsManager,
        query::{EntWrite, Read, ResRead, ResWrite, Write},
        request::{Request, Response},
        resource::{Resource, ResourceKey},
        system::{AsFnSystemKey, FnSystem, NonZeroTick, StructOrFnSystem, System, Tick},
        worker::{BuildWorker, ChMsg, Job, Work, WorkerIndex},
        EcsError,
    };
    pub use my_ecs_macros::{Component, Entity};
}

use thiserror::Error;

#[derive(Error, Debug)]
pub enum EcsError {
    #[error("`{0}` has not registered yet")]
    UnknownEntity(String),
    #[error("")]
    UnknownResource(String),
}
