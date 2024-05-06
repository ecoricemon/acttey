pub mod borrow_js;
pub mod component;
pub mod entity;
pub mod filter;
pub mod predefined;
pub mod query;
pub mod request;
pub mod resource;
pub mod schedule;
pub mod sparse_set;
pub mod system;
pub mod wait;

pub mod prelude {
    pub use super::{
        entity::EntityKey,
        filter::Filter,
        predefined::prelude::*,
        query::{Read, ResRead, ResWrite, Write},
        request::{Request, Response},
        system::{AsFnSystemKey, System},
    };
    pub use acttey_ecs_macros::Component;
}
