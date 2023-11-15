pub mod app;
pub mod ds;
pub mod ecs;
pub mod math;
pub mod primitive;
pub mod render;
pub mod util;

// Re-exports App.
pub use app::App;

// Re-exports ECS relative things.
pub use acttey_ecs_macros::{Component, Entity};
pub use ds::sparse_set::SparseSet;
pub use ecs::{
    entity::CollectGeneric,
    query::{Filter, Query, QueryMut},
    system::{Invokable, System},
};

// Re-exports Uuid.
pub use uuid::Uuid;

// Can import crate::acttey::*.
#[allow(unused_imports)]
use crate as acttey;
