pub mod app;
pub mod ds;
pub mod ecs;
pub mod math;
pub mod primitive;
pub mod render;
pub mod util;

// Re-exports App.
pub use app::App;

// Re-exports ECS derive macros.
pub use acttey_ecs_macros::{Component, Entity};

// Test code can import crate::acttey::*.
#[allow(unused_imports)]
use crate as acttey;
