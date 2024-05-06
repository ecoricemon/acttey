use crate::ecs::resource::Resource;

// TODO: Move EventManager from top module.
// Exposes `EventManager`.
pub use crate::top::event::EventManager;
impl Resource for EventManager {}

// Exposes `RenderResource`.
pub use crate::render::resource::RenderResource;
impl Resource for RenderResource {}

// Exposes `TimeStamp`.
pub struct TimeStamp(pub f64);
impl Resource for TimeStamp {}

// Exposes `SceneManager`.
pub use crate::scene::inner::SceneManager;
impl Resource for SceneManager {}

// Exposes `Scheduler`.
pub use crate::ecs::schedule::Scheduler;
impl Resource for Scheduler {}

// Exposes `MeshResource`.
pub use crate::primitive::mesh::MeshResource;
impl Resource for MeshResource {}
