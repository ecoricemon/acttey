use crate::ecs::resource::Resource;

// TODO: Move EventManager from top module.
// Exposes `EventManager` resource.
pub use crate::top::event::EventManager;
impl Resource for EventManager {}

// Exposes `RenderResource` resource.
pub use crate::render::resource::RenderResource;
impl Resource for RenderResource {}

// Exposes `TimeStamp` resource.
pub struct TimeStamp(pub f64);
impl Resource for TimeStamp {}

// Exposes `SceneManager` resource.
pub use crate::scene::inner::SceneManager;
impl Resource for SceneManager {}
