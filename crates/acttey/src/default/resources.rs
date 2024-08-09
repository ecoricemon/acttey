use crate::common::AppHasher;
use my_ecs::ecs::prelude::*;

// Exposes event queue.
use my_ecs::ds::queue::LimitedQueue;
pub type ScaleEventQueue = LimitedQueue<crate::worker::msg::MsgWindowScale>;
pub type MouseMoveEventQueue = LimitedQueue<crate::worker::msg::MsgMouseMove>;
pub type ClickEventQueue = LimitedQueue<crate::worker::msg::MsgClick>;
pub type ResizeEventQueue = LimitedQueue<crate::worker::msg::MsgCanvasResize>;
pub type SceneEventQueue = LimitedQueue<crate::default::event::SceneEvent>;

// Exposes `RenderManager`.
pub use crate::render::manager::RenderManager;
impl Resource for RenderManager {}

// Exposes `TimeStamp`.
pub struct TimeStamp(pub f64);
impl Resource for TimeStamp {}

// Exposes `SceneManager`.
pub use crate::draw::scene::SceneManager;
impl Resource for SceneManager {}

// Exposes `EcsManager`.
pub type EcsManager = my_ecs::ecs::manager::EcsManager<AppHasher>;

// Exposes `CommonStorage`.
pub use crate::top::storage::CommonStorage;
impl Resource for CommonStorage {}
