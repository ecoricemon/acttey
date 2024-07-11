use crate::{
    common::AppHasher,
    draw::scene::Scene,
    msg::{manager::MessageManager, Command, Event, Message},
    worker::msg::{MsgEventCanvasResize, MsgEventClick, MsgEventMouseMove},
};
use my_ecs::ecs::prelude::*;

// `MessageManager` is splitted into several `MessageQueue` pieces.
// It can be unsafe to access the manager while any queue is referenced.
// Therefore, it's restriced to access `MessageManager` from client code.
impl Resource for MessageManager {}

// Exposes `MessageQueue`.
pub use crate::msg::manager::MessageQueue;
impl<M: Message> Resource for MessageQueue<M> {}

// Exposes type aliasing of event queue.
pub type ResizeEventQueue = MessageQueue<Event<MsgEventCanvasResize>>;
pub type MouseMoveEventQueue = MessageQueue<Event<MsgEventMouseMove>>;
pub type ClickEventQueue = MessageQueue<Event<MsgEventClick>>;

// Exposes type aliasing of command queue.
pub type SceneCommandQueue = MessageQueue<Command<Scene>>;

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
