use crate::ecs::traits::Resource;
use std::{any::TypeId, mem::transmute_copy};

// Exposes `EventManager` resource.
pub use crate::app::event::EventManager;
impl Resource for EventManager {}

// Exposes `Storage` resource.
pub use crate::ecs::storage::Storage;
impl Resource for Storage {}

// Exposes `RenderResource` resource.
pub use crate::render::resource::RenderResource;
impl Resource for RenderResource {}

// Exposes `TimeStamp` resource.
pub struct TimeStamp(pub f64);
impl Resource for TimeStamp {}

// Exposes `Systems` resouce.
pub use crate::ecs::system::Systems;
impl Resource for Systems {}

/// Integrated resources.
pub struct ResourcePack<'a> {
    pub(crate) ev_mgr: &'a mut EventManager,
    pub(crate) storage: &'a mut Storage,
    pub(crate) render: &'a mut RenderResource,
    pub(crate) time: &'a mut TimeStamp,
    pub(crate) systems: Option<&'a mut Systems>,
}

impl<'a> ResourcePack<'a> {
    #[inline]
    pub(crate) fn get<R: Resource>(&self) -> &'a mut R {
        let ty = TypeId::of::<R>();
        // Safety: Type checked.
        unsafe {
            if ty == TypeId::of::<EventManager>() {
                transmute_copy::<&mut EventManager, _>(&self.ev_mgr)
            } else if ty == TypeId::of::<Storage>() {
                transmute_copy::<&mut Storage, _>(&self.storage)
            } else if ty == TypeId::of::<RenderResource>() {
                transmute_copy::<&mut RenderResource, _>(&self.render)
            } else if ty == TypeId::of::<TimeStamp>() {
                transmute_copy::<&mut TimeStamp, _>(&self.time)
            } else if ty == TypeId::of::<Systems>() {
                transmute_copy::<&mut Systems, _>(self.systems.as_ref().unwrap())
            } else {
                panic!();
            }
        }
    }
}
