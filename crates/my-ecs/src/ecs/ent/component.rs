use crate::ds::prelude::*;

/// Granular data such as position, speed, and something like that.
pub trait Component: 'static {
    /// Provided.
    fn key() -> ComponentKey {
        ComponentKey::of::<Self>()
    }
}

/// [`TypeId`](std::any::TypeId) of a component.
pub type ComponentKey = ATypeId<ComponentKey_>;
pub struct ComponentKey_;
