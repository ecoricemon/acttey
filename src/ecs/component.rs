use std::any::TypeId;

/// Granular data such as position, speed, and something like that.
pub trait Component: 'static {}

/// [`TypeId`] for [`Component`].
#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub struct ComponentKey(TypeId);

impl ComponentKey {
    pub const fn new(ty: TypeId) -> Self {
        Self(ty)
    }

    pub const fn as_type(&self) -> &TypeId {
        &self.0
    }
}
