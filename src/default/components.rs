use my_ecs::ecs::prelude::*;
use std::ops::AddAssign;

// Exposes `Transform`
pub use crate::primitive::transform::Transform;
impl Component for Transform {}

use crate::util::key::ObjectKey;
impl Component for ObjectKey {}

/// I32 componenet.
#[derive(Component, Debug)]
pub struct I32(pub i32);

impl AddAssign<i32> for I32 {
    fn add_assign(&mut self, rhs: i32) {
        self.0 += rhs;
    }
}

/// Scene node mapping.  
#[derive(Component, Debug, PartialEq, Eq)]
pub struct SceneNode {
    /// Scene key.
    pub(crate) scene_key: u32,

    /// Node index.
    pub(crate) node_index: usize,
}

impl SceneNode {
    const DUMMY: Self = Self {
        scene_key: u32::MAX,
        node_index: usize::MAX,
    };

    pub const fn dummy() -> Self {
        Self::DUMMY
    }

    pub fn is_dummy(&self) -> bool {
        self == &Self::DUMMY
    }
}
