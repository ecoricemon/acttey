use crate::{acttey, primitive::transform::Transform};
use acttey_ecs_macros::Component;

// Users can edit local transformations from ECS systems.
#[derive(Component, Debug, Default)]
pub struct Drawable {
    /// Local transformation.
    pub transform: Transform,

    /// Scene node mapping.
    pub(crate) node: SceneNodeKey,
}

impl Drawable {
    #[inline]
    pub fn get_scene_key(&self) -> u64 {
        self.node.scene_key
    }

    #[inline]
    pub fn get_scene_node_index(&self) -> usize {
        self.node.node_index
    }
}

/// Scene node mapping.  
#[derive(Component, Debug, Default)]
pub struct SceneNodeKey {
    /// Scene key.
    pub(crate) scene_key: u64,

    /// Node index.
    pub(crate) node_index: usize,
}
