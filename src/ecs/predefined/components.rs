use crate::{acttey, ecs::component::Component};
use acttey_ecs_macros::Component;

pub use crate::primitive::transform::Transform;
impl Component for Transform {}

/// Scene node mapping.  
#[derive(Component, Debug, Default)]
pub struct SceneNode {
    /// Scene key.
    pub(crate) scene_key: u32,

    /// Node index.
    pub(crate) node_index: usize,
}
