use crate::draw::scene::{Scene, SceneKey};
use my_ecs::ecs::ent::entity::EntityKey;

pub enum SceneEvent {
    /// Command to register a scene.
    Register { key: SceneKey, scene: Scene },
    /// Command to register an entity to the scene.
    RegisterEntity { skey: SceneKey, ekey: EntityKey },
}
