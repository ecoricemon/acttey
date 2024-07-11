use super::components;
use crate::{common::AppHasher, prelude::ObjectKey, primitive::prelude::Matrix4f};
use my_ecs::ecs::prelude::*;
use my_wgsl::wgsl_struct;

/// Test entity.
#[derive(Entity)]
#[entity_hasher(crate::common::AppHasher)]
pub struct TestEntity {
    pub i: components::I32,
}

/// An entity descriptor with very basic attributes.
#[derive(Entity)]
pub struct SimpleEntity {
    /// Mesh key.
    pub mesh: ObjectKey,

    /// Transformation.
    pub transform: components::Transform,
}

#[wgsl_struct]
pub struct WgslSimpleEntity {
    /// Global model transformation matrix.
    #[wgsl_type(mat4x4f)]
    model: Matrix4f,
}
