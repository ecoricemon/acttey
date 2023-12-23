use crate::{acttey, primitive::matrix::Matrix4f};
use acttey_ecs_macros::Component;

#[derive(Component, Default)]
pub struct Renderable {
    pub mesh_key: usize,
    // TODO: use tr, rot, scale as vec, quet, vec
    pub transformation: Matrix4f,
    pub translation: Matrix4f,
    pub rotation: Matrix4f,
    pub scale: Matrix4f,
    // dirty
}
