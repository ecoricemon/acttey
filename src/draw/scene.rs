use my_ecs::ecs::prelude::*;

#[derive(Debug)]
pub struct SceneManager {}

impl SceneManager {
    pub fn new() -> Self {
        Self {}
    }
}

/// A descriptor for [`SceneData`].
#[derive(Debug)]
pub struct Scene {
    entities: Vec<EntityKey<'static>>,
}

impl Scene {
    pub fn new() -> Self {
        Self {
            entities: Vec::new(),
        }
    }

    pub fn register_entity(&mut self, ekey: EntityKey<'static>) {
        self.entities.push(ekey);
    }
}

#[derive(Debug)]
pub(crate) struct SceneData {}
