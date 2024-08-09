use crate::{
    common::AppHasher,
    primitive::mesh::MeshKey,
    util::key::{AnObjectKey, IntoObjectKey, ObjectKey},
};
use my_ecs::ecs::prelude::*;
use std::collections::{HashMap, HashSet};

/// [`ObjectKey`] for [`Scene`].
pub type SceneKey = AnObjectKey<Scene>;

#[derive(Debug)]
pub struct SceneManager {
    /// Scene data storage.
    scenes: HashMap<SceneKey, Scene, AppHasher>,
}

impl SceneManager {
    pub(crate) fn new() -> Self {
        Self {
            scenes: HashMap::default(),
        }
    }

    pub fn register_scene<K>(&mut self, key: K, scene: Scene)
    where
        K: IntoObjectKey,
    {
        self.scenes.insert(key.into_typed_key(), scene);
    }

    pub fn get_scene(&self, key: &SceneKey) -> Option<&Scene> {
        self.scenes.get(key)
    }

    pub fn get_scene_mut(&mut self, key: &SceneKey) -> Option<&mut Scene> {
        self.scenes.get_mut(key)
    }
}

#[derive(Debug, Default)]
pub struct Scene {
    /// Entities that belong to the scene.
    ents: HashSet<EntityIndex, AppHasher>,

    /// Meshes that belong to the scene.
    meshes: HashSet<MeshKey, AppHasher>,
}

impl Scene {
    pub fn new() -> Self {
        Self {
            ents: HashSet::default(),
            meshes: HashSet::default(),
        }
    }

    pub fn contains_entity(&self, enti: &EntityIndex) -> bool {
        self.ents.contains(enti)
    }

    /// Registers the entity index.
    /// If the scene already had it, nothing will change.
    pub fn register_entity(&mut self, enti: EntityIndex) {
        self.ents.insert(enti);
    }

    /// Registers the mesh key.
    /// If the scene already had it, nothing will change.
    pub fn register_mesh(&mut self, mesh: MeshKey) {
        self.meshes.insert(mesh);
    }
}
