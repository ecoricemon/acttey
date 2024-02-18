use crate::{
    acttey,
    primitive::{matrix::Matrix4f, vector::Vector},
};
use acttey_ecs_macros::Component;

// Users can edit local transformations from ECS systems.
#[derive(Component, Debug)]
pub struct Drawable {
    /// Local translate vector.
    tr: Vector<f32, 3>,

    // TODO: Use quaternion.
    /// Local rotate vector.
    rot: Vector<f32, 3>,

    /// Local scale vector.
    scale: Vector<f32, 3>,

    /// Scene node mapping.
    pub(crate) node: SceneNodeKey,

    pub dirty: bool,
}

impl Default for Drawable {
    fn default() -> Self {
        Self {
            tr: Default::default(),
            rot: Default::default(),
            scale: Vector::<f32, 3>::new(1.0, 1.0, 1.0),
            node: Default::default(),
            dirty: Default::default(),
        }
    }
}

impl Drawable {
    #[inline]
    pub fn translate_x(&mut self, value: f32) {
        self.tr.add_x(value);
        self.dirty = true;
    }

    #[inline]
    pub fn translate_y(&mut self, value: f32) {
        self.tr.add_y(value);
        self.dirty = true;
    }

    #[inline]
    pub fn translate_z(&mut self, value: f32) {
        self.tr.add_z(value);
        self.dirty = true;
    }

    #[inline]
    pub fn rotate_x(&mut self, value: f32) {
        self.rot.add_x(value);
        self.dirty = true;
    }

    #[inline]
    pub fn rotate_y(&mut self, value: f32) {
        self.rot.add_y(value);
        self.dirty = true;
    }

    #[inline]
    pub fn rotate_z(&mut self, value: f32) {
        self.rot.add_z(value);
        self.dirty = true;
    }

    #[inline]
    pub fn get_scene_key(&self) -> u64 {
        self.node.scene_key
    }

    #[inline]
    pub fn get_scene_node_index(&self) -> usize {
        self.node.node_index
    }

    pub fn local_transform(&self) -> Matrix4f {
        use crate::primitive::transform;
        let t = transform::translate(self.tr.x(), self.tr.y(), self.tr.z());
        let rx = transform::rotate_x(self.rot.x());
        let ry = transform::rotate_y(self.rot.y());
        let rz = transform::rotate_z(self.rot.z());
        let s = transform::scale(self.scale.x(), self.scale.y(), self.scale.z());
        let mut trs = &t * &rx;
        trs = &trs * &ry;
        trs = &trs * &rz;
        &trs * &s
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
