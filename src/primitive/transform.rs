use crate::primitive::{
    matrix::Matrix4f,
    vector::{Quaternion, UnitVector, Vector},
};

/// 3D transfomation data such as translation, rotation, and scale.
#[derive(Debug, Clone)]
pub struct Transform {
    /// Translation
    tr: Vector<f32, 3>,

    /// Rotation
    rot: Quaternion,

    /// Scale
    scale: Vector<f32, 3>,

    /// Dirty flag for upding transformatino matrix.
    dirty: TransformDirty,
}

impl Transform {
    const DEFAULT: Self = Self {
        tr: Vector::<f32, 3>::new(0.0, 0.0, 0.0),
        rot: Quaternion::unit(),
        scale: Vector::<f32, 3>::new(1.0, 1.0, 1.0),
        dirty: TransformDirty::empty(),
    };

    pub const fn from_xyz(x: f32, y: f32, z: f32) -> Self {
        Self::from_translation(Vector::<f32, 3>::new(x, y, z))
    }

    pub const fn from_translation(tr: Vector<f32, 3>) -> Self {
        Self {
            tr,
            dirty: TransformDirty::TRANSLATE,
            ..Self::DEFAULT
        }
    }

    pub const fn from_rotation(rot: Quaternion) -> Self {
        Self {
            rot,
            dirty: TransformDirty::ROTATE,
            ..Self::DEFAULT
        }
    }

    pub const fn from_scale(scale: Vector<f32, 3>) -> Self {
        Self {
            scale,
            dirty: TransformDirty::SCALE,
            ..Self::DEFAULT
        }
    }

    #[inline]
    pub fn translate(&mut self, x: f32, y: f32, z: f32) {
        self.tr.set(x, y, z);
        self.dirty |= TransformDirty::TRANSLATE;
    }

    #[inline]
    pub fn translate_x(&mut self, x: f32) {
        self.tr.set_x(x);
        self.dirty |= TransformDirty::TRANSLATE;
    }

    #[inline]
    pub fn translate_y(&mut self, y: f32) {
        self.tr.set_y(y);
        self.dirty |= TransformDirty::TRANSLATE;
    }

    #[inline]
    pub fn translate_z(&mut self, z: f32) {
        self.tr.set_z(z);
        self.dirty |= TransformDirty::TRANSLATE;
    }

    /// Rotates with the given radians around `axis`.
    #[inline]
    pub fn rotate(&mut self, axis: UnitVector<f32, 3>, angle: f32) {
        self.rot = Quaternion::from_axis(axis, angle);
        self.dirty |= TransformDirty::ROTATE;
    }

    /// Rotates with the given radians around x-axis.
    #[inline]
    pub fn rotate_x(&mut self, angle: f32) {
        self.rot = Quaternion::from_rotation_x(angle);
        self.dirty |= TransformDirty::ROTATE;
    }

    /// Rotates with the given radians around y-axis.
    #[inline]
    pub fn rotate_y(&mut self, angle: f32) {
        self.rot = Quaternion::from_rotation_y(angle);
        self.dirty |= TransformDirty::ROTATE;
    }

    /// Rotates with the given radians around z-axis.
    #[inline]
    pub fn rotate_z(&mut self, angle: f32) {
        self.rot = Quaternion::from_rotation_z(angle);
        self.dirty |= TransformDirty::ROTATE;
    }

    #[inline]
    pub fn scale(&mut self, x: f32, y: f32, z: f32) {
        self.scale.set(x, y, z);
        self.dirty |= TransformDirty::SCALE;
    }

    #[inline]
    pub fn scale_x(&mut self, x: f32) {
        self.scale.set_x(x);
        self.dirty |= TransformDirty::SCALE;
    }

    #[inline]
    pub fn scale_y(&mut self, y: f32) {
        self.scale.set_y(y);
        self.dirty |= TransformDirty::SCALE;
    }

    #[inline]
    pub fn scale_z(&mut self, z: f32) {
        self.scale.set_z(z);
        self.dirty |= TransformDirty::SCALE;
    }

    #[inline]
    pub fn is_dirty(&self) -> bool {
        !self.dirty.is_empty()
    }

    /// Updates transformation matrix according to tranlation, rotattion, and scale.
    pub fn update_to(&mut self, mat: &mut Matrix4f) {
        const DIRTY_T: TransformDirty = TransformDirty::TRANSLATE;
        const DIRTY_RS: TransformDirty = TransformDirty::ROTATE.union(TransformDirty::SCALE);

        // Updates T if it's needed.
        if self.dirty.contains(DIRTY_T) {
            self.update_t(mat);
        }

        // Updates RS if it's needed.
        if self.dirty.intersects(DIRTY_RS) {
            self.update_rs(mat);
        }

        // Cleans dirty flag.
        self.dirty.clear();
    }

    /// Updates transformation matrix influenced by rotation and scale only,
    /// that is from r0c0 to r2c2.
    /// Other elements won't change.
    /// Don't forget to erase dirty flag.
    /// ref: https://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation
    fn update_rs(&mut self, mat: &mut Matrix4f) {
        let sx = self.scale.x();
        let sy = self.scale.y();
        let sz = self.scale.z();
        let x = self.rot.x();
        let y = self.rot.y();
        let z = self.rot.z();
        let w = self.rot.w();

        let two_x = 2.0 * x;
        let two_y = 2.0 * y;
        let two_z = 2.0 * z;
        let two_xx = two_x * x;
        let two_xy = two_x * y;
        let two_xz = two_x * z;
        let two_xw = two_x * w;
        let two_yy = two_y * y;
        let two_yz = two_y * z;
        let two_yw = two_y * w;
        let two_zz = two_z * z;
        let two_zw = two_z * w;

        let c0r0 = 1.0 - two_yy - two_zz;
        let c0r1 = two_xy + two_zw;
        let c0r2 = two_xz - two_yw;
        mat.set_col3(0, c0r0 * sx, c0r1 * sy, c0r2 * sz);

        let c1r0 = two_xy - two_zw;
        let c1r1 = 1.0 - two_xx - two_zz;
        let c1r2 = two_yz + two_xw;
        mat.set_col3(1, c1r0 * sx, c1r1 * sy, c1r2 * sz);

        let c2r0 = two_xz + two_yw;
        let c2r1 = two_yz - two_xw;
        let c2r2 = 1.0 - two_xx - two_yy;
        mat.set_col3(2, c2r0 * sx, c2r1 * sy, c2r2 * sz);
    }

    /// Updates transformation matrix influenced by translation,
    /// that is from r0c3 to r2c3.
    /// Other elements won't change.
    /// Don't forget to erase dirty flag.
    #[inline]
    fn update_t(&mut self, mat: &mut Matrix4f) {
        mat.set_col3(3, self.tr.x(), self.tr.y(), self.tr.z());
    }
}

impl Default for Transform {
    fn default() -> Self {
        Self {
            tr: Vector::<f32, 3>::zeros(),
            rot: Quaternion::unit(),
            scale: Vector::<f32, 3>::ones(),
            dirty: TransformDirty::empty(),
        }
    }
}

bitflags::bitflags! {
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct TransformDirty: u8 {
        /// Translation has changed.
        const TRANSLATE = 1 << 0;

        /// Rotation has changed.
        const ROTATE = 1 << 1;

        /// Scale has changed.
        const SCALE = 1 << 2;
    }
}

impl TransformDirty {
    /// Resets all bits.
    #[inline]
    pub fn clear(&mut self) {
        *self = Self::empty();
    }
}
