use crate::primitive::{
    matrix::Matrix4f,
    vector::{Quaternion, Vector},
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

    // TODO: Actually in TRS matrices, RS can be expressed as Matrix3x3 and
    // T is Ok to be seperated.
    // But for now, use Matrix4x4 for simple operation with other matrices.
    /// Transformation matrix
    tf: Matrix4f,

    /// Dirty flag for upding transformatino matrix.
    dirty: TransformDirty,
}

impl Transform {
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
    ///
    /// # Panics
    ///
    /// In debug mode only, panics if `axis` is not a unit vector.
    #[inline]
    pub fn rotate(&mut self, axis: Vector<f32, 3>, angle: f32) {
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

    #[inline]
    pub fn get_transform(&self) -> &Matrix4f {
        &self.tf
    }

    /// Sets transformation matrix with the given value, and clears dirty flag.
    /// So that no update will take place.
    #[inline]
    pub fn set_transform(&mut self, value: Matrix4f) {
        self.tf = value;
        self.dirty.clear();
    }

    /// Updates transformation matrix according to tranlation, rotattion, and scale.
    pub fn update(&mut self) {
        const DIRTY_T: TransformDirty = TransformDirty::TRANSLATE;
        const DIRTY_RS: TransformDirty = TransformDirty::ROTATE.union(TransformDirty::SCALE);

        // Updates T if it's needed.
        if self.dirty.contains(DIRTY_T) {
            self.update_t();
        }

        // Updates RS if it's needed.
        if self.dirty.intersects(DIRTY_RS) {
            self.update_rs();
        }

        // Cleans dirty flag.
        self.dirty.clear();
    }

    /// Updates transformation matrix area influenced by rotation and scale,
    /// that is from r0c0 to r2c2.
    /// Don't forget to erase dirty flag.
    /// ref: https://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation
    fn update_rs(&mut self) {
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
        self.tf.set_col3(0, c0r0 * sx, c0r1 * sy, c0r2 * sz);

        let c1r0 = two_xy - two_zw;
        let c1r1 = 1.0 - two_xx - two_zz;
        let c1r2 = two_yz + two_xw;
        self.tf.set_col3(1, c1r0 * sx, c1r1 * sy, c1r2 * sz);

        let c2r0 = two_xz + two_yw;
        let c2r1 = two_yz - two_xw;
        let c2r2 = 1.0 - two_xx - two_yy;
        self.tf.set_col3(2, c2r0 * sx, c2r1 * sy, c2r2 * sz);
    }

    /// Updates transformation matrix area influenced by translation,
    /// that is from r0c3 to r2c3.
    /// Don't forget to erase dirty flag.
    #[inline]
    fn update_t(&mut self) {
        self.tf.set_col3(3, self.tr.x(), self.tr.y(), self.tr.z());
    }
}

impl Default for Transform {
    fn default() -> Self {
        Self {
            tr: Vector::<f32, 3>::zero(),
            rot: Quaternion::unit(),
            scale: Vector::<f32, 3>::one(),
            tf: Matrix4f::default(),
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

/// Sets x translation element of TRS matrix `mat` to the given 'x'.
pub fn translate_x(mat: &mut Matrix4f, x: f32) {
    let w = mat.get_elem(3, 3);
    mat.set_elem(3, 0, x * w);
}

/// Sets x translation element of TRS matrix `mat` to the x + dx.
pub fn translate_dx(mat: &mut Matrix4f, dx: f32) {
    let w = mat.get_elem(3, 3);
    mat.add_elem(3, 0, dx * w);
}

/// Sets y translation element of TRS matrix `mat` to the given 'y'.
pub fn translate_y(mat: &mut Matrix4f, y: f32) {
    let w = mat.get_elem(3, 3);
    mat.set_elem(3, 1, y * w);
}

/// Sets y translation element of TRS matrix `mat` to the y + dy.
pub fn translate_dy(mat: &mut Matrix4f, dy: f32) {
    let w = mat.get_elem(3, 3);
    mat.add_elem(3, 1, dy * w);
}

/// Sets z translation element of TRS matrix `mat` to the given 'z'.
pub fn translate_z(mat: &mut Matrix4f, z: f32) {
    let w = mat.get_elem(3, 3);
    mat.set_elem(3, 2, z * w);
}

/// Sets z translation element of TRS matrix `mat` to the z + dz.
pub fn translate_dz(mat: &mut Matrix4f, dz: f32) {
    let w = mat.get_elem(3, 3);
    mat.add_elem(3, 2, dz * w);
}

// pub fn translate_x(d: f32) -> Matrix4f {
//     Matrix4f::new([
//         1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, d, 0.0, 0.0, 1.0,
//     ])
// }

// pub fn translate_y(d: f32) -> Matrix4f {
//     Matrix4f::new([
//         1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, d, 0.0, 1.0,
//     ])
// }

// pub fn translate_z(d: f32) -> Matrix4f {
//     Matrix4f::new([
//         1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, d, 1.0,
//     ])
// }

// pub fn translate(dx: f32, dy: f32, dz: f32) -> Matrix4f {
//     Matrix4f::new([
//         1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, dx, dy, dz, 1.0,
//     ])
// }

// pub fn rotate_x(theta: f32) -> Matrix4f {
//     let (sin, cos) = theta.sin_cos();
//     Matrix4f::new([
//         1.0, 0.0, 0.0, 0.0, 0.0, cos, sin, 0.0, 0.0, -sin, cos, 0.0, 0.0, 0.0, 0.0, 1.0,
//     ])
// }

// pub fn rotate_y(theta: f32) -> Matrix4f {
//     let (sin, cos) = theta.sin_cos();
//     Matrix4f::new([
//         cos, 0.0, -sin, 0.0, 0.0, 1.0, 0.0, 0.0, sin, 0.0, cos, 0.0, 0.0, 0.0, 0.0, 1.0,
//     ])
// }

// pub fn rotate_z(theta: f32) -> Matrix4f {
//     let (sin, cos) = theta.sin_cos();
//     Matrix4f::new([
//         cos, sin, 0.0, 0.0, -sin, cos, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
//     ])
// }

// pub fn rotate_axis(axis: Vector<f32, 3>, theta: f32) -> Matrix4f {
//     debug_assert!((axis.norm_l2() - 1.0) < 1e-6);
//     let (sin, cos) = theta.sin_cos();
//     let o_cos = 1.0 - cos;
//     let xy = axis.x() * axis.y();
//     let xz = axis.x() * axis.z();
//     let yz = axis.y() * axis.z();
//     Matrix4f::new([
//         // Column 0
//         axis.x() * axis.x() * o_cos + cos,
//         xy * o_cos + axis.z() * sin,
//         xz * o_cos - axis.y() * sin,
//         0.0,
//         // Column 1
//         xy * o_cos - axis.z() * sin,
//         axis.y() * axis.y() * o_cos + cos,
//         yz * o_cos + axis.x() * sin,
//         0.0,
//         // Column 2
//         xz * o_cos + axis.y() * sin,
//         yz * o_cos - axis.x() * sin,
//         axis.z() * axis.z() * o_cos + cos,
//         0.0,
//         // Column 3
//         0.0,
//         0.0,
//         0.0,
//         1.0,
//     ])
// }

// pub fn scale_x(f: f32) -> Matrix4f {
//     Matrix4f::new([
//         f, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
//     ])
// }

// pub fn scale_y(f: f32) -> Matrix4f {
//     Matrix4f::new([
//         1.0, 0.0, 0.0, 0.0, 0.0, f, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
//     ])
// }

// pub fn scale_z(f: f32) -> Matrix4f {
//     Matrix4f::new([
//         1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, f, 0.0, 0.0, 0.0, 0.0, 1.0,
//     ])
// }

// pub fn scale(fx: f32, fy: f32, fz: f32) -> Matrix4f {
//     Matrix4f::new([
//         fx, 0.0, 0.0, 0.0, 0.0, fy, 0.0, 0.0, 0.0, 0.0, fz, 0.0, 0.0, 0.0, 0.0, 1.0,
//     ])
// }
