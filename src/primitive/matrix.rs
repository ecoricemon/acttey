use crate::primitive::vector::Vector;
use std::ops;

/// Column major 4x4 f32 matrix.
#[derive(Clone, Copy, bytemuck::Pod, bytemuck::Zeroable, Debug, PartialEq)]
#[repr(transparent)]
pub struct Matrix4f([f32; 16]);

impl Matrix4f {
    #[inline]
    pub fn new(value: [f32; 16]) -> Self {
        Self(value)
    }

    #[inline]
    pub fn identity() -> Self {
        Self::new([
            1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
        ])
    }

    #[inline]
    pub fn get_elem(&self, col: usize, row: usize) -> f32 {
        self.0[Self::index(col, row)]
    }

    #[inline]
    pub fn set_elem(&mut self, col: usize, row: usize, value: f32) {
        self.0[Self::index(col, row)] = value;
    }

    #[inline]
    pub fn add_elem(&mut self, col: usize, row: usize, increment: f32) {
        self.0[Self::index(col, row)] += increment;
    }

    #[inline]
    pub fn set_col3(&mut self, col: usize, x: f32, y: f32, z: f32) {
        self.set_elem(col, 0, x);
        self.set_elem(col, 1, y);
        self.set_elem(col, 2, z);
    }

    #[inline]
    pub fn add_col3(&mut self, col: usize, dx: f32, dy: f32, dz: f32) {
        self.add_elem(col, 0, dx);
        self.add_elem(col, 1, dy);
        self.add_elem(col, 2, dz);
    }

    #[inline]
    pub fn set_col4(&mut self, col: usize, x: f32, y: f32, z: f32, w: f32) {
        self.set_col3(col, x, y, z);
        self.set_elem(col, 3, w);
    }

    #[inline]
    pub fn add_col4(&mut self, col: usize, x: f32, y: f32, z: f32, w: f32) {
        self.add_col3(col, x, y, z);
        self.add_elem(col, 3, w);
    }

    #[inline]
    #[must_use]
    #[rustfmt::skip]
    pub fn transpose(self) -> Self {
        Self::new([
            self.0[0], self.0[4], self.0[8],  self.0[12],
            self.0[1], self.0[5], self.0[9],  self.0[13],
            self.0[2], self.0[6], self.0[10], self.0[14],
            self.0[3], self.0[7], self.0[11], self.0[15],
        ])
    }

    #[inline(always)]
    fn index(col: usize, row: usize) -> usize {
        (col << 2) + row
    }
}

impl Default for Matrix4f {
    /// Creates 4x4 identity matrix.
    #[inline]
    fn default() -> Self {
        Matrix4f::identity()
    }
}

impl<'a> From<&'a Matrix4f> for &'a [u8] {
    fn from(value: &'a Matrix4f) -> Self {
        bytemuck::cast_slice(std::slice::from_ref(value))
    }
}

impl<'a, 'b> ops::Mul<&'b Matrix4f> for &'a Matrix4f {
    type Output = Matrix4f;

    #[must_use]
    #[rustfmt::skip]
    fn mul(self, rhs: &'b Matrix4f) -> Self::Output {
        Matrix4f::new([
            // Column 0
            self.0[0] * rhs.0[0] + self.0[4] * rhs.0[1] + self.0[8] * rhs.0[2] + self.0[12] * rhs.0[3],
            self.0[1] * rhs.0[0] + self.0[5] * rhs.0[1] + self.0[9] * rhs.0[2] + self.0[13] * rhs.0[3],
            self.0[2] * rhs.0[0] + self.0[6] * rhs.0[1] + self.0[10] * rhs.0[2] + self.0[14] * rhs.0[3],
            self.0[3] * rhs.0[0] + self.0[7] * rhs.0[1] + self.0[11] * rhs.0[2] + self.0[15] * rhs.0[3],
            // Column 1
            self.0[0] * rhs.0[4] + self.0[4] * rhs.0[5] + self.0[8] * rhs.0[6] + self.0[12] * rhs.0[7],
            self.0[1] * rhs.0[4] + self.0[5] * rhs.0[5] + self.0[9] * rhs.0[6] + self.0[13] * rhs.0[7],
            self.0[2] * rhs.0[4] + self.0[6] * rhs.0[5] + self.0[10] * rhs.0[6] + self.0[14] * rhs.0[7],
            self.0[3] * rhs.0[4] + self.0[7] * rhs.0[5] + self.0[11] * rhs.0[6] + self.0[15] * rhs.0[7],
            // Column 2
            self.0[0] * rhs.0[8] + self.0[4] * rhs.0[9] + self.0[8] * rhs.0[10] + self.0[12] * rhs.0[11],
            self.0[1] * rhs.0[8] + self.0[5] * rhs.0[9] + self.0[9] * rhs.0[10] + self.0[13] * rhs.0[11],
            self.0[2] * rhs.0[8] + self.0[6] * rhs.0[9] + self.0[10] * rhs.0[10] + self.0[14] * rhs.0[11],
            self.0[3] * rhs.0[8] + self.0[7] * rhs.0[9] + self.0[11] * rhs.0[10] + self.0[15] * rhs.0[11],
            // Column 3
            self.0[0] * rhs.0[12] + self.0[4] * rhs.0[13] + self.0[8] * rhs.0[14] + self.0[12] * rhs.0[15],
            self.0[1] * rhs.0[12] + self.0[5] * rhs.0[13] + self.0[9] * rhs.0[14] + self.0[13] * rhs.0[15],
            self.0[2] * rhs.0[12] + self.0[6] * rhs.0[13] + self.0[10] * rhs.0[14] + self.0[14] * rhs.0[15],
            self.0[3] * rhs.0[12] + self.0[7] * rhs.0[13] + self.0[11] * rhs.0[14] + self.0[15] * rhs.0[15],
        ])
    }
}

impl ops::Mul<Vector<f32, 2>> for &Matrix4f {
    type Output = Vector<f32, 2>;

    #[must_use]
    fn mul(self, rhs: Vector<f32, 2>) -> Self::Output {
        Vector::<f32, 2>::new(
            self.0[0] * rhs.x() + self.0[4] * rhs.y() + self.0[12],
            self.0[1] * rhs.x() + self.0[5] * rhs.y() + self.0[13],
        )
    }
}

impl ops::Mul<Vector<f32, 3>> for &Matrix4f {
    type Output = Vector<f32, 3>;

    #[must_use]
    fn mul(self, rhs: Vector<f32, 3>) -> Self::Output {
        Vector::<f32, 3>::new(
            self.0[0] * rhs.x() + self.0[4] * rhs.y() + self.0[8] * rhs.z() + self.0[12],
            self.0[1] * rhs.x() + self.0[5] * rhs.y() + self.0[9] * rhs.z() + self.0[13],
            self.0[2] * rhs.x() + self.0[6] * rhs.y() + self.0[10] * rhs.z() + self.0[14],
        )
    }
}

impl ops::Mul<Vector<f32, 4>> for &Matrix4f {
    type Output = Vector<f32, 4>;

    #[must_use]
    fn mul(self, rhs: Vector<f32, 4>) -> Self::Output {
        Vector::<f32, 4>::new(
            self.0[0] * rhs.x() + self.0[4] * rhs.y() + self.0[8] * rhs.z() + self.0[12] * rhs.w(),
            self.0[1] * rhs.x() + self.0[5] * rhs.y() + self.0[9] * rhs.z() + self.0[13] * rhs.w(),
            self.0[2] * rhs.x() + self.0[6] * rhs.y() + self.0[10] * rhs.z() + self.0[14] * rhs.w(),
            self.0[3] * rhs.x() + self.0[7] * rhs.y() + self.0[11] * rhs.z() + self.0[15] * rhs.w(),
        )
    }
}
