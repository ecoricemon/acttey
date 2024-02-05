use std::ops::Deref;

use crate::primitive::{mesh::SeparateGeometry, vector::Vector, Normal, Position};

#[derive(Debug)]
pub struct Box {
    pub width: f32,
    pub height: f32,
    pub depth: f32,
}

impl Box {
    pub fn new(width: f32, height: f32, depth: f32) -> Self {
        Self {
            width,
            height,
            depth,
        }
    }
}

impl Default for Box {
    /// Creates a `Box` with 1 x 1 x 1 size.
    fn default() -> Self {
        Box::new(1.0, 1.0, 1.0)
    }
}

impl From<[f32; 3]> for Box {
    fn from(value: [f32; 3]) -> Self {
        Box::new(value[0], value[1], value[2])
    }
}

impl From<Box> for SeparateGeometry {
    /// Creates `Geometry` from `Box` centered at origin(0, 0, 0).
    fn from(value: Box) -> Self {
        create_box_geometry(value, Vector::<f32, 3>::new(0.0, 0.0, 0.0))
    }
}

/// Creates `Geometry` from the given `Box` and locates its center at the `center`.
pub fn create_box_geometry(box_: Box, center: Vector<f32, 3>) -> SeparateGeometry {
    const DEFAULT: [f32; 4] = [0.0, 0.0, 0.0, 1.0];
    let (hw, hh, hd) = (box_.width / 2.0, box_.height / 2.0, box_.depth / 2.0);
    let fbl = center + Vector::<f32, 3>::new(-hw, -hh, hd);
    let ref_positions: Vec<Position> = (0..8)
        .map(|i| {
            Position::from_vec_f32(
                fbl + Vector::<f32, 3>::new(
                    if i & 1 != 0 { box_.width } else { 0.0 },
                    if i & 2 != 0 { box_.height } else { 0.0 },
                    if i & 4 != 0 { -box_.depth } else { 0.0 },
                ),
                DEFAULT,
            )
        })
        .collect();
    let ref_normals = [
        Normal::from_arr_f32([1.0, 0.0, 0.0], DEFAULT), // Right
        Normal::from_arr_f32([-1.0, 0.0, 0.0], DEFAULT), // Left
        Normal::from_arr_f32([0.0, 1.0, 0.0], DEFAULT), // Top
        Normal::from_arr_f32([0.0, -1.0, 0.0], DEFAULT), // Bottom
        Normal::from_arr_f32([0.0, 0.0, 1.0], DEFAULT), // Front
        Normal::from_arr_f32([0.0, 0.0, -1.0], DEFAULT), // Rear
    ];
    let planes: [[usize; 4]; 6] = [
        [1, 5, 3, 7],
        [4, 0, 6, 2],
        [2, 3, 6, 7],
        [1, 0, 5, 4],
        [0, 1, 2, 3],
        [5, 4, 7, 6],
    ];

    let mut positions = Vec::with_capacity(24);
    let mut normals = Vec::with_capacity(24);

    for (ni, plane) in planes.into_iter().enumerate() {
        for pi in plane {
            positions.push(ref_positions[pi]);
            normals.push(ref_normals[ni]);
        }
    }

    // CCW, Triangle List
    let indices: Vec<u16> = (0..6)
        .map(|i| i * 4)
        .flat_map(|i| [i, i + 1, i + 2, i + 2, i + 1, i + 3])
        .collect();

    let mut geo = SeparateGeometry::new();
    geo.with_position(positions.into())
        .with_normal(normals.into())
        .with_indices(indices.into());
    geo
}

#[derive(Debug, Default)]
pub struct Cube {
    b: Box,
}

impl Cube {
    #[inline]
    pub fn new(length: f32) -> Self {
        Self {
            b: Box::new(length, length, length),
        }
    }

    #[inline]
    pub fn get_length(&self) -> f32 {
        self.b.width
    }

    #[inline]
    pub fn set_length(&mut self, length: f32) {
        self.b.width = length;
        self.b.height = length;
        self.b.depth = length;
    }
}

impl Deref for Cube {
    type Target = Box;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.b
    }
}
