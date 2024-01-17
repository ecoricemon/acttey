use crate::primitive::{mesh::Geometry, vector::Vector};

#[derive(Debug)]
pub struct Plane {
    pub width: f32,
    pub height: f32,
}

impl Plane {
    pub fn new(width: f32, height: f32) -> Self {
        Self { width, height }
    }
}

impl Default for Plane {
    fn default() -> Self {
        Self::new(1.0, 1.0)
    }
}

impl From<[f32; 2]> for Plane {
    fn from(value: [f32; 2]) -> Self {
        Plane::new(value[0], value[1])
    }
}

impl From<Plane> for Geometry {
    fn from(value: Plane) -> Self {
        create_plane_geometry(value)
    }
}

pub fn create_plane_geometry(plane: Plane) -> Geometry {
    let (hw, hh) = (plane.width / 2.0, plane.height / 2.0);
    let positions = vec![
        Vector::<f32, 3>::new(-hw, hh, 0.0),  // tl
        Vector::<f32, 3>::new(-hw, -hh, 0.0), // bl
        Vector::<f32, 3>::new(hw, hh, 0.0),   // tr
        Vector::<f32, 3>::new(hw, -hh, 0.0),  // br
    ];
    let normal = Vector::<f32, 3>::new(0.0, 0.0, 1.0);
    Geometry::new()
        .with_position(positions.into())
        .with_normal(vec![normal; 4].into())
        .with_indices(vec![0_u16, 1, 2, 2, 1, 3].into())
}
