pub mod camera;
pub mod constant;
pub mod matrix;
pub mod mesh;
// pub mod shape;
pub mod shapes;
pub mod transform;
pub mod vector;

pub mod prelude {
    pub use super::{camera, constant::colors, matrix::Matrix4f, shapes, transform};
}

/// Default position type is Vector<f32, 3>.
pub type Position = vector::Vector<f32, 3>;

/// Default normal type is Vector<f32, 3>.
pub type Normal = vector::Vector<f32, 3>;

/// Default color type is Vector<u8, 4>.
pub type Color = vector::Vector<u8, 3>;

// constant setter
const fn u8x4_to_color(value: [u8; 3]) -> Color {
    Color::new(value[0], value[1], value[2])
}
