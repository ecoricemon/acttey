pub mod constant;
pub mod mesh;
pub mod shape;
pub mod matrix;
pub mod transform;
pub mod vector;

/// Default position type is Vector<f32, 3>.
pub type Position = vector::Vector<f32, 3>;

/// Default normal type is Vector<f32, 3>.
pub type Normal = vector::Vector<f32, 3>;

/// Default color type is Vector<u8, 4>.
pub type Color = vector::Vector<u8, 4>;

// constant setter
const fn u8x4_to_color(value: [u8; 4]) -> Color {
    Color::new(value[0], value[1], value[2], value[3])
}
