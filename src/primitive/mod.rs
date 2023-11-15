pub mod constant;
pub mod mesh;
pub mod shape;
pub mod vertex;

// Define Position, Color, Normal, and Vertex
crate::define_vertex!([f32; 3], [u8; 4], [f32; 3]);

// constant setter
const fn u8x4_to_color(value: [u8; 4]) -> Color {
    crate::math::vector::Vector(value)
}
