mod constant;
mod mesh;
mod shape;
mod vertex;

pub use constant::*;
pub use mesh::*;
pub use shape::*;
pub use vertex::*;

pub mod prelude {
    pub use crate::{constant::*, mesh::*, shape::*, vertex::*, Color, Normal, Position, Vertex};
}

// Define Position, Color, Normal, and Vertex
define_vertex!([f32; 3], [u8; 4], [f32; 3]);

// constant setter
const fn u8x4_to_color(value: [u8; 4]) -> Color {
    acttey_math::Vector(value)
}
