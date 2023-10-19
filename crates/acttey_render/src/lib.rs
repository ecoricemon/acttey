pub mod camera;
pub mod input;
pub mod render;

pub mod prelude {
    pub use crate::{camera::*, input::*, render::*};
}
