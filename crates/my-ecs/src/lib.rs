pub mod default;
pub mod ds;
pub mod ecs;
pub mod util;

pub mod prelude {
    pub use super::{default::prelude::*, ds::prelude::*, ecs::prelude::*, util::prelude::*};
    pub use rayon::prelude::*;
}

pub(crate) type DefaultHasher = std::hash::RandomState;
