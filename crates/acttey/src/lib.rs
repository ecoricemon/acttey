pub mod default;
pub mod draw;
pub mod ds;
pub mod event;
pub mod primitive;
pub mod render;
pub mod top;
pub mod util;
pub mod worker;

pub use acttey_macros;
pub use my_ecs;
pub use my_wgsl;

pub mod prelude {
    pub use super::common::AppHasher;
    pub use super::{
        acttey_macros::*, default::prelude::*, draw::prelude::*, event::prelude::*,
        primitive::prelude::*, render::prelude::*, top::prelude::*, util::prelude::*,
        worker::prelude::*,
    };
    pub use my_ecs::{self, default::prelude::*, ds::prelude::*, ecs::prelude::*};
}

// Allows us to use macros in my-ecs.
pub use my_ecs::{log, type_name, unwrap_or};

// Makes us be able to import like crate::acttey::*.
extern crate self as acttey;

pub(crate) mod errmsg {
    pub(crate) const WEBSYS_GET_ELEMENT: &str = "failed to get html element";
    pub(crate) const WEBSYS_REQ_ANIMATION: &str = "failed to request animation frame";
    pub(crate) const WEBSYS_ADD_ELEMENT: &str = "failed to create element";
}

pub mod common {
    pub type AppHasher = ahash::RandomState;
}
