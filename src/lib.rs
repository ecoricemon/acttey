pub mod app;
pub mod ds;
pub mod ecs;
pub mod primitive;
pub mod render;
pub mod scene;
pub mod util;

pub mod prelude {
    pub use super::{
        app::prelude::*, ds::prelude::*, ecs::prelude::*, primitive::prelude::*,
        render::prelude::*, scene::prelude::*, util::prelude::*,
    };
}

// Makes us be able to import like crate::acttey::*.
extern crate self as acttey;

pub(crate) mod errmsg {
    pub(crate) const WEBSYS_GET_ELEMENT: &str = "failed to get html element";
    pub(crate) const WEBSYS_GET_PERFORMANCE: &str = "failed to get performance";
    pub(crate) const WEBSYS_REQ_ANIMATION: &str = "failed to request animation frame";
    pub(crate) const WEBSYS_ADD_LISTENER: &str = "failed to add event listener";
    pub(crate) const WEBSYS_ADD_ELEMENT: &str = "failed to create element";
}
