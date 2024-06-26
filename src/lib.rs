pub mod ds;
pub mod ecs;
pub mod primitive;
pub mod render;
pub mod scene;
pub mod top;
pub mod unit;
pub mod util;
pub mod worker;

pub mod prelude {
    pub use super::{
        ds::prelude::*, ecs::prelude::*, impl_filter, impl_request, primitive::prelude::*,
        render::prelude::*, scene::prelude::*, tinfo, top::prelude::*, util::prelude::*,
    };
}

// Makes us be able to import like crate::acttey::*.
extern crate self as acttey;

pub(crate) mod errmsg {
    pub(crate) const WEBSYS_GET_ELEMENT: &str = "failed to get html element";
    pub(crate) const WEBSYS_REQ_ANIMATION: &str = "failed to request animation frame";
    pub(crate) const WEBSYS_ADD_ELEMENT: &str = "failed to create element";
}
