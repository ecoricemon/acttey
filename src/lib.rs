pub mod app;
pub mod ds;
pub mod ecs;
pub mod primitive;
pub mod render;
pub mod util;

pub mod prelude {
    pub use super::{
        app::prelude::*, ds::prelude::*, ecs::prelude::*, primitive::prelude::*, render,
        render::prelude::*, util::prelude::*,
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

// --- Global setttings for the resource key type. ---

// Can we use integer instead of string?
/// Resource key.
pub type ResKey = std::rc::Rc<str>;

/// Trait bound compilation for the [`ResKey`].
/// `ResKey` requires [`util::ToStr`] because it's also used as a string label.
/// Therefore, `ResKey` should be able to become &str or String.
pub trait AsResKey: Eq + std::hash::Hash + Clone + util::ToStr + 'static {}

/// Blanket implementation for the [`AsResKey`].
impl<T: Eq + std::hash::Hash + Clone + util::ToStr + 'static> AsResKey for T {}

// Dummy resource key, which is used for generating default value.
std::thread_local! {
    pub(crate) static DUMMY_RES_KEY: ResKey = std::rc::Rc::from("");
}

/// Gets the dummy resource key.
pub fn get_dummy_res_key() -> ResKey {
    DUMMY_RES_KEY.with(ResKey::clone)
}
