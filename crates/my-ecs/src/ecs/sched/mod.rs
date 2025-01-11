pub(super) mod comm;
pub(super) mod ctrl;
pub mod par;
pub(super) mod task;

pub mod prelude {
    pub use super::ctrl::{schedule_future, SubContext};
    pub use super::par::{EcsPar, IntoEcsPar};
}
