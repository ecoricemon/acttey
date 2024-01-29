pub mod app;
pub mod event;
mod r#loop;

pub mod prelude {
    pub use super::{app::App, event::EventManager};
}
