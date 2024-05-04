pub mod app;
pub mod canvas;
pub mod event;
mod r#loop;

pub mod prelude {
    pub use super::{
        app::{App, AppState},
        event::EventType,
    };
}

use crate::render::canvas::CanvasHandle;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum AppError {
    #[error("failed to get canvas using selectors {0}")]
    CanvasQueryError(String),
    #[error(
        "canvas was already created, you must use it instead of re-creating with selectors {0}"
    )]
    DoubleCanvasCreation(String),
    #[error("canvas can't transfer control to offscreen twice. your access handle was {0}")]
    DoubleOffscreenCanvas(CanvasHandle),
}
