pub mod hierarchy;
pub mod scene;

pub mod prelude {
    pub use super::{scene::Scene, SceneError};
}

use crate::prelude::RenderError;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum SceneError {
    #[error("failed to get resource")]
    NotFoundResource,
    #[error("invalid scene: {0}")]
    InvalidScene(String),
    #[error("")]
    ErrorFromRender(#[from] RenderError),
}
