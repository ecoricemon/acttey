pub mod scene;

pub mod prelude {
    pub use super::{scene::Scene, SceneError};
}

use thiserror::Error;
use crate::prelude::RenderError;

#[derive(Error, Debug)]
pub enum SceneError {
    #[error("failed to get resource")]
    NotFoundResource,
    #[error("invalid scene: {0}")]
    InvalidScene(String),
    #[error("")]
    ErrorFromRender(#[from] RenderError)
}
