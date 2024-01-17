pub mod bind;
pub mod buffer;
pub mod canvas;
pub mod context;
pub mod descs;
pub mod pipeline;
pub mod renderer;
pub mod resource;
pub mod shaders;

pub mod prelude {
    pub use super::RenderError;
}

// Re-exports RenderResource iterator types.
pub use resource::{IterBindGroupLayout, IterShader};

use buffer::*;
use context::*;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum RenderError {
    #[error("failed to get gpu adapter")]
    RequestAdapterError,
    #[error("{0:?}")]
    RequestDeviceError(wgpu::RequestDeviceError),
    #[error("invalid features")]
    InvalidFeatures,
    #[error("invalid limits")]
    InvalidLimits,
    #[error("failed to create render pipeline: {0}")]
    PipelineCreationError(String),
    #[error("failed to render: {0}")]
    RenderError(String),
    #[error("{0}")]
    BufferError(String),
    #[error("failed to get canvas using query selector: {0}")]
    CanvasQueryError(String),
}
