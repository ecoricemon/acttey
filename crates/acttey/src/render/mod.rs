pub(crate) mod bind;
pub(crate) mod buffer;
pub(crate) mod canvas;
pub(crate) mod context;
pub(crate) mod desc;
pub(crate) mod fragment;
pub(crate) mod manager;
pub(crate) mod pass;
pub(crate) mod pipeline;
pub(crate) mod shaders;
pub(crate) mod vertex;

pub mod prelude {
    pub use super::fragment::desc::{DepthStencilState, FragmentState};
}

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
    #[error("{0}")]
    CycleInPassGraph(String),
}
