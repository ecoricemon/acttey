mod default;
mod ds;
mod gpu;
mod top;
mod util;

pub mod prelude {
    pub use super::default::cmd;
    pub use super::gpu::prelude::*;
    pub use super::top::Acttey;
    pub use my_ecs::{self, prelude::*};
    pub use my_wgsl::{self, *};
    pub use wgpu;
}

pub(crate) type AppHasher = ahash::RandomState;

pub use my_ecs::ecs::EcsError;
use std::fmt;
use thiserror::Error;

#[derive(Error)]
#[repr(C)]
pub enum ActteyError<Data = ()> {
    #[error("gpu adapter error: `{0}`")]
    GpuAdapter(String, Data),
    #[error("gpu device error: `{0}`")]
    GpuDevice(String, Data),
    #[error("gpu feature error: `{0}`")]
    GpuFeature(String, Data),
    #[error("gpu limit error: `{0}`")]
    GpuLimit(String, Data),
    #[error("gpu pipeline error: `{0}`")]
    GpuPipeline(String, Data),
    #[error("gpu render error: `{0}`")]
    GpuRender(String, Data),
    #[error("gpu buffer error: `{0}`")]
    GpuBuffer(String, Data),
    #[error("conflict: `{0}`")]
    Conflict(String, Data),
    #[error("{0:?}")]
    Ecs(EcsError<Data>),
}

impl<Data> fmt::Debug for ActteyError<Data> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::GpuAdapter(reason, ..) => {
                write!(f, "ActteyError::GpuAdapter({reason}, ..)")
            }
            Self::GpuDevice(reason, ..) => {
                write!(f, "ActteyError::GpuDevice({reason}, ..)")
            }
            Self::GpuFeature(reason, ..) => {
                write!(f, "ActteyError::GpuFeature({reason}, ..)")
            }
            Self::GpuLimit(reason, ..) => {
                write!(f, "ActteyError::GpuLimits({reason}, ..)")
            }
            Self::GpuPipeline(reason, ..) => {
                write!(f, "ActteyError::GpuPipeline({reason}, ..)")
            }
            Self::GpuRender(reason, ..) => {
                write!(f, "ActteyError::GpuRender({reason}, ..)")
            }
            Self::GpuBuffer(reason, ..) => {
                write!(f, "ActteyError::GpuBuffer({reason}, ..)")
            }
            Self::Conflict(reason, ..) => {
                write!(f, "ActteyError::Conflict({reason}, ..)")
            }
            Self::Ecs(e) => {
                write!(f, "{e:?}")
            }
        }
    }
}

impl<Data> ActteyError<Data> {
    pub fn reason(&self) -> &str {
        match self {
            Self::GpuAdapter(reason, ..) => reason,
            Self::GpuDevice(reason, ..) => reason,
            Self::GpuFeature(reason, ..) => reason,
            Self::GpuLimit(reason, ..) => reason,
            Self::GpuPipeline(reason, ..) => reason,
            Self::GpuRender(reason, ..) => reason,
            Self::GpuBuffer(reason, ..) => reason,
            Self::Conflict(reason, ..) => reason,
            Self::Ecs(e) => e.reason(),
        }
    }

    pub fn take_data(self) -> Data {
        match self {
            Self::GpuAdapter(_, data) => data,
            Self::GpuDevice(_, data) => data,
            Self::GpuFeature(_, data) => data,
            Self::GpuLimit(_, data) => data,
            Self::GpuPipeline(_, data) => data,
            Self::GpuRender(_, data) => data,
            Self::GpuBuffer(_, data) => data,
            Self::Conflict(_, data) => data,
            Self::Ecs(e) => e.take_data(),
        }
    }

    pub fn without_data(self) -> ActteyError<()> {
        self.with_data(())
    }

    pub fn with_data<OutData>(self, data: OutData) -> ActteyError<OutData> {
        self.map_data(|_| data)
    }

    pub fn map_data<F, OutData>(self, f: F) -> ActteyError<OutData>
    where
        F: FnOnce(Data) -> OutData,
    {
        match self {
            Self::GpuAdapter(reason, old) => ActteyError::GpuAdapter(reason, f(old)),
            Self::GpuDevice(reason, old) => ActteyError::GpuDevice(reason, f(old)),
            Self::GpuFeature(reason, old) => ActteyError::GpuFeature(reason, f(old)),
            Self::GpuLimit(reason, old) => ActteyError::GpuLimit(reason, f(old)),
            Self::GpuPipeline(reason, old) => ActteyError::GpuPipeline(reason, f(old)),
            Self::GpuRender(reason, old) => ActteyError::GpuRender(reason, f(old)),
            Self::GpuBuffer(reason, old) => ActteyError::GpuBuffer(reason, f(old)),
            Self::Conflict(reason, old) => ActteyError::Conflict(reason, f(old)),
            Self::Ecs(e) => ActteyError::Ecs(e.map_data(f)),
        }
    }
}
