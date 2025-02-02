pub(crate) mod bind;
pub(crate) mod buffer;
pub(crate) mod common;
pub(crate) mod context;
pub(crate) mod encoder;
pub(crate) mod pipeline;
pub(crate) mod shader;

pub mod prelude {
    pub use super::bind::{BindGroup, BindGroupLayoutStorage, BindGroupStorage};
    pub use super::buffer::{GpuBuffer, GpuBufferStorage};
    pub use super::context::Gpu;
    pub use super::encoder::{
        CommandEncoder, CommandEncoderStorage, ComputePass, ComputePassCommand,
    };
    pub use super::pipeline::{ComputePipeline, ComputePipelineStorage, PipelineLayoutStorage};
    pub use super::shader::{EntryPoint, Shader, ShaderStorage};
}
