use crate::{
    gpu::{
        bind::{BindGroupLayoutStorage, BindGroupStorage},
        buffer::GpuBufferStorage,
        context::Gpu,
        encoder::CommandEncoderStorage,
        pipeline::{ComputePipelineStorage, PipelineLayoutStorage},
        shader::ShaderStorage,
    },
    ActteyError,
};
use my_ecs::prelude::*;
use std::sync::Arc;

#[derive(Debug)]
pub struct GpuInit {
    gpu: Option<Gpu>,
}

impl GpuInit {
    pub async fn new(
        features: Option<wgpu::Features>,
        limits: Option<wgpu::Limits>,
    ) -> Result<Self, ActteyError> {
        let gpu = Gpu::new(features, limits).await?;
        Ok(Self { gpu: Some(gpu) })
    }
}

impl Command for GpuInit {
    fn command(&mut self, mut ecs: Ecs<'_>) -> DynResult<()> {
        // Registers `Gpu` as a resource if it doesn't exist.
        helper(&mut ecs, || Ok(self.gpu.take().unwrap()))?;
        let gpu = ecs.get_resource::<Gpu>().unwrap();
        let dev = Arc::clone(gpu.device());

        // Registers `GpuBufferStorage` as a resource if it doesn't exist.
        let ri = helper(&mut ecs, || Ok(GpuBufferStorage::new(Arc::clone(&dev))))?;
        let stor = ecs.get_resource_mut::<GpuBufferStorage>().unwrap();
        stor.set_resource_index(ri);

        // Registers `BindGroupLayoutStorage` as a resource if it doesn't exist.
        let ri = helper(&mut ecs, || {
            Ok(BindGroupLayoutStorage::new(Arc::clone(&dev)))
        })?;
        let stor = ecs.get_resource_mut::<BindGroupLayoutStorage>().unwrap();
        stor.set_resource_index(ri);

        // Registers `BindGroupStorage` as a resource if it doesn't exist.
        let ri = helper(&mut ecs, || Ok(BindGroupStorage::new(Arc::clone(&dev))))?;
        let stor = ecs.get_resource_mut::<BindGroupStorage>().unwrap();
        stor.set_resource_index(ri);

        // Registers `ShaderStorage` as a resource if it doesn't exist.
        let ri = helper(&mut ecs, || Ok(ShaderStorage::new(Arc::clone(&dev))))?;
        let stor = ecs.get_resource_mut::<ShaderStorage>().unwrap();
        stor.set_resource_index(ri);

        // Registers `PipelineLayoutStorage` as a resource if it doesn't exist.
        let ri = helper(&mut ecs, || {
            Ok(PipelineLayoutStorage::new(Arc::clone(&dev)))
        })?;
        let stor = ecs.get_resource_mut::<PipelineLayoutStorage>().unwrap();
        stor.set_resource_index(ri);

        // Registers `ComputePipelineStorage` as a resource if it doesn't exist.
        let ri = helper(&mut ecs, || {
            Ok(ComputePipelineStorage::new(Arc::clone(&dev)))
        })?;
        let stor = ecs.get_resource_mut::<ComputePipelineStorage>().unwrap();
        stor.set_resource_index(ri);

        // Registers `CommandEncoderStorage` as a resource if it doesn't exist.
        let ri = helper(&mut ecs, || {
            Ok(CommandEncoderStorage::new(Arc::clone(&dev)))
        })?;
        let stor = ecs.get_resource_mut::<CommandEncoderStorage>().unwrap();
        stor.set_resource_index(ri);

        return Ok(());

        // === Internal helper functions ===

        /// Adds `R` if it doesn't exist in ECS, then returns its index.
        fn helper<R, F>(ecs: &mut Ecs<'_>, f: F) -> DynResult<ResourceIndex>
        where
            R: Resource,
            F: FnOnce() -> DynResult<R>,
        {
            if let Some(ri) = ecs.get_resource_index::<R>() {
                Ok(ri)
            } else {
                let r = f()?;
                let is_web = cfg!(target_arch = "wasm32");
                let desc = ResourceDesc::new().with_owned(r).with_dedicated(is_web);
                let ri = ecs
                    .add_resource(desc)
                    .take()
                    .map_err(EcsError::without_data)?;
                Ok(ri)
            }
        }
    }
}

/// Safety: [`Resource`] requires [`Send`], but GPU resources on web environment
/// cannot be sent. However, `GpuInit` command registers them as dedicated
/// resources. So they will never be sent.
#[cfg(target_arch = "wasm32")]
mod impl_send_for_gpu_resources {
    use super::*;

    unsafe impl Send for Gpu {}
    unsafe impl Send for GpuBufferStorage {}
    unsafe impl Send for BindGroupStorage {}
    unsafe impl Send for BindGroupLayoutStorage {}
    unsafe impl Send for ComputePipelineStorage {}
    unsafe impl Send for PipelineLayoutStorage {}
    unsafe impl Send for ShaderStorage {}
    unsafe impl Send for CommandEncoderStorage {}
}
