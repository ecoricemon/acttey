use super::{
    bind::BindGroup,
    buffer::GpuBuffer,
    common::{HasLabel, LabelledGenVec},
    pipeline::ComputePipeline,
};
use crate::{
    ActteyError,
    util::{AsOr, StaticStr},
};
use my_ecs::prelude::{Or, Resource, ResourceId, ResourceIndex};
use std::{
    ops::{Deref, DerefMut},
    sync::Arc,
};

#[derive(Debug, Resource)]
pub struct CommandEncoderStorage {
    dev: Arc<wgpu::Device>,
    ri: ResourceIndex,
    encoders: LabelledGenVec<CommandEncoder>,
}

impl CommandEncoderStorage {
    pub(crate) fn new(dev: Arc<wgpu::Device>) -> Self {
        Self {
            dev,
            ri: ResourceIndex::dummy(),
            encoders: LabelledGenVec::new(),
        }
    }

    pub(crate) fn set_resource_index(&mut self, ri: ResourceIndex) {
        self.ri = ri;
    }

    pub fn create(&self, desc: &wgpu::CommandEncoderDescriptor<'_>) -> CommandEncoder {
        let label = desc
            .label
            .map(|s| StaticStr::new(s.to_owned()))
            .unwrap_or_default();
        let dev = Arc::clone(&self.dev);
        CommandEncoder::new(label, dev)
    }

    pub fn add(&mut self, encoder: CommandEncoder) -> Result<ResourceId, ActteyError> {
        debug_assert!(!self.ri.is_dummy());

        let ii = self.encoders.add(encoder)?;
        let rid = ResourceId::new(self.ri, ii);
        Ok(rid)
    }

    pub fn remove<K>(&mut self, key: K) -> Option<CommandEncoder>
    where
        K: AsOr<ResourceId, str>,
    {
        return inner(self, key.as_or());

        fn inner(
            this: &mut CommandEncoderStorage,
            key: Or<&ResourceId, &str>,
        ) -> Option<CommandEncoder> {
            if matches!(key, Or::A(rid) if rid.resource_index() != this.ri) {
                return None;
            }
            let key = key.map_a(ResourceId::item_index);
            this.encoders.remove(key)
        }
    }

    pub fn get<K>(&self, key: K) -> Option<&CommandEncoder>
    where
        K: AsOr<ResourceId, str>,
    {
        return inner(self, key.as_or());

        fn inner<'s>(
            this: &'s CommandEncoderStorage,
            key: Or<&ResourceId, &str>,
        ) -> Option<&'s CommandEncoder> {
            if matches!(key, Or::A(rid) if rid.resource_index() != this.ri) {
                return None;
            }
            let key = key.map_a(ResourceId::item_index);
            this.encoders.get(key)
        }
    }

    pub fn get_mut<K>(&mut self, key: K) -> Option<&mut CommandEncoder>
    where
        K: AsOr<ResourceId, str>,
    {
        return inner(self, key.as_or());

        fn inner<'s>(
            this: &'s mut CommandEncoderStorage,
            key: Or<&ResourceId, &str>,
        ) -> Option<&'s mut CommandEncoder> {
            if matches!(key, Or::A(rid) if rid.resource_index() != this.ri) {
                return None;
            }
            let key = key.map_a(ResourceId::item_index);
            this.encoders.get_mut(key)
        }
    }
}

#[derive(Debug)]
pub struct CommandEncoder {
    dev: Arc<wgpu::Device>,
    label: StaticStr,
    cmds: Vec<GpuCommand>,
}

impl CommandEncoder {
    const fn new(label: StaticStr, dev: Arc<wgpu::Device>) -> Self {
        Self {
            label,
            dev,
            cmds: Vec::new(),
        }
    }

    pub fn begin_compute_pass(
        &mut self,
        desc: &wgpu::ComputePassDescriptor<'_>,
    ) -> ComputePassEncoder {
        let label = desc
            .label
            .map(|s| StaticStr::new(s.to_owned()))
            .unwrap_or_default();
        ComputePassEncoder {
            encoder: self,
            pass: ComputePass::new(label),
        }
    }

    pub fn copy_buffer_to_buffer(
        &mut self,
        source: GpuBuffer,
        source_offset: wgpu::BufferAddress,
        destination: GpuBuffer,
        destination_offset: wgpu::BufferAddress,
        copy_size: wgpu::BufferAddress,
    ) {
        let cmd = GpuCommand::CopyBufferToBuffer {
            source,
            source_offset,
            destination,
            destination_offset,
            copy_size,
        };
        self.cmds.push(cmd);
    }

    pub fn encode(&self) -> wgpu::CommandBuffer {
        let opt_label = if !self.label().is_empty() {
            Some(self.label())
        } else {
            None
        };
        let mut enc = self
            .dev
            .create_command_encoder(&wgpu::CommandEncoderDescriptor { label: opt_label });
        for cmd in self.cmds.iter() {
            cmd.encode(&mut enc);
        }
        enc.finish()
    }
}

impl HasLabel for CommandEncoder {
    fn label(&self) -> &str {
        &self.label
    }
}

impl Deref for CommandEncoder {
    type Target = Vec<GpuCommand>;

    fn deref(&self) -> &Self::Target {
        &self.cmds
    }
}

impl DerefMut for CommandEncoder {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.cmds
    }
}

#[derive(Debug)]
pub enum GpuCommand {
    Render(RenderPass),
    Compute(ComputePass),
    CopyBufferToBuffer {
        source: GpuBuffer,
        source_offset: wgpu::BufferAddress,
        destination: GpuBuffer,
        destination_offset: wgpu::BufferAddress,
        copy_size: wgpu::BufferAddress,
    },
}

impl GpuCommand {
    fn encode(&self, enc: &mut wgpu::CommandEncoder) {
        match self {
            Self::Render(..) => {
                unimplemented!()
            }
            Self::Compute(pass) => pass.encode(enc),
            Self::CopyBufferToBuffer {
                source,
                source_offset,
                destination,
                destination_offset,
                copy_size,
            } => {
                enc.copy_buffer_to_buffer(
                    source,
                    *source_offset,
                    destination,
                    *destination_offset,
                    *copy_size,
                );
            }
        }
    }
}

#[derive(Debug)]
pub struct ComputePassEncoder<'e> {
    encoder: &'e mut CommandEncoder,
    pass: ComputePass,
}

impl ComputePassEncoder<'_> {
    pub fn end(self) {
        let pass = GpuCommand::Compute(self.pass);
        self.encoder.push(pass);
    }
}

impl Deref for ComputePassEncoder<'_> {
    type Target = ComputePass;

    fn deref(&self) -> &Self::Target {
        &self.pass
    }
}

impl DerefMut for ComputePassEncoder<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.pass
    }
}

#[derive(Debug, Default)]
pub struct RenderPass;

#[derive(Debug, Default)]
pub struct ComputePass {
    label: StaticStr,
    cmds: Vec<ComputePassCommand>,
}

impl ComputePass {
    const fn new(label: StaticStr) -> Self {
        Self {
            label,
            cmds: Vec::new(),
        }
    }

    /// A helper method to append a [`ComputePassCommand`] associated with
    /// setting a pipeline.
    pub fn set_pipeline(&mut self, pipe: ComputePipeline) -> &mut Self {
        let cmd = ComputePassCommand::SetPipeline { pipe };
        self.cmds.push(cmd);
        self
    }

    /// A helper method to append a [`ComputePassCommand`] associated with
    /// setting a bind group.
    pub fn set_bind_group(&mut self, index: u32, bind_group: BindGroup) -> &mut Self {
        let cmd = ComputePassCommand::SetBindGroup { index, bind_group };
        self.cmds.push(cmd);
        self
    }

    /// A helper method to append a [`ComputePassCommand`] associated with
    /// dispathing workgoups.
    pub fn dispatch_workgroups(&mut self, x: u32, y: u32, z: u32) -> &mut Self {
        let cmd = ComputePassCommand::DispatchWorkgroups { x, y, z };
        self.cmds.push(cmd);
        self
    }

    fn encode(&self, enc: &mut wgpu::CommandEncoder) {
        let opt_label = if !self.label().is_empty() {
            Some(self.label())
        } else {
            None
        };
        let mut pass = enc.begin_compute_pass(&wgpu::ComputePassDescriptor {
            label: opt_label,
            timestamp_writes: None,
        });
        for cmd in self.cmds.iter() {
            cmd.encode(&mut pass);
        }
    }
}

impl HasLabel for ComputePass {
    fn label(&self) -> &str {
        &self.label
    }
}

impl Deref for ComputePass {
    type Target = Vec<ComputePassCommand>;

    fn deref(&self) -> &Self::Target {
        &self.cmds
    }
}

impl DerefMut for ComputePass {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.cmds
    }
}

#[derive(Debug)]
pub enum ComputePassCommand {
    SetPipeline { pipe: ComputePipeline },
    SetBindGroup { index: u32, bind_group: BindGroup },
    DispatchWorkgroups { x: u32, y: u32, z: u32 },
}

impl ComputePassCommand {
    fn encode(&self, pass: &mut wgpu::ComputePass) {
        match self {
            Self::SetPipeline { pipe } => {
                pass.set_pipeline(pipe);
            }
            Self::SetBindGroup { index, bind_group } => {
                pass.set_bind_group(*index, &**bind_group, &[]);
            }
            Self::DispatchWorkgroups { x, y, z } => {
                pass.dispatch_workgroups(*x, *y, *z);
            }
        }
    }
}
