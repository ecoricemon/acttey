use super::{
    bind::exe::BindGroupCommand,
    context::Gpu,
    fragment::{
        self,
        exe::{ColorAttachmentPack, DepthStencilAttachment},
    },
    pipeline::exe::RenderPipelineCommand,
    vertex::exe::{IndexBufferCommand, VertexBufferCommand},
};
use crate::{ds::share::DebugLockKey, impl_from_for_enum};
use std::{iter, ops::Range, rc::Rc, sync::Arc};

/// Render module consists of Resource(res), Description(desc), and Execution(exe) layers.
/// Execution layer is responsible for executing render or compute passes.
pub(crate) mod exe {
    use super::*;

    #[derive(Debug)]
    pub(crate) struct PassPack {
        gpu: Rc<Gpu>,
        passes: Vec<Pass>,
    }

    impl PassPack {
        /// You can append pass later.
        pub(crate) const fn new(gpu: Rc<Gpu>, passes: Vec<Pass>) -> Self {
            Self { gpu, passes }
        }

        pub(crate) fn append_pass(&mut self, pass: Pass) {
            self.passes.push(pass);
        }

        pub(crate) fn execute(&mut self) {
            // Some shared variables like textures must not change until encode finishes.
            // We're not updating them during this execution for sure,
            // so just checks for it in debug mode only.
            #[cfg(debug_assertions)]
            let frag_mod_lock = DebugLockKey::lock(&fragment::FRAG_MOD_DEBUG_LOCK);

            // Creates command encoder.
            let mut encoder = self
                .gpu
                .device
                .create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });

            // Executes passes in their order.
            for pass in self.passes.iter_mut() {
                pass.execute(&mut encoder);
            }

            // Submits command buffer.
            self.gpu.queue.submit(iter::once(encoder.finish()));

            #[cfg(debug_assertions)]
            drop(frag_mod_lock);

            // Presents textures on surfaces.
            for pass in self.passes.iter_mut() {
                match pass {
                    Pass::Render(rpass) => rpass.colors.present(),
                    Pass::Compute(_) => {}
                }
            }
        }
    }

    #[derive(Debug)]
    pub(crate) enum Pass {
        Render(RenderPass),
        Compute(ComputePass),
    }
    impl_from_for_enum!(Pass, Render, RenderPass);
    impl_from_for_enum!(Pass, Compute, ComputePass);

    impl Pass {
        pub(crate) fn execute(&mut self, encoder: &mut wgpu::CommandEncoder) {
            match self {
                Self::Render(render) => render.execute(encoder),
                Self::Compute(_) => todo!(),
            }
        }
    }

    #[derive(Debug)]
    pub(crate) struct RenderPass {
        label: Arc<str>,
        colors: ColorAttachmentPack,
        depth_stencil: Option<DepthStencilAttachment>,
        ops: PassOps,
    }

    impl RenderPass {
        pub(crate) fn new(
            label: Arc<str>,
            colors: ColorAttachmentPack,
            depth_stencil: Option<DepthStencilAttachment>,
            mut ops: PassOps,
        ) -> Self {
            // Exploits reusable commands.
            ops.dedup();

            Self {
                label,
                colors,
                depth_stencil,
                ops,
            }
        }

        /// Calls to [`wgpu::CommandEncoder::begin_render_pass`] followed by set and draw calls.
        /// And drops [`wgpu::RenderPass`] immediately.
        ///
        /// Don't forget to submit command buffer and call present method on surfaces
        /// after execution of all passes.
        pub(crate) fn execute(&mut self, encoder: &mut wgpu::CommandEncoder) {
            let color_attachments = self.colors.as_color_attachments();
            let depth_stencil_attachment = self
                .depth_stencil
                .as_ref()
                .map(|attach| attach.as_attachment());

            let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some(&self.label),
                color_attachments,
                depth_stencil_attachment,
                timestamp_writes: None,
                occlusion_query_set: None,
            });

            self.ops.execute_render_pass(&mut pass);
        }
    }

    #[derive(Debug)]
    pub(crate) struct ComputePass {}

    #[derive(Debug)]
    pub enum PassDesc {
        Render {},
        Compute { label: Arc<str>, ops: PassOps },
    }

    #[derive(Debug, Clone)]
    pub struct PassOps {
        cmds: Vec<PassCommand>,

        /// Operations that will be executed in this order.
        ops: Vec<Operation>,
    }

    impl PassOps {
        pub const fn new() -> Self {
            Self {
                cmds: Vec::new(),
                ops: Vec::new(),
            }
        }

        pub fn add_command(&mut self, cmd: PassCommand) -> usize {
            self.cmds.push(cmd);
            self.cmds.len() - 1
        }

        pub fn append_operation(&mut self, op: Operation) {
            self.ops.push(op);
        }

        pub(crate) fn execute_render_pass<'a: 'b, 'b>(&'a self, pass: &mut wgpu::RenderPass<'b>) {
            // Executes `Operation` in their order.
            for op in self.ops.iter() {
                // Executes dependent commands.
                for i in op.dep_indices.iter() {
                    self.cmds[*i].execute_render_pass(pass);
                }

                // Executes operational command.
                self.cmds[op.op_index].execute_render_pass(pass);
            }
        }

        pub(crate) fn dedup(&mut self) {
            #[cfg(debug_assertions)]
            self.validate();

            let mut set_binds: [Option<&BindGroupCommand>; 4] = [None; 4];
            let mut set_idx: Option<&IndexBufferCommand> = None;
            let mut set_verts: [Option<&VertexBufferCommand>; 8] = [None; 8];
            let mut set_pipe: Option<&RenderPipelineCommand> = None;

            let mut dup = Vec::new();
            for op in self.ops.iter_mut() {
                for i in op.dep_indices.iter().cloned() {
                    match &self.cmds[i] {
                        PassCommand::None => panic!(),
                        PassCommand::Bind(bind) => {
                            let bi = bind.index() as usize;
                            if matches!(set_binds[bi], Some(set) if set == bind) {
                                dup.push(i);
                            }
                            set_binds[bi] = Some(bind);
                        }
                        PassCommand::Index(idx) => {
                            if matches!(set_idx, Some(set) if set == idx) {
                                dup.push(i);
                            }
                            set_idx = Some(idx);
                        }
                        PassCommand::Vertex(vert) => {
                            let vi = vert.slot() as usize;
                            if matches!(set_verts[vi], Some(set) if set == vert) {
                                dup.push(i);
                            }
                            set_verts[vi] = Some(vert);
                        }
                        PassCommand::RenderPipeline(pipe) => {
                            if matches!(set_pipe, Some(set) if set == pipe) {
                                dup.push(i);
                            }
                            set_pipe = Some(pipe);
                        }
                        PassCommand::Draw(..) => unreachable!(),
                    }
                }
                dup.sort_unstable();
                while let Some(i) = dup.pop() {
                    op.dep_indices.remove(i);
                }
            }
        }

        /// # Panics
        ///
        /// Panics if the conditions below are not met.
        /// - All commands must be unique.
        /// - Operation's indices must be valid.
        #[cfg(debug_assertions)]
        fn validate(&self) {
            // Is every command different from each other?
            for i in 0..self.cmds.len() {
                for j in i + 1..self.cmds.len() {
                    assert_ne!(self.cmds.get(i), self.cmds.get(j));
                }
            }

            // Is every operation valid?
            let mut visit = vec![false; self.cmds.len()];
            let is_valid_op = self.ops.iter().all(|op| {
                // `Operation` must point to operational command.
                let is_valid_op_index = self.cmds[op.op_index].is_operational();

                // `Operation` must point to setting commands as dependencies.
                // Plus, it must not contain duplication.
                visit.fill(false);
                is_valid_op_index
                    && op.dep_indices.iter().all(|&i| {
                        let res = self.cmds[i].is_setting() && !visit[i];
                        visit[i] = true;
                        res
                    })
            });
            assert!(is_valid_op);
        }
    }

    #[derive(Debug, Clone)]
    pub struct Operation {
        /// Operational command index which is one of draw or dispatch commands.
        pub(crate) op_index: usize,

        /// Dependent command indices.
        pub(crate) dep_indices: Vec<usize>,
    }

    impl Operation {
        pub const fn new(op: usize, deps: Vec<usize>) -> Self {
            Self {
                op_index: op,
                dep_indices: deps,
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub(crate) enum PassCommand {
        None,
        Bind(BindGroupCommand),
        Index(IndexBufferCommand),
        Vertex(VertexBufferCommand),
        RenderPipeline(RenderPipelineCommand),
        Draw(DrawCommand),
    }

    impl PassCommand {
        /// Determines the command is operational command or not.
        pub(crate) fn is_operational(&self) -> bool {
            match self {
                Self::None => false,               // No command, so it's not operational.
                Self::Bind(..) => false,           // Setting command.
                Self::Index(..) => false,          // Setting command.
                Self::Vertex(..) => false,         // Setting command.
                Self::RenderPipeline(..) => false, // Setting command.
                Self::Draw(..) => true,
            }
        }

        /// Determines the command is setting command or not.
        pub(crate) fn is_setting(&self) -> bool {
            match self {
                Self::None => false,
                _ => !self.is_operational(),
            }
        }

        pub(crate) fn execute_render_pass<'a: 'b, 'b>(&'a self, pass: &mut wgpu::RenderPass<'b>) {
            match self {
                Self::None => panic!(),
                Self::Bind(bind) => bind.execute_render_pass(pass),
                Self::Index(idx) => idx.execute(pass),
                Self::Vertex(vert) => vert.execute(pass),
                Self::RenderPipeline(pipe) => pipe.execute(pass),
                Self::Draw(draw) => draw.execute(pass),
            }
        }
    }

    impl Default for PassCommand {
        fn default() -> Self {
            Self::None
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub(crate) struct DrawCommand {
        indices: Range<u32>,
        base_vertex: i32,
        instances: Range<u32>,
    }

    impl DrawCommand {
        pub(crate) const fn new(
            indices: Range<u32>,
            base_vertex: i32,
            instances: Range<u32>,
        ) -> Self {
            Self {
                indices,
                base_vertex,
                instances,
            }
        }

        pub(crate) fn execute<'a: 'b, 'b>(&'a self, pass: &mut wgpu::RenderPass<'b>) {
            pass.draw_indexed(
                self.indices.clone(),
                self.base_vertex,
                self.instances.clone(),
            );
        }
    }
}

/// Render module consists of Resource(res), Description(desc), and Execution(exe) layers.
/// Description layer is responsible for describing GPU state using something like pipeline.
pub(crate) mod desc {}

/// Render module consists of Resource(res), Description(desc), and Execution(exe) layers.
/// Resource layer is responsible for holding GPU relative data.
pub(crate) mod res {}
