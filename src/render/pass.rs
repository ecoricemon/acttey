use crate::{
    debug_format,
    ds::{
        generational::{GenIndexRc, GenVecRc},
        graph::DirectedGraph,
    },
    impl_from_for_enum,
    render::{
        canvas::{Surface, SurfacePack, SurfacePackBuffer},
        Gpu, RenderError,
    },
};
use std::{num::NonZeroU64, ops::Range, rc::Rc, sync::Arc};

/// Directed acyclic graph representing pass command dependency.
/// Direction here means `From` command *requires* `To` command to be executed beforehand.
#[derive(Debug, Clone)]
pub struct PassGraph {
    gpu: Rc<Gpu>,

    /// Common label.
    /// Pass label is formatted like `label_0`.
    label: Arc<str>,

    /// Pass descriptors, which determines types of passes.
    /// Passes are executed in this order.
    descs: Vec<PassDesc>,

    graph: DirectedGraph<PassCmd>,

    /// Dirty flag.
    /// If this is true, next Self::run() will try to detect any cycles in this.
    dirty: bool,
}

impl PassGraph {
    pub fn new(label: Arc<str>, gpu: &Rc<Gpu>) -> Self {
        Self {
            gpu: Rc::clone(gpu),
            label,
            descs: Vec::new(),
            graph: DirectedGraph::new(),
            dirty: false,
        }
    }

    /// Adds a command node.
    pub fn insert_command(&mut self, cmd: impl Into<PassCmd>) -> usize {
        self.dirty = true;
        self.graph.insert_node(cmd.into())
    }

    /// Adds an edge between nodes.
    /// `from` node depends on `to` node, so that `to` node is executed first.
    pub fn add_dependency(&mut self, from: usize, to: usize) {
        self.dirty = true;
        self.graph.add_edge(from, to);
    }

    pub fn add_pass(&mut self, desc: PassDesc) {
        self.descs.push(desc);
    }

    pub fn validate(&mut self) -> Result<(), RenderError> {
        if self.dirty {
            self.dirty = false;
            if self.graph.has_cycle() {
                let errmsg = debug_format!("detected cycle in PassGraph({})", &*self.label);
                return Err(RenderError::CycleInPassGraph(errmsg));
            }
        }
        Ok(())
    }

    /// Runs the graph.
    /// It executes all render passes and compute passes in order they are added.
    pub fn run(
        &self,
        surf_packs: &GenVecRc<SurfacePack>,
        surf_pack_bufs: &mut Vec<SurfacePackBuffer>,
        surfaces: &GenVecRc<Surface>,
        visit_buf: &mut Vec<bool>,
    ) {
        assert!(!self.dirty, "call PassGraph::validate()");

        // Clears `visit_buf`, but we can reuse its capacity.
        visit_buf.clear();
        visit_buf.resize(self.graph.len_buf(), false);

        // Makes sure `surf_pack_bufs` is as long as passes. Each pass uses its own buffer.
        surf_pack_bufs.resize_with(self.descs.len(), SurfacePackBuffer::default);

        // Creates command encoder.
        let mut encoder = self.create_command_encoder();

        for (desc, sp_buf) in self.descs.iter().zip(surf_pack_bufs.iter_mut()) {
            match desc {
                PassDesc::Render(render_desc) => {
                    // Creates color attachments.
                    let colors;
                    let color_attachments = if let Some(sp_index) = &render_desc.sp_index {
                        let surf_pack = surf_packs.get(sp_index.index).unwrap();
                        colors = surf_pack.create_color_attachments(surfaces, sp_buf);
                        colors.as_slice()
                    } else {
                        &[]
                    };

                    // Creates a render pass.
                    let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                        label: Some(&render_desc.label),
                        color_attachments,
                        depth_stencil_attachment: None,
                        timestamp_writes: None,
                        occlusion_query_set: None,
                    });

                    // Executes commands.
                    for node_index in render_desc.nodes.iter() {
                        self.execute_node(&mut render_pass, *node_index, visit_buf);
                    }
                }
                PassDesc::Compute(_compute_desc) => {
                    unimplemented!()
                }
            }
        }

        // Submits command buffer.
        self.gpu.queue.submit(std::iter::once(encoder.finish()));
        for sp_buf in surf_pack_bufs.iter_mut() {
            SurfacePack::present(sp_buf);
        }
    }

    /// Executes nodes in depth first fashion.
    fn execute_node<'a: 'b, 'b>(
        &'a self,
        render_pass: &mut wgpu::RenderPass<'b>,
        index: usize,
        visit: &mut Vec<bool>,
    ) {
        if !visit[index] {
            visit[index] = true;
            for dep_index in self.graph.iter_outbound(index) {
                self.execute_node(render_pass, dep_index, visit);
            }
            self.graph[index].execute(render_pass);
        }
    }

    fn create_command_encoder(&self) -> wgpu::CommandEncoder {
        self.gpu
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some(&self.label),
            })
    }
}

/// Pass descriptor is used for generating a pass from [`wgpu::CommandEncoder`].  
/// If this is `Render`, then [`wgpu::RenderPass`] will be generated by the [`wgpu::CommandEncoder::begin_render_pass`].  
/// If this is `Compute`, then [`wgpu::ComputePass`] will be generated by the [`wgpu::CommandEncoder::begin_compute_pass`].  
#[derive(Debug, Clone)]
pub enum PassDesc {
    Render(RenderPassDesc),
    Compute(ComputePassDesc),
}

impl_from_for_enum!(PassDesc, Render, RenderPassDesc);
impl_from_for_enum!(PassDesc, Compute, ComputePassDesc);

/// A descriptor for single render pass.
#[derive(Debug, Clone)]
pub struct RenderPassDesc {
    /// Pass label.
    label: Arc<str>,

    /// Optional index pointing to [`SurfacePack`] for generating color attachments.
    sp_index: Option<GenIndexRc>,

    // TODO: depth stencils
    // something
    /// Draw command nodes.
    nodes: Vec<usize>,
}

impl RenderPassDesc {
    pub const fn new(label: Arc<str>, surface_pack_index: Option<GenIndexRc>) -> Self {
        Self {
            label,
            sp_index: surface_pack_index,
            nodes: Vec::new(),
        }
    }

    pub fn add_draw_command(&mut self, index: usize) {
        self.nodes.push(index);
    }
}

#[derive(Debug, Clone)]
pub struct ComputePassDesc {}

#[derive(Debug, Clone)]
pub enum PassCmd {
    None,
    SetBindGroup(SetBindGroupCmd),
    SetVertexBuffer(SetVertexBufferCmd),
    SetIndexBuffer(SetIndexBufferCmd),
    SetPipeline(SetPipelineCmd),
    DrawIndexed(DrawIndexedCmd),
}

impl_from_for_enum!(PassCmd, SetBindGroup, SetBindGroupCmd);
impl_from_for_enum!(PassCmd, SetVertexBuffer, SetVertexBufferCmd);
impl_from_for_enum!(PassCmd, SetIndexBuffer, SetIndexBufferCmd);
impl_from_for_enum!(PassCmd, SetPipeline, SetPipelineCmd);
impl_from_for_enum!(PassCmd, DrawIndexed, DrawIndexedCmd);

impl Default for PassCmd {
    fn default() -> Self {
        Self::None
    }
}

impl PassCmd {
    pub fn execute<'a>(&'a self, render_pass: &mut wgpu::RenderPass<'a>) {
        match self {
            Self::None => {}
            Self::SetBindGroup(cmd) => {
                render_pass.set_bind_group(cmd.index, &cmd.bind_group, &[]);
            }
            Self::SetVertexBuffer(cmd) => {
                render_pass.set_vertex_buffer(cmd.slot, cmd.get_buffer_slice());
            }
            Self::SetIndexBuffer(cmd) => {
                render_pass.set_index_buffer(cmd.get_buffer_slice(), cmd.format);
            }
            Self::SetPipeline(cmd) => {
                render_pass.set_pipeline(&cmd.pipeline);
            }
            Self::DrawIndexed(cmd) => {
                render_pass.draw_indexed(
                    cmd.indices.clone(),
                    cmd.base_vertex,
                    cmd.instances.clone(),
                );
            }
        }
    }
}

/// Corresponding to [`wgpu::RenderPass::set_bind_group`].
#[derive(Debug, Clone)]
pub struct SetBindGroupCmd {
    index: u32,
    bind_group: Rc<wgpu::BindGroup>,
}

impl SetBindGroupCmd {
    pub fn new(index: u32, bind_group: Rc<wgpu::BindGroup>) -> Self {
        Self { index, bind_group }
    }
}

/// Corresponding to [`wgpu::RenderPass::set_vertex_buffer`].
#[derive(Debug, Clone)]
pub struct SetVertexBufferCmd {
    buf: Rc<wgpu::Buffer>,
    offset: u64,
    size: Option<NonZeroU64>,
    slot: u32,
}

impl SetVertexBufferCmd {
    /// If you set `size` to zero, then total range of the buffer is used.
    pub fn new(buf: Rc<wgpu::Buffer>, offset: u64, size: u64, slot: u32) -> Self {
        Self {
            buf,
            offset,
            size: NonZeroU64::new(size),
            slot,
        }
    }

    pub fn get_buffer_slice(&self) -> wgpu::BufferSlice {
        if let Some(size) = self.size {
            self.buf.slice(self.offset..self.offset + size.get())
        } else {
            self.buf.slice(self.offset..)
        }
    }
}

/// Corresponding to [`wgpu::RenderPass::set_index_buffer`].
#[derive(Debug, Clone)]
pub struct SetIndexBufferCmd {
    buf: Rc<wgpu::Buffer>,
    offset: u64,
    size: Option<NonZeroU64>,
    format: wgpu::IndexFormat,
}

impl SetIndexBufferCmd {
    pub fn new(buf: Rc<wgpu::Buffer>, offset: u64, size: u64, format: wgpu::IndexFormat) -> Self {
        Self {
            buf,
            offset,
            size: NonZeroU64::new(size),
            format,
        }
    }

    pub fn get_buffer_slice(&self) -> wgpu::BufferSlice {
        if let Some(size) = self.size {
            self.buf.slice(self.offset..self.offset + size.get())
        } else {
            self.buf.slice(self.offset..)
        }
    }
}

/// Corresponding to [`wgpu::RenderPass::set_pipeline`].
#[derive(Debug, Clone)]
pub struct SetPipelineCmd {
    pipeline: Rc<wgpu::RenderPipeline>,
}

impl SetPipelineCmd {
    pub fn new(pipeline: Rc<wgpu::RenderPipeline>) -> Self {
        Self { pipeline }
    }
}

/// Corresponding to [`wgpu::RenderPass::draw_indexed`].
#[derive(Debug, Clone)]
pub struct DrawIndexedCmd {
    indices: Range<u32>,
    base_vertex: i32,
    instances: Range<u32>,
}

impl DrawIndexedCmd {
    pub fn new(indices: Range<u32>, base_vertex: i32, instances: Range<u32>) -> Self {
        Self {
            indices,
            base_vertex,
            instances,
        }
    }
}
