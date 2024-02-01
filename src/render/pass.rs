use ahash::{AHashMap, AHashSet};
use bytemuck::CheckedBitPattern;

use crate::{
    ds::generational::{GenIndex, GenIndexRc, GenVec, GenVecRc}, impl_from_for_enum, render::{
        canvas::{Surface, SurfacePack, SurfacePackBuffer},
        Gpu,
        buffer::BufferSlice,
    }, util::RcStr
};
use std::{rc::Rc, ops::Range};

/// Directed acyclic graph representing pass command dependency.
/// Direction here means `From` command *requires* `To` command to be executed beforehand.
#[derive(Debug, Clone)]
pub struct PassGraph {
    gpu: Rc<Gpu>,

    /// Common label.
    /// Pass label is formatted like `label_0`.
    label: RcStr,

    /// Pass descriptors, which determines types of passes.
    /// Passes are executed in this order.
    descs: Vec<PassDesc>,

    /// Nodes containing pass commands.
    /// Length of this is always same with the things of `outbounds` and `inbounds`.
    nodes: Vec<PassCmd>,

    /// Outgoing edges of each node that explain execution order.
    /// A -> B means A depends on B, so that B must be executed first.
    /// A is index and B is value in here.
    outbounds: Vec<AHashSet<usize>>,

    /// Incoming edges of each node that explain execution order.
    /// B <- A means B must be executed before running A.
    /// A is index and B is value in here.
    inbounds: Vec<AHashSet<usize>>,

    /// Dirty flag.
    /// If this is true, next Self::run() will try to detect any cycles in this.
    dirty: bool,
}

impl PassGraph {
    pub fn new(label: impl Into<RcStr>, gpu: &Rc<Gpu>) -> Self {
        Self {
            gpu: Rc::clone(gpu),
            label: label.into(),
            descs: Vec::new(),
            nodes: Vec::new(),
            outbounds: Vec::new(),
            inbounds: Vec::new(),
            dirty: false,
        }
    }

    /// Adds a command node.
    pub fn add_node(&mut self, cmd: impl Into<PassCmd>) -> usize {
        self.dirty = true;

        let cmd = cmd.into();
        self.nodes.push(cmd);
        self.outbounds.push(AHashSet::new());
        self.inbounds.push(AHashSet::new());
        self.nodes.len() - 1
    }

    /// Adds an edge between nodes.
    /// `from` node depends on `to` node, so that `to` node is executed first.
    pub fn add_edge(&mut self, from: usize, to: usize) {
        self.dirty = true;

        self.outbounds[from].insert(to);
        self.inbounds[to].insert(from);
    }

    pub fn add_pass(&mut self, desc: PassDesc) {
        self.descs.push(desc);
    }

    pub fn run(
        &self, 
        surf_packs: &GenVecRc<SurfacePack>,
        surf_pack_buf: &mut SurfacePackBuffer,
        surfaces: &GenVecRc<Surface>,
        visit_buf: &mut Vec<bool>,
    ) {
        // It's possible to run graph without cycle.
        #[cfg(debug_assertions)]
        {
            if self.has_cycle() {
                panic!()
            }
        }

        // Clears `visit_buf`, but we can reuse its capacity.
        visit_buf.clear();
        visit_buf.resize(self.nodes.len(), false);

        // Creates command encoder.
        let mut encoder = self.create_command_encoder();

        for desc in self.descs.iter() {
            match desc {
                PassDesc::Render(render_desc) => {
                    // Creates color attachments.
                    let colors;
                    let color_attachments = if let Some(sp_index) = &render_desc.sp_index {
                        let surf_pack = surf_packs.get(sp_index.index).unwrap();
                        colors = surf_pack.create_color_attachments(surfaces, surf_pack_buf);
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
                PassDesc::Compute(compute_desc) => {
                    unimplemented!()
                }
            }
        }

        self.gpu.queue.submit(std::iter::once(encoder.finish()));
    }

    // TODO: test
    /// Determines whether this graph has cycle in it.
    pub fn has_cycle(&self) -> bool {
        if self.dirty {
            let mut visit = vec![false; self.nodes.len()];
            let mut path_visit = vec![false; self.nodes.len()];

            // Returns true if it's detected a cycle.
            fn find(
                v: usize, 
                visit: &mut [bool], 
                path_visit: &mut [bool],
                edges: &[AHashSet<usize>],
            ) -> bool {
                if !visit[v] {
                    visit[v] = true;
                    path_visit[v] = true;
                    let res = edges[v].iter().any(|&w| {
                        find(w, visit, path_visit, edges) || path_visit[w]
                    });
                    path_visit[v] = false;
                    res
                } else {
                    false
                }
            }

            (0..self.nodes.len())
                .any(|v| {
                    find(v, &mut visit, &mut path_visit, &self.outbounds)
                })
        } else {
            false
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
            for dep_index in self.outbounds[index].iter() {
                self.execute_node(render_pass, *dep_index, visit);
            }
            self.nodes[index].execute(render_pass);
        }
    }

    fn create_command_encoder(&self) -> wgpu::CommandEncoder {
        self
            .gpu
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
    label: RcStr,

    /// Optional index pointing to [`SurfacePack`] for generating color attachments.
    sp_index: Option<GenIndexRc>,

    // TODO: depth stencils
    // something

    /// Draw command nodes.
    nodes: Vec<usize>,
}

impl RenderPassDesc {
    pub fn new(label: impl Into<RcStr>, surface_pack_index: Option<GenIndexRc>) -> Self {
        Self {
            label: label.into(),
            sp_index: surface_pack_index,
            nodes: Vec::new()
        }
    }

    pub fn add_draw_command(&mut self, index: usize) {
        self.nodes.push(index);
    }
}

#[derive(Debug, Clone)]
pub struct ComputePassDesc {
    label: RcStr,
}

#[derive(Debug, Clone)]
pub enum PassCmd {
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

impl PassCmd {
    pub fn execute<'a>(&'a self, render_pass: &mut wgpu::RenderPass<'a>) {
        match self {
            Self::SetBindGroup(cmd) => {
                render_pass.set_bind_group(cmd.index, &cmd.bind_group, &[]);
            },
            Self::SetVertexBuffer(cmd) => {
                render_pass.set_vertex_buffer(cmd.slot, cmd.buf_slice.as_slice());
            },
            Self::SetIndexBuffer(cmd) => {
                render_pass.set_index_buffer(cmd.buf_slice.as_slice(), cmd.format);
            },
            Self::SetPipeline(cmd) => {
                render_pass.set_pipeline(&cmd.pipeline);
            },
            Self::DrawIndexed(cmd) => {
                render_pass.draw_indexed(cmd.indices.clone(), cmd.base_vertex, cmd.instances.clone());
            }
        }
    }
}

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

#[derive(Debug, Clone)]
pub struct SetVertexBufferCmd {
	buf_slice: BufferSlice,
	slot: u32,
}

impl SetVertexBufferCmd {
    pub fn new(buf: Rc<wgpu::Buffer>, offset: u64, size: u64, slot: u32) -> Self {
        Self {
            buf_slice: BufferSlice {
                buf,
                offset,
                size,
            },
            slot,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SetIndexBufferCmd {
	buf_slice: BufferSlice,
	format: wgpu::IndexFormat,
}

impl SetIndexBufferCmd {
    pub fn new(buf: Rc<wgpu::Buffer>, offset: u64, size: u64, format: wgpu::IndexFormat) -> Self {
        Self {
            buf_slice: BufferSlice {
                buf,
                offset,
                size,
            },
            format,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SetPipelineCmd {
	pipeline: Rc<wgpu::RenderPipeline>,
}

impl SetPipelineCmd {
    pub fn new(pipeline: Rc<wgpu::RenderPipeline>) -> Self {
        Self { pipeline }
    }
}

#[derive(Debug, Clone)]
pub struct DrawIndexedCmd {
	indices: Range<u32>,
	base_vertex: i32,
	instances: Range<u32>,
}

impl DrawIndexedCmd {
    pub fn new(indices: Range<u32>, base_vertex: i32, instances: Range<u32>) -> Self {
        Self { indices, base_vertex, instances }
    }
}
