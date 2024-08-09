use super::{
    bind::{
        desc::{StoreBindGroupState, StoreBindState},
        BindStateManager,
    },
    buffer::res::{BufferPool, BufferRc, SizeOrData, StoreBuffer},
    canvas::OffCanvas,
    context::{Gpu, RenderContext},
    fragment::{
        desc::{StoreDepthStencilState, StoreFragmentState},
        res::StoreTexture,
        FragmentStateManager,
    },
    pipeline::{desc::StoreRenderPipeline, RenderPipelineManager},
    shaders::{res::Shader, ShaderBuilder},
    vertex::{desc::StoreVertexState, VertexStateManager},
    RenderError,
};
use crate::{ds::generational::GenIndex, util::key::ObjectKey};
use std::rc::Rc;

#[derive(Debug)]
pub struct RenderManager {
    /// Window, instance, and adaptor.
    context: Rc<RenderContext>,

    /// Device and queue.
    gpu: Rc<Gpu>,

    /// GPU buffer pool.
    buf_pool: BufferPool,

    /// Shader builder.
    shader_bdr: ShaderBuilder,

    /// Bind group manager.
    bind_mgr: BindStateManager,

    /// Vertex state manager.
    vert_mgr: VertexStateManager,

    /// Fragment and depth stencil state manager.
    frag_mgr: FragmentStateManager,

    /// Render pipeline manager.
    rpipe_mgr: RenderPipelineManager,

    /// Current window scale factor.
    pub(crate) scale: f64,
}

impl RenderManager {
    pub(crate) async fn new(ref_canvas: &OffCanvas) -> Result<Self, RenderError> {
        let context = Rc::new(RenderContext::new(ref_canvas).await?);
        let gpu = Rc::new(Gpu::new(&context.adapter, None, None).await?);
        let buf_pool = BufferPool::new(Rc::clone(&gpu));
        let shader_bdr = ShaderBuilder::new(Rc::clone(&gpu));
        let bind_mgr = BindStateManager::new(Rc::clone(&gpu));
        let vert_mgr = VertexStateManager::new();
        let frag_mgr = FragmentStateManager::new(Rc::clone(&context), Rc::clone(&gpu));
        let rpipe_mgr = RenderPipelineManager::new(Rc::clone(&gpu));

        Ok(Self {
            context,
            gpu,
            buf_pool,
            shader_bdr,
            bind_mgr,
            vert_mgr,
            frag_mgr,
            rpipe_mgr,
            scale: 0.0,
        })
    }

    /// Registers canvas and creates surface for that with default configuration.
    pub(crate) fn register_canvas(&mut self, sel: String, canvas: OffCanvas) {
        // NOTE: Uses compatible texture format for now.
        self.frag_mgr.register_canvas(sel, canvas, None);
    }

    pub fn as_buffer_storage(&mut self) -> &mut impl StoreBuffer {
        &mut self.buf_pool
    }

    pub fn as_shader_builder(&mut self) -> &mut ShaderBuilder {
        &mut self.shader_bdr
    }

    pub fn as_bind_group_state_storage(&mut self) -> &mut impl StoreBindGroupState {
        &mut self.bind_mgr
    }

    pub fn as_bind_state_storage(&mut self) -> &mut impl StoreBindState {
        &mut self.bind_mgr
    }

    pub fn as_vertex_state_storage(&mut self) -> &mut impl StoreVertexState {
        &mut self.vert_mgr
    }

    pub fn as_texture_storage(&mut self) -> &mut impl StoreTexture {
        &mut self.frag_mgr
    }

    pub fn as_fragment_state_storage(&mut self) -> &mut impl StoreFragmentState {
        &mut self.frag_mgr
    }

    pub fn as_depth_stencil_state_storage(&mut self) -> &mut impl StoreDepthStencilState {
        &mut self.frag_mgr
    }

    pub fn as_render_pipeline_storage(&mut self) -> &mut impl StoreRenderPipeline {
        &mut self.rpipe_mgr
    }
}
