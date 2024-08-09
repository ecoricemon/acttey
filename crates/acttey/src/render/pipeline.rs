use super::{
    bind::desc::BindStateRef,
    fragment::desc::{DepthStencilStateRef, FragmentStateRef},
    vertex::desc::VertexStateRef,
};
use crate::{
    common::AppHasher,
    ds::{
        refs::WithRc,
        share::{DebugLock, SharedVec, SharedVecItem, SharedVecOrigin},
    },
    render::context::Gpu,
};
use desc::*;
use exe::*;
use std::{
    cell::Cell,
    hash::{Hash, Hasher},
    rc::Rc,
    sync::Arc,
};

thread_local! {
    // For debugging.
    // For now, all shared variables in this module share this counter to detect invalid mutable borrows.
    // This may change in the future.
    #[cfg(debug_assertions)]
    pub(crate) static PIPE_MOD_DEBUG_LOCK: Cell<i32> = const { Cell::new(0) };
}

#[derive(Debug)]
pub(crate) struct RenderPipelineManager {
    gpu: Rc<Gpu>,

    /// Pointer to the vector inside [`Self::org_pipes`].
    pub(crate) pipes: SharedVec<RenderPipelineRc, AppHasher>,

    /// Actual vector of [`RenderPipeline`].
    /// This field is not accessed and just keeps vector alive.
    org_pipes: SharedVecOrigin<RenderPipelineRc, AppHasher>,
}

impl RenderPipelineManager {
    pub(crate) fn new(gpu: Rc<Gpu>) -> Self {
        let (pipes, org_pipes) = SharedVec::new();

        Self {
            gpu,
            pipes,
            org_pipes,
        }
    }
}

impl StoreRenderPipeline for RenderPipelineManager {
    fn add_render_pipeline(&mut self, desc: RenderPipelineDesc) -> RenderPipelineRef {
        let pipe = RenderPipeline::new(Rc::clone(&self.gpu), desc);
        let index = self.pipes.add(WithRc::new(pipe));

        DebugLock::new(
            SharedVecItem::new(self.pipes, index),
            #[cfg(debug_assertions)]
            &PIPE_MOD_DEBUG_LOCK,
        )
    }

    fn remove_render_pipeline(
        &mut self,
        pipe: RenderPipelineRef,
    ) -> Result<RenderPipeline, RenderPipelineRef> {
        let pipe = pipe.into_inner();
        self.pipes
            .remove(pipe)
            .map(RenderPipelineRc::into_inner)
            .map_err(|pipe| {
                DebugLock::new(
                    pipe,
                    #[cfg(debug_assertions)]
                    &PIPE_MOD_DEBUG_LOCK,
                )
            })
    }
}

/// Render module consists of Resource(res), Description(desc), and Execution(exe) layers.
/// Execution layer is responsible for executing render or compute passes.
pub(crate) mod exe {
    use super::*;

    #[derive(Debug)]
    pub(crate) struct RenderPipelineCommand {
        /// Command builder.
        state: RenderPipelineRef,

        // This field is a frequently accessed value.
        // So, although we could easily clone this using `Rc`, we don't do that.
        pipe: wgpu::RenderPipeline,
    }

    impl RenderPipelineCommand {
        pub(crate) const fn new(state: RenderPipelineRef, pipe: wgpu::RenderPipeline) -> Self {
            Self { state, pipe }
        }

        pub(crate) fn execute<'a: 'b, 'b>(&'a self, pass: &mut wgpu::RenderPass<'b>) {
            pass.set_pipeline(&self.pipe);
        }
    }

    impl Clone for RenderPipelineCommand {
        // Builds new command that has the same data.
        fn clone(&self) -> Self {
            RenderPipeline::build(self.state.clone())
        }
    }

    impl PartialEq for RenderPipelineCommand {
        fn eq(&self, other: &Self) -> bool {
            // In command equality test, it's enough to test data only.
            // But pipeline is layout only structure.
            // So we compare layouts here.
            let Self {
                state: this_state,
                pipe: _this_pipe, // We can't compare wgpu's data directly.
            } = self;

            let Self {
                state: other_state,
                pipe: _other_pipe,
            } = other;

            // TODO:
            // Equality test and hashing pipeline looks heavy in terms of code size.
            // How about using address only?
            // The same issue in hash().
            this_state == other_state
        }
    }

    impl Eq for RenderPipelineCommand {}

    impl Hash for RenderPipelineCommand {
        fn hash<H: Hasher>(&self, hasher: &mut H) {
            let Self {
                state,
                pipe: _pipe, // We can't hash wgpu's data directly.
            } = self;

            state.hash(hasher);
        }
    }
}

/// Render module consists of Resource(res), Description(desc), and Execution(exe) layers.
/// Description layer is responsible for describing GPU state using something like pipeline.
pub(crate) mod desc {
    use super::*;

    /// Render pipeline insertion and removal interfaces.
    pub trait StoreRenderPipeline {
        fn add_render_pipeline(&mut self, desc: RenderPipelineDesc) -> RenderPipelineRef;
        fn remove_render_pipeline(
            &mut self,
            pipe: RenderPipelineRef,
        ) -> Result<RenderPipeline, RenderPipelineRef>;
    }

    #[derive(Debug)]
    pub struct RenderPipelineDesc {
        pub label: Arc<str>,
        pub bind: BindStateRef,
        pub vert: VertexStateRef,
        pub frag: Option<FragmentStateRef>,
        pub depth_stencil: Option<DepthStencilStateRef>,
        /// Default value is [`RenderPipeline::DEFAULT_PRIMITIVE_STATE`].
        pub primitive: Option<wgpu::PrimitiveState>,
        /// Default value is [`RenderPipeline::DEFAULT_MULTISAMPLE_STATE`].
        pub multisample: Option<wgpu::MultisampleState>,
    }

    /// Writable reference to a render pipeline.
    pub(crate) type RenderPipelineRef =
        DebugLock<SharedVecItem<RenderPipelineRc, AppHasher>, RenderPipelineRc>;

    /// Shared render pipeline.
    pub(crate) type RenderPipelineRc = WithRc<RenderPipeline>;

    #[derive(Debug)]
    pub(crate) struct RenderPipeline {
        gpu: Rc<Gpu>,

        /// Common label for the builder.
        label: Arc<str>,

        /// Bind groups for this pipeline.
        /// Note that state itself is read only.
        bind: BindStateRef,

        /// Vertex state for this pipeline.
        /// Note that state itself is read only.
        vert: VertexStateRef,

        /// Optional fragment state for this pipeline.
        /// Note that state itself is read only.
        frag: Option<FragmentStateRef>,

        /// Optional depth stencil state for this pipeline.
        /// Note that state itself is read only.
        depth_stencil: Option<DepthStencilStateRef>,

        /// Primitive state for this pipeline.
        /// Note that state itself is read only.
        primitive: wgpu::PrimitiveState,

        /// Multisample state for this pipeline.
        /// Note that state itself is read only.
        multisample: wgpu::MultisampleState,
    }

    impl RenderPipeline {
        pub(crate) const DEFAULT_PRIMITIVE_STATE: wgpu::PrimitiveState = wgpu::PrimitiveState {
            topology: wgpu::PrimitiveTopology::TriangleList,
            strip_index_format: None,
            front_face: wgpu::FrontFace::Ccw,
            cull_mode: Some(wgpu::Face::Back),
            polygon_mode: wgpu::PolygonMode::Fill,
            unclipped_depth: false,
            conservative: false,
        };

        pub(crate) const DEFAULT_MULTISAMPLE_STATE: wgpu::MultisampleState =
            wgpu::MultisampleState {
                count: 1,
                mask: !0,
                alpha_to_coverage_enabled: false,
            };

        pub(crate) fn new(gpu: Rc<Gpu>, desc: RenderPipelineDesc) -> Self {
            Self {
                gpu,
                label: desc.label,
                bind: desc.bind,
                vert: desc.vert,
                frag: desc.frag,
                depth_stencil: desc.depth_stencil,
                primitive: desc.primitive.unwrap_or(Self::DEFAULT_PRIMITIVE_STATE),
                multisample: desc.multisample.unwrap_or(Self::DEFAULT_MULTISAMPLE_STATE),
            }
        }

        pub(crate) fn build(mut this: RenderPipelineRef) -> RenderPipelineCommand {
            let mut _self = this.borrow_mut();

            // Creates pipeline layout.
            let layout = _self
                .gpu
                .device
                .create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                    label: Some(&_self.label),
                    bind_group_layouts: &_self.bind.as_ref().create_bind_group_layouts(),
                    push_constant_ranges: &[],
                });

            // Creates wgpu's vertex state.
            let buffers = _self.vert.as_ref().create_vertex_state_buffers();
            let vertex = _self.vert.as_ref().as_vertex_state(&buffers);

            // Creates wgpu's fragment state.
            let targets;
            let fragment = if let Some(frag) = _self.frag.as_ref() {
                targets = frag.as_ref().create_color_target_states();
                Some(frag.as_ref().create_fragment_state(&targets))
            } else {
                None
            };

            let pipe = _self
                .gpu
                .device
                .create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                    label: Some(&_self.label),
                    layout: Some(&layout),
                    vertex,
                    primitive: _self.primitive,
                    depth_stencil: _self
                        .depth_stencil
                        .as_ref()
                        .map(|item| item.as_ref().depth_stencil_state()),
                    multisample: _self.multisample,
                    fragment,
                    multiview: None,
                });
            drop(_self);

            RenderPipelineCommand::new(this, pipe)
        }
    }

    impl PartialEq for RenderPipeline {
        fn eq(&self, other: &Self) -> bool {
            let Self {
                gpu: _this_gpu,     // No need to compare gpu instance.
                label: _this_label, // No need to compare label.
                bind: this_bind,
                vert: this_vert,
                frag: this_frag,
                depth_stencil: this_depth_stencil,
                primitive: this_primitive,
                multisample: this_multisample,
            } = self;

            let Self {
                gpu: _other_gpu,
                label: _other_label,
                bind: other_bind,
                vert: other_vert,
                frag: other_frag,
                depth_stencil: other_depth_stencil,
                primitive: other_primitive,
                multisample: other_multisample,
            } = other;

            this_bind == other_bind
                && this_vert == other_vert
                && this_frag == other_frag
                && this_depth_stencil == other_depth_stencil
                && this_primitive == other_primitive
                && this_multisample == other_multisample
        }
    }

    impl Eq for RenderPipeline {}

    impl Hash for RenderPipeline {
        fn hash<H: Hasher>(&self, state: &mut H) {
            let Self {
                gpu: _gpu,     // No need to hash gpu instance.
                label: _label, // No need to hash label.
                bind,
                vert,
                frag,
                depth_stencil,
                primitive,
                multisample,
            } = self;

            bind.hash(state);
            vert.hash(state);
            frag.hash(state);
            depth_stencil.hash(state);
            primitive.hash(state);
            multisample.hash(state);
        }
    }
}

/// Render module consists of Resource(res), Description(desc), and Execution(exe) layers.
/// Resource layer is responsible for holding GPU relative data.
pub(crate) mod res {}
