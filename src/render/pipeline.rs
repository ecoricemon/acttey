use crate::{
    ds::{
        generational::{GenIndex, GenIndexRc, GenVec, GenVecRc},
        sparse_set::MonoSparseSet,
    },
    render::{
        buffer::BufferView,
        canvas::{Surface, SurfacePack},
        shaders::Shader,
        Gpu,
    },
    util::{key::ResKey, ToStr},
};
use std::{borrow::Borrow, rc::Rc};

#[derive(Debug)]
pub struct PipelinePack {
    gpu: Rc<Gpu>,
    pub layout_builders: GenVec<PipelineLayoutBuilder>,
    pub pipeline_builders: GenVec<PipelineBuilder>,
    pub layouts: MonoSparseSet<ResKey, Rc<wgpu::PipelineLayout>>,
    pub pipelines: MonoSparseSet<ResKey, Rc<wgpu::RenderPipeline>>,
}

impl PipelinePack {
    pub fn new(gpu: &Rc<Gpu>) -> Self {
        Self {
            gpu: Rc::clone(gpu),
            layout_builders: GenVec::new(),
            pipeline_builders: GenVec::new(),
            layouts: MonoSparseSet::new(),
            pipelines: MonoSparseSet::new(),
        }
    }

    pub fn create_layout(
        &mut self,
        builder_index: GenIndex,
        key: ResKey,
    ) -> &Rc<wgpu::PipelineLayout> {
        let builder = self.layout_builders.get(builder_index).unwrap();
        let layout = builder.build(&self.gpu.device, key.clone());
        if let Some(old) = self.layouts.insert(key.clone(), Rc::new(layout)) {
            assert!(Rc::strong_count(&old) == 1);
        }

        // Safety: Infallible.
        unsafe { self.layouts.get(key.borrow()).unwrap_unchecked() }
    }

    pub fn create_pipeline(
        &mut self,
        builder_index: GenIndex,
        key: ResKey,
        surf_packs: &GenVecRc<SurfacePack>,
        surfaces: &GenVecRc<Surface>,
    ) -> &Rc<wgpu::RenderPipeline> {
        let builder = self.pipeline_builders.get(builder_index).unwrap();
        let pipeline = builder.build(&self.gpu.device, key.clone(), surf_packs, surfaces);
        if let Some(old) = self.pipelines.insert(key.clone(), Rc::new(pipeline)) {
            assert!(Rc::strong_count(&old) == 1);
        }

        // Safety: Infallible.
        unsafe { self.pipelines.get(key.borrow()).unwrap_unchecked() }
    }
}

/// A builder of `wgpu::Pipeline`.
#[derive(Debug)]
pub struct PipelineBuilder {
    pub layout: Option<Rc<wgpu::PipelineLayout>>,
    pub vert_shader: Option<Rc<Shader>>,
    pub vert_buf_view: Vec<BufferView>,
    pub frag_shader: Option<Rc<Shader>>,
    pub surf_pack_index: Option<GenIndexRc>,
    pub primitive: wgpu::PrimitiveState,
    pub depth_stencil: Option<wgpu::DepthStencilState>,
    pub multisample: wgpu::MultisampleState,
}

impl PipelineBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_layout(
        &mut self,
        layout: Rc<wgpu::PipelineLayout>,
    ) -> Option<Rc<wgpu::PipelineLayout>> {
        self.layout.replace(layout)
    }

    pub fn set_vertex_shader(&mut self, shader: Rc<Shader>) -> Option<Rc<Shader>> {
        self.vert_shader.replace(shader)
    }

    pub fn set_fragment_shader(&mut self, shader: Rc<Shader>) -> Option<Rc<Shader>> {
        self.frag_shader.replace(shader)
    }

    pub fn set_surface_pack_index(&mut self, index: GenIndexRc) -> Option<GenIndexRc> {
        self.surf_pack_index.replace(index)
    }

    pub fn set_depth_stencil(
        &mut self,
        depth_stencil: wgpu::DepthStencilState,
    ) -> Option<wgpu::DepthStencilState> {
        self.depth_stencil.replace(depth_stencil)
    }

    pub fn free(&mut self) {
        std::mem::take(self);
    }

    /// # Panics
    ///
    /// Panics if vertex shader is unset.
    pub fn build(
        &self,
        device: &wgpu::Device,
        key: ResKey,
        surf_packs: &GenVecRc<SurfacePack>,
        surfaces: &GenVecRc<Surface>,
    ) -> wgpu::RenderPipeline {
        // Creates vertex state.
        let buffer_layouts = self
            .vert_buf_view
            .iter()
            .map(|v| v.create_buffer_layout())
            .collect::<Vec<_>>();
        let vert_state = wgpu::VertexState {
            module: &self.vert_shader.as_ref().unwrap().module,
            entry_point: self
                .vert_shader
                .as_ref()
                .unwrap()
                .entry_point
                .vert()
                .unwrap(),
            buffers: &buffer_layouts,
        };

        // Creates optional fragment state.
        let color_targets;
        let frag_state = if let Some(shader) = &self.frag_shader {
            let surf_pack_index = self.surf_pack_index.as_ref().unwrap().index;
            color_targets = surf_packs
                .get(surf_pack_index)
                .unwrap()
                .create_color_targets(surfaces);
            Some(wgpu::FragmentState {
                module: &shader.module,
                entry_point: shader.entry_point.frag().unwrap(),
                targets: &color_targets,
            })
        } else {
            None
        };

        // Creates pipeline.
        device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some(&key.to_str()),
            layout: self.layout.as_ref().map(|layout| layout.as_ref()),
            vertex: vert_state,
            primitive: self.primitive,
            depth_stencil: self.depth_stencil.clone(),
            multisample: self.multisample,
            fragment: frag_state,
            multiview: None,
        })
    }
}

impl Default for PipelineBuilder {
    fn default() -> Self {
        // Default primitive state.
        let primitive = wgpu::PrimitiveState {
            topology: wgpu::PrimitiveTopology::TriangleList,
            strip_index_format: None,
            front_face: wgpu::FrontFace::Ccw,
            cull_mode: Some(wgpu::Face::Back),
            polygon_mode: wgpu::PolygonMode::Fill,
            unclipped_depth: false,
            conservative: false,
        };

        // Default multisample state.
        let multisample = wgpu::MultisampleState {
            count: 1,
            mask: !0,
            alpha_to_coverage_enabled: false,
        };

        Self {
            layout: None,
            vert_shader: None,
            vert_buf_view: Vec::new(),
            frag_shader: None,
            surf_pack_index: None,
            primitive,
            depth_stencil: None,
            multisample,
        }
    }
}

/// A builder of `wgpu::PipelineLayout`.
#[derive(Debug, Default)]
pub struct PipelineLayoutBuilder {
    pub bind_group_layouts: Vec<Rc<wgpu::BindGroupLayout>>,
    pub push_constants: Vec<wgpu::PushConstantRange>,
}

impl PipelineLayoutBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn free(&mut self) {
        std::mem::take(self);
    }

    pub fn build(&self, device: &wgpu::Device, key: ResKey) -> wgpu::PipelineLayout {
        let bind_group_layouts = self
            .bind_group_layouts
            .iter()
            .map(|layout| layout.as_ref())
            .collect::<Vec<_>>();
        device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some(&key.to_str()),
            bind_group_layouts: &bind_group_layouts,
            push_constant_ranges: &self.push_constants,
        })
    }
}
