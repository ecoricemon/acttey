use crate::{
    ds::generational::{GenIndex, GenIndexRc, GenVecRc},
    render::{
        bind::BindPack,
        canvas::{Canvas, CanvasPack, Surface, SurfacePack},
        context::{Gpu, RenderContext},
        descs,
        pipeline::PipelinePack,
        renderer::RenderPass,
        shaders::{Shader, ShaderPack},
        BufferPool, RenderError,
    },
    ty,
};
use std::{mem::transmute_copy, rc::Rc};

use super::{
    bind::Binding,
    pipeline::{PipelineBuilder, PipelineLayoutBuilder},
};

/// Top struct in render module.
pub struct RenderResource {
    pub gpu: Rc<Gpu>,
    pub canvases: CanvasPack,
    pub context: RenderContext,
    pub surfaces: GenVecRc<Surface>,
    pub surf_packs: GenVecRc<SurfacePack>,
    pub bufs: BufferPool,
    pub shaders: ShaderPack,
    pub binds: BindPack,
    pub pipelines: PipelinePack,
    pub render_passes: GenVecRc<RenderPass>,
}

impl RenderResource {
    pub async fn new() -> Result<Self, RenderError> {
        let canvases = CanvasPack::new();
        let context = RenderContext::new(canvases.get_dummy()).await?;

        // Default gpu.
        let gpu = Rc::new(Gpu::new(&context.adapter, None, None).await?);

        // Buffers.
        let bufs = BufferPool::new(&gpu);

        // Shaders.
        let shaders = ShaderPack::new(&gpu);

        // Bind groups.
        let binds = BindPack::new(&gpu);

        // Pipelines.
        let pipelines = PipelinePack::new(&gpu);

        Ok(Self {
            gpu,
            canvases,
            context,
            surfaces: GenVecRc::new(),
            surf_packs: GenVecRc::new(),
            bufs,
            shaders,
            binds,
            pipelines,
            render_passes: GenVecRc::new(),
        })
    }

    /// Re-create gpu with the given features and limits.
    /// All resources refering to the old gpu obtain new gpu reference automatically.
    pub async fn set_gpu(
        &mut self,
        features: Option<wgpu::Features>,
        limits: Option<wgpu::Limits>,
    ) -> Result<(), RenderError> {
        // Keeps old features and limits if value was not given.
        let features = features.unwrap_or(self.gpu.device.features());
        let limits = limits.unwrap_or(self.gpu.device.limits());
        let gpu = Gpu::new(&self.context.adapter, Some(features), Some(limits)).await?;
        self.gpu = Rc::new(gpu);
        self.change_gpu();
        Ok(())
    }

    /// Adds a new canvas and returns `Rc<Canvas>`.
    /// Use `Rc<Canvas>` to make a `Surface`.
    /// Unused canvases will be removed when `clear_canvas()` called.
    #[inline]
    pub fn add_canvas(&mut self, selectors: &str) -> Result<Rc<Canvas>, RenderError> {
        self.canvases.insert(selectors)
    }

    /// Gets the canvas.
    #[inline]
    pub fn get_canvas(&self, selectors: &str) -> Option<&Rc<Canvas>> {
        self.canvases.get_by_selectors(selectors)
    }

    /// Clears unused canvases and returns the number of removed canvases.
    #[inline]
    pub fn clear_unused_canvas(&mut self) -> usize {
        self.canvases.clear()
    }

    /// Adds a new surface.
    #[inline]
    pub fn add_surface(&mut self, surface: Surface) -> GenIndexRc {
        self.surfaces.insert(surface)
    }

    /// Gets the surface.
    #[inline]
    pub fn get_surface(&self, index: GenIndex) -> Option<&Surface> {
        self.surfaces.get(index)
    }

    /// Updates the surface.
    #[inline]
    pub fn update_surface<U>(
        &mut self,
        index: GenIndex,
        f: impl FnOnce(&mut Surface) -> U,
    ) -> Option<(GenIndexRc, U)> {
        self.surfaces.update(index, f)
    }

    /// Removes unused surfaces and tries to reduce capacity.
    pub fn clear_surface(&mut self) -> usize {
        self.surfaces.clear_unused(|_| {});
        let removed = self.surfaces.clear_vacancy();
        self.surfaces.shrink_to_fit();
        removed
    }

    /// Adds a new surface set.
    #[inline]
    pub fn add_surface_pack(&mut self, pack: SurfacePack) -> GenIndexRc {
        self.surf_packs.insert(pack)
    }

    /// Gets the surface pack.
    #[inline]
    pub fn get_surface_pack(&self, index: GenIndex) -> Option<&SurfacePack> {
        self.surf_packs.get(index)
    }

    /// Updates the surface pack.
    #[inline]
    pub fn update_surface_pack<U>(
        &mut self,
        index: GenIndex,
        f: impl FnOnce(&mut SurfacePack) -> U,
    ) -> Option<(GenIndexRc, U)> {
        self.surf_packs.update(index, f)
    }

    /// Removes unused suface packs and tries to reduce capacity.
    pub fn clear_surface_pack(&mut self) -> usize {
        self.surf_packs.clear_unused(|_| {});
        let removed = self.surf_packs.clear_vacancy();
        self.surf_packs.shrink_to_fit();
        removed
    }

    /// Adds a writable vertex buffer initially filled with the data.
    #[inline]
    pub fn add_vertex_buffer(&mut self, data: &[u8]) -> Result<Rc<wgpu::Buffer>, RenderError> {
        self.bufs.request_buffer(
            wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
            None,
            Some(data),
        )
    }

    /// Adds an read-only index buffer initially filled with the data.
    #[inline]
    pub fn add_ro_index_buffer(&mut self, data: &[u8]) -> Result<Rc<wgpu::Buffer>, RenderError> {
        self.bufs
            .request_buffer(wgpu::BufferUsages::INDEX, None, Some(data))
    }

    /// Adds a writable uniform buffer initially filled with the data.
    #[inline]
    pub fn add_uniform_buffer<'a>(
        &mut self,
        data: impl Into<&'a [u8]>,
    ) -> Result<Rc<wgpu::Buffer>, RenderError> {
        self.bufs.request_buffer(
            wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            None,
            Some(data.into()),
        )
    }

    /// Adds a read-only uniform buffer initially filled with the data.
    #[inline]
    pub fn add_ro_uniform_buffer<'a>(
        &mut self,
        data: impl Into<&'a [u8]>,
    ) -> Result<Rc<wgpu::Buffer>, RenderError> {
        self.bufs
            .request_buffer(wgpu::BufferUsages::UNIFORM, None, Some(data.into()))
    }

    /// Adds a uniform or storage buffer binding with default settings.
    /// Handy, but it's lack of reusability.
    #[inline]
    pub fn add_default_buffer_bind(&mut self, desc: descs::BufferBindDesc) {
        self.binds.create_default_buffer_bind(desc);
    }

    #[inline]
    pub fn add_shader_builder(&mut self, builder: my_wgsl::Builder) -> GenIndex {
        self.shaders.builders.insert(builder)
    }

    #[inline]
    pub fn get_shader_builder(&self, index: GenIndex) -> Option<&my_wgsl::Builder> {
        self.shaders.builders.get(index)
    }

    #[inline]
    pub fn update_shader_builder<U>(
        &mut self,
        index: GenIndex,
        f: impl FnOnce(&mut my_wgsl::Builder) -> U,
    ) -> Option<(GenIndex, U)> {
        self.shaders.builders.update(index, f)
    }

    #[inline]
    pub fn remove_shader_builder(&mut self, index: GenIndex) -> Option<my_wgsl::Builder> {
        self.shaders.builders.take(index)
    }

    #[inline]
    pub fn build_shader(&mut self, index: GenIndex, label: &Rc<str>) -> &Rc<Shader> {
        self.shaders.create_shader(index, label)
    }

    #[inline]
    pub fn add_pipeline_layout_builder(&mut self, builder: PipelineLayoutBuilder) -> GenIndex {
        self.pipelines.layout_builders.insert(builder)
    }

    #[inline]
    pub fn get_pipeline_layout_builder(&self, index: GenIndex) -> Option<&PipelineLayoutBuilder> {
        self.pipelines.layout_builders.get(index)
    }

    #[inline]
    pub fn update_pipeline_layout_builder<U>(
        &mut self,
        index: GenIndex,
        f: impl FnOnce(&mut PipelineLayoutBuilder) -> U,
    ) -> Option<(GenIndex, U)> {
        self.pipelines.layout_builders.update(index, f)
    }

    #[inline]
    pub fn remove_pipeline_layout_builder(
        &mut self,
        index: GenIndex,
    ) -> Option<PipelineLayoutBuilder> {
        self.pipelines.layout_builders.take(index)
    }

    #[inline]
    pub fn build_pipeline_layout(
        &mut self,
        index: GenIndex,
        label: &str,
    ) -> &Rc<wgpu::PipelineLayout> {
        self.pipelines.create_layout(index, label)
    }

    #[inline]
    pub fn add_pipeline_builder(&mut self, builder: PipelineBuilder) -> GenIndex {
        self.pipelines.pipeline_builders.insert(builder)
    }

    #[inline]
    pub fn get_pipeline_builder(&self, index: GenIndex) -> Option<&PipelineBuilder> {
        self.pipelines.pipeline_builders.get(index)
    }

    #[inline]
    pub fn update_pipeline_builder<U>(
        &mut self,
        index: GenIndex,
        f: impl FnOnce(&mut PipelineBuilder) -> U,
    ) -> Option<(GenIndex, U)> {
        self.pipelines.pipeline_builders.update(index, f)
    }

    #[inline]
    pub fn remove_pipeline_builder(&mut self, index: GenIndex) -> Option<PipelineBuilder> {
        self.pipelines.pipeline_builders.take(index)
    }

    #[inline]
    pub fn build_pipeline(&mut self, index: GenIndex, label: &str) -> &Rc<wgpu::RenderPipeline> {
        self.pipelines
            .create_pipeline(index, label, &self.surf_packs, &self.surfaces)
    }

    /// Adds a new render pass.
    #[inline]
    pub fn add_render_pass(&mut self, pass: RenderPass) -> GenIndexRc {
        self.render_passes.insert(pass)
    }

    /// Gets the render pass.
    #[inline]
    pub fn get_render_pass(&self, index: GenIndex) -> Option<&RenderPass> {
        self.render_passes.get(index)
    }

    /// Updates the render pass.
    #[inline]
    pub fn update_render_pass<U>(
        &mut self,
        index: GenIndex,
        f: impl FnOnce(&mut RenderPass) -> U,
    ) -> Option<(GenIndexRc, U)> {
        self.render_passes.update(index, f)
    }

    /// Removes unused render passs and tries to reduce capacity.
    pub fn clear_render_pass(&mut self) -> usize {
        self.render_passes.clear_unused(|_| {});
        let removed = self.render_passes.clear_vacancy();
        self.render_passes.shrink_to_fit();
        removed
    }

    pub fn iter<'a, T: 'static>(&'a self) -> Box<dyn Iterator<Item = T> + 'a> {
        // Safety: Type checked.
        if ty!(T) == ty!(IterBindGroupLayout) {
            Box::new(self.binds.layouts.iter().map(|(k, v)| unsafe {
                transmute_copy(&IterBindGroupLayout {
                    label: k,
                    layout: v,
                })
            }))
        } else if ty!(T) == ty!(IterBindGroup) {
            Box::new(self.binds.groups.iter().map(|(k, v)| unsafe {
                transmute_copy(&IterBindGroup {
                    label: k,
                    group: &v.0,
                    bindings: &v.1,
                })
            }))
        } else if ty!(T) == ty!(IterShader) {
            Box::new(self.shaders.shaders.iter().map(|(k, v)| unsafe {
                transmute_copy(&IterShader {
                    label: k,
                    shader: v,
                })
            }))
        } else if ty!(T) == ty!(IterIndexBuffer) {
            Box::new(
                self.bufs
                    .get_index_group()
                    .unwrap()
                    .iter_used()
                    .map(|buf| unsafe { transmute_copy(&IterIndexBuffer { buf }) }),
            )
        } else if ty!(T) == ty!(IterVertexBuffer) {
            Box::new(
                self.bufs
                    .get_vertex_group()
                    .unwrap()
                    .iter_used()
                    .map(|buf| unsafe { transmute_copy(&IterVertexBuffer { buf }) }),
            )
        } else if ty!(T) == ty!(IterUniformBuffer) {
            Box::new(
                self.bufs
                    .get_uniform_group()
                    .unwrap()
                    .iter_used()
                    .map(|buf| unsafe { transmute_copy(&IterUniformBuffer { buf }) }),
            )
        } else if ty!(T) == ty!(IterStorageBuffer) {
            Box::new(
                self.bufs
                    .get_storage_group()
                    .unwrap()
                    .iter_used()
                    .map(|buf| unsafe { transmute_copy(&IterStorageBuffer { buf }) }),
            )
        } else if ty!(T) == ty!(IterRenderPipeline) {
            Box::new(self.pipelines.pipelines.iter().map(|(k, v)| unsafe {
                transmute_copy(&IterRenderPipeline {
                    label: k,
                    pipeline: v,
                })
            }))
        } else if ty!(T) == ty!(IterRenderPass) {
            Box::new(
                self.render_passes
                    .iter_occupied()
                    .map(|v| unsafe { transmute_copy(&IterRenderPass { pass: v }) }),
            )
        } else {
            panic!()
        }
    }

    pub fn resize_surfaces(&self) {
        // dummy for now
    }

    /// Changes all gpu references to the current gpu.
    fn change_gpu(&mut self) {
        todo!()
    }
}

#[derive(Debug)]
pub struct IterBindGroupLayout<'a> {
    pub label: &'a str,
    pub layout: &'a Rc<wgpu::BindGroupLayout>,
}

#[derive(Debug)]
pub struct IterBindGroup<'a> {
    pub label: &'a str,
    pub group: &'a Rc<wgpu::BindGroup>,
    pub bindings: &'a Vec<Binding>,
}

#[derive(Debug)]
pub struct IterShader<'a> {
    pub label: &'a str,
    pub shader: &'a Rc<Shader>,
}

#[derive(Debug)]
pub struct IterIndexBuffer<'a> {
    pub buf: &'a Rc<wgpu::Buffer>,
}

#[derive(Debug)]
pub struct IterVertexBuffer<'a> {
    pub buf: &'a Rc<wgpu::Buffer>,
}

#[derive(Debug)]
pub struct IterUniformBuffer<'a> {
    pub buf: &'a Rc<wgpu::Buffer>,
}

#[derive(Debug)]
pub struct IterStorageBuffer<'a> {
    pub buf: &'a Rc<wgpu::Buffer>,
}

#[derive(Debug)]
pub struct IterRenderPipeline<'a> {
    pub label: &'a String,
    pub pipeline: &'a Rc<wgpu::RenderPipeline>,
}

#[derive(Debug)]
pub struct IterRenderPass<'a> {
    pub pass: &'a RenderPass,
}
