use crate::{
    decl_return_wrap,
    ds::generational::{GenIndex, GenIndexRc, GenVecRc},
    render::{
        bind::{BindPack, Binding},
        buffer::SizeOrData,
        canvas::{OffCanvas, Surface, SurfacePack},
        context::{Gpu, RenderContext},
        descs,
        pipeline::{PipelineBuilder, PipelineLayoutBuilder, PipelinePack},
        shaders::{Shader, ShaderPack},
        BufferPool, RenderError,
    },
    ty,
    util::key::ResKey,
};
use ahash::AHashMap;
use smallvec::{smallvec, SmallVec};
use std::{borrow::Borrow, collections::HashMap, hash::Hash, mem::transmute_copy, rc::Rc};

/// Top struct in render module.
pub struct RenderResource {
    /// [`wgpu::Device`] and [`wgpu::Queue`].
    pub gpu: Rc<Gpu>,

    /// A set of [`OffCanvas`].
    pub canvases: HashMap<u32, Rc<OffCanvas>, ahash::RandomState>,

    /// [`web_sys::Window`], [`wgpu::Instance`] and [`wgpu::Adapter`].
    pub context: RenderContext,

    /// An array of [`Surface`].
    pub surfaces: GenVecRc<Surface>,

    /// An array of [`SurfacePack`].
    pub surf_packs: GenVecRc<SurfacePack>,

    /// A map to find [`Surface`] from a canvas' CSS selectors.
    pub canvas_to_surf: AHashMap<u32, SmallVec<[GenIndex; 1]>>,

    /// All GPU buffers are here.
    pub bufs: BufferPool,

    /// A set of [`my_wgsl::Builder`] and [`Shader`].
    pub shaders: ShaderPack,

    /// TODO
    pub binds: BindPack,

    /// TODO
    pub pipelines: PipelinePack,

    pub scale: f64,
}

impl RenderResource {
    pub async fn new(offcanvas: OffCanvas) -> Result<Self, RenderError> {
        // Canvases.
        let mut canvases = HashMap::default();
        let offcanvas = Rc::new(offcanvas);
        canvases.insert(offcanvas.handle(), Rc::clone(&offcanvas));

        // Render context.
        let context = RenderContext::new(offcanvas).await?;

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
            canvas_to_surf: AHashMap::new(),
            bufs,
            shaders,
            binds,
            pipelines,
            scale: 0.0,
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

    /// Adds a new canvas and returns [`Canvas`].
    /// Use it to make a [`Surface`].
    /// Unused canvases will be removed when [`Self::clear_unused_canvas`] is called.
    #[inline]
    pub fn add_canvas(&mut self, offcanvas: OffCanvas) -> CanvasReturn {
        let offcanvas = Rc::new(offcanvas);
        // TODO: use Error
        let old = self
            .canvases
            .insert(offcanvas.handle(), Rc::clone(&offcanvas));
        if old.is_some() {
            panic!();
        }
        CanvasReturn {
            recv: self,
            ret: offcanvas,
        }
    }

    /// Gets the canvas.
    #[inline]
    pub fn get_canvas(&self, handle: u32) -> Option<&Rc<OffCanvas>> {
        self.canvases.get(&handle)
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

    /// Updates the surface without increasing generation.
    #[inline]
    pub fn sneak_update_surface<U>(
        &mut self,
        index: GenIndex,
        f: impl FnOnce(&mut Surface) -> U,
    ) -> Option<U> {
        self.surfaces.sneak_update(index, f)
    }

    /// Removes unused surfaces and tries to reduce capacity.
    pub fn clear_surface(&mut self) -> usize {
        self.surfaces.clear_unused(|_| {});
        let removed = self.surfaces.clear_vacancy();
        self.surfaces.shrink_to_fit();
        removed
    }

    /// Adds a new surface pack from the given canvas selectors.
    /// Each canvas can have multiple surfaces, and the surface pack will be composed of
    /// all surfaces from all canvases.
    pub fn add_surface_pack_from<'a, I>(&mut self, handles: I) -> GenIndexRc
    where
        I: Iterator<Item = &'a u32> + Clone,
    {
        // Fixes the mapping if it's broken.
        for handle in handles.clone() {
            self.fix_canvas_surfaces(*handle);
        }

        // Gathers all surfaces and make a surface pack from them.
        let mut surf_pack = SurfacePack::new();
        for handle in handles {
            if let Some(surf_indices) = self.canvas_to_surf.get(handle) {
                for index in surf_indices.iter() {
                    // Vacant item will result in an empty slot in surface pack.
                    let rc_index = self.surfaces.own(index.index);
                    surf_pack.add_surface_index(rc_index);
                }
            }
        }

        // Adds the surface pack.
        self.add_surface_pack(surf_pack)
    }

    /// Adds a new surface pack.
    #[inline]
    pub fn add_surface_pack(&mut self, surf_pack: SurfacePack) -> GenIndexRc {
        self.surf_packs.insert(surf_pack)
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

    /// Requests a writable vertex buffer and then adds it.
    #[inline]
    pub fn add_vertex_buffer(
        &mut self,
        size_or_data: SizeOrData,
    ) -> Result<Rc<wgpu::Buffer>, RenderError> {
        self.bufs.request_buffer(
            wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
            size_or_data,
        )
    }

    /// Requests a read-only index buffer and then adds it.
    #[inline]
    pub fn add_ro_index_buffer(
        &mut self,
        size_or_data: SizeOrData,
    ) -> Result<Rc<wgpu::Buffer>, RenderError> {
        self.bufs
            .request_buffer(wgpu::BufferUsages::INDEX, size_or_data)
    }

    /// Requests a writable uniform buffer and then adds it.
    #[inline]
    pub fn add_uniform_buffer(
        &mut self,
        size_or_data: SizeOrData,
    ) -> Result<Rc<wgpu::Buffer>, RenderError> {
        self.bufs.request_buffer(
            wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            size_or_data,
        )
    }

    /// Requests a read-only uniform buffer and then adds it.
    #[inline]
    pub fn add_ro_uniform_buffer(
        &mut self,
        size_or_data: SizeOrData,
    ) -> Result<Rc<wgpu::Buffer>, RenderError> {
        self.bufs
            .request_buffer(wgpu::BufferUsages::UNIFORM, size_or_data)
    }

    /// Adds a uniform or storage buffer binding with default settings.
    /// Handy, but it's lack of reusability.
    #[inline]
    pub fn add_default_buffer_bind(&mut self, desc: descs::BufferBindDesc) {
        self.binds.create_default_buffer_bind(desc);
    }

    #[inline]
    pub fn get_bind_group_layout<Q>(&self, key: &Q) -> Option<&Rc<wgpu::BindGroupLayout>>
    where
        ResKey: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.binds.layouts.get(key)
    }

    #[inline]
    pub fn get_bind_group<Q>(&self, key: &Q) -> Option<&Rc<wgpu::BindGroup>>
    where
        ResKey: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.binds.groups.get(key).map(|(group, _)| group)
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
    pub fn build_shader(&mut self, index: GenIndex, key: &ResKey) -> &Rc<Shader> {
        self.shaders.create_shader(index, key)
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
        key: ResKey,
    ) -> &Rc<wgpu::PipelineLayout> {
        self.pipelines.create_layout(index, key)
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
    pub fn build_pipeline(&mut self, index: GenIndex, key: ResKey) -> &Rc<wgpu::RenderPipeline> {
        self.pipelines
            .create_pipeline(index, key, &self.surf_packs, &self.surfaces)
    }

    /// Gets any iterator with a bit of inefficiency.
    pub fn iter<'a, T: 'static>(&'a self) -> Box<dyn Iterator<Item = T> + 'a> {
        // Safety: Type checked.
        if ty!(T) == ty!(IterBindGroupLayout) {
            Box::new(self.binds.layouts.iter().map(|(key, value)| unsafe {
                transmute_copy(&IterBindGroupLayout { key, layout: value })
            }))
        } else if ty!(T) == ty!(IterBindGroup) {
            Box::new(self.binds.groups.iter().map(|(key, value)| unsafe {
                transmute_copy(&IterBindGroup {
                    key,
                    group: &value.0,
                    bindings: &value.1,
                })
            }))
        } else if ty!(T) == ty!(IterShader) {
            Box::new(
                self.shaders.shaders.iter().map(|(key, value)| unsafe {
                    transmute_copy(&IterShader { key, shader: value })
                }),
            )
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
            Box::new(self.pipelines.pipelines.iter().map(|(key, value)| unsafe {
                transmute_copy(&IterRenderPipeline {
                    key,
                    pipeline: value,
                })
            }))
        } else {
            panic!()
        }
    }

    /// [`Self::canvas_to_surf`] is like weak references, its indices can be broken.
    /// This fixes the mapping if it was broken.
    pub fn fix_canvas_surfaces(&mut self, handle: u32) {
        // Checks if there's something broken.
        let broken = if let Some(surf_indices) = self.canvas_to_surf.get(&handle) {
            surf_indices.iter().any(|&index| {
                if let Some(surf) = self.surfaces.get(index) {
                    surf.offcanvas.handle() != handle
                } else {
                    true
                }
            })
        } else {
            false
        };

        // Re-generate surface indices only if it was broken.
        if broken {
            // Safety: Infallible.
            let surf_indices = unsafe { self.canvas_to_surf.get_mut(&handle).unwrap_unchecked() };
            *surf_indices = self
                .surfaces
                .iter()
                .enumerate()
                .filter_map(|(index, surf)| {
                    if let Some(surf) = surf.as_ref() {
                        (surf.offcanvas.handle() == handle).then_some(GenIndex::new_forced(index))
                    } else {
                        None
                    }
                })
                .collect();
        }
    }

    /// Adds a mapping of canvas' CSS selectors to surface index.
    /// The index becomes forced index, so that no generation check takes place.
    pub(crate) fn add_canvas_to_surface(&mut self, handle: u32, surface_index: GenIndex) {
        let surface_index = surface_index.into_forced();
        self.canvas_to_surf
            .entry(handle)
            .and_modify(|indices| indices.push(surface_index))
            .or_insert(smallvec![surface_index]);
    }

    /// Changes all sub-references to the current one.
    fn change_gpu(&mut self) {
        todo!()
    }
}

decl_return_wrap!(CanvasReturn, RenderResource, Rc<OffCanvas>);

impl<'a> CanvasReturn<'a> {
    pub fn into_return(self) -> Rc<OffCanvas> {
        self.ret
    }

    /// Adds a default [`Surface`] with the canvas for render attachment.
    ///
    /// If you are going to make another type of `Surface`,
    /// use [`RenderResource::add_surface`] instead.
    pub fn with_default_surface(self) -> Self {
        let Self {
            recv: render,
            ret: canvas,
        } = self;

        // Creates a default surface.
        let surface = Surface::default(
            &render.context.instance,
            &render.context.adapter,
            &render.gpu.device,
            Rc::clone(&canvas),
        );
        let index = render.add_surface(surface);

        // Adds a mapping of CSS selectors to surface index.
        render.add_canvas_to_surface(canvas.handle(), index.index);

        Self {
            recv: render,
            ret: canvas,
        }
    }
}

#[derive(Debug)]
pub struct IterBindGroupLayout<'a> {
    pub key: &'a ResKey,
    pub layout: &'a Rc<wgpu::BindGroupLayout>,
}

#[derive(Debug)]
pub struct IterBindGroup<'a> {
    pub key: &'a ResKey,
    pub group: &'a Rc<wgpu::BindGroup>,
    pub bindings: &'a Vec<Binding>,
}

#[derive(Debug)]
pub struct IterShader<'a> {
    pub key: &'a ResKey,
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
    pub key: &'a ResKey,
    pub pipeline: &'a Rc<wgpu::RenderPipeline>,
}
