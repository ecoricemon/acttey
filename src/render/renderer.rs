use crate::{
    ds::refs::{RCell, Weaks},
    primitive::mesh::{InterleavedVertexInfo, Mesh},
    render::{Drawable, Gpu},
};
use std::{
    borrow::Cow,
    iter::once,
    ops::RangeBounds,
    rc::{Rc, Weak},
};
use wgpu::util::DeviceExt;

pub(super) struct Renderer {
    gpu: Rc<Gpu>,
    drawable: RCell<Drawable>,
    bind_groups: Vec<Rc<BindGroup>>,
    layout: Option<wgpu::PipelineLayout>,
    v_shader: ShaderView,
    f_shader: ShaderView,
    v_buffers: Vec<Rc<VertexBuffer>>,
    pipeline: Option<wgpu::RenderPipeline>,
}

impl Renderer {
    pub(super) fn new(gpu: &Rc<Gpu>, drawable: &RCell<Drawable>) -> Self {
        Self {
            gpu: Rc::clone(gpu),
            drawable: RCell::clone(drawable),
            bind_groups: vec![],
            v_shader: ShaderView::None,
            f_shader: ShaderView::None,
            v_buffers: vec![],
            layout: None,
            pipeline: None,
        }
    }

    pub(super) fn example(
        gpu: &Rc<Gpu>,
        drawable: &RCell<Drawable>,
        bind_group: Option<&Rc<BindGroup>>,
        v_shader: ShaderView,
        f_shader: ShaderView,
        v_buffer: &Rc<VertexBuffer>,
    ) -> Self {
        let mut renderer = Self::new(gpu, drawable);
        if let Some(bind_group) = bind_group {
            renderer.add_bind_group(bind_group);
        }
        renderer.set_shader(v_shader, f_shader);
        renderer.add_v_buffer(v_buffer);
        renderer.finish();
        renderer
    }

    pub(super) fn add_bind_group(&mut self, bind_group: &Rc<BindGroup>) {
        self.bind_groups.push(Rc::clone(bind_group));
    }

    pub(super) fn finish_layout(&mut self) {
        let bind_group_layouts = self
            .bind_groups
            .iter()
            .map(|bind_group| bind_group.get_layout())
            .collect::<Vec<_>>();

        let layout = self
            .gpu
            .device
            .create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                label: Some("Render pipeline layout"),
                bind_group_layouts: &bind_group_layouts,
                push_constant_ranges: &[],
            });

        self.layout = Some(layout);
    }

    pub(super) fn set_shader(&mut self, v_shader: ShaderView, f_shader: ShaderView) {
        self.v_shader = v_shader;
        self.f_shader = f_shader;
    }

    pub(super) fn add_v_buffer(&mut self, v_buffer: &Rc<VertexBuffer>) {
        self.v_buffers.push(Rc::clone(v_buffer));
    }

    pub(super) fn finish_pipeline(&mut self) {
        // Creates `VertexState`.
        let v_shader = self.v_shader.upgrade().unwrap();
        let v_layouts = self
            .v_buffers
            .iter()
            .map(|v_buffer| v_buffer.get_layout())
            .collect::<Vec<_>>();
        let vertex = wgpu::VertexState {
            module: v_shader.get_module(),
            entry_point: v_shader.get_entry(),
            buffers: &v_layouts,
        };

        // Creates `PrimitiveState`.
        let primitive = wgpu::PrimitiveState {
            topology: wgpu::PrimitiveTopology::TriangleList,
            strip_index_format: None,
            front_face: wgpu::FrontFace::Ccw,
            cull_mode: Some(wgpu::Face::Back),
            polygon_mode: wgpu::PolygonMode::Fill,
            unclipped_depth: false,
            conservative: false,
        };

        // Creates `MultisampleState`.
        let multisample = wgpu::MultisampleState {
            count: 1,
            mask: !0,
            alpha_to_coverage_enabled: false,
        };

        // Creates `FragmentState`.
        let drawable = self.drawable.borrow();
        let f_shader = self.f_shader.upgrade();
        let targets = &[Some(drawable.color_target()); 1];
        let mut fragment: Option<wgpu::FragmentState> = None;
        if f_shader.is_some() {
            let f_shader = f_shader.as_ref().unwrap();
            fragment = Some(wgpu::FragmentState {
                module: f_shader.get_module(),
                entry_point: f_shader.get_entry(),
                targets,
            });
        }

        // Creates `RenderPipeline`.
        let pipeline = self
            .gpu
            .device
            .create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                label: Some("Render pipeline"),
                layout: self.layout.as_ref(),
                vertex,
                primitive,
                depth_stencil: None,
                multisample,
                fragment,
                multiview: None,
            });

        self.pipeline = Some(pipeline);
    }

    pub(super) fn finish(&mut self) {
        if self.layout.is_none() {
            self.finish_layout();
        }
        if self.pipeline.is_none() {
            self.finish_pipeline();
        }
    }

    #[inline]
    pub(super) fn get_layout(&self) -> &wgpu::PipelineLayout {
        self.layout.as_ref().unwrap()
    }

    #[inline]
    pub(super) fn get_pipeline(&self) -> &wgpu::RenderPipeline {
        self.pipeline.as_ref().unwrap()
    }

    pub(super) fn render(&mut self) {
        let drawable = self.drawable.borrow();
        let surface_texture = drawable.get_current_texture();
        let texture_view = surface_texture.texture.create_view(&Default::default());
        let mut encoder = self
            .gpu
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("Command encoder"),
            });
        {
            let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Render pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &texture_view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color {
                            r: 0.0,
                            g: 0.0,
                            b: 0.0,
                            a: 0.0,
                        }),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
            });
            render_pass.set_pipeline(self.get_pipeline());
            for (i, v_buffer) in self.v_buffers.iter().enumerate() {
                render_pass.set_vertex_buffer(0, v_buffer.vertex_slice(..));
                render_pass.set_index_buffer(v_buffer.index_slice(..), v_buffer.index_format);
                for (bi, bind_group) in self.bind_groups.iter().enumerate() {
                    render_pass.set_bind_group(bi as u32, bind_group.get_inner(), &[]);
                }
                render_pass.draw_indexed(0..v_buffer.index_num, 0, 0..1);
            }
        }
        self.gpu.queue.submit(once(encoder.finish()));
        surface_texture.present();
    }
}

pub(super) enum ShaderViewFreeze {
    Vertex(Rc<Shader>, Rc<String>),
    Fragment(Rc<Shader>, Rc<String>),
}

impl ShaderViewFreeze {
    pub(super) fn get_module(&self) -> &wgpu::ShaderModule {
        match self {
            Self::Vertex(shader, _) => &shader.module,
            Self::Fragment(shader, _) => &shader.module,
        }
    }

    pub(super) fn get_entry(&self) -> &str {
        match self {
            Self::Vertex(_, entry) => entry,
            Self::Fragment(_, entry) => entry,
        }
    }
}

impl From<&ShaderView> for Option<ShaderViewFreeze> {
    fn from(value: &ShaderView) -> Self {
        let strong = match value {
            ShaderView::Vertex(shader, entry) => {
                let shader = shader.upgrade().unwrap();
                let entry = entry.upgrade().unwrap();
                ShaderViewFreeze::Vertex(shader, entry)
            }
            ShaderView::Fragment(shader, entry) => {
                let shader = shader.upgrade().unwrap();
                let entry = entry.upgrade().unwrap();
                ShaderViewFreeze::Fragment(shader, entry)
            }
            ShaderView::None => return None,
        };
        Some(strong)
    }
}

pub(super) enum ShaderView {
    Vertex(Weak<Shader>, Weak<String>),
    Fragment(Weak<Shader>, Weak<String>),
    None,
}

impl ShaderView {
    pub(super) fn from_as_v(shader: &Rc<Shader>, entry: usize) -> Self {
        let entry = shader.get_v_entry(entry).unwrap();
        Self::_from(shader, entry)
    }

    pub(super) fn from_as_f(shader: &Rc<Shader>, entry: usize) -> Self {
        let entry = shader.get_f_entry(entry).unwrap();
        Self::_from(shader, entry)
    }

    pub(super) fn _from(shader: &Rc<Shader>, entry: &Rc<String>) -> Self {
        let entry = Rc::downgrade(entry);
        let shader = Rc::downgrade(shader);
        Self::Vertex(shader, entry)
    }

    pub(super) fn upgrade(&self) -> Option<ShaderViewFreeze> {
        self.into()
    }
}

pub(super) struct Shader {
    module: wgpu::ShaderModule,
    v_entries: Vec<Rc<String>>,
    f_entries: Vec<Rc<String>>,
}

impl Shader {
    pub(super) fn new(gpu: &Gpu, module: &str, v_entries: &[&str], f_entries: &[&str]) -> Self {
        let module = gpu
            .device
            .create_shader_module(wgpu::ShaderModuleDescriptor {
                label: Some("Shader module"),
                source: wgpu::ShaderSource::Wgsl(Cow::from(module)),
            });

        let helper = |entries: &[&str]| {
            entries
                .iter()
                .map(|&entry| Rc::new(entry.to_owned()))
                .collect::<Vec<_>>()
        };
        let v_entries = helper(v_entries);
        let f_entries = helper(f_entries);

        Self {
            module,
            v_entries,
            f_entries,
        }
    }

    pub(super) fn get_v_entry(&self, entry: usize) -> Option<&Rc<String>> {
        self.v_entries.get(entry)
    }

    pub(super) fn get_f_entry(&self, entry: usize) -> Option<&Rc<String>> {
        self.f_entries.get(entry)
    }
}

/// Assumes that all verticies have the same VertexAttribute.
pub(super) struct VertexBuffer {
    vertex: wgpu::Buffer,
    vertex_attributes: Vec<wgpu::VertexAttribute>,
    vertex_size: u32,
    vertex_num: u32,
    index: wgpu::Buffer,
    index_format: wgpu::IndexFormat,
    index_num: u32,
}

impl VertexBuffer {
    pub(super) fn from_mesh(device: &wgpu::Device, mesh: Mesh) -> Self {
        let interleaved: InterleavedVertexInfo = mesh.into();

        let vertex = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Vertex buffer"),
            contents: &interleaved.vertex_bytes,
            usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
        });

        let index = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Index buffer"),
            contents: &interleaved.index_bytes,
            usage: wgpu::BufferUsages::INDEX,
        });

        Self {
            vertex,
            vertex_attributes: interleaved.wgpu_attributes,
            vertex_size: interleaved.vertex_size as u32,
            vertex_num: interleaved.vertex_num as u32,
            index,
            index_format: interleaved.index_format,
            index_num: interleaved.index_num as u32,
        }
    }

    pub(super) fn get_layout(&self) -> wgpu::VertexBufferLayout {
        wgpu::VertexBufferLayout {
            array_stride: self.vertex_size as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &self.vertex_attributes,
        }
    }

    #[inline(always)]
    pub(super) fn vertex_slice<S: RangeBounds<wgpu::BufferAddress>>(
        &self,
        bounds: S,
    ) -> wgpu::BufferSlice {
        self.vertex.slice(bounds)
    }

    #[inline(always)]
    pub(super) fn index_slice<S: RangeBounds<wgpu::BufferAddress>>(
        &self,
        bounds: S,
    ) -> wgpu::BufferSlice {
        self.index.slice(bounds)
    }
}

pub(super) struct BindGroup {
    gpu: Weak<Gpu>,
    entries: Weaks<Bindable>,
    layout: Option<wgpu::BindGroupLayout>,
    inner: Option<wgpu::BindGroup>,
}

impl BindGroup {
    pub(super) fn default(gpu: &Rc<Gpu>, entry: &Rc<Bindable>) -> Self {
        let mut bind_group = Self::new(gpu);
        bind_group.add_entry(entry);
        bind_group.finish();
        bind_group
    }

    pub(super) fn new(gpu: &Rc<Gpu>) -> Self {
        Self {
            gpu: Rc::downgrade(gpu),
            entries: Weaks::new(),
            layout: None,
            inner: None,
        }
    }

    pub(super) fn add_entry(&mut self, entry: &Rc<Bindable>) {
        self.entries.push(entry);
    }

    pub(super) fn finish_layout(&mut self) {
        let entries = self
            .entries
            .iter()
            .map(|weak| weak.upgrade().unwrap())
            .enumerate()
            .map(|(i, entry)| match entry.as_ref() {
                Bindable::Uniform(uniform_buffer) => wgpu::BindGroupLayoutEntry {
                    binding: i as u32,
                    visibility: uniform_buffer.visibility,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                },
            })
            .collect::<Vec<_>>();

        let gpu = self.gpu.upgrade().unwrap();
        let layout = gpu
            .device
            .create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: Some("Bind group layout"),
                entries: &entries,
            });

        self.layout = Some(layout);
        self.try_release();
    }

    pub(super) fn finish_inner(&mut self) {
        // Upgrades entries.
        self.entries.upgrade_lasting();

        let entries = self.entries.get_upgraded();
        let entries = entries
            .iter()
            .enumerate()
            .map(|(i, entry)| match entry.as_ref() {
                Bindable::Uniform(uniform_buffer) => wgpu::BindGroupEntry {
                    binding: i as u32,
                    resource: uniform_buffer.inner.as_entire_binding(),
                },
            })
            .collect::<Vec<_>>();

        let gpu = self.gpu.upgrade().unwrap();
        let bind_group = gpu.device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Bind group"),
            layout: self.get_layout(),
            entries: &entries,
        });

        self.inner = Some(bind_group);

        // Drop upgraded entries.
        self.entries.drop_upgraded();
        self.try_release();
    }

    pub(super) fn finish(&mut self) {
        if self.layout.is_none() {
            self.finish_layout();
        }
        if self.inner.is_none() {
            self.finish_inner();
        }
        self.try_release();
    }

    pub(super) fn try_release(&mut self) {
        if self.layout.is_some() && self.inner.is_some() {
            self.gpu = Weak::new();
            self.entries = Weaks::new();
        }
    }

    pub(super) fn get_layout(&self) -> &wgpu::BindGroupLayout {
        self.layout.as_ref().unwrap()
    }

    pub(super) fn get_inner(&self) -> &wgpu::BindGroup {
        self.inner.as_ref().unwrap()
    }
}

pub(super) enum Bindable {
    Uniform(UniformBuffer),
    // Storage(StorageBuffer),
    // ...
}

pub(super) struct UniformBuffer {
    inner: wgpu::Buffer,
    visibility: wgpu::ShaderStages,
}

impl UniformBuffer {
    pub(super) fn from_raw(
        device: &wgpu::Device,
        raw: &[u8],
        visibility: Option<wgpu::ShaderStages>,
    ) -> Self {
        let inner = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Uniform buffer"),
            contents: raw,
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
        });
        let visibility = visibility.unwrap_or(wgpu::ShaderStages::VERTEX_FRAGMENT);

        Self { inner, visibility }
    }
}

impl From<UniformBuffer> for Bindable {
    fn from(value: UniformBuffer) -> Self {
        Self::Uniform(value)
    }
}
