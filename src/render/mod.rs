mod buffer;
mod camera;
mod context;
mod renderer;

pub mod prelude {
    pub use super::RenderError;
}

use crate::{ds::refs::RCell, primitive::mesh::Mesh};
use context::*;
use renderer::*;
use std::rc::Rc;
use thiserror::Error;
use wasm_bindgen::closure::Closure;

pub struct RenderResource {
    context: RenderContext,
    gpu: Rc<Gpu>,
    drawables: Vec<RCell<Drawable>>,
    v_buffers: Vec<Rc<VertexBuffer>>,
    bindables: Vec<Rc<Bindable>>,
    bind_groups: Vec<Rc<BindGroup>>,
    shaders: Vec<Rc<Shader>>,
    renderers: Vec<Renderer>,
}

impl RenderResource {
    pub async fn new(
        features: Option<wgpu::Features>,
        limits: Option<wgpu::Limits>,
    ) -> Result<Self, RenderError> {
        let context = RenderContext::new().await?;
        let gpu = Gpu::new(&context.adapter, features, limits).await?;

        Ok(Self {
            context,
            gpu: Rc::new(gpu),
            drawables: vec![],
            v_buffers: vec![],
            bindables: vec![],
            bind_groups: vec![],
            shaders: vec![],
            renderers: vec![],
        })
    }

    pub fn example<U: bytemuck::Pod>(
        &mut self,
        uniform: U,
        shader: &str,
        shader_v_entry: &str,
        shader_f_entry: &str,
        mesh: Mesh,
    ) {
        let canvas = self.context.canvas_pack.get_first().unwrap();
        let mut drawable = Drawable::new(&self.context.instance, canvas);
        drawable.finish(&self.context.adapter, &self.gpu.device);
        self.drawables.push(RCell::new(drawable));

        let drawable = self.drawables.last().unwrap();

        let uniform: Bindable =
            UniformBuffer::from_raw(&self.gpu.device, bytemuck::cast_slice(&[uniform][..]), None)
                .into();

        let uniform = Rc::new(uniform);
        let bind_group = BindGroup::default(&self.gpu, &uniform);
        let bind_group = Rc::new(bind_group);

        let shader = Shader::new(
            &self.gpu,
            shader,
            &[shader_v_entry][..],
            &[shader_f_entry][..],
        );
        let shader = Rc::new(shader);
        let v_shader = ShaderView::from_as_v(&shader, 0);
        let f_shader = ShaderView::from_as_f(&shader, 0);

        let v_buffer = VertexBuffer::from_mesh(&self.gpu.device, mesh);
        let v_buffer = Rc::new(v_buffer);

        let renderer = Renderer::example(
            &self.gpu,
            drawable,
            Some(&bind_group),
            v_shader,
            f_shader,
            &v_buffer,
        );

        self.v_buffers = vec![v_buffer];
        self.bindables = vec![uniform];
        self.bind_groups = vec![bind_group];
        self.shaders = vec![shader];
        self.renderers = vec![renderer];
    }

    #[inline(always)]
    pub fn register_canvas(&mut self, id: &str) -> u32 {
        self.context.canvas_pack.insert(id)
    }

    #[inline(always)]
    pub fn unregister_canvas(&mut self, id: &str) {
        self.context.canvas_pack.remove_by_id(id)
    }

    #[inline(always)]
    pub fn get_canvas_handle(&self, id: &str) -> Option<u32> {
        self.context.canvas_pack.get_handle(id)
    }

    #[inline(always)]
    pub fn device_pixel_ratio(&self) -> f64 {
        self.context.window.device_pixel_ratio()
    }

    #[inline(always)]
    pub fn render(&mut self, i: usize) {
        self.renderers[i].render();
    }

    #[inline(always)]
    pub fn register_animation_callback(&mut self, callback: Closure<dyn FnMut(f64)>) {
        self.context.register_animation_callback(callback);
    }

    #[inline(always)]
    pub fn request_animation_frame(&self) {
        self.context.request_animation_frame();
    }

    pub fn resize_drawables(&mut self) {
        let scale_factor = self.device_pixel_ratio();
        for drawable in self.drawables.iter_mut() {
            drawable.borrow_mut().resize(scale_factor, &self.gpu.device);
        }
    }
}

#[derive(Error, Debug)]
pub enum RenderError {
    #[error("failed to get gpu adapter")]
    RequestAdapterError,
    #[error("{0:?}")]
    RequestDeviceError(wgpu::RequestDeviceError),
    #[error("invalid features\n adapter:{0:?}, requested:{1:?}")]
    InvalidFeatures(wgpu::Features, wgpu::Features),
    #[error("invalid limits\n adapter:{0:?}, requested:{1:?}")]
    InvalidLimits(wgpu::Limits, wgpu::Limits),
}
