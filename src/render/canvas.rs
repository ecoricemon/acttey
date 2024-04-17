use crate::{
    ds::generational::{GenIndexRc, GenVecRc},
    worker::msg::MsgEventCanvasResize,
};
use std::{mem::ManuallyDrop, ops::Deref, rc::Rc};

/// A set of textures and views from the surfaces used in a single render pass.
/// In a render pass, call [`SurfacePack::create_color_attachments`] and [`SurfacePack::present`].
#[derive(Debug)]
pub struct SurfacePack {
    pub(crate) surf_indices: Vec<Option<GenIndexRc>>,
}

impl SurfacePack {
    pub fn new() -> Self {
        Self {
            surf_indices: Vec::new(),
        }
    }

    pub fn add_surface_index(&mut self, index: Option<GenIndexRc>) {
        self.surf_indices.push(index);
    }

    pub fn iter_surf_indices(&self) -> impl Iterator<Item = &GenIndexRc> {
        self.surf_indices.iter().filter_map(|index| index.as_ref())
    }

    /// Creates a vector of `Option<wgpu::ColorTargetState>`.
    ///
    /// # Panics
    ///
    /// Panics if any surface index is out of bound or old generation.
    pub fn create_color_targets(
        &self,
        surfaces: &GenVecRc<Surface>,
    ) -> Vec<Option<wgpu::ColorTargetState>> {
        self.surf_indices
            .iter()
            .map(|opt_index| {
                opt_index
                    .as_ref()
                    .map(|index| surfaces.get(index.index).unwrap().color_target.clone())
            })
            .collect()
    }

    /// Creates a vector of `Option<wgpu::RenderPassColorAttachment>`.
    /// This uses a vector previously allocated,
    /// which means you can use this on a frame bases without frequent heap allocation.
    /// Use [`Self::present()`] after calling this.
    ///
    /// # Panics
    ///
    /// Panics if any surface index is out of bound or old generation.
    pub fn create_color_attachments<'b>(
        &self,
        surfaces: &GenVecRc<Surface>,
        buf: &'b mut SurfacePackBuffer,
    ) -> ManuallyDrop<Vec<Option<wgpu::RenderPassColorAttachment<'b>>>> {
        let (textures, views, ptr, len, cap) = buf.destructure();
        // Safety: Raw parts are valid because we are calling reflect...
        let mut attachments = unsafe { Vec::from_raw_parts(ptr, len, cap) };

        // Clears textures, views, and color attachments if some left.
        // But, we reuse the capacities.
        let len = self.surf_indices.len();
        textures.clear();
        textures.reserve_exact(len);
        views.clear();
        views.reserve_exact(len);
        attachments.clear();
        attachments.reserve_exact(len);

        // Fills with the new items.
        for index in self.surf_indices.iter().flatten().cloned() {
            let surface = surfaces.get(index.index).unwrap();
            let (texture, view) = surface.create_texture_and_view();
            textures.push(texture);
            views.push(view);
        }
        // We can't borrow `self.texture_views` during writing.
        // That's why we splited the loop.
        for (vi, index) in self.surf_indices.iter().flatten().cloned().enumerate() {
            let surface = surfaces.get(index.index).unwrap();
            attachments.push(Some(wgpu::RenderPassColorAttachment {
                view: &views[vi],
                resolve_target: None,
                ops: surface.get_color_operations(),
            }));
        }

        let (ptr, len, cap) = SurfacePackBuffer::disassemble_color_attachments(attachments);
        buf.reflect_color_attachments(ptr, len, cap);
        buf.get_color_attachments()
    }

    // Question: Why do we do this?
    // What WebGPU API is corresponding to this?
    pub fn present(buf: &mut SurfacePackBuffer) {
        while let Some(texture) = buf.surface_textures.pop() {
            texture.present();
        }
    }
}

impl Default for SurfacePack {
    fn default() -> Self {
        Self::new()
    }
}

// A render system has this struct instead of global render resource,
// that makes it possible to borrow the render resource without mutable authority.
// It can help to make other systems to read the render resource during
// rendering(encoding command buffer).
// Note that this is just a buffer. All operations will be done in [`SurfacePack`].
/// A helper struct for [`SurfacePack`].
/// This grabs [`wgpu::SurfaceTexture`]s, [`wgpu::TextureView`]s, and
/// [`wgpu::RenderPassColorAttachment`]s.
#[derive(Debug)]
pub struct SurfacePackBuffer {
    surface_textures: Vec<wgpu::SurfaceTexture>,
    texture_views: Vec<wgpu::TextureView>,
    attachments_ptr: *mut (), // To eliminate lifetime at RenderPassColorAttachment<'_>.
    attachments_len: usize,
    attachments_cap: usize,
}

impl SurfacePackBuffer {
    pub fn new() -> Self {
        let attachments: Vec<Option<wgpu::RenderPassColorAttachment>> = vec![];
        let raw = Self::disassemble_color_attachments(attachments);

        Self {
            attachments_ptr: raw.0,
            attachments_len: raw.1,
            attachments_cap: raw.2,
            surface_textures: Vec::new(),
            texture_views: Vec::new(),
        }
    }

    // This can help the sturcture can be split into multiple parts.
    /// Caller can generate a vector from the raw parts.
    /// But then, caller must call [`Self::reflect_color_attachments()`]
    /// to reflect the change and guarantee the raw parts are always valid.
    pub fn destructure(
        &mut self,
    ) -> (
        &mut Vec<wgpu::SurfaceTexture>,
        &mut Vec<wgpu::TextureView>,
        *mut Option<wgpu::RenderPassColorAttachment<'_>>,
        usize,
        usize,
    ) {
        (
            &mut self.surface_textures,
            &mut self.texture_views,
            self.attachments_ptr as *mut Option<wgpu::RenderPassColorAttachment<'_>>,
            self.attachments_len,
            self.attachments_cap,
        )
    }

    pub fn reflect_color_attachments(&mut self, ptr: *mut (), len: usize, cap: usize) {
        self.attachments_ptr = ptr;
        self.attachments_len = len;
        self.attachments_cap = cap;
    }

    pub fn get_color_attachments(
        &self,
    ) -> ManuallyDrop<Vec<Option<wgpu::RenderPassColorAttachment<'_>>>> {
        ManuallyDrop::new(self.assemble_color_attachments())
    }

    /// Disassembles the `attachments` into its raw parts, and prevent it to be dropped.
    /// The `attachments` will be finally dropped when this struct is dropped.
    #[inline]
    pub fn disassemble_color_attachments(
        attachments: Vec<Option<wgpu::RenderPassColorAttachment<'_>>>,
    ) -> (*mut (), usize, usize) {
        let mut attachments = ManuallyDrop::new(attachments);
        (
            attachments.as_mut_ptr() as *mut (),
            attachments.len(),
            attachments.capacity(),
        )
    }

    #[inline]
    fn assemble_color_attachments(&self) -> Vec<Option<wgpu::RenderPassColorAttachment<'_>>> {
        unsafe {
            Vec::from_raw_parts(
                self.attachments_ptr as *mut Option<wgpu::RenderPassColorAttachment>,
                self.attachments_len,
                self.attachments_cap,
            )
        }
    }
}

impl Default for SurfacePackBuffer {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for SurfacePackBuffer {
    fn drop(&mut self) {
        // Safety: Same.
        self.assemble_color_attachments();
    }
}

#[derive(Debug)]
pub struct Surface {
    pub offcanvas: Rc<OffCanvas>,
    pub surface: wgpu::Surface<'static>,
    pub surface_conf: wgpu::SurfaceConfiguration,
    pub color_target: wgpu::ColorTargetState,
    // To keep no lifetime (lost label instead)
    pub view_desc: wgpu::TextureViewDescriptor<'static>,
    pub color_ops: wgpu::Operations<wgpu::Color>,
}

impl Surface {
    /// Creates surface related to the canvas.
    /// Caller should call `configure()` before using this.
    pub fn new(instance: &wgpu::Instance, offcanvas: Rc<OffCanvas>) -> Self {
        let surface = instance
            .create_surface(wgpu::SurfaceTarget::OffscreenCanvas(
                web_sys::OffscreenCanvas::clone(&offcanvas.element),
            ))
            .unwrap();

        // Dummy conf. Caller should call `configure()` to set a proper configuration.
        let surface_conf = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: wgpu::TextureFormat::Bgra8Unorm,
            width: offcanvas.width(),
            height: offcanvas.height(),
            present_mode: wgpu::PresentMode::Fifo,
            alpha_mode: wgpu::CompositeAlphaMode::Auto,
            view_formats: vec![],
            desired_maximum_frame_latency: 2,
        };

        // Default color target.
        let color_target = wgpu::ColorTargetState {
            format: surface_conf.format,
            blend: Some(wgpu::BlendState::REPLACE),
            write_mask: wgpu::ColorWrites::ALL,
        };

        // Default texture view desc.
        let view_desc = wgpu::TextureViewDescriptor {
            label: None,
            ..Default::default()
        };

        // Default color operations.
        let color_ops = Default::default();

        Self {
            offcanvas,
            surface,
            surface_conf,
            color_target,
            view_desc,
            color_ops,
        }
    }

    /// Creates a surface from the given canvas and configures it with the default option.
    pub fn default(
        instance: &wgpu::Instance,
        adapter: &wgpu::Adapter,
        device: &wgpu::Device,
        offcanvas: Rc<OffCanvas>,
    ) -> Self {
        let mut inst = Self::new(instance, offcanvas);
        inst.configure(adapter, device, None);
        inst
    }

    #[inline]
    pub fn handle(&self) -> u32 {
        self.offcanvas.handle()
    }

    /// Sets `wgpu::ColorTargetState` without format.
    /// The format is always set by configure().
    pub fn set_color_target(
        &mut self,
        blend: Option<wgpu::BlendState>,
        write_mask: wgpu::ColorWrites,
    ) {
        self.color_target.blend = blend;
        self.color_target.write_mask = write_mask;
    }

    pub fn set_texture_view_descriptor(&mut self, view_desc: wgpu::TextureViewDescriptor) {
        self.view_desc = wgpu::TextureViewDescriptor {
            label: None,
            ..view_desc
        };
    }

    #[inline]
    pub fn set_color_operations(&mut self, ops: wgpu::Operations<wgpu::Color>) {
        self.color_ops = ops;
    }

    #[inline]
    pub fn get_color_operations(&self) -> wgpu::Operations<wgpu::Color> {
        self.color_ops
    }

    /// Configures the surface.
    /// If `conf` is None, it configures the surface in a way, fits well to the adapter.
    /// Note that color target's format is changed along the conf.
    pub fn configure(
        &mut self,
        adapter: &wgpu::Adapter,
        device: &wgpu::Device,
        conf: Option<wgpu::SurfaceConfiguration>,
    ) {
        // Configures the surface.
        let surf_caps = self.surface.get_capabilities(adapter);
        self.surface_conf = conf.unwrap_or(wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: surf_caps.formats[0],
            width: self.offcanvas.width(),
            height: self.offcanvas.height(),
            present_mode: surf_caps.present_modes[0],
            alpha_mode: surf_caps.alpha_modes[0],
            view_formats: vec![],
            desired_maximum_frame_latency: 2,
        });
        self.surface.configure(device, &self.surface_conf);

        // Modifies color target's format according to the conf.
        self.color_target.format = self.surface_conf.format;
    }

    pub fn resize(&mut self, device: &wgpu::Device, scale: f64, msg: MsgEventCanvasResize) {
        let new_width = (msg.width as f64 * scale) as u32;
        let new_height = (msg.height as f64 * scale) as u32;
        if new_width != self.offcanvas.width() || new_height != self.offcanvas.height() {
            self.surface_conf.width = new_width;
            self.surface_conf.height = new_height;
            self.surface.configure(device, &self.surface_conf);
            crate::log!(
                "[D] Surface::resize(): surface({}) has been resized to {} x {}",
                self.handle(),
                new_width,
                new_height
            );
        }
    }

    pub fn create_texture_and_view(&self) -> (wgpu::SurfaceTexture, wgpu::TextureView) {
        let texture = self.surface.get_current_texture().unwrap();
        let view = texture.texture.create_view(&self.view_desc);
        (texture, view)
    }
}

#[derive(Debug, Clone)]
pub struct OffCanvas {
    element: web_sys::OffscreenCanvas,
    handle: u32,
}

impl OffCanvas {
    #[inline]
    pub const fn new(element: web_sys::OffscreenCanvas, handle: u32) -> Self {
        Self { element, handle }
    }

    #[inline]
    pub fn destructure(self) -> (web_sys::OffscreenCanvas, u32) {
        (self.element, self.handle)
    }

    #[inline]
    pub const fn handle(&self) -> u32 {
        self.handle
    }
}

impl Deref for OffCanvas {
    type Target = web_sys::OffscreenCanvas;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.element
    }
}
