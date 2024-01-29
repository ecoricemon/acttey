use crate::{
    ds::generational::{GenIndexRc, GenVecRc},
    render::RenderError,
    util::web,
};
use ahash::AHashMap;
use std::{collections::BTreeMap, mem::ManuallyDrop, rc::Rc};
use wasm_bindgen::prelude::*;

/// A set of textures and views from the surfaces used in a single render pass.
/// In a render pass, call `create_color_attachments()` and `present()`.
pub struct SurfacePack {
    surface_indices: Vec<Option<GenIndexRc>>,
}

impl SurfacePack {
    pub fn new() -> Self {
        Self {
            surface_indices: Vec::new(),
        }
    }

    pub fn add_surface_index(&mut self, index: Option<GenIndexRc>) {
        self.surface_indices.push(index);
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
        self.surface_indices
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
        let (textures, views, ptr, len, cap) = buf.each();
        // Safety: Raw parts are valid because we are calling reflect...
        let mut attachments = unsafe { Vec::from_raw_parts(ptr, len, cap) };

        // Clears textures, views, and color attachments if some left.
        // But, we reuse the capacities.
        let len = self.surface_indices.len();
        textures.clear();
        textures.reserve_exact(len);
        views.clear();
        views.reserve_exact(len);
        attachments.clear();
        attachments.reserve_exact(len);

        // Fills with the new items.
        for index in self.surface_indices.iter().flatten().cloned() {
            let surface = surfaces.get(index.index).unwrap();
            let (texture, view) = surface.create_texture_and_view();
            textures.push(texture);
            views.push(view);
        }
        // We can't borrow `self.texture_views` during writing.
        // That's why we splited the loop.
        for (vi, index) in self.surface_indices.iter().flatten().cloned().enumerate() {
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

    pub fn present(&self, buf: &mut SurfacePackBuffer) {
        while let Some(texture) = buf.surface_textures.pop() {
            texture.present();
        }
        buf.texture_views.clear();
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
    pub fn each(
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

pub struct Surface {
    pub canvas: Rc<Canvas>,
    pub surface: wgpu::Surface,
    pub surface_conf: wgpu::SurfaceConfiguration,
    pub color_target: wgpu::ColorTargetState,
    // To keep no lifetime (lost label instead)
    pub view_desc: wgpu::TextureViewDescriptor<'static>,
    pub color_ops: wgpu::Operations<wgpu::Color>,
}

impl Surface {
    /// Creates surface related to the canvas.
    /// Caller should call `configure()` before using this.
    pub fn new(instance: &wgpu::Instance, canvas: &Rc<Canvas>) -> Self {
        // Safety: `canvas` is owned so that it lives as long as `surface`.
        // TODO: But, if HTML canvas element is removed somewhere?
        let surface = unsafe { instance.create_surface(canvas.as_ref()).ok() }
            .expect_throw("Failed to create surface.");

        // Dummy conf. Caller should call `configure()` to set a proper configuration.
        let surface_conf = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: wgpu::TextureFormat::Bgra8Unorm,
            width: canvas.element.width(),
            height: canvas.element.height(),
            present_mode: wgpu::PresentMode::Fifo,
            alpha_mode: wgpu::CompositeAlphaMode::Auto,
            view_formats: vec![],
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
            canvas: Rc::clone(canvas),
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
        canvas: &Rc<Canvas>,
    ) -> Self {
        let mut inst = Self::new(instance, canvas);
        inst.configure(adapter, device, None);
        inst
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

    #[inline]
    pub fn get_canvas_selectors(&self) -> &Rc<str> {
        &self.canvas.selectors
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
            width: self.canvas.element.width(),
            height: self.canvas.element.height(),
            present_mode: surf_caps.present_modes[0],
            alpha_mode: surf_caps.alpha_modes[0],
            view_formats: vec![],
        });
        self.surface.configure(device, &self.surface_conf);

        // Modifies color target's format according to the conf.
        self.color_target.format = self.surface_conf.format;
    }

    pub fn resize(&mut self, scale_factor: f64, device: &wgpu::Device) {
        let canvas = &self.canvas.element;
        let new_width = (canvas.client_width() as f64 * scale_factor) as u32;
        let new_height = (canvas.client_height() as f64 * scale_factor) as u32;

        if new_width != canvas.width() || new_height != canvas.height() {
            // Configuration of surface will make the canvas have the same size with it.
            self.surface_conf.width = new_width;
            self.surface_conf.height = new_height;
            self.surface.configure(device, &self.surface_conf);

            debug_assert!(
                new_width == canvas.width() && new_height == canvas.height(),
                "Canvas couldn't resize itself"
            );
            debug_assert!(
                new_width == self.surface.get_current_texture().unwrap().texture.width()
                    && new_height == self.surface.get_current_texture().unwrap().texture.height(),
                "Surface couldn't resize itself"
            );
            crate::log!(
                "cavas {} resized: ({}, {})",
                self.canvas.handle,
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

#[derive(Debug)]
pub struct CanvasPack {
    /// Handle to Canvas map.
    handle_to_canvas: BTreeMap<u32, Rc<Canvas>>,
    /// Selectors to Handle map.
    selectors_to_handle: AHashMap<Rc<str>, u32>,
    /// Monotonically increasing handle number.
    cur_handle: u32,
}

impl CanvasPack {
    const HANDLE_DUMMY: u32 = u32::MAX - 1;

    pub fn new() -> Self {
        let mut inst = Self {
            handle_to_canvas: BTreeMap::new(),
            selectors_to_handle: AHashMap::new(),
            cur_handle: Self::HANDLE_DUMMY,
        };

        // Creates dummy canvas to make compatible wgpu::Adapter.
        // TODO: is this really effective? Test it.
        let dummy_id = "acttey-dummy-canvas";
        let dummy_selectors = "#acttey-dummy-canvas";
        let dummy_canvas: web_sys::HtmlCanvasElement =
            web::create_element("canvas").expect_throw(crate::errmsg::WEBSYS_ADD_ELEMENT);
        web::set_attributes(
            &dummy_canvas,
            [("id", dummy_id), ("hidden", "")].into_iter(),
        )
        .unwrap();

        // Adds dummy canvas.
        inst.add(dummy_selectors).unwrap();

        // Handle starts from 1.
        inst.cur_handle = 1;

        inst
    }

    /// Adds the canvas selected by the given `selectors`.
    pub fn add(&mut self, selectors: impl Into<Rc<str>>) -> Result<Rc<Canvas>, RenderError> {
        let selectors = selectors.into();
        let canvas = Rc::new(Canvas::new(Rc::clone(&selectors), self.cur_handle)?);
        if let Some(orphan_handle) = self.selectors_to_handle.insert(selectors, self.cur_handle) {
            self.handle_to_canvas.remove(&orphan_handle);
        }
        self.handle_to_canvas
            .insert(self.cur_handle, Rc::clone(&canvas));
        self.cur_handle += 1;
        Ok(canvas)
    }

    pub fn get_by_selectors(&self, selectors: &str) -> Option<&Rc<Canvas>> {
        if let Some(handle) = self.selectors_to_handle.get(selectors) {
            self.handle_to_canvas.get(handle)
        } else {
            None
        }
    }

    pub fn get_by_handle(&self, handle: &u32) -> Option<&Rc<Canvas>> {
        self.handle_to_canvas.get(handle)
    }

    pub fn get_dummy(&self) -> &Rc<Canvas> {
        self.get_by_handle(&Self::HANDLE_DUMMY).unwrap()
    }

    pub fn get_first(&self) -> Option<&Rc<Canvas>> {
        self.handle_to_canvas
            .values()
            .find(|canvas| canvas.handle != Self::HANDLE_DUMMY)
    }

    pub fn get_last(&self) -> Option<&Rc<Canvas>> {
        self.handle_to_canvas
            .values()
            .rfind(|canvas| canvas.handle != Self::HANDLE_DUMMY)
    }

    pub fn selectors_to_handle(&self, selectors: &str) -> Option<u32> {
        self.selectors_to_handle.get(selectors).cloned()
    }

    /// Time complexity: O(n)
    pub fn handle_to_selectors(&self, handle: u32) -> Option<&str> {
        self.selectors_to_handle
            .iter()
            .find_map(|(selectors, &this_handle)| {
                (this_handle == handle).then_some(selectors.as_ref())
            })
    }

    pub fn contains_selectors(&self, selectors: &str) -> bool {
        self.get_by_selectors(selectors).is_some()
    }

    pub fn contains_handle(&self, handle: &u32) -> bool {
        self.get_by_handle(handle).is_some()
    }

    /// Clears unused canvases from external and returns the number of removed canvases.
    pub fn clear(&mut self) -> usize {
        let unused = self
            .handle_to_canvas
            .iter()
            .filter_map(|(&handle, canvas)| {
                (handle != Self::HANDLE_DUMMY && Rc::strong_count(canvas) == 1).then_some(handle)
            })
            .collect::<Vec<_>>();
        let removed = unused.len();
        for handle in unused {
            self.handle_to_canvas.remove(&handle);
        }
        removed
    }
}

impl Default for CanvasPack {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct Canvas {
    /// HTML element.
    element: web_sys::HtmlCanvasElement,

    // ref: https://docs.rs/raw-window-handle/0.5.0/raw_window_handle/struct.WebWindowHandle.html
    /// Integer handle. This is automatically inserted as an element attribute.
    handle: u32,

    /// CSS selectors that used to find this canvas.
    selectors: Rc<str>,
}

impl Canvas {
    pub fn new(selectors: impl Into<Rc<str>>, handle: u32) -> Result<Self, RenderError> {
        // 0 is reserved for window itself.
        assert!(handle > 0);

        // Injects `data-raw-handle` attribute into the canvas element.
        // This is required by `wgpu::Surface` and `raw-window-handle`.
        let selectors = selectors.into();
        let element = Self::get_canvas_element(&selectors)?;
        web::set_attributes(
            &element,
            [("data-raw-handle", handle.to_string().as_str())].into_iter(),
        )
        .unwrap();

        Ok(Self {
            element,
            handle,
            selectors,
        })
    }

    pub fn get_canvas_element(selectors: &str) -> Result<web_sys::HtmlCanvasElement, RenderError> {
        let element = web::query_selector(selectors)
            .map_err(|_| RenderError::CanvasQueryError(selectors.to_owned()))?;
        let element = element.ok_or(RenderError::CanvasQueryError(selectors.to_owned()))?;
        let canvas = element
            .dyn_into::<web_sys::HtmlCanvasElement>()
            .map_err(|_| RenderError::CanvasQueryError(selectors.to_owned()))?;
        let width = canvas.client_width() as u32;
        let height = canvas.client_height() as u32;
        canvas.set_width(width);
        canvas.set_height(height);
        crate::log!(
            "Canvas({selectors}) size: ({}, {})",
            canvas.width(),
            canvas.height()
        );
        Ok(canvas)
    }

    #[inline]
    pub fn handle(&self) -> u32 {
        self.handle
    }

    #[inline]
    pub fn selectors(&self) -> &Rc<str> {
        &self.selectors
    }
}

unsafe impl raw_window_handle::HasRawWindowHandle for Canvas {
    fn raw_window_handle(&self) -> raw_window_handle::RawWindowHandle {
        use raw_window_handle::{RawWindowHandle, WebWindowHandle};
        let mut handle = WebWindowHandle::empty();
        handle.id = self.handle;
        RawWindowHandle::Web(handle)
    }
}

unsafe impl raw_window_handle::HasRawDisplayHandle for Canvas {
    fn raw_display_handle(&self) -> raw_window_handle::RawDisplayHandle {
        use raw_window_handle::{RawDisplayHandle, WebDisplayHandle};
        let handle = WebDisplayHandle::empty();
        RawDisplayHandle::Web(handle)
    }
}
