use crate::render::RenderError;
use ahash::AHashMap;
use std::{collections::BTreeMap, rc::Rc};
use wasm_bindgen::prelude::*;

pub(super) struct RenderContext {
    pub(super) window: web_sys::Window,
    pub(super) instance: wgpu::Instance,
    pub(super) adapter: wgpu::Adapter,
    pub(super) canvas_pack: CanvasPack,
    animate: Closure<dyn FnMut(f64)>,
}

impl RenderContext {
    pub(super) async fn new() -> Result<Self, RenderError> {
        // web_sys::Window.
        let window = crate::util::get_window();

        // wgpu::Instance.
        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
            backends: if cfg!(feature = "webgl") {
                crate::log!("[Info] WebGL mode");
                wgpu::Backends::GL
            } else {
                crate::log!("[Info] WebGPU mode");
                wgpu::Backends::BROWSER_WEBGPU
            },
            ..Default::default()
        });

        // CanvasPack.
        let canvas_pack = CanvasPack::new();

        // wgpu::Adapter.
        let dummy_canvas = canvas_pack.get_dummy();
        let dummy_drawable = Drawable::new(&instance, &dummy_canvas); // Drops at the end.
        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::default(),
                force_fallback_adapter: false,
                compatible_surface: Some(&dummy_drawable.surface),
            })
            .await
            .ok_or(RenderError::RequestAdapterError)?;

        // Dummy animate, should be changed by `register_animation_callback`.
        let animate = Closure::new(|_| {});

        Ok(Self {
            window,
            instance,
            adapter,
            canvas_pack,
            animate,
        })
    }

    pub(super) fn register_animation_callback(&mut self, callback: Closure<dyn FnMut(f64)>) {
        self.animate = callback;
    }

    #[inline(always)]
    pub(super) fn request_animation_frame(&self) {
        self.window
            .request_animation_frame(self.animate.as_ref().unchecked_ref())
            .expect_throw(crate::errmsg::WEBSYS_REQ_ANIMATION);
    }
}

pub(super) struct Gpu {
    pub(super) device: wgpu::Device,
    pub(super) queue: wgpu::Queue,
}

impl Gpu {
    pub(super) async fn new(
        adapter: &wgpu::Adapter,
        features: Option<wgpu::Features>,
        limits: Option<wgpu::Limits>,
    ) -> Result<Self, RenderError> {
        // Unwraps features and limits or set them as default.
        let features = features.unwrap_or(wgpu::Features::empty());
        let limits = limits.unwrap_or(if cfg!(feature = "webgl") {
            crate::log!("[Info] Limits: downlevel_webgl2");
            wgpu::Limits::downlevel_webgl2_defaults()
        } else {
            crate::log!("[Info] Limits: default");
            wgpu::Limits::default() // WebGL will get error with this limit
        });

        // Checks whether the given `features` is allowed.
        let adapter_features = adapter.features();
        if !adapter_features.contains(features.clone()) {
            return Err(RenderError::InvalidFeatures(adapter_features, features));
        }

        // Checks whether the given `limits` is allowed.
        let adapter_limits = adapter.limits();
        if !limits.check_limits(&adapter_limits) {
            return Err(RenderError::InvalidLimits(adapter_limits, limits));
        }

        let (device, queue) = adapter
            .request_device(
                &wgpu::DeviceDescriptor {
                    label: None,
                    features,
                    limits,
                },
                None,
            )
            .await
            .map_err(|err| RenderError::RequestDeviceError(err))?;

        Ok(Self { device, queue })
    }
}

pub(super) struct CanvasPack {
    by_id: AHashMap<String, (Rc<Canvas>, u32)>,
    by_handle: BTreeMap<u32, (Rc<Canvas>, String)>,
    cur_handle: u32,
}

impl CanvasPack {
    const HANDLE_DUMMY: u32 = u32::MAX - 1;

    pub(super) fn new() -> Self {
        let mut instance = Self {
            by_id: AHashMap::new(),
            by_handle: BTreeMap::new(),
            cur_handle: Self::HANDLE_DUMMY,
        };

        // Creates dummy canvas to make compatible wgpu::Adapter.
        // Well, let's see the adpater will be compatible with real canvases.
        let dummy_id = "acttey-dummy-canvas";
        let dummy_canvas: web_sys::HtmlCanvasElement =
            crate::util::create_element("canvas").expect_throw(crate::errmsg::WEBSYS_ADD_ELEMENT);
        crate::util::set_attributes(
            &dummy_canvas,
            [("id", dummy_id), ("hidden", "")].into_iter(),
        )
        .unwrap();
        instance.insert(dummy_id);

        // Handle starts from 1.
        instance.cur_handle = 1;

        instance
    }

    /// Inserts a canvas having the given `id`.
    /// Returns canvas handle, which starts from 1 and always increases whenever you insert.
    pub(super) fn insert(&mut self, id: &str) -> u32 {
        let canvas = Rc::new(Canvas::new(id, self.cur_handle));

        // Inserts into `by_id`.
        self.by_id
            .insert(id.to_owned(), (Rc::clone(&canvas), self.cur_handle));

        // Inserts into `by_handle`.
        self.by_handle
            .insert(self.cur_handle, (canvas, id.to_owned()));

        // Increses `cur_handle`.
        self.cur_handle += 1;

        self.cur_handle - 1
    }

    pub(super) fn remove_by_id(&mut self, id: &str) {
        if let Some((_, handle)) = self.by_id.remove(id) {
            self.by_handle.remove(&handle);
        }
    }

    pub(super) fn remove_by_handle(&mut self, handle: &u32) {
        if let Some((_, id)) = self.by_handle.remove(handle) {
            self.by_id.remove(&id);
        }
    }

    pub(super) fn get_by_id(&self, id: &str) -> Option<&Rc<Canvas>> {
        self.by_id.get(id).map(|(elem, _)| elem)
    }

    pub(super) fn get_by_handle(&self, handle: &u32) -> Option<&Rc<Canvas>> {
        self.by_handle.get(handle).map(|(elem, _)| elem)
    }

    pub(super) fn get_dummy(&self) -> &Rc<Canvas> {
        self.get_by_handle(&Self::HANDLE_DUMMY).unwrap()
    }

    pub(super) fn get_first(&self) -> Option<&Rc<Canvas>> {
        self.by_handle
            .iter()
            .filter_map(|(&handle, (canvas, _))| (handle != Self::HANDLE_DUMMY).then(|| canvas))
            .next()
    }

    pub(super) fn get_last(&self) -> Option<&Rc<Canvas>> {
        self.by_handle
            .iter()
            .rev()
            .filter_map(|(&handle, (canvas, _))| (handle != Self::HANDLE_DUMMY).then(|| canvas))
            .next()
    }

    pub(super) fn get_handle(&self, id: &str) -> Option<u32> {
        self.by_id.get(id).map(|(_, handle)| *handle)
    }

    pub(super) fn contains_id(&self, id: &str) -> bool {
        self.by_id.contains_key(id)
    }

    pub(super) fn contains_handle(&self, handle: &u32) -> bool {
        self.by_handle.contains_key(handle)
    }
}

#[derive(Debug)]
pub(super) struct Canvas {
    element: web_sys::HtmlCanvasElement,
    // ref: https://docs.rs/raw-window-handle/0.5.0/raw_window_handle/struct.WebWindowHandle.html
    handle: u32,
}

impl Canvas {
    pub(super) fn new(id: &str, handle: u32) -> Self {
        // 0 is reserved for window itself.
        assert!(handle > 0);

        // Injects `data-raw-handle` attribute into the canvas element.
        // This is required by `wgpu::Surface` and `raw-window-handle`.
        let element = Self::get_canvas_element(id).expect_throw(crate::errmsg::WEBSYS_GET_ELEMENT);
        crate::util::set_attributes(
            &element,
            [("data-raw-handle", handle.to_string().as_str())].into_iter(),
        )
        .unwrap();

        Self { element, handle }
    }

    pub(super) fn get_canvas_element(id: &str) -> Option<web_sys::HtmlCanvasElement> {
        let element = crate::util::get_element_by_id(id)?;
        let canvas = element.dyn_into::<web_sys::HtmlCanvasElement>().ok()?;
        let width = canvas.client_width() as u32;
        let height = canvas.client_height() as u32;
        canvas.set_width(width);
        canvas.set_height(height);
        crate::log!(
            "Canvas({id}) size: ({}, {})",
            canvas.width(),
            canvas.height()
        );
        Some(canvas)
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

pub(super) struct Drawable {
    canvas: Rc<Canvas>,
    surface: wgpu::Surface,
    surface_config: Option<wgpu::SurfaceConfiguration>,
}

impl Drawable {
    pub(super) fn new(instance: &wgpu::Instance, canvas: &Rc<Canvas>) -> Self {
        // Safety: `canvas` is owned so that it lives as long as `surface`.
        let surface = unsafe { instance.create_surface(canvas.as_ref()).ok() }
            .expect_throw("Failed to create surface.");

        Self {
            canvas: Rc::clone(canvas),
            surface,
            surface_config: None,
        }
    }

    pub(super) fn finish(&mut self, adapter: &wgpu::Adapter, device: &wgpu::Device) {
        // Configures the surface.
        let surface_caps = self.surface.get_capabilities(adapter);
        let surface_config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: surface_caps.formats[0],
            width: self.canvas.element.width(),
            height: self.canvas.element.height(),
            present_mode: surface_caps.present_modes[0],
            alpha_mode: surface_caps.alpha_modes[0],
            view_formats: vec![],
        };
        self.surface.configure(device, &surface_config);
        self.surface_config = Some(surface_config);
    }

    pub(super) fn color_target(&self) -> wgpu::ColorTargetState {
        let surface_config = self.surface_config.as_ref().unwrap();
        wgpu::ColorTargetState {
            format: surface_config.format,
            blend: Some(wgpu::BlendState::REPLACE),
            write_mask: wgpu::ColorWrites::ALL,
        }
    }

    #[inline(always)]
    pub(super) fn get_current_texture(&self) -> wgpu::SurfaceTexture {
        self.surface.get_current_texture().unwrap()
    }

    pub(super) fn resize(&mut self, scale_factor: f64, device: &wgpu::Device) {
        let canvas = &self.canvas.element;
        let new_width = (canvas.client_width() as f64 * scale_factor) as u32;
        let new_height = (canvas.client_height() as f64 * scale_factor) as u32;

        if new_width != canvas.width() || new_height != canvas.height() {
            // Configuration of surface will make the canvas have the same size with it.
            if let Some(surface_config) = self.surface_config.as_mut() {
                surface_config.width = new_width;
                surface_config.height = new_height;
                self.surface.configure(device, surface_config);

                debug_assert!(
                    new_width == canvas.width() && new_height == canvas.height(),
                    "Canvas couldn't resize itself"
                );
                debug_assert!(
                    new_width == self.surface.get_current_texture().unwrap().texture.width()
                        && new_height
                            == self.surface.get_current_texture().unwrap().texture.height(),
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
    }
}
