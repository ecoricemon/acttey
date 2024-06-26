use super::{
    canvas::{OffCanvas, Surface},
    RenderError,
};
use std::rc::Rc;
use wasm_bindgen::JsCast;

#[derive(Debug)]
pub struct RenderContext {
    pub global: web_sys::DedicatedWorkerGlobalScope,
    pub instance: wgpu::Instance,
    pub adapter: wgpu::Adapter,
}

impl RenderContext {
    pub async fn new(ref_canvas: Rc<OffCanvas>) -> Result<Self, RenderError> {
        // Worker's scope.
        let global = js_sys::global().unchecked_into();

        // wgpu::Instance.
        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
            backends: if cfg!(feature = "webgl") {
                crate::log!("[I] WebGL mode");
                wgpu::Backends::GL
            } else {
                crate::log!("[I] WebGPU mode");
                wgpu::Backends::BROWSER_WEBGPU
            },
            ..Default::default()
        });

        // wgpu::Adapter.
        let dummy_surface = Surface::new(&instance, ref_canvas); // Drops at the end.
        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::default(),
                force_fallback_adapter: false,
                compatible_surface: Some(&dummy_surface.surface),
            })
            .await
            .ok_or(RenderError::RequestAdapterError)?;

        Ok(Self {
            global,
            instance,
            adapter,
        })
    }
}

#[derive(Debug)]
pub struct Gpu {
    pub device: wgpu::Device,
    pub queue: wgpu::Queue,
}

impl Gpu {
    pub async fn new(
        adapter: &wgpu::Adapter,
        features: Option<wgpu::Features>,
        limits: Option<wgpu::Limits>,
    ) -> Result<Self, RenderError> {
        // Unwraps features and limits or set them as default.
        let features = features.unwrap_or(wgpu::Features::empty());
        let limits = limits.unwrap_or(if cfg!(feature = "webgl") {
            crate::log!("[I] Limits: downlevel_webgl2");
            wgpu::Limits::downlevel_webgl2_defaults()
        } else {
            crate::log!("[I] Limits: default");
            wgpu::Limits::default() // WebGL will get error with this limit
        });

        // Checks whether the given `features` is allowed.
        let adapter_features = adapter.features();
        if !adapter_features.contains(features) {
            return Err(RenderError::InvalidFeatures);
        }

        // Checks whether the given `limits` is allowed.
        let adapter_limits = adapter.limits();
        if !limits.check_limits(&adapter_limits) {
            return Err(RenderError::InvalidLimits);
        }

        let (device, queue) = adapter
            .request_device(
                &wgpu::DeviceDescriptor {
                    label: None,
                    required_features: features,
                    required_limits: limits,
                },
                None,
            )
            .await
            .map_err(RenderError::RequestDeviceError)?;

        Ok(Self { device, queue })
    }
}
