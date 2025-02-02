use crate::{util::macros::debug_format, ActteyError};
use my_ecs::prelude::Resource;
use std::sync::Arc;

#[derive(Debug, Resource)]
pub struct Gpu {
    dev: Arc<wgpu::Device>,
    queue: Arc<wgpu::Queue>,
}

impl Gpu {
    pub(crate) async fn new(
        features: Option<wgpu::Features>,
        limits: Option<wgpu::Limits>,
    ) -> Result<Self, ActteyError> {
        // wgpu::Instance.
        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
            backends: if cfg!(not(target_arch = "wasm32")) {
                wgpu::Backends::PRIMARY
            } else if cfg!(not(feature = "webgl")) {
                wgpu::Backends::BROWSER_WEBGPU
            } else {
                wgpu::Backends::GL
            },
            ..Default::default()
        });

        // wgpu::Adapter.
        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::default(),
                force_fallback_adapter: false,
                compatible_surface: None,
            })
            .await
            .ok_or({
                let reason = debug_format!("invalid gpu adapter request");
                ActteyError::GpuAdapter(reason, ())
            })?;

        // Unwraps features and limits or set them as default.
        let features = features.unwrap_or(wgpu::Features::empty());
        let limits = limits.unwrap_or(if cfg!(feature = "webgl") {
            wgpu::Limits::downlevel_webgl2_defaults()
        } else {
            wgpu::Limits::default() // WebGL will get error with this limit
        });

        // Checks whether the given `features` is allowed.
        let adapter_features = adapter.features();
        if !adapter_features.contains(features) {
            let reason = debug_format!("invalid gpu feature");
            return Err(ActteyError::GpuFeature(reason, ()));
        }

        // Checks whether the given `limits` is allowed.
        let adapter_limits = adapter.limits();
        if !limits.check_limits(&adapter_limits) {
            let reason = debug_format!("invalid gpu limit");
            return Err(ActteyError::GpuLimit(reason, ()));
        }

        let (dev, queue) = adapter
            .request_device(
                &wgpu::DeviceDescriptor {
                    label: None,
                    required_features: features,
                    required_limits: limits,
                    memory_hints: wgpu::MemoryHints::default(),
                },
                None,
            )
            .await
            .map_err(|_e| {
                let reason = debug_format!("invalid gpu device request");
                ActteyError::GpuDevice(reason, ())
            })?;

        Ok(Self {
            dev: Arc::new(dev),
            queue: Arc::new(queue),
        })
    }

    pub fn device(&self) -> &Arc<wgpu::Device> {
        &self.dev
    }

    pub fn queue(&self) -> &Arc<wgpu::Queue> {
        &self.queue
    }
}
