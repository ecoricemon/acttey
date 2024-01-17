use crate::{
    ds::generational::{GenIndexRc, GenVecRc},
    render::{
        canvas::{Surface, SurfacePack, SurfacePackBuffer},
        Gpu,
    },
};
use std::rc::Rc;

#[derive(Debug)]
pub struct RenderPass {
    gpu: Rc<Gpu>,
    pub label: String,
    pub surf_pack_index: Option<GenIndexRc>,
}

impl RenderPass {
    pub fn new(gpu: &Rc<Gpu>, label: &str) -> Self {
        Self {
            gpu: Rc::clone(gpu),
            label: label.to_owned(),
            surf_pack_index: None,
        }
    }

    pub fn set_surface_pack_index(&mut self, index: GenIndexRc) -> Option<GenIndexRc> {
        self.surf_pack_index.replace(index)
    }

    pub fn encode(
        &self,
        surf_packs: &GenVecRc<SurfacePack>,
        surf_pack_buf: &mut SurfacePackBuffer,
        surfaces: &GenVecRc<Surface>,
        f: impl FnOnce(&mut wgpu::RenderPass),
    ) {
        // Creates command encoder.
        let mut encoder = self
            .gpu
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some(&self.label),
            });

        // Creates color attachments.
        let color_attachments;
        let color_attachments_slice = if let Some(surface_index) = &self.surf_pack_index {
            let surf_pack = surf_packs.get(surface_index.index).unwrap();
            color_attachments = surf_pack.create_color_attachments(surfaces, surf_pack_buf);
            color_attachments.as_slice()
        } else {
            &[]
        };

        // Creates render pass.
        let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some(&self.label),
            color_attachments: color_attachments_slice,
            depth_stencil_attachment: None,
            timestamp_writes: None,
            occlusion_query_set: None,
        });

        // Encodes by user's function.
        f(&mut render_pass);

        // Submits commnad buffer.
        // TODO: Do I need to collect all command buffers?
        drop(render_pass);
        self.gpu.queue.submit(std::iter::once(encoder.finish()));
    }
}
