use crate::{
    app::event::EventManager,
    ecs::{
        predefined::resource::TimeStamp,
        query::{Query, QueryMut, ResQuery, ResQueryMut},
        system::System,
    },
    render::{
        canvas::SurfacePackBuffer,
        resource::{
            IterBindGroup, IterIndexBuffer, IterRenderPass, IterRenderPipeline, IterVertexBuffer,
            RenderResource,
        },
    },
};

/// A system to resize all surfaces to match with the new size.  
/// This system works only if `resize` event is added,
/// but this system doesn't consume the event.
///
/// Please note that `resize` event should be added into window itself.
pub struct Resized;
impl System for Resized {
    type Ref = ();
    type Mut = ();
    type ResRef = ();
    type ResMut = (EventManager, RenderResource);

    fn run(
        &mut self,
        _r: <Self::Ref as Query>::Output,
        _m: <Self::Mut as QueryMut>::Output,
        _rr: <Self::ResRef as ResQuery>::Output,
        rm: <Self::ResMut as ResQueryMut>::Output,
    ) {
        let (ev_mgr, render) = rm;
        // TODO: Delete me.
        for handle in ev_mgr.iter_resized() {
            crate::log!("resize from {handle}");
        }
        if ev_mgr.iter_resized().count() > 0 {
            render.resize_surfaces();
        }
    }
}

/// A system to drop all stacked input events.  
/// It's recommended to add this system on a frame basis.
pub struct ClearInput;
impl System for ClearInput {
    type Ref = ();
    type Mut = ();
    type ResRef = ();
    type ResMut = EventManager;

    fn run(
        &mut self,
        _r: <Self::Ref as Query>::Output,
        _m: <Self::Mut as QueryMut>::Output,
        _rr: <Self::ResRef as ResQuery>::Output,
        rm: <Self::ResMut as ResQueryMut>::Output,
    ) {
        rm.clear();
    }
}

#[derive(Debug, Default)]
pub struct Render {
    surf_pack_buf: SurfacePackBuffer,
}

impl Render {
    pub fn new() -> Self {
        Self::default()
    }
}

impl System for Render {
    type Ref = ();
    type Mut = ();
    type ResRef = (RenderResource, TimeStamp);
    type ResMut = ();

    fn run(
        &mut self,
        _r: <Self::Ref as Query>::Output,
        _m: <Self::Mut as QueryMut>::Output,
        rr: <Self::ResRef as ResQuery>::Output,
        _rm: <Self::ResMut as ResQueryMut>::Output,
    ) {
        let (render, _time) = rr;
        // TODO: Hard coded.
        for item in render.iter::<IterRenderPass>() {
            item.pass.encode(
                &render.surf_packs,
                &mut self.surf_pack_buf,
                &render.surfaces,
                |pass| {
                    for (i, item) in render.iter::<IterBindGroup>().enumerate() {
                        pass.set_bind_group(i as u32, item.group, &[]);
                    }
                    for (i, item) in render.iter::<IterVertexBuffer>().enumerate() {
                        pass.set_vertex_buffer(i as u32, item.buf.slice(..));
                    }
                    if let Some(item) = render.iter::<IterIndexBuffer>().next() {
                        pass.set_index_buffer(item.buf.slice(..), wgpu::IndexFormat::Uint16);
                    }
                    if let Some(item) = render.iter::<IterRenderPipeline>().next() {
                        pass.set_pipeline(item.pipeline);
                    }
                    pass.draw_indexed(0..36, 0, 0..1);
                },
            );
        }
    }
}
