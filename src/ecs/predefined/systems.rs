use crate::{
    app::event::EventManager,
    ecs::{
        predefined::resource::TimeStamp,
        query::{Query, QueryMut, ResQuery, ResQueryMut},
        system::System,
    },
    render::{canvas::SurfacePackBuffer, resource::RenderResource},
    scene::scene::SceneManager,
};

/// A system to resize all surfaces contained active scenes.
/// This system works only if `resize` event is added,
/// but this system doesn't consume the event.
///
/// Please note that `resize` event should be added into window itself.
pub struct Resized;
impl System for Resized {
    type Ref = ();
    type Mut = ();
    type ResRef = (EventManager, SceneManager);
    type ResMut = RenderResource;

    fn run(
        &mut self,
        _r: <Self::Ref as Query>::Output,
        _m: <Self::Mut as QueryMut>::Output,
        rr: <Self::ResRef as ResQuery>::Output,
        rm: <Self::ResMut as ResQueryMut>::Output,
    ) {
        let (ev_mgr, scene_mgr) = rr;
        let render = rm;
        if ev_mgr.iter_resized().next().is_some() {
            for scene in scene_mgr.iter_active_scene() {
                let scale_factor = render.device_pixel_ratio();
                let RenderResource {
                    gpu,
                    surfaces,
                    surf_packs,
                    ..
                } = render;
                let surf_pack = surf_packs.get(scene.surf_pack_index.index).unwrap();
                for index in surf_pack.iter_surf_indices() {
                    let surf = surfaces.sneak_get_mut(index.index).unwrap();
                    surf.resize(scale_factor, &gpu.device);
                }
            }
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
    // To keep borrowing resources without mutation.
    surf_pack_bufs: Vec<SurfacePackBuffer>,

    // To keep borrowing resources without mutation.
    visit_buf: Vec<bool>,
}

impl Render {
    pub fn new() -> Self {
        Self::default()
    }
}

impl System for Render {
    type Ref = ();
    type Mut = ();
    type ResRef = (SceneManager, RenderResource, TimeStamp);
    type ResMut = ();

    fn run(
        &mut self,
        _r: <Self::Ref as Query>::Output,
        _m: <Self::Mut as QueryMut>::Output,
        rr: <Self::ResRef as ResQuery>::Output,
        _rm: <Self::ResMut as ResQueryMut>::Output,
    ) {
        let (scene_mgr, render, _time) = rr;

        for scene in scene_mgr.iter_active_scene() {
            scene.run(
                &render.surf_packs,
                &mut self.surf_pack_bufs,
                &render.surfaces,
                &mut self.visit_buf,
            );
        }
    }
}
