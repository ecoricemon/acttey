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
            RenderResource,
        },
    },
    scene::scene::SceneManager,
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
                &mut self.surf_pack_buf,
                &render.surfaces,
                &mut self.visit_buf,
            );
        }
    }
}
