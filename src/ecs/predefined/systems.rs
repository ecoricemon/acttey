use crate::{
    app::event::EventManager,
    ecs::{
        predefined::{components, resource::TimeStamp},
        query::{Filter, Query, QueryMut, ResQuery, ResQueryMut},
        system::System,
    },
    render::{canvas::SurfacePackBuffer, resource::RenderResource},
    scene::scene::SceneManager,
    util::key::ResKey,
};
use ahash::AHashMap;

/// Implement dummy constructor for consistency with other predefined systems.
macro_rules! impl_dummy_new {
    ($id:ident) => {
        impl $id {
            pub fn new() -> Self {
                Self
            }
        }
    };
}

/// A system to resize all surfaces contained in active scenes.
/// This system works only if `resize` event is added,
/// but this system doesn't consume the event.
///
/// Please note that `resize` event should be added into window itself.
#[derive(Debug)]
pub struct Resized;

impl_dummy_new!(Resized);

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
            for (_key, scene) in scene_mgr.iter_active_scenes() {
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
#[derive(Debug)]
pub struct ClearInput;

impl_dummy_new!(ClearInput);

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

pub struct DrawableFilter;
impl Filter for DrawableFilter {
    type Target = components::Drawable;
    type All = ();
    type Any = ();
    type None = ();
}

impl Render {
    pub fn new() -> Self {
        Self::default()
    }
}

impl System for Render {
    type Ref = ();
    type Mut = DrawableFilter;
    type ResRef = (RenderResource, TimeStamp);
    type ResMut = SceneManager;

    fn run(
        &mut self,
        _r: <Self::Ref as Query>::Output,
        m: <Self::Mut as QueryMut>::Output,
        rr: <Self::ResRef as ResQuery>::Output,
        rm: <Self::ResMut as ResQueryMut>::Output,
    ) {
        let (render, _time) = rr;
        let scene_mgr = rm;

        // TODO: Improve me.
        for renderables in m {
            for renderable in renderables {
                if renderable.dirty {
                    let key = renderable.get_scene_key();
                    let index = renderable.get_scene_node_index();

                    let scene = scene_mgr.get_scene_mut(&key.into()).unwrap();
                    scene
                        .get_node_mut(index)
                        .unwrap()
                        .update_ltf(renderable.local_transform());

                    renderable.dirty = false;
                }
            }
        }

        for (_key, scene) in scene_mgr.iter_active_scenes_mut() {
            scene.update_transform();
            scene.run(
                &render.surf_packs,
                &mut self.surf_pack_bufs,
                &render.surfaces,
                &mut self.visit_buf,
            );
        }
    }
}
