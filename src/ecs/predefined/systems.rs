use super::{
    components,
    resource::{EventManager, RenderResource, SceneManager},
};
use crate::{
    ecs::{
        request::{Request, Response},
        system::System,
        filter::Filter,
    },
    render::canvas::SurfacePackBuffer,
};

/// Implements dummy constructor for consistency with other predefined systems.
macro_rules! impl_dummy_new {
    ($id:ident) => {
        impl $id {
            /// Does nothing, but gives you consistency in terms of code style.
            #[inline(always)]
            #[allow(clippy::new_without_default)]
            pub fn new() -> Self {
                Self
            }
        }
    };
}

/// A system to render drawable entities.
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
    type Req = RenderRequest;

    fn run(&mut self, resp: Response<Self::Req>) {
        let render = resp.res_read;
        let mut scene_mgr = resp.res_write;

        for (_key, scene) in scene_mgr.iter_active_scenes_mut() {
            scene.run(
                &render.surf_packs,
                &mut self.surf_pack_bufs,
                &render.surfaces,
                &mut self.visit_buf,
            );
        }
    }
}

#[derive(Debug)]
pub struct RenderRequest;
impl Request for RenderRequest {
    type Read = ();
    type Write = ();
    type ResRead = RenderResource;
    type ResWrite = SceneManager;
}

/// A system to resize all surfaces contained in active scenes.
/// This system works only if `resize` event is added,
/// but this system doesn't consume the event.
/// Please note that `resize` event should be added into window itself.
#[derive(Debug)]
pub struct Resized;
impl_dummy_new!(Resized);

impl System for Resized {
    type Req = ResizedRequest;

    fn run(&mut self, resp: Response<Self::Req>) {
        let ev_mgr = resp.res_read;
        let mut render = resp.res_write;

        let RenderResource {
            gpu,
            surfaces,
            scale,
            ..
        } = &mut *render;

        // Can be time consuming, but it'd be okay for small number of surfaces.
        // It's reasonable assumption.
        for msg in ev_mgr.iter_resized() {
            if let Some(surf) = surfaces
                .sneak_iter_occupied_mut()
                .find(|surf| msg.handle == surf.handle())
            {
                surf.resize(&gpu.device, *scale, *msg);
            }
        }
    }
}

#[derive(Debug)]
pub struct ResizedRequest;
impl Request for ResizedRequest {
    type Read = ();
    type Write = ();
    type ResRead = EventManager;
    type ResWrite = RenderResource;
}

/// A system to drop all stacked input events.
/// It's recommended to add this system on a frame basis.
#[derive(Debug)]
pub struct ClearInput;
impl_dummy_new!(ClearInput);

impl System for ClearInput {
    type Req = ClearInputRequest;

    fn run(&mut self, resp: Response<Self::Req>) {
        let mut ev_mgr = resp.res_write;
        ev_mgr.clear();
    }
}

#[derive(Debug)]
pub struct ClearInputRequest;
impl Request for ClearInputRequest {
    type Read = ();
    type Write = ();
    type ResRead = ();
    type ResWrite = EventManager;
}

/// A system to update transformation of all entities.
/// Various systems can update local translation, rotation, and scale in their manners.
/// That means updating local transformations on each system can be inefficient.
/// This system traverses all drawable components and update its local transformations at once
/// according to their final states.
#[derive(Debug)]
pub struct UpdateTransform;
impl_dummy_new!(UpdateTransform);

impl System for UpdateTransform {
    type Req = UpdateTransformRequest;

    fn run(&mut self, resp: Response<Self::Req>) {
        let drawable = resp.write;
        let mut scene_mgr = resp.res_write;

        // Updates local transform.
        for mut getter in drawable {
            for i in 0..getter.len() {
                let drawable = unsafe { getter.get_unchecked(i) };
                if drawable.transform.is_dirty() {
                    // Updates local transformation matrix in drwable component.  
                    drawable.transform.update();

                    // Copies the matrix to the scene.
                    let key = drawable.get_scene_key();
                    let index = drawable.get_scene_node_index();
                    let scene = scene_mgr.get_scene_mut(&key.into()).unwrap();
                    scene
                        .get_node_mut(index)
                        .unwrap()
                        .set_transform(*drawable.transform.get_transform());
                }
            }
        }

        // Updates global transform.
        for (_key, scene) in scene_mgr.iter_active_scenes_mut() {
            scene.update_global_transform();
        }
    }
}

#[derive(Debug)]
pub struct UpdateTransformRequest;
impl Request for UpdateTransformRequest {
    type Read = ();
    type Write = DrawableFilter;
    type ResRead = ();
    type ResWrite = SceneManager;
}

#[derive(Debug)]
pub struct DrawableFilter;
impl Filter for DrawableFilter {
    type Target = components::Drawable;
    type All = ();
    type Any = ();
    type None = ();
}
