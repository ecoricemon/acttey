use crate::{
    top::event::EventManager,
    ecs::{
        predefined::{components, resource::TimeStamp},
        query::Filter,
        system::{SysParam, System},
    },
    render::{canvas::SurfacePackBuffer, resource::RenderResource},
    scene::inner::SceneManager,
};

/// Implement dummy constructor for consistency with other predefined systems.
macro_rules! impl_dummy_new {
    ($id:ident) => {
        impl $id {
            /// Does nothing, but gives you consistency in your code style.
            #[inline(always)]
            #[allow(clippy::new_without_default)]
            pub fn new() -> Self {
                Self
            }
        }
    };
}

/// A system to resize all surfaces contained in active scenes.  
/// This system works only if `resize` event is added,
/// but this system doesn't consume the event.
/// Please note that `resize` event should be added into window itself.
#[derive(Debug)]
pub struct Resized;

impl_dummy_new!(Resized);

impl System for Resized {
    type Read = ();
    type Write = ();
    type ResRead = EventManager;
    type ResWrite = RenderResource;

    fn run(&mut self, param: SysParam<'_, Self>) {
        let ev_mgr = param.res_read;
        let RenderResource {
            gpu,
            surfaces,
            scale,
            ..
        } = param.res_write;

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

/// A system to drop all stacked input events.  
/// It's recommended to add this system on a frame basis.
#[derive(Debug)]
pub struct ClearInput;

impl_dummy_new!(ClearInput);

impl System for ClearInput {
    type Read = ();
    type Write = ();
    type ResRead = ();
    type ResWrite = EventManager;

    fn run(&mut self, param: SysParam<'_, Self>) {
        param.res_write.clear();
    }
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
    type Read = ();
    type Write = DrawableFilter;
    type ResRead = ();
    type ResWrite = SceneManager;

    fn run(&mut self, param: SysParam<'_, Self>) {
        let scene_mgr = param.res_write;

        // Updates local transform.
        for mut getter in param.write {
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
    type Read = ();
    type Write = ();
    type ResRead = (RenderResource, TimeStamp);
    type ResWrite = SceneManager;

    fn run(&mut self, param: SysParam<'_, Self>) {
        let (render, _time) = param.res_read;
        let scene_mgr = param.res_write;

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

pub struct DrawableFilter;
impl Filter for DrawableFilter {
    type Target = components::Drawable;
    type All = ();
    type Any = ();
    type None = ();
}
