use super::{components, resources};
use crate::{
    ecs::{
        query::{Read, ResRead, ResWrite, Write},
        request::Response,
        system::System,
    },
    impl_filter, impl_request,
    render::canvas::SurfacePackBuffer,
};

/// A system for rendering active scenes.
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

        for (_key, scene) in scene_mgr.iter_active_scene_mut() {
            scene.run(
                &render.surf_packs,
                &mut self.surf_pack_bufs,
                &render.surfaces,
                &mut self.visit_buf,
            );
        }
    }
}

impl_request!(
    pub RenderRequest,
    ResRead = (resources::RenderResource),
    ResWrite = (resources::SceneManager)
);

/// A system for resizing all surfaces contained in active scenes.
/// This system works if and only if `resize` event is added,
/// but this system doesn't consume the event.
///
/// Please note that `resize` event should be added into **window** itself, not a canvas.
pub fn resize(
    ev_mgr: ResRead<resources::EventManager>,
    mut render: ResWrite<resources::RenderResource>,
) {
    let resources::RenderResource {
        gpu,
        surfaces,
        scale,
        ..
    } = &mut **render;

    // Can be time consuming, but it'd be okay for small number of surfaces.
    // It's reasonable assumption.
    for msg in ev_mgr.iter_resize() {
        if let Some(surf) = surfaces
            .sneak_iter_occupied_mut()
            .find(|surf| msg.handle == surf.handle())
        {
            surf.resize(&gpu.device, *scale, *msg);
        }
    }
}

/// A system for releasing all stacked events.
/// It's recommended to add this system on a frame basis.
pub fn clear_event(mut ev_mgr: ResWrite<resources::EventManager>) {
    ev_mgr.clear();
}

/// A system for updating transformation matrices of all entities.
/// Various systems can update local translation, rotation, and scale in their manners.
/// That means updating local transformations on each system can be inefficient.
/// This system traverses all drawable components and update its local transformations at once
/// according to their final states.
pub fn update_transform(
    scene_node: Read<SceneNodeFilter>,
    transform: Write<TransformFilter>,
    mut scene_mgr: ResWrite<resources::SceneManager>,
) {
    // Updates local transformation.
    debug_assert_eq!(scene_node.len(), transform.len());
    for (node_getter, mut tf_getter) in scene_node.unwrap().zip(transform.unwrap()) {
        debug_assert_eq!(node_getter.etag, tf_getter.etag);
        for i in 0..node_getter.len() {
            let node = unsafe { node_getter.get_unchecked(i) };
            let tf = unsafe { tf_getter.get_unchecked(i) };
            if tf.is_dirty() {
                let scene = scene_mgr.get_scene_mut(&node.scene_key.into()).unwrap();
                let local = scene.hierarchy.get_local_mut(node.node_index).unwrap();

                tf.update_to(local);
            }
        }
    }

    // Updates global transform.
    for (_key, scene) in scene_mgr.iter_active_scene_mut() {
        scene.update_global_transform();
    }
}

impl_filter!(
    pub TransformFilter,
    Target=(components::Transform),
    All=(components::SceneNode)
);
impl_filter!(
    pub SceneNodeFilter,
    Target=(components::SceneNode),
    All=(components::Transform)
);
