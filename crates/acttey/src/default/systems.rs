use super::{components, resources};
use crate::{
    common::AppHasher,
    draw::scene::{Scene, SceneKey},
    log,
    primitive::mesh::MeshKey,
    type_name, unwrap_or,
    util::key::ObjectKey,
};
use my_ecs::{ds::prelude::*, ecs::prelude::*, util::prelude::*};
use std::any::TypeId;

pub(crate) struct EventListenerWaker;

request!(
    pub(crate) EventListenerWakerReq,
    ResWrite = (resources::EcsManager)
);

impl System for EventListenerWaker {
    type Request = EventListenerWakerReq;

    fn run(&mut self, resp: Response<Self::Request>) {
        let mut ecs = resp.res_write;
    }
}

// pub struct SceneHandler;

// impl SceneHandler {
//     fn register_scene(
//         ecs: &mut resources::EcsManager,
//         smgr: &mut resources::SceneManager,
//         key: SceneKey,
//         scene: Scene,
//     ) {
//         log!("@@@ {}", type_name!(Self::register_scene));
//         smgr.register_scene(key, scene);
//     }

//     fn register_entity(
//         ecs: &mut resources::EcsManager,
//         smgr: &mut resources::SceneManager,
//         skey: SceneKey,
//         ekey: EntityKey,
//     ) {
//         log!("@@@ {}", type_name!(Self::register_entity));

//         // Retrieves the scene from the scene key.
//         let scene = unwrap_or!(smgr.get_scene_mut(&skey) => {
//             log!("[W] {}: unknown scene {}", type_name!(Self::register_entity), skey.name());
//             return;
//         });

//         // Turns entity key into entity index.
//         let ent = ecs.get_entity_storage();
//         let ekey = unwrap_or!(ent.convert_entity_key(&ekey, EntityKeyKind::Index) => {
//             log!("[W] {}: unknown entity: {:?}", type_name!(Self::register_entity), ekey);
//             return;
//         });
//         let enti = ekey.index();

//         // Registers the entity to the scene.
//         scene.register_entity(*enti);

//         if let Some(cont) = ent.get_entity_container(&ekey) {
//             if let Ok(mesh_keys) = cont.borrow_column_of::<MeshKey>() {
//                 for mesh_key in mesh_keys.iter() {
//                     log!("@@@ mesh key: {mesh_key:?}");
//                     scene.register_mesh(mesh_key.clone());
//                 }
//             }
//         }

//         log!("@@@ scene: {scene:?}");
//     }
// }

// request!(
//     pub SceneHandlerReq,
//     ResWrite = (
//         resources::EcsManager,
//         resources::SceneManager,
//         resources::SceneEventQueue
//     )
// );

// impl System for SceneHandler {
//     type Request = SceneHandlerReq;

//     fn run(&mut self, resp: Response<Self::Request>) {
//         let (mut ecs, mut smgr, mut queue) = resp.res_write;

//         // Consumes stacked commands.
//         for cmd in queue.drain(..) {
//             match cmd {
//                 cmd::SceneCommand::Register { key, scene } => {
//                     Self::register_scene(&mut ecs, &mut smgr, key, scene);
//                 }
//                 cmd::SceneCommand::RegisterEntity { skey, ekey } => {
//                     Self::register_entity(&mut ecs, &mut smgr, skey, ekey);
//                 }
//             }
//         }
//     }
// }
