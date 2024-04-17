#![allow(unused)]
use acttey::{
    ds::common::TypeInfo,
    ecs::entity::EntityForm,
    prelude::*,
    render::resource::RenderResource,
    tinfo,
    top::event::EventManager,
    util::string::{ArcStr, RcStr},
};
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

#[derive(Component, Default)]
struct Rotate {
    speed: f32,
    angle: f32,
}

struct MyEntity {
    drawable: components::Drawable,
    rot_speed: Rotate,
}

impl MyEntity {
    fn new(x: f32, y: f32, z: f32) -> Self {
        let mut drawable = components::Drawable::default();
        drawable.transform.translate(x, y, z);

        Self {
            drawable,
            rot_speed: Rotate {
                speed: 0.1,
                angle: 0.0,
            },
        }
    }
}

struct RotateFilter;
impl Filter for RotateFilter {
    type Target = Rotate;
    type All = ();
    type Any = ();
    type None = ();
}

struct TransformFilter;
impl Filter for TransformFilter {
    type Target = components::Drawable;
    type All = ();
    type Any = ();
    type None = ();
}

struct RotateRequest;
impl Request for RotateRequest {
    type Read = ();
    type Write = (RotateFilter, TransformFilter);
    type ResRead = ();
    type ResWrite = ();
    fn entity_write() -> impl IntoIterator<Item = acttey::ecs::entity::OwnedEntityKey> {
        std::iter::empty()
    }
}

struct RotateSystem(i32);
impl System for RotateSystem {
    type Req = RotateRequest;

    fn run(&mut self, resp: acttey::ecs::request::Response<Self::Req>) {
        let (rot_2d, obj_2d) = resp.write;
        for (mut rots, mut objs) in rot_2d.zip(obj_2d) {
            let len = rots.len();
            for i in 0..len {
                let rot = unsafe { rots.get(i).unwrap() };
                let obj = unsafe { objs.get(i).unwrap() };
                obj.transform.rotate_y(rot.angle);
                rot.angle += rot.speed;
            }
        }
    }
}

enum MyKey {
    MyCamera,
    Box,
    Red,
    Blue,
    RedBox,
    BlueBox,
    MyShader,
    MyPipeline,
    MyScene,
}

impl From<MyKey> for u64 {
    fn from(value: MyKey) -> Self {
        value as u64
    }
}

#[wasm_bindgen]
pub struct MyApp(App);

#[wasm_bindgen]
impl MyApp {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        let mut app = App::new();

        // Registers canvas and events.
        let canvas_sel = RcStr::new("#canvas0");
        app.register_canvas(canvas_sel).unwrap();

        // Calls state initializer.
        app.call_initializer(|state: &mut AppState| {
            state.spawn_worker(Some(2));

            // Registers mesh and its geometry and material.
            state
                .add_geometry(MyKey::Box, shapes::Box::new(0.3, 0.3, 0.3))
                .add_material(MyKey::Red, colors::RED)
                .add_material(MyKey::Blue, colors::BLUE)
                .add_mesh(MyKey::RedBox, MyKey::Box, MyKey::Red)
                .add_mesh(MyKey::BlueBox, MyKey::Box, MyKey::Blue);

            let ename = ArcStr::new("MyEntity");
            let mut ent_reg = EntityForm::new(ename, None, None);
            ent_reg.add_component(tinfo!(components::Drawable));
            ent_reg.add_component(tinfo!(Rotate));
            let enti = state.register_entity(ent_reg);

            let MyEntity {
                drawable,
                rot_speed,
            } = MyEntity::new(0.0, 0.0, 0.0);
            state.temp_begin_add_entity(enti);
            state.temp_add_entity_comp(enti, drawable);
            state.temp_add_entity_comp(enti, rot_speed);
            let e0 = state.temp_end_add_entity(enti);
            let MyEntity {
                drawable,
                rot_speed,
            } = MyEntity::new(0.5, 0.0, 0.0);
            state.temp_begin_add_entity(enti);
            state.temp_add_entity_comp(enti, drawable);
            state.temp_add_entity_comp(enti, rot_speed);
            let e1 = state.temp_end_add_entity(enti);

            // Makes a scene.
            let mut scene = Scene::new();
            scene
                .add_canvas(1) // TODO: This is #canvas0. Should worker have selectors too?
                .add_node(0)
                .with_camera(MyKey::MyCamera, camera::PerspectiveCamera::default());
            let node = scene
                .add_node(0)
                .with_mesh(MyKey::RedBox)
                .with_entity(enti, e0)
                .ret;
            scene
                .add_node(node)
                .with_mesh(MyKey::BlueBox)
                .with_entity(enti, e1);

            // Registers the scene to the app.
            state.add_scene(MyKey::MyScene, scene).unwrap();

            // Registers some systems.
            state.register_system(systems::Resized::new());
            state.register_system(RotateSystem(0));
            state.register_system(systems::UpdateTransform::new());
            state.register_system(systems::Render::new());
            state.register_system(systems::ClearInput::new());
        });

        Self(app)
    }
}
