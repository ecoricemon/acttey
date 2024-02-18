#![allow(unused)]
use acttey::prelude::*;
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

#[derive(Component, Default)]
struct Rotate {
    speed: f32,
}

#[derive(Entity)]
struct MyEntity {
    tr: components::Drawable,
    rot_speed: Rotate,
}

impl MyEntity {
    fn new() -> Self {
        Self {
            tr: components::Drawable::default(),
            rot_speed: Rotate { speed: 0.1 },
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

struct RotateSystem;
impl System for RotateSystem {
    type Ref = RotateFilter;
    type Mut = TransformFilter;
    type ResRef = ();
    type ResMut = ();

    fn run(
        &mut self,
        rot_2d: <Self::Ref as Query>::Output,
        obj_2d: <Self::Mut as QueryMut>::Output,
        _: <Self::ResRef as ResQuery>::Output,
        _: <Self::ResMut as ResQueryMut>::Output,
    ) {
        for (rots, objs) in rot_2d.zip(obj_2d) {
            for (rot, obj) in rots.iter().zip(objs.iter_mut()) {
                obj.rotate_y(rot.speed);
            }
        }
    }
}

enum MyKey {
    MyCamera,
    Box,
    Red,
    RedBox,
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
pub struct MyApp(Option<App>);

#[wasm_bindgen]
impl MyApp {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self(None)
    }

    #[wasm_bindgen]
    pub async fn run(&mut self) {
        let mut app = App::new().await;

        // Registers canvas and events of interest.
        app.add_canvas("#canvas0")
            .add_listen_event("", "resize")
            .add_listen_event("#canvas0", "mousemove");

        // Registers mesh and its geometry and material.
        app.add_geometry(MyKey::Box, shapes::Box::new(0.3, 0.3, 0.3))
            .add_material(MyKey::Red, colors::RED)
            .add_mesh(MyKey::RedBox, MyKey::Box, MyKey::Red);

        // TODO
        app.register_entity::<MyEntity>()
            .add_entity(0, MyEntity::new());

        // Makes a scene.
        let mut scene = Scene::new();

        // Makes the scene use the canvas.
        scene.add_canvas("#canvas0");

        // Scene is something like tree.
        // Put nodes with camera and mesh.
        scene
            .add_node(0)
            .with_camera(MyKey::MyCamera, camera::PerspectiveCamera::default())
            .add_node(0)
            .with_mesh(MyKey::RedBox)
            .with_entity::<MyEntity>(0);

        // Registers the scene to the app.
        app.add_scene(MyKey::MyScene, scene).unwrap();

        // Registers some systems.
        app.add_system(systems::Resized::new())
            .add_system(RotateSystem)
            .add_system(systems::Render::new())
            .add_system(systems::ClearInput::new());

        // Runs the app.
        app.run();

        self.0 = Some(app);
    }
}
