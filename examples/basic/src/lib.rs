#![allow(unused)]
use acttey::prelude::*;
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct MyApp(Option<App>);

#[derive(Component)]
struct Rotation {
    x: f32,
    y: f32,
    z: f32,
}

#[derive(Entity)]
struct Cube {
    renderable: component::Renderable,
    rot: Rotation,
}

struct RotFilter;
impl Filter for RotFilter {
    type Target = Rotation;
    type All = ();
    type Any = ();
    type None = ();
}

struct TransformFilter;
impl Filter for TransformFilter {
    type Target = component::Renderable;
    type All = ();
    type Any = ();
    type None = ();
}

struct RotateCube {
    rot: f32,
}
impl System for RotateCube {
    type Ref = RotFilter;
    type Mut = TransformFilter;
    type ResRef = ();
    type ResMut = ();

    fn run(
        &mut self,
        rot: <Self::Ref as Query>::Output,
        tr: <Self::Mut as QueryMut>::Output,
        _: <Self::ResRef as ResQuery>::Output,
        _: <Self::ResMut as ResQueryMut>::Output,
    ) {
        // for (rots, renderables) in r.zip(m) {
        //     for (rot, renderable) in rots.iter().zip(renderables.iter_mut()) {
        //         renderable.transformation = transform::rotate_x(rot.x);
        //         self.rot += rot.x;
        //         rm.uniform_model.model = transform::rotate_x(self.rot);
        //     }
        // }
    }
}

struct MouseMove;
impl System for MouseMove {
    type Ref = ();
    type Mut = ();
    type ResRef = EventManager;
    type ResMut = ();

    fn run(
        &mut self,
        _: <Self::Ref as Query>::Output,
        _: <Self::Mut as QueryMut>::Output,
        ev_mgr: <Self::ResRef as ResQuery>::Output,
        _: <Self::ResMut as ResQueryMut>::Output,
    ) {
        for (handle, event) in ev_mgr.iter_mouse_events() {
            let x = event.offset_x();
            let y = event.offset_y();
            crate::log!("mouse move from {handle}: ({x}, {y})");
        }
    }
}

struct SuperSystem;
impl System for SuperSystem {
    type Ref = ();
    type Mut = ();
    type ResRef = ();
    type ResMut = Systems;

    fn run(
        &mut self,
        _: <Self::Ref as Query>::Output,
        _: <Self::Mut as QueryMut>::Output,
        _: <Self::ResRef as ResQuery>::Output,
        systems: <Self::ResMut as ResQueryMut>::Output,
    ) {
        // TODO: See the comment about `Systems`.
    }
}

enum MyKey {
    MyCamera,
    BoxGeometry,
    RedMaterial,
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
impl MyApp {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self(None)
    }

    #[wasm_bindgen]
    pub async fn run(&mut self) {
        let mut app = App::new().await;

        app.add_canvas("#canvas0")
            .add_listen_event("", "resize")
            .add_listen_event("#canvas0", "mousemove")
            .add_geometry(MyKey::BoxGeometry, shapes::Box::new(0.3, 0.3, 0.3))
            .add_material(MyKey::RedMaterial, colors::RED)
            .add_mesh(MyKey::RedBox, MyKey::BoxGeometry, MyKey::RedMaterial);

        let mut scene = Scene::new();
        scene
            .add_canvas("#canvas0")
            .add_node(None)
            .with_camera(MyKey::MyCamera, camera::PerspectiveCamera::default())
            .with_mesh(MyKey::RedBox);
        app.add_scene(MyKey::MyScene, scene).unwrap();

        app.add_system(systems::Resized)
            .add_system(systems::Render::new())
            .add_system(systems::ClearInput)
            .run();

        self.0 = Some(app);
    }
}
