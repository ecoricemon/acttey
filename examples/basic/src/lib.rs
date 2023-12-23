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
    type ResRef = Input;
    type ResMut = ();

    fn run(
        &mut self,
        _: <Self::Ref as Query>::Output,
        _: <Self::Mut as QueryMut>::Output,
        input: <Self::ResRef as ResQuery>::Output,
        _: <Self::ResMut as ResQueryMut>::Output,
    ) {
        for (handle, event) in input.iter_mouse_events() {
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

#[wasm_bindgen]
impl MyApp {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self(None)
    }

    #[wasm_bindgen]
    pub async fn run(&mut self) {
        let mut app = App::new().await;
        app.register_canvas("canvas0");
        app.register_canvas("canvas1");

        // Listen to some events.
        app.register_events("canvas0", ["mousemove"].into_iter())
            .register_events("canvas1", ["mousemove"].into_iter())
            .register_events("", ["resize"].into_iter());

        // Box
        let mesh_key =
            app.insert_mesh(shape::Box::new(1.0, 1.0, 1.0, [0, 0, 128, 255].into()).into());

        app.register_entity::<Cube>().insert_entity(
            0,
            Cube {
                renderable: component::Renderable {
                    mesh_key,
                    ..Default::default()
                },
                rot: Rotation {
                    x: 0.1,
                    y: 0.0,
                    z: 0.0,
                },
            },
        );

        app.register_system(system::Resized)
            .register_system(RotateCube { rot: 0.0 })
            .register_system(MouseMove)
            .register_system(system::Render)
            .register_system(system::ClearInput)
            .register_super_system(SuperSystem);

        app.run();
        self.0 = Some(app);
    }

    #[wasm_bindgen]
    pub fn set_camera(
        &mut self,
        camera_x: f32,
        camera_y: f32,
        camera_z: f32,
        at_x: f32,
        at_y: f32,
        at_z: f32,
    ) {
        if let Some(app) = self.0.as_mut() {
            // app.set_camera(camera_x, camera_y, camera_z, at_x, at_y, at_z);
        }
    }
}
