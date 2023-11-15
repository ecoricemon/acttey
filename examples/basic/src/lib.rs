#![allow(unused)]
use acttey::*;
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct MyApp(Option<App>);

#[derive(Component, Debug)]
struct Position {
    x: f32,
    y: f32,
    z: f32,
}

#[derive(Component, Debug)]
struct Velocity {
    x: f32,
    y: f32,
    z: f32,
}

#[derive(Entity)]
struct Cube {
    pos: Position,
    vel: Velocity,
}

struct PosFilter;
impl Filter for PosFilter {
    type Target = Position;
    type FilterAll = ();
    type FilterAny = ();
    type FilterNone = ();
}

struct VelFilter;
impl Filter for VelFilter {
    type Target = Velocity;
    type FilterAll = ();
    type FilterAny = ();
    type FilterNone = ();
}

struct MoveSystem;
impl System for MoveSystem {
    type Ref = VelFilter;
    type Mut = PosFilter;

    fn run(&self, r: <Self::Ref as Query>::Output, m: <Self::Mut as QueryMut>::Output) {
        for (vel, pos) in r.zip(m) {
            for (v, p) in vel.iter().zip(pos.iter_mut()) {
                p.x += v.x;
                p.y += v.y;
                p.z += v.z;
            }
            crate::log!("{:?}", pos);
        }
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
        app.regist_entity::<Cube>()
            .insert_entity(
                0,
                Cube {
                    pos: Position {
                        x: 0.0,
                        y: 0.0,
                        z: 0.0,
                    },
                    vel: Velocity {
                        x: 0.1,
                        y: 0.1,
                        z: 0.1,
                    },
                },
            )
            .regist_system(MoveSystem)
            .animate();
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
            app.set_camera(camera_x, camera_y, camera_z, at_x, at_y, at_z);
        }
    }
}
