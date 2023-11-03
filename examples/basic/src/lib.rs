#![allow(unused)]
use acttey::*;
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct MyApp {
    app: Option<App>,
}

#[derive(Component)]
struct CompA {
    v: i32,
}

#[derive(Component)]
struct CompB {
    v: i32,
}

#[wasm_bindgen]
impl MyApp {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self { app: None }
    }

    #[wasm_bindgen]
    pub async fn run(&mut self) {
        let app = App::new().await;
        app.animate();
        self.app = Some(app);

        let mut a = HashMap::new();
        a.insert(0_usize, CompA { v: 0 });

        let mut b = vec![(0_usize, CompB { v: 1 })];

        // app.add_component();
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
        if let Some(app) = self.app.as_mut() {
            app.set_camera(camera_x, camera_y, camera_z, at_x, at_y, at_z);
        }
    }
}
