use wasm_bindgen::prelude::*;
use acttey::*;

#[derive(Component)]
pub struct Bouncing;


#[wasm_bindgen]
pub struct MyApp {
    app: Option<App>,
}

#[wasm_bindgen]
impl MyApp {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            app: None
        }
    }
    
    #[wasm_bindgen]
    pub async fn run(&mut self) {
        let app = App::new().await;
        app.animate();
        self.app = Some(app);
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
