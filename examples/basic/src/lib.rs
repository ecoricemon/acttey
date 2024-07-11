use acttey::prelude::*;
use std::{iter, time::Duration};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct MyApp(App);

#[wasm_bindgen]
impl MyApp {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        WBG_INIT.set("default".to_owned()).unwrap();
        let mut app = App::new();

        // Registers canvas.
        app.register_canvas("", &[EventType::Scale, EventType::Resize])
            .unwrap();
        // app.register_canvas("#canvas0", &[EventType::MouseMove, EventType::Click])
        app.register_canvas("#canvas0", &[]).unwrap();

        // Calls state initializer.
        app.initialize(|state: &mut AppState| {
            state.register_event(EventType::Scale);
            state.register_event(EventType::Resize);
            state.register_event(EventType::MouseMove);
            state.register_event(EventType::Click);

            state.spawn_worker(1);
            state.append_setup_system(setup);
        });

        Self(app)
    }
}

enum MyKey {
    MyCamera,
    Box,
    Red,
    Blue,
    RedBox,
    BlueBox,
    MyScene,
}

impl From<MyKey> for u32 {
    fn from(value: MyKey) -> Self {
        value as u32
    }
}

fn setup(input: ResWrite<(resources::CommonStorage, resources::EcsManager)>) {
    let (mut stor, mut ecs) = input.take();

    // Registers mesh.
    stor.register_geometry(MyKey::Box, shapes::Box::new(0.3, 0.3, 0.3));
    stor.register_material(MyKey::Red, colors::RED);
    stor.register_material(MyKey::Blue, colors::BLUE);
    stor.register_mesh(
        MyKey::RedBox,
        iter::once(MyKey::Box),
        iter::once(MyKey::Red),
    );
    stor.register_mesh(
        MyKey::BlueBox,
        iter::once(MyKey::Box),
        iter::once(MyKey::Blue),
    );

    // Registers entity.
    let ekey = ecs.register_entity::<entities::SimpleEntity>();

    ecs.append_once_system(create_scene).unwrap();

    ecs.append_system(foo, NonZeroTick::MAX, false).unwrap();
    ecs.append_system(bar, NonZeroTick::MAX, false).unwrap();
    ecs.append_system(baz, NonZeroTick::MAX, false).unwrap();

    // Adds entity.
    // ecs.add_entity(
    //     ekey.index(),
    //     entities::SimpleEntity {
    //         mesh: MyKey::RedBox.into(),
    //         transform: components::Transform::from_xyz(0.0, 0.0, 0.0),
    //     },
    // );
}

fn create_scene(queue: ResWrite<resources::SceneCommandQueue>) {
    let mut scene = Scene::new();
    scene.register_entity(entities::SimpleEntity::key());

    queue.take().push_back(Command(scene));
}

fn foo() {
    std::thread::sleep(Duration::from_millis(100));
    crate::log!("@@@ foo() on {:?}", std::thread::current().id());
}

fn bar() {
    std::thread::sleep(Duration::from_millis(100));
    crate::log!("@@@ bar() on {:?}", std::thread::current().id());
}

fn baz() {
    std::thread::sleep(Duration::from_millis(100));
    crate::log!("@@@ baz() on {:?}", std::thread::current().id());
}
