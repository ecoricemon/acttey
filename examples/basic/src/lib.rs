use acttey::{ecs::entity::EntityForm, prelude::*, scene::inner::SceneManager};
use wasm_bindgen::prelude::*;

#[derive(Component)]
struct Rotate {
    speed: f32,
    angle: f32,
}

impl_filter!(RotateFilter, Target = (Rotate));
impl_filter!(TransformFilter, Target = (components::Transform));

fn rotate(write: Write<(RotateFilter, TransformFilter)>) {
    let (rot_2d, obj_2d) = write.unwrap();
    for (mut rots, mut tfs) in rot_2d.zip(obj_2d) {
        let len = rots.len();
        for i in 0..len {
            let rot = rots.get(i).unwrap();
            let tf = tfs.get(i).unwrap();
            tf.rotate_y(rot.angle);
            rot.angle += rot.speed;
        }
    }
}

fn setup(res_write: ResWrite<(resources::MeshResource, resources::Scheduler)>) {
    let (mut meshes, mut sched) = res_write.unwrap();

    meshes.add_geometry(MyKey::Box, shapes::Box::new(0.3, 0.3, 0.3));
    meshes.add_material(MyKey::Red, colors::RED);
    meshes.add_material(MyKey::Blue, colors::BLUE);
    meshes.add_mesh(MyKey::RedBox, MyKey::Box, MyKey::Red);
    meshes.add_mesh(MyKey::BlueBox, MyKey::Box, MyKey::Blue);

    let mut ent_reg = EntityForm::new("MyEntity".into(), None, None);
    ent_reg.add_component(tinfo!(components::Transform));
    ent_reg.add_component(tinfo!(components::SceneNode));
    ent_reg.add_component(tinfo!(Rotate));
    let ekey = sched.register_entity(ent_reg);
    let enti = ekey.index();

    let e0 = sched.add_entity(
        enti,
        (
            components::Transform::from_xyz(0.0, 0.0, 0.0),
            components::SceneNode::default(),
            Rotate {
                speed: 0.1,
                angle: 0.0,
            },
        ),
    );
    let e1 = sched.add_entity(
        enti,
        (
            components::Transform::from_xyz(0.5, 0.0, 0.0),
            components::SceneNode::default(),
            Rotate {
                speed: 0.1,
                angle: 0.0,
            },
        ),
    );

    // Makes a scene.
    let mut scene = Scene::new();
    scene
        .add_canvas("#canvas0")
        .add_node(0)
        .with_camera(MyKey::MyCamera, camera::PerspectiveCamera::default());
    let node = scene
        .add_node(0)
        .with_mesh(MyKey::RedBox)
        .with_entity(e0)
        .ret;
    scene
        .add_node(node)
        .with_mesh(MyKey::BlueBox)
        .with_entity(e1);

    // Appends scene.
    sched.append_scene(MyKey::MyScene.into(), scene);

    // Appends systems.
    sched.append_system(systems::resize, u32::MAX);
    sched.append_system(rotate, 60 * 10);
    sched.append_system(systems::update_transform, 60 * 10);
    sched.append_system(systems::Render::new(), 60 * 10);
    sched.append_system(systems::clear_event, u32::MAX);
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

#[wasm_bindgen]
pub struct MyApp(App);

#[wasm_bindgen]
impl MyApp {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        let mut app = App::new();

        // Registers canvas.
        app.register_canvas("", &[EventType::Scale, EventType::Resize])
            .unwrap();
        app.register_canvas("#canvas0", &[EventType::MouseMove, EventType::Click])
            .unwrap();

        // Calls state initializer.
        app.initialize(|state: &mut AppState| {
            state.spawn_worker(2);
            state.append_setup_system(setup);
        });

        Self(app)
    }
}
