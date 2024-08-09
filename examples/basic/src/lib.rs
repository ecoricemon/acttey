use acttey::prelude::*;
use my_ecs::filter;
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
        app.register_canvas("#canvas0", &[EventType::MouseMove, EventType::Click])
            .unwrap();
        app.register_canvas("#canvas1", &[EventType::Click])
            .unwrap();

        // Calls state initializer.
        app.initialize(|state: &mut AppState| {
            state.spawn_worker(3);
            state.append_setup_system(setup);
        });

        Self(app)
    }
}

#[derive(EnumObjectKey)]
enum MyKey {
    MyCamera,
    Box,
    Red,
    Blue,
    RedBox,
    BlueBox,
    MyScene,
}

fn setup(input: ResWrite<(resources::CommonStorage, resources::EcsManager)>) {
    let (mut stor, mut ecs) = input.take();

    MyKey::set_to_global().unwrap();

    // Registers mesh.
    stor.register_geometry(MyKey::Box, shapes::Box::new(0.3, 0.3, 0.3));
    stor.register_material(MyKey::Red, colors::RED);
    stor.register_material(MyKey::Blue, colors::BLUE);
    stor.register_mesh(MyKey::RedBox, iter::once((MyKey::Box, MyKey::Red)));
    stor.register_mesh(MyKey::BlueBox, iter::once((MyKey::Box, MyKey::Blue)));

    // Registers entity.
    let enti = ecs.register_entity_of::<entities::SimpleEntity>();

    // `RedBox` is added before scene.
    ecs.add_entity(
        enti,
        entities::SimpleEntity {
            mesh_key: MeshKey::from(MyKey::RedBox),
            transform: Default::default(),
        },
    )
    .unwrap();

    // Test
    ecs.register_entity_of::<Ea>();
    ecs.register_entity_of::<Eb>();
    ecs.append_once_system(0, insert_ea).unwrap();
    ecs.append_once_system(0, insert_eb).unwrap();
    let live = NonZeroTick::new(1 * 1000 * 1000).unwrap();
    ecs.append_system(0, live, false, inc_ca).unwrap();
    ecs.append_system(0, live, false, dec_ca).unwrap();
    ecs.append_system(0, live, false, iter_ca).unwrap();
    ecs.append_system(0, live, false, iter_ca).unwrap();
    // ecs.append_system(0, foo, live, false).unwrap();
    // ecs.append_system(0, bar, live, false)
    //     .unwrap();

    // use std::sync::{Arc, Mutex};

    // let shared = Arc::new(Mutex::new(Vec::new()));
    // let shared_a = Arc::clone(&shared);

    // ecs.append_system(
    //     0,
    //     move |r: Read<(Fa, Fb)>| {
    //         let mut test = shared_a.lock().unwrap();

    //         let (a, b) = r.take();

    //         for item in a.iter().flatten() {
    //             test.push(item.0);
    //         }

    //         for item in b.iter().flatten() {
    //             test.push(item.0);
    //         }
    //     },
    //     NonZeroTick::new(2).unwrap(),
    //     true,
    // )
    // .unwrap();

    // ecs.register_entity_of::<Ea>();
    // ecs.register_entity_of::<Eb>();

    // ecs.append_once_system(0, insert_ea).unwrap();
    // ecs.append_once_system(0, insert_eb).unwrap();
}

#[derive(Component, Debug, Clone, Copy)]
struct Ca(i32);

#[derive(Component, Debug, Clone, Copy)]
struct Cb(i32);

#[derive(Entity)]
#[entity_hasher(AppHasher)]
struct Ea {
    a: Ca,
}

#[derive(Entity)]
#[entity_hasher(AppHasher)]
struct Eb {
    a: Ca,
    b: Cb,
}

filter!(Fa, Target = (Ca));
filter!(Fb, Target = (Cb));

fn insert_ea(ew: EntWrite<Ea>) {
    let mut ew = ew.take();
    ew.add_entity(Ea { a: Ca(1) });
    ew.add_entity(Ea { a: Ca(2) });
}

fn insert_eb(ew: EntWrite<Eb>) {
    let mut ew = ew.take();
    ew.add_entity(Eb { a: Ca(3), b: Cb(5) });
    ew.add_entity(Eb { a: Ca(4), b: Cb(6) });
}

fn inc_ca(w: Write<Fa>) {
    let mut test = [0; 4];
    let mut i = 0;

    for mut getter in w.take().iter_mut() {
        for ca in getter.getter.iter_mut() {
            ca.0 += 1;

            test[i] = ca.0;
            i += 1;
        }
    }

    test.sort_unstable();
    assert_eq!(test, [2, 3, 4, 5]);
    crate::log!("@@@ inc_ca OK");
}

fn dec_ca(w: Write<Fa>) {
    let mut test = [0; 4];
    let mut i = 0;

    for mut getter in w.take().iter_mut() {
        for ca in getter.getter.iter_mut() {
            ca.0 -= 1;

            test[i] = ca.0;
            i += 1;
        }
    }

    test.sort_unstable();
    assert_eq!(test, [1, 2, 3, 4]);
    crate::log!("@@@ dec_ca OK");
}

fn iter_ca(r: Read<Fa>) {
    let mut test = [0; 4];
    let mut i = 0;

    for getter in r.take().iter() {
        for ca in getter.getter.iter() {
            test[i] = ca.0;
            i += 1;
        }
    }

    test.sort_unstable();
    assert_eq!(test, [1, 2, 3, 4]);
    crate::log!("@@@ iter_ca OK");
}

fn foo(r: Read<(Fa, Fb)>) {
    // let (a, b) = r.take();
    // for item in a.iter().flatten() {
    //     crate::log!("@@@ foo a item: {item:?}");
    // }
    // for item in b.iter().flatten() {
    //     crate::log!("@@@ foo b item: {item:?}");
    // }
}

fn bar(w: Write<(Fa, Fb)>) {
    // let (mut a, mut b) = w.take();
    // for item in a.iter_mut().flatten() {
    //     crate::log!("@@@ bar a item: {item:?}");
    // }
    // for item in b.iter_mut().flatten() {
    //     crate::log!("@@@ bar b item: {item:?}");
    // }
}

fn listen_scale(mut queue: ResWrite<resources::ScaleEventQueue>) {
    for ev in queue.drain(..) {
        crate::log!("@@@ scale: {:?}", ev);
    }
}

fn listen_resize(mut queue: ResWrite<resources::ResizeEventQueue>) {
    for ev in queue.drain(..) {
        crate::log!("@@@ resize: {:?}", ev);
    }
}

fn listen_click(mut queue: ResWrite<resources::ClickEventQueue>) {
    for ev in queue.drain(..) {
        crate::log!("@@@ click: {:?}", ev);
    }
}

fn th_foo() {
    th_test("foo");
}

fn th_bar() {
    th_test("bar");
}

fn th_baz() {
    th_test("baz");
}

fn th_test(name: &str) {
    let th = std::thread::current().id();
    crate::log!("Rust user lib waiting... in {name} on {th:?}");
    std::thread::sleep(Duration::from_millis(1000));
    crate::log!("Rust user lib escaped");
}
