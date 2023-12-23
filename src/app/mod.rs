pub(crate) mod input;

use crate::{
    app::input::{Event, EventListener, Input},
    ds::refs::RCell,
    ecs::{
        predefined::resource::{ResourcePack, TimeStamp},
        storage::Storage,
        system::{Invokable, System, Systems},
        traits::Entity,
    },
    primitive::{
        matrix::Matrix4f,
        mesh::{InterleavedVertexInfo, Mesh},
    },
    render::RenderResource,
};
use std::rc::Rc;
use my_wgsl::wgsl_decl_struct;
use wasm_bindgen::prelude::*;
use web_sys::Performance;

pub mod prelude {
    pub use super::{input::Input, App};
}

#[repr(C)]
#[derive(Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
struct UniformData {
    view_proj: Matrix4f,
}

struct AppState {
    storage: Storage,
    systems: Systems,
    super_systems: Vec<Box<dyn Invokable>>,
    render: RenderResource,
}

impl AppState {
    fn new(
        storage: Storage,
        systems: Systems,
        render: RenderResource,
        super_systems: Vec<Box<dyn Invokable>>,
    ) -> Self {
        Self {
            storage,
            systems,
            super_systems,
            render,
        }
    }

    fn each(
        &mut self,
    ) -> (
        &mut Storage,
        &mut Systems,
        &mut Vec<Box<dyn Invokable>>,
        &mut RenderResource,
    ) {
        (
            &mut self.storage,
            &mut self.systems,
            &mut self.super_systems,
            &mut self.render,
        )
    }
}

pub struct App {
    input: RCell<Input>,
    meshes: Vec<(Mesh, InterleavedVertexInfo)>,
    state: RCell<AppState>,
    perf: Rc<Performance>,
}

impl App {
    pub async fn new() -> Self {
        set_panic_hook();

        let input = Input::new();
        let meshes = vec![];

        let storage = Storage::new();
        let systems = Systems::new();
        let render = RenderResource::new(None, None).await.unwrap();
        let super_systems = vec![];
        let state = AppState::new(storage, systems, render, super_systems);

        let perf = crate::util::get_window()
            .performance()
            .expect_throw(crate::errmsg::WEBSYS_GET_PERFORMANCE);

        Self {
            input: RCell::new(input).with_tag("input"),
            meshes,
            state: RCell::new(state),
            perf: Rc::new(perf),
        }
    }

    /// Inserts a canvas having the given `id`.
    /// Returns canvas handle, which starts from 1 and always increases whenever you insert.
    pub fn register_canvas(&mut self, id: &str) -> u32 {
        self.state.borrow_mut().render.register_canvas(id)
    }

    pub fn unregister_canvas(&mut self, id: &str) {
        self.state.borrow_mut().render.unregister_canvas(id)
    }

    pub fn register_entity<E: Entity>(&mut self) -> &mut Self {
        self.state.borrow_mut().storage.insert_default::<E>();
        self
    }

    pub fn insert_entity(&mut self, key: usize, ent: impl Entity) -> &mut Self {
        self.state.borrow_mut().storage.insert_entity(key, ent);
        self
    }

    pub fn register_system<S: System>(&mut self, sys: S) -> &mut Self {
        let skey = S::key();
        let sinfo = S::info();
        self.state.borrow_mut().systems.insert(skey, Box::new(sys));
        self.state.borrow_mut().storage.insert_sinfo(skey, sinfo);
        self
    }

    pub fn register_super_system<S: System>(&mut self, sys: S) -> &mut Self {
        let skey = S::key();
        let sinfo = S::info();
        self.state.borrow_mut().super_systems.push(Box::new(sys));
        self.state.borrow_mut().storage.insert_sinfo(skey, sinfo);
        self
    }

    pub fn insert_mesh(&mut self, mesh: Mesh) -> usize {
        // Test Uniform data.
        let uniform = UniformData {
            view_proj: Default::default(),
        };

        let wgsl;
        {
            use my_wgsl::*;
            let mut builder = Builder::new();

            #[wgsl_decl_struct]
            struct UniformData {
                view_proj: mat4x4<f32>,
            }

            #[wgsl_decl_struct]
            struct VertexInput {
                #[location(0)] point: vec3<f32>,
                #[location(1)] normal: vec3<f32>,
                #[location(2)] color: vec4<f32>,
            }

            #[wgsl_decl_struct]
            struct VertexOutput {
                #[builtin(position)] point: vec4<f32>,
                #[location(1)] color: vec4<f32>
            }

            wgsl_structs!(builder, UniformData, VertexInput, VertexOutput);

            wgsl_bind!(builder, group(0) binding(0) var<uniform> uni : UniformData);

            wgsl_fn!(builder,
                #[vertex]
                fn v_main(input: VertexInput) -> VertexOutput {
                    var output: VertexOutput;
                    output.point = uni.view_proj * vec4f(input.point, 1.0);
                    output.color = input.color;
                    return output;
                }
            );

            wgsl_fn!(builder,
                #[fragment]
                fn f_main(input: VertexOutput) -> #[location(0)] vec4<f32> {
                    return input.color;
                }
            );

            wgsl = builder.build();
        }
        
        // Render resource
        self.state
            .borrow_mut()
            .render
            .example(uniform, &wgsl, "v_main", "f_main", mesh.clone());

        let vertex_info = mesh.create_interleaved_vertex_info();
        self.meshes.push((mesh, vertex_info));
        self.meshes.len()
    }

    pub fn run(&mut self) {
        // Configures the storage with the given system information.
        self.state.borrow_mut().storage.invalidate_sinfo();

        // Clones to move into the animation loop;
        let input = RCell::clone(&self.input);
        let state = RCell::clone(&self.state);

        // Loop.
        let mut time_prev = 0.0;
        let animate_callback = Closure::<dyn FnMut(f64)>::new(move |time: f64| {
            // Animation resources.
            let mut input = input.borrow_mut();
            let mut state = state.borrow_mut();
            let (storage, systems, super_systems, render) = state.each();
            let mut res_pack = ResourcePack {
                input: &mut input,
                storage,
                render,
                time: &mut TimeStamp(time),
                systems: None,
            };

            // Runs all systems.
            for sys in systems.values_mut() {
                sys.invoke(&res_pack);
            }

            // Runs all super systems.
            res_pack.systems = Some(systems);
            for sys in super_systems.iter_mut() {
                sys.invoke(&res_pack);
            }
            // All commands should be processed.
            debug_assert!(res_pack.input.is_command_empty());

            // Requests next frame.
            render.request_animation_frame();
            time_prev = time;
        });

        let render = &mut self.state.borrow_mut().render;
        render.register_animation_callback(animate_callback);
        render.request_animation_frame();
    }

    pub fn register_events<'a>(
        &mut self,
        id: &str,
        types: impl Iterator<Item = &'a str>,
    ) -> &mut Self {
        for type_ in types {
            self.register_event(id, type_);
        }
        self
    }

    /// You can put empty `id` for window itself (e.g. resize event).
    /// Otherwise put in proper HTML element `id`.
    fn register_event(&mut self, id: &str, type_: &str) {
        let input = RCell::clone(&self.input);
        let handle = self
            .state
            .borrow()
            .render
            .get_canvas_handle(id)
            .unwrap_or(0);

        macro_rules! helper {
            ("resize") => {{
                self.add_noarg_event_listener(id, type_, move || {
                    let mut input = input.borrow_mut();
                    input.push(Event::Resized(handle));
                });
            }};
            ("mouse") => {{
                self.add_mouse_event_listener(id, type_, move |event: web_sys::MouseEvent| {
                    let mut input = input.borrow_mut();
                    input.push((handle, event).into());
                });
            }};
            ("keyboard") => {{
                self.add_keyboard_event_listener(
                    id,
                    type_,
                    move |event: web_sys::KeyboardEvent| {
                        let mut input = input.borrow_mut();
                        input.push((handle, event).into());
                    },
                );
            }};
            ($($t:tt)*) => {
                Unreachable
            };
        }

        match type_ {
            "resize" => helper!("resize"),
            "click" => helper!("mouse"),
            "mousemove" => helper!("mouse"),
            "keydown" => helper!("keyboard"),
            "keyup" => helper!("keyboard"),
            _ => panic!("Unsupported event type {}", type_),
        }
    }

    pub fn unregister_events<'a>(
        &mut self,
        id: &str,
        types: impl Iterator<Item = &'a str>,
    ) -> &mut Self {
        for type_ in types {
            self.unregister_event(id, type_);
        }
        self
    }

    fn unregister_event(&mut self, id: &str, type_: &str) {
        let mut input = self.input.borrow_mut();
        if let Some(old) = input.remove_event_listener(id, type_) {
            let element = crate::util::get_element_by_id(id).unwrap();
            let _ = match old {
                EventListener::NoArg(listener) => element
                    .remove_event_listener_with_callback(type_, listener.as_ref().unchecked_ref()),
                EventListener::Mouse(listener) => element
                    .remove_event_listener_with_callback(type_, listener.as_ref().unchecked_ref()),
                EventListener::Keyboard(listener) => element
                    .remove_event_listener_with_callback(type_, listener.as_ref().unchecked_ref()),
            };
        }
    }

    fn add_noarg_event_listener(&mut self, id: &str, type_: &str, f: impl Fn() + 'static) {
        let listener = Closure::<dyn Fn()>::new(f);
        self.add_event_listner(id, type_, listener.as_ref().unchecked_ref());

        self.input
            .borrow_mut()
            .add_event_listener(id, type_, listener.into());
    }

    fn add_mouse_event_listener(
        &mut self,
        id: &str,
        type_: &str,
        f: impl Fn(web_sys::MouseEvent) + 'static,
    ) {
        let listener = Closure::<dyn Fn(web_sys::MouseEvent)>::new(f);
        self.add_event_listner(id, type_, listener.as_ref().unchecked_ref());

        self.input
            .borrow_mut()
            .add_event_listener(id, type_, listener.into());
    }

    fn add_keyboard_event_listener(
        &mut self,
        id: &str,
        type_: &str,
        f: impl Fn(web_sys::KeyboardEvent) + 'static,
    ) {
        let listener = Closure::<dyn Fn(web_sys::KeyboardEvent)>::new(f);
        self.add_event_listner(id, type_, listener.as_ref().unchecked_ref());

        self.input
            .borrow_mut()
            .add_event_listener(id, type_, listener.into());
    }

    fn add_event_listner(&mut self, id: &str, type_: &str, listener: &js_sys::Function) {
        if id.is_empty() {
            let window = crate::util::get_window();
            window
                .add_event_listener_with_callback(type_, listener)
                .expect_throw(crate::errmsg::WEBSYS_ADD_LISTENER);
        } else {
            let element = crate::util::get_element_by_id(id).unwrap();
            element
                .add_event_listener_with_callback(type_, listener)
                .expect_throw(crate::errmsg::WEBSYS_ADD_LISTENER);
        }
    }
}

pub fn set_panic_hook() {
    #[cfg(debug_assertions)]
    {
        // Show panic messages on browsers
        std::panic::set_hook(Box::new(console_error_panic_hook::hook));
        console_error_panic_hook::set_once();
    }
}
