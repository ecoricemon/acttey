use super::{
    ecs::{
        entity::Entity,
        skey,
        storage::Storage,
        system::{Invokable, System},
    },
    render::{input::InputState, render_state::RenderState},
};
use std::{cell::RefCell, rc::Rc};
use wasm_bindgen::prelude::*;
use winit::event::*;

pub struct App {
    input_state: Rc<RefCell<InputState>>,
    render_state: Rc<RefCell<RenderState>>,
    storage: Rc<RefCell<Storage>>,
    systems: Rc<RefCell<Vec<Box<dyn Invokable>>>>,
}

impl App {
    pub async fn new() -> Self {
        set_panic_hook();

        // Input state.
        let input_state = Rc::new(RefCell::new(InputState::new()));

        // Render state.
        let (render_state, event_loop) = RenderState::new().await;
        RenderState::set_event_handlers(
            event_loop,
            render_state.winit_window.id(),
            input_state.clone(),
        );
        let render_state = Rc::new(RefCell::new(render_state));

        // Storage.
        let storage = Rc::new(RefCell::new(Storage::new()));

        // Systems.
        let systems = Rc::new(RefCell::new(Vec::new()));

        Self {
            input_state,
            storage,
            render_state,
            systems,
        }
    }

    pub fn regist_entity<E: Entity>(&mut self) -> &mut Self {
        self.storage.borrow_mut().insert_default::<E>();
        self
    }

    #[inline]
    pub fn insert_entity(&mut self, key: usize, ent: impl Entity) -> &mut Self {
        self.storage.borrow_mut().insert_entity(key, ent);
        self
    }

    pub fn regist_system<S: System>(&mut self, sys: S) -> &mut Self {
        self.systems.borrow_mut().push(Box::new(sys));
        self.storage.borrow_mut().insert_sinfo(skey!(S), S::info());
        self
    }

    pub fn animate(&mut self) {
        self.storage.borrow_mut().invalidate_sinfo();

        let animate = Rc::new(RefCell::new(None));
        let animate_drop = animate.clone();
        let input_state = self.input_state.clone();
        let render_state = self.render_state.clone();
        let storage = self.storage.clone();
        let systems = self.systems.clone();
        *animate_drop.borrow_mut() = Some(Closure::<dyn FnMut(f32)>::new(move |time: f32| {
            let mut input_state = input_state.borrow_mut();
            let mut render_state = render_state.borrow_mut();

            // Input
            input_state.consume().for_each(|event| match event {
                WindowEvent::Resized(_) => {
                    // Don't use physical_size in Resized for now because it doesn't work as expected in Chrome device mode.
                    let (new_width, new_height) = render_state.get_scaled_size();
                    render_state.resize(new_width, new_height);
                }
                WindowEvent::CursorMoved {
                    device_id: _,
                    position,
                } => {
                    render_state.mousemove(position.x as f32, position.y as f32);
                }
                WindowEvent::MouseInput {
                    device_id: _,
                    state,
                    button,
                } if state == ElementState::Pressed && button == MouseButton::Left => {
                    render_state.click();
                }
                WindowEvent::Touch(Touch {
                    device_id: _,
                    phase: TouchPhase::Started,
                    location,
                    force: _,
                    id: _,
                }) => {
                    render_state.mousemove(location.x as f32, location.y as f32);
                }
                _ => (),
            });

            // Update
            let mut storage = storage.borrow_mut();
            for sys in systems.borrow().iter() {
                sys.invoke(&mut storage);
            }

            // Render
            render_state.render(time);
            // Request next frame
            render_state.request_animation_frame(animate.borrow().as_ref().unwrap());
        }));

        self.render_state
            .borrow()
            .request_animation_frame(animate_drop.borrow().as_ref().unwrap());
    }

    pub fn set_camera(
        &mut self,
        camera_x: f32,
        camera_y: f32,
        camera_z: f32,
        at_x: f32,
        at_y: f32,
        at_z: f32,
    ) {
        self.render_state.borrow_mut().set_camera(
            Some((camera_x, camera_y, camera_z)),
            Some((at_x, at_y, at_z)),
            None,
        );
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
