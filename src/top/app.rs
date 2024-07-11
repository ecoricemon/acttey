use super::{r#loop::Loop, AppError};
use crate::{
    default::resources,
    default::systems::{SceneHandler, WakeCommandHandlerSystem},
    draw::scene::Scene,
    msg::{
        manager::{MessageManager, MessageQueue},
        Command, Event, EventType, Message,
    },
    render::canvas::{CanvasHandle, OffCanvas, WinCanvas, WinCanvasPack},
    ty,
    util::{string, web},
    worker::{msg, MainWorker, SubWorker},
};
use my_ecs::ecs::prelude::*;
use std::{cell::RefCell, mem, ptr::NonNull, rc::Rc};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct App {
    main: Rc<MainWorker>,
    canvases: Rc<RefCell<WinCanvasPack>>,
}

impl App {
    pub fn new() -> Self {
        set_panic_hook();

        // Canvase pack.
        let canvases = WinCanvasPack::new();

        // Spawns main worker.
        let main = MainWorker::spawn("main-worker".to_owned()).expect("failed to spawn web worker");
        let main = Rc::new(main);

        // Sends INIT message with dummy canvas to the main worker.
        let canvas = canvases.get_dummy().as_ref();
        let canvas: &WinCanvas = canvas.try_into().unwrap();
        let selectors = WinCanvasPack::DUMMY_SEL.to_owned();
        let offcanvas = canvas.transfer_control_to_offscreen();
        let (element, handle) = offcanvas.destructure();
        msg::MsgEventInit(msg::MsgEventCanvas {
            element,
            handle,
            selectors,
            scale: 0.0,
        })
        .post_to(&main.handle());

        // Detects scale change for the WINDOW_SCALE message.
        detect_scale_change();

        Self {
            main,
            canvases: Rc::new(RefCell::new(canvases)),
        }
    }

    pub fn register_canvas(
        &mut self,
        selectors: &str,
        events: &[EventType],
    ) -> Result<(), AppError> {
        if !selectors.is_empty() {
            // Adds canvas.
            let mut canvases = self.canvases.borrow_mut();
            canvases.insert(selectors.to_owned());
            // Safety: Infallible.
            let canvas = unsafe { canvases.get_by_selectors(selectors).unwrap_unchecked() };
            let canvas: &WinCanvas = canvas.as_ref().try_into().unwrap();
            let scale = canvas.width() as f64 / canvas.client_width() as f64;
            let offcanvas = canvas.transfer_control_to_offscreen();

            // Transfers offscreen canvas to the main worker.
            let (element, handle) = offcanvas.destructure();
            let selectors = selectors.to_owned();
            msg::MsgEventCanvas {
                element,
                handle,
                selectors,
                scale,
            }
            .post_to(&self.main.handle());
        }

        // Registers events.
        for event in events.iter().cloned() {
            self.register_event_proxy(selectors, event);
        }

        Ok(())
    }

    pub fn initialize(&self, f: fn(&mut AppState)) {
        // Sends user initialization function.
        let f: fn() = unsafe { mem::transmute(f) };
        msg::MsgEventFn {
            f: f as usize as _,
            disc: msg::MsgEventFnDisc::UserInit as _,
        }
        .post_to(&self.main.handle());

        // Sends run message.
        msg::MsgEventRun.post_to(&self.main.handle());
    }

    fn register_event_proxy(&mut self, selectors: &str, event: EventType) {
        fn create_mouse_move_event_proxy(
            main: Rc<MainWorker>,
            handle: CanvasHandle,
        ) -> Closure<dyn Fn(web_sys::MouseEvent)> {
            let buf = msg::MsgEventMouseMove::create_buffer();
            Closure::<dyn Fn(web_sys::MouseEvent)>::new(move |event: web_sys::MouseEvent| {
                let msg = msg::MsgEventMouseMove(msg::MsgEventMouse {
                    handle,
                    button: event.button() as _,
                    client_x: event.client_x() as _,
                    client_y: event.client_y() as _,
                    movement_x: event.movement_x() as _,
                    movement_y: event.movement_y() as _,
                    offset_x: event.offset_x() as _,
                    offset_y: event.offset_y() as _,
                });
                msg.write_body(&buf);
                main.post_message(&buf).unwrap();
            })
        }

        fn create_click_event_proxy(
            main: Rc<MainWorker>,
            handle: CanvasHandle,
        ) -> Closure<dyn Fn(web_sys::MouseEvent)> {
            let buf = msg::MsgEventClick::create_buffer();
            Closure::<dyn Fn(web_sys::MouseEvent)>::new(move |event: web_sys::MouseEvent| {
                let msg = msg::MsgEventClick(msg::MsgEventMouse {
                    handle,
                    button: event.button() as _,
                    client_x: event.client_x() as _,
                    client_y: event.client_y() as _,
                    movement_x: event.movement_x() as _,
                    movement_y: event.movement_y() as _,
                    offset_x: event.offset_x() as _,
                    offset_y: event.offset_y() as _,
                });
                msg.write_body(&buf);
                main.post_message(&buf).unwrap();
            })
        }

        fn create_scale_event_proxy(main: Rc<MainWorker>) -> Closure<dyn Fn(web_sys::CustomEvent)> {
            let buf = msg::MsgEventWindowScale::create_buffer();
            Closure::<dyn Fn(web_sys::CustomEvent)>::new(move |event: web_sys::CustomEvent| {
                let msg = msg::MsgEventWindowScale {
                    scale: event.detail().as_f64().unwrap(),
                };
                msg.write_body(&buf);
                main.post_message(&buf).unwrap();
            })
        }

        fn create_resize_event_proxy(
            main: Rc<MainWorker>,
            canvases: Rc<RefCell<WinCanvasPack>>,
        ) -> Closure<dyn Fn()> {
            let buf = msg::MsgEventCanvasResize::create_buffer();
            Closure::<dyn Fn()>::new(move || {
                let canvases = canvases.borrow();
                for canvas in canvases
                    .canvases()
                    .filter(|canvas| !canvas.is_dummy() && !canvas.is_window())
                {
                    let msg = msg::MsgEventCanvasResize {
                        handle: canvas.handle(),
                        width: canvas.client_width() as _,
                        height: canvas.client_height() as _,
                    };
                    msg.write_body(&buf);
                    main.post_message(&buf).unwrap();
                }
            })
        }

        fn add_event_listener(
            handle: CanvasHandle,
            canvas: &Rc<WinCanvas>,
            event: &str,
            listener: &js_sys::Function,
        ) {
            if handle > 0 {
                canvas
                    .add_event_listener_with_callback(event, listener)
                    .unwrap();
            } else {
                web::window()
                    .add_event_listener_with_callback(event, listener)
                    .unwrap();
            }
        }

        let main = Rc::clone(&self.main);
        let mut canvas_pack = self.canvases.borrow_mut();
        let (canvas, handle) = if !selectors.is_empty() {
            let canvas = canvas_pack.get_by_selectors(selectors).unwrap();
            let handle = canvas.handle();
            (canvas, handle)
        } else {
            let canvas = canvas_pack.get_dummy();
            let handle = CanvasHandle::window_handle();
            (canvas, handle)
        };

        match event {
            EventType::Scale if handle.is_window_handle() => {
                let proxy = create_scale_event_proxy(main);
                add_event_listener(handle, canvas, event.into(), proxy.as_ref().unchecked_ref());
                canvas_pack.register_proxy(handle, event, Box::new(proxy));
            }
            EventType::MouseMove => {
                let proxy = create_mouse_move_event_proxy(main, handle);
                add_event_listener(handle, canvas, event.into(), proxy.as_ref().unchecked_ref());
                canvas_pack.register_proxy(handle, event, Box::new(proxy));
            }
            EventType::Click => {
                let proxy = create_click_event_proxy(main, handle);
                add_event_listener(handle, canvas, event.into(), proxy.as_ref().unchecked_ref());
                canvas_pack.register_proxy(handle, event, Box::new(proxy));
            }
            EventType::Resize if handle.is_window_handle() => {
                let canvases = Rc::clone(&self.canvases);
                let proxy = create_resize_event_proxy(main, canvases);
                add_event_listener(handle, canvas, event.into(), proxy.as_ref().unchecked_ref());
                canvas_pack.register_proxy(handle, event, Box::new(proxy));
            }
            _ => {
                panic!(
                    "target found by {} isn't appropriate for the {:?} event",
                    selectors, event
                );
            }
        };
    }
}

impl Default for App {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for App {
    fn drop(&mut self) {
        crate::log!("[W] App is dropped!");
    }
}

/// Binds JS.
#[wasm_bindgen(module = "/src/util/detector.js")]
extern "C" {
    #[wasm_bindgen(js_name = "detectScaleChange")]
    fn detect_scale_change();
}

thread_local! {
    /// [`Worker`] owns this [`AppState`], which means the state belongs to worker.
    /// All graphics jobs will be done in the worker.
    //
    // Can't be const because it needs lazy initialization.
    #[allow(clippy::thread_local_initializer_can_be_made_const)]
    static APP_STATE: RefCell<AppState> = panic!();

    /// Checks out all workers are ready or not.
    /// Plus, gets thread handle [`Thread`]s from the workers.
    static FN_WAIT_WORKER: RefCell<Closure<dyn FnMut()>> = panic!();
}

#[wasm_bindgen(js_name = mainOnMessage)]
pub async fn main_onmessage(ev: web_sys::MessageEvent) {
    let data = ev.data();
    debug_assert!(data.is_array());
    let buf: js_sys::Array = data.unchecked_into();

    match msg::MsgEventHeader::from_js_value(buf.get(0)).0 {
        msg::MsgEventHeader::INIT_INNER => {
            crate::log!("[D] main_onmessage_init(): worker received INIT msg.");

            // Initializes `APP_STATE`.
            let msg = msg::MsgEventInit::read_body(&buf);
            let ref_canvas = OffCanvas::new(msg.0.element, msg.0.handle);
            let state = AppState::new(&ref_canvas).await;
            APP_STATE.set(state);

            // APP_STATE is settled now, so we can know fixed resource address.
            APP_STATE.with_borrow_mut(|state| {
                state.set_default_resource_address();
            });

            // Fully initialized. JS will change `onmessage` right away.
        }
        msg::MsgEventHeader::CANVAS_INNER => {
            crate::log!("[D] main_onmessage(): worker received CANVAS msg.");

            let msg = msg::MsgEventCanvas::read_body(&buf);
            APP_STATE.with_borrow_mut(move |state| {
                state.register_offcanvas(msg.element, msg.handle, msg.selectors, msg.scale);
            });
        }
        msg::MsgEventHeader::WINDOW_SCALE_INNER => {
            crate::log!("[D] main_onmessage(): worker received WINDOW_SCALE msg.");

            let msg = msg::MsgEventWindowScale::read_body(&buf);
            APP_STATE.with_borrow_mut(|state| {
                state.update_scale_factor(msg.scale);
            });
        }
        msg::MsgEventHeader::CANVAS_RESIZE_INNER => {
            crate::log!("[D] main_onmessage(): worker received CANVAS_RESIZE msg.");

            let msg = msg::MsgEventCanvasResize::read_body(&buf);
            APP_STATE.with_borrow_mut(|state| {
                // Safety: The queue will be referenced in scheduling,
                // but the scheduling will be performed after this.
                unsafe {
                    state.msg_mgr.push_resize(msg);
                }
            });
        }
        msg::MsgEventHeader::MOUSE_MOVE_INNER => {
            crate::log!("[D] main_onmessage(): worker received MOUSE_MOVE msg.");

            let msg = msg::MsgEventMouseMove::read_body(&buf);
            APP_STATE.with_borrow_mut(|state| {
                // Safety: The queue will be referenced in scheduling,
                // but the scheduling will be performed after this.
                unsafe { state.msg_mgr.push_mouse_move(msg) };
            });
        }
        msg::MsgEventHeader::CLICK_INNER => {
            crate::log!("[D] main_onmessage(): worker received CLICK msg.");

            let msg = msg::MsgEventClick::read_body(&buf);
            APP_STATE.with_borrow_mut(|state| {
                // Safety: The queue will be referenced in scheduling,
                // but the scheduling will be performed after this.
                unsafe { state.msg_mgr.push_click(msg) };
            });
        }
        msg::MsgEventHeader::FN_INNER => {
            crate::log!("[D] main_onmessage(): worker received FN msg.");

            let msg = msg::MsgEventFn::read_body(&buf);
            if msg.disc > msg::MsgEventFnDisc::Begin as u32 {
                // Acttey function
                match msg.disc {
                    disc if disc == msg::MsgEventFnDisc::UserInit as u32 => {
                        APP_STATE.with_borrow_mut(|state: &mut AppState| {
                            // TODO: This corresponds to App::call_initializer().
                            // But it's too error prone. Come up with better solution.
                            let f: fn(&mut AppState) = unsafe { mem::transmute(msg.f as usize) };

                            // Initializes `AppState` with user's function.
                            state.pre_initialize();
                            f(state);
                            state.post_initialize();
                        });
                    }
                    _ => {
                        panic!("unsupported msg: {:?}", msg);
                    }
                }
            } else {
                // User function
                todo!()
            }
        }
        msg::MsgEventHeader::RUN_INNER => {
            crate::log!("[D] main_onmessage(): worker received Run msg.");

            // We need to wait all workers to become ready.
            let check: Closure<dyn FnMut()> = Closure::new(|| {
                let mut failed = false;
                APP_STATE.with_borrow_mut(|state: &mut AppState| {
                    for worker in state.workers.iter_mut() {
                        if !worker.is_ready() {
                            failed = true;
                            break;
                        }
                    }

                    // If any worker is not ready yet, see it next time.
                    if failed {
                        FN_WAIT_WORKER.with_borrow(|check: &Closure<dyn FnMut()>| {
                            let cb = check.as_ref().unchecked_ref();
                            let global = js_sys::global()
                                .unchecked_into::<web_sys::DedicatedWorkerGlobalScope>();
                            global
                                .set_timeout_with_callback_and_timeout_and_arguments_0(cb, 10)
                                .unwrap();
                            crate::log!("[D] main_onmessage(): wait for workers once again");
                        });
                    } else {
                        crate::log!(
                            "[D] main_onmessage(): all {} sub workers are stable now",
                            state.workers.len()
                        );
                    }
                });

                // All workers are ready, we can finally run our main loop.
                if !failed {
                    crate::log!("[D] main_onmessage(): run AppState's main loop");
                    AppState::run();
                }
            });

            // We must finish our current task in order to let sub workers be spawned and get ready.
            // Because JS can't do the job until current job is done.
            // To do that, we use timer here.
            let cb = check.as_ref().unchecked_ref();
            let global = js_sys::global().unchecked_into::<web_sys::DedicatedWorkerGlobalScope>();
            global
                .set_timeout_with_callback_and_timeout_and_arguments_0(cb, 10)
                .unwrap();
            FN_WAIT_WORKER.set(check);
        }
        _ => {
            crate::log!(
                "[W] main_onmessage(): worker received unknown msg: {:?}",
                buf
            );
        }
    }
}

/// Resources representing the app's state.
pub struct AppState {
    /// ECS data and controller.
    ecs_mgr: resources::EcsManager,

    /// A resource managing all render resources.
    render: resources::RenderManager,

    /// Glanular data such as mesh.
    stor: resources::CommonStorage,

    /// A resource managing all scenes.
    scene_mgr: resources::SceneManager,

    /// A resource holding event and command queues.
    //
    // One of not exposed resources.
    msg_mgr: MessageManager,

    /// Loop function.
    main_loop: Loop,

    /// Sub workers.
    workers: Vec<SubWorker>,
}

impl AppState {
    pub(super) async fn new(ref_canvas: &OffCanvas) -> Self {
        Self {
            ecs_mgr: resources::EcsManager::new(),
            render: resources::RenderManager::new(ref_canvas).await.unwrap(),
            stor: resources::CommonStorage::new(),
            scene_mgr: resources::SceneManager::new(),
            msg_mgr: MessageManager::new(),
            main_loop: Loop::new(),
            workers: Vec::new(),
        }
    }

    pub fn spawn_worker(&mut self, num: usize) {
        assert!(self.workers.is_empty());

        // Spawns sub workers.
        let num = if num != 0 {
            num
        } else {
            web::hardware_concurrency() - 1
        };
        let mut name = "sub-worker-00".to_owned();

        // Indices start from 0.
        self.workers = (0..=num)
            .map(|_| {
                // Safety: Infallible.
                string::increase_rnumber(&mut name);
                SubWorker::spawn(name.clone()).unwrap()
            })
            .collect::<Vec<_>>();
    }

    pub fn append_setup_system<T, Sys, Req, F>(&mut self, sys: T)
    where
        T: Into<StructOrFnSystem<Sys, Req, F>>,
        Sys: System,
        FnSystem<Req, F>: System,
    {
        // Setup system will be removed from memory after it's expired.
        const VOLATILE: bool = true;
        const LIVE: NonZeroTick = unsafe { NonZeroTick::new_unchecked(1) };
        let must_ok = self.ecs_mgr.append_system(sys, LIVE, VOLATILE);
        assert!(must_ok.is_ok());
    }

    pub fn register_event(&mut self, event: EventType) {
        match event {
            EventType::Scale => { /* No queue */ }
            EventType::MouseMove => self.register_message::<Event<msg::MsgEventMouseMove>>(),
            EventType::Click => self.register_message::<Event<msg::MsgEventClick>>(),
            EventType::Resize => self.register_message::<Event<msg::MsgEventCanvasResize>>(),
        }
    }

    pub fn register_command<C: Message>(&mut self) {
        assert!(C::is_command());

        self.register_message::<C>();
    }

    fn register_message<E: Message>(&mut self) {
        // Creates event queue, `MessageQueue<E>`.
        if E::is_event() {
            self.msg_mgr.register_event::<E>();
        } else {
            self.msg_mgr.register_command::<E>();
        }

        // Registers event queue, `MessageQueue<E>`, as a resource.
        let rkey = ResourceKey::new(crate::ty!(MessageQueue<E>));
        let ptr = self.msg_mgr.queue_ptr::<E>().unwrap().cast();
        self.ecs_mgr.register_resource(rkey, ptr);
    }

    /// Does some jobs before user initialization.
    pub(super) fn pre_initialize(&mut self) {
        // NOTE: For now, any resource required by a system must be regitered before the system.

        // Registers built-in command and its command queue resource.
        self.register_command::<Command<Scene>>();
    }

    /// Does some jobs after user initialization.
    pub(super) fn post_initialize(&mut self) {
        // Adds built-in main system in phase B.
        let must_ok = self.ecs_mgr.set_main_system_b(WakeCommandHandlerSystem);
        debug_assert!(must_ok.is_ok());

        // Registers built-in command handler.
        let must_ok = self
            .ecs_mgr
            .register_command_handler(ty!(Command<Scene>), SceneHandler);
        debug_assert!(must_ok.is_ok());
    }

    /// Acquires addresses of resource fields from this app state.
    /// And then sets those addresses to the scheduler.
    /// Note that this method must be called once when app state is fixed in memory.
    //
    // We're not going to use `Box` or something like that here.
    // Because we are aware of when app state is fixed in memory and its resource fields will never be moved or dropped.
    pub(super) fn set_default_resource_address(&mut self) {
        use std::ptr::addr_of_mut;

        // Safety: Infallible.
        unsafe {
            // Registers `RenderManager` resource.
            let ptr = NonNull::new_unchecked(addr_of_mut!(self.render) as _);
            self.ecs_mgr.register_resource(self.render.rkey(), ptr);

            // Registers `SceneManager` resource.
            let ptr = NonNull::new_unchecked(addr_of_mut!(self.scene_mgr) as _);
            self.ecs_mgr.register_resource(self.scene_mgr.rkey(), ptr);

            // Registers `MessageManager` resource.
            let ptr = NonNull::new_unchecked(addr_of_mut!(self.msg_mgr) as _);
            self.ecs_mgr.register_resource(self.msg_mgr.rkey(), ptr);

            // Registers `EcsManager` resource.
            let ptr = NonNull::new_unchecked(addr_of_mut!(self.ecs_mgr) as _);
            self.ecs_mgr.register_resource(self.ecs_mgr.rkey(), ptr);

            // Registers `CommonStorage` resource.
            let ptr = NonNull::new_unchecked(addr_of_mut!(self.stor) as _);
            self.ecs_mgr.register_resource(self.stor.rkey(), ptr);
        }

        // Registers dedicated resource.
        // EcsManager
        self.ecs_mgr
            .register_dedicated_resource(self.ecs_mgr.rkey());
        // RenderManager
        self.ecs_mgr.register_dedicated_resource(self.render.rkey());
        // SceneManager
        self.ecs_mgr
            .register_dedicated_resource(self.scene_mgr.rkey());
    }

    pub(super) fn register_offcanvas(
        &mut self,
        element: web_sys::OffscreenCanvas,
        handle: CanvasHandle,
        selectors: String,
        scale: f64,
    ) {
        self.render
            .register_canvas(selectors, OffCanvas::new(element, handle));
        self.render.scale = scale;
    }

    pub(super) fn update_scale_factor(&mut self, scale: f64) {
        self.render.scale = scale;
    }

    pub(super) fn run() {
        // Creates main loop which is animation callback.
        let callback = Closure::<dyn FnMut(f64)>::new(move |_time: f64| {
            APP_STATE.with_borrow_mut(|state| {
                let Self {
                    ecs_mgr,
                    main_loop,
                    workers,
                    ..
                } = state;

                // TODO: super system is not scheduled for now.
                ecs_mgr.schedule(workers);

                // Requests next frame.
                main_loop.request_animation_frame();
            });
        });

        // First fire.
        APP_STATE.with_borrow_mut(|state| {
            state.main_loop.set_callback(callback);
            state.main_loop.request_animation_frame();
        })
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
