use super::{
    canvas::{Canvas, CanvasPack},
    event::{Event, EventManager, EventType},
    r#loop::Loop,
    AppError,
};
use crate::{
    ds::refs::RCell,
    ecs::{
        resource::Resource,
        schedule::Scheduler,
        system::{FnSystem, StructOrFnSystem, System},
    },
    primitive::mesh::MeshResource,
    render::{
        canvas::{CanvasHandle, OffCanvas},
        resource::RenderResource,
    },
    scene::inner::SceneManager,
    util::{string, web},
    worker::{
        msg::{self, ChMsg},
        Channel, MainChannel, MainWorker, Worker, WorkerId,
    },
};
use std::{cell::RefCell, mem, num::NonZeroU32, ptr::NonNull, rc::Rc, thread};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct App {
    main: Rc<MainWorker>,
    canvases: RCell<CanvasPack>,
}

impl App {
    pub fn new() -> Self {
        set_panic_hook();

        // Canvase pack.
        let canvases = CanvasPack::new();

        // Spawns main worker.
        let main = MainWorker::spawn("main-worker").expect_throw("failed to spawn web worker");
        let main = Rc::new(main);

        // Sends INIT message with dummy canvas to the main worker.
        let canvas = canvases.get_dummy();
        let selectors = CanvasPack::DUMMY_SELECTORS.to_owned();
        let offcanvas = canvas.transfer_control_to_offscreen().unwrap();
        let (element, handle) = offcanvas.destructure();
        msg::MsgEventInit(msg::MsgEventCanvas {
            element,
            handle,
            selectors,
            scale: 0.0,
        })
        .post_to(main.handle_js());

        // Detects scale change for the WINDOW_SCALE message.
        detect_scale_change();

        Self {
            main,
            canvases: RCell::new(canvases),
        }
    }

    pub fn register_canvas(
        &mut self,
        selectors: &str,
        events: &[EventType],
    ) -> Result<(), AppError> {
        if !selectors.is_empty() {
            // Adds canvas.
            let canvas = self.canvases.borrow_mut().insert(selectors)?;
            let scale = canvas.width() as f64 / canvas.client_width() as f64;
            let offcanvas = canvas.transfer_control_to_offscreen()?;

            // Transfers offscreen canvas to the main worker.
            let (element, handle) = offcanvas.destructure();
            let selectors = selectors.to_owned();
            msg::MsgEventCanvas {
                element,
                handle,
                selectors,
                scale,
            }
            .post_to(self.main.handle_js());
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
        .post_to(self.main.handle_js());

        // Sends run message.
        msg::MsgEventRun.post_to(self.main.handle_js());
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
            canvases: RCell<CanvasPack>,
        ) -> Closure<dyn Fn()> {
            let buf = msg::MsgEventCanvasResize::create_buffer();
            Closure::<dyn Fn()>::new(move || {
                let canvases = canvases.borrow();
                for canvas in canvases
                    .values()
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
            canvas: &Rc<Canvas>,
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
                let canvases = RCell::clone(&self.canvases);
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
#[wasm_bindgen(module = "/src/worker/workerGen.js")]
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

/// Initializes [`APP_STATE`] on main worker, not in window context.
/// That's because we assume that window and worker don't share the memory for future compatibility.
/// When initialization is over, JS replaces this event handler with [`main_onmessage`].
#[wasm_bindgen(js_name = mainOnMessageInit)]
pub async fn main_onmessage_init(event: web_sys::MessageEvent) {
    let data = event.data();
    debug_assert!(data.is_array());
    let buf: js_sys::Array = data.unchecked_into();
    match msg::MsgEventHeader::from_js_value(buf.get(0)).0 {
        msg::MsgEventHeader::INIT_INNER => {
            crate::log!("[D] main_onmessage_init(): worker received INIT msg.");

            // Initializes `APP_STATE`.
            let msg = msg::MsgEventInit::read_body(&buf);
            let offcanvas = OffCanvas::new(msg.0.element, msg.0.handle);
            let state = AppState::new(offcanvas, msg.0.selectors).await;
            APP_STATE.set(state);

            // APP_STATE is settled now, so we can know fixed resource address.
            APP_STATE.with_borrow_mut(|state| {
                state.set_resource_address();
            });

            // Fully initialized. JS will change `onmessage` right away.
        }
        _ => {
            crate::log!(
                "[W] main_onmessage_init(): worker received unknown msg: {:?}",
                buf
            );
        }
    }
}

#[wasm_bindgen(js_name = mainOnMessage)]
pub fn main_onmessage(event: web_sys::MessageEvent, _id: u32) {
    let data = event.data();
    debug_assert!(data.is_array());
    let buf: js_sys::Array = data.unchecked_into();

    match msg::MsgEventHeader::from_js_value(buf.get(0)).0 {
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
                state.ev_mgr.push(Event::Resized(msg));
            });
        }
        msg::MsgEventHeader::MOUSE_MOVE_INNER => {
            crate::log!("[D] main_onmessage(): worker received MOUSE_MOVE msg.");

            let msg = msg::MsgEventMouseMove::read_body(&buf);
            APP_STATE.with_borrow_mut(|state| {
                state.ev_mgr.push(Event::MouseMove(msg));
            });
        }
        msg::MsgEventHeader::CLICK_INNER => {
            crate::log!("[D] main_onmessage(): worker received CLICK msg.");

            let msg = msg::MsgEventClick::read_body(&buf);
            APP_STATE.with_borrow_mut(|state| {
                state.ev_mgr.push(Event::Click(msg));
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
                            f(state);
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
                    let main_id = thread::current().id();
                    for worker in state.workers.iter_mut() {
                        if !worker.is_ready() {
                            failed = true;
                            break;
                        }
                        if main_id == worker.handle_rust().id() {
                            worker.open();
                            worker.send(ChMsg::ReqHandle).unwrap();

                            let msg = state.ch.recv();
                            if let Ok(ChMsg::Handle(handle)) = msg {
                                worker.set_handle_rust(handle);
                            } else {
                                panic!();
                            }

                            worker.close().unwrap();
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
    /// System scheduler.
    sched: Scheduler,

    /// A resource managing all render resources.
    render: RenderResource,

    /// A resource managing all meshes.
    meshes: MeshResource,

    /// A resource managing all scenes.
    scene_mgr: SceneManager,

    /// A resource that handles all events such as input events.
    ev_mgr: EventManager,

    /// Loop function.
    main_loop: Loop,

    /// Sub workers.
    workers: Vec<Worker>,

    /// Receiving only channel.
    ch: MainChannel,
}

impl AppState {
    pub(super) async fn new(offcanvas: OffCanvas, selectors: String) -> Self {
        Self {
            sched: Scheduler::new(),
            render: RenderResource::new(offcanvas, selectors).await.unwrap(),
            meshes: MeshResource::new(),
            scene_mgr: SceneManager::new(),
            ev_mgr: EventManager::new(),
            main_loop: Loop::new(),
            workers: Vec::new(),
            ch: MainChannel::new(),
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
        // Worker's id is equal to index + 1.
        // This helps us find a specific worker easily.
        self.workers = (1..=num as u32)
            .map(|id| {
                // Safety: Infallible.
                let id = unsafe { NonZeroU32::new_unchecked(id) };
                string::increase_rnumber(&mut name);
                let tx_common = self.ch.clone_tx();
                Worker::spawn(&name, id, tx_common).expect_throw("failed to spawn web worker")
            })
            .collect::<Vec<_>>();
    }

    #[inline]
    pub fn append_setup_system<T, S, Req, F>(&mut self, sys: T)
    where
        T: Into<StructOrFnSystem<S, Req, F>>,
        S: System,
        FnSystem<Req, F>: System,
    {
        let must_some = self.sched.append_system(sys, 1);
        assert!(must_some.is_some());
    }

    /// Acquires addresses of resource fields from this app state.
    /// And then sets those addresses to the scheduler.
    /// Note that this method must be called once app state is fixed in memory.
    ///
    /// For performance reason, we're not going to use `Box` or something like that here.
    /// Because we are aware of when app state is fixed in memory and its resource fields will never be moved or dropped.
    pub(super) fn set_resource_address(&mut self) {
        use std::ptr::addr_of_mut;

        // Registers `render` resource.
        // Safety: Infallible.
        let ptr: NonNull<u8> = unsafe { NonNull::new_unchecked(addr_of_mut!(self.render) as _) };
        self.sched
            .register_default_resource(self.render.rkey(), ptr);

        // Registers `scene_mgr` resource.
        // Safety: Infallible.
        let ptr: NonNull<u8> = unsafe { NonNull::new_unchecked(addr_of_mut!(self.scene_mgr) as _) };
        self.sched
            .register_default_resource(self.scene_mgr.rkey(), ptr);

        // Registers `ev_mgr` resource.
        // Safety: Infallible.
        let ptr: NonNull<u8> = unsafe { NonNull::new_unchecked(addr_of_mut!(self.ev_mgr) as _) };
        self.sched
            .register_default_resource(self.ev_mgr.rkey(), ptr);

        // Registers `sched` resource.
        // Safety: Infallible.
        let ptr: NonNull<u8> = unsafe { NonNull::new_unchecked(addr_of_mut!(self.sched) as _) };
        self.sched.register_default_resource(self.sched.rkey(), ptr);

        // Registers `meshes` resource.
        // Safety: Infallible.
        let ptr: NonNull<u8> = unsafe { NonNull::new_unchecked(addr_of_mut!(self.meshes) as _) };
        self.sched
            .register_default_resource(self.meshes.rkey(), ptr);
    }

    pub(super) fn register_offcanvas(
        &mut self,
        element: web_sys::OffscreenCanvas,
        handle: CanvasHandle,
        selectors: String,
        scale: f64,
    ) {
        self.render
            .add_canvas(OffCanvas::new(element, handle), selectors)
            .with_default_surface();
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
                    sched,
                    main_loop,
                    workers,
                    ch,
                    ..
                } = state;

                // TODO: super system is not scheduled for now.
                sched.schedule(workers, ch);

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

/// # Safety
///
/// `ch` must be a valid pointer.
#[wasm_bindgen(js_name = subOnMessage)]
pub unsafe fn sub_onmessage(ch: *const Channel, id: u32) {
    // Entrypoint to the sub worker's meesage loop.
    fn entry(ch: &Channel, wid: WorkerId) {
        while let Ok(msg) = ch.recv() {
            match msg {
                ChMsg::ReqHandle => {
                    // crate::log!("[D] sub_onmessage(): worker({id}) ReqHandle msg.");

                    ch.send(ChMsg::Handle(thread::current())).unwrap();
                }
                ChMsg::End => {
                    // crate::log!("[D] sub_onmessage(): worker({id}) received End msg.");

                    return;
                }
                ChMsg::Task(mut task_ptr, mut buf_ptr) => {
                    // crate::log!("[D] sub_onmessage(): worker({id}) received Task msg.");

                    let task = unsafe { task_ptr.as_mut() };
                    let buf = unsafe { buf_ptr.as_mut() };
                    task.invoke(buf);
                    ch.send(ChMsg::Fin(wid, task.rkey())).unwrap();
                }
                msg => {
                    panic!(
                        "[E] sub_onmessage(): worker({}) received unknown msg: {:?}",
                        wid, msg
                    );
                }
            }
        }
        unreachable!(
            "[E] sub_onmessage(): worker({}) failed to receive a msg",
            wid,
        );
    }

    // Safety: `ch` came from worker, which guarantees `ch` is valid.
    // See Worker::ch and Worker::open() for more details.
    let ch = unsafe { ch.as_ref() }.unwrap();
    entry(ch, WorkerId::new(id));

    // crate::log!("[D] worker({id}) sub_onmessage has finished");
}

pub fn set_panic_hook() {
    #[cfg(debug_assertions)]
    {
        // Show panic messages on browsers
        std::panic::set_hook(Box::new(console_error_panic_hook::hook));
        console_error_panic_hook::set_once();
    }
}
