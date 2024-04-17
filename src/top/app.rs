use super::{
    canvas::CanvasPack,
    event::{Event, EventManager},
    r#loop::Loop,
    AppError,
};
use crate::{
    decl_return_wrap,
    ds::{get::GetterMut, refs::RCell},
    ecs::{
        component::Component,
        entity::{EntityForm, EntityKey},
        predefined::components,
        resource::Resource,
        schedule::Scheduler,
        system::{Client, Invokable, System, SystemKey},
    },
    primitive::mesh::{Geometry, Material, MeshResource, SeparateGeometry},
    render::{
        buffer::{BufferView, SizeOrData},
        canvas::OffCanvas,
        descs,
        pipeline::{PipelineBuilder, PipelineLayoutBuilder},
        resource::RenderResource,
    },
    scene::{
        inner::{Scene, SceneManager},
        SceneError,
    },
    ty,
    util::{
        key::ObjectKey,
        string::{self, RcStr},
        web, AsBytes,
    },
    worker::{
        msg::{self, ChMsg},
        Channel, MainChannel, MainWorker, Worker, WorkerId,
    },
};
use std::{any::Any, cell::RefCell, mem, num::NonZeroU32, ptr::NonNull, rc::Rc, thread};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct App {
    main: Rc<MainWorker>,
    canvases: RCell<CanvasPack>,
    proxies: Vec<Box<dyn Any>>,
}

impl App {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        set_panic_hook();

        // Canvase pack.
        let canvases = CanvasPack::new();

        // Spawns main worker.
        let main = MainWorker::spawn("main-worker").expect_throw("failed to spawn web worker");
        let main = Rc::new(main);

        // Sends INIT message with dummy canvas to the main worker.
        let canvas = canvases.get_dummy();
        let offcanvas = canvas.transfer_control_to_offscreen().unwrap();
        let (element, handle) = offcanvas.destructure();
        msg::MsgEventInit(msg::MsgEventCanvas { element, handle }).post_to(main.handle_js());

        let mut app = Self {
            main,
            canvases: RCell::new(canvases),
            proxies: Vec::new(),
        };

        // Registers default event proxies.
        app.register_scale_message_proxy();
        app.register_resize_message_proxy();

        // Detects scale change.
        detect_scale_change();

        app
    }

    pub fn register_canvas(&mut self, selectors: impl Into<RcStr>) -> Result<&mut Self, AppError> {
        // Adds canvas.
        let mut canvases = self.canvases.borrow_mut();
        let canvas = canvases.insert(selectors)?;
        let offcanvas = canvas.transfer_control_to_offscreen()?;

        // Transfers offscreen canvas to the main worker.
        let (element, handle) = offcanvas.destructure();
        msg::MsgEventCanvas { element, handle }.post_to(self.main.handle_js());

        drop(canvases);
        Ok(self)
    }

    pub fn call_initializer(&self, f: fn(&mut AppState)) {
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

    /// Registers WINDOW_SCALE message proxy.
    fn register_scale_message_proxy(&mut self) {
        let main = Rc::clone(&self.main);
        let buf = msg::MsgEventWindowScale::create_buffer();
        let proxy =
            Closure::<dyn Fn(web_sys::CustomEvent)>::new(move |event: web_sys::CustomEvent| {
                let msg = msg::MsgEventWindowScale {
                    scale: event.detail().as_f64().unwrap(),
                };
                msg.write_body(&buf);
                main.post_message(&buf).unwrap();
            });
        web::window()
            .add_event_listener_with_callback("scale", proxy.as_ref().unchecked_ref())
            .unwrap();
        self.proxies.push(Box::new(proxy));
    }

    /// Registers CANVAS_RESIZE message proxy.
    fn register_resize_message_proxy(&mut self) {
        let main = Rc::clone(&self.main);
        let canvases = RCell::clone(&self.canvases);
        let buf = msg::MsgEventCanvasResize::create_buffer();
        let proxy = Closure::<dyn Fn()>::new(move || {
            let canvases = canvases.borrow();
            for canvas in canvases.values().filter(|canvas| !canvas.is_dummy()) {
                let msg = msg::MsgEventCanvasResize {
                    handle: canvas.handle(),
                    width: canvas.client_width() as _,
                    height: canvas.client_height() as _,
                };
                msg.write_body(&buf);
                main.post_message(&buf).unwrap();
            }
        });
        web::window()
            .add_event_listener_with_callback("resize", proxy.as_ref().unchecked_ref())
            .unwrap();
        self.proxies.push(Box::new(proxy));
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
            let state = AppState::new(offcanvas).await;
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
                state.register_offcanvas(msg.element, msg.handle);
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

    /// TODO: merge to the scheduler.
    super_systems: Vec<Box<dyn Invokable>>,

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

    /// The last active system registered through the state.
    last_sys: Option<SystemKey>,
}

impl AppState {
    async fn new(offcanvas: OffCanvas) -> Self {
        Self {
            sched: Scheduler::new(),
            super_systems: Vec::new(),
            render: RenderResource::new(offcanvas).await.unwrap(),
            meshes: MeshResource::new(),
            scene_mgr: SceneManager::new(),
            ev_mgr: EventManager::new(),
            main_loop: Loop::new(),
            workers: Vec::new(),
            ch: MainChannel::new(),
            last_sys: None,
        }
    }

    /// Acquires addresses of resource fields from this app state.
    /// And then sets those addresses to the scheduler.
    /// Note that this method must be called once app state is fixed in memory.
    ///
    /// For performance reason, we're not going to use `Box` or something like that here.
    /// Because we are aware of when app state is fixed in memory and its resource fields will never be moved or dropped.
    fn set_resource_address(&mut self) {
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
        let ptr: NonNull<u8> = unsafe { NonNull::new_unchecked(addr_of_mut!(self.ev_mgr) as _) };
        self.sched
            .register_default_resource(self.ev_mgr.rkey(), ptr);

        // TODO: other resources
    }

    fn register_offcanvas(&mut self, element: web_sys::OffscreenCanvas, handle: u32) {
        self.render
            .add_canvas(OffCanvas::new(element, handle))
            .with_default_surface();
    }

    fn update_scale_factor(&mut self, scale: f64) {
        self.render.scale = scale;
    }

    pub fn spawn_worker(&mut self, num: Option<usize>) {
        debug_assert!(self.workers.is_empty());

        // Spawns sub workers.
        let num = num.unwrap_or(web::hardware_concurrency() - 1);
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
    pub fn add_geometry(
        &mut self,
        key: impl Into<ObjectKey>,
        geo: impl Into<SeparateGeometry>,
    ) -> GeometryReturn {
        fn inner(this: &mut AppState, key: ObjectKey, geo: SeparateGeometry) -> GeometryReturn {
            this.meshes.add_geometry(key.clone(), Geometry::from(geo));
            GeometryReturn {
                recv: this,
                ret: key,
            }
        }

        inner(self, key.into(), geo.into())
    }

    #[inline]
    pub fn add_material(
        &mut self,
        key: impl Into<ObjectKey>,
        mat: impl Into<Material>,
    ) -> MaterialReturn {
        fn inner(this: &mut AppState, key: ObjectKey, mat: Material) -> MaterialReturn {
            this.meshes.add_material(key.clone(), mat);
            MaterialReturn {
                recv: this,
                ret: key,
            }
        }

        inner(self, key.into(), mat.into())
    }

    #[inline]
    pub fn add_mesh(
        &mut self,
        mesh: impl Into<ObjectKey>,
        geo: impl Into<ObjectKey>,
        mat: impl Into<ObjectKey>,
    ) -> MeshReturn {
        fn inner(
            this: &mut AppState,
            mesh: ObjectKey,
            geo: ObjectKey,
            mat: ObjectKey,
        ) -> MeshReturn {
            this.meshes.add_mesh(mesh.clone(), geo, mat);
            MeshReturn {
                recv: this,
                ret: mesh,
            }
        }

        inner(self, mesh.into(), geo.into(), mat.into())
    }

    #[inline]
    pub fn register_entity(&mut self, reg: EntityForm) -> usize {
        self.sched.register_entity(reg)
    }

    pub fn temp_begin_add_entity(&mut self, enti: usize) {
        let cont = self
            .sched
            .entities
            .get_entity_container_mut(EntityKey::Index(enti))
            .unwrap();
        cont.begin_add_item();
    }

    pub fn temp_add_entity_comp<C: Component>(&mut self, enti: usize, value: C) {
        let cont = self
            .sched
            .entities
            .get_entity_container_mut(EntityKey::Index(enti))
            .unwrap();
        let coli = cont.get_column_index(&ty!(C)).unwrap();
        unsafe { cont.add_item(coli, &value as *const C as *const u8) };
    }

    pub fn temp_end_add_entity(&mut self, enti: usize) -> usize {
        let cont = self
            .sched
            .entities
            .get_entity_container_mut(EntityKey::Index(enti))
            .unwrap();
        cont.end_add_item()
    }

    // TODO: AppError
    #[inline]
    pub fn add_scene(&mut self, key: impl Into<ObjectKey>, scene: Scene) -> Result<(), SceneError> {
        self._add_scene(key.into(), scene)
    }

    fn _add_scene(&mut self, key: ObjectKey, mut scene: Scene) -> Result<(), SceneError> {
        let AppState {
            sched,
            render,
            meshes,
            scene_mgr,
            ..
        } = self;

        scene.set_gpu(Rc::clone(&render.gpu));

        // Makes mapping between scene's nodes and ECS's entities.
        for (i, node) in scene.hierarchy.iter_nodes() {
            if let Some((enti, eid)) = node.get_mapped_entity() {
                // TODO: Use filter, This is duplicate implementation.
                // Using system may be a good choice.
                let cont = sched
                    .entities
                    .get_entity_container_mut(EntityKey::Index(enti))
                    .unwrap();
                let coli = cont.get_column_index(&ty!(components::Drawable)).unwrap();
                let mut borrowed = cont.borrow_column_mut(coli).unwrap();
                let mut getter: GetterMut<'_, components::Drawable> = (&mut *borrowed).into();
                let value = getter.get(eid).unwrap();
                value.node.scene_key = key.id;
                value.node.node_index = i;
            }
        }

        // Makes geometry to be interleaved and sets it to the scene.
        meshes.make_mesh_geometry_interleaved(scene.meshes.iter());
        scene.fill_geometry_and_material(meshes)?;

        // Creates a surface pack and sets it to the scene.
        let index = render.add_surface_pack_from(scene.canvases.iter());
        scene.set_surface_pack_index(index);

        // Creates shader builder, builds, and sets it to the scene.
        let builder = scene.create_shader_builder(meshes, &render.surf_packs, &render.surfaces);
        let builder_index = render.add_shader_builder(builder);
        let shader = render.build_shader(builder_index, &key);
        scene.set_shader(Rc::clone(shader));

        // Calculates size for geometry buffers on consideration of alignment.
        let (vert_size, index_size) = scene.calc_geometry_buffer_size(meshes);

        // Requests geometry buffers and sets them to the scene.
        let vert_buf = render.add_vertex_buffer(SizeOrData::Size(vert_size, true))?;
        let index_buf = render.add_ro_index_buffer(SizeOrData::Size(index_size, true))?;
        scene.set_geometry_buffer(meshes, vert_buf, index_buf);

        // Instance buffer.
        let inst_buf_view = scene.get_transform_buffer_view();
        let inst_buf_size = inst_buf_view.size() as u64;
        let inst_buf = render.add_vertex_buffer(SizeOrData::Size(inst_buf_size, false))?;
        scene.set_instance_buffer(Rc::clone(&inst_buf));

        // Requests camera buffers and sets them to the scene.
        let Scene {
            cameras,
            camera_bufs,
            camera_binds,
            ..
        } = &mut scene;
        for (camera_key, camera) in cameras.iter() {
            let cam_bytes = camera.as_bytes();
            let uni_buf = render.add_uniform_buffer(SizeOrData::Data(cam_bytes))?;
            let desc = descs::BufferBindDesc {
                layout_key: camera_key.clone(),
                group_key: camera_key.clone(),
                bufs: &[&uni_buf],
                item_sizes: &[cam_bytes.len() as u64],
                ..Default::default()
            };
            render.add_default_buffer_bind(desc);
            let bind_group = render.get_bind_group(camera_key).unwrap();
            camera_binds.insert(camera_key.clone(), Rc::clone(bind_group));
            camera_bufs.insert(camera_key.clone(), uni_buf);
        }

        // Requests material buffers and sets them to the scene.
        let Scene {
            mats,
            mat_bufs,
            mat_binds,
            ..
        } = &mut scene;
        for (mat_key, _) in mats.iter() {
            let mat = meshes.get_material(mat_key).unwrap();
            let mat_bytes = mat.as_bytes();
            let uni_buf = render.add_ro_uniform_buffer(SizeOrData::Data(mat_bytes))?;
            let desc = descs::BufferBindDesc {
                layout_key: mat_key.clone(),
                group_key: mat_key.clone(),
                bufs: &[&uni_buf],
                item_sizes: &[mat_bytes.len() as u64],
                ..Default::default()
            };
            render.add_default_buffer_bind(desc);
            let bind_group = render.get_bind_group(mat_key).unwrap();
            mat_binds.insert(mat_key.clone(), Rc::clone(bind_group));
            mat_bufs.insert(mat_key.clone(), uni_buf);
        }

        // Creates pipeline layout.
        let mut builder = PipelineLayoutBuilder::new();
        let Scene { cameras, mats, .. } = &mut scene;
        if let Some(camera_key) = cameras.keys().next() {
            let bind_layout = render.get_bind_group_layout(camera_key).unwrap();
            builder.bind_group_layouts.push(Rc::clone(bind_layout));
        }
        if let Some(mat_key) = mats.keys().next() {
            let bind_layout = render.get_bind_group_layout(mat_key).unwrap();
            builder.bind_group_layouts.push(Rc::clone(bind_layout));
        }
        let plb_index = render.add_pipeline_layout_builder(builder);
        let layout = render.build_pipeline_layout(plb_index, key.clone());

        // Creates pipeline.
        let mut builder = PipelineBuilder::new();
        builder.set_layout(Rc::clone(layout));
        let shader = scene.shader.as_ref().unwrap();
        if shader.has_vertex_stage() {
            builder.set_vertex_shader(Rc::clone(shader));
        }
        if shader.has_fragment_stage() {
            builder.set_fragment_shader(Rc::clone(shader));
        }
        if let Some(geo_key) = scene.geos.keys().next() {
            let geo = meshes.get_geometry(geo_key).unwrap();
            // vertex buffer
            let view = BufferView::new_from_geometry(geo.as_interleaved().unwrap());
            builder.vert_buf_view.push(view);
            // instance buffer
            let mut view = BufferView::new(
                inst_buf_view.get_item_size(),
                inst_buf_view.len,
                inst_buf_view.offset,
            );
            view.set_buffer(inst_buf);
            view.set_vertex_step_mode(wgpu::VertexStepMode::Instance);
            // TODO: Hard coded
            view.set_vertex_attributes(&[
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Float32x4,
                    offset: 0,
                    shader_location: 2,
                },
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Float32x4,
                    offset: 16,
                    shader_location: 3,
                },
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Float32x4,
                    offset: 32,
                    shader_location: 4,
                },
                wgpu::VertexAttribute {
                    format: wgpu::VertexFormat::Float32x4,
                    offset: 48,
                    shader_location: 5,
                },
            ]);
            builder.vert_buf_view.push(view);
        }
        builder.set_surface_pack_index(scene.surf_pack_index.clone());
        let pb_index = render.add_pipeline_builder(builder);
        let pipeline = render.build_pipeline(pb_index, key.clone());
        scene.pipelines.insert(key.clone(), Rc::clone(pipeline));

        // Removes temporary builders.
        render.remove_pipeline_layout_builder(plb_index);
        render.remove_pipeline_builder(pb_index);

        // PassGraph.
        scene.build_pass_graph(&key.label);

        // Adds the scene.
        scene_mgr.insert_scene(key.clone(), scene);
        scene_mgr.activate_scene(key);
        Ok(())
    }

    /// Registers and activates a system.
    #[inline]
    pub fn register_system<S: System>(&mut self, sys: S) -> bool {
        let res = self.sched.register_system(sys);

        fn activate(this: &mut AppState, skey: SystemKey) {
            if let Some(last_sys) = this.last_sys.take() {
                this.sched.activate_system(&skey, &last_sys);
            } else {
                this.sched.activate_system_as_first(&skey);
            }
            this.last_sys = Some(skey);
        }

        activate(self, S::key());
        res
    }

    #[inline]
    pub fn register_inactive_system<S: System>(&mut self, _sys: S) {
        todo!()
    }

    /// Adds a super system.
    /// Super systems can add or remove normal systems.
    #[inline]
    pub fn add_super_system<S: System>(&mut self, _sys: S) -> &mut Self {
        todo!()
    }

    pub fn run() {
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

#[wasm_bindgen(js_name = subOnMessage)]
pub unsafe fn sub_onmessage(ch: *const Channel, id: u32) {
    // Entrypoint to the sub worker's meesage loop.
    fn entry(client: &mut Client) {
        while let Ok(msg) = client.recv() {
            match msg {
                ChMsg::ReqHandle => {
                    // crate::log!("[D] sub_onmessage(): worker({id}) ReqHandle msg.");

                    client.send(ChMsg::Handle(thread::current())).unwrap();
                }
                ChMsg::End => {
                    // crate::log!("[D] sub_onmessage(): worker({id}) received End msg.");

                    return;
                }
                ChMsg::Task(mut task_ptr, buf_ptr) => {
                    // crate::log!("[D] sub_onmessage(): worker({id}) received Task msg.");

                    let task = unsafe { task_ptr.as_mut() };
                    let buf = unsafe { buf_ptr.as_ref() };
                    task.invoke(buf);
                    client.send(ChMsg::Fin(client.wid(), task.rkey())).unwrap();
                }
                msg => {
                    panic!(
                        "[E] sub_onmessage(): worker({}) received unknown msg: {:?}",
                        client.wid(),
                        msg
                    );
                }
            }
        }
        unreachable!(
            "[E] sub_onmessage(): worker({}) failed to receive a msg",
            client.wid()
        );
    }

    // Safety: `ch` came from worker, which guarantees `ch` is valid.
    // See Worker::ch and Worker::open() for more details.
    let ch = unsafe { ch.as_ref() };
    let mut client = Client::new(ch.unwrap(), entry, WorkerId::new(id));
    entry(&mut client);

    // crate::log!("[D] worker({id}) sub_onmessage has finished");
}

decl_return_wrap!(GeometryReturn, AppState, ObjectKey);
decl_return_wrap!(MaterialReturn, AppState, ObjectKey);
decl_return_wrap!(MeshReturn, AppState, ObjectKey);

pub fn set_panic_hook() {
    #[cfg(debug_assertions)]
    {
        // Show panic messages on browsers
        std::panic::set_hook(Box::new(console_error_panic_hook::hook));
        console_error_panic_hook::set_once();
    }
}
