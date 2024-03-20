use super::{canvas::CanvasPack, AppError};
use crate::{
    top::{event::{Event, EventManager}, r#loop::Loop},
    decl_return_wrap,
    ds::refs::RCell,
    ecs::{
        predefined::{
            components,
            resource::{ResourcePack, TimeStamp},
        },
        storage::{EntityReg, Storage},
        system::{Invokable, System, SystemInfo, Systems},
        traits::{Component, TypedGetterMut},
        SystemKey,
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
    util::{key::ResKey, string::RcStr, web, AsBytes},
    worker::{msg, MainWorker},
};
use std::{any::Any, cell::RefCell, mem, rc::Rc};
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
        let main = MainWorker::spawn("main-worker", 0).expect_throw("failed to spawn web worker");
        let main = Rc::new(main);

        // Sends INIT message with dummy canvas to the main worker.
        let canvas = canvases.get_dummy();
        let offcanvas = canvas.transfer_control_to_offscreen().unwrap();
        let (element, handle) = offcanvas.destruct();
        msg::JsMsgInit(msg::JsMsgCanvas { element, handle }).post_to(&main);

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
        let (element, handle) = offcanvas.destruct();
        msg::JsMsgCanvas { element, handle }.post_to(&self.main);

        drop(canvases);
        Ok(self)
    }

    pub fn call_initializer(&self, f: fn(&mut AppState)) {
        // Sends user initialization function.
        let f: fn() = unsafe { mem::transmute(f) };
        msg::JsMsgFn {
            f: f as usize as _,
            disc: msg::JsMsgFnDisc::UserInit as _,
        }
        .post_to(&self.main);
    }

    /// Registers WINDOW_SCALE message proxy.
    fn register_scale_message_proxy(&mut self) {
        let main = Rc::clone(&self.main);
        let buf = msg::JsMsgWindowScale::create_buffer();
        let proxy =
            Closure::<dyn Fn(web_sys::CustomEvent)>::new(move |event: web_sys::CustomEvent| {
                let msg = msg::JsMsgWindowScale {
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
        let buf = msg::JsMsgCanvasResize::create_buffer();
        let proxy = Closure::<dyn Fn()>::new(move || {
            let canvases = canvases.borrow();
            for canvas in canvases.values() {
                let msg = msg::JsMsgCanvasResize {
                    handle: canvas.handle(),
                    width: canvas.client_width() as i16,
                    height: canvas.client_height() as i16,
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
    /// [`MainWorker`] owns this [`AppState`], which means the state belongs to worker.
    /// All graphics jobs will be done in the worker.
    static APP_STATE: RefCell<AppState> = panic!();

}

/// Initializes [`APP_STATE`] on main worker, not in window context.
/// That's because window and worker don't share memory (We can use shared memory with some restrictions).
/// When initialization is over, JS replaces this event handler with [`main_onmessage`].
#[wasm_bindgen]
pub async fn main_onmessage_init(event: web_sys::MessageEvent) -> bool {
    let data = event.data();
    debug_assert!(data.is_array());
    let buf: js_sys::Array = data.unchecked_into();
    match msg::JsMsgHeader::from_js_value(buf.get(0)).0 {
        msg::JsMsgHeader::INIT_INNER => {
            crate::log!("[D] main_onmessage_init(): worker received INIT msg.");

            // Initializes `APP_STATE`.
            let msg = msg::JsMsgInit::read_body(&buf);
            let offcanvas = OffCanvas::new(msg.0.element, msg.0.handle);
            let state = AppState::new(offcanvas).await;
            APP_STATE.set(state);

            // Fully initialized. JS will change `onmessage` after receiving true.
            crate::log!("[D] main_onmessage_init(): worker returns true");
            return true;
        }
        _ => {
            crate::log!(
                "[W] main_onmessage_init(): worker received unknown msg: {:?}",
                buf
            );
        }
    }
    false
}

#[wasm_bindgen]
pub fn main_onmessage(event: web_sys::MessageEvent, _id: u32) {
    let data = event.data();
    debug_assert!(data.is_array());
    let buf: js_sys::Array = data.unchecked_into();
    match msg::JsMsgHeader::from_js_value(buf.get(0)).0 {
        msg::JsMsgHeader::CANVAS_INNER => {
            crate::log!("[D] main_onmessage(): worker received CANVAS msg.");

            let msg = msg::JsMsgCanvas::read_body(&buf);
            APP_STATE.with_borrow_mut(move |state| {
                state.register_offcanvas(msg.element, msg.handle);
            });
        }
        msg::JsMsgHeader::WINDOW_SCALE_INNER => {
            crate::log!("[D] main_onmessage(): worker received WINDOW_SCALE msg.");

            let msg = msg::JsMsgWindowScale::read_body(&buf);
            APP_STATE.with_borrow_mut(|state| {
                state.update_scale_factor(msg.scale);
            });
        }
        msg::JsMsgHeader::CANVAS_RESIZE_INNER => {
            crate::log!("[D] main_onmessage(): worker received CANVAS_RESIZE msg.");

            let msg = msg::JsMsgCanvasResize::read_body(&buf);
            APP_STATE.with_borrow_mut(|state| {
                state.ev_mgr.push(Event::Resized(msg));
            });
        }
        msg::JsMsgHeader::FN_INNER => {
            crate::log!("[D] main_onmessage(): worker received FN msg.");

            let msg = msg::JsMsgFn::read_body(&buf);
            if msg.disc == msg::JsMsgFnDisc::UserInit as u32 {
                APP_STATE.with_borrow_mut(|state| {
                    match msg.disc {
                        disc if disc == msg::JsMsgFnDisc::UserInit as u32 => {
                            // TODO: This corresponds to App::call_initializer().
                            // But it's too error prone. Come up with better solution.
                            let f: fn(&mut AppState) = unsafe { mem::transmute(msg.f as usize) };
                            f(state);
                        }
                        _ => {}
                    }
                });
                AppState::run();
            }
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
    /// A resource managing all components.
    storage: Storage,

    /// A resource managing all systems.
    systems: Systems,

    /// TODO: merge to systems.
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
}

impl AppState {
    async fn new(offcanvas: OffCanvas) -> Self {
        Self {
            storage: Storage::new(),
            systems: Systems::new(),
            super_systems: Vec::new(),
            render: RenderResource::new(offcanvas).await.unwrap(),
            meshes: MeshResource::new(),
            scene_mgr: SceneManager::new(),
            ev_mgr: EventManager::new(),
            main_loop: Loop::new(),
        }
    }

    fn register_offcanvas(&mut self, element: web_sys::OffscreenCanvas, handle: u32) {
        self.render
            .add_canvas(OffCanvas::new(element, handle))
            .with_default_surface();
    }

    fn update_scale_factor(&mut self, scale: f64) {
        self.render.scale = scale;
    }

    #[inline]
    pub fn add_geometry(
        &mut self,
        key: impl Into<ResKey>,
        geo: impl Into<SeparateGeometry>,
    ) -> GeometryReturn {
        fn inner(state: &mut AppState, key: ResKey, geo: SeparateGeometry) -> GeometryReturn {
            state.meshes.add_geometry(key.clone(), Geometry::from(geo));
            GeometryReturn {
                recv: state,
                ret: key,
            }
        }

        inner(self, key.into(), geo.into())
    }

    #[inline]
    pub fn add_material(
        &mut self,
        key: impl Into<ResKey>,
        mat: impl Into<Material>,
    ) -> MaterialReturn {
        fn inner(state: &mut AppState, key: ResKey, mat: Material) -> MaterialReturn {
            state.meshes.add_material(key.clone(), mat);
            MaterialReturn {
                recv: state,
                ret: key,
            }
        }

        inner(self, key.into(), mat.into())
    }

    #[inline]
    pub fn add_mesh(
        &mut self,
        mesh: impl Into<ResKey>,
        geo: impl Into<ResKey>,
        mat: impl Into<ResKey>,
    ) -> MeshReturn {
        fn inner(state: &mut AppState, mesh: ResKey, geo: ResKey, mat: ResKey) -> MeshReturn {
            state.meshes.add_mesh(mesh.clone(), geo, mat);
            MeshReturn {
                recv: state,
                ret: mesh,
            }
        }

        inner(self, mesh.into(), geo.into(), mat.into())
    }

    #[inline]
    pub fn register_entity(&mut self, reg: EntityReg) -> usize {
        self.storage.register_entity(reg)
    }

    pub fn temp_begin_add_entity(&mut self, enti: usize) {
        let cont = self.storage.ents.get_entity_container_mut(enti).unwrap();
        cont.begin_add_item();
    }

    pub fn temp_add_entity_comp<C: Component>(&mut self, enti: usize, value: C) {
        let cont = self.storage.ents.get_entity_container_mut(enti).unwrap();
        let coli = cont.get_column_index(&ty!(C)).unwrap();
        unsafe { cont.add_item(coli, &value as *const C as *const u8) };
    }

    pub fn temp_end_add_entity(&mut self, enti: usize) -> usize {
        let cont = self.storage.ents.get_entity_container_mut(enti).unwrap();
        cont.end_add_item()
    }

    // TODO: AppError
    #[inline]
    pub fn add_scene(&mut self, key: impl Into<ResKey>, scene: Scene) -> Result<(), SceneError> {
        self._add_scene(key.into(), scene)
    }

    fn _add_scene(&mut self, key: ResKey, mut scene: Scene) -> Result<(), SceneError> {
        let AppState {
            render,
            meshes,
            scene_mgr,
            storage,
            ..
        } = self;

        scene.set_gpu(Rc::clone(&render.gpu));

        // Makes mapping between scene's nodes and ECS's entities.
        for (i, node) in scene.hierarchy.iter_nodes() {
            if let Some((enti, eid)) = node.get_mapped_entity() {
                // TODO: Use filter, This is duplicate implementation.
                // Using system may be a good choice.
                let cont = storage.ents.get_entity_container_mut(enti).unwrap();
                let coli = cont.get_column_index(&ty!(components::Drawable)).unwrap();
                let mut borrowed = cont.borrow_column_mut(coli).unwrap();
                let mut getter: TypedGetterMut<'_, components::Drawable> = (&mut *borrowed).into();
                let value = unsafe { getter.get(eid).unwrap() };
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

    /// Adds a system.
    #[inline]
    pub fn add_system<S: System>(&mut self, system: S) -> &mut Self {
        fn inner(
            state: &mut AppState,
            skey: SystemKey,
            sinfo: SystemInfo,
            system: Box<dyn Invokable>,
        ) -> &mut AppState {
            {
                state.systems.insert(skey, system);
                state.storage.register_system(skey, sinfo);
            }
            state
        }

        inner(self, S::key(), S::info(), Box::new(system))
    }

    /// Adds a super system.
    /// Super systems can add or remove normal systems.
    #[inline]
    pub fn add_super_system<S: System>(&mut self, system: S) -> &mut Self {
        fn inner(
            state: &mut AppState,
            skey: SystemKey,
            sinfo: SystemInfo,
            system: Box<dyn Invokable>,
        ) -> &mut AppState {
            {
                state.super_systems.push(system);
                state.storage.register_system(skey, sinfo);
            }
            state
        }

        inner(self, S::key(), S::info(), Box::new(system))
    }

    pub fn run() {
        // Creates main loop which is animation callback.
        let callback = Closure::<dyn FnMut(f64)>::new(move |time: f64| {
            APP_STATE.with_borrow_mut(|state| {
                let Self {
                    storage,
                    systems,
                    super_systems,
                    render,
                    scene_mgr,
                    ev_mgr,
                    main_loop,
                    ..
                } = state;

                let mut res_pack = ResourcePack {
                    ev_mgr,
                    storage,
                    render,
                    time: &mut TimeStamp(time),
                    systems: None,
                    scene_mgr,
                };

                // Invokes systems.
                for sys in systems.values_mut() {
                    sys.invoke(&res_pack);
                }

                // Invokes super systems.
                res_pack.systems = Some(systems);
                for sys in super_systems.iter_mut() {
                    sys.invoke(&res_pack);
                }

                // TODO: limit to the size of input.
                debug_assert!(res_pack.ev_mgr.is_command_empty());

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

decl_return_wrap!(GeometryReturn, AppState, ResKey);
decl_return_wrap!(MaterialReturn, AppState, ResKey);
decl_return_wrap!(MeshReturn, AppState, ResKey);

pub fn set_panic_hook() {
    #[cfg(debug_assertions)]
    {
        // Show panic messages on browsers
        std::panic::set_hook(Box::new(console_error_panic_hook::hook));
        console_error_panic_hook::set_once();
    }
}
