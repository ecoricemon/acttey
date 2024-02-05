use crate::{
    app::{
        event::{Event, EventListener, EventManager},
        r#loop::Loop,
    },
    decl_return_wrap,
    ds::refs::RCell,
    ecs::{
        predefined::resource::{ResourcePack, TimeStamp},
        storage::Storage,
        system::{Invokable, System, Systems},
        traits::Entity,
    },
    primitive::mesh::{Geometry, Material, MeshResource, SeparateGeometry},
    render::{
        buffer::{BufferView, SizeOrData},
        descs,
        pipeline::{PipelineBuilder, PipelineLayoutBuilder},
        resource::RenderResource,
    },
    scene::{
        scene::{Scene, SceneManager},
        SceneError,
    },
    util::{key::ResKey, web, AsBytes, RcStr},
};
use std::rc::Rc;
use wasm_bindgen::prelude::*;
use web_sys::Performance;

pub struct App {
    /// A resource that handles all events such as input events.
    ev_mgr: RCell<EventManager>,

    /// Resources representing current app state.
    /// See [`AppState`] for more details.
    state: RCell<AppState>,

    /// A resource that can help you measure something.
    perf: Rc<Performance>,

    /// Loop function.
    main_loop: RCell<Loop>,
}

impl App {
    pub async fn new() -> Self {
        set_panic_hook();

        let ev_mgr = EventManager::new();
        let state = AppState::new().await;
        let main_loop = Loop::new();
        let perf = web::get_window()
            .performance()
            .expect_throw(crate::errmsg::WEBSYS_GET_PERFORMANCE);

        Self {
            ev_mgr: RCell::new(ev_mgr).with_tag("ev_mgr"),
            state: RCell::new(state).with_tag("state"),
            perf: Rc::new(perf),
            main_loop: RCell::new(main_loop).with_tag("main_loop"),
        }
    }

    /// Finds out the first canvas using the given CSS selectors and registers the canvas.
    /// This automatically generate a default [`Surface`] for render attachment.
    ///
    /// # Panics
    ///
    /// Panics if it's failed to find a canvas.
    pub fn add_canvas(&mut self, selectors: impl Into<RcStr>) -> &mut Self {
        self.state
            .borrow_mut()
            .render
            .add_canvas(selectors)
            .with_default()
            .unwrap();
        self
    }

    /// Adds a system.
    pub fn add_system<S: System>(&mut self, system: S) -> &mut Self {
        let skey = S::key();
        let sinfo = S::info();
        self.state
            .borrow_mut()
            .systems
            .insert(skey, Box::new(system));
        self.state.borrow_mut().storage.insert_sinfo(skey, sinfo);
        self
    }

    /// Adds a super system.
    pub fn add_super_system<S: System>(&mut self, sys: S) -> &mut Self {
        let skey = S::key();
        let sinfo = S::info();
        self.state.borrow_mut().super_systems.push(Box::new(sys));
        self.state.borrow_mut().storage.insert_sinfo(skey, sinfo);
        self
    }

    /// Adds a list of events.  
    /// Please refer to [`Self::add_listen_event()`] for more details.
    pub fn add_listen_events<'a>(
        &mut self,
        selectors: &str,
        event_types: impl Iterator<Item = &'a str>,
    ) -> &mut Self {
        for event_type in event_types {
            self.add_listen_event(selectors, event_type);
        }
        self
    }

    /// Finds out the first element using the given CSS selectors and
    /// tells the app to listen to `event_type` event.
    /// Use empty string selectors to designate window itself.  
    ///
    /// Events are stacked in a queue, and you can use them in systems,
    /// but they won't be consumed automatically.
    /// So that it's recommended to add `system::ClearInput` system
    /// to clean unused input events on a frame basis.
    pub fn add_listen_event(&mut self, selectors: &str, event_type: &str) -> &mut Self {
        let ev_mgr = RCell::clone(&self.ev_mgr);
        let handle = self
            .state
            .borrow()
            .render
            .get_canvas(selectors)
            .map(|canvas| canvas.handle())
            .unwrap_or(0);

        macro_rules! helper {
            ("resize") => {{
                self.add_noarg_event_listener(selectors, event_type, move || {
                    ev_mgr.borrow_mut().push(Event::Resized(handle));
                });
            }};
            ("mouse") => {{
                self.add_mouse_event_listener(
                    selectors,
                    event_type,
                    move |event: web_sys::MouseEvent| {
                        ev_mgr.borrow_mut().push((handle, event).into());
                    },
                );
            }};
            ("keyboard") => {{
                self.add_keyboard_event_listener(
                    selectors,
                    event_type,
                    move |event: web_sys::KeyboardEvent| {
                        ev_mgr.borrow_mut().push((handle, event).into());
                    },
                );
            }};
            ($($t:tt)*) => {
                Unreachable
            };
        }

        match event_type {
            "resize" => helper!("resize"),
            "click" => helper!("mouse"),
            "mousemove" => helper!("mouse"),
            "keydown" => helper!("keyboard"),
            "keyup" => helper!("keyboard"),
            _ => panic!("Unsupported event type {}", event_type),
        }

        self
    }

    pub fn add_geometry(
        &mut self,
        key: impl Into<ResKey>,
        geo: impl Into<SeparateGeometry>,
    ) -> GeometryReturn {
        let key: ResKey = key.into();
        self.state
            .borrow_mut()
            .meshes
            .add_geometry(key.clone(), Geometry::from(geo.into()));
        GeometryReturn {
            recv: self,
            ret: key,
        }
    }

    pub fn add_material(
        &mut self,
        key: impl Into<ResKey>,
        mat: impl Into<Material>,
    ) -> MaterialReturn {
        let key: ResKey = key.into();
        self.state
            .borrow_mut()
            .meshes
            .add_material(key.clone(), mat);
        MaterialReturn {
            recv: self,
            ret: key,
        }
    }

    pub fn add_mesh(
        &mut self,
        mesh_key: impl Into<ResKey>,
        geo_key: impl Into<ResKey>,
        mat_key: impl Into<ResKey>,
    ) -> MeshReturn {
        let mesh_key: ResKey = mesh_key.into();
        let geo_key: ResKey = geo_key.into();
        let mat_key: ResKey = mat_key.into();
        self.state
            .borrow_mut()
            .meshes
            .add_mesh(mesh_key.clone(), geo_key, mat_key);
        MeshReturn {
            recv: self,
            ret: mesh_key,
        }
    }

    // TODO: AppError
    pub fn add_scene(
        &mut self,
        key: impl Into<ResKey>,
        mut scene: Scene,
    ) -> Result<(), SceneError> {
        let key: ResKey = key.into();
        let mut state = self.state.borrow_mut();
        let AppState {
            render,
            meshes,
            scene_mgr,
            ..
        } = &mut *state;

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
        let vert_buf = render.add_vertex_buffer(SizeOrData::Size(vert_size))?;
        let index_buf = render.add_ro_index_buffer(SizeOrData::Size(index_size))?;
        scene.set_geometry_buffer(meshes, vert_buf, index_buf);

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
            let buf_view = BufferView::new_from_geometry(geo.as_interleaved().unwrap());
            builder.vert_buf_view.push(buf_view);
        }
        builder.set_surface_pack_index(scene.surf_pack_index.clone());
        let pb_index = render.add_pipeline_builder(builder);
        let pipeline = render.build_pipeline(pb_index, key.clone());
        scene.pipelines.insert(key.clone(), Rc::clone(pipeline));

        // Removes temporary builders.
        render.remove_pipeline_layout_builder(plb_index);
        render.remove_pipeline_builder(pb_index);

        // PassGraph.
        scene.build_pass_graph(&render.gpu, &key.label);

        // Adds the scene.
        scene_mgr.insert_active_scene(key, scene);
        Ok(())
    }

    // TODO: Update
    pub fn register_entity<E: Entity>(&mut self) -> &mut Self {
        self.state.borrow_mut().storage.insert_default::<E>();
        self
    }

    // TODO: Update
    pub fn insert_entity(&mut self, key: usize, ent: impl Entity) -> &mut Self {
        self.state.borrow_mut().storage.insert_entity(key, ent);
        self
    }

    pub fn run(&mut self) {
        // Configures the storage with the given system information.
        self.state.borrow_mut().storage.invalidate_sinfo();

        // Clones to move into the animation loop;
        let ev_mgr = RCell::clone(&self.ev_mgr);
        let state = RCell::clone(&self.state);
        let main_loop = RCell::clone(&self.main_loop);

        // Loop.
        let mut time_prev = 0.0;
        let callback = Closure::<dyn FnMut(f64)>::new(move |time: f64| {
            // Animation resources.
            let mut ev_mgr = ev_mgr.borrow_mut();
            let mut state = state.borrow_mut();
            let main_loop = main_loop.borrow_mut();
            let AppState {
                storage,
                systems,
                super_systems,
                render,
                scene_mgr,
                ..
            } = &mut *state;
            let mut res_pack = ResourcePack {
                ev_mgr: &mut ev_mgr,
                storage,
                render,
                time: &mut TimeStamp(time),
                systems: None,
                scene_mgr,
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
            // TODO: limit to the size of input.
            debug_assert!(res_pack.ev_mgr.is_command_empty());

            // Requests next frame.
            main_loop.request_animation_frame();

            time_prev = time;
        });

        let mut main_loop = self.main_loop.borrow_mut();
        main_loop.set_callback(callback);
        main_loop.request_animation_frame();
    }

    pub fn unregister_events<'a>(
        &mut self,
        selectors: &str,
        types: impl Iterator<Item = &'a str>,
    ) -> &mut Self {
        for type_ in types {
            self.unregister_event(selectors, type_);
        }
        self
    }

    pub fn unregister_event(&mut self, selectors: &str, type_: &str) {
        let mut ev_mgr = self.ev_mgr.borrow_mut();
        if let Some(old) = ev_mgr.remove_event_listener(selectors, type_) {
            let element = web::get_element_by_id(selectors).unwrap();
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

    fn add_noarg_event_listener(&mut self, selectors: &str, type_: &str, f: impl Fn() + 'static) {
        let listener = Closure::<dyn Fn()>::new(f);
        self.add_event_listner(selectors, type_, listener.as_ref().unchecked_ref());

        self.ev_mgr
            .borrow_mut()
            .add_event_listener(selectors, type_, listener.into());
    }

    fn add_mouse_event_listener(
        &mut self,
        selectors: &str,
        type_: &str,
        f: impl Fn(web_sys::MouseEvent) + 'static,
    ) {
        let listener = Closure::<dyn Fn(web_sys::MouseEvent)>::new(f);
        self.add_event_listner(selectors, type_, listener.as_ref().unchecked_ref());

        self.ev_mgr
            .borrow_mut()
            .add_event_listener(selectors, type_, listener.into());
    }

    fn add_keyboard_event_listener(
        &mut self,
        selectors: &str,
        type_: &str,
        f: impl Fn(web_sys::KeyboardEvent) + 'static,
    ) {
        let listener = Closure::<dyn Fn(web_sys::KeyboardEvent)>::new(f);
        self.add_event_listner(selectors, type_, listener.as_ref().unchecked_ref());

        self.ev_mgr
            .borrow_mut()
            .add_event_listener(selectors, type_, listener.into());
    }

    fn add_event_listner(&mut self, selectors: &str, type_: &str, listener: &js_sys::Function) {
        if selectors.is_empty() {
            let window = web::get_window();
            window
                .add_event_listener_with_callback(type_, listener)
                .expect_throw(crate::errmsg::WEBSYS_ADD_LISTENER);
        } else {
            let element = web::query_selector(selectors).unwrap().unwrap();
            element
                .add_event_listener_with_callback(type_, listener)
                .expect_throw(crate::errmsg::WEBSYS_ADD_LISTENER);
        }
    }
}

/// Resources representing the app's state.
struct AppState {
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
}

impl AppState {
    async fn new() -> Self {
        Self {
            storage: Storage::new(),
            systems: Systems::new(),
            super_systems: Vec::new(),
            render: RenderResource::new().await.unwrap(),
            meshes: MeshResource::new(),
            scene_mgr: SceneManager::new(),
        }
    }
}

decl_return_wrap!(GeometryReturn, App, ResKey);
decl_return_wrap!(MaterialReturn, App, ResKey);
decl_return_wrap!(MeshReturn, App, ResKey);

pub fn set_panic_hook() {
    #[cfg(debug_assertions)]
    {
        // Show panic messages on browsers
        std::panic::set_hook(Box::new(console_error_panic_hook::hook));
        console_error_panic_hook::set_once();
    }
}
