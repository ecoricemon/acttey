use crate::{
    decl_return_wrap,
    ds::{
        generational::{GenIndexRc, GenVecRc},
        get::GetterMut,
    },
    ecs::{entity::EntityId, predefined::components},
    primitive::{camera::Camera, mesh::Geometry},
    render::{
        buffer::res::BufferRc,
        context::Gpu,
        desc,
        pass::{
            DrawIndexedCmd, Pass, PassGraph, RenderPass, SetBindGroupCmd, SetIndexBufferCmd,
            SetPipelineCmd, SetVertexBufferCmd,
        },
        shaders::{
            helper::{BindableResource, ShaderHelper},
            res::Shader,
        },
    },
    scene::{
        hierarchy::{Node, SceneHierarchy},
        SceneError,
    },
    ty,
    util::{key::ObjectKey, AsBytes, AsMultiBytes, View},
};
use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    rc::Rc,
    sync::Arc,
};

pub struct SceneManager {
    /// Scenes.
    scenes: HashMap<ObjectKey, Scene, Hasher>,

    /// Active scenes.
    active: HashSet<ObjectKey, Hasher>,
}

impl SceneManager {
    pub fn new() -> Self {
        Self {
            scenes: HashMap::default(),
            active: HashSet::default(),
        }
    }

    pub fn get_scene<Q>(&self, key: &Q) -> Option<&Scene>
    where
        ObjectKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.scenes.get(key)
    }

    pub fn get_scene_mut<Q>(&mut self, key: &Q) -> Option<&mut Scene>
    where
        ObjectKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.scenes.get_mut(key)
    }

    pub fn insert_scene(&mut self, key: ObjectKey, scene: Scene) -> Option<Scene> {
        self.scenes.insert(key, scene)
    }

    /// Tries to remove a scene, but it'll fail if the scene is in active state.
    pub fn remove_scene<Q>(&mut self, key: &Q) -> Option<Scene>
    where
        ObjectKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        if !self.active.contains(key) {
            self.scenes.remove(key)
        } else {
            None
        }
    }

    /// # Panic
    ///
    /// Panics if it failed to find scene using the given `key`.
    pub fn activate_scene(&mut self, key: ObjectKey) {
        assert!(self.scenes.contains_key(&key));
        self.active.insert(key);
    }

    /// # Panic
    ///
    /// Panics if it failed to find scene using the given `key`.
    pub fn deactivate_scene(&mut self, key: ObjectKey) {
        assert!(self.scenes.contains_key(&key));
        self.active.remove(&key);
    }

    pub fn iter_active_scene(&self) -> impl Iterator<Item = (&ObjectKey, &Scene)> {
        self.scenes
            .iter()
            .filter(|(key, _)| self.active.contains(key))
    }

    pub fn iter_active_scene_mut(&mut self) -> impl Iterator<Item = (&ObjectKey, &mut Scene)> {
        self.scenes
            .iter_mut()
            .filter(|(key, _)| self.active.contains(key))
    }

    pub fn temp_adopt(
        key: ObjectKey,
        mut scene: Scene,
        sched: &mut crate::ecs::schedule::Scheduler,
        render: &mut crate::render::manager::RenderManager,
        stor: &mut crate::top::storage::CommonStorage,
        scene_mgr: &mut Self,
    ) -> Result<(), SceneError> {
        todo!()
        /* WIP
        scene.set_gpu(Rc::clone(&render.gpu));

        // Makes mapping between scene's nodes and ECS's entities.
        for (i, node) in scene.hierarchy.iter_node() {
            if let Some(eid) = node.get_mapped_entity() {
                // TODO: Use filter, This is duplicate implementation.
                // Using system may be a good choice.
                let cont = sched.entities.get_entity_container_mut(eid.key()).unwrap();
                let coli = cont.get_column_index(&ty!(components::SceneNode)).unwrap();
                let mut borrowed = cont.borrow_column_mut(coli).unwrap();
                let mut getter: GetterMut<'_, components::SceneNode> = (&mut *borrowed).into();
                let value = getter.get(eid.index()).unwrap();
                value.scene_key = key.id();
                value.node_index = i;
            }
        }

        // Makes geometry to be interleaved and sets it to the scene.
        meshes.make_mesh_geometry_interleaved(scene.meshes.iter());
        scene.fill_geometry_and_material(meshes)?;

        // Creates a surface pack and sets it to the scene.
        let sels = scene.canvases.iter().map(|sel| sel.as_str());
        let index = render.add_surface_pack_from(sels);
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
        let mut builder = PipelineLayoutBuilderOld::new();
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
        let mut builder = PipelineBuilderOld::new();
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
            view.set_vertex_attribute(&[
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
        /* WIP
        builder.set_surface_pack_index(scene.surf_pack_index.clone());
        */
        // depth stencil
        let depth_stencil = wgpu::DepthStencilState {
            format: wgpu::TextureFormat::Depth24Plus, /* TODO: must be same with attachment */
            depth_write_enabled: true,
            depth_compare: wgpu::CompareFunction::Less,
            stencil: wgpu::StencilState::default(),
            bias: wgpu::DepthBiasState::default(),
        };
        builder.set_depth_stencil(depth_stencil);
        let pb_index = render.add_pipeline_builder(builder);
        let pipeline = render.build_pipeline(pb_index, key.clone());
        scene.pipelines.insert(key.clone(), Rc::clone(pipeline));

        // Removes temporary builders.
        render.remove_pipeline_layout_builder(plb_index);
        render.remove_pipeline_builder(pb_index);

        // PassGraph.
        scene.build_pass_graph(Arc::clone(key.label()));

        // Adds the scene.
        scene_mgr.insert_scene(key.clone(), scene);
        scene_mgr.activate_scene(key);
        Ok(())
        */
    }
}

impl Default for SceneManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Scene.  
/// It's roles are,  
/// - Generating render resources such as buffer, shader and pipeline.
/// - Synchronize with the render resources, means users modify scene nodes, that reflects on the GPU data.
///
/// Scene also corresponds to glTF scene.  
/// See https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#concepts
#[derive(Debug, Clone)]
pub struct Scene {
    /// Canvas selectors.
    pub(crate) canvases: Vec<String>,

    /// Explains node relationships.
    pub(crate) hierarchy: SceneHierarchy,

    /// Cameras in the scene.
    pub(crate) cameras: HashMap<ObjectKey, Camera, Hasher>,

    /// Camera buffers. They will be integrated at the end.
    pub(crate) camera_bufs: HashMap<ObjectKey, BufferRc, Hasher>,

    /// Camera binds. They will be integrated at the end.
    pub(crate) camera_binds: HashMap<ObjectKey, Rc<wgpu::BindGroup>, Hasher>,

    /// Meshes in the scene.
    /// This is a subset of the meshes in [`MeshResource`].
    pub(crate) meshes: HashSet<ObjectKey, Hasher>,

    pub(crate) mesh_map: HashMap<ObjectKey, (ObjectKey, ObjectKey), Hasher>,

    /// Owned materials in the scene.
    pub(crate) mats: HashMap<ObjectKey, Rc<()>, Hasher>,

    /// Material buffers. They will be integrated at the end.
    pub(crate) mat_bufs: HashMap<ObjectKey, BufferRc, Hasher>,

    /// Material binds. They will be integrated at the end.
    pub(crate) mat_binds: HashMap<ObjectKey, Rc<wgpu::BindGroup>, Hasher>,

    /// Instance buffer.
    pub(crate) inst_buf: Option<BufferRc>,

    /// Shader.
    pub(crate) shader: Option<Rc<Shader>>,

    /// Unified vertex and index buffer size.
    pub(crate) geo_buf_size: Option<(u64, u64)>,

    /// Pipelines. One pipeline for now.
    pub(crate) pipelines: HashMap<ObjectKey, Rc<wgpu::RenderPipeline>, Hasher>,

    pub(crate) pass_graph: Option<PassGraph>,

    pub(crate) gpu: Option<Rc<Gpu>>,
}

impl Scene {
    pub fn new() -> Self {
        Self {
            hierarchy: SceneHierarchy::new(),
            canvases: Vec::new(),
            cameras: HashMap::default(),
            camera_bufs: HashMap::default(),
            camera_binds: HashMap::default(),
            meshes: HashSet::default(),
            mesh_map: HashMap::default(),
            mats: HashMap::default(),
            mat_bufs: HashMap::default(),
            mat_binds: HashMap::default(),
            inst_buf: None,
            shader: None,
            geo_buf_size: None,
            pipelines: HashMap::default(),
            pass_graph: None,
            gpu: None,
        }
    }

    pub fn set_gpu(&mut self, gpu: Rc<Gpu>) {
        self.gpu = Some(gpu);
    }

    pub fn add_canvas(&mut self, selectors: &str) -> &mut Self {
        self.canvases.push(selectors.to_owned());
        self
    }

    pub fn add_node(&mut self, parent: usize) -> NodeReturn {
        let index = self.hierarchy.add_node(parent);
        NodeReturn {
            recv: self,
            ret: index,
        }
    }

    pub fn get_node(&self, index: usize) -> Option<&Node> {
        self.hierarchy.get_node(index)
    }

    pub fn get_node_mut(&mut self, index: usize) -> Option<&mut Node> {
        self.hierarchy.get_node_mut(index)
    }

    /// Updates global transformations and inserts write command to GPU command queue.
    pub fn update_global_transform(&mut self) {
        self.hierarchy.update_global_transform();
        let buf = self.inst_buf.as_ref().unwrap();
        let data = self.hierarchy.get_transform_buffer_as_bytes();
        let gpu = self.gpu.as_ref().unwrap();
        gpu.queue.write_buffer(buf, 0, data);
    }

    /* WIP
    // TODO: Allow multiple material and geometry layout?
    /// Fills empty geometries and materials with meshes from descriptor.
    /// Callers should guarantee that the mesh's geometires are the form of interleaved.
    /// Otherwise, it causes panic.
    ///
    /// Plus, this checks some assumptions, which could be removed in the future.
    /// Check-list is like something below
    ///
    /// - There's only one type of material.
    /// - All geometries have the same layout.
    pub fn fill_geometry_and_material(
        &mut self,
        mesh_res: &MeshResource,
    ) -> Result<(), SceneError> {
        // Check out if there's only one material variant.
        if let Some(mesh_key) = self.meshes.iter().next() {
            let first_var = mesh_res
                .get_mesh_material(mesh_key)
                .ok_or(SceneError::NotFoundResource)?
                .variant();
            for mesh_key in self.meshes.iter().skip(1) {
                let cur_var = mesh_res
                    .get_mesh_material(mesh_key)
                    .ok_or(SceneError::NotFoundResource)?
                    .variant();
                if cur_var != first_var {
                    let errmsg = String::from("only one type of material is allowed for now");
                    return Err(SceneError::InvalidScene(errmsg));
                }
            }
        }

        // Check out if all geometries have the same layout.
        if let Some(mesh_key) = self.meshes.iter().next() {
            let first_int_geo = mesh_res
                .get_mesh_geometry(mesh_key)
                .unwrap()
                .as_interleaved()
                .unwrap();
            for mesh_key in self.meshes.iter().skip(1) {
                let cur_int_geo = mesh_res
                    .get_mesh_geometry(mesh_key)
                    .unwrap()
                    .as_interleaved()
                    .unwrap();
                if cur_int_geo.attrs != first_int_geo.attrs
                    || cur_int_geo.index_format != first_int_geo.index_format
                {
                    let errmsg = String::from("only one layout of geometry is allowed for now");
                    return Err(SceneError::InvalidScene(errmsg));
                }
            }
        }

        // Fills geos and mats with mesh meta.
        for mesh_key in self.meshes.iter() {
            let (geo_key, geo_ref) = mesh_res
                .get_mesh_geometry_meta(mesh_key)
                .ok_or(SceneError::NotFoundResource)?;
            let (mat_key, mat_ref) = mesh_res
                .get_mesh_material_meta(mesh_key)
                .ok_or(SceneError::NotFoundResource)?;
            self.mesh_map
                .insert(mesh_key.clone(), (geo_key.clone(), mat_key.clone()));
            self.geos
                .insert(geo_key, GeometryBufferView::dummy(geo_ref));
            self.mats.insert(mat_key, mat_ref);
        }

        Ok(())
    }
    */

    /* WIP
    // TODO: consider WGSL padding
    pub fn create_shader_builder(
        &self,
        mesh_res: &MeshResource,
        surf_packs: &GenVecRc<SurfacePack>,
        surfaces: &GenVecRc<Surface>,
    ) -> my_wgsl::Builder {
        use my_wgsl::*;
        let mut builder = Builder::new();

        // Adds camera struct and its bind group.
        if let Some(camera) = self.cameras.values().next() {
            ShaderHelper::add_camera_struct(&mut builder, camera);
            ShaderHelper::add_camera_bind(
                &mut builder,
                Some(0),
                Some(0),
                Some(BindableResource::Uniform),
            );
        }

        // Adds material struct and its bind group.
        // All material types are the same.
        // See `Self::fill_geometry_and_material`.
        if let Some((mat_key, _)) = self.mats.iter().next() {
            let mat = mesh_res.get_material(mat_key).unwrap();
            ShaderHelper::add_material_struct(&mut builder, mat);
            ShaderHelper::add_material_bind(
                &mut builder,
                Some(1),
                Some(0),
                Some(BindableResource::Uniform),
            );
        }

        // Adds vertex input and output struct and vertex stage.
        if let Some((geo_key, _)) = self.geos.iter().next() {
            // Adds vertex input struct to the builder.
            let geo = mesh_res.get_geometry(geo_key).unwrap();
            ShaderHelper::add_vertex_input_struct(
                &mut builder,
                geo.as_interleaved().unwrap(),
                false,
                false,
            );

            // Adds vertex output struct to the builder.
            ShaderHelper::add_vertex_output_struct(&mut builder);

            // InstanceInput's locations starts from VertexInput's maximu location + 1.
            let st = unsafe {
                // Safety: Infallible.
                builder.get_structure("VertexInput").unwrap_unchecked()
            };
            let start_loc = st
                .members
                .iter()
                .filter_map(|member| {
                    member
                        .attrs
                        .find_attribute_partial("location", None)
                        .map(|i| member.attrs[i].inner_u32().unwrap() + 1)
                })
                .max()
                .unwrap_or_default();

            // Adds instance input struct to the builder.
            ShaderHelper::add_instance_input_struct(&mut builder, start_loc);

            // TODO: Improve me.
            let mut vert_stage = wgsl_decl_fn!(
                #[vertex]
                fn vert_stage(vert: VertexInput, inst: InstanceInput) -> VertexOutput {
                    // Assembles model matrix.
                    let model = mat4x4f(
                        inst.model0, inst.model1, inst.model2, inst.model3
                    );

                    var output: VertexOutput;
                    #[ID(camera)] {
                        output.position = camera.view_proj * model * vec4f(vert.position, 1.0);
                    }
                    #[ID(no_camera)] {
                        output.position = model * vec4f(vert.position, 1.0);
                    }
                    return output;
                }
            );
            if builder.has_structure("Camera") {
                vert_stage.remove_statement("ID", Some("no_camera"));
            } else {
                vert_stage.remove_statement("ID", Some("camera"));
            }
            builder.push_function(vert_stage);
        }

        // Adds fragment output struct and fragment stage.
        if !self.surf_pack_index.is_dummy() {
            let color_targets = surf_packs
                .get(self.surf_pack_index.index)
                .unwrap()
                .create_color_targets(surfaces);
            ShaderHelper::add_fragment_output_struct(&mut builder, &color_targets);

            // TODO: Improve me.
            let mut frag_stage = wgsl_decl_fn!(
                #[fragment]
                fn frag_stage(input: VertexOutput) -> FragmentOutput {
                    var output: FragmentOutput;
                }
            );
            if let Some(st) = builder.get_structure("FragmentOutput") {
                for member in st.members.iter() {
                    let mut stmt = "output.".to_owned();
                    stmt.push_str(&member.ident);
                    // TODO: hard coded.
                    stmt.push_str("=vec4f(material.color,1.0);");
                    frag_stage.append_statement(stmt);
                }
            }
            frag_stage.append_statement("return output;".to_owned());
            builder.push_function(frag_stage);
        }

        builder
    }
    */

    pub fn set_shader(&mut self, shader: Rc<Shader>) {
        self.shader = Some(shader);
    }

    /* WIP
    pub fn iter_geometry<'a>(
        &'a self,
        mesh_res: &'a MeshResource,
    ) -> impl Iterator<Item = &'a Geometry> {
        self.geos
            .keys()
            .map(|geo_key| &**mesh_res.get_geometry(geo_key).unwrap())
    }
    */

    /* WIP
    /// Calculates vertex and index buffer sizes for all geometries in the scene.
    /// Each geometry has it's offset and length, so it can be written appropriately.
    /// Also, they are located with alignment required by WebGPU (multiple of 4 bytes).
    pub fn calc_geometry_buffer_size(&mut self, mesh_res: &MeshResource) -> (u64, u64) {
        // Calculate size for vertices.
        let geo_buf_size = self
            .iter_geometry(mesh_res)
            .map(|geo| {
                let int_geo = geo.as_interleaved().unwrap();
                let vert_padded_num =
                    buffer::to_padded_num(int_geo.vertex_size, int_geo.vertex_num) as u64;
                let index_size = match int_geo.index_format {
                    wgpu::IndexFormat::Uint16 => 2,
                    wgpu::IndexFormat::Uint32 => 4,
                };
                let index_padded_num = buffer::to_padded_num(index_size, int_geo.index_num) as u64;
                (
                    int_geo.vertex_size as u64 * vert_padded_num,
                    index_size as u64 * index_padded_num,
                )
            })
            .fold((0, 0), |mut acc, x| {
                acc.0 += x.0;
                acc.1 += x.1;
                acc
            });

        // Remembers the size.
        // This is used to validate buffer, which is set in the future, is enough.
        self.geo_buf_size = Some(geo_buf_size);

        geo_buf_size
    }
    */

    /// Validates and determines transformation matrix buffer view for instancing.
    pub fn get_transform_buffer_view(&mut self) -> View {
        self.hierarchy.validate();
        self.hierarchy.get_transform_buffer_view()
    }

    /* WIP
    /// Sets vertex and index buffers and writes geometres to the buffers immdiately.
    /// Then, the buffers are unmapped.
    /// Caller should give enough buffers to write geometries.
    ///
    /// # Panics
    ///
    /// Panics if `buf`'s size is less than calculated size by [`Self::calc_geometry_buffer_size`].
    pub fn set_geometry_buffer(
        &mut self,
        mesh_res: &MeshResource,
        vert_buf: BufferRc,
        index_buf: BufferRc,
    ) {
        // Validates the buffer size.
        let (need_vert_size, need_index_size) = self
            .geo_buf_size
            .unwrap_or(self.calc_geometry_buffer_size(mesh_res));
        assert!(need_vert_size <= vert_buf.size());
        assert!(need_index_size <= index_buf.size());

        // Writes each geometry.
        let mut vert_offset: usize = 0; // Offest in indices.
        let mut index_offset: usize = 0; // Offset in indices.
        for (geo, view) in self
            .geos
            .iter_mut()
            .map(|(geo_key, view)| (&**mesh_res.get_geometry(geo_key).unwrap(), view))
        {
            let int_geo = geo.as_interleaved().unwrap();
            let vert_bytes = int_geo.as_bytes(0);
            let index_bytes = int_geo.as_bytes(1);

            // Writes to the buffer.
            let vert_offset_bytes = vert_offset * int_geo.vertex_size;
            let index_offset_bytes = vert_offset * int_geo.vertex_size;
            buffer::write_to_mapped_buffer(&vert_buf, vert_bytes, vert_offset_bytes);
            buffer::write_to_mapped_buffer(&index_buf, index_bytes, index_offset_bytes);

            // Updates vertex buffer view for this geometry.
            let mut vert_buf_view =
                BufferView::new(int_geo.vertex_size, int_geo.vertex_num, vert_offset);
            vert_buf_view.set_buffer(Rc::clone(&vert_buf));
            vert_buf_view.set_vertex_attribute(&int_geo.attrs);
            view.set_vertex_buffer_view(vert_buf_view);

            // Updates index buffer view for this geometry.
            let index_size: usize = match int_geo.index_format {
                wgpu::IndexFormat::Uint16 => 2,
                wgpu::IndexFormat::Uint32 => 4,
            };
            let mut index_buf_view = BufferView::new(index_size, int_geo.index_num, index_offset);
            index_buf_view.set_buffer(Rc::clone(&index_buf));
            view.set_index_buffer_view(index_buf_view);

            // Increases offsets by padded numbers.
            // Because we can access to only the aligned address to the buffer.
            vert_offset += buffer::to_padded_num(int_geo.vertex_size, int_geo.vertex_num);
            index_offset += buffer::to_padded_num(index_size, int_geo.index_num);
        }

        // Unmaps the buffer.
        vert_buf.unmap();
        index_buf.unmap();
    }
    */

    pub fn set_instance_buffer(&mut self, buf: BufferRc) {
        self.inst_buf = Some(buf);
    }

    // TODO: Improve me and sync with shader.
    /// Builds pass graph and returns old value.
    pub fn build_pass_graph(&mut self, label: Arc<str>) -> Option<PassGraph> {
        todo!()
        /* WIP
        const INVALID: usize = usize::MAX;
        let mut pass_graph = PassGraph::new(Arc::clone(&label), self.gpu.as_ref().unwrap());

        // Uses only one camera for now.
        let cam_cmd = if let Some(cam) = self.camera_binds.values().next() {
            // TODO: Something like group index can be shared across resources.
            let cmd = SetBindGroupCmd::new(0, Rc::clone(cam));
            pass_graph.insert_command(cmd)
        } else {
            INVALID
        };

        let (vert_cmd, index_cmd) = if let Some(geo_buf_view) = self.geos.values().next() {
            // Vertex
            let vert_buf = geo_buf_view.get_vertex_buffer();
            let vert_cmd = SetVertexBufferCmd::new(Rc::clone(vert_buf), 0, 0, 0);
            let vert_cmd = pass_graph.insert_command(vert_cmd);
            // Index
            let index_buf_view = geo_buf_view.get_index_buffer_view();
            let index_cmd = SetIndexBufferCmd::new(
                Rc::clone(index_buf_view.get_buffer()),
                0,
                0,
                index_buf_view.as_index_format(),
            );
            let index_cmd = pass_graph.insert_command(index_cmd);
            (vert_cmd, index_cmd)
        } else {
            (INVALID, INVALID)
        };

        // Instance
        let inst_cmd = if let Some(inst_buf) = self.inst_buf.as_ref() {
            let inst_cmd = SetVertexBufferCmd::new(Rc::clone(inst_buf), 0, 0, 1);
            pass_graph.insert_command(inst_cmd)
        } else {
            INVALID
        };

        // Uses only one pipeline for now.
        let pipe_cmd = if let Some(pipe) = self.pipelines.values().next() {
            let cmd = SetPipelineCmd::new(Rc::clone(pipe));
            pass_graph.insert_command(cmd)
        } else {
            INVALID
        };

        // Material binds
        let mut mat_cmds = HashMap::new();
        for (mat_key, mat_bind) in self.mat_binds.iter() {
            let cmd = SetBindGroupCmd::new(1, Rc::clone(mat_bind));
            let mat_cmd = pass_graph.insert_command(cmd);
            mat_cmds.insert(mat_key.clone(), mat_cmd);
        }

        let mut pass = RenderPass::new(label, Some(self.surf_pack_index.clone()));

        // Draw call per mesh.
        for (mesh_key, (geo_key, mat_key)) in self.mesh_map.iter() {
            let geo_view = self.geos.get(geo_key).unwrap();
            let vert_view = geo_view.get_vertex_buffer_view();
            let index_view = geo_view.get_index_buffer_view();

            let base_vertex = vert_view.get_index_offset() as i32;
            let start_index = index_view.get_index_offset() as u32;
            let end_index = start_index + index_view.get_item_num() as u32;

            let inst_range = self.hierarchy.get_transform_buffer_range(mesh_key).unwrap();

            let cmd = DrawIndexedCmd::new(
                start_index..end_index,
                base_vertex,
                inst_range.start as u32..inst_range.end as u32,
            );
            let draw_cmd = pass_graph.insert_command(cmd);

            if cam_cmd != INVALID {
                pass_graph.add_dependency(draw_cmd, cam_cmd);
            }
            if vert_cmd != INVALID {
                pass_graph.add_dependency(draw_cmd, vert_cmd);
            }
            if index_cmd != INVALID {
                pass_graph.add_dependency(draw_cmd, index_cmd);
            }
            if inst_cmd != INVALID {
                pass_graph.add_dependency(draw_cmd, inst_cmd);
            }
            if pipe_cmd != INVALID {
                pass_graph.add_dependency(draw_cmd, pipe_cmd);
            }
            let mat_cmd = *mat_cmds.get(mat_key).unwrap();
            pass_graph.add_dependency(draw_cmd, mat_cmd);

            pass.add_draw_command(draw_cmd);
        }

        pass_graph.add_pass(Pass::from(pass));
        pass_graph.validate().unwrap();

        self.set_pass_graph(pass_graph)
        */
    }

    pub fn set_pass_graph(&mut self, pass_graph: PassGraph) -> Option<PassGraph> {
        self.pass_graph.replace(pass_graph)
    }

    /* WIP
    /// Runs the pass graph.
    pub fn run(
        &self,
        surf_packs: &GenVecRc<SurfacePack>,
        surf_pack_bufs: &mut Vec<SurfacePackBuffer>,
        surfaces: &GenVecRc<Surface>,
        visit_buf: &mut Vec<bool>,
    ) {
        if let Some(pass_graph) = &self.pass_graph {
            pass_graph.run(surf_packs, surf_pack_bufs, surfaces, visit_buf);
        }
    }
    */
}

impl Default for Scene {
    fn default() -> Self {
        Self::new()
    }
}

decl_return_wrap!(NodeReturn, Scene, usize);

impl<'a> NodeReturn<'a> {
    pub fn with_camera(self, key: impl Into<ObjectKey>, camera: impl Into<Camera>) -> Self {
        self._with_camera(key.into(), camera.into())
    }

    fn _with_camera(self, key: ObjectKey, camera: Camera) -> Self {
        let Self {
            recv: scene,
            ret: node_index,
        } = self;

        // Adds camera to the scene.
        scene.cameras.insert(key.clone(), camera);

        // Sets the camera at the node.
        scene
            .hierarchy
            .get_node_mut(node_index)
            .unwrap()
            .set_camera(key);

        Self {
            recv: scene,
            ret: node_index,
        }
    }

    /// Sets mesh to the current node.
    pub fn with_mesh(self, key: impl Into<ObjectKey>) -> Self {
        self._with_mesh(key.into())
    }

    fn _with_mesh(self, key: ObjectKey) -> Self {
        let Self {
            recv: scene,
            ret: node_index,
        } = self;

        // Adds mesh to the scene.
        scene.meshes.insert(key.clone());

        // Sets mesh at the node.
        scene
            .hierarchy
            .get_node_mut(node_index)
            .unwrap()
            .set_mesh(key);

        Self {
            recv: scene,
            ret: node_index,
        }
    }

    pub fn with_entity(self, eid: EntityId) -> Self {
        let Self {
            recv: scene,
            ret: node_index,
        } = self;

        // Adds mapping between ECS's entity and scene's node.
        scene
            .hierarchy
            .get_node_mut(node_index)
            .unwrap()
            .set_entity(eid);

        Self {
            recv: scene,
            ret: node_index,
        }
    }
}
