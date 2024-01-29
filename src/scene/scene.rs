use crate::{
    decl_return_wrap,
    ds::generational::{GenIndexRc, GenVecRc},
    primitive::{
        camera::Camera,
        matrix::Matrix4f,
        mesh::{Geometry, MeshResource},
    },
    render::{
        buffer::{to_aligned_size, write_to_mapped_buffer},
        canvas::{Surface, SurfacePack},
        shaders::{
            helper::{BindableResource, ShaderHelper},
            Shader,
        },
    },
    scene::SceneError,
    util::{AsMultiBytes, key::ResKey},
};
use ahash::{AHashMap, AHashSet};
use std::rc::Rc;

/// Scene graph.  
/// Scene graph describes all render relative information on CPU side, but it doesn't own real resources.
/// It's roles are,  
/// - Generating render resources such as buffer, shader and pipeline.
/// - Synchronize with the render resources, means users modify scene nodes, that reflects on the GPU data.
///
/// The scene graph also corresponds to glTF scene.  
/// See https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#concepts
#[derive(Debug, Clone)]
pub struct Scene {
    /// Canvas selectors.
    pub(crate) canvases: Vec<Rc<str>>,

    /// All nodes in the scene.
    pub(crate) nodes: Vec<Node>,

    /// Cameras in the scene.
    pub(crate) cameras: AHashMap<ResKey, Camera>,

    /// Camera buffers. They will be integrated at the end.
    pub(crate) camera_bufs: AHashMap<ResKey, Rc<wgpu::Buffer>>,

    /// Camera binds. They will be integrated at the end.
    pub(crate) camera_binds: AHashMap<ResKey, Rc<wgpu::BindGroup>>,

    /// Meshes in the scene.
    pub(crate) meshes: AHashSet<ResKey>,

    /// Owned geometries in the scene.
    pub(crate) geos: AHashMap<ResKey, GeometryView>,

    /// Owned materials in the scene.
    pub(crate) mats: AHashMap<ResKey, Rc<()>>,

    /// Material buffers. They will be integrated at the end.
    pub(crate) mat_bufs: AHashMap<ResKey, Rc<wgpu::Buffer>>,

    /// Material binds. They will be integrated at the end.
    pub(crate) mat_binds: AHashMap<ResKey, Rc<wgpu::BindGroup>>,

    /// Surface pack index.
    pub(crate) surf_pack_index: GenIndexRc,

    /// Shader.
    pub(crate) shader: Option<Rc<Shader>>,

    /// Unified vertex and index buffer size.
    pub(crate) geo_buf_size: Option<(u64, u64)>,

    /// Unified vertex buffer for now, currently it's limited to have various geometry layouts.
    /// It can be changed to have multi-buffers in the future.
    pub(crate) vert_buf: Option<Rc<wgpu::Buffer>>,

    /// Unified index buffer for now, currently it's limited to have various geometry layouts.
    /// It can be changed to have multi-buffers in the future.
    pub(crate) index_buf: Option<Rc<wgpu::Buffer>>,

    // Pipelines. There's only one pipeline for now.
    // pub(crate) pipelines: 
}

impl Scene {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            cameras: AHashMap::new(),
            camera_bufs: AHashMap::new(),
            camera_binds: AHashMap::new(),
            meshes: AHashSet::new(),
            canvases: Vec::new(),
            geos: AHashMap::new(),
            mats: AHashMap::new(),
            mat_bufs: AHashMap::new(),
            mat_binds: AHashMap::new(),
            surf_pack_index: GenIndexRc::dummy(),
            shader: None,
            geo_buf_size: None,
            vert_buf: None,
            index_buf: None,
        }
    }

    pub fn add_canvas(&mut self, selectors: impl Into<Rc<str>>) -> &mut Self {
        self.canvases.push(selectors.into());
        self
    }

    pub fn add_node(&mut self, parent: Option<usize>) -> NodeReturn {
        self.nodes.push(Node::new());
        let ret = self.nodes.len() - 1;
        if let Some(parent) = parent {
            self.nodes[parent].children.insert(ret);
        }
        NodeReturn { recv: self, ret }
    }

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
    pub fn fill_geometry_and_material<'a>(
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
                    .get_material(mesh_key)
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
                    .get_geometry(mesh_key)
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

        // Fills geos and mats with newly appended mesh meta.
        for mesh_key in self.meshes.iter() {
            let (geo_key, geo_ref) = mesh_res
                .get_mesh_geometry_meta(mesh_key)
                .ok_or(SceneError::NotFoundResource)?;
            let (mat_key, mat_ref) = mesh_res
                .get_mesh_material_meta(mesh_key)
                .ok_or(SceneError::NotFoundResource)?;
            self.geos.insert(geo_key, GeometryView::new(geo_ref));
            self.mats.insert(mat_key, mat_ref);
        }

        Ok(())
    }

    pub fn set_surface_pack_index(&mut self, index: GenIndexRc) {
        self.surf_pack_index = index;
    }

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
            let geo = mesh_res.get_geometry(geo_key).unwrap();
            ShaderHelper::add_vertex_input_struct(
                &mut builder,
                geo.as_interleaved().unwrap(),
                false,
                false,
            );
            ShaderHelper::add_vertex_output_struct(&mut builder);

            // TODO: Improve me.
            let mut vert_stage = wgsl_decl_fn!(
                #[vertex]
                fn vert_stage(input: VertexInput) -> VertexOutput {
                    var output: VertexOutput;
                    #[ID(camera)] {
                        output.position = camera.view_proj * vec4f(input.position, 1.0);
                    }
                    #[ID(no_camera)] {
                        output.position = vec4f(input.position, 1.0);
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
                    stmt.push_str("= vec4f(material.color, 1.0);");
                    frag_stage.append_statement(stmt);
                }
            }
            frag_stage.append_statement("return output;".to_owned());
            builder.push_function(frag_stage);
        }

        builder
    }

    pub fn set_shader(&mut self, shader: Rc<Shader>) {
        self.shader = Some(shader);
    }

    pub fn iter_geometry<'a>(
        &'a self,
        mesh_res: &'a MeshResource,
    ) -> impl Iterator<Item = &'a Geometry> {
        self.geos
            .keys()
            .map(|geo_key| &**mesh_res.get_geometry(geo_key).unwrap())
    }

    /// Calculates vertex and index buffer sizes for all geometries in the scene.
    /// Each geometry has it's offset and length, so it can be written appropriately.
    /// Also, they are located with alignment required by wgpu (multiple of 4 bytes).
    pub fn calc_geometry_buffer_size(&mut self, mesh_res: &MeshResource) -> (u64, u64) {
        // Calculate size for vertices.
        let geo_buf_size = self
            .iter_geometry(mesh_res)
            .map(|geo| {
                let int_geo = geo.as_interleaved().unwrap();
                let vert_size = int_geo.as_bytes(0).len();
                let index_size = int_geo.as_bytes(1).len();
                (to_aligned_size(vert_size as u64), to_aligned_size(index_size as u64))
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

    /// Sets vertex and index buffers and writes geometres to the buffers immdiately.
    /// Then, the buffers are unmapped.
    /// Caller should give enough buffers to write geometries.
    ///
    /// # Panics
    ///
    /// Panics if `buf`'s size is less than calculated size by [`Self::calc_geometry_buffer_size`].
    pub fn set_geometry_buffer(&mut self, mesh_res: &MeshResource, vert_buf: Rc<wgpu::Buffer>, index_buf: Rc<wgpu::Buffer>) {
        // Validates the buffer size.
        let (need_vert_size, need_index_size) = self
            .geo_buf_size
            .unwrap_or(self.calc_geometry_buffer_size(mesh_res));
        assert!(need_vert_size <= vert_buf.size());
        assert!(need_index_size <= index_buf.size());

        // Writes each geometry.
        let (mut vert_offset, mut index_offset) = (0, 0);
        for (geo, view) in self
            .geos
            .iter_mut()
            .map(|(geo_key, view)| (&**mesh_res.get_geometry(geo_key).unwrap(), view))
        {
            let int_geo = geo.as_interleaved().unwrap();
            let vert_bytes = int_geo.as_bytes(0);
            let index_bytes = int_geo.as_bytes(1);

            // Writes to the buffer.
            write_to_mapped_buffer(&vert_buf, vert_bytes, vert_offset);
            write_to_mapped_buffer(&index_buf, index_bytes, index_offset);

            // Updates geometry view of the buffer.
            view.vert_offset = vert_offset;
            view.vert_size = to_aligned_size(vert_bytes.len() as u64);
            view.index_offset = index_offset;
            view.index_size = to_aligned_size(index_bytes.len() as u64);

            // Prepares for the next.
            vert_offset += view.vert_size as usize;
            index_offset += view.index_size as usize;
        }

        // Unmaps the buffer.
        vert_buf.unmap();
        index_buf.unmap();

        self.vert_buf = Some(vert_buf);
        self.index_buf = Some(index_buf);
    }
}

decl_return_wrap!(NodeReturn, Scene, usize);

impl<'a> NodeReturn<'a> {
    /// Sets transform matrix to the current node.
    pub fn with_transform(self, transform: Matrix4f) -> Self {
        let Self {
            recv: desc,
            ret: node_index,
        } = self;

        // Sets the transform matrix at the node.
        desc.nodes[node_index].transform = transform;

        Self {
            recv: desc,
            ret: node_index,
        }
    }

    pub fn with_camera(self, key: impl Into<ResKey>, camera: impl Into<Camera>) -> Self {
        let Self {
            recv: desc,
            ret: node_index,
        } = self;

        // Adds camera to the scene descriptor.
        let key = key.into();
        desc.cameras.insert(key.clone(), camera.into());

        // Sets the camera at the node.
        desc.nodes[node_index].camera = Some(key);

        Self {
            recv: desc,
            ret: node_index,
        }
    }

    /// Sets mesh to the current node.
    pub fn with_mesh(self, key: impl Into<ResKey>) -> Self {
        let Self {
            recv: desc,
            ret: node_index,
        } = self;

        // Adds mesh to the scene descriptor.
        let key = key.into();
        desc.meshes.insert(key.clone());

        // Sets mesh at the node.
        desc.nodes[node_index].mesh = Some(key);

        Self {
            recv: desc,
            ret: node_index,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Node {
    children: AHashSet<usize>,

    // TODO: transform should be in components array.
    // Scene doesn't have actual resource or component except camera.
    transform: Matrix4f,
    camera: Option<ResKey>,
    mesh: Option<ResKey>,
}

impl Node {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Default for Node {
    fn default() -> Self {
        Self {
            children: AHashSet::new(),
            transform: Matrix4f::identity(),
            camera: None,
            mesh: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct GeometryView {
    /// Offset in bytes to the unified vertex buffer.
    pub vert_offset: usize,

    /// Total vertex size in bytes.
    pub vert_size: u64,

    /// Offset in bytes to the unified index buffer.
    pub index_offset: usize,

    /// Total index size in bytes.
    pub index_size: u64,

    /// Geometry resource reference to own it.
    pub geo_ref: Rc<()>,
}

impl GeometryView {
    pub fn new(geo_ref: Rc<()>) -> Self {
        Self {
            vert_offset: 0, // Dummy
            vert_size: 0, // Dummy
            index_offset: 0, // Dummy
            index_size: 0, // Dummy
            geo_ref,
        }
    }
}
