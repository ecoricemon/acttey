use crate::{
    primitive::{
        camera::Camera,
        mesh::{InterleavedGeometry, Material, VertexAttributeVariant},
    },
    util::trim_end_digits,
};
use my_wgsl::*;
use smallvec::SmallVec;

/// Helper to create shader items.
/// This doesn't have any fields and methods, but associated functions.
pub struct ShaderHelper;

impl ShaderHelper {
    // `camera` is not used for now because cameras always have the same view_proj.
    // but, it can change in the future, so that I left camera parameter.
    /// Adds `Camera` struct to the `builder`.
    pub fn add_camera_struct(builder: &mut Builder, _camera: &Camera) {
        #[wgsl_decl_struct]
        struct Camera {
            view_proj: mat4x4<f32>,
        }
        wgsl_structs!(builder, Camera);
    }

    /// Adds global variable `camera` with the given group, binding
    /// and `Camera` type to the `builder`.
    ///
    /// If `group` is not given, `group(0)` is used as default.  
    /// If `binding` is not given, `binding(0)` is used as default.  
    /// If `res` is not given, `uniform` is used as default.  
    pub fn add_camera_bind(
        builder: &mut Builder,
        group: Option<u32>,
        binding: Option<u32>,
        res: Option<BindableResource>,
    ) {
        let group = group.unwrap_or(0);
        let binding = binding.unwrap_or(0);
        let res = res.unwrap_or(BindableResource::Uniform);

        // Dummy
        #[wgsl_decl_struct]
        struct Camera;

        match res {
            BindableResource::Uniform => wgsl_bind!(
                builder, group(group) binding(binding) var<uniform> camera: Camera
            ),
            BindableResource::Storage => wgsl_bind!(
                builder, group(group) binding(binding) var<storage> camera: Camera
            ),
            BindableResource::Texture | BindableResource::Sampler => panic!(),
        }
    }

    /// Adds `Material` struct to the `builder`.
    pub fn add_material_struct(builder: &mut Builder, mat: &Material) {
        match mat {
            Material::Simple(..) => {
                #[wgsl_decl_struct]
                struct Material {
                    color: vec3<f32>,
                }
                wgsl_structs!(builder, Material);
            }
        }
    }

    /// Adds global variable `material` with the given group, binding
    /// and `Material` type to the `builder`.
    ///
    /// If `group` is not given, `group(1)` is used as default.  
    /// If `binding` is not given, `binding(0)` is used as default.  
    /// If `res` is not given, `uniform` is used as default.  
    pub fn add_material_bind(
        builder: &mut Builder,
        group: Option<u32>,
        binding: Option<u32>,
        res: Option<BindableResource>,
    ) {
        let group = group.unwrap_or(1);
        let binding = binding.unwrap_or(0);
        let res = res.unwrap_or(BindableResource::Uniform);

        // Dummy
        #[wgsl_decl_struct]
        struct Material;

        match res {
            BindableResource::Uniform => wgsl_bind!(
                builder, group(group) binding(binding) var<uniform> material: Material
            ),
            BindableResource::Storage => wgsl_bind!(
                builder, group(group) binding(binding) var<storage> material: Material
            ),
            BindableResource::Texture | BindableResource::Sampler => panic!(),
        }
    }

    /// Adds `VertexInput` struct to the `builder`.  
    ///
    /// if `vert_index` is true, `@builtin(vertex_index) vertexIndex` is appended.  
    /// if `inst_index` is true, `@builtin(instance_index) instanceIndex` is appended.  
    pub fn add_vertex_input_struct(
        builder: &mut Builder,
        geo: &InterleavedGeometry,
        vert_index: bool,
        inst_index: bool,
    ) {
        let structure = Self::create_vertex_input_struct(
            geo.attrs.iter(),
            geo.attr_kinds.iter(),
            vert_index,
            inst_index,
        );
        builder.push_structure(structure);
    }

    /// Adds `VertexOutput` struct to the `builder`.
    /// For now, this is fixed struct.
    pub fn add_vertex_output_struct(builder: &mut Builder) {
        #[wgsl_decl_struct]
        struct VertexOutput {
            // Mandatory member.
            #[builtin(position)]
            position: vec4<f32>,
        }
        wgsl_structs!(builder, VertexOutput);
    }

    /// Adds `InstanceInput` struct to the `builder`.
    pub fn add_instance_input_struct(builder: &mut Builder, start_loc: u32) {
        #[wgsl_decl_struct]
        struct InstanceInput {}
        let mut st = <InstanceInput as AsStructure>::as_structure();

        // Inserts model matrix members into the structure.
        for loc in start_loc..start_loc + 4 {
            // Creates structure member looks like `@location(x) model0: vec4<f32>`
            let ident = format!("model{}", loc - start_loc);
            let ty = "vec4f".to_string();
            let loc = loc.to_string();
            let mut member = StructureMember::new(ident, ty);
            member.insert_attribute("location", Some(loc.as_str()));
            st.members.push(member);
        }

        builder.push_structure(st);
    }

    /// Adds `FragmentOutput` struct to the `builder`.
    /// You can get `colors` from [`SurfacePack::create_color_targets()`].
    pub fn add_fragment_output_struct(
        builder: &mut Builder,
        color_targets: &[Option<wgpu::ColorTargetState>],
    ) {
        #[wgsl_decl_struct]
        struct FragmentOutput {}
        let mut st = <FragmentOutput as AsStructure>::as_structure();

        // Inserts color members into the structure.
        for (i, color) in color_targets.iter().enumerate() {
            if let Some(color) = color {
                // Creates structure member looks like `@location(0) color0: vec4<f32>`.
                let i = i.to_string();
                let mut ident = "color".to_owned();
                ident.push_str(&i);
                let ty = Self::texture_format_to_shader_str(color.format).to_owned();
                let mut member = StructureMember::new(ident, ty);
                member.insert_attribute("location", Some(i.as_str()));
                st.members.push(member);
            }
        }

        // If there's only one member, trims number from the member ident.
        if st.members.len() == 1 {
            trim_end_digits(&mut st.members[0].ident);
        }

        builder.push_structure(st);
    }

    /// Converts [`wgpu::VertexFormat`] to type string used in shader.
    fn vertex_format_to_shader_str(format: wgpu::VertexFormat) -> &'static str {
        use wgpu::VertexFormat;

        // All comments are copied from wgpu.
        match format {
            // Two unsigned bytes (u8). `vec2<u32>` in shaders.
            VertexFormat::Uint8x2 => "vec2u",
            // Four unsigned bytes (u8). `vec4<u32>` in shaders.
            VertexFormat::Uint8x4 => "vec4u",
            // Two signed bytes (i8). `vec2<i32>` in shaders.
            VertexFormat::Sint8x2 => "vec2i",
            // Four signed bytes (i8). `vec4<i32>` in shaders.
            VertexFormat::Sint8x4 => "vec4i",
            // Two unsigned bytes (u8). [0, 255] converted to float [0, 1] `vec2<f32>` in shaders.
            VertexFormat::Unorm8x2 => "vec2f",
            // Four unsigned bytes (u8). [0, 255] converted to float [0, 1] `vec4<f32>` in shaders.
            VertexFormat::Unorm8x4 => "vec4f",
            // Two signed bytes (i8). [-127, 127] converted to float [-1, 1] `vec2<f32>` in shaders.
            VertexFormat::Snorm8x2 => "vec2f",
            // Four signed bytes (i8). [-127, 127] converted to float [-1, 1] `vec4<f32>` in shaders.
            VertexFormat::Snorm8x4 => "vec4f",
            // Two unsigned shorts (u16). `vec2<u32>` in shaders.
            VertexFormat::Uint16x2 => "vec2u",
            // Four unsigned shorts (u16). `vec4<u32>` in shaders.
            VertexFormat::Uint16x4 => "vec4u",
            // Two signed shorts (i16). `vec2<i32>` in shaders.
            VertexFormat::Sint16x2 => "vec2i",
            // Four signed shorts (i16). `vec4<i32>` in shaders.
            VertexFormat::Sint16x4 => "vec4i",
            // Two unsigned shorts (u16). [0, 65535] converted to float [0, 1] `vec2<f32>` in shaders.
            VertexFormat::Unorm16x2 => "vec2f",
            // Four unsigned shorts (u16). [0, 65535] converted to float [0, 1] `vec4<f32>` in shaders.
            VertexFormat::Unorm16x4 => "vec4f",
            // Two signed shorts (i16). [-32767, 32767] converted to float [-1, 1] `vec2<f32>` in shaders.
            VertexFormat::Snorm16x2 => "vec2f",
            // Four signed shorts (i16). [-32767, 32767] converted to float [-1, 1] `vec4<f32>` in shaders.
            VertexFormat::Snorm16x4 => "vec4f",
            // Two half-precision floats (no Rust equiv). `vec2<f32>` in shaders.
            VertexFormat::Float16x2 => "vec2f",
            // Four half-precision floats (no Rust equiv). `vec4<f32>` in shaders.
            VertexFormat::Float16x4 => "vec4f",
            // One single-precision float (f32). `f32` in shaders.
            VertexFormat::Float32 => "f32",
            // Two single-precision floats (f32). `vec2<f32>` in shaders.
            VertexFormat::Float32x2 => "vec2f",
            // Three single-precision floats (f32). `vec3<f32>` in shaders.
            VertexFormat::Float32x3 => "vec3f",
            // Four single-precision floats (f32). `vec4<f32>` in shaders.
            VertexFormat::Float32x4 => "vec4f",
            // One unsigned int (u32). `u32` in shaders.
            VertexFormat::Uint32 => "u32",
            // Two unsigned ints (u32). `vec2<u32>` in shaders.
            VertexFormat::Uint32x2 => "vec2u",
            // Three unsigned ints (u32). `vec3<u32>` in shaders.
            VertexFormat::Uint32x3 => "vec3u",
            // Four unsigned ints (u32). `vec4<u32>` in shaders.
            VertexFormat::Uint32x4 => "vec4u",
            // One signed int (i32). `i32` in shaders.
            VertexFormat::Sint32 => "i32",
            // Two signed ints (i32). `vec2<i32>` in shaders.
            VertexFormat::Sint32x2 => "vec2i",
            // Three signed ints (i32). `vec3<i32>` in shaders.
            VertexFormat::Sint32x3 => "vec3i",
            // Four signed ints (i32). `vec4<i32>` in shaders.
            VertexFormat::Sint32x4 => "vec4i",
            VertexFormat::Float64
            | VertexFormat::Float64x2
            | VertexFormat::Float64x3
            | VertexFormat::Float64x4 => panic!("unsupported format"),
        }
    }

    /// Converts [`wgpu::TextureFormat`] to type string used in shader.
    /// 6.5.1. Texel Formats
    /// See https://www.w3.org/TR/WGSL/#texel-formats
    fn texture_format_to_shader_str(format: wgpu::TextureFormat) -> &'static str {
        use wgpu::TextureSampleType;

        if let Some(_type) = format.sample_type(None) {
            match _type {
                TextureSampleType::Float { .. } => return "vec4f",
                TextureSampleType::Uint => return "vec4u",
                TextureSampleType::Sint => return "vec4i",
                TextureSampleType::Depth => (),
            }
        }
        panic!("unsupported type");
    }

    /// Creates [`Structure`](my_wgsl::Structure) from the given attributes,
    /// which can be obtained from [`InterleavedGeometry`](crate::primitive::mesh::InterleavedGeometry).
    /// `attrs` and `attr_kinds` should have the exactly same length.j
    fn create_vertex_input_struct<'a>(
        mut attrs: impl Iterator<Item = &'a wgpu::VertexAttribute>,
        mut attr_kinds: impl Iterator<Item = &'a VertexAttributeVariant>,
        builtin_vert_index: bool,
        builtin_inst_index: bool,
    ) -> my_wgsl::Structure {
        // Creates a structure.
        let mut st = Structure {
            ident: "VertexInput".to_owned(),
            members: SmallVec::new(),
        };

        // Adds built-in attributes.
        let mut add_builtin = |ident: String, value: BuiltinValue| {
            let mut attrs = Attributes::new();
            attrs.0.push(Attribute::Builtin(value));
            st.members.push(StructureMember {
                attrs,
                ident,
                ty: "u32".to_owned(),
            });
        };
        if builtin_vert_index {
            add_builtin("vertexIndex".to_owned(), BuiltinValue::VertexIndex);
        }
        if builtin_inst_index {
            add_builtin("instanceIndex".to_owned(), BuiltinValue::InstanceIndex);
        }

        // Adds slot attributes.
        let mut add_slot = |ident: String, loc: u32, format: wgpu::VertexFormat| {
            let mut attrs = Attributes::new();
            attrs.0.push(Attribute::Location(loc));
            st.members.push(StructureMember {
                attrs,
                ident,
                ty: Self::vertex_format_to_shader_str(format).to_owned(),
            });
        };
        let mut uv_index = 0;
        let mut color_index = 0;
        let mut joint_index = 0;
        let mut weight_index = 0;
        let mut user_a_index = 0;
        let mut user_b_index = 0;
        let mut user_c_index = 0;
        let mut user_d_index = 0;
        const UV_IDENTS: [&str; 4] = ["uv_0", "uv_1", "uv_2", "uv_3"];
        const COLOR_IDENTS: [&str; 4] = ["color_0", "color_1", "color_2", "color_3"];
        const JOINT_IDENTS: [&str; 4] = ["joint_0", "joint_1", "joint_2", "joint_3"];
        const WEIGHT_IDENTS: [&str; 4] = ["weight_0", "weight_1", "weight_2", "weight_3"];
        const USER_A_IDENTS: [&str; 4] = ["usera_0", "usera_1", "usera_2", "usera_3"];
        const USER_B_IDENTS: [&str; 4] = ["userb_0", "userb_1", "userb_2", "userb_3"];
        const USER_C_IDENTS: [&str; 4] = ["userc_0", "userc_1", "userc_2", "userc_3"];
        const USER_D_IDENTS: [&str; 4] = ["userd_0", "userd_1", "userd_2", "userd_3"];
        for (loc, (attr, kind)) in (&mut attrs).zip(&mut attr_kinds).enumerate() {
            let loc = loc as u32;
            match kind {
                VertexAttributeVariant::Position => {
                    add_slot("position".to_owned(), loc, attr.format)
                }
                VertexAttributeVariant::Normal => add_slot("normal".to_owned(), loc, attr.format),
                VertexAttributeVariant::Tangent => add_slot("tangent".to_owned(), loc, attr.format),
                VertexAttributeVariant::TexCoord => {
                    add_slot(UV_IDENTS[uv_index].to_owned(), loc, attr.format);
                    uv_index += 1;
                }
                VertexAttributeVariant::Color => {
                    add_slot(COLOR_IDENTS[color_index].to_owned(), loc, attr.format);
                    color_index += 1;
                }
                VertexAttributeVariant::Joint => {
                    add_slot(JOINT_IDENTS[joint_index].to_owned(), loc, attr.format);
                    joint_index += 1;
                }
                VertexAttributeVariant::Weight => {
                    add_slot(WEIGHT_IDENTS[weight_index].to_owned(), loc, attr.format);
                    weight_index += 1;
                }
                VertexAttributeVariant::UserA => {
                    add_slot(USER_A_IDENTS[user_a_index].to_owned(), loc, attr.format);
                    user_a_index += 1;
                }
                VertexAttributeVariant::UserB => {
                    add_slot(USER_B_IDENTS[user_b_index].to_owned(), loc, attr.format);
                    user_b_index += 1;
                }
                VertexAttributeVariant::UserC => {
                    add_slot(USER_C_IDENTS[user_c_index].to_owned(), loc, attr.format);
                    user_c_index += 1;
                }
                VertexAttributeVariant::UserD => {
                    add_slot(USER_D_IDENTS[user_d_index].to_owned(), loc, attr.format);
                    user_d_index += 1;
                }
            }
        }

        // Rename if there's only one attribute among vector types.
        if uv_index == 1 {
            // Safety: Infallible.
            unsafe {
                let ident = &mut st.get_member_mut(UV_IDENTS[0]).unwrap_unchecked().ident;
                ident.clear();
                ident.push_str("uv");
            }
        }
        if color_index == 1 {
            // Safety: Infallible.
            unsafe {
                let ident = &mut st.get_member_mut(COLOR_IDENTS[0]).unwrap_unchecked().ident;
                ident.clear();
                ident.push_str("color");
            }
        }
        if joint_index == 1 {
            // Safety: Infallible.
            unsafe {
                let ident = &mut st.get_member_mut(JOINT_IDENTS[0]).unwrap_unchecked().ident;
                ident.clear();
                ident.push_str("joint")
            }
        }
        if weight_index == 1 {
            // Safety: Infallible.
            unsafe {
                let ident = &mut st.get_member_mut(WEIGHT_IDENTS[0]).unwrap_unchecked().ident;
                ident.clear();
                ident.push_str("weight");
            }
        }
        if user_a_index == 1 {
            // Safety: Infallible.
            unsafe {
                let ident = &mut st.get_member_mut(USER_A_IDENTS[0]).unwrap_unchecked().ident;
                ident.clear();
                ident.push_str("usera")
            }
        }
        if user_b_index == 1 {
            // Safety: Infallible.
            unsafe {
                let ident = &mut st.get_member_mut(USER_B_IDENTS[0]).unwrap_unchecked().ident;
                ident.clear();
                ident.push_str("userb");
            }
        }
        if user_c_index == 1 {
            // Safety: Infallible.
            unsafe {
                let ident = &mut st.get_member_mut(USER_C_IDENTS[0]).unwrap_unchecked().ident;
                ident.clear();
                ident.push_str("userc")
            }
        }
        if user_d_index == 1 {
            // Safety: Infallible.
            unsafe {
                let ident = &mut st.get_member_mut(USER_D_IDENTS[0]).unwrap_unchecked().ident;
                ident.clear();
                ident.push_str("userd");
            }
        }

        // Checks out if two iterators have been consumed totally.
        assert_eq!(0, attrs.count());
        assert_eq!(0, attr_kinds.count());

        st
    }
}

/// Bindable resource.
/// 12.3.2. Resource Interface
/// https://www.w3.org/TR/WGSL/#resource-interface
pub enum BindableResource {
    Uniform,
    Storage,
    Texture,
    Sampler,
}
