pub mod basic;

use crate::{
    ds::{
        generational::{GenIndex, GenVec},
        sparse_set::MonoSparseSet,
    },
    primitive::mesh::GeometryAttributeVariant,
    render::Gpu,
    util::vertex_format_to_shader_str,
};
use my_wgsl::*;
use smallvec::SmallVec;
use std::{borrow::Cow, rc::Rc};

pub struct ShaderPack {
    gpu: Rc<Gpu>,
    pub builders: GenVec<my_wgsl::Builder>,
    pub shaders: MonoSparseSet<Rc<str>, Rc<Shader>>,
}

impl ShaderPack {
    pub fn new(gpu: &Rc<Gpu>) -> Self {
        Self {
            gpu: Rc::clone(gpu),
            builders: GenVec::new(),
            shaders: MonoSparseSet::new(),
        }
    }

    /// Creates a shader from builder pointed by `builder_index`.
    /// The shader will be overwritten if its label is unused anymore.
    ///
    /// # Panics
    ///
    /// Panics if `builder_index` is invalid or overwrting fails.
    pub fn create_shader(&mut self, builder_index: GenIndex, label: &Rc<str>) -> &Rc<Shader> {
        let builder = self.builders.get(builder_index).unwrap();
        let entry_point = match (
            builder.get_vertex_stage_ident(),
            builder.get_fragment_stage_ident(),
        ) {
            (Some(vert), None) => EntryPoint::Vert(vert),
            (None, Some(frag)) => EntryPoint::Frag(frag),
            (Some(vert), Some(frag)) => EntryPoint::VertFrag(vert, frag),
            _ => panic!(),
        };
        let shader = Shader::new(label, &self.gpu.device, builder, entry_point);
        if let Some(old) = self.shaders.insert(Rc::clone(label), Rc::new(shader)) {
            assert!(Rc::strong_count(&old) == 1)
        }

        // Safety: Infallible.
        unsafe { self.shaders.get(label).unwrap_unchecked() }
    }

    /// Clears unused shaders and returns the number of removed shaders.
    pub fn clear_shader(&mut self) -> usize {
        let unused = self
            .shaders
            .iter()
            .filter_map(|(label, shader)| (Rc::strong_count(shader) == 1).then_some(label.clone()))
            .collect::<Vec<_>>();
        let removed = unused.len();
        for label in unused {
            self.shaders.remove(&label);
        }
        removed
    }
}

#[derive(Debug)]
pub struct Shader {
    pub module: wgpu::ShaderModule,
    pub entry_point: EntryPoint,
}

impl Shader {
    pub fn new(
        label: &str,
        device: &wgpu::Device,
        builder: &Builder,
        entry_point: EntryPoint,
    ) -> Self {
        // Builds WGSL code.
        let wgsl = builder.build();

        // Creates `wgpu::ShaderModule`.
        let module = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some(label),
            source: wgpu::ShaderSource::Wgsl(Cow::from(wgsl)),
        });

        Self {
            module,
            entry_point,
        }
    }
}

#[derive(Debug)]
pub enum EntryPoint {
    Vert(&'static str),
    Frag(&'static str),
    VertFrag(&'static str, &'static str),
}

impl EntryPoint {
    pub fn vert(&self) -> Option<&str> {
        match self {
            Self::Vert(s) => Some(s),
            Self::VertFrag(s, _) => Some(s),
            _ => None,
        }
    }

    pub fn frag(&self) -> Option<&str> {
        match self {
            Self::Frag(s) => Some(s),
            Self::VertFrag(_, s) => Some(s),
            _ => None,
        }
    }
}

/// Replace all members in the structure to be fit with the attributes.
/// It actually drops old structure and assigns new structure.
pub fn fit_wgsl_structure<'a>(
    st: &mut my_wgsl::Structure,
    attrs: impl Iterator<Item = &'a wgpu::VertexAttribute>,
    attr_kinds: impl Iterator<Item = &'a GeometryAttributeVariant>,
    builtin_vert_index: bool,
    builtin_inst_index: bool,
) {
    *st = create_wgsl_structure(attrs, attr_kinds, builtin_vert_index, builtin_inst_index);
}

/// Creates `my_wgsl::Structure` from the given attributes,
/// which can be obtained from `InterleavedVertexInfo`.
/// `attrs` and `attr_kinds` should have the exactly same length.
pub fn create_wgsl_structure<'a>(
    mut attrs: impl Iterator<Item = &'a wgpu::VertexAttribute>,
    mut attr_kinds: impl Iterator<Item = &'a GeometryAttributeVariant>,
    builtin_vert_index: bool,
    builtin_inst_index: bool,
) -> my_wgsl::Structure {
    // Creates a structure.
    let mut st = Structure {
        ident: "VertexInput",
        members: SmallVec::new(),
    };

    // Adds built-in attributes.
    let mut add_builtin = |ident: &'static str, value: BuiltinValue| {
        let mut attrs = Attributes::new();
        attrs.0.push(Attribute::Builtin(value));
        st.members.push(StructureMember {
            attrs,
            ident,
            ty: "u32",
        });
    };
    if builtin_vert_index {
        add_builtin("vertexIndex", BuiltinValue::VertexIndex);
    }
    if builtin_inst_index {
        add_builtin("instanceIndex", BuiltinValue::InstanceIndex);
    }

    // Adds slot attributes.
    let mut add_slot = |ident: &'static str, loc: u32, format: wgpu::VertexFormat| {
        let mut attrs = Attributes::new();
        attrs.0.push(Attribute::Location(loc));
        st.members.push(StructureMember {
            attrs,
            ident,
            ty: vertex_format_to_shader_str(format),
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
            GeometryAttributeVariant::Position => add_slot("position", loc, attr.format),
            GeometryAttributeVariant::Normal => add_slot("normal", loc, attr.format),
            GeometryAttributeVariant::Tangent => add_slot("tangent", loc, attr.format),
            GeometryAttributeVariant::TexCoord => {
                add_slot(UV_IDENTS[uv_index], loc, attr.format);
                uv_index += 1;
            }
            GeometryAttributeVariant::Color => {
                add_slot(COLOR_IDENTS[color_index], loc, attr.format);
                color_index += 1;
            }
            GeometryAttributeVariant::Joint => {
                add_slot(JOINT_IDENTS[joint_index], loc, attr.format);
                joint_index += 1;
            }
            GeometryAttributeVariant::Weight => {
                add_slot(WEIGHT_IDENTS[weight_index], loc, attr.format);
                weight_index += 1;
            }
            GeometryAttributeVariant::UserA => {
                add_slot(USER_A_IDENTS[user_a_index], loc, attr.format);
                user_a_index += 1;
            }
            GeometryAttributeVariant::UserB => {
                add_slot(USER_B_IDENTS[user_b_index], loc, attr.format);
                user_b_index += 1;
            }
            GeometryAttributeVariant::UserC => {
                add_slot(USER_C_IDENTS[user_c_index], loc, attr.format);
                user_c_index += 1;
            }
            GeometryAttributeVariant::UserD => {
                add_slot(USER_D_IDENTS[user_d_index], loc, attr.format);
                user_d_index += 1;
            }
        }
    }

    // Rename if there's only one attribute among vector types.
    if uv_index == 1 {
        // Safety: Infallible.
        unsafe { st.get_member_mut(UV_IDENTS[0]).unwrap_unchecked().ident = "uv" };
    }
    if color_index == 1 {
        // Safety: Infallible.
        unsafe { st.get_member_mut(COLOR_IDENTS[0]).unwrap_unchecked().ident = "color" };
    }
    if joint_index == 1 {
        // Safety: Infallible.
        unsafe { st.get_member_mut(JOINT_IDENTS[0]).unwrap_unchecked().ident = "joint" };
    }
    if weight_index == 1 {
        // Safety: Infallible.
        unsafe { st.get_member_mut(WEIGHT_IDENTS[0]).unwrap_unchecked().ident = "weight" };
    }
    if user_a_index == 1 {
        // Safety: Infallible.
        unsafe { st.get_member_mut(USER_A_IDENTS[0]).unwrap_unchecked().ident = "usera" };
    }
    if user_b_index == 1 {
        // Safety: Infallible.
        unsafe { st.get_member_mut(USER_B_IDENTS[0]).unwrap_unchecked().ident = "userb" };
    }
    if user_c_index == 1 {
        // Safety: Infallible.
        unsafe { st.get_member_mut(USER_C_IDENTS[0]).unwrap_unchecked().ident = "userc" };
    }
    if user_d_index == 1 {
        // Safety: Infallible.
        unsafe { st.get_member_mut(USER_D_IDENTS[0]).unwrap_unchecked().ident = "userd" };
    }

    // Checks out if two iterators had been consumed totally.
    assert_eq!(0, attrs.count());
    assert_eq!(0, attr_kinds.count());

    st
}

// 12.3.1.1. Built-in Inputs and Outputs
// ref: https://www.w3.org/TR/WGSL/#builtin-inputs-outputs
/// Example of an input of vertex shader.
#[wgsl_decl_struct]
pub struct VertexInput {
    /// Built-in vertex index.
    /// Type must be u32.
    #[builtin(vertex_index)]
    vertexIndex: u32,
    /// Built-in instance index.
    /// Type must be u32.
    #[builtin(instance_index)]
    instanceIndex: u32,
    #[location(0)]
    position: vec3<f32>,
    #[location(1)]
    normal: vec3<f32>,
    #[location(2)]
    tangent: vec4<f32>,
    #[location(3)]
    uv: vec2<f32>,
    #[location(4)]
    color: vec4<f32>,
    #[location(5)]
    joint: vec4<u16>,
    #[location(6)]
    weight: vec4<f32>,
    #[location(7)]
    userA: vec4<f32>,
    #[location(8)]
    userB: vec4<f32>,
    #[location(9)]
    userC: vec4<f32>,
    #[location(10)]
    userD: vec4<f32>,
}

// 12.3.1.1. Built-in Inputs and Outputs
// ref: https://www.w3.org/TR/WGSL/#builtin-inputs-outputs
/// Example of an output of vertex stage.
#[wgsl_decl_struct]
pub struct VertexOutput {
    /// Built-in position.
    /// Type must be vec4<f32>.
    #[builtin(position)]
    position: vec4<f32>,
}

// 12.3.1.1. Built-in Inputs and Outputs
// ref: https://www.w3.org/TR/WGSL/#builtin-inputs-outputs
/// Example of an input of fragment stage.
#[wgsl_decl_struct]
pub struct FragmentInput {
    /// Built-in position. This is transferred from vertex stage.
    /// Type must be vec4<f32>.
    #[builtin(position)]
    position: vec4<f32>,
    /// Built-in front facing.
    /// Type must be bool.
    #[builtin(front_facing)]
    frontFacing: bool,
    /// Built-in sample index.
    /// Type must be u32.
    #[builtin(sample_index)]
    sampleIndex: u32,
    /// Built-in sample mask.
    /// Type must be u32.
    #[builtin(sample_mask)]
    sampleMask: u32,
}

// 12.3.1.1. Built-in Inputs and Outputs
// ref: https://www.w3.org/TR/WGSL/#builtin-inputs-outputs
/// Example of an output of fragment shader.
#[wgsl_decl_struct]
pub struct FragmentOutput {
    /// Built-in frag depth.
    /// Type must be f32.
    #[builtin(frag_depth)]
    fragDepth: f32,
    /// Built-in sample mask.
    /// Type must be u32.
    #[builtin(sample_mask)]
    sampleMask: u32,
}

// 12.3.1.1. Built-in Inputs and Outputs
// ref: https://www.w3.org/TR/WGSL/#builtin-inputs-outputs
/// Example of an input of compute shader.
#[wgsl_decl_struct]
pub struct ComputeInput {
    /// Built-in local invocation id.
    /// Type must be vec3<u32>.
    #[builtin(local_invocation_id)]
    localId: vec3<u32>,
    /// Built-in local invocation index.
    /// Type must be u32.
    #[builtin(local_invocation_index)]
    localIndex: u32,
    /// Built-in global invocation id.
    /// Type must be vec3<u32>.
    #[builtin(global_invocation_id)]
    globalId: vec3<u32>,
    /// Built-in workgroup id.
    /// Type must be vec3<u32>.
    #[builtin(workgroup_id)]
    workgroupId: vec3<u32>,
    /// Built-in number of workgroups.
    /// Type must be vec3<u32>.
    #[builtin(num_workgroups)]
    numWorkgroups: vec3<u32>,
}
