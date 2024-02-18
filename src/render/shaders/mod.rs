pub mod basic;
pub mod helper;

use crate::{
    ds::{
        generational::{GenIndex, GenVec},
        sparse_set::MonoSparseSet,
    },
    render::Gpu,
    util::{key::ResKey, ToStr},
};
use my_wgsl::*;
use std::{borrow::Cow, rc::Rc};

pub struct ShaderPack {
    gpu: Rc<Gpu>,
    pub builders: GenVec<my_wgsl::Builder>,
    pub shaders: MonoSparseSet<ResKey, Rc<Shader>>,
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
    pub fn create_shader(&mut self, builder_index: GenIndex, key: &ResKey) -> &Rc<Shader> {
        let builder = self.builders.get(builder_index).unwrap();
        let entry_point = match (
            builder.get_vertex_stage_ident(),
            builder.get_fragment_stage_ident(),
        ) {
            (Some(vert), None) => EntryPoint::Vert(vert.to_owned()),
            (None, Some(frag)) => EntryPoint::Frag(frag.to_owned()),
            (Some(vert), Some(frag)) => EntryPoint::VertFrag(vert.to_owned(), frag.to_owned()),
            _ => panic!(),
        };
        let shader = Shader::new(key.clone(), &self.gpu.device, builder, entry_point);
        if let Some(old) = self.shaders.insert(key.clone(), Rc::new(shader)) {
            assert!(Rc::strong_count(&old) == 1)
        }

        // Safety: Infallible.
        unsafe { self.shaders.get(key).unwrap_unchecked() }
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
        key: ResKey,
        device: &wgpu::Device,
        builder: &Builder,
        entry_point: EntryPoint,
    ) -> Self {
        // Builds WGSL code.
        let wgsl = builder.build();

        // Creates `wgpu::ShaderModule`.
        let module = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some(&key.to_str()),
            source: wgpu::ShaderSource::Wgsl(Cow::from(wgsl)),
        });

        Self {
            module,
            entry_point,
        }
    }

    #[inline]
    pub fn get_vertex_entry(&self) -> Option<&str> {
        self.entry_point.vert()
    }

    #[inline]
    pub fn has_vertex_stage(&self) -> bool {
        self.get_vertex_entry().is_some()
    }

    #[inline]
    pub fn get_fragment_entry(&self) -> Option<&str> {
        self.entry_point.frag()
    }

    #[inline]
    pub fn has_fragment_stage(&self) -> bool {
        self.get_fragment_entry().is_some()
    }
}

#[derive(Debug)]
pub enum EntryPoint {
    Vert(String),
    Frag(String),
    VertFrag(String, String),
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
