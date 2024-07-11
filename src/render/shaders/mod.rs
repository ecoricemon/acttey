pub(crate) mod basic;
pub(crate) mod helper;

use crate::{render::context::Gpu, util::Or};
use my_wgsl::*;
use res::*;
use std::{
    borrow::Cow,
    hash::{Hash, Hasher},
    rc::Rc,
};

#[derive(Debug)]
pub struct ShaderBuilder {
    gpu: Rc<Gpu>,
    builder: my_wgsl::Builder,

    /// Built shader by this builder.
    /// Users can check the builder and make decision to use the last shader without modification.
    /// In that case, the builder just clones this reference.
    cur_shader: Option<Rc<Shader>>,
}

impl ShaderBuilder {
    pub(crate) const fn new(gpu: Rc<Gpu>) -> Self {
        Self {
            gpu,
            builder: my_wgsl::Builder::new(),
            cur_shader: None,
        }
    }

    pub fn get(&self) -> &my_wgsl::Builder {
        &self.builder
    }

    pub fn get_mut(&mut self) -> &mut my_wgsl::Builder {
        // Getting as mutable reference invalidates generated shader.
        self.cur_shader = None;

        &mut self.builder
    }

    pub fn build(&mut self, label: Option<&str>) -> Rc<Shader> {
        if let Some(shader) = self.cur_shader.as_ref() {
            Rc::clone(shader)
        } else {
            // Generates wgsl code.
            let code = self.builder.build();

            // Creates wgpu's shader module.
            let module = self
                .gpu
                .device
                .create_shader_module(wgpu::ShaderModuleDescriptor {
                    label,
                    source: wgpu::ShaderSource::Wgsl(Cow::Owned(code)),
                });

            let shader = Rc::new(Shader::new(module, Or::A(self.builder.clone())));
            self.cur_shader = Some(Rc::clone(&shader));
            shader
        }
    }
}

/// Render module consists of Resource(res), Description(desc), and Execution(exe) layers.
/// Execution layer is responsible for executing render or compute passes.
pub(crate) mod exe {}

/// Render module consists of Resource(res), Description(desc), and Execution(exe) layers.
/// Description layer is responsible for describing GPU state using something like pipeline.
pub(crate) mod desc {}

/// Render module consists of Resource(res), Description(desc), and Execution(exe) layers.
/// Resource layer is responsible for holding GPU relative data.
pub(crate) mod res {
    use super::*;

    #[derive(Debug)]
    pub struct Shader {
        module: wgpu::ShaderModule,
        entry: EntryPoint,

        /// Read only builder.
        /// If the module is built from custom string, then this field is empty.
        builder: Option<my_wgsl::Builder>,
    }

    impl Shader {
        // Users are allowed to create custom shader module, so this is public method.
        pub fn new(
            module: wgpu::ShaderModule,
            builder_or_entry: Or<my_wgsl::Builder, EntryPoint>,
        ) -> Self {
            let (entry, builder) = match builder_or_entry {
                Or::A(builder) => {
                    let entry = match (
                        builder.get_vertex_stage_ident(),
                        builder.get_fragment_stage_ident(),
                    ) {
                        (Some(vert), None) => EntryPoint::Vert(vert.to_owned()),
                        (None, Some(frag)) => EntryPoint::Frag(frag.to_owned()),
                        (Some(vert), Some(frag)) => {
                            EntryPoint::VertFrag(vert.to_owned(), frag.to_owned())
                        }
                        _ => panic!("invalid shader builder"),
                    };
                    (entry, Some(builder))
                }
                Or::B(entry) => (entry, None),
            };

            Self {
                module,
                entry,
                builder,
            }
        }

        pub fn get_module(&self) -> &wgpu::ShaderModule {
            &self.module
        }

        pub fn get_vertex_stage(&self) -> Option<&str> {
            self.entry.vert()
        }

        pub fn get_fragment_stage(&self) -> Option<&str> {
            self.entry.frag()
        }

        pub fn has_vertex_stage(&self) -> bool {
            self.get_vertex_stage().is_some()
        }

        pub fn has_fragment_stage(&self) -> bool {
            self.get_fragment_stage().is_some()
        }

        /// Comparison process follows the rules shown below,
        /// - If adresses are the same, then returns true.
        /// - If the two have builders that are the same, then returns true.
        /// - Otherwise, returns false.
        pub(crate) fn is_same(this: &Rc<Self>, other: &Rc<Self>) -> bool {
            // Is the same address? Otherwise, is the same builder?
            Rc::ptr_eq(this, other)
                || matches!(
                    (this.builder.as_ref(), other.builder.as_ref()),
                    (Some(a), Some(b)) if a == b
                )
        }

        pub(crate) fn hash(this: &Rc<Self>, hasher: &mut impl Hasher) {
            this.builder.hash(hasher);
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
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
}

// 12.3.1.1. Built-in Inputs and Outputs
// ref: https://www.w3.org/TR/WGSL/#builtin-inputs-outputs
/// Example of an input of vertex shader.
#[wgsl_decl_struct]
pub(crate) struct VertexInput {
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
    joint: vec4<u32>,
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
pub(crate) struct VertexOutput {
    /// Built-in position.
    /// Type must be vec4<f32>.
    #[builtin(position)]
    position: vec4<f32>,
}

// 12.3.1.1. Built-in Inputs and Outputs
// ref: https://www.w3.org/TR/WGSL/#builtin-inputs-outputs
/// Example of an input of fragment stage.
#[wgsl_decl_struct]
pub(crate) struct FragmentInput {
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
pub(crate) struct FragmentOutput {
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
pub(crate) struct ComputeInput {
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
