use my_wgsl::*;

/// Basic camera uniform.
#[wgsl_decl_struct]
pub struct Camera {
    view_proj: mat4x4<f32>,
}

/// Adds basic camera struct `Camera`.
pub fn add_camera_struct(builder: &mut my_wgsl::Builder) {
    my_wgsl::wgsl_structs![builder, Camera];
}

/// Adds binding of basic camera uniform.
pub fn add_camera_bind(builder: &mut my_wgsl::Builder) {
    my_wgsl::wgsl_bind!(builder, group(0) binding(0) var<uniform> camera: Camera);
}

/// Simple material uniform.
#[wgsl_decl_struct]
pub struct SimpleMaterial {
    color: vec3<f32>,
}

/// Adds simple material struct `SimpleMaterial`.
pub fn add_material_struct(builder: &mut my_wgsl::Builder) {
    my_wgsl::wgsl_structs![builder, SimpleMaterial];
}

/// Adds binding of simple material uniform.
pub fn add_material_bind(builder: &mut my_wgsl::Builder) {
    my_wgsl::wgsl_bind!(builder, group(1) binding(0) var<uniform> simpleMaterial: SimpleMaterial);
}

/// Basic vertex stage input.
///
/// Use `fit_wgsl_structure() or create_wgsl_structure()` for the `InterleavedVertexInfo`.
#[wgsl_decl_struct]
pub struct VertexInput {
    #[location(0)]
    position: vec3<f32>,
    #[location(1)]
    color: vec3<f32>,
}

/// Adds basic vertex input struct `VertexInput`.
pub fn add_vertex_input_struct(builder: &mut my_wgsl::Builder) {
    my_wgsl::wgsl_structs![builder, VertexInput];
}

/// Basic vertex stage output.
#[wgsl_decl_struct]
pub struct VertexOutput {
    #[builtin(position)]
    position: vec4<f32>,
    #[location(0)]
    color: vec3<f32>,
}

/// Adds basic vertex output struct `VertexOutput`.
pub fn add_vertex_output_struct(builder: &mut my_wgsl::Builder) {
    my_wgsl::wgsl_structs![builder, VertexOutput];
}

/// Basic fragment stage input.
#[wgsl_decl_struct]
pub struct FragmentInput {
    #[builtin(position)]
    position: vec4<f32>,
    #[location(0)]
    color: vec3<f32>,
}

/// Adds basic fragment input struct `FragmentInput`.
pub fn add_fragment_input_struct(builder: &mut my_wgsl::Builder) {
    my_wgsl::wgsl_structs![builder, FragmentInput];
}

/// Basic fragment stage output.
#[wgsl_decl_struct]
pub struct FragmentOutput {
    #[location(0)]
    color: vec4<f32>,
}

/// Adds basic fragment output struct `FragmentOutput`.
pub fn add_fragment_output_struct(builder: &mut my_wgsl::Builder) {
    my_wgsl::wgsl_structs![builder, FragmentOutput];
}

/// create basic vertex stage.
pub fn add_vertex_stage(builder: &mut my_wgsl::Builder) {
    my_wgsl::wgsl_fn!(builder,
        #[vertex]
        fn vs_main(input: VertexInput) -> VertexOutput {
            var output: VertexOutput;
            output.position = camera.view_proj * vec4f(input.position, 1.0);
            output.color = input.color;
            return output;
        }
    );
}

pub fn add_fragment_stage(builder: &mut my_wgsl::Builder) {
    my_wgsl::wgsl_fn!(builder,
        #[fragment]
        fn fs_main(input: FragmentInput) -> FragmentOutput {
            var output: FragmentOutput;
            output.color = vec4f(input.color, 1.0);
            return output;
        }
    );
}
