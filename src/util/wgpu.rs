use my_wgsl::ToIdent;

/// Converts `wgpu::VertexFormat` to type string used in shader.
pub fn vertex_format_to_shader_str(format: wgpu::VertexFormat) -> &'static str {
    // All comments are copied from wgpu.
    match format {
        // Two unsigned bytes (u8). `vec2<u32>` in shaders.
        wgpu::VertexFormat::Uint8x2 => my_wgsl::vec2::<u32>::maybe_ident().unwrap(),
        // Four unsigned bytes (u8). `vec4<u32>` in shaders.
        wgpu::VertexFormat::Uint8x4 => my_wgsl::vec4::<u32>::maybe_ident().unwrap(),
        // Two signed bytes (i8). `vec2<i32>` in shaders.
        wgpu::VertexFormat::Sint8x2 => my_wgsl::vec2::<i32>::maybe_ident().unwrap(),
        // Four signed bytes (i8). `vec4<i32>` in shaders.
        wgpu::VertexFormat::Sint8x4 => my_wgsl::vec4::<i32>::maybe_ident().unwrap(),
        // Two unsigned bytes (u8). [0, 255] converted to float [0, 1] `vec2<f32>` in shaders.
        wgpu::VertexFormat::Unorm8x2 => my_wgsl::vec2::<f32>::maybe_ident().unwrap(),
        // Four unsigned bytes (u8). [0, 255] converted to float [0, 1] `vec4<f32>` in shaders.
        wgpu::VertexFormat::Unorm8x4 => my_wgsl::vec4::<f32>::maybe_ident().unwrap(),
        // Two signed bytes (i8). [-127, 127] converted to float [-1, 1] `vec2<f32>` in shaders.
        wgpu::VertexFormat::Snorm8x2 => my_wgsl::vec2::<f32>::maybe_ident().unwrap(),
        // Four signed bytes (i8). [-127, 127] converted to float [-1, 1] `vec4<f32>` in shaders.
        wgpu::VertexFormat::Snorm8x4 => my_wgsl::vec4::<f32>::maybe_ident().unwrap(),
        // Two unsigned shorts (u16). `vec2<u32>` in shaders.
        wgpu::VertexFormat::Uint16x2 => my_wgsl::vec2::<u32>::maybe_ident().unwrap(),
        // Four unsigned shorts (u16). `vec4<u32>` in shaders.
        wgpu::VertexFormat::Uint16x4 => my_wgsl::vec4::<u32>::maybe_ident().unwrap(),
        // Two signed shorts (i16). `vec2<i32>` in shaders.
        wgpu::VertexFormat::Sint16x2 => my_wgsl::vec2::<i32>::maybe_ident().unwrap(),
        // Four signed shorts (i16). `vec4<i32>` in shaders.
        wgpu::VertexFormat::Sint16x4 => my_wgsl::vec4::<i32>::maybe_ident().unwrap(),
        // Two unsigned shorts (u16). [0, 65535] converted to float [0, 1] `vec2<f32>` in shaders.
        wgpu::VertexFormat::Unorm16x2 => my_wgsl::vec2::<f32>::maybe_ident().unwrap(),
        // Four unsigned shorts (u16). [0, 65535] converted to float [0, 1] `vec4<f32>` in shaders.
        wgpu::VertexFormat::Unorm16x4 => my_wgsl::vec4::<f32>::maybe_ident().unwrap(),
        // Two signed shorts (i16). [-32767, 32767] converted to float [-1, 1] `vec2<f32>` in shaders.
        wgpu::VertexFormat::Snorm16x2 => my_wgsl::vec2::<f32>::maybe_ident().unwrap(),
        // Four signed shorts (i16). [-32767, 32767] converted to float [-1, 1] `vec4<f32>` in shaders.
        wgpu::VertexFormat::Snorm16x4 => my_wgsl::vec4::<f32>::maybe_ident().unwrap(),
        // Two half-precision floats (no Rust equiv). `vec2<f32>` in shaders.
        wgpu::VertexFormat::Float16x2 => my_wgsl::vec2::<f32>::maybe_ident().unwrap(),
        // Four half-precision floats (no Rust equiv). `vec4<f32>` in shaders.
        wgpu::VertexFormat::Float16x4 => my_wgsl::vec4::<f32>::maybe_ident().unwrap(),
        // One single-precision float (f32). `f32` in shaders.
        wgpu::VertexFormat::Float32 => f32::maybe_ident().unwrap(),
        // Two single-precision floats (f32). `vec2<f32>` in shaders.
        wgpu::VertexFormat::Float32x2 => my_wgsl::vec2::<f32>::maybe_ident().unwrap(),
        // Three single-precision floats (f32). `vec3<f32>` in shaders.
        wgpu::VertexFormat::Float32x3 => my_wgsl::vec3::<f32>::maybe_ident().unwrap(),
        // Four single-precision floats (f32). `vec4<f32>` in shaders.
        wgpu::VertexFormat::Float32x4 => my_wgsl::vec4::<f32>::maybe_ident().unwrap(),
        // One unsigned int (u32). `u32` in shaders.
        wgpu::VertexFormat::Uint32 => u32::maybe_ident().unwrap(),
        // Two unsigned ints (u32). `vec2<u32>` in shaders.
        wgpu::VertexFormat::Uint32x2 => my_wgsl::vec2::<u32>::maybe_ident().unwrap(),
        // Three unsigned ints (u32). `vec3<u32>` in shaders.
        wgpu::VertexFormat::Uint32x3 => my_wgsl::vec3::<u32>::maybe_ident().unwrap(),
        // Four unsigned ints (u32). `vec4<u32>` in shaders.
        wgpu::VertexFormat::Uint32x4 => my_wgsl::vec4::<u32>::maybe_ident().unwrap(),
        // One signed int (i32). `i32` in shaders.
        wgpu::VertexFormat::Sint32 => i32::maybe_ident().unwrap(),
        // Two signed ints (i32). `vec2<i32>` in shaders.
        wgpu::VertexFormat::Sint32x2 => my_wgsl::vec2::<i32>::maybe_ident().unwrap(),
        // Three signed ints (i32). `vec3<i32>` in shaders.
        wgpu::VertexFormat::Sint32x3 => my_wgsl::vec3::<i32>::maybe_ident().unwrap(),
        // Four signed ints (i32). `vec4<i32>` in shaders.
        wgpu::VertexFormat::Sint32x4 => my_wgsl::vec4::<i32>::maybe_ident().unwrap(),
        wgpu::VertexFormat::Float64
        | wgpu::VertexFormat::Float64x2
        | wgpu::VertexFormat::Float64x3
        | wgpu::VertexFormat::Float64x4 => panic!("Unsupported format"),
    }
}
