use super::share::*;
use acttey::prelude::*;
use std::slice;
use wasm_bindgen::prelude::*;

const GPU_SHADER_NAME: &str = "shader";
const GPU_WRITE_BUF_NAME: &str = "write-buffer";
const GPU_READ_BUF_NAME: &str = "read-buffer";
const GPU_ARGS_BUF_NAME: &str = "args-buffer";
const GPU_PALETTE_BUF_NAME: &str = "palette-buffer";
const GPU_BIND_GROUP_LAYOUT_NAME: &str = "bind-group-layout";
const GPU_BIND_GROUP_NAME: &str = "bind-group";
const GPU_PIPELINE_LAYOUT_NAME: &str = "pipeline-layout";
const GPU_PIPELINE_NAME: &str = "pipeline";
const GPU_COMMAND_ENCODER_NAME: &str = "command-encoder";
const GPU_COMPUTE_PASS_NAME: &str = "compute-pass";

pub(super) fn init_gpu_resources(
    rw: ResWrite<(
        ShaderStorage,
        GpuBufferStorage,
        BindGroupLayoutStorage,
        BindGroupStorage,
        PipelineLayoutStorage,
        ComputePipelineStorage,
        CommandEncoderStorage,
    )>,
) {
    let (shader_stor, buf_stor, bind_layout_stor, bind_stor, pipe_layout_stor, pipe_stor, enc_stor) =
        rw.take();

    create_shader(shader_stor);
    create_buffer(buf_stor);
    create_bind_group_layout(bind_layout_stor);
    let bind_layout = bind_layout_stor.get(GPU_BIND_GROUP_LAYOUT_NAME).unwrap();
    let write_buf = buf_stor.get(GPU_WRITE_BUF_NAME).unwrap();
    let args_buf = buf_stor.get(GPU_ARGS_BUF_NAME).unwrap();
    let palette_buf = buf_stor.get(GPU_PALETTE_BUF_NAME).unwrap();
    create_bind_group(bind_stor, bind_layout, write_buf, args_buf, palette_buf);
    create_pipeline_layout(pipe_layout_stor, bind_layout);
    let shader = shader_stor.get(GPU_SHADER_NAME).unwrap();
    let pipe_layout = pipe_layout_stor.get(GPU_PIPELINE_LAYOUT_NAME).unwrap();
    create_pipeline(pipe_stor, pipe_layout, shader);
    let pipe = pipe_stor.get(GPU_PIPELINE_NAME).unwrap().clone();
    let bind = bind_stor.get(GPU_BIND_GROUP_NAME).unwrap().clone();
    let write_buf = write_buf.clone();
    let read_buf = buf_stor.get(GPU_READ_BUF_NAME).unwrap().clone();
    create_encoder(enc_stor, pipe, bind, write_buf, read_buf);
}

pub(super) fn submit(rw: ResWrite<(Gpu, GpuBufferStorage, CommandEncoderStorage)>) {
    let (gpu, buf_stor, enc_stor) = rw.take();

    let args_buf = buf_stor.get(GPU_ARGS_BUF_NAME).unwrap();
    let data = gpu_slot();
    gpu.queue()
        .write_buffer(args_buf, 0, data.args.as_u8_slice());

    let enc = enc_stor.get(GPU_COMMAND_ENCODER_NAME).unwrap();
    let cmd_buf = enc.encode();
    gpu.queue().submit([cmd_buf]);
}

pub(super) fn read(rw: ResWrite<GpuBufferStorage>) {
    let buf_stor = rw.take();

    let read_buf = buf_stor.get(GPU_READ_BUF_NAME).unwrap();
    let c_read_buf = read_buf.clone();
    read_buf
        .slice(..)
        .map_async(wgpu::MapMode::Read, move |res| {
            // On web, this closure will be called as a microtask. Therefore, 
            // ECS must stop execution for a while so that JS runtime schedule
            // this closure.
            assert!(res.is_ok());

            let mut data = gpu_slot();
            data.buf
                .copy_from_slice(&c_read_buf.slice(..).get_mapped_range());
            drop(data);
            unload_from_gpu_slot();

            c_read_buf.unmap();
            web_util::worker_post_message(&JsValue::undefined()).unwrap();
        });
}

fn create_shader(stor: &mut ShaderStorage) {
    let code = shader_code();
    let desc = wgpu::ShaderModuleDescriptor {
        label: Some(GPU_SHADER_NAME),
        source: wgpu::ShaderSource::Wgsl(code.into()),
    };
    let entry = EntryPoint::compute("compute");
    stor.create_then_add(desc, entry).unwrap();
}

fn create_buffer(stor: &mut GpuBufferStorage) {
    let size = (canvas_width() * canvas_height() * 4) as wgpu::BufferAddress;

    let write_buf_desc = wgpu::BufferDescriptor {
        label: Some(GPU_WRITE_BUF_NAME),
        size,
        usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_SRC,
        mapped_at_creation: false,
    };
    stor.create_then_add(&write_buf_desc).unwrap();

    let read_buf_desc = wgpu::BufferDescriptor {
        label: Some(GPU_READ_BUF_NAME),
        size,
        usage: wgpu::BufferUsages::MAP_READ | wgpu::BufferUsages::COPY_DST,
        mapped_at_creation: false,
    };
    stor.create_then_add(&read_buf_desc).unwrap();

    let data = gpu_slot();
    let args_buf_desc = wgpu::util::BufferInitDescriptor {
        label: Some(GPU_ARGS_BUF_NAME),
        contents: data.args.as_u8_slice(),
        usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
    };
    stor.create_then_add(&args_buf_desc).unwrap();

    let ptr = PALETTE.as_slice().as_ptr() as *const u8;
    let len = PALETTE.as_slice().len() * 4;
    let contents = unsafe { slice::from_raw_parts(ptr, len) };
    let palette_buf_desc = wgpu::util::BufferInitDescriptor {
        label: Some(GPU_PALETTE_BUF_NAME),
        contents,
        usage: wgpu::BufferUsages::UNIFORM,
    };
    stor.create_then_add(&palette_buf_desc).unwrap();
}

fn create_bind_group_layout(stor: &mut BindGroupLayoutStorage) {
    let write_buf_entry = wgpu::BindGroupLayoutEntry {
        binding: 0,
        visibility: wgpu::ShaderStages::COMPUTE,
        ty: wgpu::BindingType::Buffer {
            ty: wgpu::BufferBindingType::Storage { read_only: false },
            has_dynamic_offset: false,
            min_binding_size: None,
        },
        count: None,
    };

    let args_buf_entry = wgpu::BindGroupLayoutEntry {
        binding: 1,
        visibility: wgpu::ShaderStages::COMPUTE,
        ty: wgpu::BindingType::Buffer {
            ty: wgpu::BufferBindingType::Uniform,
            has_dynamic_offset: false,
            min_binding_size: None,
        },
        count: None,
    };

    let palette_buf_entry = wgpu::BindGroupLayoutEntry {
        binding: 2,
        visibility: wgpu::ShaderStages::COMPUTE,
        ty: wgpu::BindingType::Buffer {
            ty: wgpu::BufferBindingType::Uniform,
            has_dynamic_offset: false,
            min_binding_size: None,
        },
        count: None,
    };

    let desc = wgpu::BindGroupLayoutDescriptor {
        label: Some(GPU_BIND_GROUP_LAYOUT_NAME),
        entries: &[write_buf_entry, args_buf_entry, palette_buf_entry],
    };
    stor.create_then_add(&desc).unwrap();
}

fn create_bind_group(
    stor: &mut BindGroupStorage,
    layout: &wgpu::BindGroupLayout,
    write_buf: &wgpu::Buffer,
    args_buf: &wgpu::Buffer,
    palette_buf: &wgpu::Buffer,
) {
    let write_buf_entry = wgpu::BindGroupEntry {
        binding: 0,
        resource: wgpu::BindingResource::Buffer(wgpu::BufferBinding {
            buffer: write_buf,
            offset: 0,
            size: None,
        }),
    };

    let args_buf_entry = wgpu::BindGroupEntry {
        binding: 1,
        resource: wgpu::BindingResource::Buffer(wgpu::BufferBinding {
            buffer: args_buf,
            offset: 0,
            size: None,
        }),
    };

    let palette_buf_entry = wgpu::BindGroupEntry {
        binding: 2,
        resource: wgpu::BindingResource::Buffer(wgpu::BufferBinding {
            buffer: palette_buf,
            offset: 0,
            size: None,
        }),
    };

    let desc = wgpu::BindGroupDescriptor {
        label: Some("bind-group"),
        layout,
        entries: &[write_buf_entry, args_buf_entry, palette_buf_entry],
    };
    stor.create_then_add(&desc).unwrap();
}

fn create_pipeline_layout(
    stor: &mut PipelineLayoutStorage,
    bind_group_layout: &wgpu::BindGroupLayout,
) {
    let desc = wgpu::PipelineLayoutDescriptor {
        label: Some(GPU_PIPELINE_LAYOUT_NAME),
        bind_group_layouts: &[bind_group_layout],
        push_constant_ranges: &[],
    };
    stor.create_then_add(&desc).unwrap();
}

fn create_pipeline(
    stor: &mut ComputePipelineStorage,
    layout: &wgpu::PipelineLayout,
    shader: &Shader,
) {
    let desc = wgpu::ComputePipelineDescriptor {
        label: Some(GPU_PIPELINE_NAME),
        layout: Some(layout),
        module: shader,
        entry_point: shader.get_entry_str(),
        compilation_options: Default::default(),
        cache: None,
    };
    stor.create_then_add(&desc).unwrap();
}

fn create_encoder(
    stor: &mut CommandEncoderStorage,
    pipe: ComputePipeline,
    bind: BindGroup,
    write_buf: GpuBuffer,
    read_buf: GpuBuffer,
) {
    let mut enc = stor.create(&wgpu::CommandEncoderDescriptor {
        label: Some(GPU_COMMAND_ENCODER_NAME),
    });
    let mut pass = enc.begin_compute_pass(&wgpu::ComputePassDescriptor {
        label: Some(GPU_COMPUTE_PASS_NAME),
        timestamp_writes: None,
    });

    let (width, height) = gpu_slot().args.size;
    pass.set_pipeline(pipe)
        .set_bind_group(0, bind)
        .dispatch_workgroups(width.div_ceil(64), height, 1);
    pass.end();

    let size = write_buf.size();
    enc.copy_buffer_to_buffer(write_buf, 0, read_buf, 0, size);
    stor.add(enc).unwrap();
}

fn shader_code() -> String {
    format!(
        "
        const MAX_ITER: u32 = {MAX_ITER};
        const N: u32 = MAX_ITER / 4;

        struct Arguments {{
            size: vec2u,
            x_range: vec2f,
            y_range: vec2f,
        }}

        @group(0) @binding(0) var<storage, read_write> data: array<u32>;
        @group(0) @binding(1) var<uniform> args: Arguments;
        @group(0) @binding(2) var<uniform> palette: array<vec4u, N>;

        @compute @workgroup_size(64)
        fn compute(@builtin(global_invocation_id) gid: vec3u) {{
            let xy = gid.xy;
            if (all(xy < args.size)) {{
                let di = xy.y * args.size.x + xy.x;
                let iter = calc_pixel(xy.x, xy.y);
                data[di] = palette[iter / 4][iter % 4];
            }}
        }}

        fn calc_pixel(x: u32, y: u32) -> u32 {{
            let c = vec2f(
                scale(x, args.size.x, args.x_range.x, args.x_range.y),
                scale(y, args.size.y, args.y_range.x, args.y_range.y),
            );
            var z = vec2f(0, 0);
            var iter: u32 = 0;
            while (
                iter < (MAX_ITER - 1) && dot(z, z) < 4f
            ) {{
                z = complex_mul(z, z) + c;
                iter++;
            }}
            return iter;
        }}

        fn scale(val: u32, val_limit: u32, low: f32, high: f32) -> f32 {{
            return (f32(val) / f32(val_limit)) * (high - low) + low;
        }}

        fn complex_mul(lhs: vec2f, rhs: vec2f) -> vec2f {{
            return vec2f(
                lhs.x * rhs.x - lhs.y * rhs.y,
                lhs.x * rhs.y + lhs.y * rhs.x,
            );
        }}
    "
    )
}
