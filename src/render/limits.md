## 3.6.2. Limits
> [Link: https://gpuweb.github.io/gpuweb/#limits](https://gpuweb.github.io/gpuweb/#limits)

| Limit name | Type | WebGPU default | WebGL2 default |
| :--- | :---: | :---: | :---: |
| max_texture_dimension_1d | u32 | 8192 | 2048 |
| max_texture_dimension_2d | u32 | 8192 | 2048 |
| max_texture_dimension_3d | u32 | 2048 | 256 |
| max_texture_array_layers | u32 | 256 | 256 |
| max_bind_groups | u32 | 4 | 4 |
| (no wgpu)maxBindGroupsPlusVertexBuffers | GPUsize32 | 24 | ? |
| max_bindings_per_bind_group | u32 | 1000 | 1000 |
| max_dynamic_uniform_buffers_per_pipeline_layout | u32 | 8 | 8 |
| max_dynamic_storage_buffers_per_pipeline_layout | u32 | 4 | 0 |
| max_sampled_textures_per_shader_stage | u32 | 16 | 16 |
| max_samplers_per_shader_stage | u32 | 16 | 16 |
| max_storage_buffers_per_shader_stage | u32 | 8 | 0 |
| max_storage_textures_per_shader_stage | u32 | 4 | 0 |
| max_uniform_buffers_per_shader_stage | u32 | 12 | 11 |
| max_uniform_buffer_binding_size | u32 | 65536 | 16384 |
| max_storage_buffer_binding_size | u32 | 134217728 (128 MiB) | 0 |
| min_uniform_buffer_offset_alignment | u32 | 256 | 256 |
| min_storage_buffer_offset_alignment | u32 | 256 | 256 |
| max_vertex_buffers | u32 | 8 | 8 |
| max_buffer_size | u64 | 268435456 (256 MiB) | 268435456 (256 MiB) |
| max_vertex_attributes | u32 | 16 | 16 |
| max_vertex_buffer_array_stride | u32 | 2048 | 255 | 
| max_inter_stage_shader_components | u32 | 60 | 31 |
| (no wgpu)maxInterStageShaderVariables | GPUSize32 | 16 | ? |
| (no wgpu)maxColorAttachments | GPUSize32 | 8 | ? |
| (no wgpu)maxColorAttachmentBytesPerSample | GPUSize32 | 32 | ? |
| max_compute_workgroup_storage_size | u32 | 16384 | 0 |
| max_compute_invocations_per_workgroup | u32 | 256 | 0 |
| max_compute_workgroup_size_x | u32 | 256 | 0 |
| max_compute_workgroup_size_y | u32 | 256 | 0 |
| max_compute_workgroup_size_z | u32 | 64 | 0 | 
| max_compute_workgroups_per_dimension | u32 | 65535 | 0 |
| (wgpu)max_push_constant_size | u32 | 0 | 0 | 
| (wgpu)max_non_sampler_bindings | u32 | 1000000 | 1000000 |

### Description

#### max_texture_dimension_1d
> The maximum allowed value for the size.width of a texture created with dimension "1d".

#### max_texture_dimension_2d
> The maximum allowed value for the size.width and size.height of a texture created with dimension "2d".

#### max_texture_dimension_3d
> The maximum allowed value for the size.width, size.height and size.depthOrArrayLayers of a texture created with dimension "3d".

#### max_texture_array_layers
> The maximum allowed value for the size.depthOrArrayLayers of a texture created with dimension "2d".

#### max_bind_groups
> The maximum number of GPUBindGroupLayouts allowed in bindGroupLayouts when creating a GPUPipelineLayout.

#### (no wgpu)maxBindGroupsPlusVertexBuffers
> The maximum number of bind group and vertex buffer slots used simultaneously, counting any empty slots below the highest index. Validated in createRenderPipeline() and in draw calls.

#### max_bindings_per_bind_group
> The number of binding indices available when creating a GPUBindGroupLayout.
>> NOTE: This limit is normative, but arbitrary. With the default binding slot limits, it is impossible to use 1000 bindings in one bind group, but this allows GPUBindGroupLayoutEntry.binding values up to 999. This limit allows implementations to treat binding space as an array, within reasonable memory space, rather than a sparse map structure.

#### max_dynamic_uniform_buffers_per_pipeline_layout
> The maximum number of GPUBindGroupLayoutEntry entries across a GPUPipelineLayout which are uniform buffers with dynamic offsets. See Exceeds the binding slot limits.

#### max_dynamic_storage_buffers_per_pipeline_layout
> The maximum number of GPUBindGroupLayoutEntry entries across a GPUPipelineLayout which are storage buffers with dynamic offsets. See Exceeds the binding slot limits.

#### max_sampled_textures_per_shader_stage
> For each possible GPUShaderStage stage, the maximum number of GPUBindGroupLayoutEntry entries across a GPUPipelineLayout which are sampled textures. See Exceeds the binding slot limits.

#### max_samplers_per_shader_stage
> For each possible GPUShaderStage stage, the maximum number of GPUBindGroupLayoutEntry entries across a GPUPipelineLayout which are samplers. See Exceeds the binding slot limits.

#### max_storage_buffers_per_shader_stage
> For each possible GPUShaderStage stage, the maximum number of GPUBindGroupLayoutEntry entries across a GPUPipelineLayout which are storage buffers. See Exceeds the binding slot limits.

#### max_storage_textures_per_shader_stage
> For each possible GPUShaderStage stage, the maximum number of GPUBindGroupLayoutEntry entries across a GPUPipelineLayout which are storage textures. See Exceeds the binding slot limits.

#### max_uniform_buffers_per_shader_stage
> For each possible GPUShaderStage stage, the maximum number of GPUBindGroupLayoutEntry entries across a GPUPipelineLayout which are uniform buffers. See Exceeds the binding slot limits.

#### max_uniform_buffer_binding_size
> The maximum GPUBufferBinding.size for bindings with a GPUBindGroupLayoutEntry entry for which entry.buffer?.type is "uniform".

#### max_storage_buffer_binding_size
> The maximum GPUBufferBinding.size for bindings with a GPUBindGroupLayoutEntry entry for which entry.buffer?.type is "storage" or "read-only-storage".

#### min_uniform_buffer_offset_alignment
> The required alignment for GPUBufferBinding.offset and the dynamic offsets provided in setBindGroup(), for bindings with a GPUBindGroupLayoutEntry entry for which entry.buffer?.type is "uniform".

#### min_storage_buffer_offset_alignment
> The required alignment for GPUBufferBinding.offset and the dynamic offsets provided in setBindGroup(), for bindings with a GPUBindGroupLayoutEntry entry for which entry.buffer?.type is "storage" or "read-only-storage".

#### max_vertex_buffers
> The maximum number of buffers when creating a GPURenderPipeline.

#### max_buffer_size
> The maximum size of size when creating a GPUBuffer.

#### max_vertex_attributes
> The maximum number of attributes in total across buffers when creating a GPURenderPipeline.

#### max_vertex_buffer_array_stride
> The maximum allowed arrayStride when creating a GPURenderPipeline.

#### max_inter_stage_shader_components
> The maximum allowed number of components of input or output variables for inter-stage communication (like vertex outputs or fragment inputs).

#### (no wgpu)maxInterStageShaderVariables
> The maximum allowed number of input or output variables for inter-stage communication (like vertex outputs or fragment inputs).

#### (no wgpu)maxColorAttachments
> The maximum allowed number of color attachments in GPURenderPipelineDescriptor.fragment.targets, GPURenderPassDescriptor.colorAttachments, and GPURenderPassLayout.colorFormats.

#### (no wgpu)maxColorAttachmentBytesPerSample
> The maximum number of bytes necessary to hold one sample (pixel or subpixel) of render pipeline output data, across all color attachments.

#### max_compute_workgroup_storage_size
> The maximum number of bytes of workgroup storage used for a compute stage GPUShaderModule entry-point.

#### max_compute_invocations_per_workgroup
> The maximum value of the product of the workgroup_size dimensions for a compute stage GPUShaderModule entry-point.

#### max_compute_workgroup_size_x
> The maximum value of the workgroup_size X dimension for a compute stage GPUShaderModule entry-point.

#### max_compute_workgroup_size_y
> The maximum value of the workgroup_size Y dimensions for a compute stage GPUShaderModule entry-point.

#### max_compute_workgroup_size_z 
> The maximum value of the workgroup_size Z dimensions for a compute stage GPUShaderModule entry-point.

#### max_compute_workgroups_per_dimension|
> The maximum value for the arguments of dispatchWorkgroups(workgroupCountX, workgroupCountY, workgroupCountZ).
