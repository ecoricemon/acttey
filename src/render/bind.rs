use crate::{
    ds::{
        generational::{GenIndex, GenIndexRc, GenVec},
        sparse_set::MonoSparseSet,
    }, render::{context::Gpu, descs}, util::{ToStr, key::ResKey},
};
use std::{num::NonZeroU64, rc::Rc};

#[derive(Debug)]
pub struct BindPack {
    gpu: Rc<Gpu>,
    pub builders: GenVec<BindBuilder>,
    pub layouts: MonoSparseSet<ResKey, Rc<wgpu::BindGroupLayout>>,
    /// Bindings grap Rc connected to the resources such as buffer.
    /// It guarantees that those resources live enough as long as the bind group lives.
    pub groups: MonoSparseSet<ResKey, (Rc<wgpu::BindGroup>, Vec<Binding>)>,
}

impl BindPack {
    pub fn new(gpu: &Rc<Gpu>) -> Self {
        Self {
            gpu: Rc::clone(gpu),
            builders: GenVec::new(),
            layouts: MonoSparseSet::new(),
            groups: MonoSparseSet::new(),
        }
    }

    /// Creates layout and group from the builder pointed by `builder_index`.
    ///
    /// # Panics
    ///
    /// Panics if `builder_index` is invalid or overwriting fails.
    pub fn create(&mut self, builder_index: GenIndex, layout_key: ResKey, group_key: ResKey) {
        let builder = self.builders.get(builder_index).unwrap();
        let (layout, group, bindings) =
            builder.build(&self.gpu.device, layout_key.clone(), group_key.clone());
        if let Some(old) = self.layouts.insert(layout_key, Rc::new(layout)) {
            assert!(Rc::strong_count(&old) == 1);
        }
        if let Some(old) = self.groups.insert(group_key, (Rc::new(group), bindings)) {
            assert!(Rc::strong_count(&old.0) == 1);
        }
    }

    /// Creates a builder temporarily and creates layout and group for a buffer binding.
    /// The temporary builder is destroyed right away.
    pub fn create_default_buffer_bind(&mut self, desc: descs::BufferBindDesc) {
        // Constructs a temporary builder.
        let mut builder = BindBuilder::new();
        for i in 0..desc.len() {
            builder.layout_entries.push(wgpu::BindGroupLayoutEntry {
                binding: desc.get_binding_or_default(i),
                visibility: desc.get_visibility_or_default(i),
                ty: wgpu::BindingType::Buffer {
                    ty: desc.bind_type,
                    has_dynamic_offset: false,
                    min_binding_size: None,
                },
                count: None,
            });
            builder.bindings.push(Binding::Buffer(BufferBinding {
                buf: Rc::clone(desc.bufs[i]),
                offset: 0,
                size: 0, // Entire range
            }));
        }

        // Builds and removes the temporary builder.
        let builder_index = self.builders.insert(builder);
        self.create(builder_index, desc.layout_key, desc.group_key);
        self.builders.take(builder_index);
    }
}

#[derive(Debug, Default)]
pub struct BindBuilder {
    pub layout_entries: Vec<wgpu::BindGroupLayoutEntry>,
    pub bindings: Vec<Binding>,
}

impl BindBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn free(&mut self) {
        std::mem::take(self);
    }

    /// Builds bind group and layout.
    /// Caller should keep `Vec<Binding>` live because it has Rc connected to resource.
    ///
    /// # Panics
    ///
    /// Panics if the number of layouts is not equivalent to the number of bindings.
    pub fn build(
        &self,
        device: &wgpu::Device,
        layout_key: ResKey,
        group_key: ResKey,
    ) -> (wgpu::BindGroupLayout, wgpu::BindGroup, Vec<Binding>) {
        // layout entries and bindings should have the same length.
        assert_eq!(self.layout_entries.len(), self.bindings.len());

        // Creates `wgpu::BindGroupLayout`.
        let layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some(&layout_key.to_str()),
            entries: &self.layout_entries,
        });

        // Creates a vector of `wgpu::BindGroupEntry`.
        let entries = self
            .layout_entries
            .iter()
            .zip(self.bindings.iter())
            .map(|(layout_entry, binding)| binding.as_entry(layout_entry))
            .collect::<Vec<_>>();

        // Creates `wgpu::BindGroup`.
        let group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some(&group_key.to_str()),
            layout: &layout,
            entries: &entries,
        });

        (layout, group, self.bindings.clone())
    }
}

/// Corresponds to [wgpu::BindingResource](https://wgpu.rs/doc/wgpu/enum.BindingResource.html).
/// This uses index instead of reference, so that you can keep this without borrowing.
/// When it comes to Array variants,
/// in WebGPU spec, it seems there's only one handle for a single binding.
/// [WebGPU spec](https://www.w3.org/TR/webgpu/#typedefdef-gpubindingresource)
/// But, wgpu has features to have multiple handles for a single binding.
/// Array variants exist just for that, it may be implemented for the future.
#[derive(Debug, Clone)]
pub enum Binding {
    Buffer(BufferBinding),
    BufferArray(Vec<BufferBinding>),
    Sampler(GenIndexRc),
    SamplerArray(Vec<GenIndexRc>),
    TextureView(GenIndexRc),
    TextureViewArray(Vec<GenIndexRc>),
}

impl Binding {
    pub fn as_entry(&self, layout_entry: &wgpu::BindGroupLayoutEntry) -> wgpu::BindGroupEntry {
        wgpu::BindGroupEntry {
            binding: layout_entry.binding,
            resource: self.as_resource(layout_entry),
        }
    }

    pub fn as_resource(&self, layout_entry: &wgpu::BindGroupLayoutEntry) -> wgpu::BindingResource {
        match (self, layout_entry.ty) {
            (Self::Buffer(bb), wgpu::BindingType::Buffer { .. }) => {
                wgpu::BindingResource::Buffer(wgpu::BufferBinding {
                    buffer: &bb.buf,
                    offset: bb.offset,
                    size: bb.size(),
                })
            }
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BufferBinding {
    /// Buffer index. Owns the buffer.
    pub buf: Rc<wgpu::Buffer>,
    /// Offset in bytes.
    pub offset: u64,
    /// Size of binding in bytes, 0 for entire range from offset.
    pub size: u64,
}

impl BufferBinding {
    #[inline(always)]
    pub fn size(&self) -> Option<NonZeroU64> {
        NonZeroU64::new(self.size)
    }
}
