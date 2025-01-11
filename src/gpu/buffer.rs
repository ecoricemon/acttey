use super::common::{HasLabel, LabelledGenVec};
use crate::{
    util::{AsOr, Or, StaticStr},
    ActteyError,
};
use my_ecs::prelude::{Resource, ResourceId, ResourceIndex};
use std::{fmt, ops::Deref, sync::Arc};
use wgpu::util::DeviceExt;

#[derive(Debug, Resource)]
pub struct GpuBufferStorage {
    dev: Arc<wgpu::Device>,
    ri: ResourceIndex,
    bufs: LabelledGenVec<GpuBuffer>,
}

impl GpuBufferStorage {
    pub(crate) fn new(dev: Arc<wgpu::Device>) -> Self {
        Self {
            dev,
            ri: ResourceIndex::dummy(),
            bufs: LabelledGenVec::new(),
        }
    }

    pub(crate) fn set_resource_index(&mut self, ri: ResourceIndex) {
        self.ri = ri;
    }

    pub fn create_then_add<'a, T>(&mut self, desc: &T) -> Result<ResourceId, ActteyError>
    where
        T: AsOr<wgpu::BufferDescriptor<'a>, wgpu::util::BufferInitDescriptor<'a>>,
    {
        let buf = self.create(desc);
        self.add(buf)
    }

    pub fn create<'a, T>(&self, desc: &T) -> GpuBuffer
    where
        T: AsOr<wgpu::BufferDescriptor<'a>, wgpu::util::BufferInitDescriptor<'a>>,
    {
        return inner(self, desc.as_or());

        fn inner(
            this: &GpuBufferStorage,
            desc: Or<&wgpu::BufferDescriptor<'_>, &wgpu::util::BufferInitDescriptor<'_>>,
        ) -> GpuBuffer {
            match desc {
                Or::A(desc) => {
                    let label = desc
                        .label
                        .map(|s| StaticStr::new(s.to_owned()))
                        .unwrap_or_default();
                    let buf = this.dev.create_buffer(desc);
                    GpuBuffer::new(label, buf)
                }
                Or::B(desc) => {
                    let label = desc
                        .label
                        .map(|s| StaticStr::new(s.to_owned()))
                        .unwrap_or_default();
                    let buf = this.dev.create_buffer_init(desc);
                    GpuBuffer::new(label, buf)
                }
            }
        }
    }

    pub fn add(&mut self, buf: GpuBuffer) -> Result<ResourceId, ActteyError> {
        debug_assert!(!self.ri.is_dummy());

        let ii = self.bufs.add(buf)?;
        let rid = ResourceId::new(self.ri, ii);
        Ok(rid)
    }

    pub fn remove<K>(&mut self, key: K) -> Option<GpuBuffer>
    where
        K: AsOr<ResourceId, str>,
    {
        return inner(self, key.as_or());

        fn inner(this: &mut GpuBufferStorage, key: Or<&ResourceId, &str>) -> Option<GpuBuffer> {
            if matches!(key, Or::A(rid) if rid.resource_index() != this.ri) {
                return None;
            }
            let key = key.map_a(ResourceId::item_index);
            this.bufs.remove(key)
        }
    }

    pub fn get<K>(&self, key: K) -> Option<&GpuBuffer>
    where
        K: AsOr<ResourceId, str>,
    {
        return inner(self, key.as_or());

        fn inner<'s>(
            this: &'s GpuBufferStorage,
            key: Or<&ResourceId, &str>,
        ) -> Option<&'s GpuBuffer> {
            if matches!(key, Or::A(rid) if rid.resource_index() != this.ri) {
                return None;
            }
            let key = key.map_a(ResourceId::item_index);
            this.bufs.get(key)
        }
    }
}

#[derive(Clone)]
#[repr(transparent)]
pub struct GpuBuffer(Arc<GpuBufferInner>);

impl GpuBuffer {
    fn new(label: StaticStr, buf: wgpu::Buffer) -> Self {
        let inner = Arc::new(GpuBufferInner { label, buf });
        Self(inner)
    }
}

impl HasLabel for GpuBuffer {
    fn label(&self) -> &str {
        &self.0.label
    }
}

impl Deref for GpuBuffer {
    type Target = wgpu::Buffer;

    fn deref(&self) -> &Self::Target {
        &self.0.buf
    }
}

impl fmt::Debug for GpuBuffer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let inner = &*self.0;
        inner.fmt(f)
    }
}

#[derive(Debug)]
struct GpuBufferInner {
    label: StaticStr,
    buf: wgpu::Buffer,
}

impl<'a> AsOr<wgpu::BufferDescriptor<'a>, wgpu::util::BufferInitDescriptor<'a>>
    for wgpu::BufferDescriptor<'a>
{
    fn as_or(&self) -> Or<&wgpu::BufferDescriptor<'a>, &wgpu::util::BufferInitDescriptor<'a>> {
        Or::A(self)
    }
}

impl<'a> AsOr<wgpu::BufferDescriptor<'a>, wgpu::util::BufferInitDescriptor<'a>>
    for wgpu::util::BufferInitDescriptor<'a>
{
    fn as_or(&self) -> Or<&wgpu::BufferDescriptor<'a>, &wgpu::util::BufferInitDescriptor<'a>> {
        Or::B(self)
    }
}
