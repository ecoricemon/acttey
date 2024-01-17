use crate::{
    debug_format,
    primitive::mesh::InterleavedGeometry,
    render::{Gpu, RenderError},
    util::{AsBytes, AsMultiBytes},
};
use smallvec::SmallVec;
use std::rc::Rc;
use wgpu::util::DeviceExt;

pub struct BufferPool {
    gpu: Rc<Gpu>,
    groups: SmallVec<[BufferGroup; 8]>,
}

impl BufferPool {
    pub const IDX: usize = 0;
    pub const VERT: usize = 1;
    pub const UNI: usize = 2;
    pub const STOR: usize = 3;
    pub const OTHER: usize = 4;
    pub const USER: usize = 5;

    pub fn new(gpu: &Rc<Gpu>) -> Self {
        let mut groups = SmallVec::new();

        let mut idx_group = BufferGroup::new();
        idx_group.init(
            Some("index_buffer"),
            wgpu::BufferUsages::INDEX | wgpu::BufferUsages::COPY_DST,
            0,
            0,
            0,
        );
        groups.push(idx_group);

        let mut vert_group = BufferGroup::new();
        vert_group.init(
            Some("vertex_buffer"),
            wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
            0,
            0,
            0,
        );
        groups.push(vert_group);

        let mut uni_group = BufferGroup::new();
        uni_group.init(
            Some("uniform_buffer"),
            wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            0,
            0,
            0,
        );
        groups.push(uni_group);

        let mut stor_group = BufferGroup::new();
        stor_group.init(
            Some("storage_buffer"),
            wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_DST,
            0,
            0,
            0,
        );
        groups.push(stor_group);

        let mut other_group = BufferGroup::new();
        other_group.init(Some("other_buffer"), wgpu::BufferUsages::empty(), 0, 0, 0);
        groups.push(other_group);

        Self {
            gpu: Rc::clone(gpu),
            groups,
        }
    }

    pub fn request_buffer(
        &mut self,
        usage: wgpu::BufferUsages,
        size: Option<u64>,
        data: Option<&[u8]>,
    ) -> Result<Rc<wgpu::Buffer>, RenderError> {
        const TEST_FLAGS: wgpu::BufferUsages = wgpu::BufferUsages::INDEX
            .union(wgpu::BufferUsages::VERTEX)
            .union(wgpu::BufferUsages::UNIFORM)
            .union(wgpu::BufferUsages::STORAGE);
        const EMPTY: wgpu::BufferUsages = wgpu::BufferUsages::empty();

        let group = match usage.intersection(TEST_FLAGS) {
            wgpu::BufferUsages::INDEX => &mut self.groups[Self::IDX],
            wgpu::BufferUsages::VERTEX => &mut self.groups[Self::VERT],
            wgpu::BufferUsages::UNIFORM => &mut self.groups[Self::UNI],
            wgpu::BufferUsages::STORAGE => &mut self.groups[Self::STOR],
            EMPTY => &mut self.groups[Self::OTHER],
            _ => {
                let errmsg = debug_format!("invalid usage: {:?}", usage,);
                return Err(RenderError::BufferError(errmsg));
            }
        };
        group.request_buffer(&self.gpu.device, usage, size, data)
    }

    pub fn get_group(&self, group_index: usize) -> Option<&BufferGroup> {
        self.groups.get(group_index)
    }

    pub fn get_group_mut(&mut self, group_index: usize) -> Option<&mut BufferGroup> {
        self.groups.get_mut(group_index)
    }

    #[inline]
    pub fn get_index_group(&self) -> Option<&BufferGroup> {
        self.get_group(Self::IDX)
    }

    #[inline]
    pub fn get_vertex_group(&self) -> Option<&BufferGroup> {
        self.get_group(Self::VERT)
    }

    #[inline]
    pub fn get_uniform_group(&self) -> Option<&BufferGroup> {
        self.get_group(Self::UNI)
    }

    #[inline]
    pub fn get_storage_group(&self) -> Option<&BufferGroup> {
        self.get_group(Self::STOR)
    }

    /// Returns appended user group's index.
    pub fn add_user_group(&mut self, group: BufferGroup) -> usize {
        self.groups.push(group);
        self.groups.len() - 1
    }

    /// Removes the group pointed by `group_index`.
    /// Note that all following groups will be moved if it succeeds.
    ///
    /// # Panics
    ///
    /// Panics if eather `index` is out of bound. Valid `index` starts from 5,
    /// because 0 ~ 4 are reserved for default groups.
    pub fn remove_user_group(&mut self, group_index: usize) -> BufferGroup {
        assert!(Self::USER <= group_index);
        self.groups.remove(group_index)
    }
}

pub struct BufferGroup {
    label: Option<String>,
    /// Common usage among buffers in this group.
    common_usage: wgpu::BufferUsages,
    /// Allowed size in total.
    quota: u64,
    /// Allowed number of buffers.
    max_num: usize,
    /// Allowed maximum size that one buffer can have.
    max_size: u64,
    /// Allocated size in total.
    alloc_size: u64,
    /// Buffers.
    bufs: Vec<Rc<wgpu::Buffer>>,
}

impl BufferGroup {
    pub fn new() -> Self {
        Self {
            label: None,
            common_usage: wgpu::BufferUsages::empty(),
            quota: 0,
            max_num: 0,
            max_size: 0,
            alloc_size: 0,
            bufs: Vec::new(),
        }
    }

    pub fn init(
        &mut self,
        label: Option<&str>,
        common_usage: wgpu::BufferUsages,
        quota: u64,
        max_num: usize,
        max_size: u64,
    ) {
        self.label = label.map(|s| s.to_owned());
        self.common_usage = common_usage;
        self.quota = Self::zero_to_inf_u64(quota);
        self.max_num = Self::zero_to_inf_usize(max_num);
        self.max_size = Self::zero_to_inf_u64(max_size);
    }

    pub fn iter(&self) -> impl Iterator<Item = &Rc<wgpu::Buffer>> {
        self.bufs.iter()
    }

    pub fn iter_used(&self) -> impl Iterator<Item = &Rc<wgpu::Buffer>> {
        self.bufs
            .iter()
            .enumerate()
            .filter_map(|(i, buf)| self.is_used(i).then_some(buf))
    }

    pub fn change_quota(&mut self, mut quota: u64) -> Result<(), RenderError> {
        quota = Self::zero_to_inf_u64(quota);
        if quota >= self.alloc_size {
            self.quota = quota;
            Ok(())
        } else {
            let errmsg = debug_format!(
                "buffer({}) is using {} bytes more than requested quota {}",
                self.label.as_deref().unwrap_or_default(),
                self.alloc_size,
                quota,
            );
            Err(RenderError::BufferError(errmsg))
        }
    }

    #[inline(always)]
    pub fn get_quota(&self) -> u64 {
        self.quota
    }

    #[inline(always)]
    pub fn get_alloc_size(&self) -> u64 {
        self.alloc_size
    }

    pub fn change_max_num(&mut self, mut max_num: usize) -> Result<(), RenderError> {
        max_num = Self::zero_to_inf_usize(max_num);
        if max_num >= self.bufs.len() {
            self.max_num = max_num;
            Ok(())
        } else {
            let errmsg = debug_format!(
                "buffer({}) is using {} buffers more than requested max_num {}",
                self.label.as_deref().unwrap_or_default(),
                self.bufs.len(),
                max_num,
            );
            Err(RenderError::BufferError(errmsg))
        }
    }

    #[inline(always)]
    pub fn get_max_num(&self) -> usize {
        self.max_num
    }

    #[inline(always)]
    pub fn get_alloc_num(&self) -> usize {
        self.bufs.len()
    }

    #[inline(always)]
    pub fn get_max_size(&self) -> u64 {
        self.max_size
    }

    #[inline]
    pub fn get(&self, index: usize) -> Option<&Rc<wgpu::Buffer>> {
        self.bufs.get(index)
    }

    /// Takes O(n)
    pub fn clear_unused(&mut self) {
        for i in (0..self.bufs.len()).rev() {
            if self.is_unused(i) {
                let old = self.bufs.swap_remove(i);
                self.alloc_size -= old.size();
            }
        }
    }

    pub fn request_buffer(
        &mut self,
        device: &wgpu::Device,
        add_usage: wgpu::BufferUsages,
        size: Option<u64>,
        data: Option<&[u8]>,
    ) -> Result<Rc<wgpu::Buffer>, RenderError> {
        let usage = self.common_usage | add_usage;
        self.request(device, usage, size, data)
    }

    #[inline]
    fn zero_to_inf_u64(v: u64) -> u64 {
        if v == 0 {
            u64::MAX
        } else {
            v
        }
    }

    #[inline]
    fn zero_to_inf_usize(v: usize) -> usize {
        if v == 0 {
            usize::MAX
        } else {
            v
        }
    }

    #[inline]
    fn is_used(&self, index: usize) -> bool {
        Rc::strong_count(&self.bufs[index]) > 1
    }

    #[inline]
    fn is_unused(&self, index: usize) -> bool {
        !self.is_used(index)
    }

    fn request(
        &mut self,
        device: &wgpu::Device,
        usage: wgpu::BufferUsages,
        size: Option<u64>,
        data: Option<&[u8]>,
    ) -> Result<Rc<wgpu::Buffer>, RenderError> {
        // Determines required size.
        let need_size = if let Some(data) = data {
            data.len() as u64
        } else {
            size.unwrap()
        };
        // If there's allocated and unused buffer.
        if let Some(i) = self.find_best_fit_unused(need_size, usage) {
            Ok(Rc::clone(&self.bufs[i]))
        }
        // No available buffers, then sees to be able to allocate a new buffer.
        // Quota is OK?
        else if need_size < self.max_size && self.alloc_size + need_size <= self.quota {
            // Can we append?
            if self.bufs.len() < self.max_num {
                Ok(Rc::clone(self.alloc(device, usage, size, data)))
            }
            // We can't append a new buffer.
            else {
                let errmsg = debug_format!(
                    "buffer({}) reached to its maximum number of buffers",
                    self.label.as_deref().unwrap_or_default(),
                );
                Err(RenderError::BufferError(errmsg))
            }
        }
        // Impossoble due to the quota.
        else {
            let errmsg = debug_format!(
                "buffer({}) reached to its quota",
                self.label.as_deref().unwrap_or_default(),
            );
            Err(RenderError::BufferError(errmsg))
        }
    }

    fn find_best_fit_unused(&self, size: u64, usage: wgpu::BufferUsages) -> Option<usize> {
        let mut best_index = 0;
        let mut best_size = u64::MAX;
        for (i, buf) in self.bufs.iter().enumerate() {
            if self.is_unused(i)
                && size <= buf.size()
                && buf.size() < best_size
                && buf.usage() == usage
            {
                best_index = i;
                best_size = buf.size();
            }
        }
        (best_size != u64::MAX).then_some(best_index)
    }

    /// Allocates a new GPU buffer.
    fn alloc(
        &mut self,
        device: &wgpu::Device,
        usage: wgpu::BufferUsages,
        size: Option<u64>,
        data: Option<&[u8]>,
    ) -> &Rc<wgpu::Buffer> {
        let buf = if let Some(size) = size {
            device.create_buffer(&wgpu::BufferDescriptor {
                label: self.label.as_deref(),
                size,
                usage,
                mapped_at_creation: false,
            })
        } else {
            device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: self.label.as_deref(),
                contents: data.unwrap(),
                usage,
            })
        };
        self.alloc_size += buf.size();
        self.bufs.push(Rc::new(buf));
        // Safety: Infallible.
        unsafe { self.bufs.last().unwrap_unchecked() }
    }
}

impl Default for BufferGroup {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct VertexBufferMeta {
    pub attrs: SmallVec<[wgpu::VertexAttribute; 4]>,
    pub vert_size: u64,
}

impl VertexBufferMeta {
    pub fn create_buffer_layout(&self) -> wgpu::VertexBufferLayout {
        wgpu::VertexBufferLayout {
            array_stride: self.vert_size,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &self.attrs,
        }
    }
}

impl From<&InterleavedGeometry> for VertexBufferMeta {
    fn from(value: &InterleavedGeometry) -> Self {
        Self {
            attrs: value.attrs.iter().cloned().collect(),
            vert_size: value.vertex_size as u64,
        }
    }
}

#[derive(Debug)]
pub struct IndexBufferMeta {
    pub format: wgpu::IndexFormat,
}

#[derive(Debug)]
pub struct UniformBufferMeta {
    pub vis: wgpu::ShaderStages,
}

/// A helper struct for easy updating GPU buffer.
/// This owns the buffer, so that you should remove it when it's not needed any longer.
#[derive(Debug)]
pub struct BufferData<T: AsBytes> {
    pub buf: Rc<wgpu::Buffer>,
    pub data: T,
}

impl<T: AsBytes> BufferData<T> {
    pub fn new(buf: Rc<wgpu::Buffer>, data: T) -> Self {
        Self { buf, data }
    }

    // Wasm is currently based on 32-bit addressing.
    /// # Panics
    ///
    /// Panics if `offset + size` is out of bound.
    #[inline]
    pub fn write_to_buffer(&self, queue: &wgpu::Queue, offset: usize, size: usize) {
        queue.write_buffer(
            &self.buf,
            offset as u64,
            &self.data.as_bytes()[offset..offset + size],
        )
    }
}

/// Similar to [`BufferData`], but multiple buffers.
#[derive(Debug)]
pub struct MultiBufferData<T: AsMultiBytes, const N: usize> {
    pub bufs: [Rc<wgpu::Buffer>; N],
    pub data: T,
}

impl<T: AsMultiBytes, const N: usize> MultiBufferData<T, N> {
    pub fn new(bufs: [Rc<wgpu::Buffer>; N], data: T) -> Self {
        Self { bufs, data }
    }

    // Wasm is currently based on 32-bit addressing.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound, or `offset + size` is out of bound.
    #[inline]
    pub fn write_to_buffer(&self, queue: &wgpu::Queue, index: usize, offset: usize, size: usize) {
        queue.write_buffer(
            &self.bufs[index],
            offset as u64,
            &self.data.as_bytes(index)[offset..offset + size],
        )
    }
}
