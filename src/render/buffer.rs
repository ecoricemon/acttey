use crate::{
    render::{context::Gpu, RenderError},
    util::{self, View, Window},
};
use my_ecs::util::prelude::*;
use res::*;
use smallvec::SmallVec;
use std::{
    hash::{Hash, Hasher},
    num::{NonZeroU64, NonZeroUsize},
    ops::Deref,
    rc::Rc,
};
use wgpu::util::DeviceExt;

/// Render module consists of Resource(res), Description(desc), and Execution(exe) layers.
/// Execution layer is responsible for executing render or compute passes.
pub(crate) mod exe {}

/// Render module consists of Resource(res), Description(desc), and Execution(exe) layers.
/// Description layer is responsible for describing GPU state using something like pipeline.
pub(crate) mod desc {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub(crate) struct IndexBuffer(BufferView);

    impl IndexBuffer {
        pub(crate) fn new(buf: BufferView) -> Self {
            assert!(is_valid_usage(buf.usage(), wgpu::BufferUsages::INDEX));

            Self(buf)
        }

        pub(crate) fn into_inner(self) -> BufferView {
            self.0
        }
    }

    impl Deref for IndexBuffer {
        type Target = BufferView;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub(crate) struct VertexBuffer(BufferView);

    impl VertexBuffer {
        pub(crate) fn new(buf: BufferView) -> Self {
            assert!(is_valid_usage(buf.usage(), wgpu::BufferUsages::VERTEX));

            Self(buf)
        }

        pub(crate) fn into_inner(self) -> BufferView {
            self.0
        }
    }

    impl Deref for VertexBuffer {
        type Target = BufferView;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub(crate) struct UniformBuffer(BufferView);

    impl UniformBuffer {
        pub(crate) fn new(buf: BufferView) -> Self {
            assert!(is_valid_usage(buf.usage(), wgpu::BufferUsages::UNIFORM));

            Self(buf)
        }

        pub(crate) fn into_inner(self) -> BufferView {
            self.0
        }
    }

    impl Deref for UniformBuffer {
        type Target = BufferView;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub(crate) struct StorageBuffer(BufferView);

    impl StorageBuffer {
        pub(crate) fn new(buf: BufferView) -> Self {
            assert!(is_valid_usage(buf.usage(), wgpu::BufferUsages::STORAGE));

            Self(buf)
        }

        pub(crate) fn into_inner(self) -> BufferView {
            self.0
        }
    }

    impl Deref for StorageBuffer {
        type Target = BufferView;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    #[derive(Debug, Clone)]
    pub(crate) struct BufferView {
        pub(crate) buf: BufferRc,
        pub(crate) view: View,
    }

    impl BufferView {
        /// If `win` is None, the whole range of the buffer is used.
        pub(crate) fn new(buf: BufferRc, item_size: NonZeroUsize, win: Option<Window>) -> Self {
            let item_size = item_size.get();

            let win = if let Some(win) = win {
                win
            } else {
                // There may be remainder, which means unused area.
                let q = buf.size() / item_size as u64;
                Window::new(0, q as usize)
            };
            let view = View {
                win,
                item_size: item_size,
            };

            Self { buf, view }
        }

        pub(crate) fn get_buffer(&self) -> &BufferRc {
            &self.buf
        }

        pub(crate) fn get_view(&self) -> &View {
            &self.view
        }

        pub(crate) fn as_buffer_binding(&self) -> wgpu::BufferBinding {
            wgpu::BufferBinding {
                buffer: &self.buf,
                offset: self.view.byte_offset() as u64,
                size: NonZeroU64::new(self.view.size() as u64),
            }
        }
    }

    impl Deref for BufferView {
        type Target = wgpu::Buffer;

        fn deref(&self) -> &Self::Target {
            &self.buf
        }
    }

    impl PartialEq for BufferView {
        fn eq(&self, other: &Self) -> bool {
            let Self {
                buf: this_buf,
                view: this_view,
            } = self;

            let Self {
                buf: other_buf,
                view: other_view,
            } = other;

            Rc::ptr_eq(this_buf, other_buf) && this_view == other_view
        }
    }

    impl Eq for BufferView {}

    impl Hash for BufferView {
        fn hash<H: Hasher>(&self, state: &mut H) {
            let Self { buf, view } = self;

            Rc::as_ptr(buf).hash(state);
            view.hash(state);
        }
    }

    fn is_valid_usage(usage: wgpu::BufferUsages, check: wgpu::BufferUsages) -> bool {
        const TEST_FLAGS: wgpu::BufferUsages = wgpu::BufferUsages::INDEX
            .union(wgpu::BufferUsages::VERTEX)
            .union(wgpu::BufferUsages::UNIFORM)
            .union(wgpu::BufferUsages::STORAGE);
        usage.intersection(TEST_FLAGS) == check
    }
}

/// Render module consists of Resource(res), Description(desc), and Execution(exe) layers.
/// Resource layer is responsible for holding GPU relative data.
pub(crate) mod res {
    use super::*;

    /// GPU buffer insertion and removal interfaces.
    pub trait StoreBuffer {
        fn add_buffer(&mut self, desc: BufferDesc) -> Result<BufferRc, RenderError>;
        fn remove_buffer(&mut self, buf: BufferRc);
    }

    #[derive(Debug)]
    pub struct BufferDesc<'a> {
        pub usage: wgpu::BufferUsages,
        pub size_or_data: SizeOrData<'a>,
    }

    impl StoreBuffer for BufferPool {
        fn add_buffer(&mut self, desc: BufferDesc) -> Result<BufferRc, RenderError> {
            self.request_buffer(desc.usage, desc.size_or_data)
        }

        fn remove_buffer(&mut self, buf: BufferRc) {
            drop(buf)
        }
    }

    #[derive(Debug)]
    pub(crate) struct BufferPool {
        gpu: Rc<Gpu>,
        groups: SmallVec<[BufferGroup; 8]>,
    }

    impl BufferPool {
        pub(crate) const IDX: usize = 0;
        pub(crate) const VERT: usize = 1;
        pub(crate) const UNI: usize = 2;
        pub(crate) const STOR: usize = 3;
        pub(crate) const OTHER: usize = 4;
        pub(crate) const USER: usize = 5;

        pub(crate) fn new(gpu: Rc<Gpu>) -> Self {
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

            Self { gpu, groups }
        }

        pub(crate) fn request_buffer(
            &mut self,
            usage: wgpu::BufferUsages,
            size_or_data: SizeOrData,
        ) -> Result<BufferRc, RenderError> {
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
            group.request_buffer(&self.gpu.device, usage, size_or_data)
        }

        pub(crate) fn get_group(&self, group_index: usize) -> Option<&BufferGroup> {
            self.groups.get(group_index)
        }

        pub(crate) fn get_group_mut(&mut self, group_index: usize) -> Option<&mut BufferGroup> {
            self.groups.get_mut(group_index)
        }

        pub(crate) fn get_index_group(&self) -> Option<&BufferGroup> {
            self.get_group(Self::IDX)
        }

        pub(crate) fn get_vertex_group(&self) -> Option<&BufferGroup> {
            self.get_group(Self::VERT)
        }

        pub(crate) fn get_uniform_group(&self) -> Option<&BufferGroup> {
            self.get_group(Self::UNI)
        }

        pub(crate) fn get_storage_group(&self) -> Option<&BufferGroup> {
            self.get_group(Self::STOR)
        }

        /// Returns appended user group's index.
        pub(crate) fn add_user_group(&mut self, group: BufferGroup) -> usize {
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
        pub(crate) fn remove_user_group(&mut self, group_index: usize) -> BufferGroup {
            assert!(Self::USER <= group_index);
            self.groups.remove(group_index)
        }
    }

    #[derive(Debug)]
    pub(crate) struct BufferGroup {
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
        bufs: Vec<BufferRc>,
    }

    impl BufferGroup {
        pub(crate) fn new() -> Self {
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

        pub(crate) fn init(
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

        pub(crate) fn iter(&self) -> impl Iterator<Item = &BufferRc> {
            self.bufs.iter()
        }

        pub(crate) fn iter_used(&self) -> impl Iterator<Item = &BufferRc> {
            self.bufs
                .iter()
                .enumerate()
                .filter_map(|(i, buf)| self.is_used(i).then_some(buf))
        }

        pub(crate) fn change_quota(&mut self, mut quota: u64) -> Result<(), RenderError> {
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

        pub(crate) fn get_quota(&self) -> u64 {
            self.quota
        }

        pub(crate) fn get_alloc_size(&self) -> u64 {
            self.alloc_size
        }

        pub(crate) fn change_max_num(&mut self, mut max_num: usize) -> Result<(), RenderError> {
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

        pub(crate) fn get_max_num(&self) -> usize {
            self.max_num
        }

        pub(crate) fn get_alloc_num(&self) -> usize {
            self.bufs.len()
        }

        pub(crate) fn get_max_size(&self) -> u64 {
            self.max_size
        }

        pub(crate) fn get(&self, index: usize) -> Option<&BufferRc> {
            self.bufs.get(index)
        }

        /// Takes O(n)
        pub(crate) fn clear_unused(&mut self) {
            for i in (0..self.bufs.len()).rev() {
                if self.is_unused(i) {
                    let old = self.bufs.swap_remove(i);
                    self.alloc_size -= old.size();
                }
            }
        }

        pub(crate) fn request_buffer(
            &mut self,
            device: &wgpu::Device,
            add_usage: wgpu::BufferUsages,
            size_or_data: SizeOrData,
        ) -> Result<BufferRc, RenderError> {
            let usage = self.common_usage | add_usage;
            self.request(device, usage, size_or_data)
        }

        fn zero_to_inf_u64(v: u64) -> u64 {
            if v == 0 {
                u64::MAX
            } else {
                v
            }
        }

        fn zero_to_inf_usize(v: usize) -> usize {
            if v == 0 {
                usize::MAX
            } else {
                v
            }
        }

        fn is_used(&self, index: usize) -> bool {
            Rc::strong_count(&self.bufs[index]) > 1
        }

        fn is_unused(&self, index: usize) -> bool {
            !self.is_used(index)
        }

        fn request(
            &mut self,
            device: &wgpu::Device,
            usage: wgpu::BufferUsages,
            size_or_data: SizeOrData,
        ) -> Result<BufferRc, RenderError> {
            // Determines required size.
            let need_size = match size_or_data {
                SizeOrData::Size(size, true) => {
                    to_aligned_addr(size) // Must be a multiple of `wgpu::COPY_BUFFER_ALIGNMENT`.
                }
                SizeOrData::Size(size, false) => {
                    // If only size is required, we can reuse old buffers.
                    let need_size = to_aligned_addr(size);
                    if let Some(i) = self.find_best_fit_unused(need_size, usage) {
                        return Ok(Rc::clone(&self.bufs[i]));
                    }
                    need_size
                }
                SizeOrData::Data(data) => data.len() as u64,
            };

            // We're going to allocate new buffer.
            if need_size > self.max_size {
                Err(RenderError::BufferError(debug_format!(
                    "buffer({}) reached to its maximum size",
                    self.label.as_deref().unwrap_or_default(),
                )))
            } else if self.alloc_size + need_size > self.quota {
                Err(RenderError::BufferError(debug_format!(
                    "buffer({}) reached to its quota",
                    self.label.as_deref().unwrap_or_default(),
                )))
            } else if self.bufs.len() >= self.max_num {
                Err(RenderError::BufferError(debug_format!(
                    "buffer({}) reached to its maximum number",
                    self.label.as_deref().unwrap_or_default(),
                )))
            } else {
                Ok(Rc::clone(self.alloc(device, usage, size_or_data)))
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
            size_or_data: SizeOrData,
        ) -> &BufferRc {
            let buf = match size_or_data {
                SizeOrData::Size(size, map) => device.create_buffer(&wgpu::BufferDescriptor {
                    label: self.label.as_deref(),
                    size,
                    usage,
                    mapped_at_creation: map,
                }),
                SizeOrData::Data(data) => {
                    device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                        label: self.label.as_deref(),
                        contents: data,
                        usage,
                    })
                }
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
    pub enum SizeOrData<'a> {
        /// Size in bytes, map
        Size(u64, bool),
        Data(&'a [u8]),
    }

    impl<'a> From<u64> for SizeOrData<'a> {
        fn from(value: u64) -> Self {
            Self::Size(value, false)
        }
    }

    impl<'a> From<&'a [u8]> for SizeOrData<'a> {
        fn from(value: &'a [u8]) -> Self {
            Self::Data(value)
        }
    }

    /// Shared buffer.
    pub(crate) type BufferRc = Rc<wgpu::Buffer>;

    /// Makes GPU buffer offset or size multiple of 4.
    /// WebGPU requires buffer offset and size must be a multple of 4.  
    /// See https://developer.mozilla.org/en-US/docs/Web/API/GPUQueue/writeBuffer
    pub(crate) const fn to_aligned_addr(addr: u64) -> u64 {
        const MASK: wgpu::BufferAddress = wgpu::COPY_BUFFER_ALIGNMENT - 1;
        (addr + MASK) & (!MASK)
    }

    /// Calculates padded number of items to be aligned in GPU memory.
    pub(crate) fn to_padded_num(unit_size: usize, unit_num: usize) -> usize {
        const ALIGN: usize = wgpu::COPY_BUFFER_ALIGNMENT as usize;
        // `lc_num` is also a power of 2.
        let lc_num = ALIGN / util::gcd(unit_size, ALIGN);
        let mask = lc_num - 1;
        (unit_num + mask) & (!mask)
    }

    /// Writes `bytes` to the mapped buffer.
    /// Call [`wgpu::Buffer::unmap`] after writing.
    pub(crate) fn write_to_mapped_buffer(buf: &wgpu::Buffer, bytes: &[u8], offset: usize) {
        buf.slice(..).get_mapped_range_mut()[offset..offset + bytes.len()].copy_from_slice(bytes);
    }
}
