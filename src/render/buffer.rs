use crate::{
    debug_format,
    primitive::mesh::InterleavedGeometry,
    render::{Gpu, RenderError},
    util::{gcd, View},
};
use smallvec::SmallVec;
use std::rc::Rc;
use wgpu::util::DeviceExt;

#[derive(Debug)]
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
        size_or_data: SizeOrData,
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
        group.request_buffer(&self.gpu.device, usage, size_or_data)
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

#[derive(Debug)]
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
        size_or_data: SizeOrData,
    ) -> Result<Rc<wgpu::Buffer>, RenderError> {
        let usage = self.common_usage | add_usage;
        self.request(device, usage, size_or_data)
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
        size_or_data: SizeOrData,
    ) -> Result<Rc<wgpu::Buffer>, RenderError> {
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
    ) -> &Rc<wgpu::Buffer> {
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

/// Two [`BufferView`] for vertex and index buffers.
#[derive(Debug, Clone)]
pub struct GeometryBufferView {
    /// A [`BufferView`] for the vertex buffer.
    vert_buf_view: BufferView,

    /// A [`BufferView`] for the index buffer.
    index_buf_view: BufferView,

    /// Geometry resource reference to own it.
    _geo_ref: Rc<()>,
}

impl GeometryBufferView {
    pub fn dummy(geo_ref: Rc<()>) -> Self {
        Self {
            vert_buf_view: BufferView::new(usize::MAX, usize::MAX, usize::MAX),
            index_buf_view: BufferView::new(usize::MAX, usize::MAX, usize::MAX),
            _geo_ref: geo_ref,
        }
    }

    #[inline]
    pub fn set_vertex_buffer_view(&mut self, buf_view: BufferView) {
        self.vert_buf_view = buf_view;
    }

    #[inline]
    pub fn get_vertex_buffer_view(&self) -> &BufferView {
        &self.vert_buf_view
    }

    #[inline]
    pub fn set_index_buffer_view(&mut self, buf_view: BufferView) {
        self.index_buf_view = buf_view;
    }

    #[inline]
    pub fn get_index_buffer_view(&self) -> &BufferView {
        &self.index_buf_view
    }

    /// # Panics
    ///
    /// Panics if buffer is not set.
    /// Call [`BufferView::set_buffer`] to do it.
    #[inline]
    pub fn get_vertex_buffer(&self) -> &Rc<wgpu::Buffer> {
        self.get_vertex_buffer_view().get_buffer()
    }

    /// # Panics
    ///
    /// Panics if buffer is not set.
    /// Call [`BufferView::set_buffer`] to do it.
    #[inline]
    pub fn get_index_buffer(&self) -> &Rc<wgpu::Buffer> {
        self.get_index_buffer_view().get_buffer()
    }
}

/// Unified buffer meta-data.
/// [`wgpu::Buffer`] is neutral stroge, and [`BufferView`] is meta-data of it.
/// This includes a way to see the buffer.
/// Also, you can create [`wgpu::BufferSlice`] from this.
#[derive(Debug, Clone)]
pub struct BufferView {
    view: View<usize>,

    /// *Optional field, default is None*  
    /// Buffer itself.
    buf: Option<Rc<wgpu::Buffer>>,

    /// *Optional field, default is empty vector*  
    /// Vertex attributes.
    vert_attrs: Vec<wgpu::VertexAttribute>,

    /// *Optional field, default is Vertex*  
    /// Vertex buffer step mode (Vertex or Instance).
    vert_step_mode: wgpu::VertexStepMode,
}

impl BufferView {
    /// Creates a view describing how to access the buffer.
    /// See [`BufferView::new`] for more details.
    ///
    /// # Panics
    ///
    /// Panics if `item_size` is zero.
    pub fn new(item_size: usize, item_num: usize, index_offset: usize) -> Self {
        assert!(item_size > 0);

        Self {
            view: View::new(index_offset, item_num, item_size),
            buf: None,
            vert_attrs: Vec::new(),
            vert_step_mode: wgpu::VertexStepMode::Vertex,
        }
    }

    /// Creates a view from [`InterleavedGeometry`].
    pub fn new_from_geometry(int_geo: &InterleavedGeometry) -> Self {
        let mut inst = Self::new(int_geo.vertex_size, int_geo.vertex_num, 0);
        inst.set_vertex_attributes(&int_geo.attrs);
        inst
    }

    /// Sets **optional** buffer.
    #[inline]
    pub fn set_buffer(&mut self, buf: Rc<wgpu::Buffer>) {
        self.buf = Some(buf);
    }

    /// # Panics
    ///
    /// Panics if buffer is not set.
    /// Call [`Self::set_buffer`] to do it.
    #[inline]
    pub fn get_buffer(&self) -> &Rc<wgpu::Buffer> {
        self.buf.as_ref().unwrap()
    }

    /// Sets **optional** vertex attributes.
    #[inline]
    pub fn set_vertex_attributes(&mut self, attrs: &[wgpu::VertexAttribute]) {
        self.vert_attrs = Vec::from(attrs);
    }

    #[inline]
    pub fn get_vertex_attributes(&self) -> &Vec<wgpu::VertexAttribute> {
        &self.vert_attrs
    }

    /// Sets **optional** vertex buffer step mode.
    #[inline]
    pub fn set_vertex_step_mode(&mut self, mode: wgpu::VertexStepMode) {
        self.vert_step_mode = mode;
    }

    #[inline]
    pub fn get_vertex_step_mode(&self) -> wgpu::VertexStepMode {
        self.vert_step_mode
    }

    #[inline]
    pub fn get_item_size(&self) -> usize {
        self.view.get_item_size()
    }

    #[inline]
    pub fn get_item_num(&self) -> usize {
        self.view.len
    }

    #[inline]
    pub fn get_index_offset(&self) -> usize {
        self.view.offset
    }

    /// Returns offset in bytes in the buffer.
    #[inline]
    pub fn offset(&self) -> usize {
        self.get_item_size() * self.get_index_offset()
    }

    /// Returns total size in bytes of this view.
    /// It can be zero that means that the total range from the offset.
    #[inline]
    pub fn size(&self) -> usize {
        self.get_item_size() * self.get_item_num()
    }

    /// # Panics
    ///
    /// Panics if buffer is not set.
    /// Call [`Self::set_buffer`] to do it.
    pub fn as_slice(&self) -> wgpu::BufferSlice {
        let buf = self.get_buffer();
        let offset = self.offset() as u64;
        let size = self.size() as u64;
        if size != 0 {
            buf.slice(offset..offset + size)
        } else {
            buf.slice(offset..)
        }
    }

    pub fn as_index_format(&self) -> wgpu::IndexFormat {
        match self.get_item_size() {
            2 => wgpu::IndexFormat::Uint16,
            4 => wgpu::IndexFormat::Uint32,
            _ => panic!(),
        }
    }

    /// # Panics
    ///
    /// Panics if vertex attributes are not set. You can set them by calling
    /// [`Self::set_vertex_attributes`].
    pub fn create_buffer_layout(&self) -> wgpu::VertexBufferLayout {
        assert!(!self.get_vertex_attributes().is_empty());
        wgpu::VertexBufferLayout {
            array_stride: self.get_item_size() as u64,
            step_mode: self.vert_step_mode,
            attributes: self.get_vertex_attributes(),
        }
    }
}

/// Makes GPU buffer offset or size multiple of 4.
/// WebGPU requires buffer offset and size must be a multple of 4.  
/// See https://developer.mozilla.org/en-US/docs/Web/API/GPUQueue/writeBuffer
#[inline]
pub const fn to_aligned_addr(addr: u64) -> u64 {
    const MASK: wgpu::BufferAddress = wgpu::COPY_BUFFER_ALIGNMENT - 1;
    (addr + MASK) & (!MASK)
}

/// Calculates padded number of items to be aligned in GPU memory.
pub fn to_padded_num(unit_size: usize, unit_num: usize) -> usize {
    const ALIGN: usize = wgpu::COPY_BUFFER_ALIGNMENT as usize;
    // `lc_num` is also a power of 2.
    let lc_num = ALIGN / gcd(unit_size, ALIGN);
    let mask = lc_num - 1;
    (unit_num + mask) & (!mask)
}

/// Writes `bytes` to the mapped buffer.
/// Call [`wgpu::Buffer::unmap`] after writing.
pub fn write_to_mapped_buffer(buf: &wgpu::Buffer, bytes: &[u8], offset: usize) {
    buf.slice(..).get_mapped_range_mut()[offset..offset + bytes.len()].copy_from_slice(bytes);
}
