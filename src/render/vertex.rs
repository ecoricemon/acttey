use super::{
    buffer::{
        desc::{BufferView, IndexBuffer, VertexBuffer},
        res::BufferRc,
    },
    shaders::res::Shader,
};
use crate::{
    common::AppHasher,
    ds::{
        refs::WithRc,
        share::{DebugLock, SharedVec, SharedVecItem, SharedVecOrigin},
    },
    util::{Or, Window},
};
use desc::*;
use exe::*;
use std::{
    cell::Cell,
    hash::{Hash, Hasher},
    mem,
    num::{NonZeroU64, NonZeroUsize},
    ops::Range,
    rc::Rc,
};

thread_local! {
    // For debugging.
    // For now, all shared variables in this module share this counter to detect invalid mutable borrows.
    // This may change in the future.
    pub(crate) static VERT_MOD_DEBUG_LOCK: Cell<i32> = const { Cell::new(0) };
}

#[derive(Debug)]
pub struct VertexStateManager {
    /// Pointer to the vector inside [`Self::org_verts`].
    pub(crate) verts: SharedVec<VertexStateRc, AppHasher>,

    /// Actual vector of [`VertexState`].
    /// This field is not accessed and just keeps vector alive.
    org_verts: SharedVecOrigin<VertexStateRc, AppHasher>,
}

impl VertexStateManager {
    pub(crate) fn new() -> Self {
        let (verts, org_verts) = SharedVec::new();

        Self { verts, org_verts }
    }
}

impl StoreVertexState for VertexStateManager {
    fn add_vertex_state(&mut self, desc: VertexStateDesc) -> VertexStateRef {
        let state = VertexState::new(desc.shader, desc.layouts);
        let index = self.verts.add(WithRc::new(state));

        DebugLock::new(
            SharedVecItem::new(self.verts, index),
            #[cfg(debug_assertions)]
            &VERT_MOD_DEBUG_LOCK,
        )
    }

    fn remove_vertex_state(
        &mut self,
        state: VertexStateRef,
    ) -> Result<VertexState, VertexStateRef> {
        let state = state.into_inner();
        self.verts
            .remove(state)
            .map(VertexStateRc::into_inner)
            .map_err(|state| {
                DebugLock::new(
                    state,
                    #[cfg(debug_assertions)]
                    &VERT_MOD_DEBUG_LOCK,
                )
            })
    }
}

/// Render module consists of Resource(res), Description(desc), and Execution(exe) layers.
/// Execution layer is responsible for executing render or compute passes.
pub(crate) mod exe {
    use super::*;

    /// A temporary set of vertex and index commands generated from a [`VertexState`].
    #[derive(Debug, Clone)]
    pub(crate) struct VertexCommandSet {
        /// Command to set index buffer.
        pub(crate) idx_cmd: IndexBufferCommand,

        /// Commands to set vertex buffers.
        pub(crate) vert_cmds: Box<[VertexBufferCommand]>,
    }

    #[derive(Debug, Clone)]
    pub(crate) struct IndexBufferCommand {
        /// Command builder.
        state: VertexStateRef,

        buf: BufferRc,

        /// Buffer range in bytes.
        range: Range<u64>,

        /// Index format.
        format: wgpu::IndexFormat,
    }

    impl IndexBufferCommand {
        /// # Panics
        ///
        /// Panics if the end position is not greater than the start position.
        pub(crate) fn new(
            state: VertexStateRef,
            buf: BufferRc,
            start_bytes: u64,
            end_bytes: Option<NonZeroU64>,
            format: wgpu::IndexFormat,
        ) -> Self {
            // Turns (start..end) into a range.
            let end_bytes = if let Some(end_bytes) = end_bytes {
                end_bytes.get()
            } else {
                buf.size()
            };
            assert!(start_bytes < end_bytes);
            let range = start_bytes..end_bytes;

            Self {
                state,
                buf,
                range,
                format,
            }
        }

        pub(crate) fn from_buffer(
            state: VertexStateRef,
            buf: IndexBuffer,
            format: wgpu::IndexFormat,
        ) -> Self {
            let BufferView { buf, view } = buf.into_inner();
            let start_bytes = view.byte_offset() as u64;
            let end_bytes = NonZeroU64::new(view.byte_end() as u64);
            Self::new(state, buf, start_bytes, end_bytes, format)
        }

        pub(crate) fn execute<'a: 'b, 'b>(&'a self, pass: &mut wgpu::RenderPass<'b>) {
            pass.set_index_buffer(self.buf.slice(self.range.clone()), self.format);
        }
    }

    impl PartialEq for IndexBufferCommand {
        fn eq(&self, other: &Self) -> bool {
            // In command equality test, it's enough to test data only.
            let Self {
                state: _this_state, // state is layout.
                buf: this_buf,
                range: this_range,
                format: this_format,
            } = self;

            let Self {
                state: _other_state,
                buf: other_buf,
                range: other_range,
                format: other_format,
            } = other;

            Rc::ptr_eq(&this_buf, &other_buf)
                && this_range == other_range
                && this_format == other_format
        }
    }

    impl Eq for IndexBufferCommand {}

    impl Hash for IndexBufferCommand {
        fn hash<H: Hasher>(&self, state: &mut H) {
            let Self {
                state: _state, // state is layout.
                buf,
                range,
                format,
            } = self;

            Rc::as_ptr(buf).hash(state);
            range.hash(state);
            format.hash(state);
        }
    }

    #[derive(Debug, Clone)]
    pub(crate) struct VertexBufferCommand {
        /// Command builder.
        state: VertexStateRef,

        buf: BufferRc,

        /// Buffer range in bytes.
        range: Range<u64>,

        /// Buffer slot.
        /// This number must match with the pipeline.
        slot: u32,
    }

    impl VertexBufferCommand {
        /// # Panics
        ///
        /// Panics if the end position is not greater than the start position.
        pub(crate) fn new(
            state: VertexStateRef,
            buf: BufferRc,
            start_bytes: u64,
            end_bytes: Option<NonZeroU64>,
            slot: u32,
        ) -> Self {
            // Turns (start..end) into a range.
            let end_bytes = if let Some(end_bytes) = end_bytes {
                end_bytes.get()
            } else {
                buf.size()
            };
            assert!(start_bytes < end_bytes);
            let range = start_bytes..end_bytes;

            Self {
                state,
                buf,
                range,
                slot,
            }
        }

        pub(crate) fn from_buffer(state: VertexStateRef, buf: VertexBuffer, slot: u32) -> Self {
            let BufferView { buf, view } = buf.into_inner();
            let start_bytes = view.byte_offset() as u64;
            let end_bytes = NonZeroU64::new(view.byte_end() as u64);
            Self::new(state, buf, start_bytes, end_bytes, slot)
        }

        pub(crate) fn slot(&self) -> u32 {
            self.slot
        }

        pub(crate) fn execute<'a: 'b, 'b>(&'a self, pass: &mut wgpu::RenderPass<'b>) {
            pass.set_vertex_buffer(self.slot, self.buf.slice(self.range.clone()));
        }
    }

    impl PartialEq for VertexBufferCommand {
        fn eq(&self, other: &Self) -> bool {
            // In command equality test, it's enough to test data only.
            let Self {
                state: _this_state, // state is layout.
                buf: this_buf,
                range: this_range,
                slot: this_slot,
            } = self;

            let Self {
                state: _other_state,
                buf: other_buf,
                range: other_range,
                slot: other_slot,
            } = other;

            Rc::ptr_eq(this_buf, other_buf) && this_range == other_range && this_slot == other_slot
        }
    }

    impl Eq for VertexBufferCommand {}

    impl Hash for VertexBufferCommand {
        fn hash<H: Hasher>(&self, state: &mut H) {
            let Self {
                state: _state, // state is layout.
                buf,
                range,
                slot,
            } = self;

            Rc::as_ptr(buf).hash(state);
            range.hash(state);
            slot.hash(state);
        }
    }
}

/// Render module consists of Resource(res), Description(desc), and Execution(exe) layers.
/// Description layer is responsible for describing GPU state using something like pipeline.
pub(crate) mod desc {
    use super::*;

    /// Vertex state insertion and removal interfaces.
    pub trait StoreVertexState {
        fn add_vertex_state(&mut self, desc: VertexStateDesc) -> VertexStateRef;
        fn remove_vertex_state(
            &mut self,
            state: VertexStateRef,
        ) -> Result<VertexState, VertexStateRef>;
    }

    #[derive(Debug)]
    pub struct VertexStateDesc {
        pub shader: Rc<Shader>,
        pub layouts: Vec<VertexLayout>,
    }

    /// Writable reference to a vertex state.
    pub(crate) type VertexStateRef =
        DebugLock<SharedVecItem<VertexStateRc, AppHasher>, VertexStateRc>;

    /// Shared vertex state.
    pub(crate) type VertexStateRc = WithRc<VertexState>;

    #[derive(Debug)]
    pub(crate) struct VertexState {
        shader: Rc<Shader>,

        /// Read only vertex buffer layouts.
        /// Vertex layout can describe vertex or instance.
        /// But in this list, there must be only one instance layout and many vetex layouts.
        layouts: Box<[VertexLayout]>,

        /// Temporary writable index buffer.
        idx_buf: Option<(IndexBuffer, wgpu::IndexFormat)>,

        /// Temporary writable vertex buffers.
        /// Vertex buffer contains vertex data or instance data.
        /// But in this list, there must be only one instance data and many vertex data.
        vert_bufs: Vec<VertexBuffer>,
    }

    impl VertexState {
        pub(crate) fn new(shader: Rc<Shader>, layouts: Vec<VertexLayout>) -> Self {
            // Shader must have vertex stage.
            assert!(shader.has_vertex_stage());

            // There must be only one instance layout.
            assert_eq!(
                1,
                layouts
                    .iter()
                    .filter(|layout| layout.step_mode() == wgpu::VertexStepMode::Instance)
                    .count()
            );

            Self {
                shader,
                layouts: layouts.into_boxed_slice(),
                idx_buf: None,
                vert_bufs: Vec::new(),
            }
        }

        pub(crate) fn get_layouts(&self) -> &[VertexLayout] {
            &self.layouts
        }

        pub(crate) fn create_vertex_state_buffers(&self) -> Vec<wgpu::VertexBufferLayout> {
            self.layouts
                .iter()
                .map(|layout| layout.create_layout())
                .collect()
        }

        pub(crate) fn as_vertex_state<'a>(
            &'a self,
            buffers: &'a [wgpu::VertexBufferLayout],
        ) -> wgpu::VertexState {
            wgpu::VertexState {
                module: &self.shader.get_module(),
                // Safety: We checked it out at Self::new().
                entry_point: unsafe { self.shader.get_vertex_stage().unwrap_unchecked() },
                buffers,
            }
        }

        /// If `win` is None, the whole range of the buffer is used.
        pub(crate) fn set_index_buffer(
            &mut self,
            buf: BufferRc,
            win: Option<Window>,
            format: wgpu::IndexFormat,
        ) {
            let item_size: usize = match format {
                wgpu::IndexFormat::Uint16 => 2,
                wgpu::IndexFormat::Uint32 => 4,
            };
            // Safety: Infallible.
            let item_size = unsafe { NonZeroUsize::new_unchecked(item_size) };
            let buf = BufferView::new(buf, item_size, win);
            self.idx_buf = Some((IndexBuffer::new(buf), format));
        }

        /// It's possible to append vertex buffer more than the number of layouts.
        /// But in that case, you must pass vertex buffer.
        pub(crate) fn append_vertex_buffer(
            &mut self,
            buf: Or<(BufferRc, Option<Window>), VertexBuffer>,
        ) {
            let buf = match buf {
                Or::A((buf, win)) => {
                    let index = self.vert_bufs.len();
                    let layout = self.layouts.get(index).unwrap();
                    let item_size = layout.calc_array_stride() as usize;
                    let item_size = NonZeroUsize::new(item_size).unwrap();
                    let buf = BufferView::new(buf, item_size, win);
                    VertexBuffer::new(buf)
                }
                Or::B(buf) => buf,
            };
            self.vert_bufs.push(buf);
        }

        pub(crate) fn build(mut this: VertexStateRef) -> VertexCommandSet {
            let state = this.clone();
            let mut _self = this.borrow_mut();

            // Takes index buffer out.
            let (buf, format) = _self.idx_buf.take().unwrap();

            // Creates index command.
            let idx_cmd = IndexBufferCommand::from_buffer(state.clone(), buf, format);

            // Allows making compatible `VertexCommandSet` that may have extra vertex buffers.
            // So, number of vertex buffers must be greater or equal to layout's.
            assert!(_self.vert_bufs.len() >= _self.layouts.len());

            // Takes vertex buffers out.
            let vert_bufs = mem::take(&mut _self.vert_bufs);

            // Creates vertex commands.
            let vert_cmds = vert_bufs
                .into_iter()
                .enumerate()
                .map(|(slot, buf)| {
                    VertexBufferCommand::from_buffer(state.clone(), buf, slot as u32)
                })
                .collect::<Vec<_>>();
            drop(_self);

            VertexCommandSet {
                idx_cmd,
                vert_cmds: vert_cmds.into_boxed_slice(),
            }
        }
    }

    impl PartialEq for VertexState {
        fn eq(&self, other: &Self) -> bool {
            let Self {
                shader: this_shader,
                layouts: this_layouts,
                idx_buf: _this_idx_buf, // Index buffer is temporary building data.
                vert_bufs: _this_vert_bufs, // Vertex buffer is temporary building data.
            } = self;

            let Self {
                shader: other_shader,
                layouts: other_layouts,
                idx_buf: _other_idx_buf,
                vert_bufs: _other_vert_bufs,
            } = other;

            // Is shader the same? Then, is vertex layout the same?
            Shader::is_same(this_shader, other_shader) && this_layouts == other_layouts
        }
    }

    impl Eq for VertexState {}

    impl Hash for VertexState {
        fn hash<H: Hasher>(&self, state: &mut H) {
            let Self {
                shader,
                layouts,
                idx_buf: _idx_buf,     // Index buffer is temporary building data.
                vert_bufs: _vert_bufs, // Vertex buffer is temporary building data.
            } = self;

            Shader::hash(shader, state);
            layouts.hash(state);
        }
    }

    #[derive(Debug, Clone)]
    pub struct VertexLayout {
        /// Read only vertex buffer attributes.
        attrs: Box<[wgpu::VertexAttribute]>,

        /// Read only vertex buffer step mode.
        step_mode: wgpu::VertexStepMode,
    }

    impl VertexLayout {
        pub fn new(attrs: Vec<wgpu::VertexAttribute>, step_mode: wgpu::VertexStepMode) -> Self {
            Self {
                attrs: attrs.into_boxed_slice(),
                step_mode,
            }
        }

        pub fn get_attribute(&self) -> &[wgpu::VertexAttribute] {
            &self.attrs
        }

        pub fn step_mode(&self) -> wgpu::VertexStepMode {
            self.step_mode
        }

        /// # Panics
        ///
        /// Panics if vertex attributes are not set.
        pub(crate) fn create_layout(&self) -> wgpu::VertexBufferLayout {
            assert!(!self.attrs.is_empty());
            wgpu::VertexBufferLayout {
                array_stride: self.calc_array_stride(),
                step_mode: self.step_mode(),
                attributes: self.get_attribute(),
            }
        }

        /// Assumes no padding.
        pub(crate) fn calc_array_stride(&self) -> u64 {
            self.get_attribute()
                .iter()
                .map(|attr| attr.offset + attr.format.size())
                .max()
                .unwrap_or_default()
        }
    }

    impl PartialEq for VertexLayout {
        fn eq(&self, other: &Self) -> bool {
            let Self {
                attrs: this_attrs,
                step_mode: this_step_mode,
            } = self;

            let Self {
                attrs: other_attrs,
                step_mode: other_step_mode,
            } = other;

            this_attrs == other_attrs && this_step_mode == other_step_mode
        }
    }

    impl Eq for VertexLayout {}

    impl Hash for VertexLayout {
        fn hash<H: Hasher>(&self, state: &mut H) {
            let Self { attrs, step_mode } = self;

            attrs.hash(state);
            step_mode.hash(state);
        }
    }
}

/// Render module consists of Resource(res), Description(desc), and Execution(exe) layers.
/// Resource layer is responsible for holding GPU relative data.
pub(crate) mod res {}
