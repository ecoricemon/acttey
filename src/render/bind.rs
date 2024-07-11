use super::{
    buffer::desc::{StorageBuffer, UniformBuffer},
    context::Gpu,
};
use crate::{
    common::AppHasher,
    ds::{
        refs::WithRc,
        share::{DebugLock, SharedVec, SharedVecItem, SharedVecOrigin},
    },
    impl_from_for_enum,
    util::Or,
};
use desc::*;
use exe::*;
use my_ecs::ds::prelude::*;
use std::{
    cell::Cell,
    hash::{Hash, Hasher},
    mem,
    num::NonZeroU64,
    rc::Rc,
    sync::Arc,
};

thread_local! {
    // For debugging.
    // For now, all shared variables in this module share this counter to detect invalid mutable borrows.
    // This may change in the future.
    #[cfg(debug_assertions)]
    pub(crate) static BIND_MOD_DEBUG_LOCK: Cell<i32> = const { Cell::new(0) };
}

#[derive(Debug)]
pub struct BindStateManager {
    gpu: Rc<Gpu>,

    /// Pointer to the vector inside [`Self::org_binds`].
    pub(crate) binds: SharedVec<BindStateRc, AppHasher>,

    /// Pointer to the vector inside [`Self::org_groups`].
    pub(crate) groups: SharedVec<BindGroupStateRc, AppHasher>,

    /// Actual vector of [`BindState`].
    /// This field is not accessed and just keeps vector alive.
    org_binds: SharedVecOrigin<BindStateRc, AppHasher>,

    /// Actual vector of [`BindGroupState`].
    /// This field is not accessed and just keeps vector alive.
    org_groups: SharedVecOrigin<BindGroupStateRc, AppHasher>,
}

impl BindStateManager {
    pub(crate) fn new(gpu: Rc<Gpu>) -> Self {
        let (binds, org_binds) = SharedVec::new();
        let (groups, org_groups) = SharedVec::new();

        Self {
            gpu,
            binds,
            groups,
            org_binds,
            org_groups,
        }
    }

    /// # Panics
    ///
    /// See [`SharedVecItem::new`].
    pub(crate) fn get_bind_state(&self, index: usize) -> BindStateRef {
        DebugLock::new(
            SharedVecItem::new(self.binds, index),
            #[cfg(debug_assertions)]
            &BIND_MOD_DEBUG_LOCK,
        )
    }

    /// # Panics
    ///
    /// See [`SharedVecItem::new`].
    pub(crate) fn get_bind_group_state(&self, index: usize) -> BindGroupStateRef {
        DebugLock::new(
            SharedVecItem::new(self.groups, index),
            #[cfg(debug_assertions)]
            &BIND_MOD_DEBUG_LOCK,
        )
    }
}

impl StoreBindState for BindStateManager {
    fn add_bind_state(&mut self, desc: BindStateDesc) -> BindStateRef {
        // Creates bind group states if it's needed.
        let mut groups = [None, None, None, None];
        for (i, opt) in desc.groups.into_iter().enumerate() {
            let group = opt.map(|group_or_desc| match group_or_desc {
                Or::A(group) => group,
                Or::B(desc) => self.add_bind_group_state(desc),
            });
            groups[i] = group;
        }

        // Creates bind state.
        let state = BindState::new(groups);
        let index = self.binds.add(WithRc::new(state));

        DebugLock::new(
            SharedVecItem::new(self.binds, index),
            #[cfg(debug_assertions)]
            &BIND_MOD_DEBUG_LOCK,
        )
    }

    fn remove_bind_state(&mut self, state: BindStateRef) -> Result<BindState, BindStateRef> {
        let state = state.into_inner();
        self.binds
            .remove(state)
            .map(BindStateRc::into_inner)
            .map_err(|state| {
                DebugLock::new(
                    state,
                    #[cfg(debug_assertions)]
                    &BIND_MOD_DEBUG_LOCK,
                )
            })
    }
}

impl StoreBindGroupState for BindStateManager {
    fn add_bind_group_state(&mut self, desc: BindGroupStateDesc) -> BindGroupStateRef {
        let state = BindGroupState::new(Rc::clone(&self.gpu), desc.label, desc.layout_entries);
        let index = self.groups.add(WithRc::new(state));

        DebugLock::new(
            SharedVecItem::new(self.groups, index),
            #[cfg(debug_assertions)]
            &BIND_MOD_DEBUG_LOCK,
        )
    }

    fn remove_bind_group_state(
        &mut self,
        state: BindGroupStateRef,
    ) -> Result<BindGroupState, BindGroupStateRef> {
        let state = state.into_inner();
        self.groups
            .remove(state)
            .map(BindGroupStateRc::into_inner)
            .map_err(|state| {
                DebugLock::new(
                    state,
                    #[cfg(debug_assertions)]
                    &BIND_MOD_DEBUG_LOCK,
                )
            })
    }
}

/// Render module consists of Resource(res), Description(desc), and Execution(exe) layers.
/// Execution layer is responsible for executing render or compute passes.
pub(crate) mod exe {
    use super::*;

    /// A temporary set of bind group commands generated from a [`BindState`].
    #[derive(Debug, Clone)]
    pub(crate) struct BindCommandSet {
        pub(crate) cmds: Box<[BindGroupCommand]>,
    }

    #[derive(Debug)]
    pub(crate) struct BindGroupCommand {
        /// Command builder and data used when the command was built.
        //
        // This is infrequently accessed and we need to reduce each command size due to enum.
        // So we use `Rc`.
        org: Rc<BindGroupCommandOrigin>,

        // This field is a frequently accessed value.
        // So, although we could easily clone this using `Rc`, we don't do that.
        group: wgpu::BindGroup,

        /// Bind group index.
        index: u32,
    }

    impl BindGroupCommand {
        pub(crate) fn new(
            org: Rc<BindGroupCommandOrigin>,
            group: wgpu::BindGroup,
            index: u32,
        ) -> Self {
            debug_assert!(index < 4);
            assert_eq!(
                org.state.as_ref().get_layout_entries().len(),
                org.entries.len()
            );

            Self { org, group, index }
        }

        pub(crate) fn index(&self) -> u32 {
            self.index
        }

        pub(crate) fn execute_render_pass<'a: 'b, 'b>(&'a self, pass: &mut wgpu::RenderPass<'b>) {
            pass.set_bind_group(self.index, &self.group, &[]);
        }
    }

    impl Clone for BindGroupCommand {
        // Builds new command that has the same data.
        fn clone(&self) -> Self {
            // Clones origin.
            let mut state = self.org.state.clone();
            let entries = self.org.entries.clone();

            // Fills binding entries.
            let mut _state = state.borrow_mut();
            for entry in entries {
                _state.append_bind_group_entry(entry);
            }
            drop(_state);

            // Builds a new command.
            let mut cmd = BindGroupState::build(state, self.index);

            // The command has its own origin, but it can be shared with this one.
            cmd.org = Rc::clone(&self.org);

            cmd
        }
    }

    impl PartialEq for BindGroupCommand {
        fn eq(&self, other: &Self) -> bool {
            // In command equality test, it's enough to test data only.
            let Self {
                org: this_org,
                group: _this_group, // We can't compare wgpu's data directly.
                index: this_index,
            } = self;

            let Self {
                org: other_org,
                group: _other_group,
                index: other_index,
            } = other;

            this_org.entries == other_org.entries && this_index == other_index
        }
    }

    impl Eq for BindGroupCommand {}

    impl Hash for BindGroupCommand {
        fn hash<H: Hasher>(&self, state: &mut H) {
            let Self {
                org,
                group: _group, // We can't use wgpu's data directly.
                index,
            } = self;

            org.entries.hash(state);
            index.hash(state);
        }
    }

    #[derive(Debug)]
    pub(crate) struct BindGroupCommandOrigin {
        /// The origin of this command.
        pub(crate) state: BindGroupStateRef,

        /// List of [`Binding`] for cloning.
        /// It's guaranteed to have the same number of entries as the [`BindGroupState`]'s layout.
        pub(crate) entries: Vec<Binding>,
    }
}

/// Render module consists of Resource(res), Description(desc), and Execution(exe) layers.
/// Description layer is responsible for describing GPU state using something like pipeline.
pub(crate) mod desc {
    use super::*;

    /// Bind state insertion and removal interfaces.
    pub trait StoreBindState {
        fn add_bind_state(&mut self, desc: BindStateDesc) -> BindStateRef;
        fn remove_bind_state(&mut self, state: BindStateRef) -> Result<BindState, BindStateRef>;
    }

    /// Bind group state insertion and removal interfaces.
    pub trait StoreBindGroupState {
        fn add_bind_group_state(&mut self, desc: BindGroupStateDesc) -> BindGroupStateRef;
        fn remove_bind_group_state(
            &mut self,
            state: BindGroupStateRef,
        ) -> Result<BindGroupState, BindGroupStateRef>;
    }

    #[derive(Debug)]
    pub struct BindStateDesc {
        pub groups: [Option<Or<BindGroupStateRef, BindGroupStateDesc>>; 4],
    }

    #[derive(Debug)]
    pub struct BindGroupStateDesc {
        pub label: Arc<str>,
        pub layout_entries: Vec<wgpu::BindGroupLayoutEntry>,
    }

    /// Writable reference to a bind state.
    pub(crate) type BindStateRef = DebugLock<SharedVecItem<BindStateRc, AppHasher>, BindStateRc>;

    /// Shared bind state.
    pub(crate) type BindStateRc = WithRc<BindState>;

    #[derive(Debug)]
    pub(crate) struct BindState {
        /// Each bind group state has its read only layout entries.
        /// And, because bind group must start from index 0 and it can't jump in terms of index,
        /// this array doesn't allow None followed by Some.
        groups: [Option<BindGroupStateRef>; 4],
    }

    impl BindState {
        pub(crate) fn new(groups: [Option<BindGroupStateRef>; 4]) -> Self {
            // `groups` doesn't allow None in the middle of Some.
            let invalid = groups
                .iter()
                .zip(groups.iter().skip(1))
                .any(|(prev, cur)| prev.is_none() && cur.is_some());
            assert!(!invalid);

            Self { groups }
        }

        pub(crate) fn create_bind_group_layouts(&self) -> Vec<&wgpu::BindGroupLayout> {
            self.groups
                .iter()
                .take_while(|opt| opt.is_some())
                .flat_map(|opt| {
                    opt.as_ref()
                        .map(|group_state| group_state.as_ref().get_layout())
                })
                .collect()
        }

        pub(crate) fn build(this: BindStateRef) -> BindCommandSet {
            let _self = this.as_ref();

            // Builds each `BindGroupCommand`.
            let cmds = _self
                .groups
                .iter()
                .enumerate()
                .map_while(|(index, opt)| {
                    opt.as_ref().map(|group_state| {
                        let group_state = group_state.clone();
                        BindGroupState::build(group_state, index as u32)
                    })
                })
                .collect::<Vec<_>>();

            BindCommandSet {
                cmds: cmds.into_boxed_slice(),
            }
        }
    }

    impl PartialEq for BindState {
        fn eq(&self, other: &Self) -> bool {
            self.groups == other.groups
        }
    }

    impl Eq for BindState {}

    impl Hash for BindState {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.groups.hash(state);
        }
    }

    /// Writable reference to a bind group state.
    pub(crate) type BindGroupStateRef =
        DebugLock<SharedVecItem<BindGroupStateRc, AppHasher>, BindGroupStateRc>;

    /// Shared bind group state.
    pub(crate) type BindGroupStateRc = WithRc<BindGroupState>;

    #[derive(Debug)]
    pub(crate) struct BindGroupState {
        gpu: Rc<Gpu>,

        /// Common label for the builder.
        label: Arc<str>,

        /// Read only layout entries.
        /// They are sorted by their binding numbers.
        layout_entries: Box<[wgpu::BindGroupLayoutEntry]>,

        /// Read only layout made of [`Self::layout_entries`].
        layout: wgpu::BindGroupLayout,

        /// Temporary writable bind group entries such as buffers, samplers, or textures.
        /// Once a bind group is built, entries are removed to release references.
        entries: Vec<Binding>,
    }

    impl BindGroupState {
        pub(crate) fn new(
            gpu: Rc<Gpu>,
            label: Arc<str>,
            mut layout_entries: Vec<wgpu::BindGroupLayoutEntry>,
        ) -> Self {
            // Sorts layout entries by their binding numbers.
            layout_entries.sort_unstable_by_key(|entry| entry.binding);
            for i in 1..layout_entries.len() {
                assert_ne!(
                    layout_entries[i - 1].binding,
                    layout_entries[i].binding,
                    "duplicate binding"
                );
            }

            // Builds layout.
            let layout = gpu
                .device
                .create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                    label: Some(&label),
                    entries: &layout_entries,
                });

            Self {
                gpu,
                label,
                layout_entries: layout_entries.into_boxed_slice(),
                layout,
                entries: Vec::new(),
            }
        }

        pub(crate) fn get_label(&self) -> &Arc<str> {
            &self.label
        }

        pub(crate) fn get_layout_entries(&self) -> &[wgpu::BindGroupLayoutEntry] {
            &self.layout_entries
        }

        pub(crate) fn get_layout(&self) -> &wgpu::BindGroupLayout {
            &self.layout
        }

        pub(crate) fn append_bind_group_entry(&mut self, entry: Binding) {
            // Before we append the entry, let's check it out with the corresponding layout entry.
            let index = self.entries.len();
            let layout_entry = self.layout_entries.get(index).unwrap();
            match (&entry, layout_entry.ty) {
                // Both are uniform buffer binding, then Okay.
                (
                    Binding::Buffer(BufferBinding(Or::A(_uni))),
                    wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Uniform,
                        ..
                    },
                ) => {}
                // Both are storage buffer binding, then Okay.
                (
                    Binding::Buffer(BufferBinding(Or::B(_stor))),
                    wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Storage { .. },
                        ..
                    },
                ) => {}
                // Otherwise, we can't proceed.
                _ => {
                    panic!("BufferBinding can be made of different types of buffer and descriptor")
                }
            }

            self.entries.push(entry);
        }

        pub(crate) fn build(mut this: BindGroupStateRef, index: u32) -> BindGroupCommand {
            let mut _self = this.borrow_mut();

            // Creates bind group.
            let bindings = _self.layout_entries.iter().map(|entry| entry.binding);
            let resources = _self
                .entries
                .iter()
                .map(|entry| entry.as_binding_resource());
            let entries = bindings
                .zip(resources)
                .map(|(binding, resource)| wgpu::BindGroupEntry { binding, resource })
                .collect::<Vec<_>>();
            let group = _self
                .gpu
                .device
                .create_bind_group(&wgpu::BindGroupDescriptor {
                    label: Some(&_self.label),
                    layout: &_self.layout,
                    entries: &entries,
                });

            // Takes entries out.
            let entries = mem::take(&mut _self.entries);
            drop(_self);

            // Panics if entry numbers don't match to each other.
            let org = Rc::new(BindGroupCommandOrigin {
                state: this,
                entries,
            });
            BindGroupCommand::new(org, group, index)
        }
    }

    impl PartialEq for BindGroupState {
        fn eq(&self, other: &Self) -> bool {
            let Self {
                gpu: _this_gpu,     // No need to compare.
                label: _this_label, // No need to compare.
                layout_entries: this_layout_entries,
                layout: _this_layout,   // We can't compare wgpu's data directly.
                entries: _this_entries, // Temporary data for building.
            } = self;

            let Self {
                gpu: _other_gpu,
                label: _other_label,
                layout_entries: other_layout_entries,
                layout: _other_layout,
                entries: _other_entries,
            } = other;

            this_layout_entries == other_layout_entries
        }
    }

    impl Eq for BindGroupState {}

    impl Hash for BindGroupState {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.layout_entries.hash(state);
        }
    }

    /// Corresponds to [`wgpu::BindingResource`].
    /// In WebGPU spec, it seems there's only one handle for a single binding
    /// [WebGPU spec](https://www.w3.org/TR/webgpu/#typedefdef-gpubindingresource).
    /// But, wgpu has features to have multiple handles for a single binding.
    /// Array variants exist just for that, it may be implemented for the future.
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub(crate) enum Binding {
        Buffer(BufferBinding),
        // BufferArray(),
        // Sampler(),
        // SamplerArray(),
        // TextureView(),
        // TextureViewArray(),
    }
    impl_from_for_enum!(Binding, Buffer, BufferBinding);

    impl Binding {
        pub(crate) fn as_binding_resource(&self) -> wgpu::BindingResource {
            match self {
                Self::Buffer(buf) => buf.as_binding_resource(),
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub(crate) struct BufferBinding(Or<UniformBuffer, StorageBuffer>);

    impl BufferBinding {
        pub(crate) const fn new(buf: Or<UniformBuffer, StorageBuffer>) -> Self {
            Self(buf)
        }

        pub(crate) fn as_binding_resource(&self) -> wgpu::BindingResource {
            let (buffer, view) = match &self.0 {
                Or::A(uni) => (uni.get_buffer(), uni.get_view()),
                Or::B(stor) => (stor.get_buffer(), stor.get_view()),
            };
            wgpu::BindingResource::Buffer(wgpu::BufferBinding {
                buffer,
                offset: view.byte_offset() as u64,
                size: NonZeroU64::new(view.size() as u64),
            })
        }
    }
}

/// Render module consists of Resource(res), Description(desc), and Execution(exe) layers.
/// Resource layer is responsible for holding GPU relative data.
pub(crate) mod res {}
