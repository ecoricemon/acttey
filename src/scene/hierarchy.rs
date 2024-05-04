use crate::{
    ds::{buf::VarChunkBuffer, graph::DirectedGraph},
    ecs::entity::EntityId,
    primitive::matrix::Matrix4f,
    util::{key::ObjectKey, View},
};
use std::{hash::Hash, mem, ops::Range};

/// Represents scene's node transformation tree structure.  
#[derive(Debug, Clone)]
pub struct SceneHierarchy {
    /// Node tree.
    nodes: DirectedGraph<Node>,

    /// Seperated local transformation matrix of the [`Node`].
    /// This field shares the same index with the [`Self::nodes`].
    //
    // NOTE: Users can manipulate local transformation matrix manually.
    // So the matrix is seperated from `Node` to be shared across threads.
    locals: Vec<Matrix4f>,

    /// Global transformation matrix of the [`Node`].
    /// [`Node::tf_idx`] points to a specific item in this field.
    globals: GlobalTransform,

    /// Not validated node.
    invalid_nodes: Vec<usize>,

    /// Indices of nodes that have changed their transformation matrices.
    /// Indices can be duplicate.
    dirties: Vec<usize>,
}

impl SceneHierarchy {
    pub fn new() -> Self {
        // Creates local transformation matrices with default root node's matrix.
        let locals = vec![Matrix4f::identity()];

        // Creates global transformation matrices.
        let mut globals = GlobalTransform::new();

        // Creates node tree.
        let mut nodes = DirectedGraph::new();

        // Root node has global identity transformation matrix, even if it's actually not used.
        let root: &mut Node = nodes.get_root_mut();
        root.tf_idx = globals.insert(None, Matrix4f::identity());

        Self {
            nodes,
            locals,
            globals,
            invalid_nodes: Vec::new(),
            dirties: Vec::new(),
        }
    }

    /// Adds default [`Node`] as a child of `parent` node.
    /// You can put in '0' for *no* parent node.
    /// This function returns index of the node, and you can access the node using the index.
    /// The index will never change after insertion or deletion in front of the node.
    pub fn add_node(&mut self, parent: usize) -> usize {
        // Adds node.
        let index = self.nodes.insert_node(Node::default());

        // Adds transformation matrix for the node.
        if self.locals.len() < (index + 1) {
            self.locals.resize(index + 1, Matrix4f::identity());
        }

        // Connects the node to the parent.
        self.nodes.add_edge(parent, index);

        // Added node must be validated later.
        self.invalid_nodes.push(index);

        index
    }

    /// Retrieves the node.
    #[inline]
    pub fn get_node(&self, index: usize) -> Option<&Node> {
        self.nodes.get_node(index)
    }

    /// Retrieves the node.
    pub fn get_node_mut(&mut self, index: usize) -> Option<&mut Node> {
        self.invalid_nodes.push(index);
        self.nodes.get_node_mut(index)
    }

    pub fn get_local_mut(&mut self, index: usize) -> Option<&mut Matrix4f> {
        if self.nodes.is_valid(index) {
            // Safety: Checked the validity.
            unsafe { Some(self.get_local_unchecked_mut(index)) }
        } else {
            None
        }
    }

    pub unsafe fn get_local_unchecked_mut(&mut self, index: usize) -> &mut Matrix4f {
        let node = self.nodes.get_node_unchecked_mut(index);
        node.dirty_transform();
        self.dirties.push(index);
        self.locals.get_unchecked_mut(index)
    }

    /// Retrieves iterator traversing all index-node pairs.
    #[inline]
    pub fn iter_node(&self) -> impl Iterator<Item = (usize, &Node)> {
        self.nodes.iter_node()
    }

    /// Retrieves transforrm matrix buffer view for instancing.
    #[inline]
    pub fn get_transform_buffer_view(&self) -> View {
        View::new(
            0,
            self.globals.mesh_buffer_len(),
            mem::size_of::<Matrix4f>(),
        )
    }

    #[inline]
    pub fn get_transform_buffer_as_bytes(&self) -> &[u8] {
        self.globals.mesh_buffer_bytes()
    }

    /// Retrieves a particular range of buffer corresponding to the `key`.
    #[inline]
    pub fn get_transform_buffer_range<Q>(&self, key: &Q) -> Option<Range<usize>>
    where
        ObjectKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.globals.mesh_buffer_range(key)
    }

    /// Validates nodes that may be updated.
    pub fn validate(&mut self) {
        while let Some(i) = self.invalid_nodes.pop() {
            self.validate_tf(i);
        }
    }

    /// Updates node's transformation matrices.
    pub fn update_global_transform(&mut self) {
        unsafe fn update(
            this: &mut SceneHierarchy,
            node_idx: usize,
            parent_idx: usize,
            mut need_update: bool,
        ) {
            let node = this.nodes.get_node_unchecked(node_idx);
            need_update |= node.dirty.contains(NodeDirty::TRANSFORM);
            if need_update {
                // TODO: improve
                if parent_idx > 0 {
                    let parent_mat = *this
                        .globals
                        .get(&this.nodes.get_node_unchecked(parent_idx).tf_idx);
                    let global_mat = this
                        .globals
                        .get_mut(&this.nodes.get_node_unchecked(node_idx).tf_idx);
                    let local_mat = this.locals.get_unchecked(node_idx);
                    *global_mat = &parent_mat * local_mat;
                } else {
                    let global_mat = this
                        .globals
                        .get_mut(&this.nodes.get_node_unchecked(node_idx).tf_idx);
                    let local_mat = this.locals.get_unchecked(node_idx);
                    *global_mat = *local_mat;
                }

                let node = this.nodes.get_node_unchecked_mut(node_idx);
                node.dirty.remove(NodeDirty::TRANSFORM);
            }
        }

        while let Some(node_idx) = self.dirties.pop() {
            // Safety: node_idx is valid because we got it from the dirty records.
            // Plus, the node must have only one parent, although it's a child of default root node.
            unsafe {
                let parent = self.nodes.get_inbound_unchecked(node_idx);
                debug_assert_eq!(1, parent.len());
                let parent_idx = parent.iter().next().unwrap_unchecked();
                update(self, node_idx, *parent_idx, false);
            }
        }
    }

    /// Validates transform matrices by newly creating or moving them.
    fn validate_tf(&mut self, node_idx: usize) {
        if let Some(node) = self.nodes.get_node_mut(node_idx) {
            // Checks if this node should be validated.
            const CHECK: NodeDirty = NodeDirty::CREATED.union(NodeDirty::MESH);
            if !node.dirty.intersects(CHECK) {
                return;
            }
            node.dirty.remove(CHECK);

            match (
                node.tf_idx.is_mesh_index(),
                node.tf_idx.is_non_mesh_index(),
                node.get_mesh_key(),
            ) {
                // New node w/ or w/o mesh.
                (false, false, mesh_key) => {
                    let mesh_key = mesh_key.cloned();
                    node.tf_idx = self.globals.insert(mesh_key, Matrix4f::identity());
                }
                // w/o mesh -> w/ mesh. (mesh has attached)
                (false, true, _mesh_key) => {
                    todo!()
                }
                // w/ mesh -> w/o mesh. (mesh has detached)
                (true, false, None) => {
                    todo!()
                }
                // w/ mesh -> w/ mesh. (mesh has changed)
                (true, false, Some(_mesh_key)) => {
                    todo!()
                }
                (true, true, _) => {
                    unreachable!()
                }
            }
        }
    }
}

impl Default for SceneHierarchy {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub(super) struct GlobalTransform {
    /// Global transformation matrices.
    /// mats[0] contains matrices for mesh nodes only.
    /// On the other hand, mats[1] contains non-mesh node matrices.
    mats: [VarChunkBuffer<ObjectKey, Matrix4f>; 2],
}

impl GlobalTransform {
    const MESH_BUF_IDX: usize = 0;
    const NON_MESH_BUF_IDX: usize = 1;

    pub(super) fn new() -> Self {
        Self {
            mats: [VarChunkBuffer::new(), VarChunkBuffer::new()],
        }
    }

    #[inline]
    pub(super) fn mesh_buffer_len(&self) -> usize {
        self.mats[Self::MESH_BUF_IDX].len()
    }

    #[inline]
    pub(super) fn mesh_buffer_bytes(&self) -> &[u8] {
        (&self.mats[Self::MESH_BUF_IDX]).into()
    }

    #[inline]
    pub(super) fn mesh_buffer_range<Q>(&self, key: &Q) -> Option<Range<usize>>
    where
        ObjectKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.mats[Self::MESH_BUF_IDX]
            .get_chunk_window2(key)
            .map(|win| win.range())
    }

    pub(super) fn insert(
        &mut self,
        mesh_key: Option<ObjectKey>,
        mat: Matrix4f,
    ) -> GlobalTransformIndex {
        let (buf_idx, mesh_key) = if let Some(mesh_key) = mesh_key {
            (Self::MESH_BUF_IDX, mesh_key)
        } else {
            (Self::NON_MESH_BUF_IDX, ObjectKey::default())
        };
        let chunk_idx = self.mats[buf_idx].insert_new(mesh_key, mat);
        let item_idx = self.mats[buf_idx].chunk_len(chunk_idx) - 1;
        GlobalTransformIndex {
            buf_idx,
            chunk_idx,
            item_idx,
        }
    }

    #[inline]
    pub(super) fn get(&self, index: &GlobalTransformIndex) -> &Matrix4f {
        self.mats[index.buf_idx].get_item(index.chunk_idx, index.item_idx)
    }

    #[inline]
    pub(super) fn get_mut(&mut self, index: &GlobalTransformIndex) -> &mut Matrix4f {
        self.mats[index.buf_idx].get_item_mut(index.chunk_idx, index.item_idx)
    }
}

impl Default for GlobalTransform {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) struct GlobalTransformIndex {
    pub(super) buf_idx: usize,
    pub(super) chunk_idx: usize,
    pub(super) item_idx: usize,
}

impl GlobalTransformIndex {
    const DUMMY_IDX: usize = usize::MAX;

    pub(super) const fn dummy() -> Self {
        Self {
            buf_idx: Self::DUMMY_IDX,
            chunk_idx: 0,
            item_idx: 0,
        }
    }

    pub(super) const fn is_dummy(&self) -> bool {
        self.buf_idx == Self::DUMMY_IDX
    }

    pub(super) const fn is_mesh_index(&self) -> bool {
        self.buf_idx == GlobalTransform::MESH_BUF_IDX
    }

    pub(super) const fn is_non_mesh_index(&self) -> bool {
        self.buf_idx == GlobalTransform::NON_MESH_BUF_IDX
    }
}

#[derive(Debug, Clone)]
pub struct Node {
    /// Index to the global transformation matrix.
    tf_idx: GlobalTransformIndex,

    /// Optional camera key.
    camera: Option<ObjectKey>,

    /// Optional mesh key and its corresponding chunk index.
    mesh: Option<ObjectKey>,

    /// Optional mapping with ECS's entity.
    entity: Option<EntityId>,

    /// Dirty flag.
    dirty: NodeDirty,
}

impl Node {
    /// Sets camera key.
    #[inline]
    pub fn set_camera(&mut self, camera: ObjectKey) {
        self.camera = Some(camera);
        self.dirty |= NodeDirty::CAMERA;
    }

    /// Gets camera key.
    #[inline]
    pub fn get_camera(&self) -> Option<&ObjectKey> {
        self.camera.as_ref()
    }

    /// Sets mesh key.
    #[inline]
    pub fn set_mesh(&mut self, mesh: ObjectKey) {
        self.mesh = Some(mesh);
        self.dirty |= NodeDirty::MESH;
    }

    /// Retrieves mesh key.
    #[inline]
    pub fn get_mesh_key(&self) -> Option<&ObjectKey> {
        self.mesh.as_ref()
    }

    /// Sets mapping with ECS's entity.
    #[inline]
    pub fn set_entity(&mut self, eid: EntityId) {
        self.entity = Some(eid);
        self.dirty |= NodeDirty::ENTITY;
    }

    #[inline]
    pub fn dirty_transform(&mut self) {
        self.dirty |= NodeDirty::TRANSFORM;
    }

    /// Gets mapped ECS's entity key and its id.
    #[inline]
    pub fn get_mapped_entity(&self) -> Option<EntityId> {
        self.entity
    }
}

impl Default for Node {
    fn default() -> Self {
        Self {
            tf_idx: GlobalTransformIndex::dummy(),
            camera: None,
            mesh: None,
            entity: None,
            dirty: NodeDirty::default(),
        }
    }
}

bitflags::bitflags! {
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct NodeDirty: u8 {
        /// Just created.
        const CREATED = 1 << 0;

        /// Transformation matrix has changed.
        const TRANSFORM = 1 << 1;

        /// Camera has changed.
        const CAMERA = 1 << 2;

        /// Mesh key has changed.
        const MESH = 1 << 3;

        /// Entity mapping has changed.
        const ENTITY = 1 << 4;
    }
}

impl NodeDirty {
    /// Resets all bits.
    #[inline]
    pub fn clear(&mut self) {
        *self = Self::empty();
    }
}

impl Default for NodeDirty {
    #[inline]
    fn default() -> Self {
        Self::CREATED
    }
}
