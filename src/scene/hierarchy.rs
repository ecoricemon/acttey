use crate::{
    ds::{
        buf::VarChunkBuffer,
        graph::DirectedGraph,
        vec::{OptVec, SetVec},
    },
    primitive::matrix::Matrix4f,
    util::{key::ResKey, slice::update_slice_by, Index2, View},
};
use std::{mem::size_of, num::NonZeroUsize};

/// Represents scene's node transformation tree structure.  
#[derive(Debug, Clone)]
pub struct SceneHierarchy {
    /// Scene's hierarchy is represented as a tree.
    tree: DirectedGraph<Node>,

    /// Global transformation matrices per mesh node.
    mesh_tfs: VarChunkBuffer<ResKey, Matrix4f>,

    /// Global transformation matrices per non-mesh node.
    non_mesh_tfs: OptVec<Matrix4f>,

    /// likely updated so that not valid nodes.
    invalid_nodes: Vec<usize>,
}

impl SceneHierarchy {
    pub fn new() -> Self {
        Self {
            tree: DirectedGraph::new(),
            mesh_tfs: VarChunkBuffer::with_capacity(64),
            non_mesh_tfs: OptVec::new(0),
            invalid_nodes: Vec::new(),
        }
    }

    /// Adds default [`Node`] as a child of `parent` node.
    /// You can put in '0' for *no* parent node.
    /// This function returns index of the node, and you can access the node using the index.
    /// The index will never change after insertion or deletion in front of the node.
    pub fn add_node(&mut self, parent: usize) -> usize {
        let index = self.tree.insert_node(Node::default());
        self.tree.add_edge(parent, index);
        self.invalid_nodes.push(index);
        index
    }

    /// Retrieves the node.
    #[inline]
    pub fn get_node(&self, index: usize) -> Option<&Node> {
        self.tree.get_node(index)
    }

    /// Retrieves the node.
    #[inline]
    pub fn get_node_mut(&mut self, index: usize) -> Option<&mut Node> {
        // Can be updated outside.
        self.invalid_nodes.push(index);
        self.tree.get_node_mut(index)
    }

    /// Retrieves iterator traversing all index-node pairs.
    #[inline]
    pub fn iter_nodes(&self) -> impl Iterator<Item = (usize, &Node)> {
        self.tree.iter_nodes()
    }

    /// Retrieves transforrm matrix buffer view for instancing.
    #[inline]
    pub fn get_transform_buffer_view(&self) -> View {
        View::new(0, self.mesh_tfs.len(), size_of::<Matrix4f>())
    }

    #[inline]
    pub fn get_transform_buffer_as_bytes(&self) -> &[u8] {
        (&self.mesh_tfs).into()
    }

    /// Retrieves a particular range of buffer corresponding to the `key`.
    #[inline]
    pub fn get_transform_buffer_range<Q>(&self, key: &Q) -> Option<std::ops::Range<usize>>
    where
        ResKey: std::borrow::Borrow<Q>,
        Q: std::hash::Hash + Eq + ?Sized,
    {
        self.mesh_tfs.get_chunk_window2(key).map(|win| win.range())
    }

    /// Validates nodes that may be updated.
    pub fn validate(&mut self) {
        while let Some(i) = self.invalid_nodes.pop() {
            self.validate_tf(i);
        }
    }

    // TODO: Currently, traverse all nodes to determine what node needs to be updated.
    // Can we skip unmodified nodes?
    /// Updates node's transformation matrices.
    pub fn update_global_transform(&mut self) {
        // Validates nodes first.
        if !self.invalid_nodes.is_empty() {
            self.validate();
        }

        let (nodes, outbounds, _) = self.tree.destruct();

        fn update(
            v: usize,
            p: usize,
            nodes: &mut OptVec<Node>,
            edges: &Vec<SetVec<NonZeroUsize>>,
            mesh_tfs: &mut VarChunkBuffer<ResKey, Matrix4f>,
            non_mesh_tfs: &mut OptVec<Matrix4f>,
            mut need_update: bool,
        ) {
            // Safety: v points to occupied node.
            let node = unsafe { nodes.get_unchecked_mut(v) };
            need_update |= node.dirty.contains(NodeDirty::TRANSFORM);
            if need_update {
                update_slice_by(nodes.as_slice_mut(), v, p, |node, parent| {
                    // Safety: v points to occupied node, but parent is None if p is zero.
                    let node = unsafe { node.as_mut().unwrap_unchecked() };

                    // Updates global matrix using local matrices.
                    if let Some(parent) = parent {
                        node.tf = &parent.tf * &node.tf;
                    }

                    // Writes to the buffer.
                    let target = if node.gtf_index.is_single() {
                        unsafe { non_mesh_tfs.get_unchecked_mut(node.gtf_index.first) }
                    } else {
                        mesh_tfs.get_item_mut(node.gtf_index.second, node.gtf_index.first)
                    };
                    *target = node.tf;

                    // Clears dirty flag.
                    node.dirty.remove(NodeDirty::TRANSFORM);
                });
            }

            for w in edges[v].values_occupied() {
                update(
                    w.get(),
                    v,
                    nodes,
                    edges,
                    mesh_tfs,
                    non_mesh_tfs,
                    need_update,
                );
            }
        }

        for v in outbounds[0].values_occupied() {
            update(
                v.get(),
                0,
                nodes,
                outbounds,
                &mut self.mesh_tfs,
                &mut self.non_mesh_tfs,
                false,
            );
        }
    }

    /// Validates transform matrices by newly creating or moving them.
    fn validate_tf(&mut self, i: usize) {
        if let Some(node) = self.tree.get_node_mut(i) {
            // Checks if this node should be validated.
            const CHECK: NodeDirty = NodeDirty::CREATED.union(NodeDirty::MESH);
            if !node.dirty.intersects(CHECK) {
                return;
            }
            node.dirty.remove(CHECK);

            match (node.gtf_index.get(), node.get_mesh_key()) {
                // Case 1. New node w/o mesh.
                ((None, None), None) => {
                    let ii = self.non_mesh_tfs.add(Matrix4f::default());
                    node.gtf_index.into_single(ii);
                }
                // Case 2. New node w/ mesh.
                ((None, None), Some(key)) => {
                    let ci = self.mesh_tfs.insert_new(key.clone(), Matrix4f::default());
                    let ii = self.mesh_tfs.chunk_len(ci) - 1;
                    node.gtf_index.into_pair(ii, ci);
                }
                // Case 3. w/o mesh -> w/ mesh. (mesh has attached)
                ((Some(ii), None), Some(key)) => {
                    todo!()
                }
                // Case 4. w/ mesh -> w/o mesh. (mesh has detached)
                ((Some(ii), Some(ci)), None) => {
                    todo!()
                }
                // Case 5. w/ mesh -> w/ mesh. (mesh has changed)
                ((Some(ii), Some(ci)), Some(key)) => {
                    todo!()
                }
                _ => {}
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
pub struct Node {
    // Dev note.
    // `tf` is a copy of components::Drawable::transform::tf.
    // The reasons why I've decieded to allow duplication are
    // - ECS system should be able to access local transformation because users may manipulate it directly.
    //   -> Accessing scene mutably is not good because it prevents running systems simultaneously.
    //   -> So that local transfromation belongs to ECS components, not to scene.
    // - Acquisition of individual local transformation from ECS storage is not cheap for now.
    //   -> The process requires a couple of indirect access for finding the item.
    //   -> It's intended to accesss whole items at once.
    // - We should update global transformations at once according to hierarchy.
    //   -> Because node's global transformation depends on either its local transformation and ancestor's global transformations.
    // - As a result, I think duplication is the most cheap way.
    /// Local transformation first, but during updating, becomes global transformation.
    tf: Matrix4f,

    /// Index to Global transformation matrix.
    /// First one is index in matrix array.
    /// Second one is index of the matrix array for mesh when it's used.
    gtf_index: Index2,

    /// Optional camera key.
    camera: Option<ResKey>,

    /// Optional mesh key and its corresponding chunk index.
    mesh: Option<ResKey>,

    /// Optional mapping with ECS's entity.
    entity: Option<(usize, usize)>,

    /// Dirty flag.
    dirty: NodeDirty,
}

impl Node {
    /// Updates transformation matrix, and then marks dirty on it.
    #[inline]
    pub fn set_transform(&mut self, tf: Matrix4f) {
        self.tf = tf;
        self.dirty |= NodeDirty::TRANSFORM;
    }

    /// Returns mutable reference of transformation matrix and marks dirty on it.
    #[inline]
    pub fn get_transform_mut(&mut self) -> &mut Matrix4f {
        self.dirty |= NodeDirty::TRANSFORM;
        &mut self.tf
    }

    /// Sets camera key.
    #[inline]
    pub fn set_camera(&mut self, camera: ResKey) {
        self.camera = Some(camera);
        self.dirty |= NodeDirty::CAMERA;
    }

    /// Gets camera key.
    #[inline]
    pub fn get_camera(&self) -> Option<&ResKey> {
        self.camera.as_ref()
    }

    /// Sets mesh key.
    #[inline]
    pub fn set_mesh(&mut self, mesh: ResKey) {
        self.mesh = Some(mesh);
        self.dirty |= NodeDirty::MESH;
    }

    /// Retrieves mesh key.
    #[inline]
    pub fn get_mesh_key(&self) -> Option<&ResKey> {
        self.mesh.as_ref()
    }

    /// Sets mapping with ECS's entity.
    #[inline]
    pub fn set_entity(&mut self, enti: usize, eid: usize) {
        self.entity = Some((enti, eid));
        self.dirty |= NodeDirty::ENTITY;
    }

    /// Gets mapped ECS's entity key and its id.
    #[inline]
    pub fn get_mapped_entity(&self) -> Option<(usize, usize)> {
        self.entity
    }
}

impl Default for Node {
    fn default() -> Self {
        Self {
            tf: Matrix4f::default(),
            gtf_index: Index2::uninit(),
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
