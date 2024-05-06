use super::set_list::SetValueList;
use crate::ds::vec::OptVec;
use std::{
    collections::HashSet,
    num::NonZeroUsize,
    ops::{Index, IndexMut},
};

/// Directed graph.
#[derive(Debug, Clone)]
pub struct DirectedGraph<T> {
    /// Nodes with default root node.
    /// Default root occupies slot at index 0.
    nodes: OptVec<T>,

    // NonZeroUsize can help us to reduce redundant size caused by Option.
    /// Outgoing edges of each node.
    /// This field preserves inserted order,
    /// so that we can traverse neighbors in order we expect.  
    outbounds: Vec<SetValueList<NonZeroUsize>>,

    /// Incoming edges of each node.
    /// This field doesn't preserve inserted order.  
    inbounds: Vec<HashSet<usize, ahash::RandomState>>,
}

impl<T: Default> DirectedGraph<T> {
    /// Creates a directed acyclic graph with default root node.
    pub fn new() -> Self {
        let mut nodes = OptVec::new();
        let root_index = nodes.add(T::default());
        debug_assert_eq!(0, root_index);
        Self {
            nodes,
            outbounds: vec![Self::new_outbound()], // For default root
            inbounds: vec![HashSet::default()],    // For default root
        }
    }
}

impl<T> DirectedGraph<T> {
    // Note
    // Sometimes, we want to update node's values while traversing the graph.
    // However, we have two problems to do so.
    // First problem is that we cannot modify `nodes` while borrowing out or inbounds.
    // We can avoid the first problem using destructuring,
    // but the second problem is about it.
    // Destructuring is dangerous with respect to invalidation of out or inbounds.
    // We don't want each node to have their children within it because it's usually not
    // considered effective way in terms of memory.
    // Unfortunately, I couldn't find a way to solve those problems together.
    // (I don't like wrapping OptVec with another struct for this purpose only, it's strange to me)
    //
    /// Returns nodes, inbounds, and outbounds.
    /// Do not insert or remove any nodes.
    #[allow(clippy::type_complexity)]
    pub fn destructure(
        &mut self,
    ) -> (
        &mut OptVec<T>,
        &Vec<SetValueList<NonZeroUsize>>,
        &Vec<HashSet<usize, ahash::RandomState>>,
    ) {
        (&mut self.nodes, &self.outbounds, &self.inbounds)
    }

    /// Retrieves the length of node buffer,
    /// which is default root node + # of vacant slots + # of occupied slots.
    //
    // NOTE: Don't implement len(). It's confusing because we put in default node.
    #[inline]
    pub fn len_buf(&self) -> usize {
        self.nodes.len()
    }

    /// Retrieves the number of nodes except default head node.
    pub fn len_occupied(&self) -> usize {
        self.nodes.len_occupied() - 1
    }

    /// Returns true if there's no node in the graph.
    /// Note that default root node is not taken into account.
    pub fn has_no_occupied(&self) -> bool {
        self.len_occupied() == 0
    }

    /// Determines `index` is in bounds and the slot is Some,
    /// which means there's a value in the slot.
    #[inline]
    pub fn is_valid(&self, index: usize) -> bool {
        self.nodes.is_valid(index)
    }

    #[inline]
    pub fn get_node(&self, index: usize) -> Option<&T> {
        self.nodes.get(index)
    }

    #[inline]
    pub fn get_root_mut(&mut self) -> &mut T {
        // Safety: There's default root node at the first position.
        unsafe { self.nodes.get_unchecked_mut(0) }
    }

    /// # Safety
    ///
    /// Undefine behavior if `index` is out of bound or the slot is vacant.
    #[inline]
    pub unsafe fn get_node_unchecked(&self, index: usize) -> &T {
        self.nodes.get_unchecked(index)
    }

    /// Returns mutable reference of the node,
    /// but it can be None if `index` is out of bound or the slot is vacant.
    #[inline]
    pub fn get_node_mut(&mut self, index: usize) -> Option<&mut T> {
        self.nodes.get_mut(index)
    }

    /// # Safety
    ///
    /// Undefine behavior if `index` is out of bound or the slot is vacant.
    #[inline]
    pub unsafe fn get_node_unchecked_mut(&mut self, index: usize) -> &mut T {
        self.nodes.get_unchecked_mut(index)
    }

    #[inline]
    pub fn get_outbound(&self, index: usize) -> Option<&SetValueList<NonZeroUsize>> {
        self.outbounds.get(index)
    }

    /// # Safety
    ///
    /// Undefine behavior if `index` is out of bound or the slot is vacant.
    #[inline]
    pub unsafe fn get_outbound_unchecked(&self, index: usize) -> &SetValueList<NonZeroUsize> {
        self.outbounds.get_unchecked(index)
    }

    #[inline]
    pub fn get_inbound(&self, index: usize) -> Option<&HashSet<usize, ahash::RandomState>> {
        self.inbounds.get(index)
    }

    /// # Safety
    ///
    /// Undefine behavior if `index` is out of bound or the slot is vacant.
    #[inline]
    pub unsafe fn get_inbound_unchecked(
        &self,
        index: usize,
    ) -> &HashSet<usize, ahash::RandomState> {
        self.inbounds.get_unchecked(index)
    }

    pub fn insert_node(&mut self, value: T) -> usize {
        let index = self.nodes.add(value);
        if index == self.nodes.len() - 1 {
            self.outbounds.push(Self::new_outbound());
            self.inbounds.push(HashSet::default());
        }
        self.connect_root(index);
        index
    }

    /// Tries to remove a node from the graph.
    /// It's only possible when the node doesn't have any outgoing edges.
    /// In other words, you can remove leaf nodes only.
    /// Returns removed node or None if the node is not a leaf node.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound or slot was vacant.
    pub fn remove_node(&mut self, index: usize) -> Option<T> {
        self.outbounds[index].has_no_occupied().then(|| {
            self.remove_incoming_edges(index);
            self.nodes.take(index).unwrap()
        })
    }

    pub fn add_edge(&mut self, from: usize, to: usize) {
        self.disconnect_root(to);
        self.outbounds[from].push_back(NonZeroUsize::new(to).unwrap());
        self.inbounds[to].insert(from);
    }

    pub fn remove_edge(&mut self, from: usize, to: usize) {
        self.outbounds[from].remove(&NonZeroUsize::new(to).unwrap());
        self.inbounds[to].remove(&from);
        if self.inbounds[to].is_empty() {
            self.connect_root(to);
        }
    }

    /// Detaches the node from other nodes completely.
    /// Therefore, the node will be an orphan node.
    pub fn remove_incoming_edges(&mut self, index: usize) {
        let nz_index = NonZeroUsize::new(index).unwrap();
        for from in self.inbounds[index].iter() {
            self.outbounds[*from].remove(&nz_index);
        }
        self.inbounds[index].clear();
    }

    pub fn iter_node(&self) -> impl Iterator<Item = (usize, &T)> {
        self.nodes.iter_occupied()
    }

    pub fn iter_outbound(&self, index: usize) -> impl Iterator<Item = usize> + Clone + '_ {
        self.outbounds[index].iter().map(|to| to.get())
    }

    pub fn iter_inbound(&self, index: usize) -> impl Iterator<Item = usize> + Clone + '_ {
        self.inbounds[index].iter().cloned()
    }

    // TODO: test
    /// Determines whether this graph has cycle or not.
    pub fn has_cycle(&self) -> bool {
        let mut visit = vec![false; self.nodes.len()];
        let mut path_visit = vec![false; self.nodes.len()];

        // Returns true if it detected a cycle.
        fn find(
            v: usize,
            visit: &mut [bool],
            path_visit: &mut [bool],
            edges: &[SetValueList<NonZeroUsize>],
        ) -> bool {
            if !visit[v] {
                visit[v] = true;
                path_visit[v] = true;
                let res = edges[v].iter().any(|&nz_w| {
                    let w = nz_w.get();
                    find(w, visit, path_visit, edges) || path_visit[w]
                });
                path_visit[v] = false;
                res
            } else {
                false
            }
        }

        find(0, &mut visit, &mut path_visit, &self.outbounds)
    }

    /// Orphan node should become a child of default root node.
    fn connect_root(&mut self, index: usize) {
        self.outbounds[0].push_back(NonZeroUsize::new(index).unwrap());
        self.inbounds[index].insert(0);
    }

    /// Child node is not an orphan anymore, therfore it should disconnect to default root node.
    fn disconnect_root(&mut self, index: usize) {
        self.outbounds[0].remove(&NonZeroUsize::new(index).unwrap());
        self.inbounds[index].remove(&0);
    }

    fn new_outbound() -> SetValueList<NonZeroUsize> {
        let dummy = unsafe { NonZeroUsize::new_unchecked(1) };
        SetValueList::new(dummy)
    }
}

impl<T: Default> Default for DirectedGraph<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Default> Index<usize> for DirectedGraph<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.get_node(index).unwrap()
    }
}

impl<T: Default> IndexMut<usize> for DirectedGraph<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.get_node_mut(index).unwrap()
    }
}
