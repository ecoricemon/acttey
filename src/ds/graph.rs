use std::ops::{Index, IndexMut};

use crate::ds::vec::SetVec;
use ahash::AHashSet;

/// Directed graph.
#[derive(Debug, Clone)]
pub struct DirectedGraph<T> {
    nodes: Vec<T>,

    /// Outgoing edges of each node.
    outbounds: Vec<SetVec<usize>>,

    /// Incoming edges of each node.
    inbounds: Vec<AHashSet<usize>>,

    /// Nodes that don't have incoming edges.
    /// It can be considered root's children.
    starts: SetVec<usize>,
}

impl<T> DirectedGraph<T> {
    /// Creates a directed acyclic graph with root node.
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            outbounds: Vec::new(),
            inbounds: Vec::new(),
            starts: SetVec::new(),
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline]
    pub fn get_node(&self, index: usize) -> Option<&T> {
        self.nodes.get(index)
    }

    #[inline]
    pub fn get_node_mut(&mut self, index: usize) -> Option<&mut T> {
        self.nodes.get_mut(index)
    }

    pub fn add_node(&mut self, value: T) -> usize {
        let index = self.len();
        self.nodes.push(value);
        self.outbounds.push(SetVec::new());
        self.inbounds.push(AHashSet::new());
        self.starts.insert(index);
        index
    }

    /// Tries to remove a node from the graph.
    /// It's only possible when the node doesn't have any outgoing edges.
    /// In other words, you can remove leaf node only.
    // pub fn remove_node(&mut self, index: usize) -> Optino<T> {
    //     self.outbounds[index].is_empty().then(|| {
    //         self.nodes.re
    //     })
    // }

    pub fn add_edge(&mut self, from: usize, to: usize) {
        self.outbounds[from].insert(to);
        self.inbounds[to].insert(from);
        self.starts.take_by_value(&to);
    }

    pub fn remove_edge(&mut self, from: usize, to: usize) {
        self.outbounds[from].take_by_value(&to);
        self.inbounds[to].remove(&from);
        if self.inbounds[to].is_empty() {
            self.starts.insert(to);
        }
    }

    pub fn get_outbounds(&self, index: usize) -> &SetVec<usize> {
        &self.outbounds[index]
    }

    pub fn get_inbounds(&self, index: usize) -> &AHashSet<usize> {
        &self.inbounds[index]
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
            edges: &[SetVec<usize>],
        ) -> bool {
            if !visit[v] {
                visit[v] = true;
                path_visit[v] = true;
                let res = edges[v]
                    .iter_occupied()
                    .any(|&w| find(w, visit, path_visit, edges) || path_visit[w]);
                path_visit[v] = false;
                res
            } else {
                false
            }
        }

        self.starts
            .iter_occupied()
            .any(|&v| find(v, &mut visit, &mut path_visit, &self.outbounds))
    }
}

impl<T> Index<usize> for DirectedGraph<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.get_node(index).unwrap()
    }
}

impl<T> IndexMut<usize> for DirectedGraph<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.get_node_mut(index).unwrap()
    }
}
