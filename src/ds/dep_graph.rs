use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

pub trait DepKey: PartialEq + Eq + Hash + Clone {}

impl<T: PartialEq + Eq + Hash + Clone> DepKey for T {}

#[derive(Debug)]
pub struct DepNode<K: DepKey> {
    children: HashSet<K, ahash::RandomState>,
    rc: usize,
}

impl<K: DepKey> DepNode<K> {
    fn new(child: Option<K>, rc: usize) -> Self {
        let mut children = HashSet::default();
        if let Some(child) = child {
            children.insert(child);
        }
        Self { children, rc }
    }
}

#[derive(Debug)]
pub struct DepGraph<K: DepKey> {
    nodes: HashMap<K, DepNode<K>, ahash::RandomState>,
}

impl<K: DepKey> DepGraph<K> {
    pub fn new() -> Self {
        Self {
            nodes: HashMap::default(),
        }
    }

    pub fn register_dependency(&mut self, cur: K, next: K) {
        let contains_cur = self.nodes.contains_key(&cur);
        let contains_next = self.nodes.contains_key(&next);
        match (contains_cur, contains_next) {
            (false, false) => {
                self.nodes.insert(cur, DepNode::new(Some(next.clone()), 0));
                self.nodes.insert(next, DepNode::new(None, 1));
            }
            (false, true) => {
                self.nodes.insert(cur, DepNode::new(Some(next.clone()), 0));
                self.nodes.get_mut(&next).unwrap().rc += 1;
            }
            (true, false) => {
                self.nodes
                    .get_mut(&cur)
                    .unwrap()
                    .children
                    .insert(next.clone());
                self.nodes.insert(next, DepNode::new(None, 1));
            }
            (true, true) => {
                if self
                    .nodes
                    .get_mut(&cur)
                    .unwrap()
                    .children
                    .insert(next.clone())
                {
                    self.nodes.get_mut(&next).unwrap().rc += 1;
                }
            }
        }
    }

    pub fn unregister_dependency(&mut self, cur: &K, next: &K) {
        if let Some(cur_node) = self.nodes.get_mut(cur) {
            if cur_node.children.remove(next) {
                self.nodes.get_mut(next).unwrap().rc -= 1;
            }
        }
    }

    pub fn clear(&mut self) {
        self.nodes.clear();
    }

    fn reduce_rc<'a>(&'a mut self, keys: impl Iterator<Item = &'a K>) {
        for k in keys {
            if let Some(node) = self.nodes.get_mut(k) {
                if node.rc > 0 {
                    node.rc -= 1;
                }
            }
        }
    }

    pub fn remove_element<F: FnMut(&K)>(&mut self, cur: &K, remove: &mut F) {
        if let Some(&DepNode { rc: 0, .. }) = self.nodes.get(cur) {
            if let Some(cur_node) = self.nodes.remove(cur) {
                remove(cur);
                self.reduce_rc(cur_node.children.iter());
                for child_node in cur_node.children {
                    self.remove_element(&child_node, remove);
                }
            }
        }
    }
}

impl<K: DepKey> Default for DepGraph<K> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;
    use wasm_bindgen_test::*;

    struct Storage<K: DepKey> {
        keys: HashSet<K>,
    }

    impl<K: DepKey> Storage<K> {
        fn new() -> Self {
            Self {
                keys: HashSet::new(),
            }
        }

        fn insert(&mut self, k: K) {
            self.keys.insert(k);
        }

        fn remove(&mut self, k: &K) {
            self.keys.remove(k);
        }

        fn clear(&mut self) {
            self.keys.clear();
        }
    }

    #[wasm_bindgen_test]
    fn test_dep_graph_register_and_remove() {
        let mut storage = Storage::<char>::new();
        let mut deps = DepGraph::<char>::new();

        // A -> B -> C <- D, Remove A, then C and D should be remained.
        storage.clear();
        deps.clear();
        storage.insert('A');
        storage.insert('B');
        storage.insert('C');
        storage.insert('D');
        deps.register_dependency('A', 'B');
        deps.register_dependency('B', 'C');
        deps.register_dependency('D', 'C');
        deps.remove_element(&'A', &mut |key: &char| storage.remove(key));
        let expect: HashSet<char> = ['C', 'D'].into_iter().collect();
        assert_eq!(&expect, &storage.keys);
    }
}
