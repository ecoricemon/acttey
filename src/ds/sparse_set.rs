use std::{borrow::Borrow, collections::HashMap, fmt::Debug, hash::BuildHasher, hash::Hash, mem};

/// Vector-like hash map.
/// Values are in an actual vector, so that they are close to each other in memory.
#[derive(Debug, Clone)]
pub struct MonoSparseSet<K, V, S> {
    sparse: HashMap<K, usize, S>,
    dense: Vec<(K, V)>,
}

impl<K, V, S> MonoSparseSet<K, V, S>
where
    K: Hash + Eq + Clone,
    S: Default,
{
    pub fn new() -> Self {
        Self {
            sparse: HashMap::default(),
            dense: Vec::with_capacity(4),
        }
    }
}
impl<K, V, S> MonoSparseSet<K, V, S> {
    pub fn clear(&mut self) {
        self.sparse.clear();
        self.dense.clear();
    }
}

impl<K, V, S> MonoSparseSet<K, V, S>
where
    K: Hash + Eq + Clone,
    S: BuildHasher,
{
    pub fn contains_key<Q>(&self, k: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.sparse.contains_key(k)
    }

    pub fn len(&self) -> usize {
        self.dense.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        if let Some(index) = self.sparse.get(&k) {
            let mut item = (k, v);
            // Safety: `index` is valid.
            let old = unsafe { self.dense.get_unchecked_mut(*index) };
            mem::swap(old, &mut item);
            Some(item.1)
        } else {
            self.sparse.insert(k.clone(), self.dense.len());
            self.dense.push((k, v));
            None
        }
    }

    pub fn remove<Q>(&mut self, k: &Q) -> Option<V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        if let Some(index) = self.sparse.get(k).cloned() {
            let old = self.dense.swap_remove(index).1;
            self.sparse.remove(k);
            if let Some(last) = self.dense.get(index) {
                unsafe {
                    *self.sparse.get_mut(last.0.borrow()).unwrap_unchecked() = index;
                }
            }
            Some(old)
        } else {
            None
        }
    }

    pub fn get<Q>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.dense.get(*self.sparse.get(k)?).map(|(_, v)| v)
    }

    pub fn get_mut<Q>(&mut self, k: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.dense.get_mut(*self.sparse.get(k)?).map(|(_, v)| v)
    }

    pub fn iter(&self) -> std::slice::Iter<'_, (K, V)> {
        self.dense.iter()
    }

    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.dense.iter().map(|(k, _)| k)
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.dense.iter().map(|(_, v)| v)
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.dense.iter_mut().map(|(_, v)| v)
    }
}

impl<K, V, S> Default for MonoSparseSet<K, V, S>
where
    K: Eq + Hash + Clone,
    S: Default,
{
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_bindgen_test::*;

    #[wasm_bindgen_test]
    fn test_monosparseset_insert_remove() {
        let mut set = MonoSparseSet::<_, _, std::hash::RandomState>::new();
        assert!(set.is_empty());

        let entries = [(0, 'a'), (1, 'b'), (2, 'c'), (3, 'd'), (4, 'e')];
        for (i, (k, v)) in entries.iter().cloned().enumerate() {
            set.insert(k, v);
            assert_eq!(i + 1, set.len());
            assert_eq!(Some(&v), set.get(&k));
        }

        let mut entries: Vec<Option<_>> = entries.into_iter().map(|entry| Some(entry)).collect();

        fn check<S>(entries: &Vec<Option<(i32, char)>>, set: &MonoSparseSet<i32, char, S>)
        where
            S: BuildHasher,
        {
            let (mut keys, mut values) = entries.iter().filter_map(|&entry| entry).fold(
                (vec![], vec![]),
                |(mut keys, mut values), (key, value)| {
                    keys.push(key);
                    values.push(value);
                    (keys, values)
                },
            );
            let mut set_keys = set.keys().cloned().collect::<Vec<_>>();
            let mut set_values = set.values().cloned().collect::<Vec<_>>();

            keys.sort_unstable();
            values.sort_unstable();
            set_keys.sort_unstable();
            set_values.sort_unstable();

            assert_eq!(keys, set_keys);
            assert_eq!(values, set_values);
        }

        check(&entries, &set);

        let remove_order = [2, 4, 0, 1, 3];

        for i in remove_order {
            let (k, _) = entries[i].take().unwrap();
            set.remove(&k);
            check(&entries, &set);
        }
    }
}
