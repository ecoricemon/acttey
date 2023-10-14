use crate::any_vec::AnyVec;
use std::{any::TypeId, collections::HashMap};

/// Sparseset with heterogeneous dense Vec.
/// Capacity: Length of sparse Vec.
/// Length: Length of dense Vec.
#[derive(Default)]
pub struct SparseSet {
    sparse: Vec<Option<usize>>,
    deref: Vec<usize>,
    denses: HashMap<TypeId, AnyVec>,
}

impl SparseSet {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        let mut sparse = Vec::with_capacity(capacity);
        let mut deref = Vec::with_capacity(capacity);
        sparse.resize(capacity, None);
        deref.resize(capacity, 0);
        Self {
            sparse,
            deref,
            denses: HashMap::new(),
        }
    }

    pub fn add_dense_type<T: 'static + Default>(&mut self) {
        self.denses.insert(TypeId::of::<T>(), AnyVec::new::<T>());
        self.sync_dense_len();
    }

    pub fn add_dense_type_with_capacity<T: 'static + Default>(&mut self, capacity: usize) {
        self.denses
            .insert(TypeId::of::<T>(), AnyVec::with_capacity::<T>(capacity));
        self.sync_dense_len();
    }

    fn sync_dense_len(&mut self) {
        if let Some(longest) = self.denses.values().map(|dense| dense.len()).max() {
            for dense in self.denses.values() {
                dense.resize(longest);
            }
        }
    }

    pub fn remove_dense_type<T: 'static>(&mut self) {
        self.denses.remove(&TypeId::of::<T>());
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.denses.values().next().map(|av| av.len()).unwrap_or(0)
    }
    
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.sparse.len()
    }

    #[inline]
    fn as_vec<T: 'static>(denses: &HashMap<TypeId, AnyVec>) -> Option<&Vec<T>> {
        Some(denses.get(&TypeId::of::<T>())?.into())
    }

    #[inline]
    fn as_vec_mut<T: 'static>(denses: &mut HashMap<TypeId, AnyVec>) -> Option<&mut Vec<T>> {
        Some(denses.get_mut(&TypeId::of::<T>())?.into())
    }

    #[inline]
    fn get_dense_index(&self, key: usize) -> Option<usize> {
        *self.sparse.get(key)?
    }

    #[inline]
    fn take_dense_index(&mut self, key: usize) -> Option<usize> {
        self.sparse.get_mut(key)?.take()
    }

    pub fn insert<I, T>(&mut self, entries: I, len: usize)
    where
        I: IntoIterator<Item = (usize, T)>,
        T: 'static,
    {
        if let Some(dense) = SparseSet::as_vec_mut(&mut self.denses) {
            self.sparse.reserve_exact(len);
            self.deref.reserve_exact(len);
            dense.reserve_exact(len);
            for (key, value) in entries.into_iter() {
                if self.sparse.len() <= key {
                    self.sparse.resize(key + 1, None);
                }
                if let Some(di) = self.sparse[key] {
                    dense[di] = value;
                } else {
                    self.sparse[key] = Some(dense.len());
                    dense.push(value);
                    self.deref.push(key);
                }
            }
            self.sync_dense_len();
        }
    }

    pub fn remove(&mut self, key: usize) {
        if let Some(index) = self.take_dense_index(key) {
            for dense in self.denses.values_mut() {
                dense.swap_remove(index);
            }
            self.deref.swap_remove(index);
            if index < self.deref.len() {
                self.sparse[self.deref[index]] = Some(index);
            }
        }
    }

    #[inline]
    pub fn get<T: 'static>(&self, key: usize) -> Option<&T> {
        // Safety
        //
        // Type and index are checked by denses.get() and self.into_dense_index() respectively.
        Some(unsafe {
            self.denses
                .get(&TypeId::of::<T>())?
                .get_unchecked::<T>(self.get_dense_index(key)?)
        })
    }

    #[inline]
    pub fn get_mut<T: 'static>(&mut self, key: usize) -> Option<&mut T> {
        // Safety
        //
        // Type and index are checked by denses.get() and self.into_dense_index() respectively.
        Some(unsafe {
            let index = self.get_dense_index(key)?;
            self.denses
                .get_mut(&TypeId::of::<T>())?
                .get_unchecked_mut::<T>(index)
        })
    }

    #[inline]
    pub fn values<T: 'static>(&self) -> impl Iterator<Item = &T> {
        SparseSet::as_vec(&self.denses)
            .map(|v| v.iter())
            .unwrap_or_default()
    }

    #[inline]
    pub fn values_mut<T: 'static>(&mut self) -> impl Iterator<Item = &mut T> {
        SparseSet::as_vec_mut(&mut self.denses)
            .map(|v| v.iter_mut())
            .unwrap_or_default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_bindgen_test::*;

    wasm_bindgen_test_configure!(run_in_browser);

    #[derive(PartialEq, Debug, Clone, Copy, Default)]
    struct SA {
        x: [usize; 2],
    }

    #[derive(PartialEq, Debug, Clone, Copy, Default)]
    struct SB {
        x: [usize; 2],
        y: [f32; 2],
    }

    #[wasm_bindgen_test]
    fn test_sparseset_insert() {
        let entries = [
            (0, SA { x: [0, 1] }),
            (0, SA { x: [2, 3] }),
            (1, SA { x: [4, 5] }),
            (10, SA { x: [6, 7] }),
        ];

        let mut s = SparseSet::with_capacity(2);
        assert_eq!(2, s.capacity());
        assert_eq!(0, s.len());

        s.add_dense_type::<SA>();

        s.insert((&entries[..1]).to_owned(), 1);
        assert_eq!(2, s.capacity());
        assert_eq!(1, s.len());
        assert_eq!(Some(&entries[0].1), s.get(entries[0].0));

        s.insert((&entries[1..3]).to_owned(), 2);
        assert_eq!(2, s.capacity());
        assert_eq!(2, s.len());
        assert_eq!(Some(&entries[1].1), s.get(entries[1].0));
        assert_eq!(Some(&entries[2].1), s.get(entries[2].0));

        s.insert((&entries[3..]).to_owned(), 1);
        assert_eq!(11, s.capacity());
        assert_eq!(3, s.len());
        assert_eq!(Some(&entries[3].1), s.get(entries[3].0));
    }

    #[wasm_bindgen_test]
    fn test_sparseset_remove() {
        let len = 10;
        let a_entries = (0..len)
            .map(|i| {
                (
                    i,
                    SA {
                        x: [i * 2, i * 2 + 1],
                    },
                )
            })
            .collect::<Vec<_>>();
        let b_entries = (0..len)
            .map(|i| {
                (
                    i,
                    SB {
                        x: [i * 2, i * 2 + 1],
                        y: Default::default(),
                    },
                )
            })
            .collect::<Vec<_>>();

        let mut s = SparseSet::new();
        s.add_dense_type::<SA>();
        s.add_dense_type::<SB>();
        s.insert(a_entries.clone(), len);
        s.insert(b_entries.clone(), len);
        assert_eq!(len, s.capacity());
        assert_eq!(len, s.len());
        for i in 0..len {
            assert_eq!(Some(&a_entries[i].1), s.get(i));
            assert_eq!(Some(&b_entries[i].1), s.get(i));
        }

        // Remove the last one
        s.remove(len - 1);
        assert_eq!(len, s.capacity());
        assert_eq!(len - 1, s.len());
        for i in 0..len - 1 {
            assert_eq!(Some(&a_entries[i].1), s.get(i));
            assert_eq!(Some(&b_entries[i].1), s.get(i));
        }
        assert_eq!(None, s.get::<SA>(len - 1));
        assert_eq!(None, s.get::<SB>(len - 1));

        // Remove the first one
        s.remove(0);
        assert_eq!(len, s.capacity());
        assert_eq!(len - 2, s.len());
        for i in 1..len - 1 {
            assert_eq!(Some(&a_entries[i].1), s.get(i));
            assert_eq!(Some(&b_entries[i].1), s.get(i));
        }
        assert_eq!(None, s.get::<SA>(0));
        assert_eq!(None, s.get::<SB>(0));
    }
}
