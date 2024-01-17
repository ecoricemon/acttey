use crate::{
    ds::vec::AnyVec,
    ecs::traits::{CollectErased, CollectGeneric, CollectNonGeneric, Downcast},
    ty,
};
use ahash::AHashMap;
use erased_generic_trait::{add_fn_table, generate_fn_table, inject_fn_table};
use std::{
    any::{Any, TypeId},
    borrow::Borrow,
    hash::Hash,
    mem::{swap, zeroed},
    ptr::copy_nonoverlapping,
};

/// Represents a resizer function that can resize a `AnyVec` to a specified length.
type Resizer = Box<dyn Fn(&mut AnyVec, usize)>;

/// Sparseset with heterogeneous dense Vec.
/// Capacity: Length of sparse Vec.
/// Length: Length of dense Vec.
#[inject_fn_table(
    CollectErased;
    fn collect_type<T: 'static>(&mut self);
    fn collect<T: 'static>(&mut self, key: usize, value: &mut T);
    fn copy<T: 'static>(&mut self, key: usize, value: &mut T) -> Option<()>;
)]
pub struct SparseSet {
    sparse: Vec<Option<usize>>,
    deref: Vec<usize>,
    denses: AHashMap<TypeId, AnyVec, ahash::RandomState>,
    resizers: AHashMap<TypeId, Option<Resizer>, ahash::RandomState>,
}

impl SparseSet {
    pub fn new() -> Self {
        Self {
            fn_table: generate_fn_table!(SparseSet),
            sparse: Vec::new(),
            deref: Vec::new(),
            denses: AHashMap::new(),
            resizers: AHashMap::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        let mut sparse = Vec::with_capacity(capacity);
        let mut deref = Vec::with_capacity(capacity);
        sparse.resize(capacity, None);
        deref.resize(capacity, 0);
        Self {
            fn_table: generate_fn_table!(SparseSet),
            sparse,
            deref,
            denses: AHashMap::new(),
            resizers: AHashMap::new(),
        }
    }

    /// Adds new dense with the type `V` and makes it have the same length of other denses.
    ///
    /// # Safety
    ///
    /// The elements must be initialized.
    pub unsafe fn add_dense_type<V: 'static>(&mut self) {
        let len = self.len();
        let mut v = AnyVec::with_capacity::<V>(len);
        // `v` grew here, but elements were not set.
        v.grow(len);
        self.denses.insert(ty!(V), v);
    }

    /// Adds new dense with the type `V` and fills it with the value returned by `f`.
    /// So that the new dense has the same length with other denses.
    /// And `f` is kept as a resizer and will be used when it needs to be resized.
    pub fn add_dense_type_with<V: 'static>(&mut self, f: Box<dyn Fn() -> V>) {
        // Make a resizer.
        let resizer = Box::new(move |dense: &mut AnyVec, new_len: usize| {
            assert!(dense.is_type_of(&ty!(V)));
            // Safety: Type checked.
            unsafe {
                dense.resize_with(new_len, f.as_ref());
            }
        });

        // Add a dense and resize it to have the same length with others.
        let len = self.len();
        let mut v = AnyVec::with_capacity::<V>(len);
        resizer.as_ref()(&mut v, len);
        self.denses.insert(ty!(V), v);

        // Keep the resizer for later.
        self.resizers.insert(ty!(V), Some(resizer));
    }

    pub fn contains_type(&self, ty: &TypeId) -> bool {
        self.denses.contains_key(ty)
    }

    /// Makes all denses have the same length by increasing samller ones.
    ///
    /// # Panics
    ///
    /// Panics if any denses resized don't have their resizers.
    fn sync_dense_len(&mut self) {
        if let Some(longest) = self.denses.values().map(|dense| dense.len()).max() {
            for (ty_id, dense) in self.denses.iter_mut() {
                if dense.len() < longest {
                    let resizer = self.resizers.get(ty_id).unwrap().as_ref().unwrap();
                    resizer(dense, longest);
                }
            }
        }
    }

    /// Returns true if all denses have the same length.
    fn has_equal_denses_len(&self) -> bool {
        let repr_len = self.len();
        self.denses
            .values()
            .map(|dense| dense.len())
            .all(|len| repr_len == len)
    }

    pub fn remove_dense_type(&mut self, ty: &TypeId) {
        self.denses.remove(ty);
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.denses
            .values()
            .next()
            .map(|dense| dense.len())
            .unwrap_or(0)
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
    fn as_vec<V: 'static>(denses: &AHashMap<TypeId, AnyVec>) -> Option<&Vec<V>> {
        Some(denses.get(&ty!(V))?.into())
    }

    #[inline]
    fn as_vec_mut<V: 'static>(denses: &mut AHashMap<TypeId, AnyVec>) -> Option<&mut Vec<V>> {
        Some(denses.get_mut(&ty!(V))?.into())
    }

    #[inline]
    fn get_dense_index(&self, key: usize) -> Option<usize> {
        *self.sparse.get(key)?
    }

    #[inline]
    fn take_dense_index(&mut self, key: usize) -> Option<usize> {
        self.sparse.get_mut(key)?.take()
    }

    /// Inserts single type values at the end of the dense array.
    /// Other denses are increased to have the same length by their resizers.
    ///
    /// # Safety
    ///
    /// The elements in the increased denses must be initialized.
    ///
    /// # Panics
    ///
    /// - Panics if the generic parameter `V` have not been added.
    /// - Panics if any denses resized don't have their resizers.
    pub fn extend_sync<I, V>(&mut self, entries: I, len: usize)
    where
        I: IntoIterator<Item = (usize, V)>,
        V: 'static,
    {
        let dense = SparseSet::as_vec_mut(&mut self.denses).unwrap();
        self.sparse.reserve_exact(len);
        self.deref.reserve_exact(len);
        dense.reserve_exact(len);
        for (key, value) in entries.into_iter() {
            if self.sparse.len() <= key {
                self.sparse.resize(key + 1, None);
            }
            match self.sparse[key] {
                Some(di) => dense[di] = value,
                None => {
                    self.sparse[key] = Some(dense.len());
                    dense.push(value);
                    self.deref.push(key);
                }
            }
        }
        self.sync_dense_len();
    }

    /// Inserts a single type value at the end of the dense array.
    /// Other denses are increased to have the same length by their resizers.
    /// To remove the overhead by putting values by the resizers,
    /// Please refer to the trait `Collect`.
    /// `SparseSet` has the impl of the trait `Collect`.
    ///
    /// # Safety
    ///
    /// The elements in the increased denses must be initialized.
    ///
    /// # Panics
    ///
    /// - Panics if the generic parameter `V` have not been added.
    /// - Panics if any denses resized don't have their resizers.
    #[inline]
    pub fn insert_sync<V: 'static>(&mut self, key: usize, value: V) {
        let dense = SparseSet::as_vec_mut(&mut self.denses).unwrap();
        if self.sparse.len() <= key {
            self.sparse.resize(key + 1, None);
        }
        match self.sparse[key] {
            Some(di) => dense[di] = value,
            None => {
                self.sparse[key] = Some(dense.len());
                dense.push(value);
                self.deref.push(key);
            }
        }
        self.sync_dense_len();
    }

    #[inline]
    pub fn remove(&mut self, key: usize) {
        if let Some(index) = self.take_dense_index(key) {
            for dense in self.denses.values_mut() {
                dense.swap_remove_no_return(index);
            }
            self.deref.swap_remove(index);
            if index < self.deref.len() {
                self.sparse[self.deref[index]] = Some(index);
            }
        }
    }

    #[inline]
    pub fn get<V: 'static>(&self, key: usize) -> Option<&V> {
        // Safety
        //
        // Type and index are checked by denses.get() and self.into_dense_index() respectively.
        Some(unsafe {
            self.denses
                .get(&ty!(V))?
                .get_unchecked::<V>(self.get_dense_index(key)?)
        })
    }

    #[inline]
    pub fn get_mut<V: 'static>(&mut self, key: usize) -> Option<&mut V> {
        // Safety
        //
        // Type and index are checked by denses.get() and self.into_dense_index() respectively.
        Some(unsafe {
            let index = self.get_dense_index(key)?;
            self.denses.get_mut(&ty!(V))?.get_unchecked_mut::<V>(index)
        })
    }

    #[inline]
    pub fn values<V: 'static>(&self) -> impl Iterator<Item = &V> {
        SparseSet::as_vec(&self.denses)
            .map(|v| v.iter())
            .unwrap_or_default()
    }

    #[inline]
    pub fn values_mut<V: 'static>(&mut self) -> impl Iterator<Item = &mut V> {
        SparseSet::as_vec_mut(&mut self.denses)
            .map(|v| v.iter_mut())
            .unwrap_or_default()
    }
}

impl std::default::Default for SparseSet {
    fn default() -> Self {
        Self::new()
    }
}

impl CollectGeneric for SparseSet {
    #[inline]
    fn collect_type<V: 'static>(&mut self) {
        add_fn_table!(self, V);
        self.denses.insert(ty!(V), AnyVec::new::<V>());
    }

    #[inline]
    fn collect<V: 'static>(&mut self, key: usize, value: &mut V) {
        let dense = SparseSet::as_vec_mut(&mut self.denses).unwrap();
        match self.sparse[key] {
            Some(di) => swap(&mut dense[di], value),
            None => {
                let mut take: V = unsafe { zeroed() };
                swap(&mut take, value);
                dense.push(take);
            }
        }
    }

    #[inline]
    fn copy<V: 'static>(&mut self, key: usize, value: &mut V) -> Option<()> {
        let v = SparseSet::get::<V>(self, key)?;
        unsafe {
            copy_nonoverlapping(v as *const V, value as *mut V, 1);
        }
        Some(())
    }
}

impl CollectNonGeneric for SparseSet {
    #[inline]
    fn contains_type(&self, ckey: &TypeId) -> bool {
        self.denses.contains_key(ckey)
    }

    #[inline]
    fn begin_collect(&mut self, key: usize) {
        debug_assert!(self.has_equal_denses_len());

        if self.sparse.len() <= key {
            self.sparse.resize(key + 1, None);
        }
    }

    #[inline]
    fn end_collect(&mut self, key: usize) {
        // Items should have been pushed.
        if self.sparse[key].is_none() {
            self.sparse[key] = Some(self.len() - 1);
            self.deref.push(key);
        }

        debug_assert!(self.has_equal_denses_len());
    }

    #[inline]
    fn remove(&mut self, key: usize) {
        self.remove(key)
    }

    #[inline]
    fn len(&self) -> usize {
        self.len()
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.is_empty()
    }

    #[inline]
    fn values_as_any(&mut self, ty: &TypeId) -> &mut dyn Any {
        self.denses.get_mut(ty).unwrap()
    }
}

impl Downcast for SparseSet {
    fn downcast<C: 'static>(any: &mut dyn Any) -> &mut [C] {
        any.downcast_mut::<AnyVec>().unwrap().into()
    }
}

#[derive(Debug)]
pub struct MonoSparseSet<K: Eq + Hash + Clone, V> {
    sparse: AHashMap<K, usize>,
    dense: Vec<(K, V)>,
}

impl<K: Eq + Hash + Clone, V> MonoSparseSet<K, V> {
    pub fn new() -> Self {
        Self {
            sparse: AHashMap::new(),
            dense: Vec::with_capacity(4),
        }
    }

    #[inline]
    pub fn clear(&mut self) {
        self.sparse.clear();
        self.dense.clear();
    }

    #[inline]
    pub fn contains_key(&self, k: &K) -> bool {
        self.sparse.contains_key(k)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.dense.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline]
    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        if let Some(index) = self.sparse.get(&k) {
            let mut item = (k, v);
            // Safety: `index` is valid.
            let old = unsafe { self.dense.get_unchecked_mut(*index) };
            swap(old, &mut item);
            Some(item.1)
        } else {
            self.sparse.insert(k.clone(), self.dense.len());
            self.dense.push((k, v));
            None
        }
    }

    #[inline]
    pub fn remove<Q>(&mut self, k: &Q) -> Option<V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        if let Some(index) = self.sparse.get(k).cloned() {
            let old_v = self.dense.swap_remove(index).1;
            self.sparse.remove(k);
            if let Some(last) = self.dense.get(index) {
                unsafe {
                    *self.sparse.get_mut(last.0.borrow()).unwrap_unchecked() = index;
                }
            }
            Some(old_v)
        } else {
            None
        }
    }

    #[inline]
    pub fn get<Q>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.dense.get(*self.sparse.get(k)?).map(|(_, v)| v)
    }

    #[inline]
    pub fn get_mut<Q>(&mut self, k: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.dense.get_mut(*self.sparse.get(k)?).map(|(_, v)| v)
    }

    #[inline]
    pub fn keys(&self) -> std::collections::hash_map::Keys<'_, K, usize> {
        self.sparse.keys()
    }

    #[inline]
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.dense.iter().map(|(_, v)| v)
    }

    #[inline]
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.dense.iter_mut().map(|(_, v)| v)
    }

    #[inline]
    pub fn iter(&self) -> std::slice::Iter<'_, (K, V)> {
        self.dense.iter()
    }
}

impl<K: Eq + Hash + Clone, V> Default for MonoSparseSet<K, V> {
    fn default() -> Self {
        Self::new()
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
    fn test_sparseset_insert_extend() {
        let entries = [
            (0, SA { x: [0, 1] }),
            (0, SA { x: [2, 3] }),
            (1, SA { x: [4, 5] }),
            (10, SA { x: [6, 7] }),
        ];

        let mut s = SparseSet::with_capacity(2);
        assert_eq!(2, s.capacity());
        assert_eq!(0, s.len());

        assert_eq!(false, s.contains_type(&ty!(SA)));
        s.add_dense_type_with(Box::new(|| SA::default()));
        assert_eq!(true, s.contains_type(&ty!(SA)));

        s.insert_sync(entries[0].0, entries[0].1);
        assert_eq!(2, s.capacity());
        assert_eq!(1, s.len());
        assert_eq!(Some(&entries[0].1), s.get(entries[0].0));

        s.extend_sync((&entries[1..3]).to_owned(), 2);
        assert_eq!(2, s.capacity());
        assert_eq!(2, s.len());
        assert_eq!(Some(&entries[1].1), s.get(entries[1].0));
        assert_eq!(Some(&entries[2].1), s.get(entries[2].0));

        s.insert_sync(entries[3].0, entries[3].1);
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
        s.add_dense_type_with(Box::new(|| SA::default()));
        s.add_dense_type_with(Box::new(|| SB::default()));
        s.extend_sync(a_entries.clone(), len);
        s.extend_sync(b_entries.clone(), len);
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

    #[wasm_bindgen_test]
    fn test_monosparseset_insert_remove() {
        let mut set = MonoSparseSet::new();
        assert!(set.is_empty());

        let entries = [(0, 'a'), (1, 'b'), (2, 'c'), (3, 'd'), (4, 'e')];
        for (i, (k, v)) in entries.iter().cloned().enumerate() {
            set.insert(k, v);
            assert_eq!(i + 1, set.len());
            assert_eq!(Some(&v), set.get(&k));
        }

        let mut entries: Vec<Option<_>> = entries.into_iter().map(|entry| Some(entry)).collect();

        fn check(entries: &Vec<Option<(i32, char)>>, set: &MonoSparseSet<i32, char>) {
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
            let (k, v) = entries[i].take().unwrap();
            set.remove(&k);
            check(&entries, &set);
        }
    }
}
