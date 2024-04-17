use crate::ds::vec::OptVec;
use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    hash::Hash,
};

/// `GroupMap` contains two layer called 'groups' and 'items'.
/// Conceptually, group has items within it, and item can belong to many groups.
/// Groups can't exist without items and vice versa.
/// In other words, they must have links to the others.
/// Group has ordered links to items while item has unordered links to groups.
///
/// You can access each group and item by their keys and indices.
/// In most cases, accessing them by indices is faster especially when key is not simple.
#[derive(Debug, Clone)]
pub struct GroupMap<GK, GV, IK, IV> {
    /// Each group contains some items.
    /// You can access each group by its key or index.
    groups: IndexedMap<GK, (GV, Vec<usize>)>,

    /// Each item belong to some groups.
    /// You can access each item by its key or index.
    items: IndexedMap<IK, (IV, HashSet<usize, ahash::RandomState>)>,
}

impl<GK, GV, IK, IV> GroupMap<GK, GV, IK, IV>
where
    GK: Hash + Eq + Clone,
    IK: Hash + Eq + Clone,
{
    pub fn new() -> Self {
        Self {
            groups: IndexedMap::new(),
            items: IndexedMap::new(),
        }
    }

    #[inline]
    pub fn contains_group(&self, index: usize) -> bool {
        self.groups.contains_index(index)
    }

    #[inline]
    pub fn contains_group2<Q>(&self, key: &Q) -> bool
    where
        GK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.groups.contains_key(key)
    }

    #[inline]
    pub fn get_group(&self, index: usize) -> Option<(&GV, &Vec<usize>)> {
        self.groups.get(index).map(|(value, links)| (value, links))
    }

    #[inline]
    pub fn get_group2<Q>(&self, key: &Q) -> Option<(&GV, &Vec<usize>)>
    where
        GK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.groups.get2(key).map(|(value, links)| (value, links))
    }

    #[inline]
    pub fn get_group_mut(&mut self, index: usize) -> Option<(&mut GV, &Vec<usize>)> {
        self.groups
            .get_mut(index)
            .map(|(value, links)| (value, &*links))
    }

    #[inline]
    pub fn get_group_mut2<Q>(&mut self, key: &Q) -> Option<(&mut GV, &Vec<usize>)>
    where
        GK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.groups
            .get_mut2(key)
            .map(|(value, links)| (value, &*links))
    }

    #[inline]
    pub fn get_group_key(&self, index: usize) -> Option<&GK> {
        self.groups.get_key(index)
    }

    #[inline]
    pub fn get_group_index<Q>(&self, key: &Q) -> Option<usize>
    where
        GK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.groups.get_index(key)
    }

    #[inline]
    pub fn contains_item(&self, index: usize) -> bool {
        self.items.contains_index(index)
    }

    #[inline]
    pub fn contains_item2<Q>(&self, key: &Q) -> bool
    where
        IK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.items.contains_key(key)
    }

    #[inline]
    pub fn get_item(&self, index: usize) -> Option<&(IV, HashSet<usize, ahash::RandomState>)> {
        self.items.get(index)
    }

    #[inline]
    pub fn get_item2<Q>(&self, key: &Q) -> Option<&(IV, HashSet<usize, ahash::RandomState>)>
    where
        IK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.items.get2(key)
    }

    #[inline]
    pub fn get_item_mut(
        &mut self,
        index: usize,
    ) -> Option<(&mut IV, &HashSet<usize, ahash::RandomState>)> {
        self.items
            .get_mut(index)
            .map(|(value, links)| (value, &*links))
    }

    #[inline]
    pub fn get_item_mut2<Q>(
        &mut self,
        key: &Q,
    ) -> Option<(&mut IV, &HashSet<usize, ahash::RandomState>)>
    where
        IK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.items
            .get_mut2(key)
            .map(|(value, links)| (value, &*links))
    }

    #[inline]
    pub fn get_item_key(&self, index: usize) -> Option<&IK> {
        self.items.get_key(index)
    }

    #[inline]
    pub fn iter_group(&self) -> impl Iterator<Item = (&GK, usize, &GV)> {
        self.groups
            .iter()
            .map(|(key, index, (value, _))| (key, index, value))
    }

    #[inline]
    pub fn iter_item(&self) -> impl Iterator<Item = (&IK, usize, &IV)> {
        self.items
            .iter()
            .map(|(key, index, (value, _))| (key, index, value))
    }

    /// # Panics
    ///
    /// - Panics if `desc` doesn't have item at all.
    /// - Panics if the map has had group already.
    pub fn add_group(&mut self, desc: impl DescribeGroup<GK, GV, IK, IV>) -> usize {
        let (group_key, group_value, items) = desc.into_group_and_items();

        // Group must contain at least one item.
        assert!(!items.is_empty());

        // Adds items.
        let item_indices = items
            .into_iter()
            .map(|(key, item)| {
                if let Some(index) = self.items.get_index(&key) {
                    index
                } else {
                    self.items.insert(key, (item, HashSet::default())).0
                }
            })
            .collect::<Vec<_>>();

        // Adds group.
        let (group_index, old_group) = self
            .groups
            .insert(group_key, (group_value, item_indices.clone()));

        // This method doesn't allow overwriting group.
        assert!(old_group.is_none());

        // Updates items by adding new link to the group.
        for index in item_indices {
            let (_, links) = self.items.get_mut(index).unwrap();
            links.insert(group_index);
        }

        group_index
    }

    pub fn remove_group(&mut self, index: usize) -> Option<GV> {
        // Removes group.
        let group_index = index;
        let (old_group, item_indices) = self.groups.remove(group_index)?;

        // Removes corresponding items if it's possible.
        for item_index in item_indices.iter().cloned() {
            let (_, group_indices) = self.items.get_mut(item_index).unwrap();
            group_indices.remove(&group_index);
            if group_indices.is_empty() {
                self.items.remove(item_index);
            }
        }

        Some(old_group)
    }

    pub fn remove_group2<Q>(&mut self, key: &Q) -> Option<GV>
    where
        GK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let index = self.groups.get_index(key)?;
        self.remove_group(index)
    }
}

impl<GK, GV, IK, IV> Default for GroupMap<GK, GV, IK, IV>
where
    GK: Hash + Eq + Clone,
    IK: Hash + Eq + Clone,
{
    fn default() -> Self {
        Self::new()
    }
}

pub trait DescribeGroup<GK, GV, IK, IV> {
    fn into_group_and_items(self) -> (GK, GV, Vec<(IK, IV)>);
}

/// A hash-map with indexing.
/// This guarantees that index won't change after insertion or deletion of any items except itself.
/// Therefore, you can always find value with its index.
/// In most cases, that would be faster than finding it by key.
///
/// You can also declare the map with the `IMAP` flag, which enables funcionality of finding keys from indices.
/// Default is true.
/// But note that enabling it requires more memory.
/// Notice that if you make two dictionaries with true and false IMAP, it will bloat your code size.
#[derive(Debug, Clone)]
pub struct IndexedMap<K, V, const IMAP: bool = true> {
    /// Key -> index of [`Self::values`].
    map: HashMap<K, usize, ahash::RandomState>,

    /// Index of [`Self::values`] -> Key. Optional.
    imap: OptVec<K>,

    /// Values.
    values: OptVec<V>,
}

impl<K, V> IndexedMap<K, V, true> {
    pub fn get_key(&self, index: usize) -> Option<&K> {
        self.imap.get(index)
    }
}

impl<K: Hash + Eq + Clone, V> IndexedMap<K, V, true> {
    /// Removes index from the map and returns its corresponding value.
    pub fn remove(&mut self, index: usize) -> Option<V> {
        let key = self.imap.get(index)?;
        self.map.remove(key);
        self.imap.take(index);
        self.values.take(index)
    }
}

impl<K: Hash + Eq + Clone, V, const IMAP: bool> IndexedMap<K, V, IMAP> {
    pub fn new() -> Self {
        Self {
            map: HashMap::default(),
            imap: OptVec::new(),
            values: OptVec::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.values.len_occupied()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn contains_key<Q>(&self, key: &Q) -> bool
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.map.contains_key(key)
    }

    pub fn contains_index(&self, index: usize) -> bool {
        index < self.values.len() && self.values.is_occupied(index)
    }

    /// Inserts `value` with `key`.  
    /// If the map has had the `key`, value is changed while its index isn't.
    /// Then returns value's index and old value if it's changed.
    pub fn insert(&mut self, key: K, value: V) -> (usize, Option<V>) {
        match self.map.entry(key) {
            Entry::Occupied(occupied) => {
                let index = *occupied.get();
                let old_value = self.values.set(index, Some(value));
                (index, old_value)
            }
            Entry::Vacant(vacant) => {
                let index = self.values.add(value);
                if IMAP {
                    self.imap.add(vacant.key().clone());
                }
                vacant.insert(index);
                (index, None)
            }
        }
    }

    /// Removes `key` from the map and returns its corresponding value.
    pub fn remove2<Q>(&mut self, key: &Q) -> Option<V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let index = self.map.remove(key)?;
        if IMAP {
            self.imap.take(index);
        }
        self.values.take(index)
    }

    /// Retrieves index corresponding to the `key`.
    #[inline]
    pub fn get_index<Q>(&self, key: &Q) -> Option<usize>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.map.get(key).cloned()
    }

    /// Retrieves value corresponding to the `index`.  
    /// It can be None if `index` is out of bound or points to a vacant slot.
    #[inline]
    pub fn get(&self, index: usize) -> Option<&V> {
        self.values.get(index)
    }

    /// Retrieves value corresponding to the `key`.
    pub fn get2<Q>(&self, key: &Q) -> Option<&V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let index = self.get_index(key)?;
        self.get(index)
    }

    /// Retrieves value corresponding to the `index`.  
    /// It can be None if `index` is out of bound or points to a vacant slot.
    #[inline]
    pub fn get_mut(&mut self, index: usize) -> Option<&mut V> {
        self.values.get_mut(index)
    }

    /// Retrieves value corresponding to the `key`.
    pub fn get_mut2<Q>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let index = self.get_index(key)?;
        self.get_mut(index)
    }

    /// Returns iterator visiting all keys, indices, and values in arbitrary order.
    pub fn iter(&self) -> impl Iterator<Item = (&K, usize, &V)> {
        self.map.iter().map(|(key, &index)| {
            let value = self.values.get(index).unwrap();
            (key, index, value)
        })
    }

    /// Returns iterator visiting all keys, indices, and values in arbitrary order.
    #[inline]
    pub fn iter_mut(&mut self) -> IndexMapIterMut<K, V> {
        IndexMapIterMut {
            map_iter: self.map.iter(),
            values: &mut self.values,
        }
    }

    /// Returns iterator visiting all indices and values in the order of index.
    #[inline]
    pub fn values(&self) -> impl Iterator<Item = (usize, &V)> {
        self.values.iter_occupied()
    }
}

impl<K: Hash + Eq + Clone, V, const IMAP: bool> Default for IndexedMap<K, V, IMAP> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct IndexMapIterMut<'a, K, V> {
    map_iter: std::collections::hash_map::Iter<'a, K, usize>,
    values: &'a mut OptVec<V>,
}

impl<'a, K, V> Iterator for IndexMapIterMut<'a, K, V> {
    type Item = (&'a K, usize, &'a mut V);

    fn next(&mut self) -> Option<Self::Item> {
        let (key, &index) = self.map_iter.next()?;
        // Safety: Breaks borrow rules. But it's Ok because this iterator is holding mutable reference
        // of values. That means values can't be dropped and it lives longer than the iterator.
        let value = unsafe { &mut *(self.values.get_mut(index)? as *mut V) };
        Some((key, index, value))
    }
}

// Don't implement From for &mut [Option<V>] because it can break mapping.
impl<'a, K, V, const IMAP: bool> From<&'a IndexedMap<K, V, IMAP>> for &'a [Option<V>] {
    #[inline]
    fn from(value: &'a IndexedMap<K, V, IMAP>) -> Self {
        value.values.as_slice()
    }
}
