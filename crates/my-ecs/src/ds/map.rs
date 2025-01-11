use crate::ds::vec::OptVec;
use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    hash::{BuildHasher, Hash},
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
pub struct GroupMap<GK, GV, IK, IV, S> {
    /// Each group contains some items.
    /// You can access each group by its key or index.
    groups: IndexedMap<GK, (GV, Vec<usize>), S>,

    /// Each item belong to some groups.
    /// You can access each item by its key or index.
    items: IndexedMap<IK, (IV, HashSet<usize, S>), S>,
}

impl<GK, GV, IK, IV, S> GroupMap<GK, GV, IK, IV, S>
where
    S: Default,
{
    pub fn new() -> Self {
        Self {
            groups: IndexedMap::new(),
            items: IndexedMap::new(),
        }
    }
}

impl<GK, GV, IK, IV, S> GroupMap<GK, GV, IK, IV, S>
where
    GK: Hash + Eq + Clone,
    IK: Hash + Eq + Clone,
    S: BuildHasher,
{
    pub fn contains_group(&self, index: usize) -> bool {
        self.groups.contains_index(index)
    }

    pub fn contains_group2<Q>(&self, key: &Q) -> bool
    where
        GK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.groups.contains_key(key)
    }

    pub fn get_group(&self, index: usize) -> Option<(&GV, &Vec<usize>)> {
        self.groups.get(index).map(|(value, links)| (value, links))
    }

    pub fn get_group2<Q>(&self, key: &Q) -> Option<(&GV, &Vec<usize>)>
    where
        GK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.groups.get2(key).map(|(value, links)| (value, links))
    }

    pub fn get_group_mut(&mut self, index: usize) -> Option<(&mut GV, &Vec<usize>)> {
        self.groups
            .get_mut(index)
            .map(|(value, links)| (value, &*links))
    }

    pub fn get_group_mut2<Q>(&mut self, key: &Q) -> Option<(&mut GV, &Vec<usize>)>
    where
        GK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.groups
            .get_mut2(key)
            .map(|(value, links)| (value, &*links))
    }

    pub fn get_group_key(&self, index: usize) -> Option<&GK> {
        self.groups.get_key(index)
    }

    pub fn get_group_index<Q>(&self, key: &Q) -> Option<usize>
    where
        GK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.groups.get_index(key)
    }

    pub fn contains_item(&self, index: usize) -> bool {
        self.items.contains_index(index)
    }

    pub fn contains_item2<Q>(&self, key: &Q) -> bool
    where
        IK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.items.contains_key(key)
    }

    pub fn get_item(&self, index: usize) -> Option<&(IV, HashSet<usize, S>)> {
        self.items.get(index)
    }

    pub fn get_item2<Q>(&self, key: &Q) -> Option<&(IV, HashSet<usize, S>)>
    where
        IK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.items.get2(key)
    }

    pub fn get_item_mut(&mut self, index: usize) -> Option<(&mut IV, &HashSet<usize, S>)> {
        self.items
            .get_mut(index)
            .map(|(value, links)| (value, &*links))
    }

    pub fn get_item_mut2<Q>(&mut self, key: &Q) -> Option<(&mut IV, &HashSet<usize, S>)>
    where
        IK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.items
            .get_mut2(key)
            .map(|(value, links)| (value, &*links))
    }

    pub fn get_item_key(&self, index: usize) -> Option<&IK> {
        self.items.get_key(index)
    }

    pub fn iter_group(&self) -> impl Iterator<Item = (&GK, usize, &GV)> {
        self.groups
            .iter()
            .map(|(key, index, (value, _))| (key, index, value))
    }

    pub fn iter_item(&self) -> impl Iterator<Item = (&IK, usize, &IV)> {
        self.items
            .iter()
            .map(|(key, index, (value, _))| (key, index, value))
    }
}

impl<GK, GV, IK, IV, S> GroupMap<GK, GV, IK, IV, S>
where
    GK: Hash + Eq + Clone,
    IK: Hash + Eq + Clone,
    S: BuildHasher + Default,
{
    /// Returns an index that will be returned when the next
    /// [`add_group`](Self::add_group) or
    /// [`add_group_from_desc`](Self::add_group_from_desc) is called.
    pub fn next_index<Q>(&self, key: &Q) -> usize
    where
        GK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.groups.next_index(key)
    }

    pub fn add_group(
        &mut self,
        desc: impl DescribeGroup<GK, GV, IK, IV>,
    ) -> Result<usize, GroupDesc<GK, GV, IK, IV>> {
        self.add_group_from_desc(desc.into_group_and_items())
    }

    pub fn add_group_from_desc(
        &mut self,
        desc: GroupDesc<GK, GV, IK, IV>,
    ) -> Result<usize, GroupDesc<GK, GV, IK, IV>> {
        // Validates the descriptor.
        assert!(!desc.items.is_empty());
        if self.contains_group2(&desc.group_key) {
            return Err(desc);
        }

        let GroupDesc {
            group_key,
            group_value,
            items,
        } = desc;

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
        debug_assert!(old_group.is_none());

        // Updates items by adding new link to the group.
        for index in item_indices {
            let (_, links) = self.items.get_mut(index).unwrap();
            links.insert(group_index);
        }

        Ok(group_index)
    }

    pub fn remove_group(&mut self, index: usize) -> Option<(GK, GV)> {
        // Removes group.
        let group_index = index;
        let (group_key, (old_group, item_indices)) = self.groups.remove_entry(group_index)?;

        // Removes corresponding items if it's possible.
        for item_index in item_indices.iter().cloned() {
            let (_, group_indices) = self.items.get_mut(item_index).unwrap();
            group_indices.remove(&group_index);
            if group_indices.is_empty() {
                self.items.remove(item_index);
            }
        }

        Some((group_key, old_group))
    }

    pub fn remove_group2<Q>(&mut self, key: &Q) -> Option<(GK, GV)>
    where
        GK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let index = self.groups.get_index(key)?;
        self.remove_group(index)
    }
}

impl<GK, GV, IK, IV, S> Default for GroupMap<GK, GV, IK, IV, S>
where
    S: Default,
{
    fn default() -> Self {
        Self::new()
    }
}

pub trait DescribeGroup<GK, GV, IK, IV> {
    fn into_group_and_items(self) -> GroupDesc<GK, GV, IK, IV>;
}

#[derive(Debug)]
pub struct GroupDesc<GK, GV, IK, IV> {
    pub group_key: GK,
    pub group_value: GV,
    pub items: Vec<(IK, IV)>,
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
pub struct IndexedMap<K, V, S, const IMAP: bool = true> {
    /// Key -> index of [`Self::values`].
    map: HashMap<K, usize, S>,

    /// Index of [`Self::values`] -> Key. Optional.
    imap: OptVec<K, S>,

    /// Values.
    values: OptVec<V, S>,
}

impl<K, V, S> IndexedMap<K, V, S, true>
where
    S: BuildHasher,
{
    pub fn get_key(&self, index: usize) -> Option<&K> {
        self.imap.get(index)
    }
}

impl<K, V, S> IndexedMap<K, V, S, true>
where
    K: Hash + Eq + Clone,
    S: BuildHasher,
{
    /// Removes index from the map and returns its corresponding value.
    pub fn remove(&mut self, index: usize) -> Option<V> {
        self.remove_entry(index).map(|(_key, value)| value)
    }

    pub fn remove_entry(&mut self, index: usize) -> Option<(K, V)> {
        let key = self.imap.get(index)?;
        let must_some = self.map.remove(key);
        debug_assert!(must_some.is_some());
        // Safety: The entry exists, checked by `?` above.
        unsafe {
            let key = self.imap.take(index).unwrap_unchecked();
            let value = self.values.take(index).unwrap_unchecked();
            Some((key, value))
        }
    }
}

impl<K, V, S, const IMAP: bool> IndexedMap<K, V, S, IMAP>
where
    S: Default,
{
    pub fn new() -> Self {
        Self {
            map: HashMap::default(),
            imap: OptVec::new(),
            values: OptVec::new(),
        }
    }
}

impl<K, V, S, const IMAP: bool> IndexedMap<K, V, S, IMAP>
where
    K: Hash + Eq + Clone,
    S: BuildHasher,
{
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

    /// Returns an index that will be returned when the next
    /// [`insert`](Self::insert) is called.
    pub fn next_index<Q>(&self, key: &Q) -> usize
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        if let Some(index) = self.map.get(key) {
            *index
        } else {
            self.values.next_index()
        }
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
                    self.imap.extend_set(index, vacant.key().clone());
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
        self.remove_entry2(key).map(|(_key, value)| value)
    }

    pub fn remove_entry2<Q>(&mut self, key: &Q) -> Option<(K, V)>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let (key, index) = self.map.remove_entry(key)?;
        if IMAP {
            let must_some = self.imap.take(index);
            debug_assert!(must_some.is_some());
        }
        // Safety: We got `index` from `self.map`, which guarantees that the
        // slot must be occupied.
        let value = unsafe { self.values.take(index).unwrap_unchecked() };
        Some((key, value))
    }

    /// Retrieves index corresponding to the `key`.
    pub fn get_index<Q>(&self, key: &Q) -> Option<usize>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.map.get(key).cloned()
    }

    /// Retrieves value corresponding to the `index`.
    /// It can be None if `index` is out of bound or points to a vacant slot.
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
    pub fn iter_mut(&mut self) -> IndexMapIterMut<K, V, S> {
        IndexMapIterMut {
            map_iter: self.map.iter(),
            values: &mut self.values,
        }
    }

    /// Returns iterator visiting all indices and values in the order of index.
    pub fn values(&self) -> impl Iterator<Item = (usize, &V)> {
        self.values.iter_occupied()
    }
}

impl<K, V, S, const IMAP: bool> Default for IndexedMap<K, V, S, IMAP>
where
    S: Default,
{
    fn default() -> Self {
        Self::new()
    }
}

pub struct IndexMapIterMut<'a, K, V, S> {
    map_iter: std::collections::hash_map::Iter<'a, K, usize>,
    values: &'a mut OptVec<V, S>,
}

impl<'a, K, V, S> Iterator for IndexMapIterMut<'a, K, V, S>
where
    S: BuildHasher,
{
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
impl<'a, K, V, S, const IMAP: bool> From<&'a IndexedMap<K, V, S, IMAP>> for &'a [Option<V>]
where
    S: BuildHasher,
{
    fn from(value: &'a IndexedMap<K, V, S, IMAP>) -> Self {
        value.values.as_slice()
    }
}
