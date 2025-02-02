use crate::ds::vec::OptVec;
use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    hash::{BuildHasher, Hash},
};

/// A hash map containing *Group*s and *Item*s.
///
/// Conceptually, group contains items in it, and item can belong to multiple
/// groups. They cannot exist without relationship to each other. In other
/// words, they must be linked. See a diagram below. In this map, group has an
/// ordered links to items, while item has an unordered links to groups.
///
/// ```text
/// Groups    G0  G1
///           /\  /\
/// Items   I0  I1  I2
/// ```
///
/// The map provides you some ways to access groups and items by their keys and
/// indices. If possible, prefer to use index to key because it can be faster.
#[derive(Debug, Clone)]
pub struct GroupMap<GK, GV, IK, IV, S> {
    /// Groups that can be accessed by either key or index.
    ///
    /// Key: Group key `GK`.
    /// Value: Group value `GV` and indices to `items`.
    groups: IndexedMap<GK, (GV, Vec<usize>), S>,

    /// Items that can be accessed by either key or index.
    ///
    /// Key: Item key `IK`.
    /// Value: Item value `IV` and indices to `groups`.
    items: IndexedMap<IK, (IV, HashSet<usize, S>), S>,
}

impl<GK, GV, IK, IV, S> GroupMap<GK, GV, IK, IV, S>
where
    S: Default,
{
    /// Creates a new empty map.
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
    /// Returns true if the map contains a group at the given group index.
    ///
    /// Consider using [`GroupMap::contains_group2`] if you need to know whether
    /// the map contains it or not by a key.
    pub fn contains_group(&self, index: usize) -> bool {
        self.groups.contains_index(index)
    }

    /// Returns true if the map contains a group corresponding to the given
    /// group key.
    ///
    /// Consider using [`GroupMap::contains_group`] if you need to know whether
    /// the map contains it or not by an index.
    pub fn contains_group2<Q>(&self, key: &Q) -> bool
    where
        GK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.groups.contains_key(key)
    }

    /// Retrieves a shared reference to a group and related item indices by the
    /// given group index.
    ///
    /// Consider using [`GroupMap::get_group2`] if you need to get a group by
    /// a key.
    pub fn get_group(&self, index: usize) -> Option<(&GV, &Vec<usize>)> {
        self.groups.get(index).map(|(value, links)| (value, links))
    }

    /// Retrieves a shared reference to a group and related item indices by the
    /// given group key.
    ///
    /// Consider using [`GroupMap::get_group`] if you need to get a group by
    /// an index.
    pub fn get_group2<Q>(&self, key: &Q) -> Option<(&GV, &Vec<usize>)>
    where
        GK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.groups.get2(key).map(|(value, links)| (value, links))
    }

    /// Retrieves a mutable reference to a group and related item indices by the
    /// given group index.
    ///
    /// Consider using [`GroupMap::get_group_mut2`] if you need to get a group
    /// by a key.
    pub fn get_group_mut(&mut self, index: usize) -> Option<(&mut GV, &Vec<usize>)> {
        self.groups
            .get_mut(index)
            .map(|(value, links)| (value, &*links))
    }

    /// Retrieves a mutable reference to a group and related item indices by the
    /// given group key.
    ///
    /// Consider using [`GroupMap::get_group_mut`] if you need to get a group
    /// by an index.
    pub fn get_group_mut2<Q>(&mut self, key: &Q) -> Option<(&mut GV, &Vec<usize>)>
    where
        GK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.groups
            .get_mut2(key)
            .map(|(value, links)| (value, &*links))
    }

    /// Retrieves group key corresponding to the given group index.
    ///
    /// You can also get a group index from a key using
    /// [`GroupMap::get_group_index`].
    pub fn get_group_key(&self, index: usize) -> Option<&GK> {
        self.groups.get_key(index)
    }

    /// Retrieves a group index corresponding to the given group key.
    ///
    /// You can also get a group key from an index using
    /// [`GroupMap::get_group_key`].
    pub fn get_group_index<Q>(&self, key: &Q) -> Option<usize>
    where
        GK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.groups.get_index(key)
    }

    /// Returns an iterator visiting all groups.
    ///
    /// The iterator yields pairs of group key, group index, and shared
    /// reference to group value.
    pub fn iter_group(&self) -> impl Iterator<Item = (&GK, usize, &GV)> {
        self.groups
            .iter()
            .map(|(key, index, (value, _))| (key, index, value))
    }

    /// Returns true if the map contains an item at the given item index.
    ///
    /// Consider using [`GroupMap::contains_item2`] if you need to know whether
    /// the map contains it or not by a key.
    pub fn contains_item(&self, index: usize) -> bool {
        self.items.contains_index(index)
    }

    /// Returns true if the map contains an item corresponding to the given item
    /// key.
    ///
    /// Consider using [`GroupMap::contains_item`] if you need to know whether
    /// the map contains it or not by an index.
    pub fn contains_item2<Q>(&self, key: &Q) -> bool
    where
        IK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.items.contains_key(key)
    }

    /// Retrieves a shared reference to an item and related group indices by the
    /// given item index.
    ///
    /// Consider using [`GroupMap::get_item2`] if you need to get an item by a
    /// key.
    pub fn get_item(&self, index: usize) -> Option<&(IV, HashSet<usize, S>)> {
        self.items.get(index)
    }

    /// Retrieves a shared reference to an item and related group indices by the
    /// given item key.
    ///
    /// Consider using [`GroupMap::get_item`] if you need to get an item by an
    /// index.
    pub fn get_item2<Q>(&self, key: &Q) -> Option<&(IV, HashSet<usize, S>)>
    where
        IK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.items.get2(key)
    }

    /// Retrieves a mutable reference to an item and related group indices by
    /// the given item index.
    ///
    /// Consider using [`GroupMap::get_item_mut2`] if you need to get an item by
    /// a key.
    pub fn get_item_mut(&mut self, index: usize) -> Option<(&mut IV, &HashSet<usize, S>)> {
        self.items
            .get_mut(index)
            .map(|(value, links)| (value, &*links))
    }

    /// Retrieves a mutable reference to an item and related group indices by
    /// the given item key.
    ///
    /// Consider using [`GroupMap::get_item_mut`] if you need to get an item by
    /// an index.
    pub fn get_item_mut2<Q>(&mut self, key: &Q) -> Option<(&mut IV, &HashSet<usize, S>)>
    where
        IK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.items
            .get_mut2(key)
            .map(|(value, links)| (value, &*links))
    }

    /// Retrieves an item key corresponding to the given item index.
    ///
    /// You can also get an item index from a key using
    /// [`GroupMap::get_item_index`].
    pub fn get_item_key(&self, index: usize) -> Option<&IK> {
        self.items.get_key(index)
    }

    /// Retrieves an item index corresponding to the given item key.
    ///
    /// You can also get an item key from an index using
    /// [`GroupMap::get_item_key`].
    pub fn get_item_index<Q>(&self, key: &Q) -> Option<usize>
    where
        IK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.items.get_index(key)
    }

    /// Returns an iterator visiting all items.
    ///
    /// The iterator yields pairs of item key, item index, and shared
    /// reference to item value.
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
    /// Returns the next index that will be returned on the next call to
    /// either [`GroupMap::add_group`] or [`GroupMap::add_group_from_desc`].
    pub fn next_index<Q>(&self, key: &Q) -> usize
    where
        GK: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.groups.next_index(key)
    }

    /// Inserts a group and related items into the map from the given group
    /// descriptor.
    ///
    /// This method is a simple wrapper of [`GroupMap::add_group_from_desc`] for
    /// easy use.
    pub fn add_group(
        &mut self,
        desc: impl DescribeGroup<GK, GV, IK, IV>,
    ) -> Result<usize, GroupDesc<GK, GV, IK, IV>> {
        self.add_group_from_desc(desc.into_group_and_items())
    }

    /// Inserts a group and related items into the map from the given group
    /// descriptor.
    ///
    /// Note that this method doesn't overwrite anything. Therefore, if the map
    /// already contains the same group key, returns error. With respect to
    /// item, only relation to the group is adapted and item value is dropped
    /// if the map already contains the item. If you want replace, remove old
    /// one first.
    ///
    /// # Panics
    ///
    /// Panics if the descriptor doesn't contain any items in it. Group cannot
    /// exist without relationship with items. See [`GroupMap`] documentation
    /// for more details.
    pub fn add_group_from_desc(
        &mut self,
        desc: GroupDesc<GK, GV, IK, IV>,
    ) -> Result<usize, GroupDesc<GK, GV, IK, IV>> {
        // Panics: Group must contain related items.
        assert!(!desc.items.is_empty());

        // Err: This method doesn't allow overwriting.
        if self.contains_group2(&desc.group_key) {
            return Err(desc);
        }

        let GroupDesc {
            group_key,
            group_value,
            items,
        } = desc;

        // Adds each item if the map doesn't contain it.
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
        debug_assert!(old_group.is_none());

        // Updates items by adding new link to the group.
        for index in item_indices {
            let (_, links) = self.items.get_mut(index).unwrap();
            links.insert(group_index);
        }

        Ok(group_index)
    }

    /// Removes a group at the given group index from the map.
    ///
    /// Related items are automatically removed as well if they don't have
    /// relationships anymore by the removal of the group.
    ///
    /// Consider using [`GroupMap::remove_group2`] if you need to remove a group
    /// by a key.
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

    /// Removes a group corresponding to the given group key from the map.
    ///
    /// Related items are automatically removed as well if they don't have
    /// relationships anymore by the removal of the group.
    ///
    /// Consider using [`GroupMap::remove_group`] if you need to remove a group
    /// by an index.
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

/// A trait for creating [`GroupDesc`].
pub trait DescribeGroup<GK, GV, IK, IV> {
    fn into_group_and_items(self) -> GroupDesc<GK, GV, IK, IV>;
}

/// A descriptor for [`GroupMap`].
///
/// `GroupMap` is a map containing groups and items. This descriptor describes
/// a group with its key, value, and associated items.
#[derive(Debug)]
pub struct GroupDesc<GK, GV, IK, IV> {
    pub group_key: GK,
    pub group_value: GV,
    pub items: Vec<(IK, IV)>,
}

/// A hash map that you can find a value by an index as well.
///
/// It's encouraged to prefer accessing item by an index over using a key
/// because it is simpler in terms of required operations. Plus, values are laid
/// on sequential memory block, so that we can expect faster iteration as well.
/// However, the map allocates more memory than usual hash map.
///
/// If you want to use an index as key interchangeably, then set `IMAP` to true.
/// Then, the map keeps `index->key` relation as well so that the map can find
/// a corresponding key from an index.
#[derive(Debug, Clone)]
pub struct IndexedMap<K, V, S, const IMAP: bool = true> {
    /// Key -> An index to an item of [`Self::values`].
    map: HashMap<K, usize, S>,

    /// Index to item of [`Self::values`] -> Key.
    ///
    /// This field is used only when `IMAP = true`.
    imap: OptVec<K, S>,

    /// Values.
    values: OptVec<V, S>,
}

impl<K, V, S> IndexedMap<K, V, S, true>
where
    S: BuildHasher,
{
    /// Retrieves a shared reference to a key corresponding to the given index.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IndexedMap;
    /// use std::hash::RandomState;
    ///
    /// let mut map = IndexedMap::<_, _, RandomState>::new();
    /// let (index, _) = map.insert('a', "alpha");
    /// assert_eq!(map.get_key(index), Some(&'a'));
    /// ```
    pub fn get_key(&self, index: usize) -> Option<&K> {
        self.imap.get(index)
    }
}

impl<K, V, S> IndexedMap<K, V, S, true>
where
    K: Hash + Eq + Clone,
    S: BuildHasher,
{
    /// Removes a value at the given index then returns it.
    ///
    /// Consider using [`IndexedMap::remove2`] if you need to remove a value
    /// by a key.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IndexedMap;
    /// use std::hash::RandomState;
    ///
    /// let mut map = IndexedMap::<_, _, RandomState>::new();
    /// let (index, _) = map.insert('a', "alpha");
    /// assert_eq!(map.remove(index), Some("alpha"));
    /// ```
    pub fn remove(&mut self, index: usize) -> Option<V> {
        self.remove_entry(index).map(|(_key, value)| value)
    }

    /// Removes a value at the given index then returns it with the
    /// corresponding key.
    ///
    /// Consider using [`IndexedMap::remove_entry2`] if you need to remove a
    /// value by a key.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IndexedMap;
    /// use std::hash::RandomState;
    ///
    /// let mut map = IndexedMap::<_, _, RandomState>::new();
    /// let (index, _) = map.insert('a', "alpha");
    /// assert_eq!(map.remove_entry(index), Some(('a', "alpha")));
    /// ```
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
    /// Creates a new empty map.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IndexedMap;
    /// use std::hash::RandomState;
    ///
    /// let map = IndexedMap::<char, &'static str, RandomState>::new();
    /// ```
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
    /// Returns number of items.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IndexedMap;
    /// use std::hash::RandomState;
    ///
    /// let mut map = IndexedMap::<_, _, RandomState>::new();
    /// map.insert('a', "alpha");
    /// assert_eq!(map.len(), 1);
    /// ```
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Returns true if the map is empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IndexedMap;
    /// use std::hash::RandomState;
    ///
    /// let map = IndexedMap::<char, &'static str, RandomState>::new();
    /// assert!(map.is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns true if the map contains the given key.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IndexedMap;
    /// use std::hash::RandomState;
    ///
    /// let mut map = IndexedMap::<_, _, RandomState>::new();
    /// map.insert('a', "alpha");
    /// assert!(map.contains_key(&'a'));
    /// ```
    pub fn contains_key<Q>(&self, key: &Q) -> bool
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.map.contains_key(key)
    }

    /// Returns true if the map contains value corresponding to the given index.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IndexedMap;
    /// use std::hash::RandomState;
    ///
    /// let mut map = IndexedMap::<_, _, RandomState>::new();
    /// let (index, _) = map.insert('a', "alpha");
    /// assert!(map.contains_index(index));
    /// ```
    pub fn contains_index(&self, index: usize) -> bool {
        self.values.get(index).is_some()
    }

    /// Returns the next index that will be returned on the next call to
    /// [`IndexedMap::insert`] with the given key.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IndexedMap;
    /// use std::hash::RandomState;
    ///
    /// let mut map = IndexedMap::<_, _, RandomState>::new();
    /// let next_index = map.next_index(&'a');
    /// let (index, _) = map.insert('a', "alpha");
    /// assert_eq!(next_index, index);
    /// ```
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

    /// Inserts the given key-value pair into the map and returns corresponding
    /// index.
    ///
    /// If the map contains the key, then replaces values and returns old value.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IndexedMap;
    /// use std::hash::RandomState;
    ///
    /// let mut map = IndexedMap::<_, _, RandomState>::new();
    /// map.insert('a', "alpha");
    /// let (_, old) = map.insert('a', "ah");
    /// assert_eq!(old, Some("alpha"));
    /// ```
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

    /// Removes a value corresponding to the given key then returns it.
    ///
    /// Consider using [`IndexedMap::remove`] if you need to remove a value by
    /// an index.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IndexedMap;
    /// use std::hash::RandomState;
    ///
    /// let mut map = IndexedMap::<_, _, RandomState>::new();
    /// map.insert('a', "alpha");
    /// assert_eq!(map.remove2(&'a'), Some("alpha"));
    /// ```
    pub fn remove2<Q>(&mut self, key: &Q) -> Option<V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.remove_entry2(key).map(|(_key, value)| value)
    }

    /// Removes a value corresponding to the given key then returns it with the
    /// key.
    ///
    /// Consider using [`IndexedMap::remove_entry`] if you need to remove a
    /// value by an index.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IndexedMap;
    /// use std::hash::RandomState;
    ///
    /// let mut map = IndexedMap::<_, _, RandomState>::new();
    /// map.insert('a', "alpha");
    /// assert_eq!(map.remove_entry2(&'a'), Some(('a', "alpha")));
    /// ```
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

    /// Retrieves an index corresponding to the given key.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IndexedMap;
    /// use std::hash::RandomState;
    ///
    /// let mut map = IndexedMap::<_, _, RandomState>::new();
    /// let (index, _) = map.insert('a', "alpha");
    /// assert_eq!(map.get_index(&'a'), Some(index));
    /// ```
    pub fn get_index<Q>(&self, key: &Q) -> Option<usize>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.map.get(key).cloned()
    }

    /// Retrieves a shared reference to a value at the given index.
    ///
    /// Consider using [`IndexedMap::get2`] if you need to get a value by a key.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IndexedMap;
    /// use std::hash::RandomState;
    ///
    /// let mut map = IndexedMap::<_, _, RandomState>::new();
    /// let (index, _) = map.insert('a', "alpha");
    /// assert_eq!(map.get(index), Some(&"alpha"));
    /// ```
    pub fn get(&self, index: usize) -> Option<&V> {
        self.values.get(index)
    }

    /// Retrieves a shared reference to a value corresponding to the given key.
    ///
    /// Consider using [`IndexedMap::get`] if you need to get a value by an
    /// index.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IndexedMap;
    /// use std::hash::RandomState;
    ///
    /// let mut map = IndexedMap::<_, _, RandomState>::new();
    /// let (index, _) = map.insert('a', "alpha");
    /// assert_eq!(map.get2(&'a'), Some(&"alpha"));
    /// ```
    pub fn get2<Q>(&self, key: &Q) -> Option<&V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let index = self.get_index(key)?;
        self.get(index)
    }

    /// Retrieves a mutable reference to a value at the given index.
    ///
    /// Consider using [`IndexedMap::get_mut2`] if you need to get a value by a
    /// key.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IndexedMap;
    /// use std::hash::RandomState;
    ///
    /// let mut map = IndexedMap::<_, _, RandomState>::new();
    /// let (index, _) = map.insert('a', "alpha");
    /// assert_eq!(map.get_mut(index), Some(&mut "alpha"));
    /// ```
    pub fn get_mut(&mut self, index: usize) -> Option<&mut V> {
        self.values.get_mut(index)
    }

    /// Retrieves a mutable reference to a value corresponding to the given key.
    ///
    /// Consider using [`IndexedMap::get_mut`] if you need to get a value by an
    /// index.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IndexedMap;
    /// use std::hash::RandomState;
    ///
    /// let mut map = IndexedMap::<_, _, RandomState>::new();
    /// let (index, _) = map.insert('a', "alpha");
    /// assert_eq!(map.get_mut2(&'a'), Some(&mut "alpha"));
    /// ```
    pub fn get_mut2<Q>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let index = self.get_index(key)?;
        self.get_mut(index)
    }

    /// Returns an iterator visiting key-index-value pairs in the map in
    /// arbitrary order.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IndexedMap;
    /// use std::hash::RandomState;
    ///
    /// let mut map = IndexedMap::<_, _, RandomState>::new();
    /// map.insert('a', "alpha");
    /// map.insert('b', "beta");
    /// for (key, index, value) in map.iter() {
    ///     println!("{key}, {index}, {value}");
    /// }
    /// ```
    pub fn iter(&self) -> impl Iterator<Item = (&K, usize, &V)> + Clone {
        self.map.iter().map(|(key, &index)| {
            let value = self.values.get(index).unwrap();
            (key, index, value)
        })
    }

    /// Returns a mutable iterator visiting key-index-value pairs in the map in
    /// arbitrary order.
    ///
    /// You can modify values only through the iterator.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IndexedMap;
    /// use std::hash::RandomState;
    ///
    /// let mut map = IndexedMap::<_, _, RandomState>::new();
    /// map.insert('a', "alpha".to_owned());
    /// map.insert('b', "beta".to_owned());
    /// for (key, index, value) in map.iter_mut() {
    ///     value.push('*');
    ///     println!("{key}, {index}, {value}");
    /// }
    /// ```
    // pub fn iter_mut(&mut self) -> IndexMapIterMut<K, V, S> {
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&K, usize, &mut V)> {
        self.map.iter().map(|(key, &index)| {
            // Safety: We're braking borrow rules about `&mut V` by converting
            // reference to pointer. But it's safe because the container `self`
            // is still being borrowed.
            let value = self.values.get_mut(index).unwrap();
            let value = value as *mut V;
            let value = unsafe { value.as_mut().unwrap_unchecked() };
            (key, index, value)
        })
    }

    /// Returns an iterator visiting all values.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IndexedMap;
    /// use std::hash::RandomState;
    ///
    /// let mut map = IndexedMap::<_, _, RandomState>::new();
    /// map.insert('a', "alpha");
    /// map.insert('b', "beta");
    /// for v in map.values() {
    ///     println!("{v}");
    /// }
    /// ```
    pub fn values(&self) -> impl Iterator<Item = &V> + Clone {
        self.values.iter()
    }

    /// Returns a mutable iterator visiting all values.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IndexedMap;
    /// use std::hash::RandomState;
    ///
    /// let mut map = IndexedMap::<_, _, RandomState>::new();
    /// map.insert('a', "alpha".to_owned());
    /// map.insert('b', "beta".to_owned());
    /// for v in map.values_mut() {
    ///     v.push('*');
    ///     println!("{v}");
    /// }
    /// ```
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.values.iter_mut()
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

// Don't implement From for &mut [Option<V>] because it can break mapping.
impl<'a, K, V, S, const IMAP: bool> From<&'a IndexedMap<K, V, S, IMAP>> for &'a [Option<V>]
where
    S: BuildHasher,
{
    fn from(value: &'a IndexedMap<K, V, S, IMAP>) -> Self {
        value.values.as_slice()
    }
}
