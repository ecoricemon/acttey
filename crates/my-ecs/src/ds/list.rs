use super::OptVec;
use std::{
    collections::HashMap,
    fmt::Display,
    hash::{BuildHasher, Hash},
    iter,
    ops::{Deref, DerefMut},
};

/// A list containing unique values.
///
/// This list is based on [`SetList`]. See documentation of it for more
/// information.
#[derive(Debug)]
#[repr(transparent)]
pub struct SetValueList<V, S>(SetList<V, V, S>);

impl<V, S> SetValueList<V, S>
where
    S: BuildHasher + Default,
{
    /// Creates a new empty list.
    ///
    /// [`SetValueList`] requires dummy head node for now, so you can put in any
    /// values.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetValueList;
    /// use std::hash::RandomState;
    ///
    /// let list = SetValueList::<&'static str, RandomState>::new("");
    /// ```
    pub fn new(dummy: V) -> Self {
        Self(SetList::new(dummy))
    }
}

impl<V, S> Default for SetValueList<V, S>
where
    V: Default,
    S: BuildHasher + Default,
{
    /// Creates a new empty list.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetValueList;
    /// use std::hash::RandomState;
    ///
    /// let list = SetValueList::<&'static str, RandomState>::default();
    /// ```
    fn default() -> Self {
        Self(SetList::<V, V, S>::default())
    }
}

impl<V, S> SetValueList<V, S>
where
    V: Hash + Eq + Clone,
    S: BuildHasher,
{
    /// Appends the given value to the end of the list.
    ///
    /// However, if the list already contains the value, nothing takes place and
    /// returns false.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetValueList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetValueList::<_, RandomState>::default();
    /// list.push_back("alpha");
    /// assert!(!list.push_back("alpha"));
    /// ```
    pub fn push_back(&mut self, value: V) -> bool {
        self.0.push_back(value.clone(), value)
    }

    /// Appends the given value to the beginning of the list.
    ///
    /// However, if the list already contains the value, nothing takes place and
    /// returns false.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetValueList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetValueList::<_, RandomState>::default();
    /// list.push_front("alpha");
    /// assert!(!list.push_front("alpha"));
    /// ```
    pub fn push_front(&mut self, value: V) -> bool {
        self.0.push_front(value.clone(), value)
    }
}

impl<V, S> Deref for SetValueList<V, S> {
    type Target = SetList<V, V, S>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<V, S> DerefMut for SetValueList<V, S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<V, S> Clone for SetValueList<V, S>
where
    V: Clone,
    S: Clone,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<V, S> Display for SetValueList<V, S>
where
    V: Display,
    S: BuildHasher,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<V, S> IntoIterator for SetValueList<V, S>
where
    S: BuildHasher,
{
    type Item = V;
    type IntoIter = IntoValues<V, S>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_values()
    }
}

impl<V, S> From<&[V]> for SetValueList<V, S>
where
    V: Hash + Eq + Clone + Default,
    S: BuildHasher + Default,
{
    fn from(value: &[V]) -> Self {
        let mut list = Self::default();
        for v in value.iter() {
            list.push_back(v.clone());
        }
        list
    }
}

impl<V, S> From<SetValueList<V, S>> for Vec<V>
where
    S: BuildHasher,
{
    fn from(value: SetValueList<V, S>) -> Self {
        value.0.nodes.into_iter().map(|node| node.value).collect()
    }
}

/// A list containg unique key-value pairs.
///
/// This is a list, but all items are laid on a single sequential memory block.
/// Therefore, we can expect more speed in terms of iteration than standard
/// linked list, [`LinkedList`](std::collections::LinkedList), but it requires
/// more memory footprint.
///
/// # NOTE
///
/// Current implementation doesn't concern about ZST.
#[derive(Debug)]
pub struct SetList<K, V, S> {
    nodes: OptVec<ListNode<V>, S>,
    tail: ListPos,
    map: HashMap<K, ListPos, S>,
}

impl<K, V, S> Default for SetList<K, V, S>
where
    V: Default,
    S: BuildHasher + Default,
{
    /// Creates a new empty list.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let list = SetList::<char, String, RandomState>::default();
    /// ```
    fn default() -> Self {
        Self::new(V::default())
    }
}

impl<K, V, S> SetList<K, V, S>
where
    S: BuildHasher + Default,
{
    /// Creates a new empty list.
    ///
    /// [`SetList`] requires dummy head node for now, so you can put in any
    /// values.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let list = SetList::<char, &'static str, RandomState>::new("");
    /// ```
    pub fn new(dummy: V) -> Self {
        let mut nodes = OptVec::new();
        let dummy_head_idx = nodes.add(ListNode {
            prev: ListPos::end(),
            next: ListPos::end(),
            value: dummy,
        });
        let tail = ListPos(dummy_head_idx);

        // Dummy node always occupies 0th slot in the OptVec. We consider that 0
        // is END index of the list. See `ListPos::end` together.
        debug_assert_eq!(ListPos::end(), tail);

        Self {
            nodes,
            tail,
            map: HashMap::default(),
        }
    }
}

impl<K, V, S> SetList<K, V, S> {
    /// Returns number of items.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_back('a', "alpha");
    /// assert_eq!(list.len(), 1);
    /// ```
    pub fn len(&self) -> usize {
        self.nodes.len() - 1 /* dummy head node */
    }

    /// Returns true if the list is empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<char, &'static str, RandomState>::default();
    /// assert!(list.is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns a shared reference to the front item.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_back('a', "alpha");
    /// list.push_back('b', "beta");
    /// assert_eq!(list.front(), Some(&"alpha"));
    /// ```
    pub fn front(&self) -> Option<&V> {
        // Returns `None` for the dummy head node.
        let first_pos = self.first_position();
        if first_pos.is_end() {
            return None;
        }

        // Safety: Position must be valid one: Ok.
        unsafe { Some(self.get_value_unchecked(first_pos)) }
    }

    /// Returns a mutable reference to the front item.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_back('a', "alpha");
    /// list.push_back('b', "beta");
    /// assert_eq!(list.front_mut(), Some(&mut "alpha"));
    /// ```
    pub fn front_mut(&mut self) -> Option<&mut V> {
        // Returns `None` for the dummy head node.
        let first_pos = self.first_position();
        if first_pos.is_end() {
            return None;
        }

        // Safety: Position must be valid one: Ok.
        unsafe { Some(self.get_value_unchecked_mut(first_pos)) }
    }

    /// Returns a shared reference to the last item.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_back('a', "alpha");
    /// list.push_back('b', "beta");
    /// assert_eq!(list.back(), Some(&"beta"));
    /// ```
    pub fn back(&self) -> Option<&V> {
        // Returns `None` for the dummy head node.
        if self.tail.is_end() {
            return None;
        }

        // Safety: Position must be valid one: Ok.
        unsafe { Some(self.get_value_unchecked(self.tail)) }
    }

    /// Returns a mutable reference to the last item.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_back('a', "alpha");
    /// list.push_back('b', "beta");
    /// assert_eq!(list.back_mut(), Some(&mut "beta"));
    /// ```
    pub fn back_mut(&mut self) -> Option<&mut V> {
        // Returns `None` for the dummy head node.
        if self.tail.is_end() {
            return None;
        }

        // Safety: Position must be valid one: Ok.
        unsafe { Some(self.get_value_unchecked_mut(self.tail)) }
    }

    /// Returns an iterator visiting all values in order.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_back('a', "alpha");
    /// list.push_back('b', "beta");
    ///
    /// for v in list.values() {
    ///     println!("{v}"); // Prints out "alpha" and "beta".
    /// }
    /// ```
    pub fn values(&self) -> Values<'_, V, S> {
        self.values_from(self.first_position())
    }

    /// Returns a mutable iterator visiting all values in order.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_back('a', "alpha".to_owned());
    /// list.push_back('b', "beta".to_owned());
    ///
    /// for v in list.values_mut() {
    ///     v.push('*');
    ///     println!("{v}"); // Prints out "alpha*" and "beta*".
    /// }
    /// ```
    pub fn values_mut(&mut self) -> ValuesMut<'_, V, S> {
        self.values_mut_from(self.first_position())
    }

    /// Returns an iterator visiting values from the given position.
    ///
    /// # Panics
    ///
    /// Panics if the position is not valid one for the list.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_back('a', "alpha");
    /// list.push_back('b', "beta");
    /// list.push_back('g', "gamma");
    ///
    /// let pos = list.get_position(&'b').unwrap();
    /// for v in list.values_from(pos) {
    ///     println!("{v}"); // Prints out "beta" and "gamma".
    /// }
    /// ```
    pub fn values_from(&self, cur: ListPos) -> Values<'_, V, S> {
        assert!(
            self.nodes.get(cur.into_inner()).is_some(),
            "{cur:?} is not a valid position"
        );

        // `ListPos::end()` passes validation above due to the dummy head node.
        // But it's Ok because `Values` will return `None` when it meets
        // `ListPos::end()`.
        Values {
            nodes: &self.nodes,
            cur,
        }
    }

    /// Returns a mutable iterator visiting values from the given position.
    ///
    /// # Panics
    ///
    /// Panics if the position is not valid one for the list.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_back('a', "alpha".to_owned());
    /// list.push_back('b', "beta".to_owned());
    /// list.push_back('g', "gamma".to_owned());
    ///
    /// let pos = list.get_position(&'b').unwrap();
    /// for v in list.values_mut_from(pos) {
    ///     v.push('*');
    ///     println!("{v}"); // Prints out "beta*" and "gamma*".
    /// }
    /// ```
    pub fn values_mut_from(&mut self, cur: ListPos) -> ValuesMut<'_, V, S> {
        assert!(
            self.nodes.get(cur.into_inner()).is_some(),
            "{cur:?} is not a valid position"
        );

        // `ListPos::end()` passes validation above due to the dummy head node.
        // But it's Ok because `ValuesMut` will return `None` when it meets
        // `ListPos::end()`.
        ValuesMut {
            nodes: &mut self.nodes,
            cur,
        }
    }

    /// Creates an iterator visiting all values in order by consuming the list.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_back('a', "alpha");
    /// list.push_back('b', "beta");
    ///
    /// for v in list.into_values() {
    ///     println!("{v}");
    /// }
    /// ```
    pub fn into_values(self) -> IntoValues<V, S> {
        let pos = self.first_position();
        self.into_values_from(pos)
    }

    /// Creates an iterator visiting values from the given position by consuming
    /// the list.
    ///
    /// # Panics
    ///
    /// Panics if the position is not valid one for the list.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_back('a', "alpha");
    /// list.push_back('b', "beta");
    /// list.push_back('g', "gamma");
    ///
    /// let pos = list.get_position(&'b').unwrap();
    /// for v in list.into_values_from(pos) {
    ///     println!("{v}"); // Prints out "beta" and "gamma".
    /// }
    /// ```
    pub fn into_values_from(self, cur: ListPos) -> IntoValues<V, S> {
        assert!(
            self.nodes.get(cur.into_inner()).is_some(),
            "{cur:?} is not a valid position"
        );

        // `ListPos::end()` passes validation above due to the dummy head node.
        // But it's Ok because `IntoValues` will return `None` when it meets
        // `ListPos::end()`.
        IntoValues {
            nodes: self.nodes,
            cur,
        }
    }

    /// Returns a shared reference to a value at the given position with the
    /// next position.
    ///
    /// If the given position is not valid one for the list, returns None.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_back('a', "alpha");
    /// list.push_back('b', "beta");
    ///
    /// let pos = list.get_position(&'b').unwrap();
    /// let (_, v) = list.iter_next(pos).unwrap();
    /// assert_eq!(v, &"beta");
    /// ```
    pub fn iter_next(&self, cur: ListPos) -> Option<(ListPos, &V)> {
        // `ListPos::end()` must be ignored due to the dummy head node.
        if cur.is_end() {
            return None;
        }

        self.nodes
            .get(cur.into_inner())
            .map(|cur_node| (cur_node.next, &cur_node.value))
    }

    /// Returns a mutable reference to a value at the given position with the
    /// next position.
    ///
    /// If the given position is not valid one for the list, returns None.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_back('a', "alpha");
    /// list.push_back('b', "beta");
    ///
    /// let pos = list.get_position(&'b').unwrap();
    /// let (_, v) = list.iter_next_mut(pos).unwrap();
    /// assert_eq!(v, &mut "beta");
    /// ```
    pub fn iter_next_mut(&mut self, cur: ListPos) -> Option<(ListPos, &mut V)> {
        // `ListPos::end()` must be ignored due to the dummy head node.
        if cur.is_end() {
            return None;
        }

        self.nodes
            .get_mut(cur.into_inner())
            .map(|cur_node| (cur_node.next, &mut cur_node.value))
    }

    /// Returns the first position of the list.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_back('a', "alpha");
    /// list.push_back('b', "beta");
    ///
    /// let pos = list.first_position();
    /// let (_, v) = list.iter_next(pos).unwrap();
    /// assert_eq!(v, &"alpha");
    /// ```
    pub fn first_position(&self) -> ListPos {
        // Safety: Dummy head occupies `ListPos::end()` so accessing it is safe.
        // See constructor for more details.
        let dummy_head_idx = ListPos::end().into_inner();
        unsafe { self.nodes.get_unchecked(dummy_head_idx).next }
    }

    /// Returns a shared reference to a value at the given position.
    ///
    /// If the position is [`ListPos::end`], then the method returns value of
    /// the dummy head node.
    ///
    /// # Safety
    ///
    /// Undefined behavior if the position is neither a valid one for the list
    /// nor the `ListPos::end`.
    unsafe fn get_value_unchecked(&self, pos: ListPos) -> &V {
        let node = unsafe { self.nodes.get_unchecked(pos.into_inner()) };
        &node.value
    }

    /// Returns a mutable reference to a value at the given position.
    ///
    /// If the position is [`ListPos::end`], then the method returns value of
    /// the dummy head node.
    ///
    /// # Safety
    ///
    /// Undefined behavior if the position is neither a valid one for the list
    /// nor the `ListPos::end`.
    unsafe fn get_value_unchecked_mut(&mut self, pos: ListPos) -> &mut V {
        let node = unsafe { self.nodes.get_unchecked_mut(pos.into_inner()) };
        &mut node.value
    }

    fn get_default_head_node_mut(&mut self) -> &mut ListNode<V> {
        // Safety: Dummy head occupies `ListPos::end()` so accessing it is safe.
        // See constructor for more details.
        let dummy_head_idx = ListPos::end().into_inner();
        unsafe { self.nodes.get_unchecked_mut(dummy_head_idx) }
    }
}

impl<K, V, S> SetList<K, V, S>
where
    K: Hash + Eq,
    S: BuildHasher,
{
    /// Returns true if the vector contains the given key.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_back('a', "alpha");
    /// assert!(list.contains_key(&'a'));
    /// ```
    pub fn contains_key<Q>(&self, key: &Q) -> bool
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.map.contains_key(key)
    }

    /// Retrieves a position of a value corresponding to the given key.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_back('a', "alpha");
    /// let pos = list.get_position(&'a').unwrap();
    /// ```
    pub fn get_position<Q>(&self, key: &Q) -> Option<ListPos>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.map.get(key).copied()
    }

    /// Retrieves a shared reference to a value corresponding to the given key.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_back('a', "alpha");
    /// assert_eq!(list.get(&'a'), Some(&"alpha"));
    /// ```
    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.get_node(key).map(|node| &node.value)
    }

    /// Retrieves a mutable reference to a value corresponding to the given key.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_back('a', "alpha");
    /// assert_eq!(list.get_mut(&'a'), Some(&mut "alpha"));
    /// ```
    pub fn get_mut<Q>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.get_node_mut(key).map(|node| &mut node.value)
    }

    /// Appends the given key-value pair to the end of the list.
    ///
    /// However, if the list already contains the key, nothing takes place and
    /// returns false.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_back('a', "alpha");
    /// assert!(!list.push_back('a', "beta"));
    /// assert_eq!(list.back(), Some(&"alpha"));
    /// ```
    pub fn push_back(&mut self, key: K, value: V) -> bool {
        if self.contains_key(&key) {
            return false;
        }

        // Appends new tail node.
        let cur_index = self.nodes.add(ListNode {
            prev: self.tail,
            next: ListPos::end(),
            value,
        });
        let cur_pos = ListPos(cur_index);

        // Updates old tail node.
        // Safety: We control self.tail is always valid.
        let old_tail = unsafe { self.nodes.get_unchecked_mut(self.tail.into_inner()) };
        old_tail.next = cur_pos;
        self.tail = cur_pos;

        // Updates imap.
        self.map.insert(key, cur_pos);

        true
    }

    /// Inserts the given key-value pair to the beginning of the list.
    ///
    /// However, if the list already contains the key, nothing takes place and
    /// returns false.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_front('a', "alpha");
    /// assert!(!list.push_front('a', "beta"));
    /// assert_eq!(list.front(), Some(&"alpha"));
    /// ```
    pub fn push_front(&mut self, key: K, value: V) -> bool {
        if self.contains_key(&key) {
            return false;
        }

        // Appends new first node (the next node of default head node).
        let first_pos = self.first_position();
        let cur_index = self.nodes.add(ListNode {
            prev: ListPos::end(),
            next: first_pos,
            value,
        });
        let cur_pos = ListPos(cur_index);

        // Updates old first node.
        if !first_pos.is_end() {
            // Safety: first_index must be zero, which is default head node, or valid index.
            let old_first = unsafe { self.nodes.get_unchecked_mut(first_pos.into_inner()) };
            old_first.prev = cur_pos;
        } else {
            // Current node may be the first tail node.
            self.tail = cur_pos;
        }
        self.get_default_head_node_mut().next = cur_pos;

        // Updates imap.
        self.map.insert(key, cur_pos);

        true
    }

    /// Inserts the given `key`-`value` pair after a node corresponding to
    /// `after`.
    ///
    /// However, if the list already contains the `key` or doesn't contain
    /// `after`, nothing takes place and returns false.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_back('a', "alpha");
    /// list.push_back('g', "gamma");
    /// list.insert('b', "beta", &'a');
    /// ```
    pub fn insert<Q>(&mut self, key: K, value: V, after: &Q) -> bool
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        if self.contains_key(key.borrow()) {
            return false;
        }

        if let Some(&after_pos) = self.map.get(after) {
            let after_idx = after_pos.into_inner();

            // Creates a new node.
            // Safety: Infallible.
            let next_pos = unsafe { self.nodes.get_unchecked_mut(after_idx).next };
            let cur_index = self.nodes.add(ListNode {
                prev: after_pos,
                next: next_pos,
                value,
            });
            let cur_pos = ListPos(cur_index);

            // Updates links of `after` and `next` nodes.
            // Safety: Infallible.
            unsafe {
                self.nodes.get_unchecked_mut(after_idx).next = cur_pos;
                if !next_pos.is_end() {
                    self.nodes.get_unchecked_mut(next_pos.into_inner()).prev = cur_pos;
                } else {
                    self.tail = cur_pos;
                }
            }

            // Updates imap.
            self.map.insert(key, cur_pos);

            true
        } else {
            false
        }
    }

    /// Removes an item corresponding to the given key.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_back('a', "alpha");
    /// assert_eq!(list.remove(&'a'), Some("alpha"));
    /// ```
    pub fn remove<Q>(&mut self, key: &Q) -> Option<V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.remove_entry(key).map(|(_, value)| value)
    }

    /// Removes an item corresponding to the given key then resturns key-value
    /// pair.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SetList;
    /// use std::hash::RandomState;
    ///
    /// let mut list = SetList::<_, _, RandomState>::default();
    /// list.push_back('a', "alpha");
    /// assert_eq!(list.remove_entry(&'a'), Some(('a', "alpha")));
    /// ```
    pub fn remove_entry<Q>(&mut self, key: &Q) -> Option<(K, V)>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let (key, pos) = self.map.remove_entry(key)?;
        let old = unsafe { self.nodes.take(pos.into_inner()).unwrap_unchecked() };
        let prev_pos = old.prev;
        let next_pos = old.next;
        unsafe {
            self.nodes.get_unchecked_mut(prev_pos.into_inner()).next = next_pos;
            if !next_pos.is_end() {
                self.nodes.get_unchecked_mut(next_pos.into_inner()).prev = prev_pos;
            } else {
                self.tail = prev_pos;
            }
        }
        Some((key, old.value))
    }

    fn get_node<Q>(&self, key: &Q) -> Option<&ListNode<V>>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let pos = self.map.get(key)?;
        self.nodes.get(pos.into_inner())
    }

    fn get_node_mut<Q>(&mut self, key: &Q) -> Option<&mut ListNode<V>>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let pos = self.map.get(key)?;
        self.nodes.get_mut(pos.into_inner())
    }
}

impl<K, V, S> SetList<K, V, S>
where
    V: Clone,
    S: BuildHasher,
{
    /// Creates `Vec` from this list.
    pub fn values_as_vec(&self) -> Vec<V> {
        let mut v = Vec::new();
        for value in self.values() {
            v.push(value.clone());
        }
        v
    }
}

impl<K, V, S> Clone for SetList<K, V, S>
where
    K: Clone,
    V: Clone,
    S: Clone,
{
    fn clone(&self) -> Self {
        Self {
            nodes: self.nodes.clone(),
            tail: self.tail,
            map: self.map.clone(),
        }
    }
}

impl<K, V, S> Display for SetList<K, V, S>
where
    V: Display,
    S: BuildHasher,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        let last = self.len() - 1;
        for (i, value) in self.values().enumerate() {
            if i == last {
                write!(f, "{value}")?;
            } else {
                write!(f, "{value}, ")?;
            }
        }
        write!(f, "]")
    }
}

impl<K, V, S> IntoIterator for SetList<K, V, S>
where
    S: BuildHasher,
{
    type Item = (K, V);
    type IntoIter = IntoIter<K, V, S>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter::new(self)
    }
}

impl<K, V, S> From<&[(K, V)]> for SetList<K, V, S>
where
    K: Hash + Eq + Clone,
    V: Default + Clone,
    S: BuildHasher + Default,
{
    fn from(value: &[(K, V)]) -> Self {
        let mut list = Self::default();
        for (k, v) in value.iter() {
            list.push_back(k.clone(), v.clone());
        }
        list
    }
}

/// A position to an item of [`SetList`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ListPos(usize);

impl ListPos {
    /// Creates a [`ListPos`] meaning end of list.
    pub const fn end() -> Self {
        Self(0)
    }

    /// Returns true if the position is end.
    pub const fn is_end(&self) -> bool {
        self.0 == 0
    }

    /// Returns inner index by consuming `self`.
    pub const fn into_inner(self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone)]
struct ListNode<V> {
    prev: ListPos,
    next: ListPos,
    value: V,
}

/// An iterator yielding shared references to values of [`SetList`].
///
/// You can create the iterator by calling [`SetList::values`]. See the
/// documentation of the method for more information.
#[derive(Debug, Clone)]
pub struct Values<'a, V, S> {
    nodes: &'a OptVec<ListNode<V>, S>,
    cur: ListPos,
}

impl<'a, V, S> Iterator for Values<'a, V, S> {
    type Item = &'a V;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.cur.is_end() {
            // Safety: self.cur is always valid.
            let node = unsafe { self.nodes.get_unchecked(self.cur.into_inner()) };
            self.cur = node.next;
            Some(&node.value)
        } else {
            None
        }
    }
}

impl<V, S> iter::FusedIterator for Values<'_, V, S> {}

/// An iterator yielding mutable references to values of [`SetList`].
///
/// You can create the iterator by calling [`SetList::values_mut`]. See the
/// documentation of the method for more information.
#[derive(Debug)]
pub struct ValuesMut<'a, V, S> {
    nodes: &'a mut OptVec<ListNode<V>, S>,
    cur: ListPos,
}

impl<'a, V, S> Iterator for ValuesMut<'a, V, S> {
    type Item = &'a mut V;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.cur.is_end() {
            // Safety: self.cur is always valid.
            let node = unsafe { self.nodes.get_unchecked_mut(self.cur.into_inner()) };
            self.cur = node.next;
            let ptr = std::ptr::addr_of_mut!(node.value);
            // Safety: Infallible.
            unsafe { ptr.as_mut() }
        } else {
            None
        }
    }
}

impl<V, S> iter::FusedIterator for ValuesMut<'_, V, S> {}

/// An owning iteartor over values of [`SetList`].
///
/// You can create the iterator by calling [`SetList::into_values`]. See the
/// documentation of the method for more information.
#[derive(Debug)]
pub struct IntoValues<V, S> {
    nodes: OptVec<ListNode<V>, S>,
    cur: ListPos,
}

impl<V, S> Iterator for IntoValues<V, S>
where
    S: BuildHasher,
{
    type Item = V;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.cur.is_end() {
            // Safety: self.cur is always valid.
            let node = unsafe { self.nodes.take(self.cur.into_inner()).unwrap_unchecked() };
            self.cur = node.next;
            Some(node.value)
        } else {
            None
        }
    }
}

impl<V, S: BuildHasher> iter::FusedIterator for IntoValues<V, S> {}

/// An owning iterator over key-value pairs of [`SetList`].
///
/// You can create the iterator by calling [`SetList::into_iter`]. See the
/// documentation of the method for more information.
#[derive(Debug)]
pub struct IntoIter<K, V, S> {
    nodes: OptVec<ListNode<V>, S>,
    map_iter: std::collections::hash_map::IntoIter<K, ListPos>,
}

impl<K, V, S> IntoIter<K, V, S> {
    fn new(list: SetList<K, V, S>) -> Self {
        Self {
            nodes: list.nodes,
            map_iter: list.map.into_iter(),
        }
    }
}

impl<K, V, S> Iterator for IntoIter<K, V, S>
where
    S: BuildHasher,
{
    type Item = (K, V);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((k, pos)) = self.map_iter.next() {
            // Safety: We got the index from the map, so the slot must be
            // occupied.
            let node = unsafe { self.nodes.take(pos.into_inner()).unwrap_unchecked() };
            Some((k, node.value))
        } else {
            None
        }
    }
}

// We can implement ExactSizeIterator because hash_map::IntoIter implements it.
impl<K, V, S: BuildHasher> ExactSizeIterator for IntoIter<K, V, S> {
    fn len(&self) -> usize {
        self.map_iter.len()
    }
}

// We can implement FusedIterator because hash_map::IntoIter implements it.
impl<K, V, S: BuildHasher> iter::FusedIterator for IntoIter<K, V, S> {}

#[cfg(test)]
mod tests {
    use super::*;
    use std::hash::RandomState;

    #[test]
    fn test_setlist() {
        // push
        let mut list = SetValueList::<_, RandomState>::default();
        list.push_front(1);
        list.push_back(2);
        list.push_front(0);
        assert_eq!(3, list.len());
        let mut iter = list.values();
        assert_eq!(Some(&0), iter.next());
        assert_eq!(Some(&1), iter.next());
        assert_eq!(Some(&2), iter.next());
        assert_eq!(None, iter.next());

        // as_vec
        assert_eq!(vec![0, 1, 2], list.values_as_vec());

        // take 1
        assert_eq!(Some(1), list.remove(&1));
        let mut iter = list.values();
        assert_eq!(Some(&0), iter.next());
        assert_eq!(Some(&2), iter.next());
        assert_eq!(None, iter.next());
        assert_eq!(2, list.len());
        // take 2
        assert_eq!(Some(2), list.remove(&2));
        let mut iter = list.values();
        assert_eq!(Some(&0), iter.next());
        assert_eq!(None, iter.next());
        assert_eq!(1, list.len());
        // take 0
        assert_eq!(Some(0), list.remove(&0));
        let mut iter = list.values();
        assert_eq!(None, iter.next());
        assert_eq!(0, list.len());

        // from<&[T]>
        let slice = &['a', 'b', 'c'][..];
        assert_eq!(
            Vec::from(slice),
            SetValueList::<_, RandomState>::from(slice).values_as_vec()
        );

        // mutable iterator
        let src = [0, 1, 2];
        let mut list = SetValueList::<_, RandomState>::from(&src[..]);
        for value in list.values_mut() {
            *value *= 2;
        }
        for (src, dst) in src.iter().cloned().zip(list.values().cloned()) {
            assert_eq!(src * 2, dst);
        }
        // iterator from
        let cur = list.first_position();
        let (next, v) = list.iter_next_mut(cur).unwrap();
        *v /= 2;
        for v in list.values_mut_from(next) {
            *v /= 2;
        }
        for (src, dst) in src.iter().cloned().zip(list.values().cloned()) {
            assert_eq!(src, dst);
        }
    }

    #[test]
    fn test_setlist_into_iter() {
        let mut list = SetValueList::<_, RandomState>::default();
        for i in 0..10 {
            list.push_back(i);
        }

        let values = list.into_iter();
        for (i, value) in values.enumerate() {
            assert_eq!(value, i as i32);
        }
    }
}
