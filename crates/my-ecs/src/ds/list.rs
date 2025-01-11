use super::vec::opt_vec::OptVec;
use std::{
    collections::HashMap,
    fmt::Display,
    hash::{BuildHasher, Hash},
    ops::{Deref, DerefMut},
};

/// A shallow wrapper of [`SetList`] for using value as a key.
#[derive(Debug)]
#[repr(transparent)]
pub struct SetValueList<V, S>(SetList<V, V, S>);

impl<V, S> SetValueList<V, S>
where
    S: BuildHasher + Default,
{
    pub fn new(dummy: V) -> Self {
        Self(SetList::new(dummy))
    }
}

impl<V, S> SetValueList<V, S>
where
    V: Default,
    S: BuildHasher + Default,
{
    pub fn new_with_default() -> Self {
        Self(SetList::<V, V, S>::new_with_default())
    }
}

impl<V, S> SetValueList<V, S>
where
    V: Hash + Eq + Clone,
    S: BuildHasher,
{
    pub fn push_back(&mut self, value: V) -> bool {
        self.0.push_back(value.clone(), value)
    }

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
        let mut list = Self::new_with_default();
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

/// Set-like list based on [`OptVec`] with `HashMap` for pointing at nodes.
/// Theoretically, insert/remove is done in O(1), but iteration is not good as
/// much as `Vec`. Because iteration is not sequential, it needs jump to reach
/// the next node. But we can expect it would be more memory friendly than
/// `LinkedList` based on `Box`. Use [`VecDeque`] if you need something like
/// queue. Or use [`LinkedList`] if you really want linked list.
///
/// # NOTE
///
/// Current implementation doesn't concern about ZST.
///
/// [`VecDeque`]: std::collections::VecDeque
/// [`LinkedList`]: std::collections::LinkedList
#[derive(Debug)]
pub struct SetList<K, V, S> {
    nodes: OptVec<ListNode<V>, S>,
    tail: ListPos,
    map: HashMap<K, ListPos, S>,
}

impl<K, V, S> SetList<K, V, S>
where
    V: Default,
    S: BuildHasher + Default,
{
    /// Creates list with default head node.
    pub fn new_with_default() -> Self {
        Self::new(V::default())
    }
}

impl<K, V, S> SetList<K, V, S>
where
    S: BuildHasher + Default,
{
    /// Creates list with default head node using the `dummy` value.
    pub fn new(dummy: V) -> Self {
        let mut nodes = OptVec::new();
        let dummy_head_idx = nodes.add(ListNode {
            prev: ListPos::end(),
            next: ListPos::end(),
            value: dummy,
        });
        let tail = ListPos(dummy_head_idx);

        // Dummy node always occupies 0th slot in the OptVec.
        // We consider that 0 is END index of the list.
        // See [`ListPos::end`] together.
        debug_assert_eq!(ListPos::end(), tail);

        Self {
            nodes,
            tail,
            map: HashMap::default(),
        }
    }
}

impl<K, V, S> SetList<K, V, S> {
    /// Retrieves the length of node buffer,
    /// which is default head node + # of vacant slots + # of occupied slots.
    //
    // NOTE: Don't implement len(). It's confusing because we put in default node.
    pub fn len_buf(&self) -> usize {
        self.nodes.len()
    }

    /// Retrieves the number of nodes except default head node.
    pub fn len_occupied(&self) -> usize {
        self.nodes.len_occupied() - 1
    }

    /// Returns true if there's no node in the graph.
    /// Note that default head node is not taken into account.
    pub fn is_occupied_empty(&self) -> bool {
        self.len_occupied() == 0
    }

    pub fn front(&self) -> Option<&V> {
        let first_pos = self.get_first_position();

        // Safety: The first poisition must be one of
        // - Position pointing to actual value which is valid.
        // - `ListPos::end()` which is valid.
        unsafe { self.get_value_unchecked(first_pos) }
    }

    pub fn front_mut(&mut self) -> Option<&mut V> {
        let first_pos = self.get_first_position();

        // Safety: The first poisition must be one of
        // - Position pointing to actual value which is valid.
        // - `ListPos::end()` which is valid.
        unsafe { self.get_value_unchecked_mut(first_pos) }
    }

    pub fn back(&self) -> Option<&V> {
        // Safety: The tail poisition must be one of
        // - Position pointing to actual value which is valid.
        // - `ListPos::end()` which is valid.
        unsafe { self.get_value_unchecked(self.tail) }
    }

    pub fn back_mut(&mut self) -> Option<&mut V> {
        // Safety: The tail poisition must be one of
        // - Position pointing to actual value which is valid.
        // - `ListPos::end()` which is valid.
        unsafe { self.get_value_unchecked_mut(self.tail) }
    }

    pub fn values(&self) -> Values<'_, V, S> {
        // Safety: get_first_position() returns valid default head node's position.
        unsafe { self.values_from(self.get_first_position()) }
    }

    pub fn values_mut(&mut self) -> ValuesMut<'_, V, S> {
        // Safety: get_first_position() returns valid default head node's position.
        unsafe { self.values_mut_from(self.get_first_position()) }
    }

    pub fn into_values(self) -> IntoValues<V, S> {
        let pos = self.get_first_position();
        // Safety: get_first_position() returns valid default head node's position.
        unsafe { self.into_values_from(pos) }
    }

    /// # Safety
    ///
    /// Undefined behavior if `cur` is invalid.
    pub unsafe fn values_from(&self, cur: ListPos) -> Values<'_, V, S> {
        Values {
            nodes: &self.nodes,
            cur,
        }
    }

    /// # Safety
    ///
    /// Undefined behavior if `cur` is invalid.
    pub unsafe fn values_mut_from(&mut self, cur: ListPos) -> ValuesMut<'_, V, S> {
        ValuesMut {
            nodes: &mut self.nodes,
            cur,
        }
    }

    /// # Safety
    ///
    /// Undefined behavior if `cur` is invalid.
    pub unsafe fn into_values_from(self, cur: ListPos) -> IntoValues<V, S> {
        IntoValues {
            nodes: self.nodes,
            cur,
        }
    }

    /// # Safety
    ///
    /// Undefined behavior if `cur` is invalid.
    pub unsafe fn iter_pos_from(&self, cur: ListPos) -> PosIter<'_, V, S> {
        PosIter {
            nodes: &self.nodes,
            cur,
        }
    }

    /// Retrieves shared reference to the value at the given position
    /// and the next position of it.
    /// If the given position is unknown position like out of bounds,
    /// Returns None.
    pub fn iter_next(&self, cur: ListPos) -> Option<(ListPos, &V)> {
        if cur.is_end() {
            return None;
        }
        self.nodes
            .get(cur.into_inner())
            .map(|cur_node| (cur_node.next, &cur_node.value))
    }

    /// Retrieves mutable reference to the value at the given position
    /// and the next position of it.
    /// If the given position is unknown position like out of bounds,
    /// Returns None.
    pub fn iter_next_mut(&mut self, cur: ListPos) -> Option<(ListPos, &mut V)> {
        if cur.is_end() {
            return None;
        }
        self.nodes
            .get_mut(cur.into_inner())
            .map(|cur_node| (cur_node.next, &mut cur_node.value))
    }

    /// Returns first position.
    /// Note that the first position may be `ListPos::end()` if the list has no items in it.
    /// Otherwise, it must be a valid position.
    //
    // Acturally, list must have dummy head even if it's just created.
    pub fn get_first_position(&self) -> ListPos {
        // Safety: Dummy head occupies `ListPos::end()` so accessing it is safe.
        // See constructor for more details.
        let dummy_head_idx = ListPos::end().into_inner();
        unsafe { self.nodes.get_unchecked(dummy_head_idx).next }
    }

    /// Retrieves shared reference to the value at the given position
    /// or returns None if the position is `ListPos::end()`.
    ///
    /// # Safety
    ///
    /// Undefined behavior if the position is not valid.
    /// Valid position means position pointing to actual value or is `ListPos::end()`.
    unsafe fn get_value_unchecked(&self, pos: ListPos) -> Option<&V> {
        if !pos.is_end() {
            let node = self.nodes.get_unchecked(pos.into_inner());
            Some(&node.value)
        } else {
            None
        }
    }

    /// Retrieves mutable reference to the value at the given position
    /// or returns None if the position is `ListPos::end()`.
    ///
    /// # Safety
    ///
    /// Undefined behavior if the position is not valid.
    /// Valid position means position pointing to actual value or is `ListPos::end()`.
    unsafe fn get_value_unchecked_mut(&mut self, pos: ListPos) -> Option<&mut V> {
        if !pos.is_end() {
            let node = self.nodes.get_unchecked_mut(pos.into_inner());
            Some(&mut node.value)
        } else {
            None
        }
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
    pub fn contains_key<Q>(&self, key: &Q) -> bool
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.map.contains_key(key)
    }

    pub fn get_position<Q>(&self, key: &Q) -> Option<ListPos>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.map.get(key).copied()
    }

    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.get_node(key).map(|node| &node.value)
    }

    pub fn get_mut<Q>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.get_node_mut(key).map(|node| &mut node.value)
    }

    pub fn get_next<Q>(&self, key: &Q) -> Option<&V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let node = self.get_node(key)?;
        self.nodes
            .get(node.next.into_inner())
            .map(|node| &node.value)
    }

    pub fn get_next_mut<Q>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let node = self.get_node(key)?;
        self.nodes
            .get_mut(node.next.into_inner())
            .map(|node| &mut node.value)
    }

    /// Returns true if the value was successfully inserted.
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

    /// Returns true if the value was successfully inserted.
    pub fn push_front(&mut self, key: K, value: V) -> bool {
        if self.contains_key(&key) {
            return false;
        }

        // Appends new first node (the next node of default head node).
        let first_pos = self.get_first_position();
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

    /// Inserts the value after the specific node `after`.
    /// If you want to insert value to the front, then use [`Self::push_front`].
    /// Returns true if the value was successfully inserted.
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

    /// Moves the node after the specific node `after`.
    /// If you want to move the node to the front, use [`Self::move_node_to_front`].
    /// Returns true if the node was successfully moved.
    pub fn move_node<Q>(&mut self, key: &Q, after: &Q) -> bool
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        if let Some((key, value)) = self.remove_entry(key) {
            self.insert(key, value, after)
        } else {
            false
        }
    }

    /// Moves the node to the first position.
    /// Returns true if the node was successfully moved.
    pub fn move_node_to_front<Q>(&mut self, key: &Q) -> bool
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        if let Some((key, value)) = self.remove_entry(key) {
            self.push_front(key, value);
            true
        } else {
            false
        }
    }

    /// Removes and returns value if it was present.
    pub fn remove<Q>(&mut self, key: &Q) -> Option<V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.remove_entry(key).map(|(_, value)| value)
    }

    /// Removes and returns value if it was present.
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
        let last = self.len_occupied() - 1;
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
        let mut list = Self::new_with_default();
        for (k, v) in value.iter() {
            list.push_back(k.clone(), v.clone());
        }
        list
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ListPos(usize);

impl ListPos {
    pub const fn end() -> Self {
        Self(0)
    }

    pub const fn is_end(&self) -> bool {
        self.0 == 0
    }

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

#[derive(Debug, Clone)]
pub struct PosIter<'a, V, S> {
    nodes: &'a OptVec<ListNode<V>, S>,
    cur: ListPos,
}

impl<V, S> Iterator for PosIter<'_, V, S> {
    type Item = ListPos;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.cur.is_end() {
            // Safety: self.cur is always valid.
            let node = unsafe { self.nodes.get_unchecked(self.cur.into_inner()) };
            let res = self.cur;
            self.cur = node.next;
            Some(res)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::hash::RandomState;

    #[test]
    fn test_setlist() {
        // push
        let mut list = SetValueList::<_, RandomState>::new_with_default();
        list.push_front(1);
        list.push_back(2);
        list.push_front(0);
        assert_eq!(3, list.len_occupied());
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
        assert_eq!(2, list.len_occupied());
        // take 2
        assert_eq!(Some(2), list.remove(&2));
        let mut iter = list.values();
        assert_eq!(Some(&0), iter.next());
        assert_eq!(None, iter.next());
        assert_eq!(1, list.len_occupied());
        // take 0
        assert_eq!(Some(0), list.remove(&0));
        let mut iter = list.values();
        assert_eq!(None, iter.next());
        assert_eq!(0, list.len_occupied());

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
        let cur = list.get_first_position();
        let (next, v) = list.iter_next_mut(cur).unwrap();
        *v /= 2;
        for v in unsafe { list.values_mut_from(next) } {
            *v /= 2;
        }
        for (src, dst) in src.iter().cloned().zip(list.values().cloned()) {
            assert_eq!(src, dst);
        }
    }

    #[test]
    fn test_setlist_into_iter() {
        let mut list = SetValueList::<_, RandomState>::new_with_default();
        for i in 0..10 {
            list.push_back(i);
        }

        let values = list.into_iter();
        for (i, value) in values.enumerate() {
            assert_eq!(value, i as i32);
        }
    }
}
