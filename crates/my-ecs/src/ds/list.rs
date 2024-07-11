use super::vec::OptVec;
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

/// Set-like list based on [`OptVec`] with `HashMap` for pointing at nodes.
/// Theoretically, insert/remove is done in O(1), but iteration is not good as much as `Vec`.
/// Because iteration is not sequential, it needs jump to reach the next node.
/// But we can expect it would be more memory friendly than `LinkedList` based on `Box`.
/// Use [`VecDeque`](std::collections::VecDeque) if you need something like queue.
/// Or use [`LinkedList`](std::collections::LinkedList) if you really want linked list.
///
/// Current implementation doesn't concern about ZST.
#[derive(Debug)]
pub struct SetList<K, V, S> {
    nodes: OptVec<ListNode<V>, S>,
    tail: usize,
    map: HashMap<K, usize, S>,
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
        let head_index = nodes.add(ListNode {
            prev: 0,
            next: 0,
            value: dummy,
        });

        // See ListPos::end().
        debug_assert_eq!(0, head_index);

        Self {
            nodes,
            tail: head_index,
            map: HashMap::default(),
        }
    }
}

impl<K, V, S> SetList<K, V, S>
where
    S: BuildHasher,
{
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
    pub fn has_no_occupied(&self) -> bool {
        self.len_occupied() == 0
    }

    pub fn front(&self) -> Option<&V> {
        // Safety: first index is valid.
        unsafe { self.get_value(self.get_first_index()) }
    }

    pub fn front_mut(&mut self) -> Option<&mut V> {
        // Safety: first index is valid.
        unsafe { self.get_value_mut(self.get_first_index()) }
    }

    pub fn back(&self) -> Option<&V> {
        // Safety: tail index is valid.
        unsafe { self.get_value(self.tail) }
    }

    pub fn back_mut(&mut self) -> Option<&mut V> {
        // Safety: tail index is valid.
        unsafe { self.get_value_mut(self.tail) }
    }

    pub fn iter(&self) -> ListIter<V, S> {
        // Safety: get_first_position() returns valid default head node's position.
        unsafe { self.iter_from(self.get_first_position()) }
    }

    pub fn iter_mut(&mut self) -> ListIterMut<V, S> {
        // Safety: get_first_position() returns valid default head node's position.
        unsafe { self.iter_mut_from(self.get_first_position()) }
    }

    /// # Safety
    ///
    /// Undefined behavior if `cur` is invalid.
    pub unsafe fn iter_from(&self, cur: ListPos) -> ListIter<V, S> {
        ListIter {
            nodes: &self.nodes,
            cur: cur.0,
        }
    }

    /// # Safety
    ///
    /// Undefined behavior if `cur` is invalid.
    pub unsafe fn iter_mut_from(&mut self, cur: ListPos) -> ListIterMut<V, S> {
        ListIterMut {
            nodes: &mut self.nodes,
            cur: cur.0,
        }
    }

    /// # Safety
    ///
    /// Undefined behavior if `cur` is invalid.
    pub unsafe fn iter_pos_from(&self, cur: ListPos) -> ListPosIter<V, S> {
        ListPosIter {
            nodes: &self.nodes,
            cur: cur.0,
        }
    }

    /// # Safety
    ///
    /// Undefined behavior if `cur` is invalid.
    pub unsafe fn get_next_unchecked(&self, cur: ListPos) -> Option<(ListPos, &V)> {
        if !cur.is_end() {
            let cur_node = self.nodes.get_unchecked(cur.0);
            let next = ListPos(cur_node.next);
            Some((next, &cur_node.value))
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// Undefined behavior if `cur` is invalid.
    pub unsafe fn get_next_unchecked_mut(&mut self, cur: ListPos) -> Option<(ListPos, &mut V)> {
        if !cur.is_end() {
            let cur_node = self.nodes.get_unchecked_mut(cur.0);
            let next = ListPos(cur_node.next);
            Some((next, &mut cur_node.value))
        } else {
            None
        }
    }

    pub fn get_first_position(&self) -> ListPos {
        ListPos(self.get_first_index())
    }

    /// # Safety
    ///
    /// Undefined behavior if `index` is invalid.
    unsafe fn get_value(&self, index: usize) -> Option<&V> {
        if index != 0 {
            let node = self.nodes.get_unchecked(index);
            Some(&node.value)
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// Undefined behavior if `index` is invalid.
    unsafe fn get_value_mut(&mut self, index: usize) -> Option<&mut V> {
        if index != 0 {
            let node = self.nodes.get_unchecked_mut(index);
            Some(&mut node.value)
        } else {
            None
        }
    }

    fn get_first_index(&self) -> usize {
        // Safety: Default head node occupies 0-th slot.
        unsafe { self.nodes.get_unchecked(0).next }
    }

    fn get_default_head_node_mut(&mut self) -> &mut ListNode<V> {
        // Safety: Default head node occupies 0-th slot.
        unsafe { self.nodes.get_unchecked_mut(0) }
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
        self.map.get(key).map(|&index| ListPos(index))
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
        self.nodes.get(node.next).map(|node| &node.value)
    }

    pub fn get_next_mut<Q>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let node = self.get_node(key)?;
        self.nodes.get_mut(node.next).map(|node| &mut node.value)
    }

    /// Returns true if the value was successfully inserted.
    pub fn push_back(&mut self, key: K, value: V) -> bool {
        if self.contains_key(&key) {
            return false;
        }

        // Appends new tail node.
        let cur_index = self.nodes.add(ListNode {
            prev: self.tail,
            next: 0,
            value,
        });

        // Updates old tail node.
        // Safety: We control self.tail is always valid.
        let old_tail = unsafe { self.nodes.get_unchecked_mut(self.tail) };
        old_tail.next = cur_index;
        self.tail = cur_index;

        // Updates imap.
        self.map.insert(key, cur_index);

        true
    }

    /// Returns true if the value was successfully inserted.
    pub fn push_front(&mut self, key: K, value: V) -> bool {
        if self.contains_key(&key) {
            return false;
        }

        // Appends new first node (the next node of default head node).
        let first_index = self.get_first_index();
        let cur_index = self.nodes.add(ListNode {
            prev: 0,
            next: first_index,
            value,
        });

        // Updates old first node.
        if first_index != 0 {
            // Safety: first_index must be zero, which is default head node, or valid index.
            let old_first = unsafe { self.nodes.get_unchecked_mut(first_index) };
            old_first.prev = cur_index;
        } else {
            // Current node may be the first tail node.
            self.tail = cur_index;
        }
        self.get_default_head_node_mut().next = cur_index;

        // Updates imap.
        self.map.insert(key, cur_index);

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

        if let Some(&after_index) = self.map.get(after) {
            // Creates a new node.
            // Safety: Infallible.
            let next_index = unsafe { self.nodes.get_unchecked_mut(after_index).next };
            let cur_index = self.nodes.add(ListNode {
                prev: after_index,
                next: next_index,
                value,
            });

            // Updates links of `after` and `next` nodes.
            // Safety: Infallible.
            unsafe {
                self.nodes.get_unchecked_mut(after_index).next = cur_index;
                if next_index != 0 {
                    self.nodes.get_unchecked_mut(next_index).prev = cur_index;
                } else {
                    self.tail = cur_index;
                }
            }

            // Updates imap.
            self.map.insert(key, cur_index);

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
        let (key, index) = self.map.remove_entry(key)?;
        let old = unsafe { self.nodes.take(index).unwrap_unchecked() };
        let prev_index = old.prev;
        let next_index = old.next;
        unsafe {
            self.nodes.get_unchecked_mut(prev_index).next = next_index;
            if next_index != 0 {
                self.nodes.get_unchecked_mut(next_index).prev = prev_index;
            } else {
                self.tail = prev_index;
            }
        }
        Some((key, old.value))
    }

    fn get_node<Q>(&self, key: &Q) -> Option<&ListNode<V>>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let index = self.map.get(key)?;
        self.nodes.get(*index)
    }

    fn get_node_mut<Q>(&mut self, key: &Q) -> Option<&mut ListNode<V>>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let index = self.map.get(key)?;
        self.nodes.get_mut(*index)
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
        for value in self.iter() {
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
        for (i, value) in self.iter().enumerate() {
            if i == last {
                write!(f, "{value}")?;
            } else {
                write!(f, "{value}, ")?;
            }
        }
        write!(f, "]")
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
}

#[derive(Debug, Clone)]
struct ListNode<V> {
    prev: usize,
    next: usize,
    value: V,
}

#[derive(Debug, Clone)]
pub struct ListIter<'a, V, S> {
    nodes: &'a OptVec<ListNode<V>, S>,
    cur: usize,
}

impl<'a, V, S> Iterator for ListIter<'a, V, S>
where
    S: BuildHasher,
{
    type Item = &'a V;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cur != 0 {
            // Safety: self.cur is always valid.
            let node = unsafe { self.nodes.get_unchecked(self.cur) };
            self.cur = node.next;
            Some(&node.value)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct ListIterMut<'a, V, S> {
    nodes: &'a mut OptVec<ListNode<V>, S>,
    cur: usize,
}

impl<'a, V, S> Iterator for ListIterMut<'a, V, S>
where
    S: BuildHasher,
{
    type Item = &'a mut V;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cur != 0 {
            // Safety: self.cur is always valid.
            let node = unsafe { self.nodes.get_unchecked_mut(self.cur) };
            self.cur = node.next;
            let ptr = std::ptr::addr_of_mut!(node.value);
            // Safety: Infallible.
            unsafe { ptr.as_mut() }
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct ListPosIter<'a, V, S> {
    nodes: &'a OptVec<ListNode<V>, S>,
    cur: usize,
}

impl<'a, V, S> Iterator for ListPosIter<'a, V, S>
where
    S: BuildHasher,
{
    type Item = ListPos;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cur != 0 {
            // Safety: self.cur is always valid.
            let node = unsafe { self.nodes.get_unchecked(self.cur) };
            let res = ListPos(self.cur);
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
        let mut iter = list.iter();
        assert_eq!(Some(&0), iter.next());
        assert_eq!(Some(&1), iter.next());
        assert_eq!(Some(&2), iter.next());
        assert_eq!(None, iter.next());

        // as_vec
        assert_eq!(vec![0, 1, 2], list.values_as_vec());

        // take 1
        assert_eq!(Some(1), list.remove(&1));
        let mut iter = list.iter();
        assert_eq!(Some(&0), iter.next());
        assert_eq!(Some(&2), iter.next());
        assert_eq!(None, iter.next());
        assert_eq!(2, list.len_occupied());
        // take 2
        assert_eq!(Some(2), list.remove(&2));
        let mut iter = list.iter();
        assert_eq!(Some(&0), iter.next());
        assert_eq!(None, iter.next());
        assert_eq!(1, list.len_occupied());
        // take 0
        assert_eq!(Some(0), list.remove(&0));
        let mut iter = list.iter();
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
        for value in list.iter_mut() {
            *value *= 2;
        }
        for (src, dst) in src.iter().cloned().zip(list.iter().cloned()) {
            assert_eq!(src * 2, dst);
        }
        // iterator from
        let cur = list.get_first_position();
        let (next, v) = unsafe { list.get_next_unchecked_mut(cur).unwrap() };
        *v /= 2;
        for v in unsafe { list.iter_mut_from(next) } {
            *v /= 2;
        }
        for (src, dst) in src.iter().cloned().zip(list.iter().cloned()) {
            assert_eq!(src, dst);
        }
    }
}
