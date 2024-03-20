use crate::ds::vec::OptVec;
use ahash::AHashMap;
use std::{borrow::Borrow, hash::Hash, rc::Rc};

const GEN_DUMMY: u64 = u64::MAX;
const GEN_IGNORE: u64 = u64::MAX - 1;

/// A generational hash map.
/// You are free to get or mutate values without generation matching.
/// However, generation increases whenever modificaions occur.
/// Therefore, you can notice that the value has been changed.
#[derive(Debug)]
pub struct GenMap<K, V> {
    items: AHashMap<K, (V, u64)>,
}

impl<K: Eq + Hash + Clone, V> GenMap<K, V> {
    pub fn new() -> Self {
        Self {
            items: AHashMap::new(),
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.items.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.items.capacity()
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.items.iter().map(|(k, (v, _))| (k, v))
    }

    #[inline]
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.items.values().map(|(v, _)| v)
    }

    #[inline]
    pub fn get<Q>(&self, key: &Q) -> Option<&(V, u64)>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.items.get(key)
    }

    #[inline]
    pub fn get_value<Q>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.get(key).map(|(v, _)| v)
    }

    #[inline]
    pub fn sneak_get_value_mut<Q>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.items.get_mut(key).map(|(v, _)| v)
    }

    #[inline]
    pub fn get_gen<Q>(&self, key: &Q) -> Option<u64>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.get(key).map(|(_, gen)| *gen)
    }

    #[inline]
    pub fn contains_key<Q>(&self, key: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.items.contains_key(key)
    }

    /// Updates the value using the given function.
    /// Returns new generation of the value if update succeeded.
    /// Otherwise, returns None.
    pub fn update<Q, U>(&mut self, key: &Q, f: impl FnOnce(&mut V) -> U) -> Option<u64>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let (value, gen) = self.items.get_mut(key)?;
        f(value);
        *gen += 1;
        Some(*gen)
    }

    pub fn sneak_update<Q>(&mut self, key: &Q, f: impl FnOnce(&mut V)) -> bool
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        if let Some((value, _)) = self.items.get_mut(key) {
            f(value);
            true
        } else {
            false
        }
    }

    /// Inserts or updates the value.
    /// Returns old value if it was.
    pub fn insert(&mut self, key: K, value: V) -> Option<(V, u64)> {
        let next_gen = self.get_gen(&key).map(|gen| gen + 1).unwrap_or_default();
        self.items.insert(key, (value, next_gen))
    }

    /// Removes the value from the map and forgets about it.
    /// If you insert another value with the same key after calling this function,
    /// The value will have generation 0 and it will grow again.
    pub fn remove<Q>(&mut self, key: &Q) -> Option<(V, u64)>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.items.remove(key)
    }
}

impl<K, V> Default for GenMap<K, V> {
    fn default() -> Self {
        Self {
            items: AHashMap::new(),
        }
    }
}

/// A generational vector and its reference counts.
/// Use this if you don't want to remove items explicitly.
/// There's no remove function, use clear_unused() instead.
/// clear_unused() will remove items that don't have references from external.
///
/// Rc is used to keep track of each reference count, and it's split from value,
/// because we want values are alongside each other in memory.
/// Therefore, Rc doesn't have anything in it, its only purpose is tracking reference.
#[derive(Debug, Default)]
pub struct GenVecRc<T> {
    values: GenVec<T>,
    /// `refs` has always the same length as `values`.
    refs: OptVec<Rc<()>>,
}

impl<T> GenVecRc<T> {
    pub fn new() -> Self {
        Self {
            values: GenVec::new(),
            refs: OptVec::new(0),
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.values.len()
    }

    #[inline]
    pub fn len_occupied(&self) -> usize {
        self.values.len_occupied()
    }

    #[inline]
    pub fn len_vacant(&self) -> usize {
        self.values.len_vacant()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.values.capacity()
    }

    /// Determines the slot is None.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    #[inline]
    pub fn is_vacant(&self, index: usize) -> bool {
        self.refs.is_vacant(index)
    }

    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    #[inline]
    pub fn is_occupied(&self, index: usize) -> bool {
        self.refs.is_occupied(index)
    }

    /// Determines the slot is Some and its reference count is 1.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    #[inline]
    pub fn is_unused(&self, index: usize) -> bool {
        if let Some(_ref) = &self.refs[index] {
            Rc::strong_count(_ref) == 1
        } else {
            false
        }
    }

    /// Determines the slot is Some and its reference count is grater than 1.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    #[inline]
    pub fn is_used(&self, index: usize) -> bool {
        if let Some(_ref) = &self.refs[index] {
            Rc::strong_count(_ref) > 1
        } else {
            false
        }
    }

    /// Owns and therefore gets its [`GenIndexRc`] if it's not vacant.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    pub fn own(&self, index: usize) -> Option<GenIndexRc> {
        self.is_occupied(index).then(|| {
            let gen = self.values.get_gen(index);
            // Safety: index points to valid item.
            unsafe {
                GenIndexRc::new(
                    GenIndex::new(gen, index),
                    self.refs[index].as_ref().unwrap_unchecked(),
                )
            }
        })
    }

    /// Removes None items from the end, therefore the lengths of values and refs
    /// could be shrink. But note that their capacities won't change.
    pub fn clear_vacancy(&mut self) -> usize {
        let removed = self.values.clear_vacancy();
        self.refs.truncate(self.values.len() - removed);
        removed
    }

    /// Tries to shrink the capacities.
    pub fn shrink_to_fit(&mut self) {
        self.values.shrink_to_fit();
        self.refs.shrink_to_fit();
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = Option<&T>> {
        self.values.iter()
    }

    #[inline]
    pub fn iter_occupied(&self) -> impl Iterator<Item = &T> {
        self.values.iter_occupied()
    }

    pub fn iter_occupied_index(&self) -> impl Iterator<Item = GenIndexRcRef> {
        self.values
            .iter_occupied_index()
            .map(|index| GenIndexRcRef {
                index,
                _ref: self.refs[index.index].as_ref().unwrap(),
            })
    }

    pub fn sneak_iter_occupied_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.values.sneak_iter_occupied_mut()
    }

    #[inline]
    pub fn get(&self, index: GenIndex) -> Option<&T> {
        self.values.get(index)
    }

    #[inline]
    pub fn sneak_get_mut(&mut self, index: GenIndex) -> Option<&mut T> {
        self.values.sneak_get_mut(index)
    }

    pub fn insert(&mut self, value: T) -> GenIndexRc {
        let index = self.values.insert(value);
        if index.index == self.refs.len() {
            self.refs.push(None);
        }
        self.refs.set(index.index, Some(Rc::new(())));
        // Safety: Infallible.
        GenIndexRc::new(index, unsafe {
            self.refs[index.index].as_ref().unwrap_unchecked()
        })
    }

    pub fn extend(&mut self, values: impl Iterator<Item = T>) {
        for value in values {
            self.insert(value);
        }
    }

    /// Changes the value pointed by `index` with the `value`.
    /// Doesn't care if the old value is vacant or not.
    /// Therefore, returns new index just if `index` is valid.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    pub fn change(&mut self, index: GenIndex, value: T) -> Option<GenIndexRc> {
        let new_index = self.values.change(index, value)?;
        self.refs.set(new_index.index, Some(Rc::new(())));
        // Safety: Infallible.
        Some(GenIndexRc::new(new_index, unsafe {
            self.refs[new_index.index].as_ref().unwrap_unchecked()
        }))
    }

    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    pub fn update<U>(
        &mut self,
        index: GenIndex,
        f: impl FnOnce(&mut T) -> U,
    ) -> Option<(GenIndexRc, U)> {
        let (new_index, u) = self.values.update(index, f)?;
        self.refs.set(new_index.index, Some(Rc::new(())));
        // Safety: Infallible.
        Some((
            GenIndexRc::new(new_index, unsafe {
                self.refs[new_index.index].as_ref().unwrap_unchecked()
            }),
            u,
        ))
    }

    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    pub fn sneak_update<U>(&mut self, index: GenIndex, f: impl FnOnce(&mut T) -> U) -> Option<U> {
        self.values.sneak_update(index, f)
    }

    /// Takes out the item pointed by `index` only if it's not referenced.
    /// If it succeeded, item will become None and its content will be returned.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    #[inline]
    pub fn take(&mut self, index: GenIndex) -> Option<T> {
        if self.is_unused(index.index) {
            let old = self.values.take(index);
            if old.is_some() {
                self.refs.take(index.index);
            }
            old
        } else {
            None
        }
    }

    /// Drops unused values and leaves None in the slots (Takes O(n)).
    /// Therefore, there's no change with respect to lengths.
    /// Use clear_vacancy() to reduce actual length,
    /// And use shrink_to_fit() to try to reduce redundant capacity after calling this function.
    pub fn clear_unused(&mut self, mut f: impl FnMut(T)) {
        for index in 0..self.refs.len() {
            if self.is_unused(index) {
                // Safety: index is valid.
                let old = unsafe {
                    self.values
                        .take_unchecked(GenIndex::new_forced(index))
                        .unwrap_unchecked()
                };
                f(old);
                self.refs.take(index);
            }
        }
    }
}

/// A generational index owns the item in [`GenVecRc`].
/// See [`GenIndex`] if you're using [`GenVec`].
#[derive(PartialEq, Debug, Clone)]
pub struct GenIndexRc {
    pub index: GenIndex,
    _ref: Rc<()>,
}

impl GenIndexRc {
    pub fn new(index: GenIndex, _ref: &Rc<()>) -> Self {
        Self {
            index,
            _ref: Rc::clone(_ref),
        }
    }

    /// Generates dummy index.
    pub fn dummy() -> Self {
        Self {
            index: GenIndex::dummy(),
            _ref: Rc::new(()),
        }
    }

    #[inline]
    pub fn is_dummy(&self) -> bool {
        self.index.is_dummy()
    }
}

// Use this when you want to defer generating [`GenIndexRc`].
/// A sort of [`GenIndexRc`] but reference of Rc.
/// This can become a [`GenIndexRc`] using [`From::from()`].
#[derive(Debug)]
pub struct GenIndexRcRef<'a> {
    pub index: GenIndex,
    _ref: &'a Rc<()>,
}

impl<'a> From<GenIndexRcRef<'a>> for GenIndexRc {
    fn from(value: GenIndexRcRef<'a>) -> Self {
        Self {
            index: value.index,
            _ref: Rc::clone(value._ref),
        }
    }
}

/// A Vector using generational index.
///
/// # References
///
/// [RustConf 2018 - Closing Keynote - Using Rust For Game Development by Catherine West](https://www.youtube.com/watch?v=aKLntZcp27M)
#[derive(PartialEq, Debug, Default)]
pub struct GenVec<T> {
    entries: Vec<GenEntry<T>>,
    vacancies: Vec<usize>,
}

impl<T> GenVec<T> {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            vacancies: Vec::new(),
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    #[inline]
    pub fn len_occupied(&self) -> usize {
        self.entries.len() - self.vacancies.len()
    }

    #[inline]
    pub fn len_vacant(&self) -> usize {
        self.vacancies.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.entries.capacity()
    }

    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    #[inline]
    pub fn is_vacant(&self, index: usize) -> bool {
        self.entries[index].is_vacant()
    }

    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    #[inline]
    pub fn is_occupied(&self, index: usize) -> bool {
        self.entries[index].is_occupied()
    }

    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    #[inline]
    pub fn get_gen(&self, index: usize) -> u64 {
        self.entries[index].gen
    }

    /// Removes None items from the end, therefore the lengths of entries and vacancies
    /// could be shrink, but their capacities won't change.
    /// Call shrink_to_fit() to reduce capacities.
    ///
    /// Note that this operation **removes entries including their generation**,
    /// which means new inserted item will obtain a generation seen in the past.
    pub fn clear_vacancy(&mut self) -> usize {
        self.vacancies.sort_unstable();
        let before = self.entries.len();
        for i in (0..self.vacancies.len()).rev() {
            let entry_index = self.vacancies[i];
            if entry_index != self.entries.len() - 1 {
                break;
            }
            self.entries.pop();
            self.vacancies.pop();
        }
        before - self.entries.len()
    }

    /// Tries to shrink the capacities.
    pub fn shrink_to_fit(&mut self) {
        self.entries.shrink_to_fit();
        self.vacancies.shrink_to_fit();
    }

    pub fn iter(&self) -> impl Iterator<Item = Option<&T>> {
        self.entries.iter().map(|entry| entry.value.as_ref())
    }

    pub fn iter_occupied(&self) -> impl Iterator<Item = &T> {
        self.entries.iter().filter_map(|entry| entry.value.as_ref())
    }

    pub fn iter_index(&self) -> impl Iterator<Item = GenIndex> + '_ {
        self.entries
            .iter()
            .enumerate()
            .map(|(index, entry)| GenIndex::new(entry.gen, index))
    }

    pub fn iter_occupied_index(&self) -> impl Iterator<Item = GenIndex> + '_ {
        self.entries
            .iter()
            .enumerate()
            .filter_map(|(index, entry)| {
                entry
                    .is_occupied()
                    .then_some(GenIndex::new(entry.gen, index))
            })
    }

    pub fn sneak_iter_occupied_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.entries
            .iter_mut()
            .filter_map(|entry| entry.value.as_mut())
    }

    /// Gets the item pointed by `index`.
    /// Returns None if the `index` is out of bound or has incorrect generation,
    pub fn get(&self, index: GenIndex) -> Option<&T> {
        let entry = self.entries.get(index.index)?;
        if entry.gen == index.gen || entry.gen == GEN_IGNORE {
            entry.value.as_ref()
        } else {
            None
        }
    }

    pub fn sneak_get_mut(&mut self, index: GenIndex) -> Option<&mut T> {
        let entry = self.entries.get_mut(index.index)?;
        if entry.gen == index.gen || entry.gen == GEN_IGNORE {
            entry.value.as_mut()
        } else {
            None
        }
    }

    pub fn insert(&mut self, value: T) -> GenIndex {
        if let Some(index) = self.vacancies.pop() {
            let entry = &mut self.entries[index];
            entry.change(value);
            GenIndex::new(entry.gen, index)
        } else {
            let entry = GenEntry::new(Some(value));
            let gen = entry.gen;
            self.entries.push(entry);
            GenIndex::new(gen, self.entries.len() - 1)
        }
    }

    pub fn extend(&mut self, values: impl Iterator<Item = T>) {
        for value in values {
            self.insert(value);
        }
    }

    /// Changes the value pointed by `index` with the `value`.
    /// Doesn't care if the old value is vacant or not.
    /// Therefore, returns new index just if `index` is valid.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    #[inline]
    pub fn change(&mut self, index: GenIndex, value: T) -> Option<GenIndex> {
        assert!(index.index < self.entries.len());
        // Safety: Checked.
        unsafe { self.change_unchecked(index, value) }
    }

    /// # Safety
    ///
    /// Undefined behavior if index is out of bound.
    pub unsafe fn change_unchecked(&mut self, index: GenIndex, value: T) -> Option<GenIndex> {
        let entry = unsafe { self.entries.get_unchecked_mut(index.index) };
        (entry.gen == index.gen || entry.gen == GEN_IGNORE).then(|| {
            entry.change(value);
            GenIndex::new(entry.gen, index.index)
        })
    }

    /// Tries to update the value pointed by `index` with `f`.
    /// This doesn't do anything if the old value was vacant, so it returns None.
    /// Otherwise, returns new index.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    #[inline]
    pub fn update<U>(
        &mut self,
        index: GenIndex,
        f: impl FnOnce(&mut T) -> U,
    ) -> Option<(GenIndex, U)> {
        assert!(index.index < self.entries.len());
        // Safety: Checked.
        unsafe { self.update_unchecked(index, f) }
    }

    /// # Safety
    ///
    /// Undefined behavior if `index` is out of bound.
    pub unsafe fn update_unchecked<U>(
        &mut self,
        index: GenIndex,
        f: impl FnOnce(&mut T) -> U,
    ) -> Option<(GenIndex, U)> {
        let entry = unsafe { self.entries.get_unchecked_mut(index.index) };
        if entry.gen == index.gen || entry.gen == GEN_IGNORE {
            entry.update(f).map(|u| {
                (
                    GenIndex {
                        gen: entry.gen,
                        index: index.index,
                    },
                    u,
                )
            })
        } else {
            None
        }
    }

    /// Tries to update the value pointed by `index` with `f` without increasing generation.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    #[inline]
    pub fn sneak_update<U>(&mut self, index: GenIndex, f: impl FnOnce(&mut T) -> U) -> Option<U> {
        assert!(index.index < self.entries.len());
        // Safety: Checked.
        unsafe { self.sneak_update_unchecked(index, f) }
    }

    /// # Safety
    ///
    /// Undefined behavior if `index` is out of bound.
    pub unsafe fn sneak_update_unchecked<U>(
        &mut self,
        index: GenIndex,
        f: impl FnOnce(&mut T) -> U,
    ) -> Option<U> {
        let entry = unsafe { self.entries.get_unchecked_mut(index.index) };
        if entry.gen == index.gen || entry.gen == GEN_IGNORE {
            entry.sneak_update(f)
        } else {
            None
        }
    }

    /// Takes out the item pointed by `index`.
    /// If it succeeded, item will become None and its content will be returned.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    #[inline]
    pub fn take(&mut self, index: GenIndex) -> Option<T> {
        assert!(index.index < self.entries.len());
        // Safety: Checked.
        unsafe { self.take_unchecked(index) }
    }

    /// # Safety
    ///
    /// Undefined behavior if index is out of bound.
    pub unsafe fn take_unchecked(&mut self, index: GenIndex) -> Option<T> {
        let entry = self.entries.get_unchecked_mut(index.index);
        if entry.gen == index.gen || entry.gen == GEN_IGNORE {
            let old = entry.take();
            if old.is_some() {
                self.vacancies.push(index.index);
            }
            return old;
        }
        None
    }
}

impl<T> FromIterator<T> for GenVec<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut v = Self::new();
        v.extend(iter.into_iter());
        v
    }
}

impl<T> IntoIterator for GenVec<T> {
    type Item = Option<T>;
    type IntoIter = GenEntryIntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter {
            iter: self.entries.into_iter(),
        }
    }
}

pub struct GenEntryIntoIter<T> {
    iter: std::vec::IntoIter<GenEntry<T>>,
}

impl<T> Iterator for GenEntryIntoIter<T> {
    type Item = Option<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|entry| entry.value)
    }
}

impl<T: Clone> Clone for GenVec<T> {
    fn clone(&self) -> Self {
        Self {
            entries: self.entries.clone(),
            vacancies: self.vacancies.clone(),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct GenEntry<T> {
    gen: u64,
    value: Option<T>,
}

impl<T> GenEntry<T> {
    pub fn new(value: Option<T>) -> Self {
        Self { gen: 0, value }
    }

    #[inline]
    pub fn is_vacant(&self) -> bool {
        self.value.is_none()
    }

    #[inline]
    pub fn is_occupied(&self) -> bool {
        self.value.is_some()
    }

    pub fn take(&mut self) -> Option<T> {
        let old = self.value.take();
        if old.is_some() {
            self.gen += 1;
        }
        old
    }

    pub fn update<U>(&mut self, f: impl FnOnce(&mut T) -> U) -> Option<U> {
        self.value.is_some().then(|| {
            self.gen += 1;
            // Safety: Infallible.
            unsafe { f(self.value.as_mut().unwrap_unchecked()) }
        })
    }

    /// Updates without going to next generation.
    pub fn sneak_update<U>(&mut self, f: impl FnOnce(&mut T) -> U) -> Option<U> {
        self.value.is_some().then(|| {
            // Safety: Infallible.
            unsafe { f(self.value.as_mut().unwrap_unchecked()) }
        })
    }

    pub fn change(&mut self, value: T) -> Option<T> {
        let old = self.value.take();
        self.value = Some(value);
        self.gen += 1;
        old
    }

    pub fn reset(&mut self) {
        self.gen = 0;
    }
}

impl<T: Clone> Clone for GenEntry<T> {
    fn clone(&self) -> Self {
        Self {
            gen: self.gen,
            value: self.value.clone(),
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct GenIndex {
    pub gen: u64,
    pub index: usize,
}

impl GenIndex {
    pub fn new(gen: u64, index: usize) -> Self {
        Self { gen, index }
    }

    /// Ignores generation.
    pub fn new_forced(index: usize) -> Self {
        Self {
            gen: GEN_IGNORE,
            index,
        }
    }

    /// Generates dummy index.
    pub fn dummy() -> Self {
        Self {
            gen: GEN_DUMMY,
            index: usize::MAX,
        }
    }

    #[inline]
    pub fn is_dummy(&self) -> bool {
        self == &Self::dummy()
    }

    /// Ignores generation.
    pub fn into_forced(mut self) -> Self {
        self.gen = GEN_IGNORE;
        self
    }
}

impl From<&GenIndexRc> for GenIndex {
    fn from(value: &GenIndexRc) -> Self {
        value.index
    }
}

pub enum LabelOrGenIndexRc<'a, S> {
    Label(S),
    Index(&'a GenIndexRc),
}

impl<'a, S: Borrow<str>> From<S> for LabelOrGenIndexRc<'a, S> {
    fn from(value: S) -> Self {
        Self::Label(value)
    }
}

impl<'a, S: Borrow<str>> From<&'a GenIndexRc> for LabelOrGenIndexRc<'a, S> {
    fn from(value: &'a GenIndexRc) -> Self {
        Self::Index(value)
    }
}

pub enum LabelOrGenIndex<'a, S> {
    Label(S),
    Index(&'a GenIndex),
}

impl<'a, S: Borrow<str>> From<S> for LabelOrGenIndex<'a, S> {
    fn from(value: S) -> Self {
        Self::Label(value)
    }
}

impl<'a, S: Borrow<str>> From<&'a GenIndex> for LabelOrGenIndex<'a, S> {
    fn from(value: &'a GenIndex) -> Self {
        Self::Index(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_bindgen_test::*;

    #[wasm_bindgen_test]
    fn test_gen_vec() {
        // 0 to 10
        let mut v = (0..10).collect::<GenVec<_>>();
        let mut expect = 0;
        for value in v.iter() {
            assert_eq!(Some(&expect), value);
            expect += 1;
        }
        let mut expect = 0;
        for value in v.iter_occupied() {
            assert_eq!(&expect, value);
            expect += 1;
        }
        assert_eq!(10, v.len());
        assert_eq!(10, v.len_occupied());

        // Updates odd indices.
        for i in 0..10 {
            if i % 2 != 0 {
                let new_index = v.update(GenIndex::new(0, i), |v| *v += 10);
                assert_eq!(Some((GenIndex::new(1, i), ())), new_index);
            }
        }
        let mut expect = 0;
        for (i, value) in v.iter().enumerate() {
            if i % 2 != 0 {
                assert_eq!(Some(&(expect + 10)), value);
            } else {
                assert_eq!(Some(&expect), value);
            }
            expect += 1;
        }

        // Removes odd numbers.
        for i in 0..10 {
            if i % 2 != 0 {
                let old_value = v.take(GenIndex::new(1, i));
                assert_eq!(Some(i + 10), old_value);
            }
        }
        let mut expect = 0;
        for (i, value) in v.iter().enumerate() {
            if i % 2 != 0 {
                assert_eq!(None, value);
            } else {
                assert_eq!(Some(&expect), value);
            }
            expect += 1;
        }
        let mut expect = 0;
        for value in v.iter_occupied() {
            assert_eq!(&expect, value);
            expect += 2;
        }
        assert_eq!(10, v.len());
        assert_eq!(5, v.len_occupied());

        // Removes even numbers.
        for i in 0..10 {
            if i % 2 == 0 {
                let old_value = v.take(GenIndex::new(0, i));
                assert_eq!(Some(i), old_value);
            }
        }
        assert_eq!(10, v.len());
        assert_eq!(0, v.len_occupied());
        let removed = v.clear_vacancy();
        assert_eq!(10, removed);
        assert_eq!(0, v.len());
    }
}
