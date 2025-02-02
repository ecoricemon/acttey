use std::{cmp::Ordering, collections::HashSet, hash::BuildHasher, mem, ops::Index, slice};

/// A vector containing optional values.
///
/// The vector would be useful when you need invariant index regardless of
/// insertion and removal. If you remove an item from the vector, the slot
/// remains vacant rather than removing the space by moving right items. Then
/// the vacant slot can be filled when you insert an item into the vector.
///
/// For more understanding, see the diagram below.
///
/// ```text
/// vector -> [ None, Some, None ]
/// occupied    No    Yes   No
/// vacant      Yes   No    Yes
///
/// * length: 1
/// * number of slots: 3
/// * number of vacancies: 2
/// ```
#[derive(Debug, Clone, Default)]
pub struct OptVec<T, S> {
    /// Optional values.
    values: Vec<Option<T>>,

    /// A set of indices to vacant slots.
    vacancies: HashSet<usize, S>,
}

impl<T, S> OptVec<T, S>
where
    S: Default,
{
    /// Creates a new empty vector.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<i32, RandomState>::new();
    /// ```
    pub fn new() -> Self {
        Self {
            values: Vec::new(),
            vacancies: HashSet::default(),
        }
    }
}

impl<T, S> OptVec<T, S> {
    /// Returns number of items, which is occupied slots in other words.
    ///
    /// Returned value is equal to `self.num_slots() - self.num_vacancies()`.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.add(0);
    /// assert_eq!(v.len(), 1);
    /// ```
    pub fn len(&self) -> usize {
        self.num_slots() - self.num_vacancies()
    }

    /// Returns true is the vector is empty.
    ///
    /// Note that vector may have slots in it even if it's empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<i32, RandomState>::new();
    /// assert!(v.is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns number of all slots including occupied and vacant slots.
    ///
    /// Returned value is equal to `self.len() + self.num_vacancies()`.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.add(0);
    /// assert_eq!(v.num_slots(), 1);
    /// ```
    pub fn num_slots(&self) -> usize {
        self.values.len()
    }

    /// Returns number of vacant slots.
    ///
    /// Returned value is equal to `self.num_slots() - self.len()`.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.add(0);
    /// v.take(0);
    /// assert_eq!(v.num_vacancies(), 1);
    /// ```
    pub fn num_vacancies(&self) -> usize {
        self.vacancies.len()
    }

    /// Returns true if the slot is vacant.
    ///
    /// # Panics
    ///
    /// Panics if the given index is out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.add(0);
    /// v.take(0);
    /// assert!(v.is_vacant(0));
    /// ```
    pub fn is_vacant(&self, index: usize) -> bool {
        self.values[index].is_none()
    }

    /// Returns true if the slot is occupied.
    ///
    /// # Panics
    ///
    /// Panics if the given index is out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.add(0);
    /// assert!(v.is_occupied(0));
    /// ```
    pub fn is_occupied(&self, index: usize) -> bool {
        self.values[index].is_some()
    }

    /// Creates a slice from the vector.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.add(0);
    /// v.add(1);
    /// assert_eq!(v.as_slice(), &[Some(0), Some(1)]);
    /// ```
    pub fn as_slice(&self) -> &[Option<T>] {
        &self.values
    }

    /// Creates a mutable slice from the vector.
    ///
    /// Caller must not modify occupied/vacant status in the returned slice
    /// because the vector is tracking the status.
    ///
    /// # Safety
    ///
    /// Undefined behavior if caller take out a value from an occupied slot, or
    /// insert a value into a vacant slot.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.add(0);
    /// v.add(1);
    /// assert_eq!(v.as_slice(), &[Some(0), Some(1)]);
    /// ```
    pub unsafe fn as_mut_slice(&mut self) -> &mut [Option<T>] {
        &mut self.values
    }

    /// Returns shared reference to the value at the given index if the index is
    /// in bounds and the slot is occupied.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.add(0);
    /// assert_eq!(v.get(0), Some(&0));
    /// ```
    pub fn get(&self, index: usize) -> Option<&T> {
        self.values.get(index)?.as_ref()
    }

    /// Returns shared reference to the value at the given index.
    ///
    /// # Safety
    ///
    /// Undefined behavior if
    /// - The index is out of bounds.
    /// - The slot is vacant.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.add(0);
    /// assert_eq!(unsafe { v.get_unchecked(0) }, &0);
    /// ```
    pub unsafe fn get_unchecked(&self, index: usize) -> &T {
        unsafe { self.values.get_unchecked(index).as_ref().unwrap_unchecked() }
    }

    /// Returns mutable reference to the value at the given index if the index
    /// is in bounds and the slot is occupied.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.add(0);
    /// assert_eq!(v.get_mut(0), Some(&mut 0));
    /// ```
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.values.get_mut(index)?.as_mut()
    }

    /// Returns shared reference to the value at the given index.
    ///
    /// # Safety
    ///
    /// Undefined behavior if
    /// - The index is out of bounds.
    /// - The slot is vacant.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.add(0);
    /// assert_eq!(unsafe { v.get_unchecked_mut(0) }, &mut 0);
    /// ```
    pub unsafe fn get_unchecked_mut(&mut self, index: usize) -> &mut T {
        unsafe {
            self.values
                .get_unchecked_mut(index)
                .as_mut()
                .unwrap_unchecked()
        }
    }

    /// Returns an iterator visiting all values with corresponding indices.
    ///
    /// As return type says, vacant slots are filtered out from the iteration.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.add('a');
    /// v.add('b');
    /// let pairs = v.pairs().collect::<Vec<_>>();
    /// assert_eq!(pairs, [(0, &'a'), (1, &'b')]);
    /// ```
    pub fn pairs(&self) -> impl Iterator<Item = (usize, &T)> {
        self.values
            .iter()
            .enumerate()
            .filter_map(|(i, v)| v.as_ref().map(|v| (i, v)))
    }

    /// Returns a mutable iterator visiting all values with corresponding
    /// indices.
    ///
    /// As return type says, vacant slots are filtered out from the iteration.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.add('a');
    /// v.add('b');
    /// let pairs = v.pairs_mut().collect::<Vec<_>>();
    /// assert_eq!(pairs, [(0, &mut 'a'), (1, &mut 'b')]);
    /// ```
    pub fn pairs_mut(&mut self) -> impl Iterator<Item = (usize, &mut T)> {
        self.values
            .iter_mut()
            .enumerate()
            .filter_map(|(i, v)| v.as_mut().map(|v| (i, v)))
    }

    /// Returns an iterator visiting all slots regardless of whether the slot
    /// is occupied or not.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.add('a');
    /// v.add('b');
    /// v.take(0);
    /// let slots = v.slots().cloned().collect::<Vec<_>>();
    /// assert_eq!(slots, [None, Some('b')]);
    /// ```
    pub fn slots(&self) -> slice::Iter<'_, Option<T>> {
        self.values.iter()
    }

    /// Returns an iterator visiting all values.
    ///
    /// As return type says, vacant slots are filtered out from the iteration.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.add('a');
    /// v.add('b');
    /// let values = v.iter().collect::<Vec<_>>();
    /// assert_eq!(values, [&'a', &'b']);
    /// ```
    pub fn iter(&self) -> impl Iterator<Item = &T> + Clone {
        self.values.iter().filter_map(|v| v.as_ref())
    }

    /// Returns a mutable iterator visiting all values.
    ///
    /// As return type says, vacant slots are filtered out from the iteration.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.add('a');
    /// v.add('b');
    /// let values = v.iter_mut().collect::<Vec<_>>();
    /// assert_eq!(values, [&mut 'a', &mut 'b']);
    /// ```
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.values.iter_mut().filter_map(|v| v.as_mut())
    }
}

impl<T, S> OptVec<T, S>
where
    S: BuildHasher,
{
    /// Sets a slot with the given optional value and returns old value.
    ///
    /// # Panics
    ///
    /// Panics if the given index is out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.add(0);
    /// assert_eq!(v.set(0, None), Some(0));
    /// ```
    pub fn set(&mut self, index: usize, value: Option<T>) -> Option<T> {
        if value.is_some() {
            self.vacancies.remove(&index);
        } else {
            self.vacancies.insert(index);
        }
        mem::replace(&mut self.values[index], value)
    }

    /// Returns the next index that will be returned on the next call to
    /// [`OptVec::add`].
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// assert_eq!(v.next_index(), 0);
    ///
    /// v.add(0);
    /// v.add(1);
    /// v.take(0);
    /// assert_eq!(v.next_index(), 0);
    /// ```
    pub fn next_index(&self) -> usize {
        if let Some(index) = self.vacancies.iter().next() {
            *index
        } else {
            self.values.len()
        }
    }

    /// Inserts the given value into the vector.
    ///
    /// The vector prefers to insert values into vacant slots if possible. But
    /// if the vector doesn't have any vacant slots, then the value is appended
    /// to the end of the vector.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.add(0);
    /// assert_eq!(v.len(), 1);
    /// ```
    pub fn add(&mut self, value: T) -> usize {
        if let Some(index) = self.vacancies.iter().next() {
            let index = *index;
            self.vacancies.remove(&index);
            self.values[index] = Some(value);
            index
        } else {
            self.values.push(Some(value));
            self.values.len() - 1
        }
    }

    /// Appends the given optional value to the end of the vector.
    ///
    /// Note that this method won't insert the value into a vacant slot. It just
    /// makes a new slot at the end of the vector then puts the value in the
    /// slot.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.add(0);
    /// v.take(0);
    /// v.push(Some(0));
    /// assert_eq!(v.as_slice(), &[None, Some(0)]);
    /// ```
    pub fn push(&mut self, value: Option<T>) {
        if value.is_none() {
            self.vacancies.insert(self.values.len());
        }
        self.values.push(value);
    }

    /// Takes value out of the slot at the given index.
    ///
    /// After calling the method, the slot remains vacant.
    ///
    /// # Panics
    ///
    /// Panics if the given index is out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.add(0);
    /// assert_eq!(v.take(0), Some(0));
    /// assert_eq!(v.take(0), None);
    /// ```
    pub fn take(&mut self, index: usize) -> Option<T> {
        let old = self.values[index].take()?;
        self.vacancies.insert(index);
        Some(old)
    }

    /// Resizes the vector to the given length.
    ///
    /// If the new length is greater than previous length of the vector, then
    /// the vector is extended with the given optional value. Otherwise, the
    /// vector is shrunk.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.resize(2, Some(0));
    /// assert_eq!(v.as_slice(), &[Some(0), Some(0)]);
    /// ```
    pub fn resize(&mut self, new_len: usize, value: Option<T>)
    where
        T: Clone,
    {
        self.resize_with(new_len, || value.clone());
    }

    /// Resizes the vector to the given length.
    ///
    /// If the new length is greater than previous length of the vector, then
    /// the vector is extended with optional values the given function
    /// generates. In this case, generated values are appended in order.
    /// Otherwise, the vector is shrunk.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.resize_with(2, || Some(0));
    /// assert_eq!(v.as_slice(), &[Some(0), Some(0)]);
    /// ```
    pub fn resize_with<F>(&mut self, new_len: usize, mut f: F)
    where
        F: FnMut() -> Option<T>,
    {
        match new_len.cmp(&self.num_slots()) {
            Ordering::Less => self.truncate(new_len),
            Ordering::Equal => {}
            Ordering::Greater => {
                let range = self.num_slots()..new_len;
                for _ in range {
                    self.push(f());
                }
            }
        }
    }

    /// Shrinks the vector to the given length, and drops abandoned items.
    ///
    /// If the given length is greater than or equal to the current length of
    /// the vector, nothing takes place.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.resize_with(4, || Some(0));
    /// v.truncate(2);
    /// assert_eq!(v.as_slice(), &[Some(0), Some(0)]);
    /// ```
    pub fn truncate(&mut self, len: usize) {
        for index in len..self.values.len() {
            if self.values.pop().is_none() {
                self.vacancies.remove(&index);
            }
        }
    }

    /// Removes vacant slots from the end of the vector, then shrinks capacity
    /// of the vector as much as possible.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.resize(5, Some(0));
    /// v.resize(10, None);
    /// v.shrink_to_fit();
    /// assert_eq!(v.num_vacancies(), 0);
    /// ```
    pub fn shrink_to_fit(&mut self) {
        while let Some(None) = self.values.last() {
            self.values.pop();
            self.vacancies.remove(&self.values.len());
        }
        self.values.shrink_to_fit();
        self.vacancies.shrink_to_fit();
    }

    /// Sets a slot with the given optional value and returns old value.
    ///
    /// Unlike [`OptVec::set`], this method doesn't panic even if the index is
    /// out of bounds, extending the vector with vacant slots instead.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::OptVec;
    /// use std::hash::RandomState;
    ///
    /// let mut v = OptVec::<_, RandomState>::new();
    /// v.extend_set(2, 0);
    /// assert_eq!(v.as_slice(), &[None, None, Some(0)]);
    /// ```
    pub fn extend_set(&mut self, index: usize, value: T) -> Option<T> {
        while self.num_slots() < (index + 1) {
            self.push(None);
        }
        self.set(index, Some(value))
    }
}

// Do not implement IndexMut because we need to modify vacancies if users take
// the value out of the slot.
impl<T, S> Index<usize> for OptVec<T, S> {
    type Output = Option<T>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.values[index]
    }
}

impl<T, S> IntoIterator for OptVec<T, S>
where
    S: BuildHasher,
{
    type Item = T;
    type IntoIter = IntoIter<T, S>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter::new(self)
    }
}

impl<T, S> From<OptVec<T, S>> for Vec<T>
where
    S: BuildHasher,
{
    fn from(value: OptVec<T, S>) -> Self {
        value.into_iter().collect()
    }
}

// TODO: Improve
#[derive(Debug)]
pub struct IntoIter<T, S> {
    vec: OptVec<T, S>,
    cur: usize,
}

impl<T, S> IntoIter<T, S> {
    fn new(vec: OptVec<T, S>) -> Self {
        Self { vec, cur: 0 }
    }
}

impl<T, S> Iterator for IntoIter<T, S>
where
    S: BuildHasher,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.vec.is_empty() {
            return None;
        }

        let mut output = None;
        while output.is_none() {
            output = self.vec.take(self.cur);
            self.cur += 1;
        }
        output
    }
}
