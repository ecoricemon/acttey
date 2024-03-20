use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    ops,
};

/// This doesn't allow duplication and support looking up values in O(1) like hash set.
/// Plus this guarantees inserted order like vector.
/// Similar to [`GenVec`](crate::ds::generational::GenVec), but this doesn't have generation.
/// This is based on [`OptVec`] so that it's also recommended to use NonZero series if it's possible.
#[derive(Debug, Clone)]
pub struct SetVec<T> {
    /// Optional values.
    values: OptVec<T>,

    /// Inverse map.  
    /// Vacant slots are not in here.
    imap: HashMap<T, usize, ahash::RandomState>,
}

impl<T: Clone + Hash + PartialEq + Eq> SetVec<T> {
    pub fn new() -> Self {
        Self {
            values: OptVec::new(0),
            imap: HashMap::default(),
        }
    }

    #[inline]
    pub fn contains<Q>(&self, value: &Q) -> bool
    where
        T: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.imap.contains_key(value)
    }

    #[inline]
    pub fn get_index<Q>(&self, value: &Q) -> Option<usize>
    where
        T: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.imap.get(value).cloned()
    }

    pub fn insert(&mut self, value: T) -> usize {
        if let Some(index) = self.get_index(&value) {
            index
        } else {
            let index = self.values.add(value.clone());
            self.imap.insert(value, index);
            index
        }
    }

    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    pub fn take_by_index(&mut self, index: usize) -> Option<T> {
        let old = self.values.take(index);
        if old.is_some() {
            // Safety: Infallible.
            unsafe {
                self.imap.remove(old.as_ref().unwrap_unchecked());
            }
        }
        old
    }

    pub fn take_by_value<Q>(&mut self, value: &Q) -> Option<T>
    where
        T: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.take_by_index(self.get_index(value)?)
    }
}

impl<T: Clone + Hash + PartialEq + Eq> Default for SetVec<T> {
    fn default() -> Self {
        Self::new()
    }
}

// Do not implement DerefMut because we need to synchronize imap.
impl<T: Clone + Hash + PartialEq + Eq> ops::Deref for SetVec<T> {
    type Target = OptVec<T>;

    fn deref(&self) -> &Self::Target {
        &self.values
    }
}

/// Vector with optional values.  
/// If you don't want to change indices after insert or remove operations,
/// it's a good signal to concern use of this.
/// Plus, this helps you to reuse vacant slots.  
/// Note that it's recommended to use NonZero series as generic argument
/// becuase you can save memery thanks to Rust optimization.
#[derive(Debug, Clone, Default)]
pub struct OptVec<T> {
    /// Optionable values.
    values: Vec<Option<T>>,

    /// Vacant slot list.
    vacancies: HashSet<usize, ahash::RandomState>,

    offset: usize,
}

impl<T> OptVec<T> {
    /// Slots under `offset` will be filled with None.
    pub fn new(offset: usize) -> Self {
        // values with fixed vacant slots.
        let mut values = Vec::new();
        for _ in 0..offset {
            values.push(None);
        }

        Self {
            values,
            vacancies: HashSet::default(),
            offset,
        }
    }

    /// Gets total number of slots including vacant slot.
    #[inline]
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Gets number of *occupied* slots.
    /// This is equivalent to [`Self::len`] - [`Self::len_vacant`].
    #[inline]
    pub fn len_occupied(&self) -> usize {
        self.len() - self.len_vacant()
    }

    /// Gets number of vacant slots.
    #[inline]
    pub fn len_vacant(&self) -> usize {
        self.vacancies.len()
    }

    /// Determins whether there's no slots at all.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Determines the slot is None, which means no value is in the slot.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    #[inline]
    pub fn is_vacant(&self, index: usize) -> bool {
        self.values[index].is_none()
    }

    /// Determines the slot is Some, which means there's a value in the slot.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    #[inline]
    pub fn is_occupied(&self, index: usize) -> bool {
        self.values[index].is_some()
    }

    #[inline]
    pub fn as_slice(&self) -> &[Option<T>] {
        &self.values
    }

    /// Do not modify Some or None status.
    #[inline]
    pub fn as_slice_mut(&mut self) -> &mut [Option<T>] {
        &mut self.values
    }

    /// Retrieves value located at the `index`.
    /// It can be None if the slot is vacant or `index` is out of bound.
    #[inline]
    pub fn get(&self, index: usize) -> Option<&T> {
        self.values.get(index)?.as_ref()
    }

    /// # Safety
    ///
    /// Undefine behavior if `index` is out of bound or
    /// the slot is vacant.
    pub unsafe fn get_unchecked(&self, index: usize) -> &T {
        self.values.get_unchecked(index).as_ref().unwrap_unchecked()
    }

    /// Gets a value located at the `index`.
    /// It can be None if `index` is out of bound or the slot is vacant.
    #[inline]
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.values.get_mut(index)?.as_mut()
    }

    /// # Safety
    ///
    /// Undefine behavior if `index` is out of bound or
    /// the slot is vacant.
    pub unsafe fn get_unchecked_mut(&mut self, index: usize) -> &mut T {
        self.values
            .get_unchecked_mut(index)
            .as_mut()
            .unwrap_unchecked()
    }

    /// Sets value wrapped with Option itself.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    pub fn set(&mut self, index: usize, value: Option<T>) -> Option<T> {
        if value.is_some() {
            self.vacancies.remove(&index);
        } else {
            self.vacancies.insert(index);
        }
        std::mem::replace(&mut self.values[index], value)
    }

    /// Adds the value to a vacant slot if it's possible or put it at the end of vector.
    /// Then returns its index.
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

    /// Appends the *Optional* value at the end of the vector.
    pub fn push(&mut self, value: Option<T>) {
        if value.is_none() {
            self.vacancies.insert(self.values.len());
        }
        self.values.push(value);
    }

    /// Takes the value out from the slot located at index, then returns the value.  
    /// It'd be None if the slot was vacant.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    pub fn take(&mut self, index: usize) -> Option<T> {
        let old = self.values[index].take()?;
        if index >= self.offset {
            self.vacancies.insert(index);
        }
        Some(old)
    }

    /// Swaps two occupied items.
    ///
    /// # Panics
    ///
    /// Panics if `a` or `b` are out of bounds or vacant.
    pub fn swap_occupied(&mut self, a: usize, b: usize) {
        assert!(self.is_occupied(a) && self.is_occupied(b));
        self.values.swap(a, b);
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (usize, &Option<T>)> {
        self.values.iter().enumerate()
    }

    pub fn iter_occupied(&self) -> impl Iterator<Item = (usize, &T)> {
        self.iter().filter_map(|(i, v)| v.as_ref().map(|v| (i, v)))
    }

    pub fn iter_occupied_mut(&mut self) -> impl Iterator<Item = (usize, &mut T)> {
        self.values
            .iter_mut()
            .enumerate()
            .filter_map(|(i, v)| v.as_mut().map(|v| (i, v)))
    }

    // `Option<T>` is not supposed to be modified without concerning vacancies.
    // That's why no iter_mut().
    /// Returns an iterator traversing over all types of slots.
    #[inline]
    pub fn values(&self) -> std::slice::Iter<'_, Option<T>> {
        self.values.iter()
    }

    /// Returns an iterator traversing only *occupied* slots.
    /// Use [`Self::values`] to iterate over all slots including vacant slots.
    pub fn values_occupied(&self) -> impl Iterator<Item = &T> + Clone {
        self.values().filter_map(|v| v.as_ref())
    }

    // This operation is allowed because it can't take inner value inside Option.
    /// Returns an iterator traversing only *occupied* slots.
    /// Use [`Self::values`] to iterate over all slots including vacant slots.
    pub fn values_occupied_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.values.iter_mut().filter_map(|v| v.as_mut())
    }

    /// Removes some slots from the end, so that `len` slots will remain after that.
    /// It does nothing if `len` is equal to or grater than currenet length.
    /// Note that you can't truncate less than offset you set.
    pub fn truncate(&mut self, len: usize) {
        for index in len.max(self.offset)..self.values.len() {
            if self.values.pop().is_none() {
                self.vacancies.remove(&index);
            }
        }
    }

    /// Shrinks the capacity.
    #[inline]
    pub fn shrink_to_fit(&mut self) {
        self.values.shrink_to_fit();
        self.vacancies.shrink_to_fit();
    }
}

// Do not implement IndexMut because we need to modify vacancies if users take the value from the slot.
impl<T> ops::Index<usize> for OptVec<T> {
    type Output = Option<T>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.values[index]
    }
}
