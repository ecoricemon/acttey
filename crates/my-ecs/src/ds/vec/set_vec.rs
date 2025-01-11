use super::OptVec;
use std::{
    collections::HashMap,
    hash::{BuildHasher, Hash},
    ops::Deref,
};

/// This doesn't allow duplication and supports looking up values in O(1) like hash set.
/// Plus this guarantees inserted order like vector.
/// Similar to [`GenVec`](crate::ds::generational::GenVec), but this doesn't have generation.
/// This is based on [`OptVec`] so that it's also recommended to use NonZero series if it's possible.
#[derive(Debug, Clone)]
pub struct SetVec<T, S> {
    /// Optional values.
    values: OptVec<T, S>,

    /// Inverse map.  
    /// Vacant slots don't belong to this.
    imap: HashMap<T, usize, S>,
}

impl<T, S> SetVec<T, S>
where
    T: Clone + Hash + PartialEq + Eq,
    S: Default,
{
    pub fn new() -> Self {
        Self {
            values: OptVec::new(),
            imap: HashMap::default(),
        }
    }
}

impl<T, S> SetVec<T, S>
where
    T: Clone + Hash + PartialEq + Eq,
    S: BuildHasher,
{
    pub fn contains<Q>(&self, value: &Q) -> bool
    where
        T: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.imap.contains_key(value)
    }

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

impl<T, S> Default for SetVec<T, S>
where
    T: Clone + Hash + PartialEq + Eq,
    S: Default,
{
    fn default() -> Self {
        Self::new()
    }
}

// Do not implement DerefMut because we need to synchronize imap.
impl<T, S> Deref for SetVec<T, S>
where
    T: Clone + Hash + PartialEq + Eq,
{
    type Target = OptVec<T, S>;

    fn deref(&self) -> &Self::Target {
        &self.values
    }
}
