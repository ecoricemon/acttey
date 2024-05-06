use crate::{ds::vec::VarChunkVec, util::Window};
use std::{borrow::Borrow, collections::HashMap, hash::Hash};

#[derive(Debug, Clone)]
pub struct VarChunkBuffer<K, V> {
    /// Key to view index.
    map: HashMap<K, usize, ahash::RandomState>,

    /// Chunks including fragments.
    vec: VarChunkVec<V>,
}

impl<K, V> VarChunkBuffer<K, V> {
    pub fn new() -> Self {
        Self {
            map: HashMap::default(),
            vec: VarChunkVec::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            map: HashMap::default(),
            vec: VarChunkVec::with_capacity(capacity),
        }
    }

    /// Retrieves length of the buffer, which contains all chunks and fragments.
    #[inline]
    pub fn len(&self) -> usize {
        self.vec.len()
    }

    /// Determines the buffer is empty or not.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns iterator visiting all keys.
    #[inline]
    pub fn keys(&self) -> std::collections::hash_map::Keys<'_, K, usize> {
        self.map.keys()
    }

    /// Returns iterator visiting all chunks only, not fragments.
    #[inline]
    pub fn chunks(&self) -> impl Iterator<Item = (&K, &[V])> {
        self.map.iter().map(|(key, ci)| (key, self.as_chunk(*ci)))
    }

    /// Retrieves chunk window.
    ///
    /// # Panics
    ///
    /// - `ci` is out of bound.
    /// - `ci` points to a vacant slot.
    /// - In debug mode only, `ci` points to a fragment.
    #[inline]
    pub fn get_chunk_window(&self, ci: usize) -> &Window {
        self.vec.get_chunk_window(ci)
    }

    /// Gets all range of buffer as a slice.
    /// Note that the slice includes not only chunks but fragments as well.
    /// If you want a specific chunk slice, use [`Self::as_chunk`].
    #[inline]
    pub fn as_slice(&self) -> &[V] {
        self.vec.as_slice()
    }

    /// Retrieves chunk by chunk index.
    ///
    /// # Panics
    ///
    /// - `ci` is out of bound.
    /// - `ci` points to a vacant slot.
    /// - In debug mode only, `ci` points to a fragment.
    #[inline]
    pub fn as_chunk(&self, ci: usize) -> &[V] {
        self.vec.as_chunk(ci)
    }

    /// Retrieves chunk by chunk index.
    ///
    /// # Panics
    ///
    /// - `ci` is out of bound.
    /// - `ci` points to a vacant slot.
    /// - In debug mode only, `ci` points to a fragment.
    #[inline]
    pub fn as_chunk_mut(&mut self, ci: usize) -> &mut [V] {
        self.vec.as_chunk_mut(ci)
    }

    /// Retrieves chunk length.  
    /// But if you put in a fragment index, it'll be length of the fragment.
    ///
    /// # Panics
    ///
    /// - `ci` is out of bound.
    /// - `ci` points to a vacant slot.
    #[inline]
    pub fn chunk_len(&self, ci: usize) -> usize {
        self.vec.chunk_len(ci)
    }

    /// Retrieves item by chunk and item indices.
    ///
    /// # Panics
    ///
    /// - `ci` or `ii` are out of bounds.
    /// - In debug mode only, `ci` points to a vacant slot.
    #[inline]
    pub fn get_item(&self, ci: usize, ii: usize) -> &V {
        self.vec.get_item(ci, ii)
    }

    /// Retrieves item by chunk and item indices.
    ///
    /// # Panics
    ///
    /// - `ci` or `ii` are out of bounds.
    /// - In debug mode only, `ci` points to a vacant slot.
    #[inline]
    pub fn get_item_mut(&mut self, ci: usize, ii: usize) -> &mut V {
        self.vec.get_item_mut(ci, ii)
    }
}

impl<K, V> Default for VarChunkBuffer<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V: Default + Copy> VarChunkBuffer<K, V> {
    /// # Panics
    ///
    /// Panics if `ci` is out of bound or points to a fragment or vacant slot.
    #[inline]
    pub fn insert(&mut self, ci: usize, value: V) {
        self.vec.push_item_to_chunk(ci, value)
    }
}

impl<K: Hash + Eq, V: Default + Copy> VarChunkBuffer<K, V> {
    pub fn contains_key<Q>(&self, key: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.get_index(key).is_some()
    }

    /// Retrieves chunk window.
    #[inline]
    pub fn get_chunk_window2<Q>(&self, key: &Q) -> Option<&Window>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.get_index(key).map(|ci| self.get_chunk_window(ci))
    }

    /// Retrieves chunk by `key`.
    pub fn as_chunk2<Q>(&self, key: &Q) -> Option<&[V]>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.get_index(key).map(|ci| self.as_chunk(ci))
    }

    /// Retrieves chunk by `key`.
    pub fn as_chunk_mut2<Q>(&mut self, key: &Q) -> Option<&mut [V]>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.get_index(key).map(|ci| self.as_chunk_mut(ci))
    }

    /// Retrieves item by `key` and item index.
    ///
    /// # Panics
    ///
    /// Panics if `ii` is out of bound.
    pub fn get_item2<Q>(&self, key: &Q, ii: usize) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.get_index(key).map(|ci| self.get_item(ci, ii))
    }

    /// Retrieves item by `key` and item index.
    ///
    /// # Panics
    ///
    /// Panics if `ii` is out of bound.
    pub fn get_item_mut2<Q>(&mut self, key: &Q, ii: usize) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.get_index(key).map(|ci| self.get_item_mut(ci, ii))
    }

    /// Inserts `value` to present chunk or newly generated chunk, then returns the chunk's index.
    pub fn insert_new(&mut self, key: K, value: V) -> usize {
        if let Some(ci) = self.get_index(&key) {
            self.insert(ci, value);
            ci
        } else {
            let ci = self.vec.push_item(value);
            self.map.insert(key, ci);
            ci
        }
    }

    /// Returns true if `value` is appended successfully.
    pub fn insert2<Q>(&mut self, key: &Q, value: V) -> bool
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        if let Some(ci) = self.get_index(key) {
            self.insert(ci, value);
            true
        } else {
            false
        }
    }

    #[inline]
    fn get_index<Q>(&self, key: &Q) -> Option<usize>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.map.get(key).cloned()
    }
}

impl<'a, K, V: bytemuck::Pod> From<&'a VarChunkBuffer<K, V>> for &'a [u8] {
    fn from(value: &'a VarChunkBuffer<K, V>) -> Self {
        bytemuck::cast_slice(value.as_slice())
    }
}
