use super::opt_vec::OptVec;
use crate::util;
use std::{
    collections::{HashMap, HashSet},
    iter,
    ops::{Deref, DerefMut, Range},
};

// TODO: Test
// TODO: Alignment?, default is 1
// TODO: Automatical defragmmentation
/// Vector containing varying sized chunks.
/// All chunks lay on a single vector.
/// When you put an item in the buffer, only the corresponding chunk may move,
/// not making move all follwing items. Therefore, there can be fragmentations.
#[derive(Debug, Clone)]
pub struct VarChunkVec<T> {
    /// Chunk views. You can find chunk using the [`ChunkView`].
    /// It's guaranteed that index of each view won't change.
    views: OptVec<ChunkView>,

    /// Fragment list.
    /// Assumes that there's no much fragments.
    frags: HashSet<usize, ahash::RandomState>,

    /// Buffer index to view index, which is used to find next view.
    /// It always points to the valid [`ChunkView`].
    deref: HashMap<usize, usize, ahash::RandomState>,

    /// Raw data.
    buf: Vec<T>,
}

impl<T> VarChunkVec<T> {
    #[inline]
    pub fn new() -> Self {
        Self::with_capacity(0)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            views: OptVec::new(),
            frags: HashSet::default(),
            deref: HashMap::default(),
            buf: Vec::with_capacity(capacity),
        }
    }

    /// Retrieves length of the buffer, which contains all chunks and fragments.
    #[inline]
    pub fn len(&self) -> usize {
        self.buf.len()
    }

    /// Determines the buffer is empty or not.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns iterator visiting all chunks and fragments.
    /// Use [`Self::chunks_occupied`] if you want to skip fragments.
    pub fn iter(&self) -> impl Iterator<Item = &[T]> {
        self.views
            .values_occupied()
            .map(|view| &self.buf[view.range()])
    }

    /// Returns iterator visiting all chunks only, not fragments.
    pub fn chunks(&self) -> impl Iterator<Item = &[T]> {
        self.views
            .values_occupied()
            .filter_map(|view| (!view.is_fragment()).then_some(&self.buf[view.range()]))
    }

    /// Gets all range of buffer as a slice.
    /// Note that the slice includes not only chunks but fragments as well.
    /// If you want a specific chunk slice, use [`Self::as_chunk`].
    #[inline]
    pub fn as_slice(&self) -> &[T] {
        &self.buf
    }

    /// Retrieves chunk window.
    ///
    /// # Panics
    ///
    /// - `ci` is out of bound.
    /// - `ci` points to a vacant slot.
    /// - In debug mode only, `ci` points to a fragment.
    #[inline]
    pub fn get_chunk_window(&self, ci: usize) -> &util::Window {
        let view = self.views.get(ci).unwrap();
        debug_assert!(!view.is_fragment());
        &view.win
    }

    /// Retrieves chunk by chunk index.
    ///
    /// # Panics
    ///
    /// - `ci` is out of bound.
    /// - `ci` points to a vacant slot.
    /// - In debug mode only, `ci` points to a fragment.
    #[inline]
    pub fn as_chunk(&self, ci: usize) -> &[T] {
        let range = self.get_chunk_window(ci).range();
        &self.buf[range]
    }

    /// Retrieves chunk by chunk index.
    ///
    /// # Panics
    ///
    /// - `ci` is out of bound.
    /// - `ci` points to a vacant slot.
    /// - In debug mode only, `ci` points to a fragment.
    #[inline]
    pub fn as_chunk_mut(&mut self, ci: usize) -> &mut [T] {
        let range = self.get_chunk_window(ci).range();
        &mut self.buf[range]
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
        self.views.get(ci).unwrap().len
    }

    /// Retrieves item by chunk and item indices.
    ///
    /// # Panics
    ///
    /// - `ci` or `ii` are out of bounds.
    /// - `ci` points to a vacant slot.
    /// - In debug mode only, `ci` points to a vacant slot.
    #[inline]
    pub fn get_item(&self, ci: usize, ii: usize) -> &T {
        let index = self.get_chunk_window(ci).index(ii);
        &self.buf[index]
    }

    /// Retrieves item by chunk and item indices.
    ///
    /// # Panics
    ///
    /// - `ci` or `ii` are out of bounds.
    /// - `ci` points to a vacant slot.
    /// - In debug mode only, `ci` points to a vacant slot.
    #[inline]
    pub fn get_item_mut(&mut self, ci: usize, ii: usize) -> &mut T {
        let index = self.get_chunk_window(ci).index(ii);
        &mut self.buf[index]
    }

    /// Creates a chunk and adds `value` immediately.
    /// Returns the chunk index.
    pub fn push_item(&mut self, value: T) -> usize {
        if let Some(fi) = self.find_fragment(1) {
            // Unregisters `fi` from fragment list.
            self.unregister_fragment(fi);

            // Edits this view from fragment to chunk.
            let view = unsafe { self.views.get_unchecked_mut(fi) };
            self.buf[view.offset] = value;
            unsafe { self.shrink_chunk_unchecked(fi, 1) };

            fi
        } else {
            self.buf.push(value);
            self.create_view(self.buf.len() - 1, 1, false)
        }
    }

    /// Shrinks the chunk to have `left` items only.
    /// Then right range will become fragment.
    /// But if `left` is greater or equal to the current chunk size, nothing takes place.
    ///
    /// # Safety
    ///
    /// Undefined behavior if `ci` is out of bound or the view is vacant.
    unsafe fn shrink_chunk_unchecked(&mut self, ci: usize, left: usize) {
        let view = self.views.get_unchecked_mut(ci);
        if view.len > left {
            let roff = view.offset + left;
            let rlen = view.len - left;
            view.len = left;
            self.create_view(roff, rlen, true);
        }
    }

    fn create_view(&mut self, offset: usize, len: usize, frag: bool) -> usize {
        let fi = self.views.add(ChunkView::new(offset, len, frag));
        self.deref.insert(offset, fi);
        if frag {
            self.register_fragment(fi);
        }
        fi
    }

    fn register_fragment(&mut self, fi: usize) {
        self.frags.insert(fi);

        let view = self.views.get_mut(fi).unwrap();
        view.frag = true;
    }

    fn unregister_fragment(&mut self, ci: usize) {
        self.frags.remove(&ci);

        let view = self.views.get_mut(ci).unwrap();
        view.frag = false;
    }

    /// Retrieves the next chunk index in buffer perspective.
    fn get_next_chunk_index(&self, ci: usize) -> Option<usize> {
        let view = self.views.get(ci)?;
        self.deref.get(&view.end()).cloned()
    }

    /// Retrieves a fragment that has enough space using linear search.
    fn find_fragment(&self, len: usize) -> Option<usize> {
        // Safety: `self.frags` always has occupied indices.
        self.frags
            .iter()
            .find(|&&fi| unsafe { self.views.get(fi).unwrap_unchecked().len >= len })
            .cloned()
    }

    /// Shrinks fragment from left side by `amount`.
    /// Note that this assumes you put in a fragment index.
    ///
    /// # Panics
    ///
    /// Panics if `amount` is greater than the current fragement size.
    ///
    /// # Safety
    ///
    /// Undefined behavior if `fi` is out of bound or the slot is vacant.
    unsafe fn shrink_rev_unchecked(&mut self, fi: usize, amount: usize) {
        let view = self.views.get_unchecked_mut(fi);
        self.deref.remove(&view.offset);
        view.shrink_rev(amount);
        if view.is_empty() {
            self.views.take(fi);
            self.unregister_fragment(fi);
        } else {
            self.deref.insert(view.offset + amount, fi);
        }
    }

    /// Appends the `value` to the chunk pointed by `ci`.
    ///
    /// # Safety
    ///
    /// Undefined behavior if `ci` is out of bound or the slot is vacant.
    unsafe fn push_item_unchecked(&mut self, ci: usize, value: T) {
        self.unregister_fragment(ci);
        let view = unsafe { self.views.get_unchecked_mut(ci) };
        view.len += 1;
        if view.end() <= self.buf.len() {
            self.buf[view.end() - 1] = value;
        } else {
            self.buf.push(value);
        }
    }

    /// Tries to append the `value` using the next fragment's space.
    /// If it succedded, returns true.
    fn push_item_using_fragment(&mut self, ci: usize, value: T) -> bool {
        if let Some(nci) = self.get_next_chunk_index(ci) {
            // Safety: `ci` and `nci` are valid, so corresponding views are valid as well.
            unsafe {
                let nview = self.views.get_unchecked_mut(nci);
                if nview.is_fragment() && !nview.is_empty() {
                    self.shrink_rev_unchecked(nci, 1);
                    self.push_item_unchecked(ci, value);
                }
            };
            true
        } else {
            false
        }
    }
}

impl<T> Default for VarChunkVec<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Default + Copy> VarChunkVec<T> {
    /// Appends the `value` at the end of the chunk.
    ///
    /// # Panics
    ///
    /// Panics if `ci` is out of bound or points to a fragment or vacant slot.
    pub fn push_item_to_chunk(&mut self, ci: usize, value: T) {
        // Checks.
        let view = *self.views.get(ci).unwrap();
        assert!(!view.is_fragment());

        // First, tries to use the next fragment.
        if self.push_item_using_fragment(ci, value) {
            return;
        }

        // If it failed, tries to use enough large fragment.
        let (fi, ii) = if let Some(fi) = self.find_fragment(view.len + 1) {
            // Safety: `fi` is valid.
            let frag_offset = unsafe { self.views.get(fi).unwrap_unchecked().offset };
            self.buf.copy_within(view.range(), frag_offset);
            (fi, frag_offset + view.len)
        } else {
            // If it failed, makes new fragment.
            let fi = self.create_view(self.buf.len(), view.len + 1, true);
            self.extend_buffer(view.len + 1, view.range());
            (fi, self.buf.len() - 1)
        };

        // Appends `value`.
        self.buf[ii] = value;

        // Swaps views.
        self.views.swap_occupied(ci, fi);

        // Now, `ci` is not a fragment and `fi` is a fragment.
        self.unregister_fragment(ci);
        self.register_fragment(fi);

        // Adjusts length.
        unsafe { self.shrink_chunk_unchecked(ci, view.len + 1) };
    }

    /// # Panics
    ///
    /// Panics if `src` range is greater than `len`.
    fn extend_buffer(&mut self, len: usize, src: Range<usize>) {
        let diff = len - src.len();
        self.buf.reserve_exact(len);
        self.buf.extend_from_within(src);
        let defaults = iter::repeat(T::default()).take(diff);
        self.buf.extend(defaults);
    }

    // /// It's recommended to put all data in at once to avoid fragmentations.
    // /// To do so, you can reserve some space first.
    // /// If the `key` is new one or `len` is greater than old one,
    // /// only newly extended slots are filled with default values.
    // /// Call [`Self::get_chunk_mut`] to set actual data to the chunk.
    // pub fn reserve_chunk(&mut self, key: K, len: usize) {
    //     // Prevents empty values.
    //     assert!(len > 0);

    //     if let Some(vi) = self.map.get(&key).cloned() {
    //         // Expands chunk.
    //     } else {
    //         self.map.insert(key, self.views.len());
    //         self.views.push(ChunkView::new_occupied(self.buf.len(), len));
    //         self.extend_buffer(len, 0..0);
    //     }
    // }

    // pub fn insert_item<Q>(&mut self, key: &Q, value: V) -> Option<usize>
    // where
    //     K: Borrow<Q>,
    //     Q: Hash + Eq + ?Sized,
    // {
    //     if let Some(vi) = self.map.get(key).cloned() {
    //         let new_len = self.views[vi].len + 1;
    //         self.resize_chunk(vi, new_len);
    //         let last = self.views[vi].offset + new_len - 1;
    //         self.buf[last] = value;
    //         Some(new_len - 1)
    //     } else {
    //         None
    //     }
    // }

    // /// Removes all fragments by moving occupied chunks to the front of the buffer.
    // pub fn defragment(&mut self) {
    //     // Brings all occupied chunks to the front.
    //     let n = self.views.len();
    //     let mut cur = 0;
    //     let mut l = 0;
    //     for r in 0..n {
    //         match &mut self.views[r] {
    //             ChunkView::Occupied(view) => {
    //                 if cur < view.offset {
    //                     self.buf.copy_within(view.range(), cur);
    //                     view.offset = cur;
    //                 }
    //                 cur += view.len;
    //                 self.views.swap(l, r);
    //                 l += 1;
    //             },
    //             ChunkView::Fragment(_) => {},
    //         }
    //     }

    //     // Removes fragments.
    //     self.views.truncate(l);

    //     // Reduces the length, but keeps the capacity.
    //     self.buf.truncate(cur);

    //     self.frag_size = 0;
    // }

    // /// Resizes the chunk such that the chunk has `len` items within it.
    // /// If `len` is greater than the currrent length, there can be filled with garbage values.
    // /// Otherwise, length is shrinked, but items may not be dropped.
    // fn resize_chunk(&mut self, vi: usize, len: usize) {
    //     let cur_len = self.views[vi].len;
    //     if cur_len < len {
    //         let diff = len - cur_len;
    //         match self.views.get(vi + 1) {
    //             // Am I the last one? then extend myself only.
    //             None => {
    //                 self.views[vi].len = len;
    //                 self.extend_buffer(diff, 0..0);
    //             }
    //             // Can use the next fragment? then adjust views only.
    //             Some(ChunkView::Fragment(next)) if diff <= next.len => {
    //                 self.views[vi].len = len;
    //                 self.views[vi + 1].shrink_rev(diff);
    //             },
    //             // Hard to extend in-place. Move it.
    //             _ => {
    //                 let mut view = self.views[vi].into_fragment();
    //                 view.reset(self.buf.len(), len);
    //                 self.views.push(view);
    //                 self.extend_buffer(len, self.views[vi].range());
    //             }
    //         }
    //     } else { // Shrink or do nothing
    //         todo!()
    //     }
    // }
}

impl<'a, T: bytemuck::Pod> From<&'a VarChunkVec<T>> for &'a [u8] {
    fn from(value: &'a VarChunkVec<T>) -> Self {
        bytemuck::cast_slice(value.as_slice())
    }
}

#[derive(Debug, Clone, Copy)]
struct ChunkView {
    /// A range of buffer.
    win: util::Window,

    /// If true, it means this chunk is a fragmment.
    frag: bool,
}

impl ChunkView {
    /// Creates a new view.
    #[inline]
    pub fn new(offset: usize, len: usize, frag: bool) -> Self {
        Self {
            win: util::Window::new(offset, len),
            frag,
        }
    }

    /// Determines whether this is a fragment or not.
    #[inline]
    pub fn is_fragment(&self) -> bool {
        self.frag
    }
}

impl Deref for ChunkView {
    type Target = util::Window;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.win
    }
}

impl DerefMut for ChunkView {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.win
    }
}
