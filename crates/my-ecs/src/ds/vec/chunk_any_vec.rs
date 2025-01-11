use super::{
    super::{
        raw::{
            AsRawIter, FlatIter, FlatIterMut, FlatRawIter, ParFlatIter, ParFlatIterMut, RawIter,
        },
        types::{FnCloneRaw, FnDropRaw, TypeInfo},
    },
    AnyVec,
};
use crate::{
    ds::raw::{AsFlatRawIter, ParFlatRawIter},
    util::prelude::*,
};
use std::{
    any::{self, TypeId},
    cmp, mem,
    ptr::{self, NonNull},
};

#[derive(Debug, Clone)]
pub struct ChunkAnyVec {
    /// The very first chunk is used for special purpose.
    /// If the type is zero sized type, only the first chunk is used.
    /// Otherwise, that is used for the cloning.
    chunks: Vec<AnyVec>,

    /// Maximum length and initial capacity of each chunk.
    /// Zero is allowed and it's considered as usize::MAX + 1.
    chunk_len: PowerOfTwo,

    /// Number of items in all chunks.
    len: usize,

    /// Current capacity.
    cap: usize,

    /// If the type is zero sized, this value is 1 due to the first special chunk.
    /// Otherwise, this value is 0.
    chunk_offset: usize,

    /// Vector can grow as long as this value.
    max_cap: usize,
}

impl ChunkAnyVec {
    /// If the type size is zero, then `chunk_len` is ignored.
    pub fn new(tinfo: TypeInfo, chunk_len: usize) -> Self {
        let mut v = Self {
            chunks: vec![AnyVec::new(tinfo)],
            chunk_len: PowerOfTwo::new(chunk_len).unwrap(),
            len: 0,
            cap: 0,
            max_cap: 0,
            chunk_offset: 1,
        };

        // If ZST, we won't allocate any memory for the vector.
        // But, adjust capacity like `Vec`.
        if v.is_zst() {
            v.chunk_len = PowerOfTwo::new(0).unwrap();
            v.cap = usize::MAX;
            v.max_cap = usize::MAX;
            v.chunk_offset = 0;
        } else if v.chunk_len() > isize::MAX as usize {
            // We need to restrict isize::MAX + 1.
            panic!("chunk_len must be less than isize::MAX");
        } else {
            // Same limitation with `Vec` or `AnyVec`.
            v.max_cap = isize::MAX as usize / v.chunks[0].item_size();
        }

        v
    }

    pub fn type_info(&self) -> &TypeInfo {
        self.chunks[0].type_info()
    }

    pub fn type_id(&self) -> TypeId {
        self.chunks[0].type_id()
    }

    pub fn type_name(&self) -> &'static str {
        self.chunks[0].type_name()
    }

    pub fn item_size(&self) -> usize {
        self.chunks[0].item_size()
    }

    pub fn is_zst(&self) -> bool {
        self.chunks[0].is_zst()
    }

    pub fn align(&self) -> usize {
        self.chunks[0].align()
    }

    pub fn fn_drop(&self) -> FnDropRaw {
        self.chunks[0].fn_drop()
    }

    pub fn fn_clone(&self) -> FnCloneRaw {
        self.chunks[0].fn_clone()
    }

    pub fn is_clone(&self) -> bool {
        self.chunks[0].is_clone()
    }

    pub fn is_send(&self) -> bool {
        self.chunks[0].is_send()
    }

    pub fn is_sync(&self) -> bool {
        self.chunks[0].is_sync()
    }

    pub fn is_type_of(&self, ty: &TypeId) -> bool {
        self.chunks[0].is_type_of(ty)
    }

    pub const fn len(&self) -> usize {
        self.len
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub const fn capacity(&self) -> usize {
        self.cap
    }

    pub const fn chunk_len(&self) -> usize {
        self.chunk_len.get()
    }

    pub fn chunk_count(&self) -> usize {
        self.chunks.len() - self.chunk_offset
    }

    pub fn get_chunk(&self, ci: usize) -> Option<&AnyVec> {
        self.chunks.get(ci + self.chunk_offset)
    }

    pub fn iter_of<T: 'static>(&self) -> FlatIter<'_, T> {
        assert!(
            self.is_type_of(&TypeId::of::<T>()),
            "type doesn't match, contains {:?} but {:?} requested",
            self.type_name(),
            any::type_name::<T>()
        );
        // Safety: Vector contains type `T` data in it.
        unsafe { AsFlatRawIter::iter_of(self) }
    }

    pub fn par_iter_of<T: 'static>(&self) -> ParFlatIter<'_, T> {
        assert!(
            self.is_type_of(&TypeId::of::<T>()),
            "type doesn't match, contains {:?} but {:?} requested",
            self.type_name(),
            any::type_name::<T>()
        );
        // Safety: Vector contains type `T` data in it.
        unsafe { AsFlatRawIter::par_iter_of(self) }
    }

    pub fn iter_mut_of<T: 'static>(&mut self) -> FlatIterMut<'_, T> {
        assert!(
            self.is_type_of(&TypeId::of::<T>()),
            "type doesn't match, contains {:?} but {:?} requested",
            self.type_name(),
            any::type_name::<T>()
        );
        // Safety: Vector contains type `T` data in it.
        unsafe { AsFlatRawIter::iter_mut_of(self) }
    }

    pub fn par_iter_mut_of<T: 'static>(&mut self) -> ParFlatIterMut<'_, T> {
        assert!(
            self.is_type_of(&TypeId::of::<T>()),
            "type doesn't match, contains {:?} but {:?} requested",
            self.type_name(),
            any::type_name::<T>()
        );
        // Safety: Vector contains type `T` data in it.
        unsafe { AsFlatRawIter::par_iter_mut_of(self) }
    }

    pub fn reserve(&mut self, add_num: usize) {
        self.reserve_exact(add_num);
    }

    pub fn reserve_exact(&mut self, add_num: usize) {
        let need_cap = self.len.saturating_add(add_num);
        if self.capacity() < need_cap {
            if need_cap > self.max_capacity() {
                panic!("can't allocate {need_cap} x {} bytes", self.item_size());
            }

            // Rounds up the target capacity to be a multiple of chunk length.
            // This operation doesn't overflow because cap and len are restricted.
            let new_cap = (need_cap + self.chunk_len() - 1) & !(self.chunk_len() - 1);

            // If the new_cap is clamped by this operation,
            // the last chunk length will be the same as the others' - 1.
            let new_cap = cmp::min(new_cap, self.max_capacity());

            let mut remain = new_cap - self.capacity();
            while remain > 0 {
                let partial = cmp::min(self.chunk_len(), remain);
                let chunk = self.new_chunk(partial);
                self.chunks.push(chunk);

                remain -= partial;
            }

            self.cap = new_cap;
        }
    }

    /// # Examples
    ///
    /// ```
    /// # use my_ecs::prelude::*;
    ///
    /// let chunk_len = 4;
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), chunk_len);
    /// assert_eq!(v.capacity(), 0);
    ///
    /// unsafe { v.push(1_i32) };
    /// assert_eq!(v.capacity(), chunk_len);
    ///
    /// v.reserve(chunk_len);
    /// assert_eq!(v.capacity(), chunk_len * 2);
    ///
    /// v.shrink_to_fit();
    /// assert_eq!(v.capacity(), chunk_len);
    ///
    /// v.pop_drop();
    /// v.shrink_to_fit();
    /// assert_eq!(v.capacity(), 0);
    /// ```
    pub fn shrink_to_fit(&mut self) {
        if self.is_zst() || self.len() == self.capacity() {
            return;
        }

        let mut remain = self.capacity() - self.len();

        unsafe {
            while remain > 0 && remain >= self.chunks.last().unwrap_unchecked().capacity() {
                let last = self.chunks.pop().unwrap_unchecked();
                remain -= last.capacity();
            }
        }

        self.cap = self.len() + remain;
    }

    /// # Safety
    ///
    /// - `new_len` must be less than or equal to [`Self::capacity`].
    /// - Caller must initialized extended items.
    pub unsafe fn set_len(&mut self, new_len: usize) {
        debug_assert!(new_len <= self.capacity());

        self.len = new_len;
    }

    /// Caller should make sure data pointed by `ptr` not to be dropped.
    /// To make it not to be dropped, call [`std::mem::forget`].
    ///
    /// # Safety
    ///
    /// `ptr` must point to valid data type.
    pub unsafe fn push_raw(&mut self, ptr: NonNull<u8>) {
        self.reserve(1);

        let (ci, _) = self.index_2d(self.len());
        self.chunks[ci].push_raw(ptr);

        // Safety: Infallible.
        unsafe { self.set_len(self.len().checked_add(1).unwrap()) };
    }

    /// # Safety
    ///
    /// `write` must write correct type on given buffer.
    pub unsafe fn push_with<F>(&mut self, write: F)
    where
        F: FnOnce(*mut u8),
    {
        self.reserve(1);

        let (ci, _) = self.index_2d(self.len());
        self.chunks[ci].push_with(write);

        // Safety: Infallible.
        unsafe { self.set_len(self.len().checked_add(1).unwrap()) };
    }

    /// # Safety
    ///
    /// Type of the value `T` must be the same as the type the vector knows.
    pub unsafe fn push<T: 'static>(&mut self, mut value: T) {
        debug_assert!(self.is_type_of(&TypeId::of::<T>()));

        let ptr = NonNull::new_unchecked(&mut value as *mut T as *mut u8);
        self.push_raw(ptr);
        mem::forget(value);
    }

    /// Overwrites value located at index with the given pointer.
    /// Note that this doesn't drop old value.
    ///
    /// # Safety
    ///
    /// - `index` must be in bound.
    /// - `ptr` must point to valid data type.
    pub unsafe fn update_unchecked(&mut self, index: usize, ptr: NonNull<u8>) {
        let (ci, ii) = self.index_2d(index);
        self.chunks[ci].update_unchecked(ii, ptr);
    }

    /// Removes the last item from the vector and writes it to the given
    /// buffer, then returns `Some`.
    ///
    /// If removing is successful, caller becomes to own the item in the
    /// buffer, so that caller must call `drop()` on it correctly.
    /// Otherwise, returns `None` without change to the buffer.
    ///
    /// # Examples
    ///
    /// ```
    /// # use my_ecs::prelude::*;
    ///
    /// let chunk_len = 4;
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), chunk_len);
    /// unsafe { v.push(42_i32) };
    /// assert_eq!(v.len(), 1);
    ///
    /// let mut buf = 0_i32;
    /// let res = unsafe { v.pop_raw(&mut buf as *mut i32 as *mut u8) };
    /// assert!(res.is_some());
    /// assert!(v.is_empty());
    /// assert_eq!(buf, 42);
    /// ```
    ///
    /// # Safety
    ///
    /// Undefined behavior if conditions below are not met.
    /// - `buf` must have enough size to be copied an item.
    /// - When `Some` is returned, `buf` must be correctly handled as an item.
    ///   For example, if an item should be dropped, caller must call drop() on
    ///   it.
    /// - When `None` is returned, `buf` must be correctly handled as it was.
    pub unsafe fn pop_raw(&mut self, buf: *mut u8) -> Option<()> {
        if self.is_empty() {
            None
        } else {
            // Safety: Vector is not empty.
            let chunk = self._pop();
            chunk.pop_raw(buf);
            Some(())
        }
    }

    /// # Safety
    ///
    /// Type of the value `T` must be the same as the type the vector knows.
    pub unsafe fn pop<T: 'static>(&mut self) -> Option<T> {
        if self.is_empty() {
            None
        } else {
            // Safety: Vector is not empty.
            let chunk = unsafe { self._pop() };
            chunk.pop()
        }
    }

    pub fn pop_drop(&mut self) -> Option<()> {
        if self.is_empty() {
            None
        } else {
            // Safety: Vector is not empty.
            let chunk = unsafe { self._pop() };
            chunk.pop_drop()
        }
    }

    pub fn pop_forget(&mut self) -> Option<()> {
        if self.is_empty() {
            None
        } else {
            // Safety: Vector is not empty.
            let chunk = unsafe { self._pop() };
            chunk.pop_forget()
        }
    }

    /// Reduces length by 1 and returns the last chunk.
    /// Don't forget to call pop from the chunk.
    ///
    /// # Safety
    ///
    /// Length of the vector must not be zero.
    unsafe fn _pop(&mut self) -> &mut AnyVec {
        // Safety: Decreasing is safe.
        self.set_len(self.len() - 1);

        let (ci, _) = self.index_2d(self.len());

        // Safety: `r` is valid.
        unsafe { self.chunks.get_unchecked_mut(ci) }
    }

    /// Removes an item at the given index from the vector and writes it to the
    /// given buffer.
    ///
    /// Therefore the item is actually moved from the vector to the given
    /// buffer. So caller must take care of calling drop on it.
    ///
    /// # Examples
    ///
    /// ```
    /// # use my_ecs::prelude::*;
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
    /// unsafe {
    ///     v.push(0_i32);
    ///     v.push(1_i32);
    ///     v.push(2_i32);
    /// }
    /// assert_eq!(v.len(), 3);
    ///
    /// let mut buf = 3_i32;
    /// unsafe { v.swap_remove_raw(0, &mut buf as *mut i32 as *mut u8) };
    /// assert_eq!(buf, 0);
    ///
    /// unsafe {
    ///     assert_eq!(v.pop::<i32>(), Some(1));
    ///     assert_eq!(v.pop::<i32>(), Some(2));
    /// }
    /// ```
    ///
    /// # Panics
    ///
    /// Panics if the given index is out of bounds.
    ///
    /// # Safety
    ///
    /// Undefined behavior if conditions below are not met.
    /// - `buf` must have enough size to be copied an item.
    /// - When `Some` is returned, `buf` must be correctly handled as an item.
    ///   For example, if an item should be dropped, caller must call drop() on
    ///   it.
    /// - When `None` is returned, `buf` must be correctly handled as it was.
    pub unsafe fn swap_remove_raw(&mut self, index: usize, buf: *mut u8) {
        // If index is out of bounds or len() - 1 overflows, swap() panics.
        self.swap(index, self.len().wrapping_sub(1));
        self.pop_raw(buf);
    }

    /// # Panics
    ///
    /// Panics if `index` is out of bounds.
    ///
    /// # Safety
    ///
    /// Type of the value `T` must be the same as the type the vector knows.
    pub unsafe fn swap_remove<T: 'static>(&mut self, index: usize) -> T {
        // If index is out of bounds or len() - 1 overflows, swap() panics.
        self.swap(index, self.len().wrapping_sub(1));
        self.pop().unwrap()
    }

    /// # Panics
    ///
    /// Panics if `index` is out of bounds.
    pub fn swap_remove_drop(&mut self, index: usize) {
        // If index is out of bounds or len() - 1 overflows, swap() panics.
        self.swap(index, self.len().wrapping_sub(1));
        self.pop_drop();
    }

    /// # Panics
    ///
    /// Panics if `index` is out of bounds.
    pub fn swap_remove_forget(&mut self, index: usize) {
        // If index is out of bounds or len() - 1 overflows, swap() panics.
        self.swap(index, self.len().wrapping_sub(1));
        self.pop_forget();
    }

    pub fn swap(&mut self, index0: usize, index1: usize) {
        assert!(index0 < self.len());
        assert!(index1 < self.len());

        unsafe {
            let (ci0, ii0) = self.index_2d(index0);
            let (ci1, ii1) = self.index_2d(index1);
            let ptr0 = self.chunks[ci0].get_ptr(ii0);
            let ptr1 = self.chunks[ci1].get_ptr(ii1);
            if ptr0 != ptr1 {
                ptr::swap_nonoverlapping(ptr0, ptr1, self.item_size());
            }
        }
    }

    pub fn get_raw(&self, index: usize) -> Option<NonNull<u8>> {
        if index < self.len() {
            unsafe { Some(self.get_raw_unchecked(index)) }
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// `index` must be inbound and result address must not overflow `isize`.
    pub unsafe fn get_raw_unchecked(&self, index: usize) -> NonNull<u8> {
        let (ci, ii) = self.index_2d(index);
        self.chunks[ci].get_raw_unchecked(ii)
    }

    /// # Safety
    ///
    /// Type of the value `T` must be the same as the type the vector knows.
    pub unsafe fn get<T: 'static>(&self, index: usize) -> Option<&T> {
        if index < self.len() {
            let (ci, ii) = self.index_2d(index);
            // Type is checked here.
            self.chunks[ci].get(ii)
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// Type of the value `T` must be the same as the type the vector knows.
    pub unsafe fn get_mut<T: 'static>(&mut self, index: usize) -> Option<&mut T> {
        if index < self.len() {
            let (ci, ii) = self.index_2d(index);
            // Type is checked here.
            self.chunks[ci].get_mut(ii)
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// Type of the value `T` must be the same as the type the vector knows.
    pub unsafe fn resize<T>(&mut self, new_len: usize, value: T)
    where
        T: Clone + 'static,
    {
        self.resize_with(new_len, || value.clone());
    }

    /// # Safety
    ///
    /// Conditions below must be met.
    /// - Type of value pointed by `ptr` must be the same as the type the vector
    ///   knows.
    /// - Type must implement [`Clone`].
    pub unsafe fn resize_raw(&mut self, new_len: usize, val_ptr: NonNull<u8>) {
        debug_assert!(self.is_clone());

        if new_len > self.len() {
            let mut remain = new_len - self.len();

            self.reserve(remain);

            let mut ci = if self.is_empty() {
                self.chunk_offset
            } else {
                let (ci, _) = self.index_2d(self.len() - 1);
                ci
            };

            while remain > 0 {
                let clen = self.chunks[ci].len();
                let partial = cmp::min(self.chunk_len() - clen, remain);
                self.chunks[ci].resize_raw(clen + partial, val_ptr);

                remain -= partial;
                ci += 1;
            }

            unsafe {
                self.set_len(new_len);
            }
        } else {
            self.truncate(new_len);
        }
    }

    /// Resizes the vector with values the given function generates.
    ///
    /// Generated values will be added in order.
    ///
    /// # Safety
    ///
    /// Type of the value `T` must be the same as the type the vector knows.
    pub unsafe fn resize_with<T, F>(&mut self, new_len: usize, mut f: F)
    where
        T: 'static,
        F: FnMut() -> T,
    {
        debug_assert!(self.is_type_of(&TypeId::of::<T>()));

        if new_len > self.len() {
            let mut remain = new_len - self.len();

            self.reserve(remain);

            let mut ci = if self.is_empty() {
                self.chunk_offset
            } else {
                let (ci, _) = self.index_2d(self.len() - 1);
                ci
            };

            while remain > 0 {
                let clen = self.chunks[ci].len();
                let partial = cmp::min(self.chunk_len() - clen, remain);
                self.chunks[ci].resize_with(clen + partial, &mut f);

                remain -= partial;
                ci += 1;
            }

            unsafe {
                self.set_len(new_len);
            }
        } else {
            self.truncate(new_len);
        }
    }

    pub fn truncate(&mut self, len: usize) {
        if len >= self.len() {
            return;
        }

        let mut remain = self.len() - len;
        while remain > 0 {
            let (ci, _) = self.index_2d(self.len() - 1);
            let clen = self.chunks[ci].len();
            let partial = cmp::min(clen, remain);
            self.chunks[ci].truncate(clen - partial);

            remain -= partial;
        }

        unsafe {
            self.set_len(len);
        }
    }

    /// Drops redundant chunks such that the capacity is close to the twice the current length.
    /// If the capacity is less than that, this method doens't do anything.
    pub fn shrink(&mut self) {
        if self.is_zst() {
            return;
        }

        let target = self.len() * 2;
        if target > isize::MAX as usize {
            return;
        }

        let target = (target + self.chunk_len() - 1) & !(self.chunk_len() - 1);

        if let Some(redundant) = self.capacity().checked_sub(target) {
            let preserve = self.chunks.len() - redundant / self.chunk_len();
            self.chunks.truncate(preserve);
            self.cap = target;
        }
    }

    /// Creates a new chunk and allocate memory for the chunk.
    fn new_chunk(&self, cap: usize) -> AnyVec {
        let mut chunk = self.chunks[0].clone();
        chunk.reserve_exact(cap);
        chunk
    }

    /// Converts 1D index into 2D index.
    const fn index_2d(&self, index: usize) -> (usize, usize) {
        (
            self.chunk_len.quotient(index) + self.chunk_offset,
            self.chunk_len.remainder(index),
        )
    }

    const fn max_capacity(&self) -> usize {
        self.max_cap
    }

    #[inline]
    unsafe fn fn_iter(this: NonNull<u8>, chunk_idx: usize) -> RawIter {
        let this = this.cast::<ChunkAnyVec>().as_ref();
        if let Some(chunk) = this.get_chunk(chunk_idx) {
            chunk.iter()
        } else {
            RawIter::empty()
        }
    }

    #[inline]
    unsafe fn fn_find(this: NonNull<u8>, item_idx: usize) -> (RawIter, usize, usize) {
        let this = this.cast::<ChunkAnyVec>().as_ref();
        let (ci, ii) = this.index_2d(item_idx);
        let iter = if let Some(chunk) = this.chunks.get(ci) {
            chunk.iter()
        } else {
            RawIter::empty()
        };
        (iter, ci - this.chunk_offset, ii)
    }
}

impl AsFlatRawIter for ChunkAnyVec {
    /// # Safety
    ///
    /// Returned itereator is not bounded by lifetime.
    /// But it actually relies on `&self`, so it must be used as if
    /// it's borrowed.
    unsafe fn iter(&self) -> FlatRawIter {
        // Safety: Infallible.
        let this = unsafe {
            let ptr = self as *const _ as *const u8;
            NonNull::new_unchecked(ptr.cast_mut())
        };

        let chunks = self.chunk_count();
        let stride = self.item_size().max(self.align());

        FlatRawIter::new(
            this,
            chunks,
            Self::fn_iter,
            Self::fn_find,
            stride,
            self.len(),
        )
    }

    /// # Safety
    ///
    /// Returned itereator is not bounded by lifetime.
    /// But it actually relies on `&self`, so it must be used as if
    /// it's borrowed.
    unsafe fn par_iter(&self) -> ParFlatRawIter {
        ParFlatRawIter(self.iter())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tinfo;

    #[test]
    fn test_chunkanyvec_push_pop() {
        // Safety: Type is correct.
        unsafe {
            let chunk_num = 4 * 4;
            let chunk_len = 2;
            let mut v = ChunkAnyVec::new(tinfo!(usize), chunk_len);
            for r in 0..chunk_num {
                for c in 0..chunk_len {
                    let index = r * chunk_len + c;
                    let value = r * chunk_len + c;

                    v.push::<usize>(value);

                    assert_eq!(index + 1, v.len());
                    assert_eq!(Some(&value), v.get(index));
                    assert_eq!((r + 1) * chunk_len, v.capacity());
                }
            }

            assert_eq!(chunk_num * chunk_len, v.capacity());
            assert_eq!(
                v.capacity(),
                v.chunks.iter().map(|chunk| chunk.capacity()).sum::<usize>()
            );

            for r in (chunk_num / 4..chunk_num).rev() {
                for c in (0..chunk_len).rev() {
                    let value = r * chunk_len + c;
                    assert_eq!(Some(value), v.pop());
                }
            }

            v.shrink();
            assert_eq!(chunk_num / 4 * chunk_len, v.len());
            assert_eq!(v.len() * 2, v.capacity());
        }
    }

    #[test]
    fn test_chunkanyvec_swapremove() {
        // Safety: Type is correct.
        unsafe {
            let chunk_size = 8;
            let item_num = 13;
            let chunk_num = (item_num as f32 / chunk_size as f32).ceil() as usize;
            let mut v = ChunkAnyVec::new(tinfo!(i32), chunk_size);
            let mut expect = vec![];
            for i in 0..item_num {
                v.push(i);
                expect.push(i);
            }

            enum Pos {
                Start,
                Middle,
                End,
            }

            let mut pos = Pos::Start;
            for _i in 0..item_num {
                let index = match pos {
                    Pos::Start => {
                        pos = Pos::Middle;
                        0
                    }
                    Pos::Middle => {
                        pos = Pos::End;
                        v.len() / 2
                    }
                    Pos::End => {
                        pos = Pos::Start;
                        v.len() - 1
                    }
                };
                let expect_val = expect.swap_remove(index);
                let popped_val: i32 = v.swap_remove(index);

                assert_eq!(expect_val, popped_val);
                assert_eq!(expect.len(), v.len());
                for j in 0..v.len() {
                    assert_eq!(expect.get(j), v.get(j));
                }
            }

            assert!(v.is_empty());
            assert_eq!(chunk_num * chunk_size, v.capacity());
        }
    }

    #[test]
    fn test_chunkanyvec_resize() {
        // Safety: Type is correct.
        unsafe {
            // Tests resize().
            let mut v = ChunkAnyVec::new(tinfo!(i32), 8);
            assert!(v.is_empty());

            // Resizes the vector.
            v.resize(20, 42);
            assert_eq!(v.len(), 20);
            for val in v.iter_of::<i32>() {
                assert_eq!(*val, 42);
            }

            // Tests resize_raw().
            #[derive(Clone)]
            struct Val(String);

            let mut v = ChunkAnyVec::new(crate::tinfo!(Val), 8);

            let val = Val("42".to_owned());
            let val_ptr = NonNull::new(&val as *const Val as *mut Val as *mut u8).unwrap();
            v.resize_raw(20, val_ptr);
            assert_eq!(v.len(), 20);

            for val in v.iter_of::<Val>() {
                assert_eq!(val.0.as_str(), "42");
            }
        }
    }

    #[test]
    fn test_chunkanyvec_iter() {
        // Safety: Type is correct.
        unsafe {
            const CHUNK_SIZES: &[usize] = &[1, 2, 4, 8];
            const CHUNK_COUNTS: &[usize] = &[1, 2, 3];

            for chunk_size in CHUNK_SIZES {
                for chunk_count in CHUNK_COUNTS {
                    let mut v = ChunkAnyVec::new(tinfo!(i32), *chunk_size);
                    let num = (chunk_size * chunk_count) as i32;
                    for i in 0..num {
                        v.push(i);
                    }

                    test_iter(
                        v.iter_of::<i32>(),
                        (0..num).into_iter(),
                        |l: &i32, r: i32| *l == r,
                    );
                }
            }
        }
    }

    #[test]
    fn test_chunkanyvec_iter_split() {
        use rayon::iter::plumbing::Producer;
        use std::collections::VecDeque;
        use std::ops::Range;

        // Safety: Type is correct.
        unsafe {
            const CHUNK_COUNTS: &[usize] = &[1, 2, 3];
            const CHUNK_SIZES: &[usize] = &[1, 2, 4, 8];
            const TARGET_CHUNK_LENS: &[usize] = &[1, 2, 3, 4];

            for chunk_size in CHUNK_SIZES {
                for chunk_count in CHUNK_COUNTS {
                    let mut v = ChunkAnyVec::new(tinfo!(i32), *chunk_size);
                    let num = (chunk_size * chunk_count) as i32;
                    for i in 0..num {
                        v.push(i);
                    }

                    let iter = v.par_iter_of::<i32>();
                    let mut pieces = VecDeque::new();
                    pieces.push_front((iter, 0..num));

                    for target_chunk_len in TARGET_CHUNK_LENS {
                        inner(pieces.clone(), *target_chunk_len);
                    }
                }
            }
        }

        fn inner(mut pieces: VecDeque<(ParFlatIter<i32>, Range<i32>)>, target_chunk_len: usize) {
            loop {
                let (piece, range) = pieces.pop_front().unwrap();
                if piece.len() <= target_chunk_len {
                    break;
                }

                let mid = piece.len() / 2;
                let (lpiece, rpiece) = piece.split_at(mid);

                let mid = (range.start + range.end) / 2;
                let lrange = range.start..mid;
                let rrange = mid..range.end;

                if lpiece.len() <= target_chunk_len {
                    pieces.push_back((lpiece, lrange))
                } else {
                    pieces.push_front((lpiece, lrange));
                }
                if rpiece.len() <= target_chunk_len {
                    pieces.push_back((rpiece, rrange));
                } else {
                    pieces.push_front((rpiece, rrange));
                }
            }

            for (piece, range) in pieces {
                test_iter(piece.into_seq(), range, |l: &i32, r: i32| *l == r)
            }
        }
    }

    #[test]
    fn test_chunkanyvec_zst() {
        // Safety: Type is correct.
        unsafe {
            let chunk_size = 8; // no effect.
            let mut v = ChunkAnyVec::new(crate::tinfo!(()), chunk_size);
            assert!(v.is_empty());
            for i in 1..=chunk_size {
                v.push(());
                assert_eq!(v.len(), i);
            }

            // Not allocated.
            assert_eq!(
                v.get_chunk(0).unwrap().get_ptr(0),
                AnyVec::aligned_dangling(crate::tinfo!(()).align).as_ptr()
            );

            for i in (1..=chunk_size).rev() {
                v.pop_drop();
                assert_eq!(v.len(), i - 1);
            }
        }
    }

    fn test_iter<I, J, II, JI, F>(iter: I, expect: J, eq: F)
    where
        I: Iterator<Item = II> + ExactSizeIterator + DoubleEndedIterator + Clone,
        J: Iterator<Item = JI> + ExactSizeIterator + DoubleEndedIterator + Clone,
        II: std::cmp::PartialEq + std::fmt::Debug,
        JI: std::cmp::PartialEq + std::fmt::Debug,
        F: Fn(II, JI) -> bool,
    {
        // `iter` and `expect` must have the same number of items.
        assert_eq!(iter.len(), expect.len());

        // Traverses forward.
        let mut zipped = iter.clone().zip(expect.clone());
        assert!(zipped.all(|(l, r)| eq(l, r)));

        // Traverses backward.
        let mut zipped = iter.clone().rev().zip(expect.clone().rev());
        assert!(zipped.all(|(l, r)| eq(l, r)));

        // Traverses forward and backward alternately.
        let mut zipped = iter.zip(expect);
        let mut forward = true;
        let n = zipped.len();
        for _ in 0..n {
            let pair = if forward {
                zipped.next()
            } else {
                zipped.next_back()
            };
            let (l, r) = pair.unwrap();
            assert!(eq(l, r));
            forward = !forward;
        }
    }
}
