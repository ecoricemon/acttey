use super::{
    super::{
        raw::{
            AsRawIter, FlatIter, FlatIterMut, FlatRawIter, ParFlatIter, ParFlatIterMut, RawIter,
        },
        types::{FnCloneRaw, FnDropRaw, TypeInfo},
    },
    AnyVec,
};
use crate::{ds::raw::AsFlatRawIter, util::prelude::*};
use std::{
    any::{self, TypeId},
    cmp::{Ord, Ordering},
    mem,
    ptr::{self, NonNull},
};

/// A type-erased vector containing values of the same type.
///
/// This is very similar to [`AnyVec`] except memory management. As the name
/// says, this vector is based on chunks. When you put values and the vector
/// doesn't have sufficient capacity for them, the vector appends chunks and
/// then put the values in the chunks. Therefore, the vector will be more
/// efficient than `AnyVec` when frequent insertion/removal are expected.
/// Note, however, that the iteration performance would be worse than `AnyVec`
/// due to the split chunks in memory.
#[derive(Debug, Clone)]
pub struct ChunkAnyVec {
    /// Chunks.
    ///
    /// The very first chunk plays a role of giving type information without
    /// storing values. Use [`ChunkAnyVec::index_2d`] to convert an index into
    /// chunk and item indices.
    chunks: Vec<AnyVec>,

    /// Maximum length and initial capacity of each chunk.
    ///
    /// The length is limited to be a power of two.
    /// Zero length is considered as `usize::MAX + 1`.
    chunk_cap: PowerOfTwo,

    /// Number of items.
    len: usize,

    /// Offset to the first chunk that contains values.
    ///
    /// If the type is not ZST, the offset is 1 due to the first special chunk.
    /// Otherwise, the offset is 0.
    chunk_offset: usize,

    /// Maximum capacity the vector can grow.
    ///
    /// If the type is not ZST, the maximum is `isize::MAX / self.item_size`.
    /// Otherwise, the maximum is `usize::MAX`.
    max_cap: usize,
}

impl ChunkAnyVec {
    /// Creates a new empty vector.
    ///
    /// The vector will create and append chunks that can contain items as many
    /// as the given chunk capacity. But the type is ZST, chunk capacity is
    /// ignored.
    ///
    /// # Panics
    ///
    /// Panics if the given chunk capacity is not a power of two.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let chunk_cap = 2;
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), chunk_cap);
    /// assert_eq!(v.capacity(), 0);
    ///
    /// unsafe {
    ///     for _ in 0..chunk_cap {
    ///         v.push(0_i32);
    ///         assert_eq!(v.capacity(), chunk_cap);
    ///     }
    ///     v.push(0_i32);
    ///     assert_eq!(v.capacity(), chunk_cap * 2);
    /// }
    /// ```
    pub fn new(tinfo: TypeInfo, chunk_cap: usize) -> Self {
        let mut v = Self {
            chunks: vec![AnyVec::new(tinfo)],
            chunk_cap: PowerOfTwo::new(chunk_cap).unwrap(),
            len: 0,
            max_cap: 0,
            chunk_offset: 1,
        };

        // If ZST, we won't allocate any memory for the vector.
        // But, adjust capacity like `Vec`.
        if v.is_zst() {
            v.chunk_cap = PowerOfTwo::new(0).unwrap();
            v.max_cap = usize::MAX;
            v.chunk_offset = 0;
        } else if v.default_chunk_capacity() > isize::MAX as usize {
            // We need to restrict isize::MAX + 1.
            panic!("`chunk_cap` must be less than isize::MAX");
        } else {
            // Same limitation with `Vec` or `AnyVec`.
            v.max_cap = isize::MAX as usize / v.chunks[0].item_size();
        }

        v
    }

    /// Returns [`TypeInfo`] of the item.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let v = ChunkAnyVec::new(tinfo!(i32), 2);
    /// assert_eq!(v.type_info(), &tinfo!(i32));
    /// ```
    pub fn type_info(&self) -> &TypeInfo {
        self.chunks[0].type_info()
    }

    /// Returns [`TypeId`] of the item.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let v = ChunkAnyVec::new(tinfo!(i32), 2);
    /// assert_eq!(v.type_id(), std::any::TypeId::of::<i32>());
    /// ```
    pub fn type_id(&self) -> TypeId {
        self.chunks[0].type_id()
    }

    /// Returns name of the item based on [`type_name`](any::type_name).
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let v = ChunkAnyVec::new(tinfo!(i32), 2);
    /// println!("{}", v.type_name());
    /// ```
    pub fn type_name(&self) -> &'static str {
        self.chunks[0].type_name()
    }

    /// Returns size in bytes of the item.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let v = ChunkAnyVec::new(tinfo!(i32), 2);
    /// assert_eq!(v.item_size(), std::mem::size_of::<i32>());
    /// ```
    pub fn item_size(&self) -> usize {
        self.chunks[0].item_size()
    }

    /// Returns whether the item is zero-sized type or not.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let v = ChunkAnyVec::new(tinfo!(i32), 2);
    /// assert!(!v.is_zst());
    /// let v = ChunkAnyVec::new(tinfo!(()), 2);
    /// assert!(v.is_zst());
    /// ```
    pub fn is_zst(&self) -> bool {
        self.chunks[0].is_zst()
    }

    /// Returns alignment in bytes of the item.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let v = ChunkAnyVec::new(tinfo!(i32), 2);
    /// assert_eq!(v.align(), std::mem::align_of::<i32>());
    /// ```
    pub fn align(&self) -> usize {
        self.chunks[0].align()
    }

    /// Returns raw drop function pointer.
    pub fn fn_drop(&self) -> FnDropRaw {
        self.chunks[0].fn_drop()
    }

    /// Returns raw clone function pointer.
    pub fn fn_clone(&self) -> FnCloneRaw {
        self.chunks[0].fn_clone()
    }

    /// Returns whether the item is [`Clone`] or not.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let v = ChunkAnyVec::new(tinfo!(i32), 2);
    /// assert!(v.is_clone());
    ///
    /// struct S;
    /// let v = ChunkAnyVec::new(tinfo!(S), 2);
    /// assert!(!v.is_clone());
    /// ```
    pub fn is_clone(&self) -> bool {
        self.chunks[0].is_clone()
    }

    /// Returns whether the item is [`Send`] or not.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let v = ChunkAnyVec::new(tinfo!(i32), 2);
    /// assert!(v.is_send());
    /// // let v = ChunkAnyVec::new(tinfo!(*mut u8), 2); // Disallowed for now.
    /// ```
    pub fn is_send(&self) -> bool {
        self.chunks[0].is_send()
    }

    /// Returns whether the item is [`Sync`] or not.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let v = ChunkAnyVec::new(tinfo!(i32), 2);
    /// assert!(v.is_sync());
    /// // let v = ChunkAnyVec::new(tinfo!(*mut u8), 2); // Disallowed for now.
    /// ```
    pub fn is_sync(&self) -> bool {
        self.chunks[0].is_sync()
    }

    /// Returns true if the given [`TypeId`] is the same as item of the vector.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let v = ChunkAnyVec::new(tinfo!(i32), 2);
    /// assert!(v.is_type_of::<i32>());
    /// ```
    pub fn is_type_of<T: 'static>(&self) -> bool {
        self.chunks[0].is_type_of::<T>()
    }

    /// Returns number of item.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    /// unsafe { v.push(0_i32) };
    /// assert_eq!(v.len(), 1);
    /// ```
    pub const fn len(&self) -> usize {
        self.len
    }

    /// Returns true if the vector is empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let v = ChunkAnyVec::new(tinfo!(i32), 2);
    /// assert!(v.is_empty());
    /// ```
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns capacity in bytes of the vector.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    /// v.reserve(10);
    /// assert!(v.capacity() >= 10);
    /// ```
    pub fn capacity(&self) -> usize {
        if self.is_zst() {
            self.max_cap
        } else {
            // Chunk always has the fixed capacity.
            self.num_chunks() * self.default_chunk_capacity()
        }
    }

    /// Returns default chunk capacity, which is a power of two.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let chunk_cap = 2;
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), chunk_cap);
    /// assert_eq!(v.default_chunk_capacity(), chunk_cap);
    /// ```
    pub const fn default_chunk_capacity(&self) -> usize {
        self.chunk_cap.get()
    }

    /// Returns number of chunks.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    /// assert_eq!(v.num_chunks(), 0);
    ///
    /// unsafe {
    ///     v.push(0_i32);
    ///     v.push(1_i32);
    ///     assert_eq!(v.num_chunks(), 1);
    ///     v.push(2_i32);
    ///     assert_eq!(v.num_chunks(), 2);
    /// }
    /// ```
    pub fn num_chunks(&self) -> usize {
        self.chunks.len() - self.chunk_offset
    }

    /// Returns shared reference to a chunk at the given chunk index.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let chunk_cap = 2;
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), chunk_cap);
    ///
    /// unsafe {
    ///     v.push(0_i32);
    ///     v.push(1_i32);
    ///     v.push(2_i32);
    /// }
    ///
    /// let first_chunk = v.get_chunk(0).unwrap();
    /// let first_slice = unsafe { first_chunk.as_slice::<i32>() };
    /// assert_eq!(first_slice, &[0, 1]);
    /// ```
    pub fn get_chunk(&self, chunk_index: usize) -> Option<&AnyVec> {
        self.chunks.get(chunk_index + self.chunk_offset)
    }

    /// Returns iterator visiting all items.
    ///
    /// # Panics
    ///
    /// Panics if the given type is not the same as the one vector contains.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    /// unsafe {
    ///     v.push(0_i32);
    ///     v.push(1_i32);
    /// }
    /// for x in v.iter_of::<i32>() {
    ///     println!("{x}");
    /// }
    /// ```
    pub fn iter_of<T: 'static>(&self) -> FlatIter<'_, T> {
        assert!(
            self.is_type_of::<T>(),
            "expected `{}`, but received `{}`",
            self.type_name(),
            any::type_name::<T>()
        );

        // Safety: Vector contains type `T` data in it.
        unsafe { AsFlatRawIter::iter_of(self) }
    }

    /// Returns mutable iterator visiting all items.
    ///
    /// # Panics
    ///
    /// Panics if the given type is not the same as the vector contains.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    /// unsafe {
    ///     v.push(0_i32);
    ///     v.push(1_i32);
    /// }
    /// for x in v.iter_mut_of::<i32>() {
    ///     *x += 10;
    /// }
    /// ```
    pub fn iter_mut_of<T: 'static>(&mut self) -> FlatIterMut<'_, T> {
        assert!(
            self.is_type_of::<T>(),
            "expected `{}`, but received `{}`",
            self.type_name(),
            any::type_name::<T>()
        );

        // Safety: Vector contains type `T` data in it.
        unsafe { AsFlatRawIter::iter_mut_of(self) }
    }

    /// Returns parallel iterator visiting all items.
    ///
    /// Parallel iterator implements [`rayon`]'s parallel iterator traits, so
    /// that it can be split into multiple CPU cores then consumed at the same
    /// time.
    ///
    /// # Panics
    ///
    /// Panics if the given type is not the same as the vector contains.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    /// use my_ecs::ds::ChunkAnyVec;
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    /// unsafe {
    ///     v.push(1_i32);
    ///     v.push(2_i32);
    ///     v.push(3_i32);
    /// }
    /// let sum: i32 = v.par_iter_of::<i32>().sum();
    /// assert_eq!(sum, 6);
    /// ```
    pub fn par_iter_of<T: 'static>(&self) -> ParFlatIter<'_, T> {
        assert!(
            self.is_type_of::<T>(),
            "expected `{}`, but received `{}`",
            self.type_name(),
            any::type_name::<T>()
        );

        // Safety: Vector contains type `T` data in it.
        unsafe { AsFlatRawIter::par_iter_of(self) }
    }

    /// Returns mutable parallel iterator visiting all items.
    ///
    /// Parallel iterator implements [`rayon`]'s parallel iterator traits, so
    /// that it can be split into multiple CPU cores then consumed at the same
    /// time.
    ///
    /// # Panics
    ///
    /// Panics if the given type is not the same as the vector contains.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    /// use my_ecs::ds::ChunkAnyVec;
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    /// unsafe {
    ///     v.push(1_i32);
    ///     v.push(2_i32);
    ///     v.push(3_i32);
    /// }
    /// let sum: i32 = v.par_iter_mut_of::<i32>().map(|x| *x + 1).sum();
    /// assert_eq!(sum, 9);
    /// ```
    pub fn par_iter_mut_of<T: 'static>(&mut self) -> ParFlatIterMut<'_, T> {
        assert!(
            self.is_type_of::<T>(),
            "expected `{}`, but received `{}`",
            self.type_name(),
            any::type_name::<T>()
        );

        // Safety: Vector contains type `T` data in it.
        unsafe { AsFlatRawIter::par_iter_mut_of(self) }
    }

    /// Reserves additional capacity more than or equal to the given value.
    ///
    /// If capacity of the vector is already sufficient, nothing takes place.
    /// Otherwise, allocates chunks and append them to the vector so that the
    /// capacity will be greater than or equal to `self.len() + additional`.
    /// Unlike [`AnyVec::reserve`], this method appends chunks as little as
    /// possible.
    ///
    /// # Panics
    ///
    /// Panics if total memory in bytes after calling this method will exceed
    /// [`isize::MAX`].
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    /// v.reserve(10);
    /// assert!(v.capacity() >= 10);
    /// ```
    pub fn reserve(&mut self, additional: usize) {
        self.reserve_exact(additional);
    }

    /// This method is the same as [`ChunkAnyVec::reserve`].
    //
    // For consistency.
    pub fn reserve_exact(&mut self, additional: usize) {
        let new_cap = self.len.saturating_add(additional);

        if self.capacity() >= new_cap {
            return;
        }
        if new_cap > self.max_capacity() {
            panic!("can't allocate {new_cap} x {} bytes", self.item_size());
        }

        // Rounds up the target capacity to be a multiple of chunk length.
        // This operation doesn't overflow because cap and len are restricted.
        let new_cap =
            (new_cap + self.default_chunk_capacity() - 1) & !(self.default_chunk_capacity() - 1);

        // If the new_cap is clamped by this operation,
        // the last chunk length will be the same as the others' - 1.
        let new_cap = new_cap.min(self.max_capacity());

        let mut remain = (new_cap - self.capacity()) as isize;
        while remain > 0 {
            self.chunks.push(self.create_chunk());
            remain -= self.default_chunk_capacity() as isize; // Fixed cap
        }
    }

    /// Shrinks capacity of the vector as much as possible.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let chunk_cap = 4;
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), chunk_cap);
    /// assert_eq!(v.capacity(), 0);
    ///
    /// unsafe { v.push(1_i32) };
    /// assert_eq!(v.capacity(), chunk_cap);
    ///
    /// v.reserve(chunk_cap);
    /// assert_eq!(v.capacity(), chunk_cap * 2);
    ///
    /// v.shrink_to_fit();
    /// assert_eq!(v.capacity(), chunk_cap);
    ///
    /// v.pop_drop();
    /// v.shrink_to_fit();
    /// assert_eq!(v.capacity(), 0);
    /// ```
    pub fn shrink_to_fit(&mut self) {
        if self.is_zst() {
            return;
        }

        let redundant_cap = self.capacity() - self.len();
        let redundant_chunks = redundant_cap / self.default_chunk_capacity();
        self.chunks.truncate(self.chunks.len() - redundant_chunks);
    }

    /// Sets length of the vector to the given value without any additional
    /// operations.
    ///
    /// # Safety
    ///
    /// - `new_len` must be less than or equal to `self.capacity()`.
    /// - If `new_len` is greater than the previous length, caller must
    ///   initialize the extended range with proper values.
    /// - If `new_len` is less than the previous length, caller must drop
    ///   abandoned values from the vector properly.
    pub unsafe fn set_len(&mut self, new_len: usize) {
        debug_assert!(new_len <= self.capacity());

        self.len = new_len;
    }

    /// Copies data as much as `self.item_size()` from `src` address to the end
    /// of the vector.
    ///
    /// If the vector doesn't have sufficient capacity for the appended value,
    /// then the vector increases its capacity first by calling
    /// [`ChunkAnyVec::reserve`] which allocates memory more than just one value,
    /// then does the copy.
    ///
    /// # Safety
    ///
    /// - `src` must point to a valid type that the vector contains.
    /// - `src` must not be dropped after calling this method because it is
    ///   moved into the vector.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    /// use std::ptr::NonNull;
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    ///
    /// let value = 0x01020304_i32;
    /// unsafe {
    ///     let ptr = (&value as *const i32 as *const u8).cast_mut();
    ///     v.push_raw(NonNull::new(ptr).unwrap());
    ///     assert_eq!(v.pop(), Some(value));
    /// }
    /// ```
    pub unsafe fn push_raw(&mut self, ptr: NonNull<u8>) {
        self.reserve(1);

        let (ci, _) = self.index_2d(self.len());

        unsafe {
            self.chunks[ci].push_raw(ptr);
            // If ZST, new length can overflow.
            let new_len = self.len().checked_add(1).unwrap();
            self.set_len(new_len);
        }
    }

    /// Writes a value to the end of the vector by calling the given function.
    ///
    /// If the vector doesn't have sufficient capacity for the appended value,
    /// then the vector increases its capacity first by calling
    /// [`ChunkAnyVec::reserve`] which allocates a chunk, then does the write
    /// operation.
    ///
    /// # Safety
    ///
    /// - `write` must write a valid type that the vector contains.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    /// use std::ptr::{self, NonNull};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    ///
    /// let value = 0x01020304_i32;
    /// unsafe {
    ///     v.push_with(|dst| {
    ///         ptr::write(dst as *mut i32, value);
    ///     });
    ///     assert_eq!(v.pop(), Some(value));
    /// }
    /// ```
    pub unsafe fn push_with<F>(&mut self, write: F)
    where
        F: FnOnce(*mut u8),
    {
        self.reserve(1);

        let (ci, _) = self.index_2d(self.len());

        unsafe {
            self.chunks[ci].push_with(write);
            // If ZST, new length can overflow.
            let new_len = self.len().checked_add(1).unwrap();
            self.set_len(new_len);
        }
    }

    /// Appends the given value to the end of the vector.
    ///
    /// # Safety
    ///
    /// - `value` must be the same type as the vector contains.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    /// unsafe {
    ///     v.push(0_i32);
    ///     assert_eq!(v.pop(), Some(0_i32));
    /// }
    /// ```
    pub unsafe fn push<T: 'static>(&mut self, mut value: T) {
        debug_assert!(
            self.is_type_of::<T>(),
            "expected `{}`, but received `{}`",
            self.type_name(),
            any::type_name::<T>()
        );

        unsafe {
            let ptr = NonNull::new_unchecked(&mut value as *mut T as *mut u8);
            self.push_raw(ptr);
        }
        mem::forget(value);
    }

    /// Removes the last item from the vector and writes it to the given
    /// buffer, then returns `Some`.
    ///
    /// If removing is successful, caller becomes to own the item in the
    /// buffer, so that caller must call `drop()` on it correctly.
    /// Otherwise, returns `None` without change to the buffer.
    ///
    /// # Safety
    ///
    /// Undefined behavior if conditions below are not met.
    /// - `buf` must have enough size to be copied an item.
    /// - When `Some` is returned, `buf` must be correctly handled as an item.
    ///   For example, if an item should be dropped, caller must call drop() on
    ///   it.
    /// - When `None` is returned, `buf` must be correctly handled as it was.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let chunk_cap = 4;
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), chunk_cap);
    /// unsafe { v.push(42_i32) };
    /// assert_eq!(v.len(), 1);
    ///
    /// let mut buf = 0_i32;
    /// let res = unsafe { v.pop_raw(&mut buf as *mut i32 as *mut u8) };
    /// assert!(res.is_some());
    /// assert!(v.is_empty());
    /// assert_eq!(buf, 42);
    /// ```
    pub unsafe fn pop_raw(&mut self, buf: *mut u8) -> Option<()> {
        if self.is_empty() {
            return None;
        }

        // Safety:
        // - The vector must not be empty: Ok.
        // - After calling `reduce_len_then_get_last_chunk`, caller must remove
        //   an item from the returned chunk: Ok.
        unsafe {
            let last_chunk = self.reduce_len_then_get_last_chunk();
            last_chunk.pop_raw(buf);
        }
        Some(())
    }

    /// Removes the last item from the vector.
    ///
    /// # Safety
    ///
    /// - `T` must be the same type as the vector contains.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    ///
    /// unsafe {
    ///     v.push(0_i32);
    ///     let value = v.pop::<i32>().unwrap();
    ///     assert_eq!(value, 0_i32);
    /// }
    /// ```
    pub unsafe fn pop<T: 'static>(&mut self) -> Option<T> {
        if self.is_empty() {
            return None;
        }

        // Safety:
        // - The vector must not be empty: Ok.
        // - After calling `reduce_len_then_get_last_chunk`, caller must remove
        //   an item from the returned chunk: Ok.
        unsafe {
            let last_chunk = self.reduce_len_then_get_last_chunk();
            last_chunk.pop()
        }
    }

    /// Removes the last item from the vector then drops it immediately.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    ///
    /// unsafe {
    ///     v.push(0_i32);
    ///     assert_eq!(v.pop_drop(), Some(()));
    /// }
    /// ```
    pub fn pop_drop(&mut self) -> Option<()> {
        if self.is_empty() {
            return None;
        }

        // Safety:
        // - The vector must not be empty: Ok.
        // - After calling `reduce_len_then_get_last_chunk`, caller must remove
        //   an item from the returned chunk: Ok.
        let last_chunk = unsafe { self.reduce_len_then_get_last_chunk() };
        last_chunk.pop_drop()
    }

    /// Removes the last item from the vector without calling drop function on
    /// it.
    ///
    /// # Safety
    ///
    /// Rust safety doesn't include calling drop function. See
    /// [`forget`](mem::forget) for more information. However, caller must
    /// guarantee that not calling drop function is fine for the item.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    ///
    /// unsafe {
    ///     v.push(0_i32);
    ///     assert_eq!(v.pop_forget(), Some(()));
    /// }
    /// ```
    pub fn pop_forget(&mut self) -> Option<()> {
        if self.is_empty() {
            return None;
        }

        // Safety:
        // - The vector must not be empty: Ok.
        // - After calling `reduce_len_then_get_last_chunk`, caller must remove
        //   an item from the returned chunk: Ok.
        let last_chunk = unsafe { self.reduce_len_then_get_last_chunk() };
        last_chunk.pop_forget()
    }

    /// Reduces length by 1 and returns mutable reference to the last chunk.
    ///
    /// # Safety
    ///
    /// - The vector must not be empty.
    /// - Caller must remove an item from the returned chunk.
    unsafe fn reduce_len_then_get_last_chunk(&mut self) -> &mut AnyVec {
        // Safety: Decreasing is safe.
        unsafe { self.set_len(self.len() - 1) };

        let (ci, _) = self.index_2d(self.len());

        // Safety: `r` is valid.
        unsafe { self.chunks.get_unchecked_mut(ci) }
    }

    /// Removes an item at the given index from the vector and writes it to the
    /// given buffer.
    ///
    /// Therefore, the item is actually moved from the vector to the given
    /// buffer. So caller must take care of calling drop on it.
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
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
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
    pub unsafe fn swap_remove_raw(&mut self, index: usize, buf: *mut u8) {
        // If index is out of bounds or len() - 1 overflows, swap() panics.
        self.swap(index, self.len().wrapping_sub(1));
        unsafe { self.pop_raw(buf) };
    }

    /// Removes an item at the given index from the vector and returns it.
    ///
    /// Then the last item of the vector is moved to the vacant slot.
    ///
    /// # Panics
    ///
    /// Panics if the given index is out of bounds.
    ///
    /// # Safety
    ///
    /// - `T` must be the same type as the vector contains.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    ///
    /// unsafe {
    ///     v.push(0_i32);
    ///     v.push(1_i32);
    ///     v.push(2_i32);
    ///     assert_eq!(v.swap_remove::<i32>(0), 0);
    ///     assert_eq!(v.swap_remove::<i32>(0), 2);
    ///     assert_eq!(v.swap_remove::<i32>(0), 1);
    /// }
    /// ```
    pub unsafe fn swap_remove<T: 'static>(&mut self, index: usize) -> T {
        // If index is out of bounds or len() - 1 overflows, swap() panics.
        self.swap(index, self.len().wrapping_sub(1));
        unsafe { self.pop().unwrap() }
    }

    /// Removes an item at the given index from the vector and drops it
    /// immediately.
    ///
    /// Then the last item of the vector is moved to the vacant slot.
    ///
    /// # Panics
    ///
    /// Panics if the given index is out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    ///
    /// unsafe {
    ///     v.push(0_i32);
    ///     v.swap_remove_drop(0);
    ///     assert!(v.is_empty());
    /// }
    /// ```
    pub fn swap_remove_drop(&mut self, index: usize) {
        // If index is out of bounds or len() - 1 overflows, swap() panics.
        self.swap(index, self.len().wrapping_sub(1));
        self.pop_drop();
    }

    /// Removes an item at the given index from the vector without calling drop
    /// function on it.
    ///
    /// Then the last item of the vector is moved to the vacant slot.
    ///
    /// # Panics
    ///
    /// Panics if given index is out of bounds.
    ///
    /// # Safety
    ///
    /// Rust safety doesn't include calling drop function. See
    /// [`forget`](mem::forget) for more information. However, caller must
    /// guarantee that not calling drop function is fine for the item.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    ///
    /// unsafe {
    ///     v.push(0_i32);
    ///     v.swap_remove_forget(0);
    ///     assert!(v.is_empty());
    /// }
    /// ```
    pub fn swap_remove_forget(&mut self, index: usize) {
        // If index is out of bounds or len() - 1 overflows, swap() panics.
        self.swap(index, self.len().wrapping_sub(1));
        self.pop_forget();
    }

    /// Replaces an item with another item in the vector.
    ///
    /// # Panics
    ///
    /// Panics if any given indices is out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    ///
    /// unsafe {
    ///     v.push(0_i32);
    ///     v.push(1_i32);
    ///     v.swap(0, 1);
    ///     assert_eq!(v.pop(), Some(0));
    ///     assert_eq!(v.pop(), Some(1));
    /// }
    /// ```
    pub fn swap(&mut self, index0: usize, index1: usize) {
        assert!(index0 < self.len(), "{index0} is out of bounds");
        assert!(index1 < self.len(), "{index1} is out of bounds");

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

    /// Returns a pointer to an item at the given index.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    ///
    /// let value = 0x01020304_i32;
    /// unsafe { v.push(value) };
    ///
    /// let ptr = v.get_raw(0).unwrap().cast::<i32>().as_ptr().cast_const();
    /// unsafe {
    ///     assert_eq!(std::ptr::read(ptr), value);
    /// }
    /// ```
    pub fn get_raw(&self, index: usize) -> Option<NonNull<u8>> {
        if index < self.len() {
            unsafe { Some(self.get_raw_unchecked(index)) }
        } else {
            None
        }
    }

    /// Returns a pointer to an item at the given index.
    ///
    /// # Safety
    ///
    /// - Index must be in bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    ///
    /// let value = 0x01020304_i32;
    /// unsafe {
    ///     v.push(value);
    ///     let ptr = v.get_raw_unchecked(0)
    ///         .cast::<i32>()
    ///         .as_ptr()
    ///         .cast_const();
    ///     assert_eq!(std::ptr::read(ptr), value);
    /// }
    /// ```
    pub unsafe fn get_raw_unchecked(&self, index: usize) -> NonNull<u8> {
        let (ci, ii) = self.index_2d(index);
        unsafe { self.chunks[ci].get_raw_unchecked(ii) }
    }

    /// Returns shared reference to an item at the given index.
    ///
    /// # Safety
    ///
    /// - `T` must be the same type as the vector contains.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    ///
    /// unsafe {
    ///     v.push(0_i32);
    ///     assert_eq!(v.get(0), Some(&0_i32));
    /// }
    /// ```
    pub unsafe fn get<T: 'static>(&self, index: usize) -> Option<&T> {
        if index < self.len() {
            let (ci, ii) = self.index_2d(index);
            // Type is checked here.
            unsafe { self.chunks[ci].get(ii) }
        } else {
            None
        }
    }

    /// Returns mutable reference to an item at the given index.
    ///
    /// # Safety
    ///
    /// - `T` must be the same type as the vector contains.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    ///
    /// unsafe {
    ///     v.push(0_i32);
    ///     *v.get_mut(0).unwrap() = 1_i32;
    ///     assert_eq!(v.get(0), Some(&1_i32));
    /// }
    /// ```
    pub unsafe fn get_mut<T: 'static>(&mut self, index: usize) -> Option<&mut T> {
        if index < self.len() {
            let (ci, ii) = self.index_2d(index);
            // Type is checked here.
            unsafe { self.chunks[ci].get_mut(ii) }
        } else {
            None
        }
    }

    /// Resizes the vector to the given length.
    ///
    /// If the new length is greater than previous length of the vector, then
    /// the vector is extended with the given value. Otherwise, the vector is
    /// shrunk.
    ///
    /// # Panics
    ///
    /// Panics if `T` is not the same type as the vector contains.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    ///
    /// unsafe {
    ///     v.resize(2, 0_i32);
    ///     assert_eq!(v.pop(), Some(0_i32));
    ///     assert_eq!(v.pop(), Some(0_i32));
    ///     assert!(v.is_empty());
    /// }
    /// ```
    pub fn resize<T>(&mut self, new_len: usize, value: T)
    where
        T: Clone + 'static,
    {
        // Panic: `self.resize_with` panics if the given type `T` is incorrect.
        self.resize_with(new_len, || value.clone())
    }

    /// Resizes the vector to the given length.
    ///
    /// If the new length is greater than previous length of the vector, then
    /// the vector is extended with clones of a value pointed by the given
    /// pointer. Otherwise, the vector is shrunk.
    ///
    /// # Panics
    ///
    /// Panics if vector item is not [`Clone`].
    ///
    /// # Safety
    ///
    /// - `val_ptr` must point to a valid type that the vector contains.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    /// use std::ptr::NonNull;
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    ///
    /// let value = 0x01020304_i32;
    /// unsafe {
    ///     let ptr = (&value as *const i32 as *const u8).cast_mut();
    ///     v.resize_raw(2, NonNull::new(ptr).unwrap());
    ///     assert_eq!(v.pop(), Some(value));
    ///     assert_eq!(v.pop(), Some(value));
    ///     assert!(v.is_empty());
    /// }
    /// ```
    pub unsafe fn resize_raw(&mut self, new_len: usize, val_ptr: NonNull<u8>) {
        assert!(self.is_clone(), "expected `Clone` type");

        unsafe {
            self._resize(new_len, |last_chunk, new_last_chunk_len| {
                last_chunk.resize_raw(new_last_chunk_len, val_ptr);
            });
        }
    }

    /// Resizes the vector to the given length.
    ///
    /// If the new length is greater than previous length of the vector, then
    /// the vector is extended with values the given function generates. In this
    /// case, generated values are appended in order. Otherwise, the vector is
    /// shrunk.
    ///
    /// # Panics
    ///
    /// Panics if `T` is not the same type as the vector contains.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    ///
    /// unsafe {
    ///     v.resize_with(2, || 0_i32);
    ///     assert_eq!(v.pop(), Some(0_i32));
    ///     assert_eq!(v.pop(), Some(0_i32));
    ///     assert!(v.is_empty());
    /// }
    /// ```
    pub fn resize_with<T, F>(&mut self, new_len: usize, mut f: F)
    where
        T: 'static,
        F: FnMut() -> T,
    {
        assert!(
            self.is_type_of::<T>(),
            "expected `{}`, but received `{}`",
            self.type_name(),
            any::type_name::<T>()
        );

        // Safety: Closure must fill the chunk as much as the given length: Ok.
        unsafe {
            self._resize(new_len, move |last_chunk, new_last_chunk_len| {
                last_chunk.resize_with(new_last_chunk_len, &mut f)
            });
        }
    }

    /// # Safety
    ///
    /// `fill_chunk` function must make the given chunk to be the given length.
    unsafe fn _resize<F>(&mut self, new_len: usize, mut fill_chunk: F)
    where
        F: FnMut(&mut AnyVec, usize), // Last chunk, New last chunk length
    {
        match new_len.cmp(&self.len()) {
            Ordering::Less => self.truncate(new_len),
            Ordering::Equal => {}
            Ordering::Greater => {
                let mut remain = new_len - self.len();

                self.reserve(remain);

                let last_index = self.len().saturating_sub(1);
                let (mut ci, _) = self.index_2d(last_index);
                let chunk_cap = self.default_chunk_capacity();

                while remain > 0 {
                    let last_chunk = &mut self.chunks[ci];
                    let partial = remain.min(chunk_cap - last_chunk.len());
                    let new_last_chunk_len = last_chunk.len() + partial;

                    fill_chunk(last_chunk, new_last_chunk_len);
                    debug_assert_eq!(last_chunk.len(), new_last_chunk_len);

                    remain -= partial;
                    ci += 1;
                }

                unsafe { self.set_len(new_len) };
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
    /// use my_ecs::{tinfo, ds::ChunkAnyVec};
    ///
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), 2);
    ///
    /// unsafe { v.resize(10, 0_i32) };
    /// v.truncate(5);
    /// assert_eq!(v.len(), 5);
    /// ```
    pub fn truncate(&mut self, len: usize) {
        if len >= self.len() {
            return;
        }

        let mut cur_len = self.len();

        while cur_len > len {
            // `len > 0` because of early exit above, so `cur_len > 1`.
            let (ci, _) = self.index_2d(cur_len - 1);
            let last_chunk_len = self.chunks[ci].len();
            let partial = last_chunk_len.min(cur_len - len);

            // We do not drop chunk itself to avoid frequent allocations. Just
            // eliminates items in the chunk only.
            self.chunks[ci].truncate(last_chunk_len - partial);

            cur_len -= partial;
        }

        unsafe { self.set_len(cur_len) };
    }

    /// Creates a new chunk.
    ///
    /// Note that chunk always has the fixed capacity.
    fn create_chunk(&self) -> AnyVec {
        let mut chunk = self.chunks[0].clone();
        chunk.reserve_exact(self.default_chunk_capacity());
        chunk
    }

    /// Converts 1D index into 2D index.
    const fn index_2d(&self, index: usize) -> (usize, usize) {
        (
            self.chunk_cap.quotient(index) + self.chunk_offset,
            self.chunk_cap.remainder(index),
        )
    }

    const fn max_capacity(&self) -> usize {
        self.max_cap
    }

    #[inline]
    unsafe fn fn_iter(this: NonNull<u8>, chunk_idx: usize) -> RawIter {
        let this = unsafe { this.cast::<ChunkAnyVec>().as_ref() };
        if let Some(chunk) = this.get_chunk(chunk_idx) {
            chunk.iter()
        } else {
            RawIter::empty()
        }
    }

    #[inline]
    unsafe fn fn_find(this: NonNull<u8>, item_idx: usize) -> (RawIter, usize, usize) {
        let this = unsafe { this.cast::<ChunkAnyVec>().as_ref() };
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
    /// Returned iterator is not bounded by lifetime.
    /// But it actually relies on `&self`, so it must be used as if
    /// it's borrowed.
    unsafe fn iter(&self) -> FlatRawIter {
        // Safety: Infallible.
        let this = unsafe {
            let ptr = self as *const _ as *const u8;
            NonNull::new_unchecked(ptr.cast_mut())
        };

        let chunks = self.num_chunks();
        let stride = self.item_size().max(self.align());

        unsafe {
            FlatRawIter::new(
                this,
                chunks,
                Self::fn_iter,
                Self::fn_find,
                stride,
                self.len(),
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tinfo;

    #[test]
    fn test_chunk_any_vec_push_pop() {
        // Safety: Type is correct.
        unsafe {
            let chunk_num = 4 * 4;
            let chunk_cap = 2;
            let mut v = ChunkAnyVec::new(tinfo!(usize), chunk_cap);
            for r in 0..chunk_num {
                for c in 0..chunk_cap {
                    let index = r * chunk_cap + c;
                    let value = r * chunk_cap + c;

                    v.push::<usize>(value);

                    assert_eq!(index + 1, v.len());
                    assert_eq!(Some(&value), v.get(index));
                    assert_eq!((r + 1) * chunk_cap, v.capacity());
                }
            }

            assert_eq!(chunk_num * chunk_cap, v.capacity());
            assert_eq!(
                v.capacity(),
                v.chunks.iter().map(|chunk| chunk.capacity()).sum::<usize>()
            );

            for r in (chunk_num / 4..chunk_num).rev() {
                for c in (0..chunk_cap).rev() {
                    let value = r * chunk_cap + c;
                    assert_eq!(Some(value), v.pop());
                }
            }

            assert_eq!(chunk_num / 4 * chunk_cap, v.len());
        }
    }

    #[test]
    fn test_chunk_any_vec_swap_remove() {
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
    fn test_chunk_any_vec_resize() {
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

            let mut v = ChunkAnyVec::new(tinfo!(Val), 8);

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
    fn test_chunk_any_vec_iter() {
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
    fn test_chunk_any_vec_iter_split() {
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
                let (l_piece, r_piece) = piece.split_at(mid);

                let mid = (range.start + range.end) / 2;
                let l_range = range.start..mid;
                let r_range = mid..range.end;

                if l_piece.len() <= target_chunk_len {
                    pieces.push_back((l_piece, l_range))
                } else {
                    pieces.push_front((l_piece, l_range));
                }
                if r_piece.len() <= target_chunk_len {
                    pieces.push_back((r_piece, r_range));
                } else {
                    pieces.push_front((r_piece, r_range));
                }
            }

            for (piece, range) in pieces {
                test_iter(piece.into_seq(), range, |l: &i32, r: i32| *l == r)
            }
        }
    }

    #[test]
    fn test_chunk_any_vec_zst() {
        // Safety: Type is correct.
        unsafe {
            let chunk_size = 8; // no effect.
            let mut v = ChunkAnyVec::new(tinfo!(()), chunk_size);
            assert!(v.is_empty());
            for i in 1..=chunk_size {
                v.push(());
                assert_eq!(v.len(), i);
            }

            // Not allocated.
            assert_eq!(
                v.get_chunk(0).unwrap().get_ptr(0),
                AnyVec::aligned_dangling(tinfo!(()).align).as_ptr()
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
        II: PartialEq + std::fmt::Debug,
        JI: PartialEq + std::fmt::Debug,
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
