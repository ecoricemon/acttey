use super::super::{
    raw::{AsRawIter, FlatRawIter, Iter, IterMut, ParIter, ParIterMut, ParRawIter, RawIter},
    types::{FnCloneRaw, FnDropRaw, TypeInfo},
};
use crate::ds::ptr::SendSyncPtr;
use rayon::iter::IntoParallelRefIterator;
use std::{
    alloc::{self, Layout},
    any::{self, TypeId},
    mem, ops,
    ptr::{self, NonNull},
    slice,
};

// TODO: Not tested in boundary conditions.
/// Vector containing same type of data without generic parameter.
#[derive(Debug)]
pub struct AnyVec {
    ptr: NonNull<u8>,
    len: usize,
    cap: usize,
    tinfo: TypeInfo,
}

// We're restricting type to be Send and Sync.
// See `AnyVec::new()`
unsafe impl Send for AnyVec {}
unsafe impl Sync for AnyVec {}

impl AnyVec {
    pub fn new(tinfo: TypeInfo) -> Self {
        // For now, allows only Send and Sync type.
        assert!(tinfo.is_send, "AnyVec doesn't allow not Send type for now");
        assert!(tinfo.is_sync, "AnyVec doesn't allow not Sync type for now");

        let mut v = Self {
            tinfo,
            ptr: Self::aligned_dangling(tinfo.align),
            len: 0,
            cap: 0,
        };

        // If ZST, we won't allocate any memory for the vector.
        // But, adjust capacity like `Vec`.
        if v.is_zst() {
            v.cap = usize::MAX;
        }
        v
    }

    pub const fn type_info(&self) -> &TypeInfo {
        &self.tinfo
    }

    pub const fn type_id(&self) -> TypeId {
        self.tinfo.ty
    }

    pub const fn type_name(&self) -> &'static str {
        self.tinfo.name
    }

    pub const fn item_size(&self) -> usize {
        self.tinfo.size
    }

    pub const fn is_zst(&self) -> bool {
        self.item_size() == 0
    }

    pub const fn align(&self) -> usize {
        self.tinfo.align
    }

    pub const fn fn_drop(&self) -> FnDropRaw {
        self.tinfo.fn_drop
    }

    pub const fn fn_clone(&self) -> FnCloneRaw {
        self.tinfo.fn_clone
    }

    pub const fn is_clone(&self) -> bool {
        self.tinfo.is_clone
    }

    pub const fn is_send(&self) -> bool {
        self.tinfo.is_send
    }

    pub const fn is_sync(&self) -> bool {
        self.tinfo.is_sync
    }

    pub fn is_type_of(&self, ty: &TypeId) -> bool {
        &self.tinfo.ty == ty
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

    pub fn iter_of<T: 'static>(&self) -> Iter<'_, T> {
        assert!(
            self.is_type_of(&TypeId::of::<T>()),
            "type doesn't match, contains {:?} but {:?} requested",
            self.type_name(),
            any::type_name::<T>()
        );
        // Safety: Vector contains type `T` data in it.
        unsafe { AsRawIter::iter_of(self) }
    }

    pub fn iter_mut_of<T: 'static>(&mut self) -> IterMut<'_, T> {
        assert!(
            self.is_type_of(&TypeId::of::<T>()),
            "type doesn't match, contains {:?} but {:?} requested",
            self.type_name(),
            any::type_name::<T>()
        );
        // Safety: Vector contains type `T` data in it.
        unsafe { AsRawIter::iter_mut_of(self) }
    }

    #[inline]
    pub fn par_iter_of<T: Send + Sync + 'static>(&self) -> ParIter<'_, T> {
        assert!(
            self.is_type_of(&TypeId::of::<T>()),
            "type doesn't match, contains {:?} but {:?} requested",
            self.type_name(),
            any::type_name::<T>()
        );
        // Safety: Vector contains type `T` data in it.
        unsafe { AsRawIter::par_iter_of(self) }
    }

    #[inline]
    pub fn par_iter_mut_of<T: Send + Sync + 'static>(&mut self) -> ParIterMut<'_, T> {
        assert!(
            self.is_type_of(&TypeId::of::<T>()),
            "type doesn't match, contains {:?} but {:?} requested",
            self.type_name(),
            any::type_name::<T>()
        );
        // Safety: Vector contains type `T` data in it.
        unsafe { AsRawIter::par_iter_mut_of(self) }
    }

    pub fn reserve(&mut self, add_num: usize) {
        let need_cap = self.len().saturating_add(add_num);
        if self.capacity() < need_cap {
            let max_cap = self.max_capacity();
            if need_cap > max_cap {
                panic!("can't allocate {need_cap} x {} bytes", self.item_size());
            }

            // self.capacity() * 2 doesn't overflow.
            // If sized, self.capacity() is less than or equal to isize::MAX.
            // otherwise, self.capacity() is usize::MAX, so that it can't reach.
            let new_cap = (self.capacity() * 2).clamp(need_cap, max_cap);

            // Safety: Infallible.
            unsafe { self._reserve(new_cap) };
        }
    }

    pub fn reserve_exact(&mut self, add_num: usize) {
        let need_cap = self.len().saturating_add(add_num);
        if self.capacity() < need_cap {
            if need_cap > self.max_capacity() {
                panic!("can't allocate {need_cap} x {} bytes", self.item_size());
            }

            // Safety: Infallible.
            unsafe { self._reserve(need_cap) };
        }
    }

    /// # Safety
    ///
    /// `new_cap` x [`Self::item_size`] must be greater than zero and lesser or
    /// equal to [`isize::MAX`].
    unsafe fn _reserve(&mut self, new_cap: usize) {
        let item_size = self.item_size();
        let new_size = new_cap * item_size;

        debug_assert!((1..=isize::MAX as usize).contains(&new_size));

        if self.capacity() == 0 {
            let layout = Layout::from_size_align(new_size, self.align()).unwrap();

            // Safety:
            let ptr = unsafe { alloc::alloc(layout) };
            if ptr.is_null() {
                alloc::handle_alloc_error(layout);
            }
            self.ptr = NonNull::new_unchecked(ptr);
            self.cap = new_cap;
        } else {
            self.realloc_unchecked(new_cap);
        };
    }

    /// # Examples
    ///
    /// ```
    /// # use my_ecs::prelude::*;
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
    /// assert_eq!(v.capacity(), 0);
    ///
    /// v.reserve(10);
    /// assert!(v.capacity() >= 10);
    ///
    /// unsafe { v.push(1_i32) };
    /// v.shrink_to_fit();
    /// assert!(v.capacity() >= 1);
    ///
    /// v.pop_drop();
    /// v.shrink_to_fit();
    /// assert_eq!(v.capacity(), 0);
    /// ```
    pub fn shrink_to_fit(&mut self) {
        if self.is_zst() || self.len() == self.capacity() {
            return;
        }

        if self.is_empty() {
            self.dealloc();
        } else {
            // Safety:
            // - Extra capacity, so current pointer is valid.
            // - Not ZST and empty, so that new capacity size in bytes is
            //   greater than zero.
            // - Size of current items in bytes cannot exceed `isize::MAX`.
            unsafe { self.realloc_unchecked(self.len()) };
        }
    }

    /// # Safety
    ///
    /// - `new_len` must be less than or equal to [`Self::capacity`].
    /// - Caller must initialized extended items.
    pub unsafe fn set_len(&mut self, new_len: usize) {
        debug_assert!(new_len <= self.capacity());

        self.len = new_len;
    }

    /// # Safety
    ///
    /// `index` must be inbound.
    /// `ptr` must point to valid data type.
    pub unsafe fn set_raw_unchecked(&mut self, index: usize, ptr: NonNull<u8>) {
        let dst = self.get_ptr(index);
        (self.fn_drop())(dst);
        ptr::copy_nonoverlapping(ptr.as_ptr(), dst, self.item_size());
    }

    /// Caller should make sure data pointed by `ptr` not to be dropped.
    /// To make it not to be dropped, call [`std::mem::forget`].
    ///
    /// # Safety
    ///
    /// `ptr` must point to valid data type.
    pub unsafe fn push_raw(&mut self, ptr: NonNull<u8>) {
        if !self.is_zst() {
            self.reserve(1);

            // Safety: index is valid.
            self.update_unchecked(self.len(), ptr);
        }

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
        if !self.is_zst() {
            self.reserve(1);

            let dst = self.get_ptr(self.len());
            write(dst);
        }

        // Safety: Infallible.
        unsafe { self.set_len(self.len().checked_add(1).unwrap()) };
    }

    /// # Safety
    ///
    /// Type of the value `T` must be the same as the type the vector knows.
    pub unsafe fn push<T: 'static>(&mut self, mut value: T) {
        debug_assert!(self.is_type_of(&TypeId::of::<T>()));

        // Safety: Infallible.
        unsafe {
            let ptr = NonNull::new_unchecked(&mut value as *mut T as *mut u8);
            self.push_raw(ptr);
        }
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
        let dst = self.get_ptr(index);
        ptr::copy_nonoverlapping(ptr.as_ptr(), dst, self.item_size());
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
    /// let mut v = AnyVec::new(tinfo!(i32));
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
            let ptr = self._pop();
            ptr::copy_nonoverlapping(ptr, buf, self.item_size());
            Some(())
        }
    }

    /// # Safety
    ///
    /// Type of the value `T` must be the same as the type the vector knows.
    pub unsafe fn pop<T: 'static>(&mut self) -> Option<T> {
        debug_assert!(self.is_type_of(&TypeId::of::<T>()));

        if self.is_empty() {
            None
        } else {
            // Safety: Vector is not empty.
            let ptr = unsafe { self._pop() as *mut T };
            let value = unsafe { ptr.read() };
            Some(value)
        }
    }

    pub fn pop_drop(&mut self) -> Option<()> {
        if self.is_empty() {
            None
        } else {
            // Safety: Vector is not empty.
            unsafe { (self.fn_drop())(self._pop()) };
            Some(())
        }
    }

    pub fn pop_forget(&mut self) -> Option<()> {
        if self.is_empty() {
            None
        } else {
            // Safety: Vector is not empty.
            unsafe { self._pop() };
            Some(())
        }
    }

    /// # Safety
    ///
    /// Length of the vector must not be zero.
    unsafe fn _pop(&mut self) -> *mut u8 {
        // Safety: Decreasing is safe.
        self.set_len(self.len() - 1);

        // Safety: We're using `Layout::from_size_align` which restricts size to be under the limit.
        self.get_ptr(self.len())
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
    /// Panics if `index` is out of bound..
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
            let ptr0 = self.get_ptr(index0);
            let ptr1 = self.get_ptr(index1);
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
        NonNull::new_unchecked(self.get_ptr(index))
    }

    /// `index` is an index of T, not u8.
    ///
    /// # Safety
    ///
    /// `index` must be inbound and result address must not overflow `isize`.
    pub const unsafe fn get_ptr(&self, index: usize) -> *mut u8 {
        let offset = index * self.item_size();
        self.ptr.as_ptr().add(offset)
    }

    /// # Safety
    ///
    /// Type of the value `T` must be the same as the type the vector knows.
    pub unsafe fn get<T: 'static>(&self, index: usize) -> Option<&T> {
        debug_assert!(self.is_type_of(&TypeId::of::<T>()));

        self.get_raw(index)
            .map(|ptr| unsafe { (ptr.as_ptr() as *const T).as_ref().unwrap_unchecked() })
    }

    /// # Safety
    ///
    /// Type of the value `T` must be the same as the type the vector knows.
    pub unsafe fn get_mut<T: 'static>(&mut self, index: usize) -> Option<&mut T> {
        debug_assert!(self.is_type_of(&TypeId::of::<T>()));

        self.get_raw(index)
            .map(|ptr| unsafe { (ptr.as_ptr() as *mut T).as_mut().unwrap_unchecked() })
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
        debug_assert!(self.tinfo.is_clone);

        let val_ptr = val_ptr.as_ptr().cast_const();

        if new_len > self.len() {
            self.reserve(new_len - self.len());

            let (mut offset, stride) = self.get_ptr_offset(self.len());

            let range = self.len()..new_len;
            for _ in range {
                unsafe {
                    let dest = self.ptr.as_ptr().add(offset);
                    (self.tinfo.fn_clone)(val_ptr, dest);
                };
                offset += stride;
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
            self.reserve(new_len - self.len());

            let (mut offset, stride) = self.get_ptr_offset(self.len());

            let range = self.len()..new_len;
            for _ in range {
                let ptr = unsafe { self.ptr.as_ptr().add(offset) } as *mut T;
                unsafe { ptr.write(f()) };
                offset += stride;
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

        let (mut offset, stride) = self.get_ptr_offset(len);

        let range = len..self.len();
        for _ in range {
            unsafe {
                let ptr = self.ptr.as_ptr().add(offset);
                (self.fn_drop())(ptr);
            }
            offset += stride;
        }

        unsafe {
            self.set_len(len);
        }
    }

    pub fn as_vec_mut<T: 'static>(&mut self) -> TypedAnyVec<'_, T> {
        assert!(self.is_type_of(&TypeId::of::<T>()));

        let typed = unsafe {
            Vec::from_raw_parts(self.ptr.as_ptr() as *mut T, self.len(), self.capacity())
        };
        TypedAnyVec { typed, any: self }
    }

    pub fn as_slice<T: 'static>(&self) -> &[T] {
        assert!(self.is_type_of(&TypeId::of::<T>()));

        unsafe { slice::from_raw_parts(self.ptr.as_ptr() as *const T, self.len()) }
    }

    pub fn as_mut_slice<T: 'static>(&mut self) -> &mut [T] {
        assert!(self.is_type_of(&TypeId::of::<T>()));

        unsafe { slice::from_raw_parts_mut(self.ptr.as_ptr() as *mut T, self.len()) }
    }

    /// # Safety
    ///
    /// Undefined behavior if conditions below are not met.
    /// - Current pointer must point to a valid memory location.
    /// - `new_cap` x [`Self::item_size`] is greater than zero and lesser or
    ///   equal to [`isize::MAX`].
    unsafe fn realloc_unchecked(&mut self, new_cap: usize) {
        let item_size = self.item_size();
        let old_size = self.capacity() * item_size;
        let old_layout = Layout::from_size_align(old_size, self.align()).unwrap();
        let new_size = new_cap * item_size;

        debug_assert_ne!(self.ptr, Self::aligned_dangling(self.align()));
        debug_assert!((1..=isize::MAX as usize).contains(&new_size));

        // Safety
        // - `ptr` and `layout` are valid.
        // - `new_size` doesn't overflow `isize::MAX`.
        let ptr = unsafe { alloc::realloc(self.ptr.as_ptr(), old_layout, new_size) };
        if ptr.is_null() {
            let layout = Layout::from_size_align(new_size, self.align()).unwrap();
            alloc::handle_alloc_error(layout);
        }

        self.ptr = NonNull::new_unchecked(ptr);
        self.cap = new_cap;
    }

    /// Drops all items in the vector and frees memory.
    fn dealloc(&mut self) {
        // Calls every drop method.
        self.truncate(0);

        // Releases the memory.
        if !self.is_zst() && self.capacity() > 0 {
            let size = self.capacity() * self.item_size();
            let layout = Layout::from_size_align(size, self.align()).unwrap();
            unsafe { alloc::dealloc(self.ptr.as_ptr(), layout) };

            self.ptr = Self::aligned_dangling(self.align());
            self.cap = 0;
        }
    }

    /// Converts start index into start pointer offset from the beginning of the vector and stride in bytes.
    /// If the type is zero sized, it will return all zeros.
    /// So, you must not use offset as loop counter.
    /// And caller must provide valid index.
    /// This method assumes that, therefore doesn't check either index validity and arithmetic overflow.
    const fn get_ptr_offset(&self, index: usize) -> (usize, usize) {
        if self.is_zst() {
            (0, 0)
        } else {
            let item_size = self.item_size();
            // Valid pointer offset can't exceed isize::MAX.
            (index * item_size, item_size)
        }
    }

    /// Mimics [`NonNull::dangling`].
    /// This helps to use lots of ptr module's APIs because they request aligned pointer even if the type is zero sized.
    pub(crate) fn aligned_dangling(align: usize) -> NonNull<u8> {
        NonNull::new(align as *mut u8).unwrap()
    }

    pub(crate) fn iter2(&self) -> FlatRawIter {
        let this = (self as *const Self as *const u8).cast_mut();
        let this = unsafe { NonNull::new_unchecked(this) };
        let chunks = 1;
        unsafe fn fn_iter(this: NonNull<u8>, chunk_idx: usize) -> RawIter {
            if chunk_idx == 0 {
                let this = this.cast::<AnyVec>().as_ref();
                this.iter()
            } else {
                RawIter::empty()
            }
        }
        unsafe fn fn_find(this: NonNull<u8>, item_idx: usize) -> (RawIter, usize, usize) {
            let this = this.cast::<AnyVec>().as_ref();
            (this.iter(), 0, item_idx)
        }
        // If ZST, alignment will become stride.
        let stride = self.item_size().max(self.align());
        let len = self.len();

        unsafe { FlatRawIter::new(this, chunks, fn_iter as _, fn_find as _, stride, len) }
    }

    /// Maximum capacity only if the type is not zero sized.
    const fn max_capacity(&self) -> usize {
        isize::MAX as usize / self.item_size()
    }
}

impl Clone for AnyVec {
    fn clone(&self) -> Self {
        let ptr = if self.is_zst() || self.is_empty() {
            self.ptr
        } else {
            let item_size = self.item_size();

            let size = self.len() * item_size;
            let layout = Layout::from_size_align(size, self.align()).unwrap();
            let ptr = unsafe { alloc::alloc(layout) };
            if ptr.is_null() {
                alloc::handle_alloc_error(layout);
            }

            let mut offset = 0;
            while offset < size {
                unsafe {
                    let src = self.ptr.as_ptr().add(offset);
                    let dst = ptr.add(offset);
                    // If data type doesn't support clone, panics here.
                    (self.fn_clone())(src, dst);
                }
                offset += item_size;
            }

            NonNull::new(ptr).unwrap()
        };

        Self {
            tinfo: self.tinfo,
            ptr,
            len: self.len(),
            cap: self.len(),
        }
    }
}

impl Drop for AnyVec {
    fn drop(&mut self) {
        self.dealloc();
    }
}

impl AsRawIter for AnyVec {
    fn iter(&self) -> RawIter {
        let start = self.ptr;
        // If ZST, alignment will become stride.
        let stride = self.item_size().max(self.align());
        let end = unsafe { self.ptr.add(self.len() * stride) };
        // Safety: We got both pointers from a vector,
        // which proves they are valid, and stride must not be zero
        // because align is not zero.
        unsafe { RawIter::new(start, end, stride) }
    }
}

impl IntoIterator for &AnyVec {
    type Item = SendSyncPtr<u8>;
    type IntoIter = RawIter;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'data> IntoParallelRefIterator<'data> for AnyVec {
    type Item = SendSyncPtr<u8>;
    type Iter = ParRawIter;

    fn par_iter(&'data self) -> Self::Iter {
        AsRawIter::par_iter(self)
    }
}

/// [`Vec`]-like typed vector you can get from [`AnyVec`].
/// When this is dropped, any changes you've made reflect to the `AnyVec`.
pub struct TypedAnyVec<'a, T> {
    typed: Vec<T>,
    any: &'a mut AnyVec,
}

impl<T> ops::Deref for TypedAnyVec<'_, T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.typed
    }
}

impl<T> ops::DerefMut for TypedAnyVec<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.typed
    }
}

impl<T> Drop for TypedAnyVec<'_, T> {
    fn drop(&mut self) {
        let ptr = self.typed.as_mut_ptr() as *mut u8;
        self.any.ptr = NonNull::new(ptr).unwrap();
        self.any.len = self.typed.len();
        self.any.cap = self.typed.capacity();
        let v = mem::take(&mut self.typed);
        mem::forget(v);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(PartialEq, Debug, Clone, Copy, Default)]
    struct SA {
        x: [usize; 2],
    }

    #[cfg(debug_assertions)]
    #[derive(PartialEq, Debug, Clone, Copy, Default)]
    struct SB {
        x: [usize; 2],
        y: [f32; 2],
    }

    #[test]
    fn test_anyvec_clone() {
        // Safety: Type is correct.
        unsafe {
            let mut a = AnyVec::new(crate::tinfo!(SA));
            let mut b = a.clone();

            a.push(SA { x: [0, 0] });
            b.push(SA { x: [1, 1] });

            let c = a.clone();
            let d = b.clone();

            assert_eq!(a.len(), c.len());
            assert_eq!(a.get::<SA>(0), c.get::<SA>(0));
            assert_eq!(b.len(), d.len());
            assert_eq!(b.get::<SA>(0), d.get::<SA>(0));
        }
    }

    #[test]
    #[should_panic]
    fn test_anyvec_uncloneable_panic() {
        // Safety: Type is correct.
        unsafe {
            #[allow(dead_code)]
            struct S(i32);
            let mut a = AnyVec::new(crate::tinfo!(S));
            a.push(S(0));
            let _ = a.clone();
        }
    }

    #[test]
    fn test_anyvec_drop() {
        use std::sync::{Arc, Mutex};

        struct X(Arc<Mutex<i32>>);
        impl Drop for X {
            fn drop(&mut self) {
                *self.0.lock().unwrap() += 1;
            }
        }

        // Safety: Type is correct.
        unsafe {
            let cnt = Arc::new(Mutex::new(0));
            let mut v = AnyVec::new(crate::tinfo!(X));
            for _ in 0..10 {
                v.push(X(Arc::clone(&cnt)));
            }

            for i in 1..=5 {
                v.pop_drop();
                assert_eq!(*cnt.lock().unwrap(), i);
            }

            drop(v);
            assert_eq!(*cnt.lock().unwrap(), 10);
        }
    }

    #[test]
    fn test_anyvec_push_pop() {
        // Safety: Type is correct.
        unsafe {
            let mut a = AnyVec::new(crate::tinfo!(SA));
            assert_eq!(true, a.is_empty());

            a.push(SA { x: [0, 1] });
            assert_eq!(1, a.len());
            assert!(a.capacity() >= 1);
            assert_eq!(false, a.is_empty());

            a.push(SA { x: [2, 3] });
            assert_eq!(2, a.len());
            assert!(a.capacity() >= 2);
            assert_eq!(false, a.is_empty());

            assert_eq!(Some(SA { x: [2, 3] }), a.pop::<SA>());
            assert_eq!(1, a.len());
            assert!(a.capacity() >= 1);
            assert_eq!(false, a.is_empty());

            assert_eq!(Some(SA { x: [0, 1] }), a.pop::<SA>());
            assert_eq!(0, a.len());
            assert_eq!(true, a.is_empty());

            assert_eq!(None, a.pop::<SA>());
        }
    }

    #[test]
    fn test_anyvec_remove() {
        // Safety: Type is correct.
        unsafe {
            let mut a = AnyVec::new(crate::tinfo!(SA));

            a.push(SA { x: [0, 1] });
            a.push(SA { x: [2, 3] });
            a.push(SA { x: [4, 5] });
            a.push(SA { x: [6, 7] });

            let removed = a.swap_remove(1);
            assert_eq!(SA { x: [2, 3] }, removed);
            assert_eq!(3, a.len());
            assert_eq!(Some(&SA { x: [0, 1] }), a.get(0));
            assert_eq!(Some(&SA { x: [6, 7] }), a.get(1));
            assert_eq!(Some(&SA { x: [4, 5] }), a.get(2));

            a.swap_remove_drop(1);
            assert_eq!(2, a.len());
            assert_eq!(Some(&SA { x: [0, 1] }), a.get(0));
            assert_eq!(Some(&SA { x: [4, 5] }), a.get(1));
        }
    }

    #[test]
    fn test_anyvec_resize() {
        unsafe {
            // Tests resize().
            let mut v = AnyVec::new(crate::tinfo!(i32));
            assert!(v.is_empty());

            v.resize(10, 42_i32);
            assert_eq!(v.len(), 10);

            for val in v.iter_of::<i32>() {
                assert_eq!(*val, 42);
            }

            // Tests resize_raw().
            #[derive(Clone)]
            struct Val(String);

            let mut v = AnyVec::new(crate::tinfo!(Val));

            let val = Val("42".to_owned());
            let val_ptr = NonNull::new(&val as *const Val as *mut Val as *mut u8).unwrap();
            v.resize_raw(10, val_ptr);
            assert_eq!(v.len(), 10);

            for val in v.iter_of::<Val>() {
                assert_eq!(val.0.as_str(), "42");
            }
        }
    }

    #[cfg(debug_assertions)]
    #[test]
    #[should_panic]
    fn test_anyvec_push_incorrect_type_panic() {
        // Unsafe: It will be panicked in debug mode.
        unsafe {
            let mut a = AnyVec::new(crate::tinfo!(SA));
            a.push(SB {
                x: [0, 1],
                y: [0.1, 0.2],
            });
        }
    }

    #[cfg(debug_assertions)]
    #[test]
    #[should_panic]
    fn test_anyvec_pop_incorrect_type_panic() {
        // Unsafe: It will be panicked in debug mode.
        unsafe {
            let mut a = AnyVec::new(crate::tinfo!(SB));
            a.push(SB {
                x: [0, 1],
                y: [0.1, 0.2],
            });
            let _ = a.pop::<SA>();
        }
    }

    #[test]
    fn test_anyvec_into_vec_push_pop() {
        // Safety: Type is correct.
        unsafe {
            let mut a = AnyVec::new(crate::tinfo!(SA));
            {
                let mut v = (&mut a).as_vec_mut::<SA>();
                v.push(SA { x: [0, 1] });
                v.push(SA { x: [2, 3] });
                assert_eq!(Some(SA { x: [2, 3] }), v.pop());
            }

            assert_eq!(Some(SA { x: [0, 1] }), a.pop::<SA>());
            assert_eq!(None, a.pop::<SA>());

            {
                let mut v = (&mut a).as_vec_mut::<SA>();
                v.push(SA { x: [0, 1] });
                v.push(SA { x: [2, 3] });
            }
            let v_imm = a.as_slice::<SA>();
            assert_eq!(Some(&SA { x: [0, 1] }), v_imm.get(0));
            assert_eq!(Some(&SA { x: [2, 3] }), v_imm.get(1));
        }
    }

    #[test]
    fn test_anyvec_zst() {
        // Safety: Type is correct.
        unsafe {
            let mut v = AnyVec::new(crate::tinfo!(()));
            assert!(v.is_empty());
            assert_eq!(v.capacity(), usize::MAX);

            // Pusing ZST must be possible, and length must be grown by pushing.
            for i in 1..10 {
                v.push(());
                assert_eq!(v.len(), i);
            }

            // We've pushed ZST values, but the vector must not have allocated
            // memory.
            assert_eq!(v.ptr, AnyVec::aligned_dangling(crate::tinfo!(()).align));

            // Popping ZST must be possible, and length must be shrunk by
            // popping.
            for i in (1..10).rev() {
                v.pop_drop();
                assert_eq!(v.len(), i - 1);
            }

            // Reserving capacity for ZST in the vector must have no effects.
            v.reserve(100);
            assert_eq!(v.capacity(), usize::MAX);

            // Shrinknig capacity for ZST in the vector must have no effects.
            v.shrink_to_fit();
            assert_eq!(v.capacity(), usize::MAX);
        }
    }
}
