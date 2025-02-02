use super::super::{
    raw::{AsRawIter, FlatRawIter, Iter, IterMut, ParIter, ParIterMut, ParRawIter, RawIter},
    types::{FnCloneRaw, FnDropRaw, TypeInfo},
};
use crate::ds::ptr::SendSyncPtr;
use rayon::iter::IntoParallelRefIterator;
use std::{
    alloc::{self, Layout},
    any::{self, TypeId},
    cmp::Ordering,
    mem,
    num::NonZeroUsize,
    ops::{Deref, DerefMut},
    ptr::{self, NonNull},
    slice,
};

/// A type-erased vector containing values of the same type.
///
/// This vector would be useful when you need to hold vectors of heterogeneous
/// types in a single variable. Instead, this vector has methods that looks
/// quite dirty and not easy to use. Most methods are unsafe because they take
/// or return pointer instead of concrete type.
#[derive(Debug)]
pub struct AnyVec {
    ptr: NonNull<u8>,
    len: usize,
    cap: usize,
    tinfo: TypeInfo,
}

// `AnyVec` is currently restricted to be `Send` and `Sync`. See
// `AnyVec::new()`.
unsafe impl Send for AnyVec {}
unsafe impl Sync for AnyVec {}

impl AnyVec {
    /// Creates a new empty vector.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let v = AnyVec::new(tinfo!(i32));
    /// ```
    pub fn new(tinfo: TypeInfo) -> Self {
        assert!(tinfo.is_send, "expected `Send` and `Sync` type");
        assert!(tinfo.is_sync, "expected `Send` and `Sync` type");

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

    /// Returns [`TypeInfo`] of the item.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let v = AnyVec::new(tinfo!(i32));
    /// assert_eq!(v.type_info(), &tinfo!(i32));
    /// ```
    pub const fn type_info(&self) -> &TypeInfo {
        &self.tinfo
    }

    /// Returns [`TypeId`] of the item.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let v = AnyVec::new(tinfo!(i32));
    /// assert_eq!(v.type_id(), std::any::TypeId::of::<i32>());
    /// ```
    pub const fn type_id(&self) -> TypeId {
        self.tinfo.ty
    }

    /// Returns name of the item based on [`type_name`](any::type_name).
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let v = AnyVec::new(tinfo!(i32));
    /// println!("{}", v.type_name());
    /// ```
    pub const fn type_name(&self) -> &'static str {
        self.tinfo.name
    }

    /// Returns size in bytes of the item.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let v = AnyVec::new(tinfo!(i32));
    /// assert_eq!(v.item_size(), std::mem::size_of::<i32>());
    /// ```
    pub const fn item_size(&self) -> usize {
        self.tinfo.size
    }

    /// Returns whether the item is zero-sized type or not.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let v = AnyVec::new(tinfo!(i32));
    /// assert!(!v.is_zst());
    /// let v = AnyVec::new(tinfo!(()));
    /// assert!(v.is_zst());
    /// ```
    pub const fn is_zst(&self) -> bool {
        self.item_size() == 0
    }

    /// Returns alignment in bytes of the item.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let v = AnyVec::new(tinfo!(i32));
    /// assert_eq!(v.align(), std::mem::align_of::<i32>());
    /// ```
    pub const fn align(&self) -> usize {
        self.tinfo.align
    }

    /// Returns raw drop function pointer.
    pub const fn fn_drop(&self) -> FnDropRaw {
        self.tinfo.fn_drop
    }

    /// Returns raw clone function pointer.
    pub const fn fn_clone(&self) -> FnCloneRaw {
        self.tinfo.fn_clone
    }

    /// Returns whether the item is [`Clone`] or not.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let v = AnyVec::new(tinfo!(i32));
    /// assert!(v.is_clone());
    ///
    /// struct S;
    /// let v = AnyVec::new(tinfo!(S));
    /// assert!(!v.is_clone());
    /// ```
    pub const fn is_clone(&self) -> bool {
        self.tinfo.is_clone
    }

    /// Returns whether the item is [`Send`] or not.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let v = AnyVec::new(tinfo!(i32));
    /// assert!(v.is_send());
    /// // let v = AnyVec::new(tinfo!(*const u8)); // Disallowed for now.
    /// ```
    pub const fn is_send(&self) -> bool {
        self.tinfo.is_send
    }

    /// Returns whether the item is [`Sync`] or not.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let v = AnyVec::new(tinfo!(i32));
    /// assert!(v.is_sync());
    /// // let v = AnyVec::new(tinfo!(*const u8)); // Disallowed for now.
    /// ```
    pub const fn is_sync(&self) -> bool {
        self.tinfo.is_sync
    }

    /// Returns true if the given [`TypeId`] is the same as item of the vector.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let v = AnyVec::new(tinfo!(i32));
    /// assert!(v.is_type_of::<i32>());
    /// ```
    pub fn is_type_of<T: 'static>(&self) -> bool {
        self.tinfo.ty == TypeId::of::<T>()
    }

    /// Returns number of item.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
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
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let v = AnyVec::new(tinfo!(i32));
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
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
    /// v.reserve(10);
    /// assert!(v.capacity() >= 10);
    /// ```
    pub const fn capacity(&self) -> usize {
        self.cap
    }

    /// Returns iterator visiting all items.
    ///
    /// # Panics
    ///
    /// Panics if the given type is not the same as the vector contains.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
    /// unsafe {
    ///     v.push(0_i32);
    ///     v.push(1_i32);
    /// }
    /// for x in v.iter_of::<i32>() {
    ///     println!("{x}");
    /// }
    /// ```
    pub fn iter_of<T: 'static>(&self) -> Iter<'_, T> {
        assert!(
            self.is_type_of::<T>(),
            "type doesn't match, contains {:?} but {:?} requested",
            self.type_name(),
            any::type_name::<T>()
        );
        // Safety: Vector contains type `T` data in it.
        unsafe { AsRawIter::iter_of(self) }
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
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
    /// unsafe {
    ///     v.push(0_i32);
    ///     v.push(1_i32);
    /// }
    /// for x in v.iter_mut_of::<i32>() {
    ///     *x += 10;
    /// }
    /// ```
    pub fn iter_mut_of<T: 'static>(&mut self) -> IterMut<'_, T> {
        assert!(
            self.is_type_of::<T>(),
            "type doesn't match, contains {:?} but {:?} requested",
            self.type_name(),
            any::type_name::<T>()
        );
        // Safety: Vector contains type `T` data in it.
        unsafe { AsRawIter::iter_mut_of(self) }
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
    /// use my_ecs::ds::AnyVec;
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
    /// unsafe {
    ///     v.push(1_i32);
    ///     v.push(2_i32);
    /// }
    /// let sum: i32 = v.par_iter_of::<i32>().sum();
    /// assert_eq!(sum, 3);
    /// ```
    #[inline]
    pub fn par_iter_of<T: Send + Sync + 'static>(&self) -> ParIter<'_, T> {
        assert!(
            self.is_type_of::<T>(),
            "type doesn't match, contains {:?} but {:?} requested",
            self.type_name(),
            any::type_name::<T>()
        );
        // Safety: Vector contains type `T` data in it.
        unsafe { AsRawIter::par_iter_of(self) }
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
    /// use my_ecs::ds::AnyVec;
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
    /// unsafe {
    ///     v.push(1_i32);
    ///     v.push(2_i32);
    /// }
    /// let sum: i32 = v.par_iter_mut_of::<i32>().map(|x| *x + 1).sum();
    /// assert_eq!(sum, 5);
    /// ```
    #[inline]
    pub fn par_iter_mut_of<T: Send + Sync + 'static>(&mut self) -> ParIterMut<'_, T> {
        assert!(
            self.is_type_of::<T>(),
            "type doesn't match, contains {:?} but {:?} requested",
            self.type_name(),
            any::type_name::<T>()
        );
        // Safety: Vector contains type `T` data in it.
        unsafe { AsRawIter::par_iter_mut_of(self) }
    }

    /// Reserves additional capacity more than or equal to the given value.
    ///
    /// If capacity of the vector is already sufficient, nothing takes place.
    /// Otherwise, allocates new memory or reallocates the old memory so that
    /// the capacity will be greater than or equal to `self.len() + additional`.
    /// This method deliberately allocates more memory than requested to avoid
    /// frequent reallocation.
    ///
    /// # Panics
    ///
    /// Panics if total memory in bytes after calling this method will exceed
    /// [`isize::MAX`].
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
    /// v.reserve(10);
    /// assert!(v.capacity() >= 10);
    /// ```
    pub fn reserve(&mut self, additional: usize) {
        let need_cap = self.len().saturating_add(additional);
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

    /// Reserves additional capacity as much as the given value.
    ///
    /// If capacity of the vector is already sufficient, nothing takes place.
    /// Otherwise, allocates new memory or reallocates the old memory so that
    /// the capacity will be equal to `self.len() + additional` exactly.
    ///
    /// Note, however, that allocator may give a memory block that is greater
    /// than requested for some reason. The exact size of memory block is
    /// invisible from clients, but you can look into it using something like
    /// [`libc::malloc_usable_size`](https://docs.rs/libc/latest/libc/fn.malloc_usable_size.html).
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
    /// v.reserve(10);
    /// assert_eq!(v.capacity(), 10);
    /// ```
    pub fn reserve_exact(&mut self, additional: usize) {
        let need_cap = self.len().saturating_add(additional);
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

            let ptr = unsafe { alloc::alloc(layout) };
            let Some(ptr) = NonNull::new(ptr) else {
                alloc::handle_alloc_error(layout);
            };

            self.ptr = ptr;
            self.cap = new_cap;
        } else {
            unsafe { self.realloc_unchecked(new_cap) }
        };
    }

    /// Shrinks capacity of the vector as much as possible.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
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
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
    ///
    /// unsafe {
    ///     v.push(0_i32);
    ///     v.set_len(0);
    /// }
    /// ```
    pub unsafe fn set_len(&mut self, new_len: usize) {
        debug_assert!(new_len <= self.capacity());

        self.len = new_len;
    }

    /// Copies data as much as [`AnyVec::item_size`] from `src` address to the
    /// memory pointed by `index`.
    ///
    /// Note that the old stored value is dropped before the copy, which means
    /// the given index must be in bounds.
    ///
    /// # Safety
    ///
    /// - `index` must be in bounds.
    /// - `src` must point to a valid type that the vector contains.
    /// - Memory range pointed by `index` must not need to be dropped.
    /// - `src` must not be dropped after calling this method because it is
    ///   moved into the vector.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
    /// use std::ptr::NonNull;
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
    ///
    /// unsafe {
    ///     v.push(0_i32);
    ///     let value = 1_i32;
    ///     let ptr = (&value as *const i32 as *const u8).cast_mut();
    ///     v.set_raw_unchecked(0, NonNull::new(ptr).unwrap());
    ///     assert_eq!(v.pop(), Some(1_i32));
    /// }
    /// ```
    pub unsafe fn set_raw_unchecked(&mut self, index: usize, src: NonNull<u8>) {
        unsafe {
            let dst = self.get_ptr(index);
            let fn_drop = self.fn_drop();
            fn_drop(dst);
            ptr::copy_nonoverlapping(src.as_ptr(), dst, self.item_size());
        }
    }

    /// Copies data as much as `self.item_size()` from `src` address to the end
    /// of the vector.
    ///
    /// If the vector doesn't have sufficient capacity for the appended value,
    /// then the vector increases its capacity first by calling
    /// [`AnyVec::reserve`] which allocates memory more than just one value,
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
    /// use my_ecs::{tinfo, ds::AnyVec};
    /// use std::ptr::NonNull;
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
    ///
    /// let value = 0x01020304_i32;
    /// unsafe {
    ///     let ptr = (&value as *const i32 as *const u8).cast_mut();
    ///     v.push_raw(NonNull::new(ptr).unwrap());
    ///     assert_eq!(v.pop(), Some(value));
    /// }
    /// ```
    pub unsafe fn push_raw(&mut self, src: NonNull<u8>) {
        if !self.is_zst() {
            self.reserve(1);

            // Safety: index is valid.
            unsafe {
                let dst = self.get_ptr(self.len());
                ptr::copy_nonoverlapping(src.as_ptr(), dst, self.item_size());
            }
        }

        // Safety: Infallible.
        unsafe { self.set_len(self.len().checked_add(1).unwrap()) };
    }

    /// Writes a value to the end of the vector by calling the given function.
    ///
    /// If the vector doesn't have sufficient capacity for the appended value,
    /// then the vector increases its capacity first by calling
    /// [`AnyVec::reserve`] which allocates memory more than just one value,
    /// then does the write operation.
    ///
    /// # Safety
    ///
    /// - `write` must write a valid type that the vector contains.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
    /// use std::ptr::{self, NonNull};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
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
        if !self.is_zst() {
            self.reserve(1);

            let dst = unsafe { self.get_ptr(self.len()) };
            write(dst);
        }

        // Safety: Infallible.
        unsafe { self.set_len(self.len().checked_add(1).unwrap()) };
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
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
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

        // Safety: Infallible.
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
    /// use my_ecs::{tinfo, ds::AnyVec};
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
    pub unsafe fn pop_raw(&mut self, buf: *mut u8) -> Option<()> {
        if self.is_empty() {
            None
        } else {
            unsafe {
                // Safety: Vector is not empty.
                let ptr = self._pop();
                ptr::copy_nonoverlapping(ptr, buf, self.item_size());
            }
            Some(())
        }
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
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
    ///
    /// unsafe {
    ///     v.push(0_i32);
    ///     let value = v.pop::<i32>().unwrap();
    ///     assert_eq!(value, 0_i32);
    /// }
    /// ```
    pub unsafe fn pop<T: 'static>(&mut self) -> Option<T> {
        debug_assert!(
            self.is_type_of::<T>(),
            "expected `{}`, but received `{}`",
            self.type_name(),
            any::type_name::<T>()
        );

        if self.is_empty() {
            None
        } else {
            // Safety: Vector is not empty.
            let ptr = unsafe { self._pop() as *mut T };
            let value = unsafe { ptr.read() };
            Some(value)
        }
    }

    /// Removes the last item from the vector then drops it immediately.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
    ///
    /// unsafe {
    ///     v.push(0_i32);
    ///     assert_eq!(v.pop_drop(), Some(()));
    /// }
    /// ```
    pub fn pop_drop(&mut self) -> Option<()> {
        if self.is_empty() {
            None
        } else {
            let fn_drop = self.fn_drop();
            // Safety: Vector is not empty.
            unsafe { fn_drop(self._pop()) };
            Some(())
        }
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
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
    ///
    /// unsafe {
    ///     v.push(0_i32);
    ///     assert_eq!(v.pop_forget(), Some(()));
    /// }
    /// ```
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
        unsafe {
            // Safety: Decreasing is safe.
            self.set_len(self.len() - 1);

            // Safety: We're using `Layout::from_size_align` which restricts
            // size to be under the limit.
            self.get_ptr(self.len())
        }
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
    /// use my_ecs::{tinfo, ds::AnyVec};
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
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
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
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
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
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
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
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
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
            let ptr0 = self.get_ptr(index0);
            let ptr1 = self.get_ptr(index1);
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
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
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
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
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
        unsafe {
            let item_ptr = self.get_ptr(index);
            NonNull::new_unchecked(item_ptr)
        }
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
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
    ///
    /// unsafe {
    ///     v.push(0_i32);
    ///     assert_eq!(v.get(0), Some(&0_i32));
    /// }
    /// ```
    pub unsafe fn get<T: 'static>(&self, index: usize) -> Option<&T> {
        debug_assert!(
            self.is_type_of::<T>(),
            "expected `{}`, but received `{}`",
            self.type_name(),
            any::type_name::<T>()
        );

        self.get_raw(index)
            .map(|ptr| unsafe { (ptr.as_ptr() as *const T).as_ref().unwrap_unchecked() })
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
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
    ///
    /// unsafe {
    ///     v.push(0_i32);
    ///     *v.get_mut(0).unwrap() = 1_i32;
    ///     assert_eq!(v.get(0), Some(&1_i32));
    /// }
    /// ```
    pub unsafe fn get_mut<T: 'static>(&mut self, index: usize) -> Option<&mut T> {
        debug_assert!(
            self.is_type_of::<T>(),
            "expected `{}`, but received `{}`",
            self.type_name(),
            any::type_name::<T>()
        );

        self.get_raw(index)
            .map(|ptr| unsafe { (ptr.as_ptr() as *mut T).as_mut().unwrap_unchecked() })
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
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
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
        self.resize_with(new_len, || value.clone());
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
    /// use my_ecs::{tinfo, ds::AnyVec};
    /// use std::ptr::NonNull;
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
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

        match new_len.cmp(&self.len()) {
            Ordering::Less => self.truncate(new_len),
            Ordering::Equal => {}
            Ordering::Greater => {
                self.reserve(new_len - self.len());

                let (mut offset, stride) = self.get_ptr_offset(self.len());
                let src = val_ptr.as_ptr().cast_const();

                let range = self.len()..new_len;
                for _ in range {
                    unsafe {
                        let dst = self.ptr.as_ptr().add(offset);
                        (self.tinfo.fn_clone)(src, dst);
                    };
                    offset += stride;
                }

                unsafe { self.set_len(new_len) };
            }
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
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
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

        match new_len.cmp(&self.len()) {
            Ordering::Less => self.truncate(new_len),
            Ordering::Equal => {}
            Ordering::Greater => {
                self.reserve(new_len - self.len());

                let (mut offset, stride) = self.get_ptr_offset(self.len());

                let range = self.len()..new_len;
                for _ in range {
                    let ptr = unsafe { self.ptr.as_ptr().add(offset) } as *mut T;
                    unsafe { ptr.write(f()) };
                    offset += stride;
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
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
    ///
    /// unsafe { v.resize(10, 0_i32) };
    /// v.truncate(5);
    /// assert_eq!(v.len(), 5);
    /// ```
    pub fn truncate(&mut self, len: usize) {
        if len >= self.len() {
            return;
        }

        let (mut offset, stride) = self.get_ptr_offset(len);

        let range = len..self.len();
        let fn_drop = self.fn_drop();
        for _ in range {
            unsafe {
                let ptr = self.ptr.as_ptr().add(offset);
                fn_drop(ptr);
            }
            offset += stride;
        }

        unsafe { self.set_len(len) };
    }

    /// Creates [`TypedAnyVec`] which looks like `Vec<T>`.
    ///
    /// You can call tons of useful methods on [`Vec`] by converting the vector.
    ///
    /// # Panics
    ///
    /// Panics if `T` it not the same type as the vector contains.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
    /// {
    ///     let mut tv = v.as_vec_mut::<i32>();
    ///     tv.push(0);
    ///     tv.push(1);
    /// }
    /// unsafe {
    ///     assert_eq!(v.pop(), Some(1_i32));
    ///     assert_eq!(v.pop(), Some(0_i32));
    /// }
    /// ```
    pub fn as_vec_mut<T: 'static>(&mut self) -> TypedAnyVec<'_, T> {
        assert!(
            self.is_type_of::<T>(),
            "expected `{}`, but received `{}`",
            self.type_name(),
            any::type_name::<T>()
        );

        let typed = unsafe {
            Vec::from_raw_parts(self.ptr.as_ptr() as *mut T, self.len(), self.capacity())
        };
        TypedAnyVec { typed, any: self }
    }

    /// Creates a slice from the vector.
    ///
    /// # Panics
    ///
    /// Panics if `T` it not the same type as the vector contains.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
    /// unsafe {
    ///     v.push(0_i32);
    ///     v.push(1_i32);
    /// }
    /// assert_eq!(v.as_slice::<i32>(), &[0, 1]);
    /// ```
    pub fn as_slice<T: 'static>(&self) -> &[T] {
        assert!(
            self.is_type_of::<T>(),
            "expected `{}`, but received `{}`",
            self.type_name(),
            any::type_name::<T>()
        );

        unsafe { slice::from_raw_parts(self.ptr.as_ptr() as *const T, self.len()) }
    }

    /// Creates a mutable slice from the vector.
    ///
    /// # Panics
    ///
    /// Panics if `T` it not the same type as the vector contains.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::AnyVec};
    ///
    /// let mut v = AnyVec::new(tinfo!(i32));
    /// unsafe { v.push(0_i32) };
    /// let slice = v.as_mut_slice::<i32>();
    /// assert_eq!(slice, &mut [0]);
    /// ```
    pub fn as_mut_slice<T: 'static>(&mut self) -> &mut [T] {
        assert!(
            self.is_type_of::<T>(),
            "expected `{}`, but received `{}`",
            self.type_name(),
            any::type_name::<T>()
        );

        unsafe { slice::from_raw_parts_mut(self.ptr.as_ptr() as *mut T, self.len()) }
    }

    /// `index` is an index of T, not u8.
    ///
    /// # Safety
    ///
    /// `index` must be inbound and result address must not overflow `isize`.
    pub(crate) const unsafe fn get_ptr(&self, index: usize) -> *mut u8 {
        let offset = index * self.item_size();
        unsafe { self.ptr.as_ptr().add(offset) }
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

        self.ptr = unsafe { NonNull::new_unchecked(ptr) };
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
        unsafe fn fn_iter(this: NonNull<u8>, chunk_idx: usize) -> RawIter {
            if chunk_idx == 0 {
                let this = unsafe { this.cast::<AnyVec>().as_ref() };
                this.iter()
            } else {
                RawIter::empty()
            }
        }
        unsafe fn fn_find(this: NonNull<u8>, item_idx: usize) -> (RawIter, usize, usize) {
            let this = unsafe { this.cast::<AnyVec>().as_ref() };
            (this.iter(), 0, item_idx)
        }
        // If ZST, alignment will become stride.
        let stride = self.item_size().max(self.align());
        let len = self.len();

        unsafe {
            let this = (self as *const Self as *const u8).cast_mut();
            let this = NonNull::new_unchecked(this);
            let chunks = 1;
            FlatRawIter::new(this, chunks, fn_iter as _, fn_find as _, stride, len)
        }
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
            let fn_clone = self.fn_clone();
            while offset < size {
                unsafe {
                    let src = self.ptr.as_ptr().add(offset);
                    let dst = ptr.add(offset);
                    // If data type doesn't support clone, panics here.
                    fn_clone(src, dst);
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
        unsafe {
            // Safety: Alignment must be at least 1.
            let stride = NonZeroUsize::new_unchecked(self.item_size().max(self.align()));
            RawIter::new(self.ptr, self.len(), stride)
        }
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
    type Iter = ParRawIter;
    type Item = SendSyncPtr<u8>;

    fn par_iter(&'data self) -> Self::Iter {
        AsRawIter::par_iter(self)
    }
}

/// A [`Vec`]-like type you can get from [`AnyVec`].
///
/// This struct implements [`Deref`] and [`DerefMut`] for `Vec<T>`, so that you
/// can access the struct as `Vec<T>`. When this struct is dropped, any changes
/// you've made are reflected to the `AnyVec`.
pub struct TypedAnyVec<'a, T> {
    typed: Vec<T>,
    any: &'a mut AnyVec,
}

impl<T> Deref for TypedAnyVec<'_, T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.typed
    }
}

impl<T> DerefMut for TypedAnyVec<'_, T> {
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
    fn test_any_vec_clone() {
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
    fn test_any_vec_non_cloneable_panic() {
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
    fn test_any_vec_drop() {
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
    fn test_any_vec_push_pop() {
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
    fn test_any_vec_remove() {
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
    fn test_any_vec_resize() {
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
    fn test_any_vec_push_incorrect_type_panic() {
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
    fn test_any_vec_pop_incorrect_type_panic() {
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
    fn test_any_vec_into_vec_push_pop() {
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
    fn test_any_vec_zst() {
        // Safety: Type is correct.
        unsafe {
            let mut v = AnyVec::new(crate::tinfo!(()));
            assert!(v.is_empty());
            assert_eq!(v.capacity(), usize::MAX);

            // Pushing ZST must be possible, and length must be grown by
            // pushing.
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

            // Shrinking capacity for ZST in the vector must have no effects.
            v.shrink_to_fit();
            assert_eq!(v.capacity(), usize::MAX);
        }
    }
}
