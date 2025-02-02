use super::ptr::SendSyncPtr;
use crate::util::macros::{
    impl_into_iterator_for_parallel, impl_parallel_iterator, impl_unindexed_producer,
};
use rayon::iter::{plumbing::Producer, IntoParallelIterator};
use std::{
    iter,
    marker::PhantomData,
    num::NonZeroUsize,
    ops::{Deref, DerefMut},
    ptr::NonNull,
    slice,
};

/// A trait to create type-erased iterators.
///
/// Type-erased iterator [`RawIter`] is commonly used in type-erased data
/// containers. Also, the iterators can become parallel iterators that can be
/// split over multiple CPU cores for parallel comuputing.
pub trait AsRawIter {
    /// Returns a new [`RawIter`].
    ///
    /// Give [`par_iter`](`AsRawIter::par_iter`) a try if you need CPU parallel
    /// computing.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::{AnyVec, AsRawIter}};
    ///
    /// // `AnyVec` implements `AsRawIter`.
    /// let v = AnyVec::new(tinfo!(i32));
    /// let iter = v.iter();
    /// ```
    fn iter(&self) -> RawIter;

    /// Returns a new [`ParRawIter`].
    ///
    /// Unlike [`RawIter`], this iterator can be split over multiple CPU cores
    /// then consumed at the same time for performance. But note that this is
    /// not a silver bullet. It could be worse than sequential iterator
    /// `RawIter` for a short iteration.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::{AnyVec, AsRawIter}};
    ///
    /// // `AnyVec` implements `AsRawIter`.
    /// let v = AnyVec::new(tinfo!(i32));
    /// let iter = v.par_iter();
    /// ```
    #[inline]
    fn par_iter(&self) -> ParRawIter {
        ParRawIter(self.iter())
    }

    /// Returns a new iterator.
    ///
    /// # Safety
    ///
    /// The given type `T` must be proper type.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::{AnyVec, AsRawIter}};
    ///
    /// // `AnyVec` implements `AsRawIter`.
    /// let mut v = AnyVec::new(tinfo!(i32));
    /// unsafe {
    ///     v.push(0_i32);
    ///     v.push(1_i32);
    /// }
    /// for v in unsafe { AsRawIter::iter_of::<i32>(&v) } {
    ///     println!("{v}");
    /// }
    /// ```
    #[inline]
    unsafe fn iter_of<T>(&self) -> Iter<'_, T> {
        unsafe { Iter::new(self) }
    }

    /// Returns a new mutable iterator.
    ///
    /// # Safety
    ///
    /// The given type `T` must be proper type.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::{AnyVec, AsRawIter}};
    ///
    /// // `AnyVec` implements `AsRawIter`.
    /// let mut v = AnyVec::new(tinfo!(i32));
    /// unsafe {
    ///     v.push(0_i32);
    ///     v.push(1_i32);
    /// }
    /// for v in unsafe { AsRawIter::iter_mut_of::<i32>(&mut v) } {
    ///     *v += 1;
    ///     println!("{v}");
    /// }
    /// ```
    #[inline]
    unsafe fn iter_mut_of<T>(&mut self) -> IterMut<'_, T> {
        unsafe { IterMut::new(self) }
    }

    /// Returns a new parallel iterator.
    ///
    /// # Safety
    ///
    /// The given type `T` must be proper type.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::{AnyVec, AsRawIter}};
    ///
    /// // `AnyVec` implements `AsRawIter`.
    /// let mut v = AnyVec::new(tinfo!(i32));
    /// unsafe {
    ///     v.push(0_i32);
    ///     v.push(1_i32);
    /// }
    /// for v in unsafe { AsRawIter::par_iter_of::<i32>(&v) } {
    ///     println!("{v}");
    /// }
    /// ```
    #[inline]
    unsafe fn par_iter_of<T>(&self) -> ParIter<'_, T> {
        unsafe { ParIter::new(self) }
    }

    /// Returns a new parallel mutable iterator.
    ///
    /// # Safety
    ///
    /// The given type `T` must be proper type.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::{AnyVec, AsRawIter}};
    ///
    /// // `AnyVec` implements `AsRawIter`.
    /// let mut v = AnyVec::new(tinfo!(i32));
    /// unsafe {
    ///     v.push(0_i32);
    ///     v.push(1_i32);
    /// }
    /// for v in unsafe { AsRawIter::par_iter_mut_of::<i32>(&mut v) } {
    ///     *v += 1;
    ///     println!("{v}");
    /// }
    /// ```
    #[inline]
    unsafe fn par_iter_mut_of<T>(&mut self) -> ParIterMut<'_, T> {
        ParIterMut::new(self)
    }
}

impl<T> AsRawIter for [T] {
    fn iter(&self) -> RawIter {
        let ptr = self.as_ptr();
        let len = self.len();
        unsafe {
            // Safety: Infallible.
            let ptr = NonNull::new_unchecked(ptr.cast_mut());

            // Safety: Alignment must be at least 1.
            let stride = NonZeroUsize::new_unchecked(size_of::<T>().max(align_of::<T>()));

            // Safety: slice guarantees.
            RawIter::new(ptr.cast(), len, stride)
        }
    }
}

/// A trait to create type-erased flatten iterators from nested types such as
/// `Vec<Vec<T>>`.
///
/// Type-erased iterator [`FlatRawIter`] is commonly used in type-erased data
/// containers. Also, the iterators can become parallel iterators that can be
/// split over mupltiple CPU cores for parallel computing.
pub trait AsFlatRawIter {
    /// Returns a new [`FlatRawIter`].
    ///
    /// # Safety
    ///
    /// - The created iterator's lifetime must rely on the `&self`.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::{ChunkAnyVec, AsFlatRawIter}};
    ///
    /// // `ChunkAnyVec` implements `AsFlatRawIter`.
    /// let chunk_cap = 2;
    /// let v = ChunkAnyVec::new(tinfo!(i32), chunk_cap);
    /// let iter = unsafe { v.iter() };
    /// ```
    //
    // Unlike `AsRawIter::iter`, this method need the container to be accessible
    // even if the iterator yields poniters. That's why this method is unsafe.
    unsafe fn iter(&self) -> FlatRawIter;

    /// Returns a new [`ParFlatRawIter`].
    ///
    /// # Safety
    ///
    /// - The created iterator's lifetime must rely on the `&self`.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::{ChunkAnyVec, AsFlatRawIter}};
    ///
    /// // `ChunkAnyVec` implements `AsFlatRawIter`.
    /// let chunk_cap = 2;
    /// let v = ChunkAnyVec::new(tinfo!(i32), chunk_cap);
    /// let iter = unsafe { v.par_iter() };
    /// ```
    //
    // Unlike `AsRawIter::par_iter`, this method need the container to be
    // accessible even if the iterator yields poniters. That's why this method
    // is unsafe.
    unsafe fn par_iter(&self) -> ParFlatRawIter {
        ParFlatRawIter(self.iter())
    }

    /// Returns a new iterator.
    ///
    /// # Safety
    ///
    /// The given type `T` must be proper type.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::{ChunkAnyVec, AsFlatRawIter}};
    ///
    /// // `ChunkAnyVec` implements `AsFlatRawIter`.
    /// let chunk_cap = 2;
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), chunk_cap);
    /// unsafe {
    ///     v.push(0_i32);
    ///     v.push(1_i32);
    /// }
    /// for v in unsafe { AsFlatRawIter::iter_of::<i32>(&v) } {
    ///     println!("{v}");
    /// }
    /// ```
    #[inline]
    unsafe fn iter_of<T>(&self) -> FlatIter<'_, T> {
        FlatIter::new(self)
    }

    /// Returns a new mutable iterator.
    ///
    /// # Safety
    ///
    /// The given type `T` must be proper type.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::{ChunkAnyVec, AsFlatRawIter}};
    ///
    /// // `ChunkAnyVec` implements `AsFlatRawIter`.
    /// let chunk_cap = 2;
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), chunk_cap);
    /// unsafe {
    ///     v.push(0_i32);
    ///     v.push(1_i32);
    /// }
    /// for v in unsafe { AsFlatRawIter::iter_mut_of::<i32>(&mut v) } {
    ///     *v += 1;
    ///     println!("{v}");
    /// }
    /// ```
    #[inline]
    unsafe fn iter_mut_of<T>(&mut self) -> FlatIterMut<'_, T> {
        FlatIterMut::new(self)
    }

    /// Returns a new parallel iterator.
    ///
    /// # Safety
    ///
    /// The given type `T` must be proper type.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::{ChunkAnyVec, AsFlatRawIter}};
    ///
    /// // `ChunkAnyVec` implements `AsFlatRawIter`.
    /// let chunk_cap = 2;
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), chunk_cap);
    /// unsafe {
    ///     v.push(0_i32);
    ///     v.push(1_i32);
    /// }
    /// for v in unsafe { AsFlatRawIter::par_iter_of::<i32>(&v) } {
    ///     println!("{v}");
    /// }
    /// ```
    #[inline]
    unsafe fn par_iter_of<T>(&self) -> ParFlatIter<'_, T> {
        ParFlatIter(self.iter_of())
    }

    /// Returns a new parallel mutable iterator.
    ///
    /// # Safety
    ///
    /// The given type `T` must be proper type.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::{ChunkAnyVec, AsFlatRawIter}};
    ///
    /// // `ChunkAnyVec` implements `AsFlatRawIter`.
    /// let chunk_cap = 2;
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), chunk_cap);
    /// unsafe {
    ///     v.push(0_i32);
    ///     v.push(1_i32);
    /// }
    /// for v in unsafe { AsFlatRawIter::par_iter_mut_of::<i32>(&mut v) } {
    ///     *v += 1;
    ///     println!("{v}");
    /// }
    /// ```
    #[inline]
    unsafe fn par_iter_mut_of<T>(&mut self) -> ParFlatIterMut<'_, T> {
        ParFlatIterMut(self.iter_mut_of())
    }
}

/// A type-erased iterator of a slice.
///
/// This iterator yields type-erased raw pointer instead of concrete type. If
/// you can, wrap this iterator in [`Iter`] which bounds concrete type and
/// lifetime.
///
/// # Safety
///
/// The iterator includes raw pointers, but it implements [`Send`] ans [`Sync`].
/// You must carefully send this iterator to another worker.
#[derive(Debug, Clone, Copy)]
pub struct RawIter {
    cur: SendSyncPtr<u8>,
    end: SendSyncPtr<u8>,
    stride: NonZeroUsize,
}

impl RawIter {
    /// Creates a new [`RawIter`] from start address, number of items, and
    /// stride in bytes.
    ///
    /// # Safety
    ///
    /// `ptr + len * stride` must not exceed [`isize::MAX`].
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::RawIter;
    /// use std::{ptr::NonNull, num::NonZeroUsize};
    ///
    /// let arr = [0, 1, 2];
    /// let ptr = NonNull::new(arr.as_ptr().cast_mut()).unwrap();
    /// let stride = NonZeroUsize::new(4).unwrap();
    /// let iter = unsafe { RawIter::new(ptr.cast(), arr.len(), stride) };
    /// ```
    #[inline]
    pub unsafe fn new(ptr: NonNull<u8>, len: usize, stride: NonZeroUsize) -> Self {
        unsafe {
            let end = ptr.add(len * stride.get());
            Self::from_range(ptr, end, stride)
        }
    }

    /// Creates a new [`RawIter`] from start address, end address, and stride
    /// in bytes.
    ///
    /// # Safety
    ///
    /// - `start` address must be less than or equal to `end` address.
    /// - `end` address must not exceed [`isize::MAX`].
    /// - `end` address must be able to devided by `stride`.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::RawIter;
    /// use std::{ptr::NonNull, num::NonZeroUsize};
    ///
    /// let arr = [0, 1, 2];
    /// let range = arr.as_ptr_range();
    /// let start = NonNull::new(range.start.cast_mut()).unwrap();
    /// let end = NonNull::new(range.end.cast_mut()).unwrap();
    /// let stride = NonZeroUsize::new(4).unwrap();
    /// let iter = unsafe { RawIter::from_range(start.cast(), end.cast(), stride) };
    /// ```
    #[inline]
    pub unsafe fn from_range(start: NonNull<u8>, end: NonNull<u8>, stride: NonZeroUsize) -> Self {
        debug_assert!(start <= end);
        debug_assert!(end.as_ptr() as usize <= isize::MAX as usize);
        debug_assert_eq!(
            (end.as_ptr() as usize - start.as_ptr() as usize) % stride,
            0
        );

        Self {
            cur: SendSyncPtr::new(start),
            end: SendSyncPtr::new(end),
            stride,
        }
    }

    /// Creates an empty [`RawIter`].
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::RawIter;
    ///
    /// let empty_iter = RawIter::empty();
    /// ```
    pub const fn empty() -> Self {
        Self {
            cur: SendSyncPtr::dangling(),
            end: SendSyncPtr::dangling(),
            stride: NonZeroUsize::MIN,
        }
    }

    /// Returns number of remaining items the iterator can visit.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::RawIter;
    /// use std::{ptr::NonNull, num::NonZeroUsize};
    ///
    /// let arr = [0, 1, 2];
    /// let ptr = NonNull::new(arr.as_ptr().cast_mut()).unwrap();
    /// let stride = NonZeroUsize::new(4).unwrap();
    /// let iter = unsafe { RawIter::new(ptr.cast(), arr.len(), stride) };
    /// assert_eq!(iter.len(), 3);
    /// ```
    #[inline]
    pub const fn len(&self) -> usize {
        let end = self.end.as_ptr();
        let cur = self.cur.as_ptr();
        // Safety: Owners guarantee safety.
        unsafe { end.offset_from(cur) as usize / self.stride.get() }
    }

    /// Returns true if the iterator has consumed completely so that it cannot
    /// yield anything.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::RawIter;
    /// use std::{ptr::NonNull, num::NonZeroUsize};
    ///
    /// let arr: [i32; 0] = [];
    /// let ptr = NonNull::new(arr.as_ptr().cast_mut()).unwrap();
    /// let stride = NonZeroUsize::new(4).unwrap();
    /// let mut iter = unsafe { RawIter::new(ptr.cast(), arr.len(), stride) };
    /// assert!(iter.is_empty());
    /// ```
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns a slice from the remaining iterator.
    ///
    /// # Safety
    ///
    /// - Caller must provide correct type `T` for the iterator.
    /// - Lifetime define by caller must be sufficient to the iterator.
    pub unsafe fn as_slice<'o, T>(&self) -> &'o [T] {
        // We need properly aligned pointer for the type `T`, not `u8`.
        let ptr = if self.cur.is_dangling() {
            NonNull::<T>::dangling().as_ptr().cast_const()
        } else {
            self.cur.as_ptr().cast::<T>().cast_const()
        };
        slice::from_raw_parts(ptr, self.len())
    }

    /// Returns a mutable slice from the remaining iterator.
    ///
    /// # Safety
    ///
    /// - Caller must provide correct type `T` for the iterator.
    /// - Lifetime define by caller must be sufficient to the iterator.
    pub unsafe fn as_mut_slice<'o, T>(&mut self) -> &'o mut [T] {
        // We need properly aligned pointer for the type `T`, not `u8`.
        let ptr = if self.cur.is_dangling() {
            NonNull::<T>::dangling().as_ptr()
        } else {
            self.cur.as_ptr().cast::<T>()
        };
        slice::from_raw_parts_mut(ptr, self.len())
    }
}

impl Iterator for RawIter {
    type Item = SendSyncPtr<u8>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.cur < self.end {
            let res = self.cur;
            // Safety: Owners guarantee safety.
            self.cur = unsafe { self.cur.add(self.stride.get()) };
            Some(res)
        } else {
            None
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = Self::len(self);
        (len, Some(len))
    }
}

impl iter::FusedIterator for RawIter {}

impl ExactSizeIterator for RawIter {
    #[inline]
    fn len(&self) -> usize {
        Self::len(self)
    }
}

impl DoubleEndedIterator for RawIter {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.cur < self.end {
            // Safety: Owners guarantee safety.
            self.end = unsafe { self.end.sub(self.stride.get()) };
            Some(self.end)
        } else {
            None
        }
    }
}

/// A type-erased parallel iterator of a slice.
///
/// This is a new type of [`RawIter`]. [`Iterator`] and
/// [`rayon::iter::ParallelIterator`] have the same signature methods, So
/// clients have to write fully-qualified syntax to specify a method. This new
/// type helps clients avoid it. If you don't want parallelism, just use
/// [`RawIter`].
#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct ParRawIter(pub RawIter);

impl ParRawIter {
    /// Converts the parallel iterator into sequential iterator by unwrapping
    /// self.
    #[inline]
    pub const fn into_seq(self) -> RawIter {
        self.0
    }

    /// Returns remaining length of the iterator.
    #[inline]
    pub const fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns true if the iterator has consumed completely so that it cannot
    /// yield anything.
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Deref for ParRawIter {
    type Target = RawIter;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ParRawIter {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Producer for ParRawIter {
    type Item = SendSyncPtr<u8>;
    type IntoIter = RawIter;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.into_seq()
    }

    #[inline]
    fn split_at(self, index: usize) -> (Self, Self) {
        let l_cur = self.cur;
        let l_end = unsafe { self.cur.add(index * self.stride.get()) };
        let r_cur = l_end;
        let r_end = self.end;

        // Safety: Splitting is safe.
        let (l, r) = unsafe {
            (
                RawIter::from_range(l_cur.as_nonnull(), l_end.as_nonnull(), self.stride),
                RawIter::from_range(r_cur.as_nonnull(), r_end.as_nonnull(), self.stride),
            )
        };
        (ParRawIter(l), ParRawIter(r))
    }
}

impl_into_iterator_for_parallel!(
    "for" = ParRawIter; "to" = RawIter; "item" = SendSyncPtr<u8>;
);
impl_parallel_iterator!(
    "for" = ParRawIter; "item" = SendSyncPtr<u8>;
);
impl_unindexed_producer!(
    "for" = ParRawIter; "item" = SendSyncPtr<u8>;
);

/// An iterator that can be created from [`RawIter`] with concrete type and
/// lifetime.
///
/// This iterator is useful when you have `RawIter` and know what type the
/// iterator visits. You can create this iterator from the `RawIter` then use it
/// like normal Rust iterators.
#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct Iter<'cont, T: 'cont> {
    inner: RawIter,
    _marker: PhantomData<&'cont T>,
}

impl<'cont, T> Iter<'cont, T> {
    /// Borrows the given container then returns an iterator of the container.
    ///
    /// # Safety
    ///
    /// Given container must contain type `T` items.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::Iter;
    ///
    /// let arr = &[1, 2, 3][..]; // [T] implements `AsRawIter`.
    /// let iter = unsafe { Iter::<i32>::new(arr) };
    /// ```
    #[inline]
    pub unsafe fn new<C>(cont: &'cont C) -> Self
    where
        C: AsRawIter + ?Sized,
    {
        // We're borrowing container, so lifetime is tied up.
        unsafe { Self::from_raw(cont.iter()) }
    }

    /// Creates a new [`Iter`] from the given [`RawIter`].
    ///
    /// # Safety
    ///
    /// - The `RawIter` must yield pointers to type `T`.
    /// - Lifetime defined by caller must be sufficient to the `RawIter`.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::{RawIter, Iter};
    /// use std::{ptr::NonNull, num::NonZeroUsize};
    ///
    /// let arr = [0, 1, 2];
    /// let ptr = NonNull::new(arr.as_ptr().cast_mut()).unwrap();
    /// let stride = NonZeroUsize::new(4).unwrap();
    /// unsafe {
    ///     let raw_iter = RawIter::new(ptr.cast(), arr.len(), stride);
    ///     let iter = Iter::<i32>::from_raw(raw_iter);
    /// }
    /// ```
    #[inline]
    pub const unsafe fn from_raw(raw: RawIter) -> Self {
        Self {
            inner: raw,
            _marker: PhantomData,
        }
    }

    /// Returns number of remaining items the iterator can visit.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::Iter;
    ///
    /// let arr = &[1, 2, 3][..]; // [T] implements `AsRawIter`.
    /// let iter = unsafe { Iter::<i32>::new(arr) };
    /// assert_eq!(iter.len(), 3);
    /// ```
    #[inline]
    pub const fn len(&self) -> usize {
        self.inner.len()
    }

    /// Returns true if the iterator has been consumed completely so that it
    /// cannot yield anything.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::Iter;
    ///
    /// let arr: &[i32] = &[][..]; // [T] implements `AsRawIter`.
    /// let mut iter = unsafe { Iter::<i32>::new(arr) };
    /// assert!(iter.is_empty());
    /// ```
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<'cont, T> Iterator for Iter<'cont, T> {
    type Item = &'cont T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|ptr| unsafe { ptr.cast().as_ref() })
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = Self::len(self);
        (len, Some(len))
    }
}

impl<T> iter::FusedIterator for Iter<'_, T> {}

impl<T> ExactSizeIterator for Iter<'_, T> {
    #[inline]
    fn len(&self) -> usize {
        Self::len(self)
    }
}

impl<T> DoubleEndedIterator for Iter<'_, T> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner
            .next_back()
            .map(|ptr| unsafe { ptr.cast().as_ref() })
    }
}

/// A mutable iterator that can be created from [`RawIter`] with concrete type
/// and lifetime.
///
/// This iterator is useful when you have `RawIter` and know what type the
/// iterator visits. You can create this iterator from the `RawIter` then use it
/// like normal Rust iterators.
#[derive(Debug)]
#[repr(transparent)]
pub struct IterMut<'cont, T: 'cont> {
    inner: RawIter,
    _marker: PhantomData<&'cont mut T>,
}

impl<'cont, T> IterMut<'cont, T> {
    /// Borrows the given container then returns a mutable iterator of the
    /// container.
    ///
    /// # Safety
    ///
    /// Given container must contain type `T` items.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IterMut;
    ///
    /// let arr = &mut [1, 2, 3][..]; // [T] implements `AsRawIter`.
    /// let iter = unsafe { IterMut::<i32>::new(arr) };
    /// ```
    #[inline]
    pub unsafe fn new<C>(cont: &'cont mut C) -> Self
    where
        C: AsRawIter + ?Sized,
    {
        // We're borrowing container, so lifetime is tied up.
        unsafe { Self::from_raw(cont.iter()) }
    }

    /// Create a new [`IterMut`] from the given [`RawIter`].
    ///
    /// # Safety
    ///
    /// - The `RawIter` must yield pointers to type `T`.
    /// - The `RawIter` must not be used elsewhere at the same time because
    ///   created iterator must be exclusive.
    /// - Lifetime defined by caller must be sufficient to the `RawIter`.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::{RawIter, IterMut};
    /// use std::{ptr::NonNull, num::NonZeroUsize};
    ///
    /// let arr = [0, 1, 2];
    /// let ptr = NonNull::new(arr.as_ptr().cast_mut()).unwrap();
    /// let stride = NonZeroUsize::new(4).unwrap();
    /// unsafe {
    ///     let raw_iter = RawIter::new(ptr.cast(), arr.len(), stride);
    ///     let iter = IterMut::<i32>::from_raw(raw_iter);
    /// }
    /// ```
    #[inline]
    pub unsafe fn from_raw(raw: RawIter) -> Self {
        Self {
            inner: raw,
            _marker: PhantomData,
        }
    }

    /// Returns number of remaining items the iterator can visit.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IterMut;
    ///
    /// let arr = &mut [1, 2, 3][..]; // [T] implements `AsRawIter`.
    /// let iter = unsafe { IterMut::<i32>::new(arr) };
    /// assert_eq!(iter.len(), 3);
    /// ```
    #[inline]
    pub const fn len(&self) -> usize {
        self.inner.len()
    }

    /// Returns true if the iterator has been consumed completely so that it
    /// cannot yield anything.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::IterMut;
    ///
    /// let arr: &mut [i32] = &mut [][..]; // [T] implements `AsRawIter`.
    /// let mut iter = unsafe { IterMut::<i32>::new(arr) };
    /// assert!(iter.is_empty());
    /// ```
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<'cont, T> Iterator for IterMut<'cont, T> {
    type Item = &'cont mut T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|ptr| unsafe { ptr.cast().as_mut() })
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = Self::len(self);
        (len, Some(len))
    }
}

impl<T> iter::FusedIterator for IterMut<'_, T> {}

impl<T> ExactSizeIterator for IterMut<'_, T> {
    #[inline]
    fn len(&self) -> usize {
        Self::len(self)
    }
}

impl<T> DoubleEndedIterator for IterMut<'_, T> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner
            .next_back()
            .map(|ptr| unsafe { ptr.cast().as_mut() })
    }
}

/// An iterator that can be created from [`ParRawIter`] with concrete type and
/// lifetime.
///
/// This iterator is useful when you have `ParRawIter` and know what type the
/// iterator visits. You can create this iterator from the `ParRawIter` then use
/// it like normal Rust iterators.
#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct ParIter<'cont, T: 'cont> {
    inner: ParRawIter,
    _marker: PhantomData<&'cont T>,
}

impl<'cont, T> ParIter<'cont, T> {
    /// Borrows the given container then returns an iterator of the container.
    ///
    /// # Safety
    ///
    /// Given container must contain type `T` items.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::ParIter;
    ///
    /// let arr = &[1, 2, 3][..]; // [T] implements `AsRawIter`.
    /// let iter = unsafe { ParIter::<i32>::new(arr) };
    /// ```
    #[inline]
    pub unsafe fn new<C>(cont: &'cont C) -> Self
    where
        C: AsRawIter + ?Sized,
    {
        // We're borrowing container, so lifetime is tied up.
        unsafe { Self::from_raw(cont.par_iter()) }
    }

    /// Creates a new [`ParIter`] from the given [`ParRawIter`].
    ///
    /// # Safety
    ///
    /// - The `ParRawIter` must yield pointers to type `T`.
    /// - Lifetime defined by caller must be sufficient to the `ParRawIter`.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::{RawIter, ParRawIter, ParIter};
    /// use std::{ptr::NonNull, num::NonZeroUsize};
    ///
    /// let arr = [0, 1, 2];
    /// let ptr = NonNull::new(arr.as_ptr().cast_mut()).unwrap();
    /// let stride = NonZeroUsize::new(4).unwrap();
    /// unsafe {
    ///     let raw_iter = RawIter::new(ptr.cast(), arr.len(), stride);
    ///     let par_raw_iter = ParRawIter(raw_iter);
    ///     let iter = ParIter::<i32>::from_raw(par_raw_iter);
    /// }
    /// ```
    #[inline]
    pub const unsafe fn from_raw(raw: ParRawIter) -> Self {
        Self {
            inner: raw,
            _marker: PhantomData,
        }
    }

    /// Converts the parallel iterator into sequential iterator.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::ParIter;
    ///
    /// let arr = &[1, 2, 3][..]; // [T] implements `AsRawIter`.
    /// let iter = unsafe { ParIter::<i32>::new(arr) };
    /// let seq_iter = iter.into_seq();
    /// ```
    #[inline]
    pub const fn into_seq(self) -> Iter<'cont, T> {
        // Safety: Correct type and lifetime are given.
        unsafe { Iter::from_raw(self.inner.into_seq()) }
    }

    /// Returns number of remaining items the iterator can visit.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::ParIter;
    ///
    /// let arr = &[1, 2, 3][..]; // [T] implements `AsRawIter`.
    /// let iter = unsafe { ParIter::<i32>::new(arr) };
    /// assert_eq!(iter.len(), 3);
    /// ```
    #[inline]
    pub const fn len(&self) -> usize {
        self.inner.len()
    }

    /// Returns true if the iterator has been consumed completely so that it
    /// cannot yield anything.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::ParIter;
    ///
    /// let arr: &[i32] = &[][..]; // [T] implements `AsRawIter`.
    /// let mut iter = unsafe { ParIter::<i32>::new(arr) };
    /// assert!(iter.is_empty());
    /// ```
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<'cont, T: Send + Sync + 'cont> Producer for ParIter<'cont, T> {
    type Item = &'cont T;
    type IntoIter = Iter<'cont, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.into_seq()
    }

    #[inline]
    fn split_at(self, index: usize) -> (Self, Self) {
        let (l, r) = self.inner.split_at(index);
        unsafe { (Self::from_raw(l), Self::from_raw(r)) }
    }
}

impl_into_iterator_for_parallel!(
    "lifetimes" = 'cont; "bounds" = T: {'cont};
    "for" = ParIter; "to" = Iter<'cont, T>; "item" = &'cont T;
);
impl_parallel_iterator!(
    "lifetimes" = 'cont; "bounds" = T: {Send + Sync + 'cont};
    "for" = ParIter; "item" = &'cont T;
);
impl_unindexed_producer!(
    "lifetimes" = 'cont; "bounds" = T: {Send + Sync + 'cont};
    "for" = ParIter; "item" = &'cont T;
);

/// A mutable iterator that can be created from [`ParRawIter`] with concrete
/// type and lifetime.
///
/// This iterator is useful when you have `ParRawIter` and know what type the
/// iterator visits. You can create this iterator from the `ParRawIter` then use
/// it like normal Rust iterators.
#[derive(Debug)]
#[repr(transparent)]
pub struct ParIterMut<'cont, T: 'cont> {
    inner: ParRawIter,
    _marker: PhantomData<&'cont mut T>,
}

impl<'cont, T> ParIterMut<'cont, T> {
    /// Borrows the given container then returns a mutable iterator of the
    /// container.
    ///
    /// # Safety
    ///
    /// Given container must contain type `T` items.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::ParIterMut;
    ///
    /// let arr = &mut [1, 2, 3][..]; // [T] implements `AsRawIter`.
    /// let iter = unsafe { ParIterMut::<i32>::new(arr) };
    /// ```
    #[inline]
    pub fn new<C>(cont: &'cont mut C) -> Self
    where
        C: AsRawIter + ?Sized,
    {
        // We're borrowing container, so lifetime is tied up.
        unsafe { Self::from_raw(cont.par_iter()) }
    }

    /// Creates a new [`ParIterMut`] from the given [`ParRawIter`].
    ///
    /// # Safety
    ///
    /// - The `ParRawIter` must yield pointers to type `T`.
    /// - The `ParRawIter` must not be used elsewhere at the same time because
    ///   created iterator must be exclusive.
    /// - Lifetime defined by caller must be sufficient to the `ParRawIter`.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::{RawIter, ParRawIter, ParIterMut};
    /// use std::{ptr::NonNull, num::NonZeroUsize};
    ///
    /// let arr = [0, 1, 2];
    /// let ptr = NonNull::new(arr.as_ptr().cast_mut()).unwrap();
    /// let stride = NonZeroUsize::new(4).unwrap();
    /// unsafe {
    ///     let raw_iter = RawIter::new(ptr.cast(), arr.len(), stride);
    ///     let par_raw_iter = ParRawIter(raw_iter);
    ///     let iter = ParIterMut::<i32>::from_raw(par_raw_iter);
    /// }
    /// ```
    #[inline]
    pub unsafe fn from_raw(raw: ParRawIter) -> Self {
        Self {
            inner: raw,
            _marker: PhantomData,
        }
    }

    /// Converts the parallel iterator into sequential iterator.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::ParIterMut;
    ///
    /// let arr = &mut [1, 2, 3][..]; // [T] implements `AsRawIter`.
    /// let iter = unsafe { ParIterMut::<i32>::new(arr) };
    /// let seq_iter = iter.into_seq();
    /// ```
    #[inline]
    pub fn into_seq(self) -> IterMut<'cont, T> {
        // Safety: Correct type and lifetime are given.
        unsafe { IterMut::from_raw(self.inner.into_seq()) }
    }

    /// Returns number of remaining items the iterator can visit.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::ParIterMut;
    ///
    /// let arr = &mut [1, 2, 3][..]; // [T] implements `AsRawIter`.
    /// let iter = unsafe { ParIterMut::<i32>::new(arr) };
    /// assert_eq!(iter.len(), 3);
    /// ```
    #[inline]
    pub const fn len(&self) -> usize {
        self.inner.len()
    }

    /// Returns true if the iterator has been consumed completely so that it
    /// cannot yield anything.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::ParIterMut;
    ///
    /// let arr: &mut [i32] = &mut [][..]; // [T] implements `AsRawIter`.
    /// let mut iter = unsafe { ParIterMut::<i32>::new(arr) };
    /// assert!(iter.is_empty());
    /// ```
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<'cont, T: Send + Sync + 'cont> Producer for ParIterMut<'cont, T> {
    type Item = &'cont mut T;
    type IntoIter = IterMut<'cont, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.into_seq()
    }

    #[inline]
    fn split_at(self, index: usize) -> (Self, Self) {
        let (l, r) = self.inner.split_at(index);
        (
            Self {
                inner: l,
                _marker: PhantomData,
            },
            Self {
                inner: r,
                _marker: PhantomData,
            },
        )
    }
}

impl_into_iterator_for_parallel!(
    "lifetimes" = 'cont; "bounds" = T: {'cont};
    "for" = ParIterMut; "to" = IterMut<'cont, T>; "item" = &'cont mut T;
);
impl_parallel_iterator!(
    "lifetimes" = 'cont; "bounds" = T: {Send + Sync + 'cont};
    "for" = ParIterMut; "item" = &'cont mut T;
);
impl_unindexed_producer!(
    "lifetimes" = 'cont; "bounds" = T: {Send + Sync + 'cont};
    "for" = ParIterMut; "item" = &'cont mut T;
);

/// A type-erased nested iterator.
///
/// This iterator is intended to be used by containers that has nested structure
/// like `Vec<Vec<T>>`. [`ChunkAnyVec`](crate::ds::ChunkAnyVec) is one of
/// containers that support this iterator. Plus, the iterator is using some
/// terminologies such as 'chunk' used in `ChunkAnyVec`, but you can think of it
/// as inner vector in `Vec<Vec<T>>`.
///
/// For better compiler optimization, the iterator is manually flattened, so it
/// is quite big in terms of size, but we can expect high iteration performance.
///
/// # Safety
///
/// The iterator includes raw pointers, but it implements [`Send`] ans [`Sync`].
/// You must carefully send this iterator to another worker.
//
// To implement rayon's ParallelIterator, we have to implement
// `ExactSizeIterator` and `DoubleEndedIterator`. So we track right side status
// and total number of items which are not needed for just `Iterator`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FlatRawIter {
    /// Left [RawIter::cur].
    ///
    /// When [Iterator::next] is called, this pointer may be returned.
    pub(crate) ll: SendSyncPtr<u8>,

    /// Left [RawIter::end], which is right boundary of the left iterator.
    ///
    /// Note that `end` is not included in the range. It's the next pointer of
    /// the right bound.
    pub(crate) lr: SendSyncPtr<u8>,

    /// Right [RawIter::cur], which is left boundary of the right iterator.
    pub(crate) rl: SendSyncPtr<u8>,

    /// Right [RawIter::end].
    ///
    /// When [DoubleEndedIterator::next_back] is called, the pointer before this
    /// may be returned. Note that `end` is not included in the range. It's the
    /// next pointer of the right bound.
    pub(crate) rr: SendSyncPtr<u8>,

    /// Pointer to the actual container.
    pub(crate) this: SendSyncPtr<u8>,

    /// Left chunk index for the next left [RawIter].
    ///
    /// This index always points to the next left iterator, not current one,
    /// and starts from 0.
    pub(crate) li: usize,

    /// Right chunk index for the current right [RawIter].
    ///
    /// This index always points to the current right iterator, and starts from
    /// number of chunks.
    pub(crate) ri: usize,

    /// Function that returns [`RawIter`] of a chunk for the given chunk index.
    ///
    /// If the chunk index is out of bounds, empty [`RawIter`] is returned.
    pub(crate) fn_iter: unsafe fn(this: NonNull<u8>, chunk_idx: usize) -> RawIter,

    /// Function that returns [`RawIter`], chunk index, and offset from an item
    /// index.
    ///
    /// If the item index is out of bounds, empty [`RawIter`] is returned.
    pub(crate) fn_find: unsafe fn(this: NonNull<u8>, item_idx: usize) -> (RawIter, usize, usize),

    /// Stride in bytes.
    pub(crate) stride: usize,

    // used for parallel.
    pub(crate) off: usize,

    /// Number of remaining items.
    pub(crate) len: usize,
}

impl FlatRawIter {
    /// Creates a new [`FlatRawIter`] from raw parts.
    ///
    /// * this - A pointer to a container.
    /// * chunks - Number of chunks of the container.
    /// * fn_iter - A function that returns [`RawIter`] for a chunk index.
    /// * stride - Stride in bytes.
    /// * len - Total number of items.
    ///
    /// # Safety
    ///
    /// Caller must guarantee that the created iterator must have valid access
    /// to the given pointer while it is in use.
    #[inline]
    pub const unsafe fn new(
        this: NonNull<u8>,
        chunks: usize,
        fn_iter: unsafe fn(this: NonNull<u8>, chunk_idx: usize) -> RawIter,
        fn_find: unsafe fn(this: NonNull<u8>, item_idx: usize) -> (RawIter, usize, usize),
        stride: usize,
        len: usize,
    ) -> Self {
        Self {
            ll: SendSyncPtr::dangling(),
            lr: SendSyncPtr::dangling(),
            rl: SendSyncPtr::dangling(),
            rr: SendSyncPtr::dangling(),
            this: SendSyncPtr::new(this),
            li: 0,
            ri: chunks,
            fn_iter,
            fn_find,
            stride,
            off: 0,
            len,
        }
    }

    /// Returns number of remaining items the iterator can visit.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::{ChunkAnyVec, FlatRawIter, AsFlatRawIter}};
    ///
    /// // `ChunkAnyVec` supports `FlatRawIter`.
    /// let chunk_cap = 2;
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), chunk_cap);
    /// unsafe { v.push(0_i32 ) };
    ///
    /// let iter: FlatRawIter = unsafe { v.iter() };
    /// assert_eq!(iter.len(), 1);
    /// ```
    #[inline]
    pub const fn len(&self) -> usize {
        self.len
    }

    /// Returns true if the iterator has consumed completely so that it cannot
    /// yield anything.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::{ChunkAnyVec, FlatRawIter, AsFlatRawIter}};
    ///
    /// // `ChunkAnyVec` supports `FlatRawIter`.
    /// let chunk_cap = 2;
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), chunk_cap);
    ///
    /// let iter: FlatRawIter = unsafe { v.iter() };
    /// assert!(iter.is_empty());
    /// ```
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns a slice from the remaining iterator.
    ///
    /// # Safety
    ///
    /// - Caller must provide correct type `T` for the iterator.
    /// - Lifetime define by caller must be sufficient to the iterator.
    pub unsafe fn as_slice<'o, T>(&self, chunk_idx: usize) -> &'o [T] {
        // `fn_iter` gives us empty iterator if `chunk` is out of bounds.
        let raw_iter = unsafe { (self.fn_iter)(self.this.as_nonnull(), chunk_idx) };
        raw_iter.as_slice()
    }

    /// Returns a mutable slice from the remaining iterator.
    ///
    /// # Safety
    ///
    /// - Caller must provide correct type `T` for the iterator.
    /// - Lifetime define by caller must be sufficient to the iterator.
    pub unsafe fn as_mut_slice<'o, T>(&mut self, chunk_idx: usize) -> &'o mut [T] {
        // `fn_iter` gives us empty iterator if `chunk` is out of bounds.
        let mut raw_iter = unsafe { (self.fn_iter)(self.this.as_nonnull(), chunk_idx) };
        raw_iter.as_mut_slice()
    }

    /// Returns number of chunks if you've not been called
    /// [`next_back`](Self::next_back) on this iterator.
    pub(crate) fn num_chunks(&self) -> usize {
        self.ri
    }
}

impl Iterator for FlatRawIter {
    type Item = SendSyncPtr<u8>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.ll == self.lr {
            (self.ll, self.lr) = if self.li < self.ri {
                // Safety: Owners guarantee validity.
                let next = unsafe { (self.fn_iter)(self.this.as_nonnull(), self.li) };
                (next.cur, next.end)
            } else {
                (self.rl, self.rr)
            };
            if self.ll == self.lr {
                return None;
            }
            self.li += 1;
        }

        let res = Some(self.ll);
        // Safety: Owners guarantee validity.
        self.ll = unsafe { self.ll.add(self.stride) };
        if self.li > self.ri {
            self.rl = self.ll;
        }
        self.len -= 1;
        res
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = Self::len(self);
        (len, Some(len))
    }
}

impl iter::FusedIterator for FlatRawIter {}

impl ExactSizeIterator for FlatRawIter {
    #[inline]
    fn len(&self) -> usize {
        Self::len(self)
    }
}

impl DoubleEndedIterator for FlatRawIter {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.rl == self.rr {
            (self.rl, self.rr) = if self.li < self.ri {
                // Safety: Owners guarantee validity.
                let next = unsafe { (self.fn_iter)(self.this.as_nonnull(), self.ri - 1) };
                (next.cur, next.end)
            } else {
                (self.ll, self.lr)
            };
            if self.rl == self.rr {
                return None;
            }
            self.ri -= 1;
        }

        // Safety: Owners guarantee validity.
        self.rr = unsafe { self.rr.sub(self.stride) };
        if self.li > self.ri {
            self.lr = self.rr;
        }
        self.len -= 1;
        Some(self.rr)
    }
}

/// A type-erased parallel nested iterator.
///
/// This is a new type of [`FlatRawIter`]. [`Iterator`] and
/// [`rayon::iter::ParallelIterator`] have the same signature methods, So
/// clients have to write fully-qualified syntax to specify a method. This new
/// type helps clients avoid it. If you don't want parallelism, just use
/// [`FlatRawIter`].
#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct ParFlatRawIter(pub FlatRawIter);

impl ParFlatRawIter {
    /// Converts the parallel iterator into sequential iterator by unwrapping
    /// self.
    #[inline]
    pub const fn into_seq(self) -> FlatRawIter {
        self.0
    }

    /// Returns remaining length of the iterator.
    #[inline]
    pub const fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns true if the iterator has consumed completely so that it cannot
    /// yield anything.
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Deref for ParFlatRawIter {
    type Target = FlatRawIter;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ParFlatRawIter {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Producer for ParFlatRawIter {
    type Item = SendSyncPtr<u8>;
    type IntoIter = FlatRawIter;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.into_seq()
    }

    #[inline]
    fn split_at(self, index: usize) -> (Self, Self) {
        let (
            RawIter {
                cur: ml, end: mr, ..
            },
            mi,
            off,
        ) = unsafe { (self.fn_find)(self.this.as_nonnull(), self.off + index) };
        let mm = unsafe { ml.add(off * self.stride) };

        // Basic idea to split is something like so,
        //
        // Left-chunk      Mid-chunk         Right-chunk
        //      li              mi                ri
        // [**********] .. [**********]  ..  [**********]
        // ^          ^    ^     ^    ^      ^          ^
        // ll         lr   ml    mm   mr     rl         rr
        // |          |    |     ||    \     |          |
        // [**********] .. [*****][*****] .. [**********]
        // |---- Left child -----||---- Right child ----|
        //
        // But, we must consider something like
        // - Imagine that mid-chunk is left chunk, but not split
        //   as depicted below.
        //
        // ml              mm   mr
        // v               v    v
        // [********************]
        //              [****]
        //              ^    ^
        //              ll   lr

        let is_left_chunk_cut = mi + 1 == self.li;
        let lchild = if !is_left_chunk_cut {
            FlatRawIter {
                ll: self.ll,
                lr: self.lr,
                rl: ml,
                rr: mm,
                this: self.this,
                li: self.li,
                ri: mi,
                fn_iter: self.fn_iter,
                fn_find: self.fn_find,
                stride: self.stride,
                off: self.off,
                len: index,
            }
        } else {
            FlatRawIter {
                ll: self.ll,
                lr: mm,
                rl: self.ll,
                rr: mm,
                this: self.this,
                li: mi + 1,
                ri: mi,
                fn_iter: self.fn_iter,
                fn_find: self.fn_find,
                stride: self.stride,
                off: self.off,
                len: index,
            }
        };

        let is_right_chunk_cut = mi == self.ri;
        let rchild = if !is_right_chunk_cut {
            FlatRawIter {
                ll: mm,
                lr: mr,
                rl: self.rl,
                rr: self.rr,
                this: self.this,
                li: mi + 1,
                ri: self.ri,
                fn_iter: self.fn_iter,
                fn_find: self.fn_find,
                stride: self.stride,
                off: self.off + index,
                len: self.len - index,
            }
        } else {
            FlatRawIter {
                ll: mm,
                lr: self.rr,
                rl: mm,
                rr: self.rr,
                this: self.this,
                li: mi + 1,
                ri: mi,
                fn_iter: self.fn_iter,
                fn_find: self.fn_find,
                stride: self.stride,
                off: self.off + index,
                len: self.len - index,
            }
        };

        (ParFlatRawIter(lchild), ParFlatRawIter(rchild))
    }
}

impl_into_iterator_for_parallel!(
    "for" = ParFlatRawIter; "to" = FlatRawIter; "item" = SendSyncPtr<u8>;
);
impl_parallel_iterator!(
    "for" = ParFlatRawIter; "item" = SendSyncPtr<u8>;
);
impl_unindexed_producer!(
    "for" = ParFlatRawIter; "item" = SendSyncPtr<u8>;
);

/// An iterator that can be created from [`FlatRawIter`] with concrete type and
/// lifetime.
///
/// This iterator is useful when you have `FlatRawIter` and know what type the
/// iterator visits. You can create this iterator from the `FlatRawIter` then
/// use it like normal Rust iterators.
#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct FlatIter<'cont, T: 'cont> {
    inner: FlatRawIter,
    _marker: PhantomData<&'cont T>,
}

impl<'cont, T> FlatIter<'cont, T> {
    /// Borrows the given container then returns an iterator of the container.
    ///
    /// # Safety
    ///
    /// Given container must contain type `T` items.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::{ChunkAnyVec, FlatIter}};
    ///
    /// // `ChunkAnyVec` implements `AsFlatRawIter`.
    /// let chunk_cap = 2;
    /// let v = ChunkAnyVec::new(tinfo!(i32), chunk_cap);
    /// let iter = FlatIter::<i32>::new(&v);
    /// ```
    #[inline]
    pub fn new<C>(cont: &'cont C) -> Self
    where
        C: AsFlatRawIter + ?Sized,
    {
        // We're borrowing container, so lifetime is tied up.
        unsafe { Self::from_raw(cont.iter()) }
    }

    /// Creates a new [`FlatIter`] from the given [`FlatRawIter`].
    ///
    /// # Safety
    ///
    /// - The `FlatRawIter` must yield pointers to type `T`.
    /// - Lifetime defined by caller must be sufficient to the `FlatRawIter`.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::{ChunkAnyVec, AsFlatRawIter, FlatIter}};
    ///
    /// // `ChunkAnyVec` implements `AsFlatRawIter`.
    /// let chunk_cap = 2;
    /// let v = ChunkAnyVec::new(tinfo!(i32), chunk_cap);
    /// unsafe {
    ///     let raw_iter = v.iter();
    ///     let iter = FlatIter::<i32>::from_raw(raw_iter);
    /// }
    /// ```
    #[inline]
    pub const unsafe fn from_raw(raw: FlatRawIter) -> Self {
        Self {
            inner: raw,
            _marker: PhantomData,
        }
    }

    /// Returns number of remaining items the iterator can visit.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::{ChunkAnyVec, FlatIter}};
    ///
    /// // `ChunkAnyVec` implements `AsFlatRawIter`.
    /// let chunk_cap = 2;
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), chunk_cap);
    /// unsafe { v.push(0_i32) };
    ///
    /// let iter = FlatIter::<i32>::new(&v);
    /// assert_eq!(iter.len(), 1);
    /// ```
    #[inline]
    pub const fn len(&self) -> usize {
        self.inner.len()
    }

    /// Returns true if the iterator has been consumed completely so that it
    /// cannot yield anything.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::{ChunkAnyVec, FlatIter}};
    ///
    /// // `ChunkAnyVec` implements `AsFlatRawIter`.
    /// let chunk_cap = 2;
    /// let v = ChunkAnyVec::new(tinfo!(i32), chunk_cap);
    ///
    /// let iter = FlatIter::<i32>::new(&v);
    /// assert!(iter.is_empty());
    /// ```
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<'cont, T> Iterator for FlatIter<'cont, T> {
    type Item = &'cont T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|ptr| unsafe { ptr.cast().as_ref() })
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = Self::len(self);
        (len, Some(len))
    }
}

impl<T> iter::FusedIterator for FlatIter<'_, T> {}

impl<T> ExactSizeIterator for FlatIter<'_, T> {
    #[inline]
    fn len(&self) -> usize {
        Self::len(self)
    }
}

impl<T> DoubleEndedIterator for FlatIter<'_, T> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner
            .next_back()
            .map(|ptr| unsafe { ptr.cast().as_ref() })
    }
}

/// A mutable iterator that can be created from [`FlatRawIter`] with concrete
/// type and lifetime.
///
/// This iterator is useful when you have `FlatRawIter` and know what type the
/// iterator visits. You can create this iterator from the `FlatRawIter` then
/// use it like normal Rust iterators.
#[derive(Debug)]
#[repr(transparent)]
pub struct FlatIterMut<'cont, T: 'cont> {
    inner: FlatRawIter,
    _marker: PhantomData<&'cont mut T>,
}

impl<'cont, T> FlatIterMut<'cont, T> {
    /// Borrows the given container then returns an iterator of the container.
    ///
    /// # Safety
    ///
    /// Given container must contain type `T` items.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::{ChunkAnyVec, FlatIterMut}};
    ///
    /// // `ChunkAnyVec` implements `AsFlatRawIter`.
    /// let chunk_cap = 2;
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), chunk_cap);
    /// let iter = FlatIterMut::<i32>::new(&mut v);
    /// ```
    #[inline]
    pub fn new<C>(cont: &'cont mut C) -> Self
    where
        C: AsFlatRawIter + ?Sized,
    {
        // We're borrowing container, so lifetime is tied up.
        unsafe { Self::from_raw(cont.iter()) }
    }

    /// Creates a new [`FlatIterMut`] from the given [`FlatRawIter`].
    ///
    /// # Safety
    ///
    /// - The `FlatRawIter` must yield pointers to type `T`.
    /// - The `FlatRawIter` must not be used elsewhere at the same time because
    ///   created iterator must be exclusive.
    /// - Lifetime defined by caller must be sufficient to the `FlatRawIter`.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::{ChunkAnyVec, AsFlatRawIter, FlatIterMut}};
    ///
    /// // `ChunkAnyVec` implements `AsFlatRawIter`.
    /// let chunk_cap = 2;
    /// let v = ChunkAnyVec::new(tinfo!(i32), chunk_cap);
    /// unsafe {
    ///     let raw_iter = v.iter();
    ///     let iter = FlatIterMut::<i32>::from_raw(raw_iter);
    /// }
    /// ```
    #[inline]
    pub unsafe fn from_raw(raw: FlatRawIter) -> Self {
        Self {
            inner: raw,
            _marker: PhantomData,
        }
    }

    /// Returns number of remaining items the iterator can visit.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::{ChunkAnyVec, FlatIterMut}};
    ///
    /// // `ChunkAnyVec` implements `AsFlatRawIter`.
    /// let chunk_cap = 2;
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), chunk_cap);
    /// unsafe { v.push(0_i32) };
    ///
    /// let iter = FlatIterMut::<i32>::new(&mut v);
    /// assert_eq!(iter.len(), 1);
    /// ```
    #[inline]
    pub const fn len(&self) -> usize {
        self.inner.len()
    }

    /// Returns true if the iterator has been consumed completely so that it
    /// cannot yield anything.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::{tinfo, ds::{ChunkAnyVec, FlatIterMut}};
    ///
    /// // `ChunkAnyVec` implements `AsFlatRawIter`.
    /// let chunk_cap = 2;
    /// let mut v = ChunkAnyVec::new(tinfo!(i32), chunk_cap);
    ///
    /// let iter = FlatIterMut::<i32>::new(&mut v);
    /// assert!(iter.is_empty());
    /// ```
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<'cont, T> Iterator for FlatIterMut<'cont, T> {
    type Item = &'cont mut T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|ptr| unsafe { ptr.cast().as_mut() })
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = Self::len(self);
        (len, Some(len))
    }
}

impl<T> iter::FusedIterator for FlatIterMut<'_, T> {}

impl<T> ExactSizeIterator for FlatIterMut<'_, T> {
    #[inline]
    fn len(&self) -> usize {
        Self::len(self)
    }
}

impl<T> DoubleEndedIterator for FlatIterMut<'_, T> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner
            .next_back()
            .map(|ptr| unsafe { ptr.cast().as_mut() })
    }
}

/// A parallel nested iterator.
///
/// This is a new type of [`FlatIter`]. [`Iterator`] and
/// [`rayon::iter::ParallelIterator`] have the same signature methods, So
/// clients have to write fully-qualified syntax to specify a method. This new
/// type helps clients avoid it. If you don't want parallelism, just use
/// [`FlatIter`].
#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct ParFlatIter<'cont, T: 'cont>(pub FlatIter<'cont, T>);

impl<'cont, T> ParFlatIter<'cont, T> {
    /// Converts the parallel iterator into sequential iterator by unwrapping
    /// self.
    #[inline]
    pub const fn into_seq(self) -> FlatIter<'cont, T> {
        self.0
    }

    /// Returns remaining length of the iterator.
    #[inline]
    pub const fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns true if the iterator has consumed completely so that it cannot
    /// yield anything.
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<'cont, T: Send + Sync + 'cont> Producer for ParFlatIter<'cont, T> {
    type Item = &'cont T;
    type IntoIter = FlatIter<'cont, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.into_seq()
    }

    #[inline]
    fn split_at(self, index: usize) -> (Self, Self) {
        let par_iter = ParFlatRawIter(self.0.inner);
        let (l, r) = par_iter.split_at(index);

        // Safety: Splitting is safe.
        unsafe {
            let l = FlatIter::from_raw(l.0);
            let r = FlatIter::from_raw(r.0);
            (Self(l), Self(r))
        }
    }
}

impl_into_iterator_for_parallel!(
    "lifetimes" = 'cont; "bounds" = T: {'cont};
    "for" = ParFlatIter; "to" = FlatIter<'cont, T>; "item" = &'cont T;
);
impl_parallel_iterator!(
    "lifetimes" = 'cont; "bounds" = T: {Send + Sync + 'cont};
    "for" = ParFlatIter; "item" = &'cont T;
);
impl_unindexed_producer!(
    "lifetimes" = 'cont; "bounds" = T: {Send + Sync + 'cont};
    "for" = ParFlatIter; "item" = &'cont T;
);

/// A parallel nested mutable iterator.
///
/// This is a new type of [`FlatIterMut`]. [`Iterator`] and
/// [`rayon::iter::ParallelIterator`] have the same signature methods, So
/// clients have to write fully-qualified syntax to specify a method. This new
/// type helps clients avoid it. If you don't want parallelism, just use
/// [`FlatIterMut`].
#[derive(Debug)]
#[repr(transparent)]
pub struct ParFlatIterMut<'cont, T: 'cont>(pub FlatIterMut<'cont, T>);

impl<'cont, T> ParFlatIterMut<'cont, T> {
    /// Converts the parallel iterator into sequential iterator by unwrapping
    /// self.
    #[inline]
    pub fn into_seq(self) -> FlatIterMut<'cont, T> {
        self.0
    }

    /// Returns remaining length of the iterator.
    #[inline]
    pub const fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns true if the iterator has consumed completely so that it cannot
    /// yield anything.
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<'cont, T: Send + Sync + 'cont> Producer for ParFlatIterMut<'cont, T> {
    type Item = &'cont mut T;
    type IntoIter = FlatIterMut<'cont, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.into_seq()
    }

    #[inline]
    fn split_at(self, index: usize) -> (Self, Self) {
        let par_iter = ParFlatRawIter(self.0.inner);
        let (l, r) = par_iter.split_at(index);

        // Safety: Splitting is safe.
        unsafe {
            let l = FlatIterMut::from_raw(l.0);
            let r = FlatIterMut::from_raw(r.0);
            (Self(l), Self(r))
        }
    }
}

impl_into_iterator_for_parallel!(
    "lifetimes" = 'cont; "bounds" = T: {'cont};
    "for" = ParFlatIterMut; "to" = FlatIterMut<'cont, T>;
    "item" = &'cont mut T;
);
impl_parallel_iterator!(
    "lifetimes" = 'cont; "bounds" = T: {Send + Sync + 'cont};
    "for" = ParFlatIterMut; "item" = &'cont mut T;
);
impl_unindexed_producer!(
    "lifetimes" = 'cont; "bounds" = T: {Send + Sync + 'cont};
    "for" = ParFlatIterMut; "item" = &'cont mut T;
);

/// A raw getter for a container of items.
///
/// The getter basically provides [`RawGetter::get`] method for random access,
/// but you can register sequential access function through
/// [`RawGetter::with_iter`]. Then you can call [`RawGetter::iter`] for
/// sequential access.
///
/// Sequential access would be way faster than random access due to its cache
/// friendly access pattern, so it's recommended to perfer it over random access
/// as much as possible.
#[derive(Debug, Clone, Copy)]
pub struct RawGetter {
    /// Pointer to the container.
    this: SendSyncPtr<u8>,

    /// Number of items.
    len: usize,

    /// A function pointer for random access from a pointer of container.
    fn_get: unsafe fn(this: NonNull<u8>, index: usize) -> NonNull<u8>,

    /// A function pointer for generating sequential access iterator from a
    /// pointer of container.
    fn_iter: unsafe fn(this: NonNull<u8>) -> FlatRawIter,
}

impl RawGetter {
    /// Creates a new [`RawGetter`] from raw parts.
    ///
    /// * this - A pointer to a container.
    /// * len - Number of items of the container.
    /// * fn_get - A function that returns pointer to an item for an item index.
    ///
    /// # Safety
    ///
    /// Caller must guarantee that the created getter must have valid access
    /// to the given pointer while it is in use.
    pub const unsafe fn new(
        this: NonNull<u8>,
        len: usize,
        fn_get: unsafe fn(this: NonNull<u8>, index: usize) -> NonNull<u8>,
    ) -> Self {
        Self {
            this: SendSyncPtr::new(this),
            len,
            fn_get,
            fn_iter: |_| panic!("have you registered iterator function?"),
        }
    }

    /// Registers the given function to the raw getter.
    ///
    /// * fn_iter - A function pointer for generating sequential access
    ///   iterator.
    pub const fn with_iter(mut self, fn_iter: unsafe fn(this: NonNull<u8>) -> FlatRawIter) -> Self {
        self.fn_iter = fn_iter;
        self
    }

    /// Returns number of items in the associated container.
    pub const fn len(&self) -> usize {
        self.len
    }

    /// Returns true if the associated container is empty.
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns a type-erased pointer to an item of the associated container.
    pub fn get(&self, index: usize) -> Option<NonNull<u8>> {
        if index < self.len {
            unsafe { Some(self.get_unchecked(index)) }
        } else {
            None
        }
    }

    /// Returns a type-erased pointer to an item of the associated container.
    ///
    /// # Safety
    ///
    /// Undefined behavior if the given index is out of bounds.
    pub unsafe fn get_unchecked(&self, index: usize) -> NonNull<u8> {
        // Safety: In addition to index, `self.this` must be a valid pointer,
        // which is guaranteed by owners.
        unsafe { (self.fn_get)(self.this.as_nonnull(), index) }
    }

    /// Returns a sequential access iterator for the associated container.
    ///
    /// Make sure that you have registered a function through
    /// [`RawGetter::with_iter`].
    ///
    /// # Panics
    ///
    /// Panics if iterator geneartion function have not registered yet.
    pub fn iter(&self) -> FlatRawIter {
        // Safety: Owners guarantee validity.
        unsafe { (self.fn_iter)(self.this.as_nonnull()) }
    }

    /// Returns a type-erased pointer to the associated container.
    pub fn ptr(&self) -> NonNull<u8> {
        self.this.as_nonnull()
    }
}

/// A getter that can be created from [`RawGetter`] with concrete type and
/// lifetime.
///
/// This getter is useful when you have `RawGetter` and know what type the
/// getter will return. You can create this getter from the `RawGetter` then
/// use it as a `get` function of an associated container.
#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct Getter<'cont, T: 'cont> {
    raw: RawGetter,
    _marker: PhantomData<&'cont T>,
}

impl<'cont, T> Getter<'cont, T> {
    /// Creates a new [`Getter`] from the given [`RawGetter`].
    ///
    /// # Safety
    ///
    /// - The `RawGetter` must return pointers to type `T`.
    /// - Lifetime defined by caller must be sufficient to the `RawGetter`.
    pub unsafe fn from_raw(raw: RawGetter) -> Self {
        Self {
            raw,
            _marker: PhantomData,
        }
    }

    /// Converts the getter into [`RawGetter`] by unwrpping self.
    pub fn into_raw(self) -> RawGetter {
        self.raw
    }

    /// Returns number of items in the associated container.
    pub fn len(&self) -> usize {
        self.raw.len()
    }

    /// Returns true is the associated container is empty.
    pub fn is_empty(&self) -> bool {
        self.raw.is_empty()
    }

    /// Returns a shared reference to a value at the given index in the
    /// associated container.
    pub fn get(&self, index: usize) -> Option<&T> {
        if index < self.len() {
            // Safety: We checked the length.
            unsafe { Some(self.get_unchecked(index)) }
        } else {
            None
        }
    }

    /// Returns a shared reference to a value at the given index in the
    /// associated container.
    ///
    /// # Safety
    ///
    /// Undefined behavior if the given index is out of bounds.
    pub unsafe fn get_unchecked(&self, index: usize) -> &T {
        unsafe {
            let ptr = self.raw.get_unchecked(index).cast();
            ptr.as_ref()
        }
    }

    /// Returns a sequential access iterator of the associated container.
    ///
    /// But if you created the iterator from a [`RawGetter`] without calling
    /// [`RawGetter::with_iter`], this will panic.
    ///
    /// # Panics
    ///
    /// Panics if iterator geneartion function have not registered yet.
    pub fn iter(&self) -> FlatIter<'cont, T> {
        // Safety: Correct type and lifetime are given.
        unsafe { FlatIter::from_raw(self.raw.iter()) }
    }

    /// Returns a parallel iterator of the associated container.
    ///
    /// But if you created the iterator from a [`RawGetter`] without calling
    /// [`RawGetter::with_iter`], this will panic.
    ///
    /// # Panics
    ///
    /// Panics if iterator geneartion function have not registered yet.
    #[inline]
    pub fn par_iter(&self) -> ParFlatIter<'cont, T> {
        ParFlatIter(self.iter())
    }

    /// Returns number of chunks.
    pub fn num_chunks(&self) -> usize {
        let flat_iter = self.raw.iter();
        flat_iter.num_chunks()
    }

    /// Retrieves a slice for the given chunk index.
    ///
    /// If the given chunk index is out of bounds, empty slice is returned.
    pub fn as_slice(&self, chunk_index: usize) -> &[T] {
        // Safety: Type is correct.
        unsafe { self.raw.iter().as_slice(chunk_index) }
    }
}

impl<'cont, T> IntoIterator for Getter<'cont, T> {
    type Item = &'cont T;
    type IntoIter = FlatIter<'cont, T>;

    fn into_iter(self) -> Self::IntoIter {
        Self::iter(&self)
    }
}

impl<'cont, T: Send + Sync> IntoParallelIterator for Getter<'cont, T> {
    type Iter = ParFlatIter<'cont, T>;
    type Item = &'cont T;

    fn into_par_iter(self) -> Self::Iter {
        Self::par_iter(&self)
    }
}

/// A mutable getter that can be created from [`RawGetter`] with concrete type
/// and lifetime.
///
/// This getter is useful when you have `RawGetter` and know what type the
/// getter will return. You can create this getter from the `RawGetter` then
/// use it as a `get_mut` function of an associated container.
#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct GetterMut<'cont, T: 'cont> {
    raw: RawGetter,
    _marker: PhantomData<&'cont mut T>,
}

impl<'cont, T> GetterMut<'cont, T> {
    /// Creates a new [`GetterMut`] from the given [`RawGetter`].
    ///
    /// # Safety
    ///
    /// - The `RawGetter` must return pointers to type `T`.
    /// - The `RawGetter` must not be used elsewhere at the same time because
    ///   created getter must be exclusive.
    /// - Lifetime defined by caller must be sufficient to the `RawGetter`.
    pub unsafe fn from_raw(raw: RawGetter) -> Self {
        Self {
            raw,
            _marker: PhantomData,
        }
    }

    /// Converts the getter into [`RawGetter`] by unwrpping self.
    pub fn into_raw(self) -> RawGetter {
        self.raw
    }

    /// Returns number of items in the associated container.
    pub fn len(&self) -> usize {
        self.raw.len()
    }

    /// Returns true is the associated container is empty.
    pub fn is_empty(&self) -> bool {
        self.raw.is_empty()
    }

    /// Returns a shared reference to a value at the given index in the
    /// associated container.
    pub fn get(&self, index: usize) -> Option<&T> {
        if index < self.len() {
            // Safety: We checked the length.
            unsafe { Some(self.get_unchecked(index)) }
        } else {
            None
        }
    }

    /// Returns a shared reference to a value at the given index in the
    /// associated container.
    ///
    /// # Safety
    ///
    /// Undefined behavior if the given index is out of bounds.
    pub unsafe fn get_unchecked(&self, index: usize) -> &T {
        unsafe {
            let ptr = self.raw.get_unchecked(index).cast();
            ptr.as_ref()
        }
    }

    /// Returns a mutable reference to a value at the given index in the
    /// associated container.
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        if index < self.len() {
            // Safety: `index` is in bounds.
            unsafe { Some(self.get_unchecked_mut(index)) }
        } else {
            None
        }
    }

    /// Returns a mutable reference to a value at the given index in the
    /// associated container.
    ///
    /// # Safety
    ///
    /// Undefined behavior if the given index is out of bounds.
    pub unsafe fn get_unchecked_mut(&mut self, index: usize) -> &mut T {
        unsafe {
            let mut ptr = self.raw.get_unchecked(index).cast();
            ptr.as_mut()
        }
    }

    /// Returns a sequential access iterator of the associated container.
    ///
    /// But if you created the iterator from a [`RawGetter`] without calling
    /// [`RawGetter::with_iter`], this will panic.
    ///
    /// # Panics
    ///
    /// Panics if iterator geneartion function have not registered yet.
    pub fn iter(&self) -> FlatIter<'cont, T> {
        // Safety: Correct type and lifetime are given.
        unsafe { FlatIter::from_raw(self.raw.iter()) }
    }

    /// Returns a mutable sequential access iterator of the associated
    /// container.
    ///
    /// But if you created the iterator from a [`RawGetter`] without calling
    /// [`RawGetter::with_iter`], this will panic.
    ///
    /// # Panics
    ///
    /// Panics if iterator geneartion function have not registered yet.
    pub fn iter_mut(&mut self) -> FlatIterMut<'cont, T> {
        // Safety: Correct type and lifetime are given.
        unsafe { FlatIterMut::from_raw(self.raw.iter()) }
    }

    /// Returns a parallel iterator of the associated container.
    ///
    /// But if you created the iterator from a [`RawGetter`] without calling
    /// [`RawGetter::with_iter`], this will panic.
    ///
    /// # Panics
    ///
    /// Panics if iterator geneartion function have not registered yet.
    #[inline]
    pub fn par_iter(&self) -> ParFlatIter<'cont, T> {
        ParFlatIter(self.iter())
    }

    /// Returns a parallel mutable iterator of the associated container.
    ///
    /// But if you created the iterator from a [`RawGetter`] without calling
    /// [`RawGetter::with_iter`], this will panic.
    ///
    /// # Panics
    ///
    /// Panics if iterator geneartion function have not registered yet.
    #[inline]
    pub fn par_iter_mut(&mut self) -> ParFlatIterMut<'cont, T> {
        ParFlatIterMut(self.iter_mut())
    }

    /// Returns number of chunks.
    pub fn num_chunks(&self) -> usize {
        let flat_iter = self.raw.iter();
        flat_iter.num_chunks()
    }

    /// Retrieves a shared slice for the given chunk index.
    ///
    /// If the given chunk index is out of bounds, empty slice will be returned.
    pub fn as_slice(&self, chunk_index: usize) -> &[T] {
        // Safety: Type is correct.
        unsafe { self.raw.iter().as_slice(chunk_index) }
    }

    /// Retrieves a mutable slice for the given chunk index.
    ///
    /// If the given chunk index is out of bounds, empty slice will be returned.
    pub fn as_mut_slice(&mut self, chunk_index: usize) -> &mut [T] {
        // Safety: Type is correct.
        unsafe { self.raw.iter().as_mut_slice(chunk_index) }
    }
}

impl<'cont, T> IntoIterator for GetterMut<'cont, T> {
    type Item = &'cont mut T;
    type IntoIter = FlatIterMut<'cont, T>;

    fn into_iter(mut self) -> Self::IntoIter {
        Self::iter_mut(&mut self)
    }
}

impl<'cont, T: Send + Sync> IntoParallelIterator for GetterMut<'cont, T> {
    type Iter = ParFlatIterMut<'cont, T>;
    type Item = &'cont mut T;

    fn into_par_iter(mut self) -> Self::Iter {
        Self::par_iter_mut(&mut self)
    }
}
