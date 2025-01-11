use super::ptr::SendSyncPtr;
use crate::{impl_into_iterator_for_parallel, impl_parallel_iterator, impl_unindexed_producer};
use rayon::iter::{plumbing::Producer, IntoParallelIterator};
use std::{
    iter,
    marker::PhantomData,
    mem,
    ops::{Deref, DerefMut, Range},
    ptr::NonNull,
    slice,
};

pub trait AsRawIter {
    fn iter(&self) -> RawIter;

    /// Provided.
    #[inline]
    fn par_iter(&self) -> ParRawIter {
        ParRawIter(self.iter())
    }

    /// # Safety
    ///
    /// Given container must contain type `T` data.
    #[inline]
    unsafe fn iter_of<T>(&self) -> Iter<'_, T> {
        Iter::new(self)
    }

    /// # Safety
    ///
    /// Given container must contain type `T` data.
    #[inline]
    unsafe fn iter_mut_of<T>(&mut self) -> IterMut<'_, T> {
        IterMut::new(self)
    }

    /// # Safety
    ///
    /// Given container must contain type `T` data.
    #[inline]
    unsafe fn par_iter_of<T>(&self) -> ParIter<'_, T> {
        ParIter::new(self)
    }

    /// # Safety
    ///
    /// Given container must contain type `T` data.
    #[inline]
    unsafe fn par_iter_mut_of<T>(&mut self) -> ParIterMut<'_, T> {
        ParIterMut::new(self)
    }
}

impl<T> AsRawIter for [T] {
    fn iter(&self) -> RawIter {
        let Range { start, end } = self.as_ptr_range();
        unsafe {
            // Safety: Infallible.
            let start = NonNull::new(start.cast_mut()).unwrap_unchecked();
            let end = NonNull::new(end.cast_mut()).unwrap_unchecked();
            let stride = mem::size_of::<T>().max(mem::align_of::<T>());

            // Safety: slice guarantees.
            RawIter::new(start.cast(), end.cast(), stride)
        }
    }
}

pub trait AsFlatRawIter {
    /// # Safety
    ///
    /// Returned itereator is not bounded by lifetime.
    /// But it actually relies on `&self`, so it must be used as if
    /// it's borrowed.
    unsafe fn iter(&self) -> FlatRawIter;

    /// # Safety
    ///
    /// Returned itereator is not bounded by lifetime.
    /// But it actually relies on `&self`, so it must be used as if
    /// it's borrowed.
    unsafe fn par_iter(&self) -> ParFlatRawIter;

    /// # Safety
    ///
    /// Given container must contain type `T` data.
    #[inline]
    unsafe fn iter_of<T>(&self) -> FlatIter<'_, T> {
        FlatIter::new(self)
    }

    /// # Safety
    ///
    /// Given container must contain type `T` data.
    #[inline]
    unsafe fn iter_mut_of<T>(&mut self) -> FlatIterMut<'_, T> {
        FlatIterMut::new(self)
    }

    /// # Safety
    ///
    /// Given container must contain type `T` data.
    #[inline]
    unsafe fn par_iter_of<T>(&self) -> ParFlatIter<'_, T> {
        ParFlatIter::new(self)
    }

    /// # Safety
    ///
    /// Given container must contain type `T` data.
    #[inline]
    unsafe fn par_iter_mut_of<T>(&mut self) -> ParFlatIterMut<'_, T> {
        ParFlatIterMut::new(self)
    }
}

/// Iterator for slice without explict type.
/// It's recommended to wrap this iterator with concrete type and liftime.
#[derive(Debug, Clone, Copy)]
pub struct RawIter {
    cur: SendSyncPtr<u8>,
    end: SendSyncPtr<u8>,
    stride: usize,
}

impl RawIter {
    /// # Safety
    ///
    /// Undefined behavior if
    /// - `end` is less then `start`.
    /// - `end` exceeds isize::MAX.
    /// - `stride` is zero.
    /// - Diffrence between `start` and `end` cannot be divided by `stride`.
    #[inline]
    pub unsafe fn new(start: NonNull<u8>, end: NonNull<u8>, stride: usize) -> Self {
        debug_assert!(start <= end);
        debug_assert!(end.as_ptr() as usize <= isize::MAX as usize);
        debug_assert!(stride > 0);
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

    pub const fn empty() -> Self {
        Self {
            cur: SendSyncPtr::dangling(),
            end: SendSyncPtr::dangling(),
            stride: 1,
        }
    }

    #[inline]
    pub const fn len(&self) -> usize {
        let end = self.end.as_ptr();
        let cur = self.cur.as_ptr();
        // Safety: Owners guarantee safety.
        unsafe { end.offset_from(cur) as usize / self.stride }
    }

    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Iterator for RawIter {
    type Item = SendSyncPtr<u8>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.cur < self.end {
            let res = self.cur;
            // Safety: Owners guarantee safety.
            self.cur = unsafe { self.cur.add(self.stride) };
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
            self.end = unsafe { self.end.sub(self.stride) };
            Some(self.end)
        } else {
            None
        }
    }
}

/// Parallel [`RawIter`].
//
// `Iterator` and `ParallelIterator` have the same signature methods,
// So clients have to write fully-qualified syntax to specify methods.
// This new type helps clients avoid it.
#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct ParRawIter(pub RawIter);

impl ParRawIter {
    #[inline]
    pub const fn into_seq(self) -> RawIter {
        self.0
    }

    #[inline]
    pub const fn len(&self) -> usize {
        self.0.len()
    }

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
        let l_end = unsafe { self.cur.add(index * self.stride) };
        let r_cur = l_end;
        let r_end = self.end;

        // Safety: Splitting is safe.
        let (l, r) = unsafe {
            (
                RawIter::new(l_cur.as_nonnull(), l_end.as_nonnull(), self.stride),
                RawIter::new(r_cur.as_nonnull(), r_end.as_nonnull(), self.stride),
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

/// [`RawIter`] with concrete type and lifetime.
#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct Iter<'cont, T: 'cont> {
    inner: RawIter,
    _marker: PhantomData<&'cont T>,
}

impl<'cont, T> Iter<'cont, T> {
    /// Borrows container and returns its iterator.
    ///
    /// # Safety
    ///
    /// Given container must contain type `T` data.
    #[inline]
    pub unsafe fn new<C>(cont: &'cont C) -> Self
    where
        C: AsRawIter + ?Sized,
    {
        // We're borrowing container, so lifetime is tied up.
        Self::from_raw(cont.iter())
    }

    /// # Safety
    ///
    /// Undefined behavior if any of the conditions described below are not met.
    /// - Given raw iterator must yield pointers to type `T`.
    /// - Lifetime defined by clients must be sufficient about the raw iterator.
    #[inline]
    pub const unsafe fn from_raw(raw: RawIter) -> Self {
        Self {
            inner: raw,
            _marker: PhantomData,
        }
    }

    #[inline]
    pub const fn len(&self) -> usize {
        self.inner.len()
    }

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

#[derive(Debug)]
#[repr(transparent)]
pub struct IterMut<'cont, T: 'cont> {
    inner: RawIter,
    _marker: PhantomData<&'cont mut T>,
}

impl<'cont, T> IterMut<'cont, T> {
    /// Borrows container and returns its iterator.
    ///
    /// # Safety
    ///
    /// Given container must contain type `T` data.
    #[inline]
    pub unsafe fn new<C>(cont: &'cont mut C) -> Self
    where
        C: AsRawIter + ?Sized,
    {
        // We're borrowing container, so lifetime is tied up.
        unsafe { Self::from_raw(cont.iter()) }
    }

    /// # Safety
    ///
    /// Undefined behavior if any of the conditions described below are not met.
    /// - Given raw iterator must yield pointers to type `T`.
    /// - Lifetime defined by clients must be sufficient about the raw iterator.
    #[inline]
    pub unsafe fn from_raw(raw: RawIter) -> Self {
        Self {
            inner: raw,
            _marker: PhantomData,
        }
    }

    #[inline]
    pub const fn len(&self) -> usize {
        self.inner.len()
    }

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

/// [`ParRawIter`] with concrete type and lifetime.
/// This is parallel version of [`Iter`].
//
// `Iterator` and `ParallelIterator` have the same signature methods,
// So clients have to write fully-qualified syntax to specify methods.
// This new type helps clients avoid it.
#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct ParIter<'cont, T: 'cont> {
    inner: ParRawIter,
    _marker: PhantomData<&'cont T>,
}

impl<'cont, T> ParIter<'cont, T> {
    /// Borrows container and returns its iterator.
    ///
    /// # Safety
    ///
    /// Given container must contain type `T` data.
    #[inline]
    pub unsafe fn new<C>(cont: &'cont C) -> Self
    where
        C: AsRawIter + ?Sized,
    {
        // We're borrowing container, so lifetime is tied up.
        unsafe { Self::from_raw(cont.par_iter()) }
    }

    /// # Safety
    ///
    /// Undefined behavior if any of the conditions described below are not met.
    /// - Given raw iterator must yield pointers to type `T`.
    /// - Lifetime defined by clients must be sufficient about the raw iterator.
    #[inline]
    pub const unsafe fn from_raw(raw: ParRawIter) -> Self {
        Self {
            inner: raw,
            _marker: PhantomData,
        }
    }

    #[inline]
    pub const fn into_seq(self) -> Iter<'cont, T> {
        // Safety: Correct type and lifetime are given.
        unsafe { Iter::from_raw(self.inner.into_seq()) }
    }

    #[inline]
    pub const fn len(&self) -> usize {
        self.inner.len()
    }

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

/// [`ParRawIter`] with concrete type and lifetime.
/// This is parallel version of [`IterMut`].
//
// `Iterator` and `ParallelIterator` have the same signature methods,
// So clients have to write fully-qualified syntax to specify methods.
// This new type helps clients avoid it.
#[derive(Debug)]
#[repr(transparent)]
pub struct ParIterMut<'cont, T: 'cont> {
    inner: ParRawIter,
    _marker: PhantomData<&'cont mut T>,
}

impl<'cont, T> ParIterMut<'cont, T> {
    /// Borrows container and returns its iterator.
    ///
    /// # Safety
    ///
    /// Given container must contain type `T` data.
    #[inline]
    pub fn new<C>(cont: &'cont mut C) -> Self
    where
        C: AsRawIter + ?Sized,
    {
        // We're borrowing container, so lifetime is tied up.
        unsafe { Self::from_raw(cont.par_iter()) }
    }

    /// # Safety
    ///
    /// Undefined behavior if any of the conditions described below are not met.
    /// - Given raw iterator must yield pointers to type `T`.
    /// - Lifetime defined by clients must be sufficient about the raw iterator.
    #[inline]
    pub unsafe fn from_raw(raw: ParRawIter) -> Self {
        Self {
            inner: raw,
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn into_seq(self) -> IterMut<'cont, T> {
        // Safety: Correct type and lifetime are given.
        unsafe { IterMut::from_raw(self.inner.into_seq()) }
    }

    #[inline]
    pub const fn len(&self) -> usize {
        self.inner.len()
    }

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

/// Nested [`RawIter`] which yields `RawIter`.
/// You can call [`Iterator::flatten`] to access each item,
/// which is [`SendSyncPtr`], but it may be slower than you thought.
/// In that case, consider using [`FlatRawIter`] instaed.
#[derive(Debug, Clone)]
pub struct NestedRawIter {
    this: NonNull<u8>,
    fn_iter: unsafe fn(this: NonNull<u8>, index: usize) -> RawIter,
    cur: usize,
    len: usize,
}

impl NestedRawIter {
    /// # Safety
    ///
    /// `fn_iter` must operate safely while the iterator lives.
    #[inline]
    pub const unsafe fn new(
        this: NonNull<u8>,
        fn_iter: unsafe fn(this: NonNull<u8>, index: usize) -> RawIter,
        len: usize,
    ) -> Self {
        Self {
            this,
            fn_iter,
            cur: 0,
            len,
        }
    }

    #[inline]
    pub const fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Iterator for NestedRawIter {
    type Item = RawIter;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.cur < self.len {
            // Safety: Owners guarantee safety.
            let res = unsafe { (self.fn_iter)(self.this, self.cur) };
            self.cur += 1;
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

impl iter::FusedIterator for NestedRawIter {}

impl ExactSizeIterator for NestedRawIter {
    #[inline]
    fn len(&self) -> usize {
        Self::len(self)
    }
}

/// Nested [RawIter], but flatten iterator.
/// Explicit flatten iterator can give us better optimization in for loop.
//
// To implement rayon's ParallelIterator, we have to implement
// `ExactSizeIterator` and `DoubleEndedIterator`. So we track right side status
// and total number of items.
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
    /// * this - Pointer to container.
    /// * chunks - Number of chunks.
    /// * fn_iter - Function that returns [RawIter] for an index.
    /// * stride - Stride in bytes.
    /// * len - Total number of items.
    ///
    /// # Safety
    ///
    /// `fn_iter` must operate safely while the iterator lives.
    #[inline]
    pub unsafe fn new(
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

    #[inline]
    pub const fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns number of chunks if you've not been called
    /// [`next_back`](Self::next_back) on this iterator.
    pub(crate) fn num_chunks(&self) -> usize {
        self.ri
    }

    /// # Safety
    ///
    /// T must be correct.
    pub(crate) unsafe fn as_slice<'o, T>(&self, chunk_idx: usize) -> &'o [T] {
        // `fn_iter` gives us empty iterator if `chunk` is out of bounds.
        let raw_iter = (self.fn_iter)(self.this.as_nonnull(), chunk_idx);
        if !raw_iter.is_empty() {
            let ptr = raw_iter.cur.as_nonnull().cast::<T>().as_ptr();
            slice::from_raw_parts(ptr, raw_iter.len())
        } else {
            // Even if empty slice, we need an aligned and non-null pointer.
            let ptr = std::ptr::NonNull::dangling().as_ptr();
            slice::from_raw_parts(ptr, 0)
        }
    }

    /// # Safety
    ///
    /// T must be correct.
    pub(crate) unsafe fn as_slice_mut<'o, T>(&self, chunk_idx: usize) -> &'o mut [T] {
        // `fn_iter` gives us empty iterator if `chunk` is out of bounds.
        let raw_iter = (self.fn_iter)(self.this.as_nonnull(), chunk_idx);
        if !raw_iter.is_empty() {
            let ptr = raw_iter.cur.as_nonnull().cast::<T>().as_ptr();
            slice::from_raw_parts_mut(ptr, raw_iter.len())
        } else {
            // Even if empty slice, we need an aligned and non-null pointer.
            let ptr = std::ptr::NonNull::dangling().as_ptr();
            slice::from_raw_parts_mut(ptr, 0)
        }
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

/// Parallel [FlatRawIter].
//
// `Iterator` and `ParallelIterator` have the same signature methods,
// So clients have to write fully-qualified syntax to specify methods.
// This new type helps clients avoid it.
#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct ParFlatRawIter(pub FlatRawIter);

impl ParFlatRawIter {
    #[inline]
    pub const fn into_seq(self) -> FlatRawIter {
        self.0
    }

    #[inline]
    pub const fn len(&self) -> usize {
        self.0.len()
    }

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

        // Basic idea to split is somthing like so,
        //
        // Left chunk      Mid chunk         Right chunk
        //      li              mi                ri
        // [**********] .. [**********]  ..  [**********]
        // ^          ^    ^     ^    ^      ^          ^
        // ll         lr   ml    mm   mr     rl         rr
        // |          |    |     ||    \     |          |
        // [**********] .. [*****][*****] .. [**********]
        // |---- Left child -----||---- Right child ----|
        //
        // But, we must consider something like
        // - Imagine that mid chunk is left chunk, but not splitted
        //   as depectied below.
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

/// [FlatRawIter] with concrete type and lifetime.
#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct FlatIter<'cont, T: 'cont> {
    inner: FlatRawIter,
    _marker: PhantomData<&'cont T>,
}

impl<'cont, T> FlatIter<'cont, T> {
    /// Borrows container and returns its iterator.
    ///
    /// # Safety
    ///
    /// Given container must contain type `T` data.
    #[inline]
    pub fn new<C>(cont: &'cont C) -> Self
    where
        C: AsFlatRawIter + ?Sized,
    {
        // We're borrowing container, so lifetime is tied up.
        unsafe { Self::from_raw(cont.iter()) }
    }

    /// # Safety
    ///
    /// Undefined behavior if any of the conditions described below are not met.
    /// - Given raw iterator must yield pointers to type `T`.
    /// - Lifetime defined by clients must be sufficient about the raw iterator.
    #[inline]
    pub const unsafe fn from_raw(raw: FlatRawIter) -> Self {
        Self {
            inner: raw,
            _marker: PhantomData,
        }
    }

    #[inline]
    pub const fn len(&self) -> usize {
        self.inner.len()
    }

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

#[derive(Debug)]
#[repr(transparent)]
pub struct FlatIterMut<'cont, T: 'cont> {
    inner: FlatRawIter,
    _marker: PhantomData<&'cont mut T>,
}

impl<'cont, T> FlatIterMut<'cont, T> {
    /// Borrows container and returns its iterator.
    ///
    /// # Safety
    ///
    /// Given container must contain type `T` data.
    #[inline]
    pub fn new<C>(cont: &'cont mut C) -> Self
    where
        C: AsFlatRawIter + ?Sized,
    {
        // We're borrowing container, so lifetime is tied up.
        unsafe { Self::from_raw(cont.iter()) }
    }

    /// # Safety
    ///
    /// Undefined behavior if any of the conditions described below are not met.
    /// - Given raw iterator must yield pointers to type `T`.
    /// - Lifetime defined by clients must be sufficient about the raw iterator.
    #[inline]
    pub unsafe fn from_raw(raw: FlatRawIter) -> Self {
        Self {
            inner: raw,
            _marker: PhantomData,
        }
    }

    #[inline]
    pub const fn len(&self) -> usize {
        self.inner.len()
    }

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

/// [ParFlatRawIter] with concrete type and lifetime.
/// This is parallel version of [FlatIter].
//
// `Iterator` and `ParallelIterator` have the same signature methods,
// So clients have to write fully-qualified syntax to specify methods.
// This new type helps clients avoid it.
#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct ParFlatIter<'cont, T: 'cont> {
    inner: FlatRawIter,
    _marker: PhantomData<&'cont T>,
}

impl<'cont, T> ParFlatIter<'cont, T> {
    /// Borrows container and returns its iterator.
    ///
    /// # Safety
    ///
    /// Given container must contain type `T` data.
    #[inline]
    pub fn new<C>(cont: &'cont C) -> Self
    where
        C: AsFlatRawIter + ?Sized,
    {
        // We're borrowing container, so lifetime is tied up.
        unsafe { Self::from_raw(cont.iter()) }
    }

    /// # Safety
    ///
    /// Undefined behavior if any of the conditions described below are not met.
    /// - Given raw iterator must yield pointers to type `T`.
    /// - Lifetime defined by clients must be sufficient about the raw iterator.
    #[inline]
    pub const unsafe fn from_raw(raw: FlatRawIter) -> Self {
        Self {
            inner: raw,
            _marker: PhantomData,
        }
    }

    #[inline]
    pub const fn into_seq(self) -> FlatIter<'cont, T> {
        // Safety: Correct type and lifetime are given.
        unsafe { FlatIter::from_raw(self.inner) }
    }

    #[inline]
    pub const fn len(&self) -> usize {
        self.inner.len()
    }

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
        let par_iter = ParFlatRawIter(self.inner);
        let (l, r) = par_iter.split_at(index);
        // Safety: Splitting doesn't affect both type and lifetime.
        unsafe { (Self::from_raw(l.0), Self::from_raw(r.0)) }
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

/// [`ParFlatRawIter`] with concrete type and lifetime.
/// This is parallel version of [`FlatIterMut`].
//
// `Iterator` and `ParallelIterator` have the same signature methods,
// So clients have to write fully-qualified syntax to specify methods.
// This new type helps clients avoid it.
#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct ParFlatIterMut<'cont, T: 'cont> {
    inner: FlatRawIter,
    _marker: PhantomData<&'cont mut T>,
}

impl<'cont, T> ParFlatIterMut<'cont, T> {
    /// Borrows container and returns its iterator.
    ///
    /// # Safety
    ///
    /// Given container must contain type `T` data.
    #[inline]
    pub fn new<C>(cont: &'cont mut C) -> Self
    where
        C: AsFlatRawIter + ?Sized,
    {
        // We're borrowing container, so lifetime is tied up.
        unsafe { Self::from_raw(cont.iter()) }
    }

    /// # Safety
    ///
    /// Undefined behavior if any of the conditions described below are not met.
    /// - Given raw iterator must yield pointers to type `T`.
    /// - Lifetime defined by clients must be sufficient about the raw iterator.
    #[inline]
    pub unsafe fn from_raw(raw: FlatRawIter) -> Self {
        Self {
            inner: raw,
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn into_seq(self) -> FlatIterMut<'cont, T> {
        // Safety: Correct type and lifetime are given.
        unsafe { FlatIterMut::from_raw(self.inner) }
    }

    #[inline]
    pub const fn len(&self) -> usize {
        self.inner.len()
    }

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
        let par_iter = ParFlatRawIter(self.inner);
        let (l, r) = par_iter.split_at(index);
        // Safety: Splitting doesn't affect both type and lifetime.
        unsafe { (Self::from_raw(l.0), Self::from_raw(r.0)) }
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

#[derive(Debug, Clone, Copy)]
pub struct RawGetter {
    /// Pointer to the container.
    this: SendSyncPtr<u8>,

    /// Number of items.
    len: usize,

    /// Random access getter.
    fn_get: unsafe fn(this: NonNull<u8>, index: usize) -> NonNull<u8>,

    /// Sequential access iterator.
    fn_iter: unsafe fn(this: NonNull<u8>) -> FlatRawIter,
}

impl RawGetter {
    /// # Safety
    ///
    /// Pointer must be valid while this is alive.
    pub const unsafe fn new(
        this: NonNull<u8>,
        len: usize,
        fn_get: unsafe fn(this: NonNull<u8>, index: usize) -> NonNull<u8>,
    ) -> Self {
        Self {
            this: SendSyncPtr::new(this),
            len,
            fn_get,
            fn_iter: |_| unimplemented!(),
        }
    }

    pub const fn with_iter(mut self, fn_iter: unsafe fn(this: NonNull<u8>) -> FlatRawIter) -> Self {
        self.fn_iter = fn_iter;
        self
    }

    pub fn get(&self, index: usize) -> Option<NonNull<u8>> {
        if index < self.len {
            unsafe { Some(self.get_unchecked(index)) }
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// Undefined behavior if `index` is out of bound.
    pub unsafe fn get_unchecked(&self, index: usize) -> NonNull<u8> {
        // Safety: In addition to index, `self.this` must be a valid pointer,
        // which is guaranteed by owners.
        unsafe { (self.fn_get)(self.this.as_nonnull(), index) }
    }

    pub const fn len(&self) -> usize {
        self.len
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&self) -> FlatRawIter {
        // Safety: Owners guarantee validity.
        unsafe { (self.fn_iter)(self.this.as_nonnull()) }
    }

    pub fn ptr(&self) -> NonNull<u8> {
        self.this.as_nonnull()
    }
}

pub trait AsGetter {
    type Item;

    fn as_getter(&self) -> Getter<'_, Self::Item>;
    fn as_getter_mut(&mut self) -> GetterMut<'_, Self::Item>;
}

/// An accessor to an array.
/// Getter provides primitive methods like [`Self::get`] and [`Self::iter`].
/// `get()` is random access method so you can retrive an item at an index.
/// `iter()`, on the other hand, returns sequential access iterator.
/// The iterator can be way faster than access via `get()` thanks to inline
/// optimization.
#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct Getter<'cont, T: 'cont> {
    raw: RawGetter,
    _marker: PhantomData<&'cont T>,
}

impl<'cont, T> Getter<'cont, T> {
    pub fn new(container: &'cont impl AsGetter<Item = T>) -> Self {
        container.as_getter()
    }

    /// Instead of creating from [`AsGetter`], you can create from [`RawGetter`].
    /// But, you must provide valid concrete type `T` and lifetime `'cont`.
    ///
    /// # Safety
    ///
    /// Undefined behavior if concrete type `T` is not valid or
    /// container pointer inside [`RawGetter`] is invalidated.
    pub unsafe fn from_raw(raw: RawGetter) -> Self {
        Self {
            raw,
            _marker: PhantomData,
        }
    }

    pub fn into_raw(self) -> RawGetter {
        self.raw
    }

    pub fn len(&self) -> usize {
        self.raw.len()
    }

    pub fn is_empty(&self) -> bool {
        self.raw.is_empty()
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        if index < self.len() {
            // Safety: We checked the length.
            unsafe { Some(self.get_unchecked(index)) }
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// Undefined behavior if `index` is out of bound.
    pub unsafe fn get_unchecked(&self, index: usize) -> &T {
        let ptr = self.raw.get_unchecked(index).cast();
        ptr.as_ref()
    }

    pub fn iter(&self) -> FlatIter<'cont, T> {
        // Safety: Correct type and lifetime are given.
        unsafe { FlatIter::from_raw(self.raw.iter()) }
    }

    #[inline]
    pub fn par_iter(&self) -> ParFlatIter<'cont, T> {
        // Safety: Correct type and lifetime are given.
        unsafe { ParFlatIter::from_raw(self.raw.iter()) }
    }

    /// Returns number of chunks.
    pub fn num_chunks(&self) -> usize {
        let flat_iter = self.raw.iter();
        flat_iter.num_chunks()
    }

    /// Retrieves a slice for the given chunk index.
    ///
    /// If the given chunk index is out of bounds, empty slice will be returned.
    pub fn as_slice(&self, chunk: usize) -> &[T] {
        // Safety: Type is correct.
        unsafe { self.raw.iter().as_slice(chunk) }
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
    type Item = &'cont T;
    type Iter = ParFlatIter<'cont, T>;

    fn into_par_iter(self) -> Self::Iter {
        Self::par_iter(&self)
    }
}

/// An accessor to an array.  
/// Getter provides primitive methods like [`get_mut`][0] and [`iter_mut`][1].
///
/// - [get_mut][0] is random access method so you can retrive an item at an index.
/// - [iter_mut][1], on the other hand, returns sequential access iterator.
///   The iterator can be faster than access via [get_mut][0] thanks to inline
///   optimization. But it strongly relies on build options.
///
/// [0]: Self::get_mut
/// [1]: Self::iter_mut
#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct GetterMut<'cont, T: 'cont> {
    raw: RawGetter,
    _marker: PhantomData<&'cont mut T>,
}

impl<'cont, T> GetterMut<'cont, T> {
    pub fn new(container: &'cont mut impl AsGetter<Item = T>) -> Self {
        container.as_getter_mut()
    }

    /// Instead of creating from [AsGetter], you can create from [RawGetter].
    /// But, you must provide valid concrete type `T` and lifetime `'cont`.
    ///
    /// # Safety
    ///
    /// Undefined behavior if concrete type `T` is not valid or
    /// container pointer inside [RawGetter] is invalidated.
    pub unsafe fn from_raw(raw: RawGetter) -> Self {
        Self {
            raw,
            _marker: PhantomData,
        }
    }

    pub fn into_raw(self) -> RawGetter {
        self.raw
    }

    pub fn len(&self) -> usize {
        self.raw.len()
    }

    pub fn is_empty(&self) -> bool {
        self.raw.is_empty()
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        if index < self.len() {
            // Safety: We checked the length.
            unsafe { Some(self.get_unchecked(index)) }
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// Undefined behavior if `index` is out of bound.
    pub unsafe fn get_unchecked(&self, index: usize) -> &T {
        let ptr = self.raw.get_unchecked(index).cast();
        ptr.as_ref()
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        if index < self.len() {
            // Safety: `index` is in bounds.
            unsafe { Some(self.get_unchecked_mut(index)) }
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// Undefined behavior if `index` is out of bound.
    pub unsafe fn get_unchecked_mut(&mut self, index: usize) -> &mut T {
        let mut ptr = self.raw.get_unchecked(index).cast();
        ptr.as_mut()
    }

    pub fn iter(&self) -> FlatIter<'cont, T> {
        // Safety: Correct type and lifetime are given.
        unsafe { FlatIter::from_raw(self.raw.iter()) }
    }

    pub fn iter_mut(&mut self) -> FlatIterMut<'cont, T> {
        // Safety: Correct type and lifetime are given.
        unsafe { FlatIterMut::from_raw(self.raw.iter()) }
    }

    #[inline]
    pub fn par_iter(&self) -> ParFlatIter<'cont, T> {
        // Safety: Correct type and lifetime are given.
        unsafe { ParFlatIter::from_raw(self.raw.iter()) }
    }

    #[inline]
    pub fn par_iter_mut(&mut self) -> ParFlatIterMut<'cont, T> {
        // Safety: Correct type and lifetime are given.
        unsafe { ParFlatIterMut::from_raw(self.raw.iter()) }
    }

    /// Returns number of chunks.
    pub fn num_chunks(&self) -> usize {
        let flat_iter = self.raw.iter();
        flat_iter.num_chunks()
    }

    /// Retrieves a shared slice for the given chunk index.
    ///
    /// If the given chunk index is out of bounds, empty slice will be returned.
    pub fn as_slice(&self, chunk: usize) -> &[T] {
        // Safety: Type is correct.
        unsafe { self.raw.iter().as_slice(chunk) }
    }

    /// Retrieves a mutable slice for the given chunk index.
    ///
    /// If the given chunk index is out of bounds, empty slice will be returned.
    pub fn as_slice_mut(&mut self, chunk: usize) -> &mut [T] {
        // Safety: Type is correct.
        unsafe { self.raw.iter().as_slice_mut(chunk) }
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
    type Item = &'cont mut T;
    type Iter = ParFlatIterMut<'cont, T>;

    fn into_par_iter(mut self) -> Self::Iter {
        Self::par_iter_mut(&mut self)
    }
}
