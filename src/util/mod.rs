#![allow(dead_code)]

use std::{
    mem::{size_of, transmute},
    thread,
};

pub mod macros;
pub mod web;
pub use web::*;

pub mod prelude {
    pub use crate::log;
}

use std::mem::{transmute_copy, MaybeUninit};

#[inline(always)]
pub(crate) fn upcast_slice<T>(v: &mut [T]) -> *mut [()] {
    v as *mut [T] as *mut [()]
}

/// # Safety
///
/// Caller must provide the original type `T` of the `ptr`.
/// Calling this method with the incorrect type is *undefined behavior*.
#[inline(always)]
pub(crate) unsafe fn downcast_slice<'a, T>(ptr: *mut [()]) -> &'a [T] {
    &*(ptr as *const [T])
}

/// # Safety
///
/// Caller must provide the original type `T` of the `ptr`.
/// Calling this method with the incorrect type is *undefined behavior*.
#[inline(always)]
pub(crate) unsafe fn downcast_mut_slice<'a, T>(ptr: *mut [()]) -> &'a mut [T] {
    &mut *(ptr as *mut [T])
}

pub(crate) fn current_thread_id() -> u64 {
    let id = thread::current().id();

    // Safety: ThreadId and u64 are both 8 bytes.
    debug_assert_eq!(size_of::<thread::ThreadId>(), size_of::<u64>());
    unsafe { transmute(id) }
}

pub(crate) struct PowerOfTwo {
    pub(crate) value: usize,
    pub(crate) power: usize,
    pub(crate) r_mask: usize,
}

impl PowerOfTwo {
    /// # Panics
    ///
    /// Calling with `value` 0 can cause panic.
    pub(crate) fn new(value: usize) -> Option<Self> {
        value.is_power_of_two().then_some(Self {
            value,
            power: value.trailing_zeros() as usize,
            r_mask: value - 1,
        })
    }

    #[inline(always)]
    pub(crate) fn quotient(&self, lhs: usize) -> usize {
        lhs >> self.power
    }

    #[inline(always)]
    pub(crate) fn remainder(&self, lhs: usize) -> usize {
        lhs & self.r_mask
    }
}

/// Splits slice into some of individual shared references.
/// `indices` must be sorted in ascending order and valid for the given `v`.
pub(crate) fn split<T, const N: usize>(mut v: &[T], mut indices: [usize; N]) -> [&T; N] {
    debug_assert!(
        indices.windows(2).all(|window| window[0] < window[1])
            && N <= v.len()
            && indices[N - 1] < v.len()
    );

    // Calculates diff of indices.
    for i in (1..indices.len()).rev() {
        indices[i] -= indices[i - 1] + 1;
    }

    // Safety: We're going to initialze right after.
    let mut arr: [MaybeUninit<&T>; N] = unsafe { MaybeUninit::uninit().assume_init() };

    for (i, mid) in indices.into_iter().enumerate() {
        let right = v.split_at(mid).1;
        let (x, right) = right.split_first().unwrap();
        arr[i].write(x);
        v = right;
    }

    // Safety: Everything is initialized and type is correct.
    // I don't know why we can't use transmute here?
    unsafe { transmute_copy::<_, _>(&arr) }
}

/// Splits slice into some of individual mutable references.
/// `indices` must be sorted in ascending order and valid for the given `v`.
pub(crate) fn split_mut<T, const N: usize>(
    mut v: &mut [T],
    mut indices: [usize; N],
) -> [&mut T; N] {
    debug_assert!(
        indices.windows(2).all(|window| window[0] < window[1])
            && N <= v.len()
            && indices[N - 1] < v.len()
    );

    // Calculates diff of indices.
    for i in (1..indices.len()).rev() {
        indices[i] -= indices[i - 1] + 1;
    }

    // Safety: We're going to initialze right after.
    let mut arr: [MaybeUninit<&mut T>; N] = unsafe { MaybeUninit::uninit().assume_init() };

    for (i, mid) in indices.into_iter().enumerate() {
        let right = v.split_at_mut(mid).1;
        let (x, right) = right.split_first_mut().unwrap();
        arr[i].write(x);
        v = right;
    }

    // Safety: Everything is initialized and type is correct.
    // I don't know why we can't use transmute here?
    unsafe { transmute_copy::<_, _>(&arr) }
}
