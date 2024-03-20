use std::mem::{transmute_copy, MaybeUninit};

#[inline(always)]
pub fn upcast_slice<T>(v: &mut [T]) -> *mut [()] {
    v as *mut [T] as *mut [()]
}

/// # Safety
///
/// Caller must provide the original type `T` of the `ptr`.
/// Calling this method with the incorrect type is *undefined behavior*.
#[inline(always)]
pub unsafe fn downcast_slice<'a, T>(ptr: *mut [()]) -> &'a [T] {
    &*(ptr as *const [T])
}

/// # Safety
///
/// Caller must provide the original type `T` of the `ptr`.
/// Calling this method with the incorrect type is *undefined behavior*.
#[inline(always)]
pub unsafe fn downcast_mut_slice<'a, T>(ptr: *mut [()]) -> &'a mut [T] {
    &mut *(ptr as *mut [T])
}

/// Splits slice into some of individual shared references.
/// `indices` must be sorted in ascending order and valid for the given `v`.
pub fn split<T, const N: usize>(mut v: &[T], mut indices: [usize; N]) -> [&T; N] {
    debug_assert!(
        indices.windows(2).all(|window| window[0] < window[1])
            && N <= v.len()
            && indices[N - 1] < v.len()
    );

    // Calculates diff of indices.
    for i in (1..indices.len()).rev() {
        indices[i] -= indices[i - 1] + 1;
    }

    // Safety: We're going to initialze right away.
    let mut arr: [MaybeUninit<&T>; N] = unsafe { MaybeUninit::uninit().assume_init() };

    for (i, mid) in indices.into_iter().enumerate() {
        let (x, tail) = v.split_at(mid).1.split_first().unwrap();
        arr[i].write(x);
        v = tail;
    }

    // Safety: Everything is initialized and type is correct.
    // I don't know why we can't use transmute here?
    unsafe { transmute_copy::<_, _>(&arr) }
}

/// Splits slice into some of individual mutable references.
/// `indices` must be sorted in ascending order and valid for the given `v`.
pub fn split_mut<T, const N: usize>(mut v: &mut [T], mut indices: [usize; N]) -> [&mut T; N] {
    debug_assert!(
        indices.windows(2).all(|window| window[0] < window[1])
            && N <= v.len()
            && indices[N - 1] < v.len()
    );

    // Calculates diff of indices.
    for i in (1..indices.len()).rev() {
        indices[i] -= indices[i - 1] + 1;
    }

    // Safety: We're going to initialze right away.
    let mut arr: [MaybeUninit<&mut T>; N] = unsafe { MaybeUninit::uninit().assume_init() };

    for (i, mid) in indices.into_iter().enumerate() {
        let (x, tail) = v.split_at_mut(mid).1.split_first_mut().unwrap();
        arr[i].write(x);
        v = tail;
    }

    // Safety: Everything is initialized and type is correct.
    // I don't know why we can't use transmute here?
    unsafe { transmute_copy::<_, _>(&arr) }
}

/// Updates value located at `target` using value at `by`.
/// If you need more values, consider using [`split_mut`], which is more powerful but more complex.
///
/// # Panics
///
/// Panics if `target` or `by` is out of index, or `target` is equal to `by`.
pub fn update_slice_by<T>(values: &mut [T], target: usize, by: usize, f: impl FnOnce(&mut T, &T)) {
    if target < by {
        let (tvalue, tail) = values.split_at_mut(target).1.split_first_mut().unwrap();
        f(tvalue, &tail[by - target - 1]);
    } else {
        let (bvalue, tail) = values.split_at_mut(by).1.split_first_mut().unwrap();
        f(&mut tail[target - by - 1], bvalue);
    }
}
