#![allow(dead_code)]

use std::{
    mem::{size_of, transmute},
    ops::Deref,
    rc::Rc,
    thread,
};

pub mod macros;
pub mod web;
pub use web::*;
pub mod wgpu;
pub use wgpu::*;

pub mod prelude {
    pub use crate::{log, ty};
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

    // Safety: We're going to initialze right away.
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

    // Safety: We're going to initialze right away.
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

pub(crate) fn concat_string(l: &str, r: &str) -> String {
    let mut s = String::with_capacity(l.len() + r.len());
    s.push_str(l);
    s.push_str(r);
    s
}

pub(crate) fn concat_opt_string(l: Option<&str>, r: &str) -> Option<String> {
    l.map(|l| concat_string(l, r))
}

pub enum AorB<AA, BB> {
    A(AA),
    B(BB),
}

/// Common [`AorB`] implementation for &str and String.
impl Deref for AorB<&str, String> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::A(s) => s,
            Self::B(s) => &s,
        }
    }
}

/// Used to be shown as a string even if it's not a string type.
pub trait ToStr {
    fn to_str(&self) -> AorB<&str, String>;
}

/// Common [`ToStr`] implementation for [`str`].
impl ToStr for str {
    fn to_str(&self) -> AorB<&str, String> {
        AorB::A(self)
    }
}

/// Common [`ToStr`] implementation for [`String`].
impl ToStr for String {
    fn to_str(&self) -> AorB<&str, String> {
        AorB::A(&self)
    }
}

/// Common [`ToStr`] implementation for [`Rc<str>`].
impl ToStr for Rc<str> {
    fn to_str(&self) -> AorB<&str, String> {
        AorB::A(&self)
    }
}

/// Common [`ToStr`] implementation for [`Box<str>`].
impl ToStr for Box<str> {
    fn to_str(&self) -> AorB<&str, String> {
        AorB::A(&self)
    }
}

/// It's same with Into<&\[u8\>.
/// Structs that implements Into<&\[u8\]> also implements this automatically.
pub trait AsBytes {
    fn as_bytes(&self) -> &[u8];
}

/// Blanket impl.
impl<T> AsBytes for T
where
    for<'a> &'a T: Into<&'a [u8]>,
{
    #[inline]
    fn as_bytes(&self) -> &[u8] {
        self.into()
    }
}

pub trait AsMultiBytes {
    fn as_bytes(&self, index: usize) -> &[u8];
}

/// A wrapper of `Option<&str>`. Empty string is considered as None.
/// This is interchangable with `Option<&str>` and `&str` using [`From::from()`] or [`Into::into()`].
pub struct OptionStr<'a>(Option<&'a str>);

impl<'a> OptionStr<'a> {
    /// Creates `Option<Rc<str>>` from the [`OptionStr`].
    /// You can use that for a shared string.
    #[inline]
    pub fn as_rc_str(&self) -> Option<Rc<str>> {
        self.0.map(Rc::from)
    }
}

impl<'a> Deref for OptionStr<'a> {
    type Target = Option<&'a str>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// &str -> OptionStr
impl<'a> From<&'a str> for OptionStr<'a> {
    #[inline]
    fn from(value: &'a str) -> Self {
        Self(if value.is_empty() { None } else { Some(value) })
    }
}

// OptionStr -> &str
impl<'a> From<OptionStr<'a>> for &'a str {
    #[inline]
    fn from(value: OptionStr<'a>) -> Self {
        value.0.unwrap_or_default()
    }
}

// Option<&str> -> OptionStr
impl<'a> From<Option<&'a str>> for OptionStr<'a> {
    #[inline]
    fn from(value: Option<&'a str>) -> Self {
        match value {
            Some(s) if !s.is_empty() => Self(value),
            _ => Self(None),
        }
    }
}

// OptionStr -> Option<&str>
impl<'a> From<OptionStr<'a>> for Option<&'a str> {
    #[inline]
    fn from(value: OptionStr<'a>) -> Self {
        value.0
    }
}
