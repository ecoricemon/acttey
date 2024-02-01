#![allow(dead_code)]

use std::{
    borrow::{Borrow, Cow}, hash::Hash, mem::{size_of, transmute}, ops::{Deref, Mul, Rem, Div}, rc::Rc, thread
};

pub mod macros;
pub mod web;
pub mod key;

pub mod prelude {
    pub use crate::{log, ty};
    pub use super::key::ResKey;
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

pub(crate) fn trim_end_digits(s: &mut String) {
    let digit_num = s.chars().rev().take_while(|c| c.is_ascii_digit()).count();
    s.truncate(s.len() - digit_num);
}

/// Used to be shown as a string even if it's not a string type.
pub trait ToStr {
    fn to_str(&self) -> Cow<str>;
}

/// Common [`ToStr`] implementation for [`str`].
impl ToStr for str {
    fn to_str(&self) -> Cow<str> {
        Cow::Borrowed(self)
    }
}

/// Common [`ToStr`] implementation for [`String`].
impl ToStr for String {
    fn to_str(&self) -> Cow<str> {
        Cow::Borrowed(self.as_str())
    }
}

/// Common [`ToStr`] implementation for [`Rc<str>`].
impl ToStr for Rc<str> {
    fn to_str(&self) -> Cow<str> {
        Cow::Borrowed(self)
    }
}

/// Common [`ToStr`] implementation for [`Box<str>`].
impl ToStr for Box<str> {
    fn to_str(&self) -> Cow<str> {
        Cow::Borrowed(self)
    }
}

/// Encodes a single byte into base64.
#[inline(always)]
pub const fn encode_base64(byte: u8) -> u8 {
    match byte {
        0..=25 => b'A' + byte,
        26..=51 => b'a' + byte - 26,
        52..=61 => b'0' + byte - 52,
        // URL safe version, standard: '+'
        62 => b'-',
        // URL safe version, standard: '/'
        63 => b'_',
        _ => panic!(),
    }
}

/// Encodes a single u32 value into base64.
#[inline]
pub const fn encode_base64_u32(value: u32) -> [u8; 6] {
    const MASK: u32 = (1 << 6) - 1;
    [
        encode_base64(((value >> 26) & MASK) as u8),
        encode_base64(((value >> 20) & MASK) as u8),
        encode_base64(((value >> 14) & MASK) as u8),
        encode_base64(((value >> 8) & MASK) as u8),
        encode_base64(((value >> 2) & MASK) as u8),
        encode_base64(((value << 4) & MASK) as u8),
    ]
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
    /// Creates `Option<RcStr>` from the [`OptionStr`].
    /// You can use that for a shared string.
    #[inline]
    pub fn as_rc_str(&self) -> Option<RcStr> {
        self.0.map(RcStr::from)
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

/// Rc\<str\> with From implementations.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct RcStr(Rc<str>);

impl From<&Rc<str>> for RcStr {
    fn from(value: &Rc<str>) -> Self {
        Self(Rc::clone(value))
    }
}

impl From<Rc<str>> for RcStr {
    fn from(value: Rc<str>) -> Self {
        Self(value)
    }
}

impl From<&RcStr> for RcStr {
    fn from(value: &RcStr) -> Self {
        value.clone()
    }
}

impl From<&str> for RcStr {
    fn from(value: &str) -> Self {
        Self(Rc::from(value))
    }
}

impl From<String> for RcStr {
    fn from(value: String) -> Self {
        Self(Rc::from(value))
    }
}

impl Deref for RcStr {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Borrow<str> for RcStr {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl AsRef<str> for RcStr {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

pub fn gcd<T>(a: T, b: T) -> T 
where
    T: Default + PartialEq + PartialOrd + Rem<Output = T> + Copy
{
    let _gcd = |mut a: T, mut b: T| {
        let zero = T::default();
        while b != zero {
            let r = a % b;
            a = b;
            b = r;
        }
        a
    };

    if a > b {
        _gcd(a, b)
    } else {
        _gcd(b, a)
    }
}

pub fn lcm<T>(a: T, b: T) -> T 
where
    T: Default + PartialEq + PartialOrd + Rem<Output = T> + Copy + Mul<Output = T> + Div<Output = T>,
{
    a * b / gcd(a, b)
}
