#![allow(dead_code)]

use std::{
    borrow::{Borrow, Cow},
    hash::Hash,
    mem::{size_of, transmute},
    ops::{Deref, DerefMut, Div, Mul, Rem},
    rc::Rc,
    thread,
};

pub mod key;
pub mod macros;
pub mod web;

pub mod prelude {
    pub use super::key::ResKey;
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

#[derive(Debug, Clone)]
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
pub(crate) fn update_slice_by<T>(
    values: &mut [T],
    target: usize,
    by: usize,
    f: impl FnOnce(&mut T, &T),
) {
    if target < by {
        let (tvalue, tail) = values.split_at_mut(target).1.split_first_mut().unwrap();
        f(tvalue, &tail[by - target - 1]);
    } else {
        let (bvalue, tail) = values.split_at_mut(by).1.split_first_mut().unwrap();
        f(&mut tail[target - by - 1], bvalue);
    }
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
    T: Default + PartialEq + PartialOrd + Rem<Output = T> + Copy,
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
    T: Default
        + PartialEq
        + PartialOrd
        + Rem<Output = T>
        + Copy
        + Mul<Output = T>
        + Div<Output = T>,
{
    a * b / gcd(a, b)
}

/// A window in an array.
/// It's represented by *offset* and *length*.
#[derive(Debug, Clone, Copy)]
pub struct Window {
    pub offset: usize,
    pub len: usize,
}

impl Window {
    #[inline]
    pub const fn new(offset: usize, len: usize) -> Self {
        Self { offset, len }
    }

    /// Determines whether window's length is zero or not.
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len == 0
    }

    #[inline]
    pub fn reset(&mut self, offset: usize, len: usize) {
        self.offset = offset;
        self.len = len;
    }

    #[inline]
    pub const fn range(&self) -> std::ops::Range<usize> {
        self.offset..self.end()
    }

    #[inline]
    pub const fn end(&self) -> usize {
        self.offset + self.len
    }

    /// Shrinks window from left side by `amount`.
    ///
    /// # Panic
    ///
    /// Panics if `amount` is greater than `len`.
    #[inline]
    pub fn shrink_rev(&mut self, amount: usize) {
        self.offset += amount;
        self.len -= amount;
    }
}

/// A view is a descriptor to see a part of a buffer.
/// This is composed of [`Window`] and *unit item size* in byte.
#[derive(Debug, Clone, Copy)]
pub struct View<T = usize> {
    win: Window,
    item_size: T,
}

impl<T> View<T> 
where
    T: Copy + From<usize> + Mul<T, Output = T> 
{
    #[inline]
    pub const fn new(offset: usize, len: usize, item_size: T) -> Self {
        Self { win: Window::new(offset, len), item_size }
    }

    /// Sets unit item size in bytes.
    #[inline]
    pub fn set_item_size(&mut self, item_size: T) {
        self.item_size = item_size;
    }

    /// Retrieves unit item size in bytes.
    #[inline]
    pub const fn get_item_size(&self) -> T {
        self.item_size
    }

    /// Retrieves offset in bytes.
    #[inline]
    pub fn byte_offset(&self) -> T {
        T::from(self.win.offset) * self.item_size
    }

    /// Retrieves end position in bytes. End here means the next byte of the view.
    #[inline]
    pub fn byte_end(&self) -> T {
        T::from(self.win.end()) * self.item_size
    }

    /// Retrieves buffer window size in bytes.
    #[inline]
    pub fn size(&self) -> T {
        T::from(self.win.len) * self.item_size
    }

    #[inline]
    pub fn byte_range(&self) -> std::ops::Range<T> {
        self.byte_offset()..self.byte_end()
    }
}

impl Deref for View {
    type Target = Window;

    fn deref(&self) -> &Self::Target {
        &self.win
    }
}

impl DerefMut for View {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.win
    }
}

/// 2D or 1D index.
/// Two indices used together to point a specific item in nested array.
/// Or you can use one index only.
#[derive(Debug, Clone, Copy)]
pub struct Index2 {
    /// Primary index.
    pub first: usize,

    /// Secondary index, which can be unused.
    pub second: usize,
}

impl Index2 {
    const INVALID: usize = usize::MAX;

    /// Creates with uninitialized state.
    #[inline]
    pub fn uninit() -> Self {
        Self {
            first: Self::INVALID,
            second: Self::INVALID,
        }
    }

    #[inline]
    pub fn into_single(&mut self, first: usize) {
        self.first = first;
        self.second = Self::INVALID;
    }

    #[inline]
    pub fn into_pair(&mut self, first: usize, second: usize) {
        self.first = first;
        self.second = second;
    }

    #[inline]
    pub fn is_single(&self) -> bool {
        self.first != Self::INVALID && self.second == Self::INVALID
    }

    #[inline]
    pub fn is_pair(&self) -> bool {
        self.first != Self::INVALID && self.second != Self::INVALID
    }

    #[inline]
    pub fn is_valid(&self) -> bool {
        self.is_single() || self.is_pair()
    }

    /// For convenience.
    #[inline]
    pub fn get(&self) -> (Option<usize>, Option<usize>) {
        (
            (self.first != Self::INVALID).then_some(self.first),
            (self.second != Self::INVALID).then_some(self.second),
        )
    }
}
