#![allow(dead_code)]

pub mod key;
pub mod macros;
pub mod slice;
pub mod string;
pub mod web;

pub mod prelude {
    pub use super::key::ResKey;
    pub use crate::{log, ty};
}

use std::{
    mem::{size_of, transmute},
    ops::{Deref, DerefMut, Div, Mul, Rem},
    thread,
};

pub fn current_thread_id() -> u64 {
    let id = thread::current().id();

    // Safety: ThreadId and u64 are both 8 bytes.
    debug_assert_eq!(size_of::<thread::ThreadId>(), size_of::<u64>());
    unsafe { transmute(id) }
}

/// A structure representing 2^k value.
/// But you can designate zero to this structure although zero is not 2^k.
/// In that case, zero is considered as usize::MAX + 1.
#[derive(Debug, Clone)]
pub struct PowerOfTwo {
    value: usize,
    k: u32,
    mask: usize,
}

impl PowerOfTwo {
    pub const fn new(value: usize) -> Option<Self> {
        if value.is_power_of_two() {
            Some(if value == 0 {
                Self {
                    value,
                    k: 0,
                    mask: 0,
                }
            } else {
                Self {
                    value,
                    k: value.trailing_zeros(),
                    mask: usize::MAX,
                }
            })
        } else {
            None
        }
    }

    #[inline(always)]
    pub const fn get(&self) -> usize {
        self.value
    }

    #[inline(always)]
    pub const fn quotient(&self, lhs: usize) -> usize {
        (lhs >> self.k) & self.mask
    }

    #[inline(always)]
    pub const fn remainder(&self, lhs: usize) -> usize {
        lhs & self.value.wrapping_sub(1)
    }
}

/// It's same with Into<&\[u8]\>.
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

    /// Retrieves offset + index.  
    /// You can add them manually if you don't want bound check.
    ///
    /// # Panics
    ///
    /// Panics if offset + index is out of bound.
    #[inline]
    pub const fn index(&self, index: usize) -> usize {
        let res = self.offset + index;
        assert!(res < self.end());
        res
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
    T: Copy + From<usize> + Mul<T, Output = T>,
{
    #[inline]
    pub const fn new(offset: usize, len: usize, item_size: T) -> Self {
        Self {
            win: Window::new(offset, len),
            item_size,
        }
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
