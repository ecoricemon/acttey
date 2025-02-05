//! Utilities for the crate.

pub(crate) mod macros;
pub mod str;
pub mod web;

pub mod prelude {
    pub use super::str as str_util;
    #[cfg(target_arch = "wasm32")]
    pub use super::web as web_util;
    pub use super::{Or, PowerOfTwo, TakeRecur, With, WithResult};
}

use my_ecs_macros::repeat_macro;
use std::{
    fmt, hash,
    ops::{Deref, DerefMut},
    sync::{Arc, Condvar, Mutex},
    thread,
};

/// A trait for taking inner value out.
///
/// Various types have methods like `take()` to take something out by consuming
/// the type itself. If the taken value also can be unwrapped, then clients
/// need to write code like `take().take()`. This trait helps to avoid something
/// like that and replace it with just one call.
///
/// # Examples
///
/// ```
/// use my_ecs::prelude::TakeRecur;
///
/// struct A(B);
/// struct B(C);
/// struct C(i32);
///
/// impl TakeRecur for A
/// where
///     B: TakeRecur
/// {
///     type Inner = <B as TakeRecur>::Inner;
///     fn take_recur(self) -> Self::Inner { self.0.take_recur() }
/// }
///
/// impl TakeRecur for B
/// where
///     C: TakeRecur,
/// {
///     type Inner = <C as TakeRecur>::Inner;
///     fn take_recur(self) -> Self::Inner { self.0.take_recur() }
/// }
///
/// impl TakeRecur for C {
///     type Inner = i32;
///     fn take_recur(self) -> Self::Inner { self.0 }
/// }
///
/// let value = A(B(C(42)));
/// assert_eq!(value.take_recur(), 42);
/// ```
pub trait TakeRecur {
    type Inner;

    /// Takes out inner value recursively then returns it.
    ///
    /// # Examples
    ///
    /// See [`TakeRecur`] documentation.
    fn take_recur(self) -> Self::Inner;
}

/// Implements [`TakeRecur`] for (A0, A1), (A0, A1, A2), ...
macro_rules! impl_take_recur_for_tuple {
    ($n:expr, $($i:expr),*) => {const _: () = {
        use paste::paste;

        paste! {
            impl<$([<A $i>]: TakeRecur),*> TakeRecur for ( $([<A $i>]),* ) {
                type Inner = ( $([<A $i>]::Inner),* );

                fn take_recur(self) -> Self::Inner {
                    ( $( self.$i.take_recur(), )* )
                }
            }
        }
    };};
}
repeat_macro!(impl_take_recur_for_tuple, 2..=8);

/// A type representing 2^k `usize`.
///
/// Zero is not 2^k though you can create [`PowerOfTwo`] by zero. In that case,
/// zero is considered as `usize::MAX + 1` which is another 2^k.
#[derive(Debug, Clone, Copy)]
pub struct PowerOfTwo {
    value: usize,
    k: u32,
    mask: usize,
}

impl PowerOfTwo {
    /// Creates a new [`PowerOfTwo`] from the given value.
    pub const fn new(value: usize) -> Option<Self> {
        if value == 0 {
            Some(Self {
                value,
                k: 0,
                mask: 0,
            })
        } else if value.is_power_of_two() {
            Some(Self {
                value,
                k: value.trailing_zeros(),
                mask: usize::MAX,
            })
        } else {
            None
        }
    }

    /// Returns inner value.
    pub const fn get(&self) -> usize {
        self.value
    }

    /// Returns `numerator / self`.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::PowerOfTwo;
    ///
    /// let v = PowerOfTwo::new(4).unwrap();
    /// assert_eq!(v.quotient(3), 0);
    /// assert_eq!(v.quotient(4), 1);
    /// assert_eq!(v.quotient(5), 1);
    /// ```
    pub const fn quotient(&self, numerator: usize) -> usize {
        (numerator >> self.k) & self.mask
    }

    /// Returns `numerator % self`.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::PowerOfTwo;
    ///
    /// let v = PowerOfTwo::new(4).unwrap();
    /// assert_eq!(v.remainder(3), 3);
    /// assert_eq!(v.remainder(4), 0);
    /// assert_eq!(v.remainder(5), 1);
    /// ```
    pub const fn remainder(&self, numerator: usize) -> usize {
        numerator & self.value.wrapping_sub(1)
    }
}

impl PartialEq for PowerOfTwo {
    fn eq(&self, other: &Self) -> bool {
        // It's sufficient to compare `value` only because others are determined
        // by the value.
        self.value == other.value
    }
}

impl Eq for PowerOfTwo {}

impl PartialOrd for PowerOfTwo {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PowerOfTwo {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // It's sufficient to compare `value` only because others are determined
        // by the value.
        self.value.cmp(&other.value)
    }
}

impl hash::Hash for PowerOfTwo {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        // It's sufficient to hash `value` only because others are determined
        // by the value.
        self.value.hash(state);
    }
}

/// A type that either `A` or `B`.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Or<A, B> {
    A(A),
    B(B),
}

impl<A, B> Or<A, B> {
    /// Applies the given function to the value `A` then returns the result.
    pub fn map_a<F, X>(self, op: F) -> Or<X, B>
    where
        F: FnOnce(A) -> X,
    {
        match self {
            Or::A(a) => Or::A(op(a)),
            Or::B(b) => Or::B(b),
        }
    }

    /// Applies the given function to the value `B` then returns the result.
    pub fn map_b<G, Y>(self, op: G) -> Or<A, Y>
    where
        G: FnOnce(B) -> Y,
    {
        match self {
            Or::A(a) => Or::A(a),
            Or::B(b) => Or::B(op(b)),
        }
    }

    /// Applies the given functions to the value `A` and `B` respectively then
    /// returns the result.
    pub fn map_ab<F, G, X, Y>(self, op_a: F, op_b: G) -> Or<X, Y>
    where
        F: FnOnce(A) -> X,
        G: FnOnce(B) -> Y,
    {
        match self {
            Or::A(a) => Or::A(op_a(a)),
            Or::B(b) => Or::B(op_b(b)),
        }
    }
}

impl<A: fmt::Debug, B: fmt::Debug> fmt::Debug for Or<A, B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::A(a) => a.fmt(f),
            Self::B(b) => b.fmt(f),
        }
    }
}

/// A value with another value.
///
/// This type is almost the same as tuple `(T, U)` except how the type is
/// displayed by [`Display`](fmt::Display). This type only prints first value
/// only.
#[derive(Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
#[repr(C)]
pub struct With<T, U> {
    pub value: T,
    pub with: U,
}

impl<T, U> fmt::Display for With<T, U>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl<T, U> With<T, U> {
    /// Creates a new [`With`].
    pub const fn new(value: T, with: U) -> Self {
        Self { value, with }
    }
}

/// A value with [`Result`].
///
/// This type implements [`Deref`] and [`DerefMut`] for the value, therefore it
/// looks like value. But the type provides some `Result` methods as well.
pub type WithResult<O, T, E> = With<O, Result<T, E>>;

impl<O, T, E> WithResult<O, T, E> {
    /// Converts [`WithResult`] into [`Result`] by unwraping self.
    pub fn take(self) -> Result<T, E> {
        self.with
    }

    /// See [`Result::is_ok`].
    pub const fn is_ok(&self) -> bool {
        self.with.is_ok()
    }

    /// See [`Result::is_ok_and`].
    pub fn is_ok_and(self, f: impl FnOnce(T) -> bool) -> bool {
        self.with.is_ok_and(f)
    }

    /// See [`Result::is_err`].
    pub const fn is_err(&self) -> bool {
        self.with.is_err()
    }

    /// See [`Result::is_err_and`].
    pub fn is_err_and(self, f: impl FnOnce(E) -> bool) -> bool {
        self.with.is_err_and(f)
    }

    /// See [`Result::ok`].
    pub fn ok(self) -> Option<T> {
        self.with.ok()
    }

    /// See [`Result::err`].
    pub fn err(self) -> Option<E> {
        self.with.err()
    }

    /// See [`Result::as_ref`].
    pub const fn as_ref(&self) -> Result<&T, &E> {
        self.with.as_ref()
    }

    /// See [`Result::as_mut`].
    pub fn as_mut(&mut self) -> Result<&mut T, &mut E> {
        self.with.as_mut()
    }

    /// Applies the given function to the result. See [`Result::map`].
    pub fn map<F, U>(self, op: F) -> WithResult<O, U, E>
    where
        F: FnOnce(T) -> U,
    {
        WithResult::new(self.value, self.with.map(op))
    }

    /// Applies the given function to the result. See [`Result::map_err`].
    pub fn map_err<F, D>(self, op: F) -> WithResult<O, T, D>
    where
        F: FnOnce(E) -> D,
    {
        WithResult::new(self.value, self.with.map_err(op))
    }

    /// See [`Result::expect`].
    pub fn expect(self, msg: &str) -> T
    where
        E: fmt::Debug,
    {
        self.with.expect(msg)
    }

    /// See [`Result::unwrap`].
    pub fn unwrap(self) -> T
    where
        E: fmt::Debug,
    {
        self.with.unwrap()
    }
}

impl<O, T, E> Deref for WithResult<O, T, E>
where
    E: fmt::Debug,
{
    type Target = O;

    fn deref(&self) -> &Self::Target {
        if let Err(e) = &self.with {
            panic!("{e:?}");
        }
        &self.value
    }
}

impl<O, T, E> DerefMut for WithResult<O, T, E>
where
    E: fmt::Debug,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        if let Err(e) = &self.with {
            panic!("{e:?}");
        }
        &mut self.value
    }
}

pub fn call_timeout<F>(mut f: F, name: &str, repeat: usize, mut timeout: std::time::Duration)
where
    F: FnMut() + Send + 'static,
{
    let pair = Arc::new((Mutex::new(false), Condvar::new()));
    let pair2 = Arc::clone(&pair);

    let _handle = thread::spawn(move || {
        for _ in 0..repeat {
            f();
        }

        let (lock, cvar) = &*pair2;
        let mut fin = lock.lock().unwrap();
        *fin = true;
        cvar.notify_one();
    });

    let (lock, cvar) = &*pair;
    let mut fin = lock.lock().unwrap();
    while !*fin {
        let start = std::time::Instant::now();

        let res = cvar.wait_timeout(fin, timeout).unwrap();
        fin = res.0;

        let elapsed = start.elapsed();
        if let Some(remain) = timeout.checked_sub(elapsed) {
            timeout = remain;
        } else {
            #[cfg(unix)]
            {
                use std::os::unix::thread::JoinHandleExt;
                unsafe { libc::pthread_cancel(_handle.into_pthread_t()) };
                panic!("timed out: {name}");
            }

            #[cfg(not(unix))]
            {
                eprintln!("timed out: {name}");
                std::process::exit(101); // Panic result.
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_power_of_two() {
        // Test new method with valid powers of two
        let v4 = PowerOfTwo::new(4).unwrap();
        assert_eq!(v4.get(), 4);
        assert_eq!(v4.quotient(3), 0);
        assert_eq!(v4.quotient(4), 1);
        assert_eq!(v4.quotient(5), 1);

        // Test new method with invalid number
        assert_eq!(PowerOfTwo::new(3), None);

        // Test zero case
        let v0 = PowerOfTwo::new(0).unwrap();
        assert_eq!(v0.get(), 0);
        assert_eq!(v0.quotient(0), 0);
        assert_eq!(v0.quotient(42), 0);
        assert_eq!(v0.remainder(0), 0);
        assert_eq!(v0.remainder(1), 1);

        // Test one case
        let v1 = PowerOfTwo::new(1).unwrap();
        assert_eq!(v1.get(), 1);
        assert_eq!(v1.quotient(3), 3);
        assert_eq!(v1.quotient(4), 4);
        assert_eq!(v1.remainder(2), 0);
    }
}
