pub mod prelude {
    pub use super::{PowerOfTwo, Twin};
    pub use crate::{debug_format, filter, request, tinfo};
}

use std::ops::{Deref, DerefMut};

#[macro_export]
macro_rules! fname {
    ($f:expr) => {{
        fn name<T: ?Sized>(_x: &T) -> &str {
            std::any::type_name::<T>()
        }
        name(&$f)
    }};
}

#[macro_export]
macro_rules! panic_if_called_twice {
    ($($msg: tt)*) => {
        use std::sync::atomic::{AtomicU8, Ordering};
        static mut ONCE: AtomicU8 = AtomicU8::new(0);
        let old = unsafe { ONCE.fetch_add(1, Ordering::Relaxed) };
        assert!(old == 0, $($msg)*);
    };
}

#[macro_export]
macro_rules! debug_format {
    ($($t:tt)*) => {{
        #[cfg(debug_assertions)]
        {
            format!($($t)*)
        }
        #[cfg(not(debug_assertions))]
        {
            String::new()
        }
    }};
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

    pub const fn get(&self) -> usize {
        self.value
    }

    pub const fn quotient(&self, lhs: usize) -> usize {
        (lhs >> self.k) & self.mask
    }

    pub const fn remainder(&self, lhs: usize) -> usize {
        lhs & self.value.wrapping_sub(1)
    }
}

#[derive(Debug, Clone)]
pub struct Twin<T> {
    a: T,
    b: T,
    is_a: bool,
}

impl<T> Twin<T> {
    pub fn new(a: T, b: T) -> Self {
        Self { a, b, is_a: true }
    }

    pub fn toggle(&mut self) {
        self.is_a = !self.is_a;
    }

    pub fn is_a(&self) -> bool {
        self.is_a
    }

    pub fn is_b(&self) -> bool {
        !self.is_a()
    }
}

impl<T> Deref for Twin<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        if self.is_a() {
            &self.a
        } else {
            &self.b
        }
    }
}

impl<T> DerefMut for Twin<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        if self.is_a() {
            &mut self.a
        } else {
            &mut self.b
        }
    }
}
