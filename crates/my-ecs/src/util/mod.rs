pub mod prelude {
    pub use super::{AsType, AsTypes, Multi, Or, PowerOfTwo};
    pub use crate::{debug_format, filter, impl_from_for_enum, request, tinfo, unwrap_or};
}

use crate::ds::prelude::*;
use std::{
    fmt,
    ops::{Deref, DerefMut},
    slice::{Iter, IterMut},
};

#[macro_export]
macro_rules! type_name {
    ($e:expr) => {{
        fn name<T: ?Sized>(_x: &T) -> &str {
            std::any::type_name::<T>()
        }
        name(&$e)
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

/// Implements [`From`] and [`TryFrom`] for the enum.
///
/// # Examples
///
/// ```
/// # use my_ecs::impl_from_for_enum;
///
/// struct AA;
/// struct BB;
/// enum MyEnum {
///     A(AA),
///     B(BB),
/// }
///
/// impl_from_for_enum!(MyEnum, A, AA);
/// impl_from_for_enum!(MyEnum, B, BB);
/// ```
#[macro_export]
macro_rules! impl_from_for_enum {
    ($enum:ident, $var_outer:ident, $var_inner:ty) => {
        impl From<$var_inner> for $enum {
            fn from(value: $var_inner) -> Self {
                Self::$var_outer(value)
            }
        }

        impl TryFrom<$enum> for $var_inner {
            type Error = ();

            fn try_from(value: $enum) -> Result<Self, Self::Error> {
                if let $enum::$var_outer(inner) = value {
                    Ok(inner)
                } else {
                    Err(())
                }
            }
        }

        impl<'a> TryFrom<&'a mut $enum> for &'a mut $var_inner {
            type Error = ();

            fn try_from(value: &'a mut $enum) -> Result<Self, Self::Error> {
                if let $enum::$var_outer(inner) = value {
                    Ok(inner)
                } else {
                    Err(())
                }
            }
        }

        impl<'a> TryFrom<&'a $enum> for &'a $var_inner {
            type Error = ();

            fn try_from(value: &'a $enum) -> Result<Self, Self::Error> {
                if let $enum::$var_outer(inner) = value {
                    Ok(inner)
                } else {
                    Err(())
                }
            }
        }
    };
}

#[macro_export]
macro_rules! log {
    ($($t:tt)*) => {
        #[cfg(target_arch = "wasm32")]
        {
            $crate::util::console_log(format!($($t)*))
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            println!($($t)*)
        }
    }
}

#[cfg(target_arch = "wasm32")]
pub fn console_log(s: String) {
    web_sys::console::log_1(&s.into());
}

/// Sometimes, we want exiting early in a function due to deeply nested statements.
/// In that case, you can use this macro to hide boilerplate code.
#[macro_export]
macro_rules! unwrap_or {
    ($rhs:expr) => {
        if let Some(x) = $rhs {
            x
        } else {
            return;
        }
    };
    ($rhs:expr => $($else:tt)*) => {
        if let Some(x) = $rhs {
            x
        } else {
            $($else)*
        }
    };
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
pub struct Multi<T, const N: usize> {
    items: [T; N],
    cur: usize,
}

impl<T, const N: usize> Multi<T, N> {
    pub const fn new(items: [T; N]) -> Self {
        // Validates that N is non zero at compile time.
        let _: () = const { assert!(N > 0, "N must be greater than 0") };

        Self { items, cur: 0 }
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        self.items.get(index)
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.items.get_mut(index)
    }

    pub fn switch_to(&mut self, index: usize) -> &mut Self {
        assert!(index < N);
        self.cur = index;
        self
    }

    #[allow(clippy::len_without_is_empty)]
    pub const fn len(&self) -> usize {
        N
    }

    pub const fn is(&self, index: usize) -> bool {
        self.cur == index
    }

    pub fn iter(&self) -> Iter<T> {
        self.items.iter()
    }

    pub fn iter_mut(&mut self) -> IterMut<T> {
        self.items.iter_mut()
    }
}

impl<T, const N: usize> Deref for Multi<T, N> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // Safety: N must be greater than 0, and index must be in bounds.
        unsafe { self.items.get_unchecked(self.cur) }
    }
}

impl<T, const N: usize> DerefMut for Multi<T, N> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // Safety: N must be greater than 0, and index must be in bounds.
        unsafe { self.items.get_unchecked_mut(self.cur) }
    }
}

/// A trait to get [`TypeId`]s of elements inside a tuple.
/// Any tuple of types that implement [`AsType`] implements [`AsTypes`] as well automatically.
//
// See `impl_as_types` below.
pub trait AsTypes {
    fn types() -> impl Iterator<Item = TypeIdExt>;
}

/// Prevents confliction of blanket impl of [`AsTypes`].
/// We need to implement this trait manually for the types that need to implement `AsTypes`.
pub trait AsType: 'static {
    /// Provided.
    fn ty() -> TypeIdExt {
        TypeIdExt::of::<Self>()
    }
}

/// Implements [`AsTypes`] for an anonymous tuple.
#[macro_export]
macro_rules! impl_as_types {
    ($n:expr, $($i:expr),*) => {const _: () = {
        #[allow(unused_imports)]
        use $crate::{
            util::{AsTypes, AsType},
            ds::types::TypeIdExt,
        };
        use paste::paste;

        paste! {
            #[allow(unused_parens)]
            // We use `AsType` instead of 'static to avoid confliction.
            impl<$([<A $i>]: AsType),*> AsTypes for ( $([<A $i>]),* ) {
                fn types() -> impl Iterator<Item = TypeIdExt> {
                    [$(
                        TypeIdExt::of::<[<A $i>]>()
                    ),*].into_iter()
                }
            }
        }
    };};
}
impl_as_types!(0,);
impl_as_types!(1, 0);
impl_as_types!(2, 0, 1);
impl_as_types!(3, 0, 1, 2);
impl_as_types!(4, 0, 1, 2, 3);
impl_as_types!(5, 0, 1, 2, 3, 4);
impl_as_types!(6, 0, 1, 2, 3, 4, 5);
impl_as_types!(7, 0, 1, 2, 3, 4, 5, 6);
impl_as_types!(8, 0, 1, 2, 3, 4, 5, 6, 7);

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Or<A, B> {
    A(A),
    B(B),
}

impl<A: fmt::Debug, B: fmt::Debug> fmt::Debug for Or<A, B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::A(a) => a.fmt(f),
            Self::B(b) => b.fmt(f),
        }
    }
}
