#[macro_export]
macro_rules! log {
    ($($t:tt)*) => {
        #[cfg(debug_assertions)]
        {
            $crate::util::macros::console_log(format!($($t)*));
        }
    }
}

pub fn console_log(s: String) {
    web_sys::console::log_1(&s.into());
}

/// Gets `TypeId` from the type itself or a reference.
///
/// # Examples
///
/// ```
/// use acttey::prelude::*;
///
/// struct A;
///
/// let a = A {};
///
/// assert_eq!(std::any::TypeId::of::<A>(), ty!(A));
/// assert_eq!(std::any::TypeId::of::<A>(), ty!(&&a));
/// ```
#[macro_export]
macro_rules! ty {
    (&$t:ident) => {
        std::any::Any::type_id($t)
    };
    (&&$t:ident) => {
        std::any::Any::type_id(&$t)
    };
    ($t:ty) => {
        std::any::TypeId::of::<$t>()
    };
    (<$type:ident as $trait:ident>::$item:ident) => {
        std::any::TypeId::of::<<$type as $trait>::$item>()
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

#[macro_export]
macro_rules! ntimes {
    ($n:expr, $($t:tt)*) => {
        for _ in 0..$n {
            $($t)*;
        }
    };
}

/// Implements [`From`] and [`TryFrom`] for the enum.
///
/// # Examples
///
/// ```
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
