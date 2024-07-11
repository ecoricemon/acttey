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
/// # use acttey::impl_from_for_enum;
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

/// Declares and implements a return struct to make call chain.
/// The return struct has `recv` and `ret` fields.
/// `recv` field is mutable receiver such as `&mut self`.
/// `ret` field is function's own return.
/// Also, this implements [`Deref`](std::ops::Deref) and [`DerefMut`](std::ops::DerefMut)
/// for the `recv` field, so that you can get the receiver by dereferencing this struct.
#[macro_export]
macro_rules! decl_return_wrap {
    ($struct_id:ident, $recv_type:ty, $ret_type:ty $(,$life:lifetime)?) => {
        pub struct $struct_id<'a $(,$life)?> {
            pub recv: &'a mut $recv_type,
            pub ret: $ret_type,
        }

        impl<'a $(,$life)?> std::ops::Deref for $struct_id<'a $(,$life)?> {
            type Target = $recv_type;

            fn deref(&self) -> &Self::Target {
                self.recv
            }
        }

        impl<'a $(,$life)?> std::ops::DerefMut for $struct_id<'a $(,$life)?> {
            fn deref_mut(&mut self) -> &mut Self::Target {
                self.recv
            }
        }
    };
}
