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
/// struct A;
///
/// let a = A {};
///
/// assert_eq!(std::any::TypeId::of<A>(), ty!(<A>));
/// assert_eq!(std::any::TypeId::of<A>(), ty!(&a));
/// ```
#[macro_export]
macro_rules! ty {
    (<$t:ty>) => {
        std::any::TypeId::of::<$t>()
    };
    ($($t:tt)*) => {
        std::any::Any::type_id($($t)*)
    };
}
