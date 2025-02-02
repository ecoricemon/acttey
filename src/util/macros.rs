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
pub(crate) use debug_format;
