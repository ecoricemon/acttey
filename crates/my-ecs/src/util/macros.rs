#[macro_export]
macro_rules! type_name {
    ($e:expr) => {{
        fn name<T: ?Sized>(_t: &T) -> &str {
            let name = std::any::type_name::<T>();
            if matches!(name.chars().next(), Some(c) if c == '&') {
                &name[1..]
            } else {
                name
            }
        }
        name(&$e)
    }};
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
/// # use std::borrow::Cow;
///
/// enum MyEnum<'a, 'b: 'a, X, Y: Send + Sync, const N: usize, const M: usize> {
///     A(&'a Cow<'b, str>),
///     B((X, Y)),
///     C([char; N]),
///     D([char; M]),
/// }
///
/// impl_from_for_enum!(
///     "lifetimes" = 'a, 'b: {'a}; "bounds" = X, Y: {Send + Sync};
///     "consts" = N: usize, M: usize;
///     "outer" = MyEnum; "var" = A; "inner" = &'a Cow<'b, str>
/// );
///
/// ```
#[macro_export]
macro_rules! impl_from_for_enum {
    // ()
    (
        "outer" = $outer:ident;
        "var" = $var:ident;
        "inner" = $inner:ty $(;)?
    ) => {
        impl From<$inner> for $outer {
            fn from(v: $inner) -> Self { Self::$var(v) }
        }
        impl TryFrom<$outer> for $inner {
            type Error = ();
            fn try_from(v: $outer) -> Result<Self, Self::Error> {
                match v { $outer::$var(iv) => Ok(iv), _ => Err(()) }
            }
        }
        impl<'_ref> TryFrom<&'_ref $outer> for &'_ref $inner {
            type Error = ();
            fn try_from(v: &'_ref $outer) -> Result<Self, Self::Error> {
                match v { $outer::$var(iv) => Ok(iv), _ => Err(()) }
            }
        }
        impl<'_ref> TryFrom<&'_ref mut $outer> for &'_ref mut $inner {
            type Error = ();
            fn try_from(v: &'_ref mut $outer) -> Result<Self, Self::Error> {
                match v { $outer::$var(iv) => Ok(iv), _ => Err(()) }
            }
        }
    };
    // (consts)
    (
        "consts" = $( $c_id:ident : $c_ty:ty ),*;
        "outer" = $outer:ident;
        "var" = $var:ident;
        "inner" = $inner:ty $(;)?
    ) => {
        // inner -> outer
        impl<
            $( const $c_id : $c_ty ),*
        > From<$inner> for $outer<
            $( $c_id ),*
        >
        { fn from(v: $inner) -> Self { Self::$var(v) } }
        // outer -> inner
        impl<
            $( const $c_id : $c_ty ),*
        > TryFrom<$outer<
            $( $c_id ),*
        >> for $inner
        {
            type Error = ();
            fn try_from(v: $outer<
                $( $c_id ),*
            >) -> Result<Self, Self::Error> {
                match v { $outer::$var(iv) => Ok(iv), _ => Err(()) }
            }
        }
        // & outer -> & inner
        impl<
            '_ref,
            $( const $c_id : $c_ty ),*
        > TryFrom<&'_ref $outer<
            $( $c_id ),*
        >> for &'_ref $inner
        {
            type Error = ();
            fn try_from(v: &'_ref $outer<
                $( $c_id ),*
            >) -> Result<Self, Self::Error> {
                match v { $outer::$var(iv) => Ok(iv), _ => Err(()) }
            }
        }
        // &mut outer -> &mut inner
        impl<
            '_ref,
            $( const $c_id : $c_ty ),*
        > TryFrom<&'_ref mut $outer<
            $( $c_id ),*
        >> for &'_ref mut $inner
        {
            type Error = ();
            fn try_from(v: &'_ref mut $outer<
                $( $c_id ),*
            >) -> Result<Self, Self::Error> {
                match v { $outer::$var(iv) => Ok(iv), _ => Err(()) }
            }
        }
    };
    // (bounds, ..)
    (
        "bounds" = $( $b_id:ident $( : { $($b_sup:tt)* } )? ),*;
        $( "consts" = $( $c_id:ident : $c_ty:ty ),* ; )?
        "outer" = $outer:ident;
        "var" = $var:ident;
        "inner" = $inner:ty $(;)?
    ) => {
        // inner -> outer
        impl<
            $( $b_id $( : $($b_sup)* )? ),*
            $(, $( const $c_id : $c_ty ),* )?
        > From<$inner> for $outer<
            $( $b_id ),*
            $(, $( $c_id ),* )?
        >
        { fn from(v: $inner) -> Self { Self::$var(v) } }
        // outer -> inner
        impl<
            $( $b_id $( : $($b_sup)* )? ),*
            $(, $( const $c_id : $c_ty ),* )?
        > TryFrom<$outer<
            $( $b_id ),*
            $(, $( $c_id ),* )?
        >> for $inner
        {
            type Error = ();
            fn try_from(v: $outer<
                $( $b_id ),*
                $(, $( $c_id ),* )?
            >) -> Result<Self, Self::Error> {
                match v { $outer::$var(iv) => Ok(iv), _ => Err(()) }
            }
        }
        // & outer -> & inner
        impl<
            '_ref,
            $( $b_id $( : $($b_sup)* )? ),*
            $(, $( const $c_id : $c_ty ),* )?
        > TryFrom<&'_ref $outer<
            $( $b_id ),*
            $(, $( $c_id ),* )?
        >> for &'_ref $inner
        {
            type Error = ();
            fn try_from(v: &'_ref $outer<
                $( $b_id ),*
                $(, $( $c_id ),* )?
            >) -> Result<Self, Self::Error> {
                match v { $outer::$var(iv) => Ok(iv), _ => Err(()) }
            }
        }
        // &mut outer -> &mut inner
        impl<
            '_ref,
            $( $b_id $( : $($b_sup)* )? ),*
            $(, $( const $c_id : $c_ty ),* )?
        > TryFrom<&'_ref mut $outer<
            $( $b_id ),*
            $(, $( $c_id ),* )?
        >> for &'_ref mut $inner
        {
            type Error = ();
            fn try_from(v: &'_ref mut $outer<
                $( $b_id ),*
                $(, $( $c_id ),* )?
            >) -> Result<Self, Self::Error> {
                match v { $outer::$var(iv) => Ok(iv), _ => Err(()) }
            }
        }
    };
    // (lifetimes, ..)
    (
        "lifetimes" = $( $lt:lifetime $( : { $($lt_sup:tt)* } )? ),*;
        $( "bounds" = $( $b_id:ident $( : { $($b_sup:tt)* } )? ),* ; )?
        $( "consts" = $( $c_id:ident : $c_ty:ty ),* ; )?
        "outer" = $outer:ident;
        "var" = $var:ident;
        "inner" = $inner:ty $(;)?
    ) => {
        // inner -> outer
        impl<
            $( $lt $( : $($lt_sup)* )? ),*
            $(, $( $b_id $( : $($b_sup)* )? ),* )?
            $(, $( const $c_id : $c_ty ),* )?
        > From<$inner> for $outer<
            $($lt),*
            $(, $( $b_id ),* )?
            $(, $( $c_id ),* )?
        >
        { fn from(v: $inner) -> Self { Self::$var(v) } }
        // outer -> inner
        impl<
            $( $lt $( : $($lt_sup)* )? ),*
            $(, $( $b_id $( : $($b_sup)* )? ),* )?
            $(, $( const $c_id : $c_ty ),* )?
        > TryFrom<$outer<
            $($lt),*
            $(, $( $b_id ),* )?
            $(, $( $c_id ),* )?
        >> for $inner
        {
            type Error = ();
            fn try_from(v: $outer<
                $($lt),*
                $(, $( $b_id ),* )?
                $(, $( $c_id ),* )?
            >) -> Result<Self, Self::Error> {
                match v { $outer::$var(iv) => Ok(iv), _ => Err(()) }
            }
        }
        // & outer -> & inner
        impl<
            '_ref,
            $( $lt $( : $($lt_sup)* )? ),*
            $(, $( $b_id $( : $($b_sup)* )? ),* )?
            $(, $( const $c_id : $c_ty ),* )?
        > TryFrom<&'_ref $outer<
            $($lt),*
            $(, $( $b_id ),* )?
            $(, $( $c_id ),* )?
        >> for &'_ref $inner
        {
            type Error = ();
            fn try_from(v: &'_ref $outer<
                $($lt),*
                $(, $( $b_id ),* )?
                $(, $( $c_id ),* )?
            >) -> Result<Self, Self::Error> {
                match v { $outer::$var(iv) => Ok(iv), _ => Err(()) }
            }
        }
        // &mut outer -> &mut inner
        impl<
            '_ref,
            $( $lt $( : $($lt_sup)* )? ),*
            $(, $( $b_id $( : $($b_sup)* )? ),* )?
            $(, $( const $c_id : $c_ty ),* )?
        > TryFrom<&'_ref mut $outer<
            $($lt),*
            $(, $( $b_id ),* )?
            $(, $( $c_id ),* )?
        >> for &'_ref mut $inner
        {
            type Error = ();
            fn try_from(v: &'_ref mut $outer<
                $($lt),*
                $(, $( $b_id ),* )?
                $(, $( $c_id ),* )?
            >) -> Result<Self, Self::Error> {
                match v { $outer::$var(iv) => Ok(iv), _ => Err(()) }
            }
        }
    };
}

#[macro_export]
macro_rules! log {
    ($($t:tt)*) => {
        #[cfg(target_arch = "wasm32")]
        {
            $crate::util::macros::console_log(format!($($t)*))
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
//
// TODO: I heard Rust made something like this...
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

#[macro_export]
macro_rules! impl_into_iterator_body_for_parallel {
    ($item:ty, $to:ty) => {
        type Item = $item;
        type IntoIter = $to;

        #[inline]
        fn into_iter(self) -> Self::IntoIter {
            self.into_seq()
        }
    };
}

// bound matching pattern.
#[macro_export]
macro_rules! impl_into_iterator_for_parallel {
    // ()
    (
        "for" = $for:ident;
        "to" = $to:ty;
        "item" = $item:ty $(;)?
    ) => {
        impl IntoIterator for $for
        {
            $crate::impl_into_iterator_body_for_parallel!($item, $to);
        }
    };
    // (consts)
    (
        "consts" = $( $c_id:ident : $c_ty:ty ),*;
        "for" = $for:ident;
        "to" = $to:ty;
        "item" = $item:ty $(;)?
    ) => {
        impl<
            $( const $c_id : $c_ty ),*
        > IntoIterator for $for<
            $( $c_id ),*
        >
        {
            $crate::impl_into_iterator_body_for_parallel!($item, $to);
        }
    };
    // (bounds, ..)
    (
        "bounds" = $( $b_id:ident $( : { $($b_sup:tt)* } )? ),*;
        $( "consts" = $( $c_id:ident : $c_ty:ty ),* ; )?
        "for" = $for:ident;
        "to" = $to:ty;
        "item" = $item:ty $(;)?
    ) => {
        impl<
            $( $b_id $( : $($b_sup)* )? ),*
            $(, $( const $c_id : $c_ty ),* )?
        > IntoIterator for $for<
            $( $b_id ),*
            $(, $( $c_id ),* )?
        >
        {
            $crate::impl_into_iterator_body_for_parallel!($item, $to);
        }
    };
    // (lifetimes, ..)
    (
        "lifetimes" = $( $lt:lifetime ),*;
        $( "bounds" = $( $b_id:ident $( : { $($b_sup:tt)* } )? ),* ; )?
        $( "consts" = $( $c_id:ident : $c_ty:ty ),* ; )?
        "for" = $for:ident;
        "to" = $to:ty;
        "item" = $item:ty $(;)?
    ) => {
        impl<
            $($lt),*
            $(, $( $b_id $( : $($b_sup)* )? ),* )?
            $(, $( const $c_id : $c_ty ),* )?
        > IntoIterator for $for<
            $($lt),*
            $(, $( $b_id ),* )?
            $(, $( $c_id ),* )?
        >
        {
            $crate::impl_into_iterator_body_for_parallel!($item, $to);
        }
    };
}

#[macro_export]
macro_rules! impl_parallel_iterator_body {
    ($item:ty) => {
        type Item = $item;

        #[inline]
        fn drive_unindexed<C>(self, consumer: C) -> C::Result
        where
            C: rayon::iter::plumbing::UnindexedConsumer<Self::Item>,
        {
            rayon::iter::plumbing::bridge(self, consumer)
        }
    };
}

#[macro_export]
macro_rules! impl_indexed_parallel_iterator_body {
    () => {
        #[inline]
        fn len(&self) -> usize {
            Self::len(self)
        }

        #[inline]
        fn drive<C: rayon::iter::plumbing::Consumer<Self::Item>>(self, consumer: C) -> C::Result {
            rayon::iter::plumbing::bridge(self, consumer)
        }

        #[inline]
        fn with_producer<CB: rayon::iter::plumbing::ProducerCallback<Self::Item>>(
            self,
            callback: CB,
        ) -> CB::Output {
            callback.callback(self)
        }
    };
}

// bound matching pattern.
#[macro_export]
macro_rules! impl_parallel_iterator {
    // ()
    (
        "for" = $for:ident;
        "item" = $item:ty $(;)?
    ) => {
        impl rayon::iter::ParallelIterator for $for {
            $crate::impl_parallel_iterator_body!($item);
        }

        impl rayon::iter::IndexedParallelIterator for $for
        {
            $crate::impl_indexed_parallel_iterator_body!();
        }
    };
    // (consts)
    (
        "consts" = $( $c_id:ident : $c_ty:ty ),*;
        "for" = $for:ident;
        "item" = $item:ty $(;)?
    ) => {
        impl<
            $( const $c_id : $c_ty ),*
        > rayon::iter::ParallelIterator for $for<
            $( $c_id ),*
        > {
            $crate::impl_parallel_iterator_body!($item);
        }

        impl<
            $( $b_id $( : $($b_sup)* )? ),*
        > rayon::iter::IndexedParallelIterator for $for<
            $( $c_id ),*
        >
        {
            $crate::impl_indexed_parallel_iterator_body!();
        }
    };
    // (bounds, ..)
    (
        "bounds" = $( $b_id:ident $( : { $($b_sup:tt)* } )? ),*;
        $( "consts" = $( $c_id:ident : $c_ty:ty ),* ; )?
        "for" = $for:ident;
        "item" = $item:ty $(;)?
    ) => {
        impl<
            $( $b_id $( : $($b_sup)* )? ),*
            $(, $( const $c_id : $c_ty ),* )?
        > rayon::iter::ParallelIterator for $for<
            $( $b_id ),*
            $(, $( $c_id ),* )?
        > {
            $crate::impl_parallel_iterator_body!($item);
        }

        impl<
            $( $b_id $( : $($b_sup)* )? ),*
            $(, $( const $c_id : $c_ty ),* )?
        > rayon::iter::IndexedParallelIterator for $for<
            $( $b_id ),*
            $(, $( $c_id ),* )?
        >
        {
            $crate::impl_indexed_parallel_iterator_body!();
        }
    };
    // (lifetimes, ..)
    (
        "lifetimes" = $( $lt:lifetime ),*;
        $( "bounds" = $( $b_id:ident $( : { $($b_sup:tt)* } )? ),* ; )?
        $( "consts" = $( $c_id:ident : $c_ty:ty ),* ; )?
        "for" = $for:ident;
        "item" = $item:ty $(;)?
    ) => {
        impl<
            $($lt),*
            $(, $( $b_id $( : $($b_sup)* )? ),* )?
            $(, $( const $c_id : $c_ty ),* )?
        > rayon::iter::ParallelIterator for $for<
            $($lt),*
            $(, $( $b_id ),* )?
            $(, $( $c_id ),* )?
        > {
            $crate::impl_parallel_iterator_body!($item);
        }

        impl<
            $($lt),*
            $(, $( $b_id $( : $($b_sup)* )? ),* )?
            $(, $( const $c_id : $c_ty ),* )?
        > rayon::iter::IndexedParallelIterator for $for<
            $($lt),*
            $(, $( $b_id ),* )?
            $(, $( $c_id ),* )?
        >
        {
            $crate::impl_indexed_parallel_iterator_body!();
        }
    };
}

#[macro_export]
macro_rules! impl_unindexed_producer_body {
    ($item:ty) => {
        type Item = $item;

        #[inline]
        fn split(self) -> (Self, Option<Self>) {
            let mid = Self::len(&self) / 2;
            let (l, r) = self.split_at(mid);
            (l, (!r.is_empty()).then_some(r))
        }

        #[inline]
        fn fold_with<F>(self, folder: F) -> F
        where
            F: rayon::iter::plumbing::Folder<Self::Item>,
        {
            folder.consume_iter(self.into_seq())
        }
    };
}

// bound matching pattern.
#[macro_export]
macro_rules! impl_unindexed_producer {
    // ()
    (
        "for" = $for:ident;
        "item" = $item:ty $(;)?
    ) => {
        impl rayon::iter::plumbing::UnindexedProducer for $for
        {
            $crate::impl_unindexed_producer_body!($item);
        }
    };
    // (consts)
    (
        "consts" = $( $c_id:ident : $c_ty:ty ),*;
        "for" = $for:ident;
        "item" = $item:ty $(;)?
    ) => {
        impl<
            $( const $c_id : $c_ty ),*
        > rayon::iter::plumbing::UnindexedProducer for $for<
            $( $c_id ),*
        >
        {
            $crate::impl_unindexed_producer_body!($item);
        }
    };
    // (bounds, ..)
    (
        "bounds" = $( $b_id:ident $( : { $($b_sup:tt)* } )? ),*;
        $( "consts" = $( $c_id:ident : $c_ty:ty ),* ; )?
        "for" = $for:ident;
        "item" = $item:ty $(;)?
    ) => {
        impl<
            $( $b_id $( : $($b_sup)* )? ),*
            $(, $( const $c_id : $c_ty ),* )?
        > rayon::iter::plumbing::UnindexedProducer for $for<
            $( $b_id ),*
            $(, $( $c_id ),* )?
        >
        {
            $crate::impl_unindexed_producer_body!($item);
        }
    };
    // (lifetimes, ..)
    (
        "lifetimes" = $( $lt:lifetime ),*;
        $( "bounds" = $( $b_id:ident $( : { $($b_sup:tt)* } )? ),* ; )?
        $( "consts" = $( $c_id:ident : $c_ty:ty ),* ; )?
        "for" = $for:ident;
        "item" = $item:ty $(;)?
    ) => {
        impl<
            $($lt),*
            $(, $( $b_id $( : $($b_sup)* )? ),* )?
            $(, $( const $c_id : $c_ty ),* )?
        > rayon::iter::plumbing::UnindexedProducer for $for<
            $($lt),*
            $(, $( $b_id ),* )?
            $(, $( $c_id ),* )?
        >
        {
            $crate::impl_unindexed_producer_body!($item);
        }
    };
}
