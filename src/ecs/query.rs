use super::{
    borrow_js::JsAtomic,
    filter::{FilterInfo, FilterKey, Filtered, FilteredIter, FilteredIterMut, StoreFilterInfo},
    resource::ResourceKey,
};
use crate::ds::borrow::Borrowed;
use std::{
    any::TypeId,
    ops::{Deref, DerefMut},
    ptr::NonNull,
    sync::Arc,
};

/// A shallow wrapper structure for the [`Query::Output`].
pub struct Read<'r, R: Query>(pub(crate) <R as Query>::Output<'r>);

impl<'r, R: Query> Read<'r, R> {
    #[inline]
    pub fn unwrap(self) -> <R as Query>::Output<'r> {
        self.0
    }
}

impl<'r, R: Query> Deref for Read<'r, R> {
    type Target = <R as Query>::Output<'r>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// A shallow wrapper structure for the [`QueryMut::Output`].
pub struct Write<'w, W: QueryMut>(pub(crate) <W as QueryMut>::Output<'w>);

impl<'w, W: QueryMut> Write<'w, W> {
    #[inline]
    pub fn unwrap(self) -> <W as QueryMut>::Output<'w> {
        self.0
    }
}

impl<'w, W: QueryMut> Deref for Write<'w, W> {
    type Target = <W as QueryMut>::Output<'w>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'w, W: QueryMut> DerefMut for Write<'w, W> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// A shallow wrapper structure for the [`ResQuery::Output`].
pub struct ResRead<RR: ResQuery>(pub(crate) <RR as ResQuery>::Output);

impl<RR: ResQuery> ResRead<RR> {
    #[inline]
    pub fn unwrap(self) -> <RR as ResQuery>::Output {
        self.0
    }
}

impl<RR: ResQuery> Deref for ResRead<RR> {
    type Target = <RR as ResQuery>::Output;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// A shallow wrapper structure for the [`ResQueryMut::Output`].
pub struct ResWrite<RW: ResQueryMut>(pub(crate) <RW as ResQueryMut>::Output);

impl<RW: ResQueryMut> ResWrite<RW> {
    #[inline]
    pub fn unwrap(self) -> <RW as ResQueryMut>::Output {
        self.0
    }
}

impl<RW: ResQueryMut> Deref for ResWrite<RW> {
    type Target = <RW as ResQueryMut>::Output;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<RW: ResQueryMut> DerefMut for ResWrite<RW> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// [`Query`] is a combination of [`Filter`](super::filter::Filter)s for read-only access.
/// For instance, `()`, `FilterA`, and `(FilterA, FilterB)` are all `Query`.
pub trait Query: 'static {
    type Output<'o>;

    fn key() -> QueryKey {
        QueryKey::new(TypeId::of::<Self>())
    }

    fn info<S: StoreQueryInfo>(info_stor: &mut S) -> Arc<QueryInfo>;
    fn convert(input: &[Filtered]) -> Self::Output<'_>;
}

/// [`QueryMut`] is a combination of [`Filter`](super::filter::Filter)s for writable access.
/// For instance, `()`, `FilterA`, and `(FilterA, FilterB)` are all `Query`.
pub trait QueryMut: 'static + Sized {
    type Output<'o>;

    fn key() -> QueryKey {
        struct QueryMutSalt;
        QueryKey::new(TypeId::of::<(Self, QueryMutSalt)>())
    }

    fn info<S: StoreQueryInfo>(info_stor: &mut S) -> Arc<QueryInfo>;
    fn convert(input: &mut [Filtered]) -> Self::Output<'_>;
}

pub trait StoreQueryInfo: StoreFilterInfo {
    fn get(&self, key: &QueryKey) -> Option<&Arc<QueryInfo>>;
    fn insert(&mut self, key: QueryKey, info: Arc<QueryInfo>);
    fn remove(&mut self, key: &QueryKey) -> Option<Arc<QueryInfo>>;
}

/// [`TypeId`] for [`Query`].
#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub struct QueryKey(TypeId);

impl QueryKey {
    pub const fn new(ty: TypeId) -> Self {
        Self(ty)
    }

    pub const fn as_type(&self) -> &TypeId {
        &self.0
    }
}

#[derive(Debug, Clone)]
pub struct QueryInfo {
    name: &'static str,
    filters: Box<[(FilterKey, Arc<FilterInfo>)]>,
}

impl QueryInfo {
    #[inline]
    pub fn name(&self) -> &'static str {
        self.name
    }

    #[inline]
    pub fn filters(&self) -> &[(FilterKey, Arc<FilterInfo>)] {
        &self.filters
    }
}

/// Implements the trait [`Query`] and [`QueryMut`] for an anonymous tuple of [`Filter`](super::filter::Filter)s.
#[macro_export]
macro_rules! impl_query {
    () => {const _: () = {
        use $crate::ecs::{
            query::{Query, QueryInfo, StoreQueryInfo},
            filter::Filtered,
        };
        use std::{any::type_name, sync::Arc};

        // Implements `Query` for ().
        impl Query for () {
            type Output<'o> = ();

            fn info<S>(info_stor: &mut S) -> Arc<QueryInfo>
            where
                S: StoreQueryInfo
            {
                let key = <Self as Query>::key();
                if let Some(info) = StoreQueryInfo::get(info_stor, &key) {
                    Arc::clone(info)
                } else {
                    let info = Arc::new(QueryInfo {
                        name: type_name::<Self>(),
                        filters: [].into(),
                    });
                    StoreQueryInfo::insert(info_stor, key, Arc::clone(&info));
                    info
                }
            }

            fn convert(input: &[Filtered]) -> Self::Output<'_> {
                debug_assert!(input.is_empty());
            }
        }

        // Implements `QueryMut` for ().
        impl QueryMut for () {
            type Output<'o> = ();

            fn info<S>(info_stor: &mut S) -> Arc<QueryInfo>
            where
                S: StoreQueryInfo
            {
                let key = <Self as QueryMut>::key();
                if let Some(info) = StoreQueryInfo::get(info_stor, &key) {
                    Arc::clone(info)
                } else {
                    let info = Arc::new(QueryInfo {
                        name: type_name::<Self>(),
                        filters: [].into(),
                    });
                    StoreQueryInfo::insert(info_stor, key, Arc::clone(&info));
                    info
                }
            }

            fn convert(input: &mut [Filtered]) -> Self::Output<'_> {
                debug_assert!(input.is_empty());
            }
        }
    };};
    ($id:ident) => {const _:() = {
        use $crate::ecs::{
            query::{Query, QueryInfo, StoreQueryInfo},
            filter::{Filter, Filtered},
        };
        use std::{any::type_name, sync::Arc};

        // Implements `Query` for A.
        impl<$id: Filter> Query for $id {
            type Output<'o> = FilteredIter<'o, <$id as Filter>::Target>;

            fn info<S>(info_stor: &mut S) -> Arc<QueryInfo>
            where
                S: StoreQueryInfo
            {
                let key = <Self as Query>::key();
                if let Some(info) = StoreQueryInfo::get(info_stor, &key) {
                    Arc::clone(info)
                } else {
                    let info = Arc::new(QueryInfo {
                        name: type_name::<Self>(),
                        filters: [
                            (<$id as Filter>::key(), <$id as Filter>::info(info_stor))
                        ].into(),
                    });
                    StoreQueryInfo::insert(info_stor, key, Arc::clone(&info));
                    info
                }
            }

            fn convert(input: &[Filtered]) -> Self::Output<'_> {
                debug_assert_eq!(1, input.len());
                FilteredIter::new(input[0].query_res(), input[0].ent_tags())
            }
        }

        // Implements `QueryMut` for A.
        impl<$id: Filter> QueryMut for $id {
            type Output<'o> = FilteredIterMut<'o, <$id as Filter>::Target>;

            fn info<S>(info_stor: &mut S) -> Arc<QueryInfo>
            where
                S: StoreQueryInfo
            {
                let key = <Self as QueryMut>::key();
                if let Some(info) = StoreQueryInfo::get(info_stor, &key) {
                    Arc::clone(info)
                } else {
                    let info = Arc::new(QueryInfo {
                        name: type_name::<Self>(),
                        filters: [
                            (<$id as Filter>::key(), <$id as Filter>::info(info_stor))
                        ].into(),
                    });
                    StoreQueryInfo::insert(info_stor, key, Arc::clone(&info));
                    info
                }
            }

            fn convert(input: &mut [Filtered]) -> Self::Output<'_> {
                debug_assert_eq!(1, input.len());
                let (etags, _, getters) = input[0].destructure();
                FilteredIterMut::new(getters, etags)
            }
        }
    };};
    ($n:expr, $($i:expr),+) => {const _: () = {
        use $crate::ecs::{
            query::{Query, QueryInfo, StoreQueryInfo},
            filter::{Filter, Filtered},
        };
        use std::{any::type_name, sync::Arc};
        use paste::paste;

        // Implements `Query` for (A0, A1, ...).
        paste! {
            impl<$([<A $i>]: Filter),+> Query for ( $([<A $i>]),+ ) {
                type Output<'o> = ( $(FilteredIter<'o, <[<A $i>] as Filter>::Target>),+ );

                fn info<S>(info_stor: &mut S) -> Arc<QueryInfo>
                where
                    S: StoreQueryInfo
                {
                    let key = <Self as Query>::key();
                    if let Some(info) = StoreQueryInfo::get(info_stor, &key) {
                        Arc::clone(info)
                    } else {
                        let info = Arc::new(QueryInfo {
                            name: type_name::<Self>(),
                            filters: [$((
                                <[<A $i>] as Filter>::key(),
                                <[<A $i>] as Filter>::info(info_stor)
                            )),+].into(),
                        });
                        StoreQueryInfo::insert(info_stor, key, Arc::clone(&info));
                        info
                    }
                }

                fn convert(input: &[Filtered]) -> Self::Output<'_> {
                    debug_assert_eq!($n, input.len());
                    ( $(
                        FilteredIter::new(input[$i].query_res(), input[$i].ent_tags())
                    ),+ )
                }
            }
        }

        // Implements `QueryMut` for (A0, A1, ...).
        paste! {
            impl<$([<A $i>]: Filter),+> QueryMut for ( $([<A $i>]),+ ) {
                type Output<'o> = ( $(FilteredIterMut<'o, <[<A $i>] as Filter>::Target>),+ );

                fn info<S>(info_stor: &mut S) -> Arc<QueryInfo>
                where
                    S: StoreQueryInfo
                {
                    let key = <Self as QueryMut>::key();
                    if let Some(info) = StoreQueryInfo::get(info_stor, &key) {
                        Arc::clone(info)
                    } else {
                        let info = Arc::new(QueryInfo {
                            name: type_name::<Self>(),
                            filters: [$((
                                <[<A $i>] as Filter>::key(),
                                <[<A $i>] as Filter>::info(info_stor)
                            )),+].into(),
                        });
                        StoreQueryInfo::insert(info_stor, key, Arc::clone(&info));
                        info
                    }
                }

                fn convert(input: &mut [Filtered]) -> Self::Output<'_> {
                    debug_assert_eq!($n, input.len());
                    ( $( {
                        let (etags, _, getters) = input[$i].destructure();
                        FilteredIterMut::new(getters, etags)
                    } ),+ )
                }
            }
        }
    };};
}
impl_query!();
impl_query!(A);
impl_query!(2, 0, 1);
impl_query!(3, 0, 1, 2);
impl_query!(4, 0, 1, 2, 3);
impl_query!(5, 0, 1, 2, 3, 4);
impl_query!(6, 0, 1, 2, 3, 4, 5);
impl_query!(7, 0, 1, 2, 3, 4, 5, 6);
impl_query!(8, 0, 1, 2, 3, 4, 5, 6, 7);

/// [`ResQuery`] is a combination of [`Resource`](super::resource::Resource)s for read-only access.
/// For instance, `()`, `ResA`, and `(ResA, ResB)` are all a sort of `ResQuery`.
pub trait ResQuery: 'static {
    type Output;

    fn key() -> ResQueryKey {
        ResQueryKey::new(TypeId::of::<Self>())
    }

    fn info() -> ResQueryInfo;
    fn convert(input: &[Borrowed<NonNull<u8>, JsAtomic>]) -> Self::Output;
}

/// [`ResQueryMut`] is a combination of [`Resource`](super::resource::Resource)s for writable access.
/// For instance, `()`, `ResA`, and `(ResA, ResB)` are all a sort of `ResQuery`.
pub trait ResQueryMut: 'static + Sized {
    type Output;

    fn key() -> ResQueryKey {
        struct ResQueryMutSalt;
        ResQueryKey::new(TypeId::of::<(Self, ResQueryMutSalt)>())
    }

    fn info() -> ResQueryInfo;
    fn convert(input: &mut [Borrowed<NonNull<u8>, JsAtomic>]) -> Self::Output;
}

/// [`TypeId`] for [`ResQuery`].
#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub struct ResQueryKey(TypeId);

impl ResQueryKey {
    pub const fn new(ty: TypeId) -> Self {
        Self(ty)
    }

    pub const fn as_type(&self) -> &TypeId {
        &self.0
    }
}

#[derive(Debug, Clone)]
pub struct ResQueryInfo {
    name: &'static str,
    rkeys: Box<[ResourceKey]>,
}

impl ResQueryInfo {
    #[inline]
    pub fn name(&self) -> &'static str {
        self.name
    }

    #[inline]
    pub fn rkeys(&self) -> &[ResourceKey] {
        &self.rkeys
    }
}

/// Implements the trait [`ResQuery`] and [`ResQueryMut`] for an anonymous tuple of [`Resource`](super::resource::Resource)s.
#[macro_export]
macro_rules! impl_res_query {
    () => {const _: () = {
        use $crate::{
            ecs::{query::{ResQuery, ResQueryInfo}, borrow_js::JsAtomic},
            ds::borrow::Borrowed,
        };
        use std::{any::type_name, ptr::NonNull};

        // Implements `ResQuery` for ().
        impl ResQuery for () {
            type Output = ();

            fn info() -> ResQueryInfo {
                ResQueryInfo {
                    name: type_name::<Self>(),
                    rkeys: [].into(),
                }
            }

            fn convert(input: &[Borrowed<NonNull<u8>, JsAtomic>]) -> Self::Output {
                debug_assert!(input.is_empty());
            }
        }

        // Implements `ResQueryMut` for ().
        impl ResQueryMut for () {
            type Output = ();

            fn info() -> ResQueryInfo {
                ResQueryInfo {
                    name: type_name::<Self>(),
                    rkeys: [].into(),
                }
            }

            fn convert(input: &mut [Borrowed<NonNull<u8>, JsAtomic>]) -> Self::Output {
                debug_assert!(input.is_empty());
            }
        }
    };};
    ($id:ident) => {const _:() = {
        use $crate::{
            ecs::{
                query::{ResQuery, ResQueryInfo},
                resource::{Resource, ResourceRef, ResourceMut},
                borrow_js::JsAtomic
            },
            ds::borrow::Borrowed,
        };
        use std::{any::type_name, ptr::NonNull};

        // Implements `ResQuery` for A.
        impl<$id: Resource> ResQuery for $id {
            type Output = ResourceRef<$id>;

            fn info() -> ResQueryInfo {
                ResQueryInfo {
                    name: type_name::<Self>(),
                    rkeys: [<$id as Resource>::key()].into(),
                }
            }

            fn convert(input: &[Borrowed<NonNull<u8>, JsAtomic>]) -> Self::Output {
                debug_assert_eq!(1, input.len());

                // Safety: Infallible.
                unsafe {
                    ResourceRef::new(
                        NonNull::new_unchecked( (
                            &input[0] as *const Borrowed<NonNull<u8>, JsAtomic>
                        ).cast_mut() )
                    )
                }
            }
        }

        // Implements `ResQueryMut` for A.
        impl<$id: Resource> ResQueryMut for $id {
            type Output = ResourceMut<$id>;

            fn info() -> ResQueryInfo {
                ResQueryInfo {
                    name: type_name::<Self>(),
                    rkeys: [<$id as Resource>::key()].into(),
                }
            }

            fn convert(input: &mut [Borrowed<NonNull<u8>, JsAtomic>]) -> Self::Output {
                debug_assert_eq!(1, input.len());

                // Safety: Infallible.
                unsafe {
                    ResourceMut::new(
                        NonNull::new_unchecked(&mut input[0] as *mut _)
                    )
                }
            }
        }
    };};
    ($n:expr, $($i:expr),+) => {const _: () = {
        use $crate::{
            ecs::{
                query::{ResQuery, ResQueryInfo},
                resource::{Resource, ResourceRef, ResourceMut},
                borrow_js::JsAtomic
            },
            ds::borrow::Borrowed,
        };
        use std::{any::type_name, ptr::NonNull};
        use paste::paste;

        // Implements `ResQuery` for (A0, A1, ...).
        paste! {
            impl<$([<A $i>]: Resource),+> ResQuery for ( $([<A $i>]),+ ) {
                type Output = ( $(ResourceRef<[<A $i>]>),+ );

                fn info() -> ResQueryInfo {
                    ResQueryInfo {
                        name: type_name::<Self>(),
                        rkeys: [$(<[<A $i>] as Resource>::key()),+].into(),
                    }
                }

                fn convert(input: &[Borrowed<NonNull<u8>, JsAtomic>]) -> Self::Output {
                    debug_assert_eq!($n, input.len());

                    // Safety: Infallible.
                    unsafe {( $(
                        ResourceRef::new(
                            NonNull::new_unchecked( (
                                &input[$i] as *const Borrowed<NonNull<u8>, JsAtomic>
                            ).cast_mut() )
                        )
                    ),+ )}
                }
            }
        }

        // Implements `ResQueryMut` for (A0, A1, ...).
        paste!{
            impl<$([<A $i>]: Resource),+> ResQueryMut for ( $([<A $i>]),+ ) {
                type Output = ( $(ResourceMut<[<A $i>]>),+ );

                fn info() -> ResQueryInfo {
                    ResQueryInfo {
                        name: type_name::<Self>(),
                        rkeys: [$(<[<A $i>] as Resource>::key()),+].into(),
                    }
                }

                fn convert(input: &mut [Borrowed<NonNull<u8>, JsAtomic>]) -> Self::Output {
                    debug_assert_eq!($n, input.len());

                    // Safety: Infallible.
                    unsafe {( $(
                        ResourceMut::new(
                            NonNull::new_unchecked(&mut input[$i] as *mut _)
                        )
                    ),+ )}
                }
            }
        }
    };};
}
impl_res_query!();
impl_res_query!(A);
impl_res_query!(2, 0, 1);
impl_res_query!(3, 0, 1, 2);
impl_res_query!(4, 0, 1, 2, 3);
impl_res_query!(5, 0, 1, 2, 3, 4);
impl_res_query!(6, 0, 1, 2, 3, 4, 5);
impl_res_query!(7, 0, 1, 2, 3, 4, 5, 6);
impl_res_query!(8, 0, 1, 2, 3, 4, 5, 6, 7);
