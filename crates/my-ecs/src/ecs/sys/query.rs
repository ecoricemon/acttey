use super::filter::{FilterInfo, FilterKey, Filtered, StoreFilterInfo};
use crate::ds::prelude::*;
use crate::ecs::{
    ent::entity::{ContainEntity, EntityKey},
    resource::ResourceKey,
};
use std::{
    ops::{Deref, DerefMut},
    ptr::NonNull,
    rc::Rc,
    sync::atomic::AtomicI32,
};

/// A shallow wrapper structure for the [`Query::Output`].
#[repr(transparent)]
pub struct Read<R: Query>(pub(crate) <R as Query>::Output);

impl<R: Query> Read<R> {
    pub fn take(self) -> <R as Query>::Output {
        self.0
    }
}

impl<R: Query> Deref for Read<R> {
    type Target = <R as Query>::Output;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// A shallow wrapper structure for the [`QueryMut::Output`].
#[repr(transparent)]
pub struct Write<W: QueryMut>(pub(crate) <W as QueryMut>::Output);

impl<W: QueryMut> Write<W> {
    pub fn take(self) -> <W as QueryMut>::Output {
        self.0
    }
}

impl<W: QueryMut> Deref for Write<W> {
    type Target = <W as QueryMut>::Output;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<W: QueryMut> DerefMut for Write<W> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// A shallow wrapper structure for the [`ResQuery::Output`].
#[repr(transparent)]
pub struct ResRead<RR: ResQuery>(pub(crate) <RR as ResQuery>::Output);

impl<RR: ResQuery> ResRead<RR> {
    pub fn take(self) -> <RR as ResQuery>::Output {
        self.0
    }
}

impl<RR: ResQuery> Deref for ResRead<RR> {
    type Target = <RR as ResQuery>::Output;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// A shallow wrapper structure for the [`ResQueryMut::Output`].
#[repr(transparent)]
pub struct ResWrite<RW: ResQueryMut>(pub(crate) <RW as ResQueryMut>::Output);

impl<RW: ResQueryMut> ResWrite<RW> {
    pub fn take(self) -> <RW as ResQueryMut>::Output {
        self.0
    }
}

impl<RW: ResQueryMut> Deref for ResWrite<RW> {
    type Target = <RW as ResQueryMut>::Output;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<RW: ResQueryMut> DerefMut for ResWrite<RW> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// A shallow wrapper structure for the [`EntQueryMut::Output`].
#[repr(transparent)]
pub struct EntWrite<EW: EntQueryMut>(pub(crate) <EW as EntQueryMut>::Output);

impl<EW: EntQueryMut> EntWrite<EW> {
    pub fn take(self) -> <EW as EntQueryMut>::Output {
        self.0
    }
}

impl<EW: EntQueryMut> Deref for EntWrite<EW> {
    type Target = <EW as EntQueryMut>::Output;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<EW: EntQueryMut> DerefMut for EntWrite<EW> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// [`TypeId`] of a [`Query`] or [`QueryMut`].
pub type QueryKey = ATypeId<QueryKey_>;
pub struct QueryKey_;

/// [`TypeId`] of a [`ResQuery`] or [`ResQueryMut`].
pub type ResQueryKey = ATypeId<ResQueryKey_>;
pub struct ResQueryKey_;

/// [`TypeId`] of a [`EntQueryMut`].
pub type EntQueryKey = ATypeId<EntQueryKey_>;
pub struct EntQueryKey_;

#[derive(Debug, Clone)]
pub struct QueryInfo {
    name: &'static str,
    filters: Box<[(FilterKey, Rc<FilterInfo>)]>,
}

impl QueryInfo {
    pub fn name(&self) -> &'static str {
        self.name
    }

    pub fn filters(&self) -> &[(FilterKey, Rc<FilterInfo>)] {
        &self.filters
    }
}

#[derive(Debug, Clone)]
pub struct ResQueryInfo {
    name: &'static str,
    rkeys: Box<[ResourceKey]>,
}

impl ResQueryInfo {
    pub fn name(&self) -> &'static str {
        self.name
    }

    pub fn rkeys(&self) -> &[ResourceKey] {
        &self.rkeys
    }
}

#[derive(Debug, Clone)]
pub struct EntQueryInfo {
    pub name: &'static str,
    pub ekeys: Box<[EntityKey]>,
}

impl EntQueryInfo {
    pub fn name(&self) -> &'static str {
        self.name
    }

    pub fn ekeys(&self) -> &[EntityKey] {
        &self.ekeys
    }
}

pub trait StoreQueryInfo: StoreFilterInfo {
    fn get(&self, key: &QueryKey) -> Option<Rc<QueryInfo>>;
    fn insert(&mut self, key: QueryKey, info: &Rc<QueryInfo>);
}

pub trait StoreResQueryInfo {
    fn get(&self, key: &ResQueryKey) -> Option<Rc<ResQueryInfo>>;
    fn insert(&mut self, key: ResQueryKey, info: &Rc<ResQueryInfo>);
}

pub trait StoreEntQueryInfo {
    fn get(&self, key: &EntQueryKey) -> Option<Rc<EntQueryInfo>>;
    fn insert(&mut self, key: EntQueryKey, info: &Rc<EntQueryInfo>);
}

/// [`Query`] is a combination of [`Filter`](super::filter::Filter)s for read-only access.
/// For instance, `()`, `FilterA`, and `(FilterA, FilterB)` are sorts `Query`.
pub trait Query: 'static {
    type Output;

    /// Provided.
    fn key() -> QueryKey {
        QueryKey::of::<Self>()
    }

    /// Required.
    fn get_info<S: StoreQueryInfo>(stor: &mut S) -> Rc<QueryInfo>;

    /// Required.
    fn convert(input: &[Filtered]) -> Self::Output;
}

/// [`QueryMut`] is a combination of [`Filter`](super::filter::Filter)s for writable access.
/// For instance, `()`, `FilterA`, and `(FilterA, FilterB)` are sorts of `Query`.
pub trait QueryMut: 'static + Sized {
    type Output;

    /// Provided.
    fn key() -> QueryKey {
        struct QueryMutSalt;
        QueryKey::of::<(Self, QueryMutSalt)>()
    }

    /// Required.
    fn get_info<S: StoreQueryInfo>(stor: &mut S) -> Rc<QueryInfo>;

    /// Required.
    fn convert(input: &mut [Filtered]) -> Self::Output;
}

/// [`ResQuery`] is a combination of [`Resource`](super::resource::Resource)s for read-only access.
/// For instance, `()`, `ResA`, and `(ResA, ResB)` are sorts of `ResQuery`.
pub trait ResQuery: 'static {
    type Output;

    /// Provided.
    fn key() -> ResQueryKey {
        ResQueryKey::of::<Self>()
    }

    /// Provided.
    fn get_info<S: StoreResQueryInfo>(stor: &mut S) -> Rc<ResQueryInfo> {
        let key = <Self as ResQuery>::key();
        if let Some(info) = StoreResQueryInfo::get(stor, &key) {
            info
        } else {
            let info = Rc::new(Self::info());
            StoreResQueryInfo::insert(stor, key, &info);
            info
        }
    }

    /// Required.
    fn info() -> ResQueryInfo;

    /// Required.
    fn convert(input: &mut Vec<Borrowed<ManagedConstPtr<u8>, AtomicI32>>) -> Self::Output;
}

/// [`ResQueryMut`] is a combination of [`Resource`](super::resource::Resource)s for writable access.
/// For instance, `()`, `ResA`, and `(ResA, ResB)` are sorts of `ResQueryMut`.
pub trait ResQueryMut: 'static + Sized {
    type Output;

    /// Provided.
    fn key() -> ResQueryKey {
        struct ResQueryMutSalt;
        ResQueryKey::of::<(Self, ResQueryMutSalt)>()
    }

    /// Provided.
    fn get_info<S: StoreResQueryInfo>(stor: &mut S) -> Rc<ResQueryInfo> {
        let key = <Self as ResQueryMut>::key();
        if let Some(info) = StoreResQueryInfo::get(stor, &key) {
            info
        } else {
            let info = Rc::new(Self::info());
            StoreResQueryInfo::insert(stor, key, &info);
            info
        }
    }

    /// Required.
    fn info() -> ResQueryInfo;

    /// Required.
    fn convert(input: &mut Vec<Borrowed<ManagedMutPtr<u8>, AtomicI32>>) -> Self::Output;
}

/// [`EntQueryMut`] is a combination of [`ContainEntity`](super::entity::ContainEntity)s for writable access.
/// For instance, `()`, `EntA`, and `(EntA, EntB)` are sorts of `EntQueryMut`.
pub trait EntQueryMut: 'static {
    type Output;

    /// Provided.
    fn key() -> EntQueryKey {
        EntQueryKey::of::<Self>()
    }

    /// Provided.
    fn get_info<S: StoreEntQueryInfo>(stor: &mut S) -> Rc<EntQueryInfo> {
        let key = <Self as EntQueryMut>::key();
        if let Some(info) = StoreEntQueryInfo::get(stor, &key) {
            info
        } else {
            let info = Rc::new(Self::info());
            StoreEntQueryInfo::insert(stor, key, &info);
            info
        }
    }

    /// Required.
    fn info() -> EntQueryInfo;

    /// Required.
    fn convert(input: &mut Vec<Borrowed<NonNull<dyn ContainEntity>, AtomicI32>>) -> Self::Output;
}

/// Implements the trait [`Query`] and [`QueryMut`] for an anonymous tuple of [`Filter`](super::filter::Filter)s.
#[macro_export]
macro_rules! impl_query {
    ($n:expr, $($i:expr),*) => {const _: () = {
        #[allow(unused_imports)]
        use $crate::ecs::sys::{
            query::{Query, QueryInfo, StoreQueryInfo},
            filter::{Filter, Filtered, RawFiltered, RawFilteredMut},
        };
        use std::{any::type_name, rc::Rc};
        use paste::paste;

        // Implements `Query` for (A0, A1, ...).
        paste! {
            #[allow(unused_parens)]
            impl<$([<A $i>]: Filter),*> Query for ( $([<A $i>]),* ) {
                type Output = ( $(RawFiltered<<[<A $i>] as Filter>::Target>),* );

                fn get_info<S>(stor: &mut S) -> Rc<QueryInfo>
                where
                    S: StoreQueryInfo
                {
                    let key = <Self as Query>::key();
                    if let Some(info) = StoreQueryInfo::get(stor, &key) {
                        info
                    } else {
                        let info = Rc::new(QueryInfo {
                            name: type_name::<Self>(),
                            filters: [$((
                                <[<A $i>] as Filter>::key(),
                                <[<A $i>] as Filter>::info(stor)
                            )),*].into(),
                        });
                        StoreQueryInfo::insert(stor, key, &info);
                        info
                    }
                }

                fn convert(input: &[Filtered]) -> Self::Output {
                    debug_assert_eq!($n, input.len());
                    #[allow(clippy::unused_unit)]
                    ( $(
                        // Safety: Scheduler guarantees that input are valid
                        // and not aliased mutably until the iterator is dropped.
                        unsafe {
                            RawFiltered::new(input[$i].query_res(), input[$i].entity_tags())
                        }
                    ),* )
                }
            }
        }

        // Implements `QueryMut` for (A0, A1, ...).
        paste! {
            #[allow(unused_parens)]
            impl<$([<A $i>]: Filter),*> QueryMut for ( $([<A $i>]),* ) {
                type Output = ( $(RawFilteredMut<<[<A $i>] as Filter>::Target>),* );

                fn get_info<S>(stor: &mut S) -> Rc<QueryInfo>
                where
                    S: StoreQueryInfo
                {
                    let key = <Self as QueryMut>::key();
                    if let Some(info) = StoreQueryInfo::get(stor, &key) {
                        info
                    } else {
                        let info = Rc::new(QueryInfo {
                            name: type_name::<Self>(),
                            filters: [$((
                                <[<A $i>] as Filter>::key(),
                                <[<A $i>] as Filter>::info(stor)
                            )),*].into(),
                        });
                        StoreQueryInfo::insert(stor, key, &info);
                        info
                    }
                }

                fn convert(input: &mut [Filtered]) -> Self::Output {
                    debug_assert_eq!($n, input.len());
                    #[allow(clippy::unused_unit)]
                    ( $( {
                        let (etags, _, getters) = input[$i].take();
                        // Safety: Scheduler guarantees that input are valid
                        // and not aliased until the iterator is dropped.
                        unsafe {
                            RawFilteredMut::new(getters, etags)
                        }
                    } ),* )
                }
            }
        }
    };};
}
impl_query!(0,);
impl_query!(1, 0);
impl_query!(2, 0, 1);
impl_query!(3, 0, 1, 2);
impl_query!(4, 0, 1, 2, 3);
impl_query!(5, 0, 1, 2, 3, 4);
impl_query!(6, 0, 1, 2, 3, 4, 5);
impl_query!(7, 0, 1, 2, 3, 4, 5, 6);
impl_query!(8, 0, 1, 2, 3, 4, 5, 6, 7);

/// Implements the trait [`ResQuery`] and [`ResQueryMut`] for an anonymous tuple of [`Resource`](super::resource::Resource)s.
#[macro_export]
macro_rules! impl_res_query {
    ($n:expr, $($i:expr),*) => {const _: () = {
        #[allow(unused_imports)]
        use $crate::{
            ecs::{
                sys::query::{ResQuery, ResQueryInfo},
                resource::Resource,
            },
            ds::{borrow::Borrowed, ptr::{ManagedConstPtr, ManagedMutPtr}},
        };
        use std::{any::type_name, sync::atomic::AtomicI32};
        use paste::paste;

        // Implements `ResQuery` for (A0, A1, ...).
        paste! {
            #[allow(unused_parens)]
            impl<$([<A $i>]: Resource),*> ResQuery for ( $([<A $i>]),* ) {
                type Output = ( $(Borrowed<ManagedConstPtr<[<A $i>]>, AtomicI32>),* );

                fn info() -> ResQueryInfo {
                    ResQueryInfo {
                        name: type_name::<Self>(),
                        rkeys: [$(<[<A $i>] as Resource>::key()),*].into(),
                    }
                }

                fn convert(input: &mut Vec<Borrowed<ManagedConstPtr<u8>, AtomicI32>>) -> Self::Output {
                    #[cfg(debug_assertions)]
                    {
                        // Input length must be the same as output length.
                        assert_eq!($n, input.len());

                        // In debug mode, managed pointer may have type info in it.
                        // But resource pointer must have the type info.
                        // So we can check if the pointers are correctly given.
                        $(
                            let ptr: &NonNullExt<_> = input[$i].get_inner();
                            let lhs: &TypeIdExt = ptr.get_type().unwrap();

                            let rkey: ResourceKey = [<A $i>]::key();
                            let ty: ATypeId<_> = *rkey.get_inner();
                            let rhs: &TypeIdExt = ty.get_inner();

                            assert_eq!(lhs, rhs);
                        )*
                    }

                    let res = ( $(
                        // Safety: input[$i] won't be useed(dropped) any longer.
                        unsafe {
                            input[$i].map_copy(|ptr| ptr.cast::<[<A $i>]>())
                        }
                    ),* );

                    // Safety: All items in `input` moved into `res` by `map_copy()`
                    // Therefore we need to forget about them.
                    unsafe { input.set_len(0); }

                    res
                }
            }
        }

        // Implements `ResQueryMut` for (A0, A1, ...).
        paste!{
            #[allow(unused_parens)]
            impl<$([<A $i>]: Resource),*> ResQueryMut for ( $([<A $i>]),* ) {
                type Output = ( $(Borrowed<ManagedMutPtr<[<A $i>]>, AtomicI32>),* );

                fn info() -> ResQueryInfo {
                    ResQueryInfo {
                        name: type_name::<Self>(),
                        rkeys: [$(<[<A $i>] as Resource>::key()),*].into(),
                    }
                }

                fn convert(input: &mut Vec<Borrowed<ManagedMutPtr<u8>, AtomicI32>>) -> Self::Output {
                    #[cfg(debug_assertions)]
                    {
                        // Input length must be the same as output length.
                        assert_eq!($n, input.len());

                        // In debug mode, managed pointer may have type info in it.
                        // But resource pointer must have the type info.
                        // So we can check if the pointers are correctly given.
                        $(
                            let ptr: &NonNullExt<_> = input[$i].get_inner();
                            let lhs: &TypeIdExt = ptr.get_type().unwrap();

                            let rkey: ResourceKey = [<A $i>]::key();
                            let ty: ATypeId<_> = *rkey.get_inner();
                            let rhs: &TypeIdExt = ty.get_inner();

                            assert_eq!(lhs, rhs);
                        )*
                    }

                    let res = ( $(
                        // Safety: input[$i] won't be useed(dropped) any longer.
                        unsafe {
                            input[$i].map_copy(|ptr| ptr.cast::<[<A $i>]>() )
                        }
                    ),* );

                    // Safety: All items in `input` moved into `res` by `map_copy()`
                    // Therefore we need to forget about them.
                    unsafe { input.set_len(0); }

                    res
                }
            }
        }
    };};
}
impl_res_query!(0,);
impl_res_query!(1, 0);
impl_res_query!(2, 0, 1);
impl_res_query!(3, 0, 1, 2);
impl_res_query!(4, 0, 1, 2, 3);
impl_res_query!(5, 0, 1, 2, 3, 4);
impl_res_query!(6, 0, 1, 2, 3, 4, 5);
impl_res_query!(7, 0, 1, 2, 3, 4, 5, 6);
impl_res_query!(8, 0, 1, 2, 3, 4, 5, 6, 7);

/// Implements the trait [`EntQueryMut`] for an anonymous tuple of [`ContainEntity`](super::entity::ContainEntity)s.
#[macro_export]
macro_rules! impl_ent_query {
    ($n:expr, $($i:expr),*) => {const _: () = {
        #[allow(unused_imports)]
        use $crate::{
            ecs::{
                sys::query::{EntQueryMut, EntQueryInfo},
                ent::{
                    entity::{Entity, ContainEntity},
                    storage::TypedEntityContainer},
            },
            ds::borrow::Borrowed,
        };
        use std::{any::type_name, sync::atomic::AtomicI32};
        use paste::paste;

        // Implements `EntQueryMut` for (A0, A1, ...).
        paste!{
            #[allow(unused_parens)]
            impl<$([<A $i>]: Entity),*> EntQueryMut for ( $([<A $i>]),* ) {
                type Output = ( $(TypedEntityContainer<[<A $i>]>),* );

                fn info() -> EntQueryInfo {
                    EntQueryInfo {
                        name: type_name::<Self>(),
                        ekeys: [$(<[<A $i>] as Entity>::key()),*].into(),
                    }
                }

                fn convert(input: &mut Vec<Borrowed<NonNull<dyn ContainEntity>, AtomicI32>>) -> Self::Output {
                    debug_assert_eq!($n, input.len());

                    // clippy warns about possibility that
                    // users of the macro could write unsafe code without
                    // usafe block due to '$i' which is given by users and is in unsafe block.
                    // But 'input[$i]' is safe code.
                    #[allow(clippy::macro_metavars_in_unsafe)]
                    let res = ( $(
                        unsafe { TypedEntityContainer::new_copy(&input[$i]) }
                    ),* );

                    // Safety: All items in `input` moved into `res` by `new_copy()`.
                    // Therefore we need to forget about them.
                    unsafe { input.set_len(0); }

                    res
                }
            }
        }
    };};
}
impl_ent_query!(0,);
impl_ent_query!(1, 0);
impl_ent_query!(2, 0, 1);
impl_ent_query!(3, 0, 1, 2);
impl_ent_query!(4, 0, 1, 2, 3);
impl_ent_query!(5, 0, 1, 2, 3, 4);
impl_ent_query!(6, 0, 1, 2, 3, 4, 5);
impl_ent_query!(7, 0, 1, 2, 3, 4, 5, 6);
impl_ent_query!(8, 0, 1, 2, 3, 4, 5, 6, 7);
