use super::select::{
    FilterInfo, FilterKey, FilteredRaw, SelectInfo, SelectKey, SelectedRaw, StoreSelectInfo,
};
use crate::{
    ds::{ATypeId, Borrowed, ManagedConstPtr, ManagedMutPtr, NonNullExt},
    ecs::resource::ResourceKey,
    util::TakeRecur,
};
use my_ecs_macros::repeat_macro;
use std::{
    fmt,
    ops::{Deref, DerefMut},
    sync::Arc,
};

pub(crate) trait StoreQueryInfo: StoreSelectInfo {
    fn contains(&self, key: &QueryKey) -> bool;
    fn get(&self, key: &QueryKey) -> Option<&Arc<QueryInfo>>;
    fn insert(&mut self, key: QueryKey, info: Arc<QueryInfo>);
}

pub(crate) trait StoreResQueryInfo {
    fn contains(&self, key: &ResQueryKey) -> bool;
    fn get(&self, key: &ResQueryKey) -> Option<&Arc<ResQueryInfo>>;
    fn insert(&mut self, key: ResQueryKey, info: Arc<ResQueryInfo>);
}

pub(crate) trait StoreEntQueryInfo: StoreSelectInfo {
    fn contains(&self, key: &EntQueryKey) -> bool;
    fn get(&self, key: &EntQueryKey) -> Option<&Arc<EntQueryInfo>>;
    fn insert(&mut self, key: EntQueryKey, info: Arc<EntQueryInfo>);
}

#[allow(private_interfaces, private_bounds)]
pub trait Query: 'static {
    type Output<'buf>;

    #[doc(hidden)]
    fn info_from<S>(stor: &mut S) -> QueryInfo
    where
        S: StoreQueryInfo + ?Sized;

    #[doc(hidden)]
    fn convert(buf: &mut [SelectedRaw]) -> Self::Output<'_>;

    #[doc(hidden)]
    fn key() -> QueryKey {
        QueryKey::of::<Self>()
    }

    #[doc(hidden)]
    fn get_info_from<S>(stor: &mut S) -> &Arc<QueryInfo>
    where
        S: StoreQueryInfo + ?Sized,
    {
        let key = Self::key();

        if !StoreQueryInfo::contains(stor, &key) {
            let qinfo = Arc::new(Self::info_from(stor));
            StoreQueryInfo::insert(stor, key, qinfo);
        }

        // Safety: Inserted right before.
        unsafe { StoreQueryInfo::get(stor, &key).unwrap_unchecked() }
    }
}

#[allow(private_interfaces, private_bounds)]
pub trait QueryMut: 'static {
    type Output<'buf>;

    #[doc(hidden)]
    fn info_from<S>(stor: &mut S) -> QueryInfo
    where
        S: StoreQueryInfo + ?Sized;

    #[doc(hidden)]
    fn convert(buf: &mut [SelectedRaw]) -> Self::Output<'_>;

    #[doc(hidden)]
    fn key() -> QueryKey {
        QueryKey::of::<Self>()
    }

    #[doc(hidden)]
    fn get_info_from<S>(stor: &mut S) -> &Arc<QueryInfo>
    where
        S: StoreQueryInfo + ?Sized,
    {
        let key = Self::key();

        if !StoreQueryInfo::contains(stor, &key) {
            let qinfo = Arc::new(Self::info_from(stor));
            StoreQueryInfo::insert(stor, key, qinfo);
        }

        // Safety: Inserted right before.
        unsafe { StoreQueryInfo::get(stor, &key).unwrap_unchecked() }
    }
}

#[allow(private_interfaces, private_bounds)]
pub trait ResQuery: 'static {
    type Output<'buf>;

    #[doc(hidden)]
    fn info() -> ResQueryInfo;

    #[doc(hidden)]
    fn convert(buf: &mut Vec<Borrowed<ManagedConstPtr<u8>>>) -> Self::Output<'_>;

    #[doc(hidden)]
    fn key() -> ResQueryKey {
        ResQueryKey::of::<Self>()
    }

    #[doc(hidden)]
    fn get_info_from<S>(stor: &mut S) -> &Arc<ResQueryInfo>
    where
        S: StoreResQueryInfo + ?Sized,
    {
        let key = Self::key();

        if !stor.contains(&key) {
            let rqinfo = Arc::new(Self::info());
            stor.insert(key, rqinfo);
        }

        // Safety: Inserted right before.
        unsafe { stor.get(&key).unwrap_unchecked() }
    }
}

#[allow(private_interfaces, private_bounds)]
pub trait ResQueryMut: 'static {
    type Output<'buf>;

    fn info() -> ResQueryInfo;

    fn convert(buf: &mut Vec<Borrowed<ManagedMutPtr<u8>>>) -> Self::Output<'_>;

    #[doc(hidden)]
    fn key() -> ResQueryKey {
        ResQueryKey::of::<Self>()
    }

    #[doc(hidden)]
    fn get_info_from<S>(stor: &mut S) -> &Arc<ResQueryInfo>
    where
        S: StoreResQueryInfo + ?Sized,
    {
        let key = Self::key();

        if !stor.contains(&key) {
            let rqinfo = Arc::new(Self::info());
            stor.insert(key, rqinfo);
        }

        // Safety: Inserted right before.
        unsafe { stor.get(&key).unwrap_unchecked() }
    }
}

#[allow(private_interfaces, private_bounds)]
pub trait EntQueryMut: 'static {
    type Output<'buf>;

    #[doc(hidden)]
    fn info_from<S>(stor: &mut S) -> EntQueryInfo
    where
        S: StoreEntQueryInfo + ?Sized;

    fn convert(buf: &mut [FilteredRaw]) -> Self::Output<'_>;

    #[doc(hidden)]
    fn key() -> EntQueryKey {
        EntQueryKey::of::<Self>()
    }

    #[doc(hidden)]
    fn get_info_from<S>(stor: &mut S) -> &Arc<EntQueryInfo>
    where
        S: StoreEntQueryInfo + ?Sized,
    {
        let key = Self::key();

        if !StoreEntQueryInfo::contains(stor, &key) {
            let eqinfo = Arc::new(Self::info_from(stor));
            StoreEntQueryInfo::insert(stor, key, eqinfo);
        }

        // Safety: Inserted right before.
        unsafe { StoreEntQueryInfo::get(stor, &key).unwrap_unchecked() }
    }
}

/// Read request for a set of components.
///
/// This is a part of a system request, clients need to declare the whole system
/// request. Take a look at [`request`](crate::prelude::request).
#[repr(transparent)]
pub struct Read<'buf, R: Query>(pub(crate) R::Output<'buf>);

impl<'buf, R: Query> Read<'buf, R> {
    pub fn take(self) -> R::Output<'buf> {
        self.0
    }
}

impl<'buf, R: Query> Deref for Read<'buf, R> {
    type Target = R::Output<'buf>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'buf, R: Query> TakeRecur for Read<'buf, R>
where
    R::Output<'buf>: TakeRecur,
{
    type Inner = <R::Output<'buf> as TakeRecur>::Inner;

    fn take_recur(self) -> Self::Inner {
        self.0.take_recur()
    }
}

/// Write request for a set of components.
///
/// This is a part of a system request, clients need to declare the whole system
/// request. Take a look at [`request`](crate::prelude::request).
#[repr(transparent)]
pub struct Write<'buf, W: QueryMut>(pub(crate) W::Output<'buf>);

impl<'buf, W: QueryMut> Write<'buf, W> {
    pub fn take(self) -> W::Output<'buf> {
        self.0
    }
}

impl<'buf, W: QueryMut> Deref for Write<'buf, W> {
    type Target = W::Output<'buf>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<W: QueryMut> DerefMut for Write<'_, W> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'buf, W: QueryMut> TakeRecur for Write<'buf, W>
where
    W::Output<'buf>: TakeRecur,
{
    type Inner = <W::Output<'buf> as TakeRecur>::Inner;

    fn take_recur(self) -> Self::Inner {
        self.0.take_recur()
    }
}

/// Read request for a set of resources.
///
/// This is a part of a system request, clients need to declare the whole system
/// request. Take a look at [`request`](crate::prelude::request).
#[repr(transparent)]
pub struct ResRead<'buf, RR: ResQuery>(pub(crate) RR::Output<'buf>);

impl<'buf, RR: ResQuery> ResRead<'buf, RR> {
    pub fn take(self) -> RR::Output<'buf> {
        self.0
    }
}

impl<'buf, RR: ResQuery> Deref for ResRead<'buf, RR> {
    type Target = RR::Output<'buf>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'buf, RR: ResQuery> TakeRecur for ResRead<'buf, RR>
where
    RR::Output<'buf>: TakeRecur,
{
    type Inner = <RR::Output<'buf> as TakeRecur>::Inner;

    fn take_recur(self) -> Self::Inner {
        self.0.take_recur()
    }
}

/// Write request for a set of resources.
///
/// This is a part of a system request, clients need to declare the whole system
/// request. Take a look at [`request`](crate::prelude::request).
#[repr(transparent)]
pub struct ResWrite<'buf, RW: ResQueryMut>(pub(crate) RW::Output<'buf>);

impl<'buf, RW: ResQueryMut> ResWrite<'buf, RW> {
    pub fn take(self) -> RW::Output<'buf> {
        self.0
    }
}

impl<'buf, RW: ResQueryMut> Deref for ResWrite<'buf, RW> {
    type Target = RW::Output<'buf>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<RW: ResQueryMut> DerefMut for ResWrite<'_, RW> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'buf, RW: ResQueryMut> TakeRecur for ResWrite<'buf, RW>
where
    RW::Output<'buf>: TakeRecur,
{
    type Inner = <RW::Output<'buf> as TakeRecur>::Inner;

    fn take_recur(self) -> Self::Inner {
        self.0.take_recur()
    }
}

/// Write request for a set of entity container.
///
/// This is a part of a system request, clients need to declare the whole system
/// request. Take a look at [`request`](crate::prelude::request).
#[repr(transparent)]
pub struct EntWrite<'buf, EW: EntQueryMut>(pub(crate) EW::Output<'buf>);

impl<'buf, EW: EntQueryMut> EntWrite<'buf, EW> {
    pub fn take(self) -> EW::Output<'buf> {
        self.0
    }
}

impl<'buf, EW: EntQueryMut> Deref for EntWrite<'buf, EW> {
    type Target = EW::Output<'buf>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<EW: EntQueryMut> DerefMut for EntWrite<'_, EW> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'buf, EW: EntQueryMut> TakeRecur for EntWrite<'buf, EW>
where
    EW::Output<'buf>: TakeRecur,
{
    type Inner = <EW::Output<'buf> as TakeRecur>::Inner;

    fn take_recur(self) -> Self::Inner {
        self.0.take_recur()
    }
}

/// Unique identifier for a type implementing [`Query`] or [`QueryMut`].
pub(crate) type QueryKey = ATypeId<QueryKey_>;
pub(crate) struct QueryKey_;

/// Unique identifier for a type implementing [`ResQuery`] or [`ResQueryMut`].
pub(crate) type ResQueryKey = ATypeId<ResQueryKey_>;
pub(crate) struct ResQueryKey_;

/// Unique identifier for a type implementing [`EntQueryMut`].
pub(crate) type EntQueryKey = ATypeId<EntQueryKey_>;
pub(crate) struct EntQueryKey_;

#[derive(Clone)]
pub(crate) struct QueryInfo {
    sels: Box<[(SelectKey, Arc<SelectInfo>)]>,
    name: &'static str,
}

impl fmt::Debug for QueryInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let sinfos = self.select_infos().collect::<Vec<_>>();

        f.debug_struct("QueryInfo")
            .field("name", &self.name())
            .field("sels", &sinfos)
            .finish()
    }
}

impl QueryInfo {
    const fn new(sels: Box<[(SelectKey, Arc<SelectInfo>)]>, name: &'static str) -> Self {
        Self { sels, name }
    }

    pub(crate) const fn selectors(&self) -> &[(SelectKey, Arc<SelectInfo>)] {
        &self.sels
    }

    pub(crate) fn select_infos(&self) -> impl ExactSizeIterator<Item = &Arc<SelectInfo>> + Clone {
        self.sels.iter().map(|(_key, info)| info)
    }

    pub(crate) const fn name(&self) -> &'static str {
        self.name
    }
}

#[derive(Clone)]
pub(crate) struct ResQueryInfo {
    rkeys: Box<[ResourceKey]>,
    name: &'static str,
}

impl fmt::Debug for ResQueryInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ResQueryInfo")
            .field("name", &self.name())
            .field("rkeys", &self.resource_keys())
            .finish()
    }
}

impl ResQueryInfo {
    const fn new(rkeys: Box<[ResourceKey]>, name: &'static str) -> Self {
        Self { rkeys, name }
    }

    pub(crate) const fn resource_keys(&self) -> &[ResourceKey] {
        &self.rkeys
    }

    pub(crate) const fn name(&self) -> &'static str {
        self.name
    }
}

#[derive(Clone)]
pub(crate) struct EntQueryInfo {
    filters: Box<[(FilterKey, Arc<FilterInfo>)]>,
    name: &'static str,
}

impl fmt::Debug for EntQueryInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("EntQueryInfo")
            .field("name", &self.name())
            .field("filters", &self.filters())
            .finish()
    }
}

impl EntQueryInfo {
    const fn new(filters: Box<[(FilterKey, Arc<FilterInfo>)]>, name: &'static str) -> Self {
        Self { filters, name }
    }

    pub(crate) const fn filters(&self) -> &[(FilterKey, Arc<FilterInfo>)] {
        &self.filters
    }

    pub(crate) const fn name(&self) -> &'static str {
        self.name
    }
}

/// Implements [`Query`] and [`QueryMut`] for a tuple of
/// [`Select`](crate::ecs::sys::select::Select).
macro_rules! impl_query {
    ($n:expr, $($i:expr),*) => {const _: () = {
        #[allow(unused_imports)]
        use $crate::ecs::sys::{
            query::{Query, QueryInfo, StoreQueryInfo, StoreSelectInfo},
            select::{Select, SelectKey, SelectedRaw, Selected, SelectedMut},
        };
        use std::{any::type_name, sync::Arc};
        use paste::paste;

        // Implements `Query` for (A0, A1, ...).
        paste! {
            #[allow(unused_parens)]
            #[allow(private_interfaces, private_bounds)]
            impl<$([<A $i>]: Select),*> Query for ( $([<A $i>]),* ) {
                type Output<'buf> = ( $(Selected<'buf, [<A $i>]::Target>),* );

                fn info_from<S>(_stor: &mut S) -> QueryInfo
                where
                    S: StoreQueryInfo + ?Sized
                {
                    let sels = [ $(
                        (
                            [<A $i>]::key(),
                            Arc::clone([<A $i>]::get_info_from(_stor))
                        )
                    ),* ];
                    let sels: Box<[(SelectKey, Arc<SelectInfo>)]> = sels.into();
                    let name = type_name::<Self>();
                    QueryInfo::new(sels, name)
                }

                fn convert(buf: &mut [SelectedRaw]) -> Self::Output<'_> {
                    debug_assert_eq!($n, buf.len());

                    #[allow(clippy::unused_unit)]
                    ( $(
                        // Splitting slice via `split_first_mut` is quite tiresome task.
                        // Anyway, we're connecting lifetime from each input to output,
                        // so no problem.
                        // Safety: Infallible.
                        {
                            let x: *mut SelectedRaw = &mut buf[$i] as *mut _;
                            let x: &mut SelectedRaw = unsafe { x.as_mut().unwrap_unchecked() };
                            Selected::new(x)
                        }
                    ),* )
                }
            }
        }

        // Implements `QueryMut` for (A0, A1, ...).
        paste! {
            #[allow(unused_parens)]
            #[allow(private_interfaces, private_bounds)]
            impl<$([<A $i>]: Select),*> QueryMut for ( $([<A $i>]),* ) {
                type Output<'buf> = ( $(SelectedMut<'buf, [<A $i>]::Target>),* );

                fn info_from<S>(_stor: &mut S) -> QueryInfo
                where
                    S: StoreQueryInfo + ?Sized
                {
                    let sels = [ $(
                        (
                            [<A $i>]::key(),
                            Arc::clone([<A $i>]::get_info_from(_stor))
                        )
                    ),* ];
                    let sels: Box<[(SelectKey, Arc<SelectInfo>)]> = sels.into();
                    let name = type_name::<Self>();
                    QueryInfo::new(sels, name)
                }

                fn convert(buf: &mut [SelectedRaw]) -> Self::Output<'_> {
                    debug_assert_eq!($n, buf.len());

                    #[allow(clippy::unused_unit)]
                    ( $(
                        // Splitting slice via `split_first_mut` is quite tiresome task.
                        // Anyway, we're connecting lifetime from each input to output,
                        // so no problem.
                        // Safety: Infallible.
                        {
                            let x: *mut SelectedRaw = &mut buf[$i] as *mut _;
                            let x: &mut SelectedRaw = unsafe { x.as_mut().unwrap_unchecked() };
                            SelectedMut::new(x)
                        }
                    ),* )
                }
            }
        }
    };};
}
repeat_macro!(impl_query, ..=8);

/// Implements the trait [`ResQuery`] and [`ResQueryMut`] for an anonymous tuple of [`Resource`](super::resource::Resource)s.
macro_rules! impl_res_query {
    ($n:expr, $($i:expr),*) => {const _: () = {
        #[allow(unused_imports)]
        use $crate::{
            ecs::{
                sys::query::{ResQuery, ResQueryInfo},
                resource::Resource,
            },
            ds::{Borrowed, ManagedConstPtr, ManagedMutPtr, TypeIdExt},
        };
        use std::any::type_name;
        use paste::paste;

        // Implements `ResQuery` for (A0, A1, ...).
        paste! {
            #[allow(unused_parens)]
            #[allow(private_interfaces, private_bounds)]
            impl<$([<A $i>]: Resource),*> ResQuery for ( $([<A $i>]),* ) {
                type Output<'buf> = ( $( &'buf [<A $i>] ),* );

                fn info() -> ResQueryInfo {
                    let rkeys = [$([<A $i>]::key()),*].into();
                    let name = type_name::<Self>();
                    ResQueryInfo::new(rkeys, name)
                }

                fn convert(_buf: &mut Vec<Borrowed<ManagedConstPtr<u8>>>) -> Self::Output<'_> {
                    debug_assert_eq!($n, _buf.len());

                    // Managed pointer may have type info in it.
                    // But resource pointer must have the type info.
                    // So we can check if the pointers are correctly given.
                    #[cfg(feature = "check")]
                    {
                        $(
                            let ptr: NonNullExt<_> = _buf[$i].as_nonnullext();
                            let lhs: &TypeIdExt = ptr.get_type().unwrap();

                            let rkey: ResourceKey = [<A $i>]::key();
                            let rhs: &TypeIdExt = rkey.get_inner();

                            assert_eq!(lhs, rhs);
                        )*
                    }

                    #[allow(clippy::unused_unit)]
                    ( $( {
                        let ptr: NonNullExt<[<A $i>]> = _buf[$i].as_nonnullext().cast();
                        // Safety: Infallible, Plus output is bounded by input's
                        // 'buf lifetime
                        unsafe { ptr.as_ref() } // &'buf mut A#
                    } ),* )
                }
            }
        }

        // Implements `ResQueryMut` for (A0, A1, ...).
        paste!{
            #[allow(unused_parens)]
            #[allow(private_interfaces, private_bounds)]
            impl<$([<A $i>]: Resource),*> ResQueryMut for ( $([<A $i>]),* ) {
                type Output<'buf> = ( $( &'buf mut [<A $i>] ),* );

                fn info() -> ResQueryInfo {
                    let rkeys = [$([<A $i>]::key()),*].into();
                    let name = type_name::<Self>();
                    ResQueryInfo::new(rkeys, name)
                }

                fn convert(_buf: &mut Vec<Borrowed<ManagedMutPtr<u8>>>) -> Self::Output<'_> {
                    debug_assert_eq!($n, _buf.len());

                    // Managed pointer may have type info in it.
                    // But resource pointer must have the type info.
                    // So we can check if the pointers are correctly given.
                    #[cfg(feature = "check")]
                    { $(
                        let ptr: NonNullExt<_> = _buf[$i].as_nonnullext();
                        let lhs: &TypeIdExt = ptr.get_type().unwrap();

                        let rkey: ResourceKey = [<A $i>]::key();
                        let rhs: &TypeIdExt = rkey.get_inner();

                        assert_eq!(lhs, rhs);
                    )* }

                    #[allow(clippy::unused_unit)]
                    ( $( {
                        let mut ptr: NonNullExt<[<A $i>]> = _buf[$i].as_nonnullext().cast();
                        // Safety: Infallible, Plus output is bounded by input's
                        // 'buf lifetime
                        unsafe { ptr.as_mut() } // &'buf mut A#
                    } ),* )
                }
            }
        }
    };};
}
repeat_macro!(impl_res_query, ..=8);

/// Implements the trait [`EntQueryMut`] for an anonymous tuple of [`ContainEntity`](super::entity::ContainEntity)s.
macro_rules! impl_ent_query {
    ($n:expr, $($i:expr),*) => {const _: () = {
        #[allow(unused_imports)]
        use $crate::{
            ecs::{
                sys::{
                    query::{EntQueryMut, EntQueryInfo},
                    select::{Filter, FilteredMut},
                },
                ent::{
                    entity::{Entity, ContainEntity},
                    storage::EntityContainerRef
                },
            },
            ds::Borrowed,
        };
        use std::any::type_name;
        use paste::paste;

        // Implements `EntQueryMut` for (A0, A1, ...).
        paste! {
            #[allow(unused_parens)]
            #[allow(private_interfaces, private_bounds)]
            impl<$([<A $i>]: Filter),*> EntQueryMut for ( $([<A $i>]),* ) {
                type Output<'buf> = ( $(FilteredMut<'buf, [<A $i>]>),* );

                fn info_from<S>(_stor: &mut S) -> EntQueryInfo
                where
                    S: StoreEntQueryInfo + ?Sized
                {
                    let finfos = [$(
                        (
                            [<A $i>]::key(),
                            Arc::clone([<A $i>]::get_info_from(_stor))
                        )
                    ),*].into();
                    let name = type_name::<Self>();
                    EntQueryInfo::new(finfos, name)
                }

                fn convert(buf: &mut [FilteredRaw]) -> Self::Output<'_> {
                    debug_assert_eq!($n, buf.len());

                    #[allow(clippy::unused_unit)]
                    ( $(
                        // Splitting slice via `split_first_mut` is quite tiresome task.
                        // Anyway, we're connecting lifetime from each input to output,
                        // so no problem.
                        // Safety: Infallible.
                        {
                            let x: *mut FilteredRaw = &mut buf[$i] as *mut _;
                            let x: &mut FilteredRaw = unsafe { x.as_mut().unwrap_unchecked() };
                            FilteredMut::new(x)
                        }
                    ),* )
                }
            }
        }
    };};
}
repeat_macro!(impl_ent_query, ..=8);
