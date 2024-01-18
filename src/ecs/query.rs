use crate::{
    ecs::{
        fkey, predefined::resource::ResourcePack, qkey, storage::Store, traits::Component,
        FilteResKey, QueryKey, SystemKey,
    },
    ty,
    util::{downcast_mut_slice, downcast_slice},
};
use std::{
    any::TypeId,
    marker::PhantomData,
    ptr::NonNull,
    slice::{Iter, IterMut},
};

/// A filter to select slices of `Component`.
/// You should fill out this form of filter.
///
/// - `Target` is what `Component` you want. You will receive slices of this `Target`.
/// - `FilterAll` is a tuple of `Component`s to select entities that have all these `Component`s. Empty tuple means selecting all entities.
/// - `FilterAny` is a tuple of `Component`s to select entities that have any of these `Component`s. `Target` automatically belongs to this, so that empty tuple means selecting entities including `Target`.
/// - `FilterNone` is a tuple of `Component`s not to select entities that have any of these `Component`s. Empty tuple means selecting all entities.
pub trait Filter: 'static {
    type Target: Component;
    type All: Identify;
    type Any: Identify;
    type None: Identify;

    #[inline]
    fn ids() -> [Vec<TypeId>; 3] {
        [
            <Self::All as Identify>::ids(),
            <Self::Any as Identify>::ids(),
            <Self::None as Identify>::ids(),
        ]
    }

    #[inline]
    fn info(qkey: QueryKey) -> FilterInfo {
        let [all, any, none] = Self::ids();
        FilterInfo {
            fkey: fkey!(Self, qkey),
            target: ty!(Self::Target),
            all,
            any,
            none,
        }
    }
}

pub struct FilterInfo {
    pub fkey: FilteResKey,
    pub target: TypeId,
    pub all: Vec<TypeId>,
    pub any: Vec<TypeId>,
    pub none: Vec<TypeId>,
}

impl FilterInfo {
    #[allow(dead_code)]
    #[inline]
    fn new<T: Filter>(qkey: QueryKey) -> Self {
        T::info(qkey)
    }
}

/// A trait to get `TypeId`s of elements inside a tuple.
pub trait Identify {
    type Output;

    #[deprecated]
    fn _ids() -> Self::Output;

    #[deprecated]
    fn as_slice(ids: &Self::Output) -> &[TypeId];

    fn ids() -> Vec<TypeId>;
}

pub struct QueryMutTypeIdSalt;

pub trait Query<'a>: 'static {
    type Output;

    #[inline]
    fn gen_key(skey: SystemKey) -> QueryKey {
        qkey!(Self, skey)
    }

    fn query(storage: &mut impl Store, skey: SystemKey) -> Self::Output;
    fn fkeys(skey: SystemKey) -> Vec<FilteResKey>;
    fn info(skey: SystemKey) -> QueryInfo;
}

pub trait QueryMut<'a>: 'static {
    type Output;

    #[inline]
    fn gen_key_mut(skey: SystemKey) -> QueryKey {
        qkey!((QueryMutTypeIdSalt, Self), skey)
    }

    fn query_mut(storage: &mut impl Store, skey: SystemKey) -> Self::Output;
    fn fkeys_mut(skey: SystemKey) -> Vec<FilteResKey>;
    fn info_mut(skey: SystemKey) -> QueryInfo;
}

pub struct QueryInfo {
    pub finfo: Vec<FilterInfo>,
}

impl QueryInfo {
    #[allow(dead_code)]
    #[inline]
    pub fn new<T: for<'a> Query<'a>>(skey: SystemKey) -> Self {
        T::info(skey)
    }
}

pub trait ResQuery<'a> {
    type Output;

    fn query(res_pack: &'a ResourcePack) -> Self::Output;
}

pub trait ResQueryMut<'a> {
    type Output;

    fn query_mut(res_pack: &'a ResourcePack) -> Self::Output;
}

pub struct QueryIter<'a, T> {
    iter: Iter<'a, NonNull<[()]>>,
    _marker: PhantomData<T>,
}

impl<'a, T> QueryIter<'a, T> {
    /// # Safety
    ///
    /// Borrow check breaks here.
    /// Caller should guarantee that `v` is invariant during its usage.
    /// Plus, generic parameter `T` should match with the original type of the `v`.
    #[inline]
    pub unsafe fn new(v: &Vec<NonNull<[()]>>) -> Self {
        Self {
            iter: (*(v as *const Vec<NonNull<[()]>>)).iter(),
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn empty() -> Self {
        Self {
            iter: Default::default(),
            _marker: PhantomData,
        }
    }
}

impl<'a, T: 'a> Iterator for QueryIter<'a, T> {
    type Item = &'a [T];

    // Safety: Downcasting will be guaranteed by the caller(See comment at the constructor).
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|ptr| unsafe { downcast_slice(ptr.as_ptr()) })
    }
}

pub struct QueryIterMut<'a, T> {
    iter: IterMut<'a, NonNull<[()]>>,
    _marker: PhantomData<T>,
}

impl<'a, T> QueryIterMut<'a, T> {
    /// # Safety
    ///
    /// Borrow check breaks here.
    /// Caller should guarantee that `v` is invariant during its usage.
    /// Plus, generic parameter `T` should match with the original type of the `v`.
    #[inline]
    pub unsafe fn new(v: &mut Vec<NonNull<[()]>>) -> Self {
        Self {
            iter: (*(v as *mut Vec<NonNull<[()]>>)).iter_mut(),
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn empty() -> Self {
        Self {
            iter: Default::default(),
            _marker: PhantomData,
        }
    }
}

impl<'a, T: 'a> Iterator for QueryIterMut<'a, T> {
    type Item = &'a mut [T];

    // Safety: Downcasting will be guaranteed by the caller(See comment at the constructor).
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|ptr| unsafe { downcast_mut_slice(ptr.as_ptr()) })
    }
}

#[macro_export]
macro_rules! impl_identify {
    (0) => {
        impl $crate::acttey::ecs::query::Identify for () {
            type Output = [std::any::TypeId; 0];

            #[inline]
            fn _ids() -> Self::Output {
                []
            }

            #[inline]
            fn as_slice(ids: &Self::Output) -> &[std::any::TypeId] {
                ids
            }

            #[inline]
            fn ids() -> Vec<std::any::TypeId> {
                vec![]
            }
        }
    };
    (1, $id:ident) => {
        impl<$id: $crate::acttey::ecs::traits::Component>
            $crate::acttey::ecs::query::Identify for $id
        {
            type Output = [std::any::TypeId; 1];

            #[inline]
            fn _ids() -> Self::Output {
                [std::any::TypeId::of::<$id>()]
            }

            #[inline]
            fn as_slice(ids: &Self::Output) -> &[std::any::TypeId] {
                ids
            }

            #[inline]
            fn ids() -> Vec<std::any::TypeId> {
                vec![$crate::ty!($id)]
            }
        }
    };
    ($n:expr, $($id:ident),+) => {
        impl<$($id: $crate::acttey::ecs::traits::Component),+>
            $crate::acttey::ecs::query::Identify for ( $($id),+ )
        {
            type Output = [std::any::TypeId; $n];

            #[inline]
            fn _ids() -> Self::Output {
                [$(std::any::TypeId::of::<$id>()),+]
            }

            #[inline]
            fn as_slice(ids: &Self::Output) -> &[std::any::TypeId] {
                ids
            }

            #[inline]
            fn ids() -> Vec<std::any::TypeId> {
                vec![$( $crate::ty!($id) ),+]
            }
        }
    }
}

// impl `Identify` for tuple combinations.
impl_identify!(0);
impl_identify!(1, A);
impl_identify!(2, A, B);
impl_identify!(3, A, B, C);
impl_identify!(4, A, B, C, D);
impl_identify!(5, A, B, C, D, E);
impl_identify!(6, A, B, C, D, E, F);
impl_identify!(7, A, B, C, D, E, F, G);
impl_identify!(8, A, B, C, D, E, F, G, H);
impl_identify!(9, A, B, C, D, E, F, G, H, I);
impl_identify!(10, A, B, C, D, E, F, G, H, I, J);
impl_identify!(11, A, B, C, D, E, F, G, H, I, J, K);
impl_identify!(12, A, B, C, D, E, F, G, H, I, J, K, L);
impl_identify!(13, A, B, C, D, E, F, G, H, I, J, K, L, M);
impl_identify!(14, A, B, C, D, E, F, G, H, I, J, K, L, M, N);
impl_identify!(15, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O);
impl_identify!(16, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P);

#[macro_export]
macro_rules! impl_query {
    (0) => {
        impl<'a> $crate::acttey::ecs::query::Query<'a> for ()
        {
            type Output = $crate::acttey::ecs::query::QueryIter<'a, ()>;

            #[inline]
            fn query(
                _storage: &mut impl $crate::acttey::ecs::storage::Store,
                _skey: $crate::acttey::ecs::SystemKey,
            ) -> Self::Output {
                QueryIter::empty()
            }

            #[inline]
            fn fkeys(_skey: $crate::acttey::ecs::SystemKey)
                -> Vec<$crate::acttey::ecs::FilteResKey>
            {
                vec![]
            }

            #[inline]
            fn info(_skey: $crate::acttey::ecs::SystemKey) -> $crate::acttey::ecs::query::QueryInfo {
                $crate::acttey::ecs::query::QueryInfo {
                    finfo: vec![]
                }
            }
        }
        impl<'a> $crate::acttey::ecs::query::QueryMut<'a> for ()
        {
            type Output = $crate::acttey::ecs::query::QueryIterMut<'a, ()>;


            #[inline]
            fn query_mut(
                _storage: &mut impl $crate::acttey::ecs::storage::Store,
                _skey: $crate::acttey::ecs::SystemKey,
            ) -> Self::Output {
                QueryIterMut::empty()
            }

            #[inline]
            fn fkeys_mut(_skey: $crate::acttey::ecs::SystemKey)
                -> Vec<$crate::acttey::ecs::FilteResKey>
            {
                vec![]
            }

            #[inline]
            fn info_mut(_skey: $crate::acttey::ecs::SystemKey) -> $crate::acttey::ecs::query::QueryInfo {
                $crate::acttey::ecs::query::QueryInfo {
                    finfo: vec![]
                }
            }
        }

    };
    (1, $id:ident) => {
        impl<'a,
            $id: $crate::acttey::ecs::query::Filter>
            $crate::acttey::ecs::query::Query<'a> for $id
        {
            type Output = $crate::acttey::ecs::query::QueryIter<'a, $id::Target>;

            #[inline]
            fn query(
                storage: &mut impl $crate::acttey::ecs::storage::Store,
                skey: $crate::acttey::ecs::SystemKey,
            ) -> Self::Output {
                storage.get::<$id>(Self::gen_key(skey))
            }

            #[inline]
            fn fkeys(skey: $crate::acttey::ecs::SystemKey)
                -> Vec<$crate::acttey::ecs::FilteResKey>
            {
                vec![$crate::acttey::ecs::FilteResKey::new(
                    std::any::TypeId::of::<$id>(),
                    Self::gen_key(skey)
                )]
            }

            #[inline]
            fn info(skey: $crate::acttey::ecs::SystemKey) -> $crate::acttey::ecs::query::QueryInfo {
                $crate::acttey::ecs::query::QueryInfo {
                    finfo: vec![$id::info(Self::gen_key(skey))]
                }
            }
        }
        impl<'a,
            $id: $crate::acttey::ecs::query::Filter>
            $crate::acttey::ecs::query::QueryMut<'a> for $id
        {
            type Output = $crate::acttey::ecs::query::QueryIterMut<'a, $id::Target>;


            #[inline]
            fn query_mut(
                storage: &mut impl $crate::acttey::ecs::storage::Store,
                skey: $crate::acttey::ecs::SystemKey,
            ) -> Self::Output {
                storage.get_mut::<$id>(Self::gen_key_mut(skey))
            }

            #[inline]
            fn fkeys_mut(skey: $crate::acttey::ecs::SystemKey)
                -> Vec<$crate::acttey::ecs::FilteResKey>
            {
                vec![$crate::acttey::ecs::FilteResKey::new(
                    std::any::TypeId::of::<$id>(),
                    Self::gen_key_mut(skey)
                )]
            }

            #[inline]
            fn info_mut(skey: $crate::acttey::ecs::SystemKey) -> $crate::acttey::ecs::query::QueryInfo {
                $crate::acttey::ecs::query::QueryInfo {
                    finfo: vec![$id::info(Self::gen_key_mut(skey))]
                }
            }
        }
    };
    ($n:expr, $($id:ident),+) => {
        impl<'a,
            $($id: $crate::acttey::ecs::query::Filter),+>
            $crate::acttey::ecs::query::Query<'a> for ( $($id),+ )
        {
            type Output = ( $($crate::acttey::ecs::query::QueryIter<'a, $id::Target>),+ );

            #[inline]
            fn query(
                storage: &mut impl $crate::acttey::ecs::storage::Store,
                skey: $crate::acttey::ecs::SystemKey,
            ) -> Self::Output {
                let qkey = Self::gen_key(skey);
                ( $( storage.get::<$id>(qkey) ),+ )
            }

            #[inline]
            fn fkeys(skey: $crate::acttey::ecs::SystemKey)
                -> Vec<$crate::acttey::ecs::FilteResKey>
            {
                let qkey = Self::gen_key(skey);
                vec![
                    $( $crate::acttey::ecs::FilteResKey::new(
                        std::any::TypeId::of::<$id>(),
                        qkey
                    ) ),+
                ]
            }

            #[inline]
            fn info(skey: $crate::acttey::ecs::SystemKey) -> $crate::acttey::ecs::query::QueryInfo {
                let qkey = Self::gen_key(skey);
                $crate::acttey::ecs::query::QueryInfo {
                    finfo: vec![$( $id::info(qkey) ),+]
                }
            }
        }
        impl<'a,
            $($id: $crate::acttey::ecs::query::Filter),+>
            $crate::acttey::ecs::query::QueryMut<'a> for ( $($id),+ )
        {
            type Output = ( $($crate::acttey::ecs::query::QueryIterMut<'a, $id::Target>),+ );

            #[inline]
            fn query_mut(
                storage: &mut impl $crate::acttey::ecs::storage::Store,
                skey: $crate::acttey::ecs::SystemKey,
            ) -> Self::Output {
                let qkey = Self::gen_key_mut(skey);
                ( $( storage.get_mut::<$id>(qkey) ),+ )
            }

            #[inline]
            fn fkeys_mut(skey: $crate::acttey::ecs::SystemKey)
                -> Vec<$crate::acttey::ecs::FilteResKey>
            {
                let qkey = Self::gen_key_mut(skey);
                vec![
                    $( $crate::acttey::ecs::FilteResKey::new(
                        std::any::TypeId::of::<$id>(),
                        qkey
                    ) ),+
                ]
            }

            #[inline]
            fn info_mut(skey: $crate::acttey::ecs::SystemKey) -> $crate::acttey::ecs::query::QueryInfo {
                let qkey = Self::gen_key_mut(skey);
                $crate::acttey::ecs::query::QueryInfo {
                    finfo: vec![$( $id::info(qkey) ),+]
                }
            }
        }
    }
}

// impl `Query` and `QueryMut` for tuple combinations.
impl_query!(0);
impl_query!(1, A);
impl_query!(2, A, B);
impl_query!(3, A, B, C);
impl_query!(4, A, B, C, D);
impl_query!(5, A, B, C, D, E);
impl_query!(6, A, B, C, D, E, F);
impl_query!(7, A, B, C, D, E, F, G);
impl_query!(8, A, B, C, D, E, F, G, H);
impl_query!(9, A, B, C, D, E, F, G, H, I);
impl_query!(10, A, B, C, D, E, F, G, H, I, J);
impl_query!(11, A, B, C, D, E, F, G, H, I, J, K);
impl_query!(12, A, B, C, D, E, F, G, H, I, J, K, L);
impl_query!(13, A, B, C, D, E, F, G, H, I, J, K, L, M);
impl_query!(14, A, B, C, D, E, F, G, H, I, J, K, L, M, N);
impl_query!(15, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O);
impl_query!(16, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P);

macro_rules! impl_res_query {
    (0) => {
        impl<'a> $crate::acttey::ecs::query::ResQuery<'a> for ()
        {
            type Output = ();

            #[inline]
            fn query(_res_pack: &'a $crate::acttey::ecs::predefined::resource::ResourcePack) -> Self::Output {
            }
        }

        impl<'a> $crate::acttey::ecs::query::ResQueryMut<'a> for ()
        {
            type Output = ();

            #[inline]
            fn query_mut(_res_pack: &'a $crate::acttey::ecs::predefined::resource::ResourcePack) -> Self::Output {
            }
        }
    };
    (1, $id:ident) => {
        impl<'a,
            $id: $crate::acttey::ecs::traits::Resource>
            $crate::acttey::ecs::query::ResQuery<'a> for $id
        {
            type Output = &'a $id;

            #[inline]
            fn query(res_pack: &'a $crate::acttey::ecs::predefined::resource::ResourcePack) -> Self::Output {
                res_pack.get::<$id>()
            }
        }

        impl<'a,
            $id: $crate::acttey::ecs::traits::Resource>
            $crate::acttey::ecs::query::ResQueryMut<'a> for $id
        {
            type Output = &'a mut $id;

            #[inline]
            fn query_mut(res_pack: &'a $crate::acttey::ecs::predefined::resource::ResourcePack) -> Self::Output {
                res_pack.get::<$id>()
            }
        }
    };
    ($n:expr, $($id:ident),+) => {
        impl<'a,
            $($id: $crate::acttey::ecs::traits::Resource),+>
            $crate::acttey::ecs::query::ResQuery<'a> for ( $($id),+ )
        {
            type Output = ( $(&'a $id),+ );

            #[inline]
            fn query(res_pack: &'a $crate::acttey::ecs::predefined::resource::ResourcePack) -> Self::Output {
                ( $(res_pack.get::<$id>()),+ )
            }
        }

        impl<'a,
            $($id: $crate::acttey::ecs::traits::Resource),+>
            $crate::acttey::ecs::query::ResQueryMut<'a> for ( $($id),+ )
        {
            type Output = ( $(&'a mut $id),+ );

            #[inline]
            fn query_mut(res_pack: &'a $crate::acttey::ecs::predefined::resource::ResourcePack) -> Self::Output {
                ( $(res_pack.get::<$id>()),+ )
            }
        }
    };
}

// impl `ResQuery` and `ResQueryMut` for tuple combinations.
impl_res_query!(0);
impl_res_query!(1, A);
impl_res_query!(2, A, B);
impl_res_query!(3, A, B, C);
impl_res_query!(4, A, B, C, D);
impl_res_query!(5, A, B, C, D, E);
impl_res_query!(6, A, B, C, D, E, F);
impl_res_query!(7, A, B, C, D, E, F, G);
impl_res_query!(8, A, B, C, D, E, F, G, H);
impl_res_query!(9, A, B, C, D, E, F, G, H, I);
impl_res_query!(10, A, B, C, D, E, F, G, H, I, J);
impl_res_query!(11, A, B, C, D, E, F, G, H, I, J, K);
impl_res_query!(12, A, B, C, D, E, F, G, H, I, J, K, L);
impl_res_query!(13, A, B, C, D, E, F, G, H, I, J, K, L, M);
impl_res_query!(14, A, B, C, D, E, F, G, H, I, J, K, L, M, N);
impl_res_query!(15, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O);
impl_res_query!(16, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        acttey,
        ecs::{fkey, skey},
    };
    use acttey_ecs_macros::Component;
    use wasm_bindgen_test::*;

    wasm_bindgen_test_configure!(run_in_browser);

    #[derive(Component)]
    struct CA;

    #[derive(Component)]
    struct CB;

    struct FA;
    impl Filter for FA {
        type Target = CA;
        type All = (CA, CB);
        type Any = CA;
        type None = ();
    }

    struct FB;
    impl Filter for FB {
        type Target = CB;
        type All = ();
        type Any = (CB, CA);
        type None = CA;
    }

    type Q = (FA, FB);

    #[wasm_bindgen_test]
    fn test_query() {
        // gen_key() and gen_key_mut()
        let skey = skey!(());
        let qkey = <Q as Query>::gen_key(skey);
        let qkey_mut = <Q as QueryMut>::gen_key_mut(skey);
        assert_ne!(qkey, qkey_mut);

        // gen_fkeys() and gen_fkeys_mut()
        assert_eq!(
            vec![fkey!(FA, qkey), fkey!(FB, qkey)],
            <Q as Query>::fkeys(skey)
        );
        assert_eq!(
            vec![fkey!(FA, qkey_mut), fkey!(FB, qkey_mut)],
            <Q as QueryMut>::fkeys_mut(skey)
        );
    }
}
