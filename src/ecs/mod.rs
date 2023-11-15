pub mod entity;
pub mod query;
pub mod storage;
pub mod system;

use std::any::TypeId;

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub struct SystemKey(TypeId);

impl SystemKey {
    #[inline]
    pub fn new(ty: TypeId) -> Self {
        Self(ty)
    }
}

#[allow(unused_macros)]
macro_rules! skey {
    (&$t:ident) => {
        $crate::ecs::SystemKey::new($crate::ty!(&$t))
    };
    (&&$t:ident) => {
        $crate::ecs::SystemKey::new($crate::ty!(&&$t))
    };
    ($t:ty) => {
        $crate::ecs::SystemKey::new($crate::ty!($t))
    };
}

#[allow(unused_imports)]
pub(crate) use skey;

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub struct QueryKey {
    q_ty: TypeId,
    skey: SystemKey,
}

impl QueryKey {
    #[inline]
    pub fn new(q_ty: TypeId, skey: SystemKey) -> Self {
        Self { q_ty, skey }
    }
}

#[allow(unused_macros)]
macro_rules! qkey {
    (&$t:ident, $skey:ident) => {
        $crate::ecs::QueryKey::new($crate::ty!(&$t), $skey)
    };
    (&&$t:ident, $skey:ident) => {
        $crate::ecs::QueryKey::new($crate::ty!(&&$t), $skey)
    };
    ($t:ty, $skey:ident) => {
        $crate::ecs::QueryKey::new($crate::ty!($t), $skey)
    };
}

#[allow(unused_imports)]
pub(crate) use qkey;

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub struct FilterKey {
    f_ty: TypeId,
    qkey: QueryKey,
}

impl FilterKey {
    #[inline]
    pub fn new(f_ty: TypeId, qkey: QueryKey) -> Self {
        Self { f_ty, qkey }
    }
}

#[allow(unused_macros)]
macro_rules! fkey {
    (&$t:ident, $qkey:ident) => {
        $crate::ecs::FilterKey::new($crate::ty!(&$t), $qkey)
    };
    (&&$t:ident, $qkey:ident) => {
        $crate::ecs::FilterKey::new($crate::ty!(&&$t), $qkey)
    };
    ($t:ty, $qkey:ident) => {
        $crate::ecs::FilterKey::new($crate::ty!($t), $qkey)
    };
}

#[allow(unused_imports)]
pub(crate) use fkey;

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub struct EntityKey(TypeId);

impl EntityKey {
    #[inline]
    pub fn new(ty: TypeId) -> Self {
        Self(ty)
    }
}

#[allow(unused_macros)]
macro_rules! ekey {
    (&$t:ident) => {
        $crate::ecs::EntityKey::new($crate::ty!(&$t))
    };
    (&&$t:ident) => {
        $crate::ecs::EntityKey::new($crate::ty!(&&$t))
    };
    ($t:ty) => {
        $crate::ecs::EntityKey::new($crate::ty!($t))
    };
}

#[allow(unused_imports)]
pub(crate) use ekey;

#[derive(Hash, PartialEq, Eq, Clone, Copy)]
pub struct ComponentKey {
    c_ty: TypeId,
    ekey: EntityKey,
}

impl ComponentKey {
    #[inline]
    pub fn new(c_ty: TypeId, ekey: EntityKey) -> Self {
        Self { c_ty, ekey }
    }
}

#[allow(unused_macros)]
macro_rules! ckey {
    (&$t:ident, $ekey:ident) => {
        $crate::ecs::ComponentKey::new($crate::ty!(&$t), $ekey)
    };
    (&&$t:ident, $ekey:ident) => {
        $crate::ecs::ComponentKey::new($crate::ty!(&&$t), $ekey)
    };
    ($t:ty, $ekey:ident) => {
        $crate::ecs::ComponentKey::new($crate::ty!($t), $ekey)
    };
}

#[allow(unused_imports)]
pub(crate) use ckey;
