use super::{
    filter::Filtered,
    query::{
        EntQueryInfo, EntQueryKey, EntQueryMut, Query, QueryInfo, QueryKey, QueryMut, ResQuery,
        ResQueryInfo, ResQueryKey, ResQueryMut, StoreEntQueryInfo, StoreQueryInfo,
        StoreResQueryInfo,
    },
};
use crate::ds::prelude::*;
use crate::ecs::ent::{
    component::ComponentKey,
    entity::{ContainEntity, EntityKey},
};
use crate::ecs::resource::ResourceKey;
use std::{any, marker::PhantomData, ptr::NonNull, rc::Rc, sync::atomic::AtomicI32};

/// A single request is about needs for all sorts of components, resources, and entity containers.
/// In other words, a request is a combination of [`Query`]s, [`ResQuery`]s, and queries for entity containers.
/// They must be requested at once in order to prevent dead lock.
/// You can make a request by implementing this trait and put it in a system.
pub trait Request: 'static {
    /// Read-only access [`Query`] composed of [`Filter`](super::filter::Filter)s.
    /// Read-only access can help us execute systems simultaneously.
    type Read: Query;

    /// Writable access [`QueryMut`] composed of [`Filter`](super::filter::Filter)s.
    /// Writable access always takes exclusive authority over the target.
    type Write: QueryMut;

    /// Read-only access [`ResQuery`] composed of [`Resource`](super::resource::Resource)s.
    /// Read-only access can help us execute systems simultaneously.
    type ResRead: ResQuery;

    /// Writable access [`ResQueryMut`] composed of [`Resource`](super::resource::Resource)s.
    /// Writable access always takes exclusive authority over the target.
    type ResWrite: ResQueryMut;

    /// Writable access [`EntQueryMut`] composed of [`Entity`](super::entity::Entity).
    /// Writable acess always takes exclusive authority over the target.
    type EntWrite: EntQueryMut;

    fn key() -> RequestKey {
        RequestKey::of::<Self>()
    }
}

pub(crate) trait PrivateRequest: Request {
    fn get_info<S>(stor: &mut S) -> Rc<RequestInfo>
    where
        S: StoreRequestInfo,
    {
        let key = Self::key();
        if let Some(info) = StoreRequestInfo::get(stor, &key) {
            info
        } else {
            let info = Rc::new(RequestInfo {
                name: any::type_name::<Self>(),
                read: (
                    <Self::Read as Query>::key(),
                    <Self::Read as Query>::get_info(stor),
                ),
                write: (
                    <Self::Write as QueryMut>::key(),
                    <Self::Write as QueryMut>::get_info(stor),
                ),
                res_read: (
                    <Self::ResRead as ResQuery>::key(),
                    <Self::ResRead as ResQuery>::get_info(stor),
                ),
                res_write: (
                    <Self::ResWrite as ResQueryMut>::key(),
                    <Self::ResWrite as ResQueryMut>::get_info(stor),
                ),
                ent_write: (
                    <Self::EntWrite as EntQueryMut>::key(),
                    <Self::EntWrite as EntQueryMut>::get_info(stor),
                ),
            });
            StoreRequestInfo::insert(stor, key, &info);
            info
        }
    }
}

impl<T: Request> PrivateRequest for T {}

/// Blanket implementation of [`Request`] for tuples of queries.
impl<R, W, RR, RW, EW> Request for (R, W, RR, RW, EW)
where
    R: Query,
    W: QueryMut,
    RR: ResQuery,
    RW: ResQueryMut,
    EW: EntQueryMut,
{
    type Read = R;
    type Write = W;
    type ResRead = RR;
    type ResWrite = RW;
    type EntWrite = EW;
}

/// A macro for declaration of an empty structure and implementation [`Request`] for the structure.
#[macro_export]
macro_rules! request {
    (
        $vis:vis $id:ident
        $(, Read=( $($read:ty),+ ))?
        $(, Write=( $($write:ty),+ ))?
        $(, ResRead=( $($res_read:ty),+ ))?
        $(, ResWrite=( $($res_write:ty),+ ))?
        $(, EntWrite=( $($ent_write:ty),+ ))?
    ) => {
        #[derive(Debug)]
        $vis struct $id;
        impl $crate::ecs::sys::request::Request for $id {
            #[allow(unused_parens)]
            type Read = ( $( $($read),+ )? );

            #[allow(unused_parens)]
            type Write = ( $( $($write),+ )? );

            #[allow(unused_parens)]
            type ResRead = ( $( $($res_read),+ )? );

            #[allow(unused_parens)]
            type ResWrite = ( $( $($res_write),+ )? );

            #[allow(unused_parens)]
            type EntWrite = ( $( $($ent_write),+ )? );
        }
    };
}

pub(crate) trait StoreRequestInfo:
    StoreQueryInfo + StoreResQueryInfo + StoreEntQueryInfo
{
    fn get(&self, key: &RequestKey) -> Option<Rc<RequestInfo>>;
    fn insert(&mut self, key: RequestKey, info: &Rc<RequestInfo>);
    fn remove(&mut self, key: &RequestKey);
}

/// [`TypeId`] of a [`Request`].
pub type RequestKey = ATypeId<RequestKey_>;
pub struct RequestKey_;

#[derive(Debug, Clone)]
pub(crate) struct RequestInfo {
    name: &'static str,
    read: (QueryKey, Rc<QueryInfo>),
    write: (QueryKey, Rc<QueryInfo>),
    res_read: (ResQueryKey, Rc<ResQueryInfo>),
    res_write: (ResQueryKey, Rc<ResQueryInfo>),
    ent_write: (EntQueryKey, Rc<EntQueryInfo>),
}

impl RequestInfo {
    pub(crate) fn name(&self) -> &'static str {
        self.name
    }

    pub(crate) fn read(&self) -> &(QueryKey, Rc<QueryInfo>) {
        &self.read
    }

    pub(crate) fn write(&self) -> &(QueryKey, Rc<QueryInfo>) {
        &self.write
    }

    pub(crate) fn res_read(&self) -> &(ResQueryKey, Rc<ResQueryInfo>) {
        &self.res_read
    }

    pub(crate) fn res_write(&self) -> &(ResQueryKey, Rc<ResQueryInfo>) {
        &self.res_write
    }

    pub(crate) fn ent_write(&self) -> &(EntQueryKey, Rc<EntQueryInfo>) {
        &self.ent_write
    }

    pub(crate) fn resource_keys(&self) -> impl Iterator<Item = &ResourceKey> {
        let read = self.res_read().1.as_ref();
        let write = self.res_write().1.as_ref();
        read.rkeys().iter().chain(write.rkeys())
    }

    pub(crate) fn entity_keys(&self) -> impl Iterator<Item = &EntityKey> {
        self.ent_write().1.as_ref().ekeys().iter()
    }

    /// Determines whether the request info is valid or not.
    /// Request info that meets conditions below is valid.
    /// 1. Targets of filters in read & write queries are unique.
    /// 2. Entities in entity write query don't contain any targets
    ///    in read & write queries.
    /// 3. Resource in read & write resource queries are unique.
    pub(crate) fn is_valid(
        &self,
        mut contains_comp: impl FnMut(&EntityKey, &ComponentKey) -> bool,
        mut has_dup: impl FnMut(&[EntityKey]) -> bool,
    ) -> bool {
        // 1. Are there duplicate filter targets?
        let reads = self
            .read()
            .1
            .filters()
            .iter()
            .map(|(_fkey, finfo)| finfo.target());
        let writes = self
            .write()
            .1
            .filters()
            .iter()
            .map(|(_fkey, finfo)| finfo.target());
        let mut ckeys = reads.chain(writes).collect::<Vec<_>>();
        ckeys.sort_unstable();
        for win in ckeys.windows(2) {
            if win[0] == win[1] {
                return false;
            }
        }

        // 2. Does entity write query conflict with read & write queries?
        let ekeys = self.ent_write().1.ekeys();
        if has_dup(ekeys) {
            return false;
        }
        for ekey in ekeys.iter() {
            for ckey in ckeys.iter() {
                if contains_comp(ekey, ckey) {
                    return false;
                }
            }
        }

        // 3. Are there duplicate resources?
        let reads = self.res_read().1.rkeys().iter();
        let writes = self.res_write().1.rkeys().iter();
        let mut rkeys = reads.chain(writes).collect::<Vec<_>>();
        rkeys.sort_unstable();
        for win in rkeys.windows(2) {
            if win[0] == win[1] {
                return false;
            }
        }

        true
    }
}

/// Empty request.
impl Request for () {
    type Read = ();
    type Write = ();
    type ResRead = ();
    type ResWrite = ();
    type EntWrite = ();
}

#[derive(Debug)]
pub struct RequestBuffer {
    /// Read-only component buffer corresponding to each [`Filter`](super::filter::Filter) in read [`Query`].
    pub(crate) read: Box<[Filtered]>,

    /// Writable component buffer corresponding to each [`Filter`](super::filter::Filter) in write [`QueryMut`].
    pub(crate) write: Box<[Filtered]>,

    /// Read-only resource buffer correspopnding to each request in [`ResQuery`].
    pub(crate) res_read: Vec<Borrowed<ManagedConstPtr<u8>, AtomicI32>>,

    /// Writable resource buffer correspopnding to each request in [`ResQueryMut`].
    pub(crate) res_write: Vec<Borrowed<ManagedMutPtr<u8>, AtomicI32>>,

    /// Writable entity buffer corresponding to each request in [`EntQueryMut`].
    pub(crate) ent_write: Vec<Borrowed<NonNull<dyn ContainEntity>, AtomicI32>>,
}

impl RequestBuffer {
    pub fn new() -> Self {
        Self {
            read: [].into(),
            write: [].into(),
            res_read: Vec::new(),
            res_write: Vec::new(),
            ent_write: Vec::new(),
        }
    }

    pub fn read(&self) -> &[Filtered] {
        &self.read
    }

    pub fn write(&self) -> &[Filtered] {
        &self.write
    }

    pub fn res_read(&self) -> &[Borrowed<ManagedConstPtr<u8>, AtomicI32>] {
        &self.res_read
    }

    pub fn res_write(&self) -> &[Borrowed<ManagedMutPtr<u8>, AtomicI32>] {
        &self.res_write
    }

    pub fn ent_write(&self) -> &[Borrowed<NonNull<dyn ContainEntity>, AtomicI32>] {
        &self.ent_write
    }
}

impl Default for RequestBuffer {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Response<'a, Req: Request> {
    pub read: <Req::Read as Query>::Output,
    pub write: <Req::Write as QueryMut>::Output,
    pub res_read: <Req::ResRead as ResQuery>::Output,
    pub res_write: <Req::ResWrite as ResQueryMut>::Output,
    pub ent_write: <Req::EntWrite as EntQueryMut>::Output,
    _marker: PhantomData<&'a mut ()>,
}

impl<'a, Req: Request> Response<'a, Req> {
    pub fn new(buf: &'a mut RequestBuffer) -> Self {
        Self {
            read: <Req::Read as Query>::convert(&buf.read),
            write: <Req::Write as QueryMut>::convert(&mut buf.write),
            res_read: <Req::ResRead as ResQuery>::convert(&mut buf.res_read),
            res_write: <Req::ResWrite as ResQueryMut>::convert(&mut buf.res_write),
            ent_write: <Req::EntWrite as EntQueryMut>::convert(&mut buf.ent_write),
            _marker: PhantomData,
        }
    }
}
