use super::{
    entity::ContainEntity,
    filter::Filtered,
    query::{
        EntQueryInfo, EntQueryKey, EntQueryMut, Query, QueryInfo, QueryKey, QueryMut, ResQuery,
        ResQueryInfo, ResQueryKey, ResQueryMut, StoreEntQueryInfo, StoreQueryInfo,
        StoreResQueryInfo,
    },
};
use crate::ds::borrow::Borrowed;
use std::{
    any::{self, TypeId},
    ptr::NonNull,
    sync::{atomic::AtomicI32, Arc},
};

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
        RequestKey::new(TypeId::of::<Self>())
    }

    fn get_info<S>(info_stor: &mut S) -> Arc<RequestInfo>
    where
        S: StoreRequestInfo,
    {
        let key = Self::key();
        if let Some(info) = StoreRequestInfo::get(info_stor, &key) {
            Arc::clone(info)
        } else {
            let info = Arc::new(RequestInfo {
                name: any::type_name::<Self>(),
                read: (
                    <Self::Read as Query>::key(),
                    <Self::Read as Query>::get_info(info_stor),
                ),
                write: (
                    <Self::Write as QueryMut>::key(),
                    <Self::Write as QueryMut>::get_info(info_stor),
                ),
                res_read: (
                    <Self::ResRead as ResQuery>::key(),
                    <Self::ResRead as ResQuery>::get_info(info_stor),
                ),
                res_write: (
                    <Self::ResWrite as ResQueryMut>::key(),
                    <Self::ResWrite as ResQueryMut>::get_info(info_stor),
                ),
                ent_write: (
                    <Self::EntWrite as EntQueryMut>::key(),
                    <Self::EntWrite as EntQueryMut>::get_info(info_stor),
                ),
            });
            StoreRequestInfo::insert(info_stor, key, Arc::clone(&info));
            info
        }
    }
}

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
        impl $crate::ecs::request::Request for $id {
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

pub trait StoreRequestInfo: StoreQueryInfo + StoreResQueryInfo + StoreEntQueryInfo {
    fn get(&self, key: &RequestKey) -> Option<&Arc<RequestInfo>>;
    fn insert(&mut self, key: RequestKey, info: Arc<RequestInfo>);
    fn remove(&mut self, key: &RequestKey) -> Option<Arc<RequestInfo>>;
}

/// [`TypeId`] for [`Request`].
#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
#[repr(transparent)]
pub struct RequestKey(TypeId);

impl RequestKey {
    pub const fn new(ty: TypeId) -> Self {
        Self(ty)
    }

    pub const fn as_type(&self) -> &TypeId {
        &self.0
    }
}

#[derive(Debug, Clone)]
pub struct RequestInfo {
    name: &'static str,
    read: (QueryKey, Arc<QueryInfo>),
    write: (QueryKey, Arc<QueryInfo>),
    res_read: (ResQueryKey, Arc<ResQueryInfo>),
    res_write: (ResQueryKey, Arc<ResQueryInfo>),
    ent_write: (EntQueryKey, Arc<EntQueryInfo>),
}

impl RequestInfo {
    pub fn name(&self) -> &'static str {
        self.name
    }

    pub fn read(&self) -> &(QueryKey, Arc<QueryInfo>) {
        &self.read
    }

    pub fn write(&self) -> &(QueryKey, Arc<QueryInfo>) {
        &self.write
    }

    pub fn res_read(&self) -> &(ResQueryKey, Arc<ResQueryInfo>) {
        &self.res_read
    }

    pub fn res_write(&self) -> &(ResQueryKey, Arc<ResQueryInfo>) {
        &self.res_write
    }

    pub fn ent_write(&self) -> &(EntQueryKey, Arc<EntQueryInfo>) {
        &self.ent_write
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
    pub(super) read: Box<[Filtered]>,

    /// Writable component buffer corresponding to each [`Filter`](super::filter::Filter) in write [`QueryMut`].
    pub(super) write: Box<[Filtered]>,

    /// Read-only resource buffer correspopnding to each request in [`ResQuery`].
    pub(super) res_read: Vec<Borrowed<NonNull<u8>, AtomicI32>>,

    /// Writable resource buffer correspopnding to each request in [`ResQueryMut`].
    pub(super) res_write: Vec<Borrowed<NonNull<u8>, AtomicI32>>,

    /// Writable entity buffer corresponding to each request in [`EntQueryMut`].
    pub(super) ent_write: Vec<Borrowed<NonNull<dyn ContainEntity>, AtomicI32>>,
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

    pub fn res_read(&self) -> &[Borrowed<NonNull<u8>, AtomicI32>] {
        &self.res_read
    }

    pub fn res_write(&self) -> &[Borrowed<NonNull<u8>, AtomicI32>] {
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

pub struct Response<Req: Request> {
    pub read: <Req::Read as Query>::Output,
    pub write: <Req::Write as QueryMut>::Output,
    pub res_read: <Req::ResRead as ResQuery>::Output,
    pub res_write: <Req::ResWrite as ResQueryMut>::Output,
    pub ent_write: <Req::EntWrite as EntQueryMut>::Output,
}

impl<Req: Request> Response<Req> {
    pub fn new(buf: &mut RequestBuffer) -> Self {
        Self {
            read: <Req::Read as Query>::convert(&buf.read),
            write: <Req::Write as QueryMut>::convert(&mut buf.write),
            res_read: <Req::ResRead as ResQuery>::convert(&mut buf.res_read),
            res_write: <Req::ResWrite as ResQueryMut>::convert(&mut buf.res_write),
            ent_write: <Req::EntWrite as EntQueryMut>::convert(&mut buf.ent_write),
        }
    }
}
