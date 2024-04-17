use super::{
    borrow_js::JsAtomic,
    entity::OwnedEntityKey,
    filter::Filtered,
    query::{
        Query, QueryInfo, QueryKey, QueryMut, ResQuery, ResQueryInfo, ResQueryKey, ResQueryMut,
        StoreQueryInfo,
    },
};
use crate::ds::borrow::Borrowed;
use std::{
    any::{type_name, TypeId},
    ptr::NonNull,
    sync::Arc,
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

    /// Writable access [`ResQuery`] composed of [`Resource`](super::resource::Resource)s.
    /// Writable access always takes exclusive authority over the target.
    type ResWrite: ResQueryMut;

    /// If you want to insert or remove entity, you have to make a request for it.
    /// Otherwise, ignore this method.
    fn entity_write() -> impl IntoIterator<Item = OwnedEntityKey> {
        std::iter::empty()
    }

    fn key() -> RequestKey {
        RequestKey::new(TypeId::of::<Self>())
    }

    fn info<S>(info_stor: &mut S) -> Arc<RequestInfo>
    where
        S: StoreRequestInfo,
    {
        let key = Self::key();
        if let Some(info) = StoreRequestInfo::get(info_stor, &key) {
            Arc::clone(info)
        } else {
            let info = Arc::new(RequestInfo {
                name: type_name::<Self>(),
                read: (
                    <Self::Read as Query>::key(),
                    <Self::Read as Query>::info(info_stor),
                ),
                write: (
                    <Self::Write as QueryMut>::key(),
                    <Self::Write as QueryMut>::info(info_stor),
                ),
                res_read: (
                    <Self::ResRead as ResQuery>::key(),
                    <Self::ResRead as ResQuery>::info(),
                ),
                res_write: (
                    <Self::ResWrite as ResQueryMut>::key(),
                    <Self::ResWrite as ResQueryMut>::info(),
                ),
                ent_write: Self::entity_write()
                    .into_iter()
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            });
            StoreRequestInfo::insert(info_stor, key, Arc::clone(&info));
            info
        }
    }
}

pub trait StoreRequestInfo: StoreQueryInfo {
    fn get(&self, key: &RequestKey) -> Option<&Arc<RequestInfo>>;
    fn insert(&mut self, key: RequestKey, info: Arc<RequestInfo>);
    fn remove(&mut self, key: &RequestKey) -> Option<Arc<RequestInfo>>;
}

/// [`TypeId`] for [`Request`].
#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
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
    res_read: (ResQueryKey, ResQueryInfo),
    res_write: (ResQueryKey, ResQueryInfo),
    ent_write: Box<[OwnedEntityKey]>,
}

impl RequestInfo {
    #[inline]
    pub fn name(&self) -> &'static str {
        self.name
    }

    #[inline]
    pub fn read(&self) -> &(QueryKey, Arc<QueryInfo>) {
        &self.read
    }

    #[inline]
    pub fn write(&self) -> &(QueryKey, Arc<QueryInfo>) {
        &self.write
    }

    #[inline]
    pub fn res_read(&self) -> &(ResQueryKey, ResQueryInfo) {
        &self.res_read
    }

    #[inline]
    pub fn res_write(&self) -> &(ResQueryKey, ResQueryInfo) {
        &self.res_write
    }

    #[inline]
    pub fn ent_write(&self) -> &[OwnedEntityKey] {
        &self.ent_write
    }
}

/// An implementation of [`Request`] that requests nothing.
#[derive(Debug, Clone, Copy)]
pub(crate) struct EmptyRequest;

impl Request for EmptyRequest {
    type Read = ();
    type Write = ();
    type ResRead = ();
    type ResWrite = ();
}

#[derive(Debug)]
pub struct RequestBuffer {
    /// Read-only component buffer corresponding to each [`Filter`](super::filter::Filter) in read [`Query`].
    pub(super) read: Box<[Filtered]>,

    /// Writable component buffer corresponding to each [`Filter`](super::filter::Filter) in write [`QueryMut`].
    pub(super) write: Box<[Filtered]>,

    /// Read-only resource buffer correspopnding to each request in read [`ResQuery`].
    pub(super) res_read: Vec<Borrowed<NonNull<u8>, JsAtomic>>,

    /// Writable resource buffer correspopnding to each request in write [`ResQueryMut`].
    pub(super) res_write: Vec<Borrowed<NonNull<u8>, JsAtomic>>,
    // TODO: ent_write
}

impl RequestBuffer {
    pub fn new() -> Self {
        Self {
            read: [].into(),
            write: [].into(),
            res_read: Vec::new(),
            res_write: Vec::new(),
        }
    }

    #[inline]
    pub fn read(&self) -> &[Filtered] {
        &self.read
    }

    #[inline]
    pub fn write(&self) -> &[Filtered] {
        &self.write
    }

    #[inline]
    pub fn res_read(&self) -> &[Borrowed<NonNull<u8>, JsAtomic>] {
        &self.res_read
    }

    #[inline]
    pub fn res_write(&self) -> &[Borrowed<NonNull<u8>, JsAtomic>] {
        &self.res_write
    }

    // TODO: ent_write
}

impl Default for RequestBuffer {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Response<'a, R: Request> {
    pub read: <R::Read as Query>::Output<'a>,
    pub write: <R::Write as QueryMut>::Output<'a>,
    pub res_read: <R::ResRead as ResQuery>::Output,
    pub res_write: <R::ResWrite as ResQueryMut>::Output,
    pub ent_write: (), /* TODO */
}

impl<'a, R: Request> Response<'a, R> {
    pub fn new(buf: &'a RequestBuffer) -> Self {
        Self {
            read: <R::Read as Query>::convert(&buf.read),
            write: <R::Write as QueryMut>::convert(&buf.write),
            res_read: <R::ResRead as ResQuery>::convert(&buf.res_read),
            res_write: <R::ResWrite as ResQueryMut>::convert(&buf.res_write),
            ent_write: (), /* TODO */
        }
    }
}
