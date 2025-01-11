use super::{
    query::{
        EntQueryInfo, EntQueryKey, EntQueryMut, Query, QueryInfo, QueryKey, QueryMut, ResQuery,
        ResQueryInfo, ResQueryKey, ResQueryMut, StoreEntQueryInfo, StoreQueryInfo,
        StoreResQueryInfo,
    },
    select::{
        FilterInfo, FilterKey, FilteredRaw, SelectInfo, SelectKey, SelectedRaw, StoreFilterInfo,
        StoreSelectInfo,
    },
};
use crate::{debug_format, ds::prelude::*, ecs::resource::ResourceKey, DefaultHasher};
use std::{
    any,
    collections::HashMap,
    fmt,
    hash::BuildHasher,
    marker::PhantomData,
    ptr::NonNull,
    sync::{Arc, LazyLock, Mutex},
};

/// A storage including request, query and filter information together.
//
// When a system is registered, it's corresponding request and
// related other information is registered here, and it can be shared from other systems.
// When it comes to unregister, each system data will unregister itself from
// this stroage when it's dropped.
pub(crate) static RINFO_STOR: LazyLock<Arc<Mutex<RequestInfoStorage<DefaultHasher>>>> =
    const { LazyLock::new(|| Arc::new(Mutex::new(RequestInfoStorage::new()))) };

/// Storage containig request and other info.
#[derive(Debug)]
pub(crate) struct RequestInfoStorage<S> {
    /// [`RequestKey`] -> [`RequestInfo`].
    rinfo: HashMap<RequestKey, Arc<RequestInfo>, S>,

    /// [`QueryKey`] -> [`QueryInfo`].
    qinfo: HashMap<QueryKey, Arc<QueryInfo>, S>,

    /// [`ResQueryKey`] -> [`ResQueryInfo`].
    rqinfo: HashMap<ResQueryKey, Arc<ResQueryInfo>, S>,

    /// [`EntQueryKey`] -> [`EntQueryInfo`].
    eqinfo: HashMap<EntQueryKey, Arc<EntQueryInfo>, S>,

    /// [`SelectKey`] -> [`SelectInfo`].
    sinfo: HashMap<SelectKey, Arc<SelectInfo>, S>,

    /// [`FilterKey`] -> [`FilterInfo`].
    finfo: HashMap<FilterKey, Arc<FilterInfo>, S>,
}

impl<S> RequestInfoStorage<S>
where
    S: Default,
{
    fn new() -> Self {
        Self {
            rinfo: HashMap::default(),
            qinfo: HashMap::default(),
            rqinfo: HashMap::default(),
            eqinfo: HashMap::default(),
            sinfo: HashMap::default(),
            finfo: HashMap::default(),
        }
    }
}

impl<S> RequestInfoStorage<S>
where
    S: BuildHasher,
{
    // for future use.
    #[allow(dead_code)]
    pub(crate) fn get_request_info(&self, key: &RequestKey) -> Option<&Arc<RequestInfo>> {
        StoreRequestInfo::get(self, key)
    }

    // for future use.
    #[allow(dead_code)]
    pub(crate) fn get_query_info(&self, key: &QueryKey) -> Option<&Arc<QueryInfo>> {
        StoreQueryInfo::get(self, key)
    }

    // for future use.
    #[allow(dead_code)]
    pub(crate) fn get_resource_query_info(&self, key: &ResQueryKey) -> Option<&Arc<ResQueryInfo>> {
        StoreResQueryInfo::get(self, key)
    }

    // for future use.
    #[allow(dead_code)]
    pub(crate) fn get_entity_query_info(&self, key: &EntQueryKey) -> Option<&Arc<EntQueryInfo>> {
        StoreEntQueryInfo::get(self, key)
    }

    // for future use.
    #[allow(dead_code)]
    pub(crate) fn get_select_info(&self, key: &SelectKey) -> Option<&Arc<SelectInfo>> {
        StoreSelectInfo::get(self, key)
    }

    // for future use.
    #[allow(dead_code)]
    pub(crate) fn get_filter_info(&self, key: &FilterKey) -> Option<&Arc<FilterInfo>> {
        StoreFilterInfo::get(self, key)
    }

    fn remove(&mut self, key: &RequestKey) {
        // Removes request info if it's not referenced from external anymore.
        if matches!(self.rinfo.get(key), Some(x) if Arc::strong_count(x) == 1) {
            // Safety: We checked it in matches.
            let rinfo = unsafe { self.rinfo.remove(key).unwrap_unchecked() };

            // `RequestInfo` contains other info, so copy keys and drop rinfo first
            // in order to keep remove code simple.
            let read_key = rinfo.read().0;
            let write_key = rinfo.write().0;
            let res_read_key = rinfo.res_read().0;
            let res_write_key = rinfo.res_write().0;
            let ent_write_key = rinfo.ent_write().0;
            drop(rinfo);

            // Removes read & write query and select info.
            remove_qinfo_sinfo(self, &read_key);
            remove_qinfo_sinfo(self, &write_key);

            // Removes read & write resource info.
            remove_rqinfo(self, &res_read_key);
            remove_rqinfo(self, &res_write_key);

            // Removes write entity info.
            remove_eqinfo(self, &ent_write_key);
        }

        // Removes query and select info if it's not referenced from external anymore.
        // This function must be called inside `remove()`.
        fn remove_qinfo_sinfo<S>(this: &mut RequestInfoStorage<S>, key: &QueryKey)
        where
            S: BuildHasher,
        {
            // `self.qinfo` = 1.
            const QINFO_EMPTY_STRONG_CNT: usize = 1;

            if matches! (
                this.qinfo.get(key),
                Some(x) if Arc::strong_count(x) == QINFO_EMPTY_STRONG_CNT
            ) {
                // `QueryInfo` contains `FilterInfo` in it.
                // We need to remove `FilterInfo` first.
                // Safety: We checked it in matches.
                let qinfo = unsafe { this.qinfo.remove(key).unwrap_unchecked() };

                // Removes filter info it's not referenced from external anymore.
                for (fkey, sinfo) in qinfo.selectors() {
                    // `sinfo` + `self.sinfo` = 2.
                    const FINFO_EMPTY_STRONG_CNT: usize = 2;

                    if Arc::strong_count(sinfo) == FINFO_EMPTY_STRONG_CNT {
                        this.sinfo.remove(fkey);
                    }
                }
            }
        }

        // Removes resource query info if it's not referenced from external anymore.
        // This function must be called inside `remove()`.
        fn remove_rqinfo<S>(this: &mut RequestInfoStorage<S>, key: &ResQueryKey)
        where
            S: BuildHasher,
        {
            // `self.rqinfo` = 1.
            const EMPTY_STRONG_CNT: usize = 1;

            if matches! (
                this.rqinfo.get(key),
                Some(x) if Arc::strong_count(x) == EMPTY_STRONG_CNT
            ) {
                this.rqinfo.remove(key);
            }
        }

        // Removes entity query info if it's not referenced from external anymore.
        // This function must be called inside `remove()`.
        fn remove_eqinfo<S>(this: &mut RequestInfoStorage<S>, key: &EntQueryKey)
        where
            S: BuildHasher,
        {
            // `self.eqinfo` = 1.
            const EMPTY_STRONG_CNT: usize = 1;

            if matches! (
                this.eqinfo.get(key),
                Some(x) if Arc::strong_count(x) == EMPTY_STRONG_CNT
            ) {
                this.eqinfo.remove(key);
            }
        }
    }
}

impl<S> Default for RequestInfoStorage<S>
where
    S: Default,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<S> StoreRequestInfo for RequestInfoStorage<S>
where
    S: BuildHasher,
{
    fn contains(&self, key: &RequestKey) -> bool {
        self.rinfo.contains_key(key)
    }

    fn get(&self, key: &RequestKey) -> Option<&Arc<RequestInfo>> {
        self.rinfo.get(key)
    }

    fn insert(&mut self, key: RequestKey, info: Arc<RequestInfo>) {
        self.rinfo.insert(key, info);
    }

    // Top level cleaner.
    fn remove(&mut self, key: &RequestKey) {
        self.remove(key)
    }
}

impl<S> StoreQueryInfo for RequestInfoStorage<S>
where
    S: BuildHasher,
{
    fn contains(&self, key: &QueryKey) -> bool {
        self.qinfo.contains_key(key)
    }

    fn get(&self, key: &QueryKey) -> Option<&Arc<QueryInfo>> {
        self.qinfo.get(key)
    }

    fn insert(&mut self, key: QueryKey, info: Arc<QueryInfo>) {
        self.qinfo.insert(key, info);
    }
}

impl<S> StoreResQueryInfo for RequestInfoStorage<S>
where
    S: BuildHasher,
{
    fn contains(&self, key: &ResQueryKey) -> bool {
        self.rqinfo.contains_key(key)
    }

    fn get(&self, key: &ResQueryKey) -> Option<&Arc<ResQueryInfo>> {
        self.rqinfo.get(key)
    }

    fn insert(&mut self, key: ResQueryKey, info: Arc<ResQueryInfo>) {
        self.rqinfo.insert(key, info);
    }
}

impl<S> StoreEntQueryInfo for RequestInfoStorage<S>
where
    S: BuildHasher,
{
    fn contains(&self, key: &EntQueryKey) -> bool {
        self.eqinfo.contains_key(key)
    }

    fn get(&self, key: &EntQueryKey) -> Option<&Arc<EntQueryInfo>> {
        self.eqinfo.get(key)
    }

    fn insert(&mut self, key: EntQueryKey, info: Arc<EntQueryInfo>) {
        self.eqinfo.insert(key, info);
    }
}

impl<S> StoreSelectInfo for RequestInfoStorage<S>
where
    S: BuildHasher,
{
    fn contains(&self, key: &SelectKey) -> bool {
        self.sinfo.contains_key(key)
    }

    fn get(&self, key: &SelectKey) -> Option<&Arc<SelectInfo>> {
        self.sinfo.get(key)
    }

    fn insert(&mut self, key: SelectKey, info: Arc<SelectInfo>) {
        self.sinfo.insert(key, info);
    }
}

impl<S> StoreFilterInfo for RequestInfoStorage<S>
where
    S: BuildHasher,
{
    fn contains(&self, key: &FilterKey) -> bool {
        self.finfo.contains_key(key)
    }

    fn get(&self, key: &FilterKey) -> Option<&Arc<FilterInfo>> {
        self.finfo.get(key)
    }

    fn insert(&mut self, key: FilterKey, info: Arc<FilterInfo>) {
        self.finfo.insert(key, info);
    }
}

/// A single request is about needs for all sorts of components, resources,
/// and entity containers.
/// In other words, a request is a combination of [`Query`]s, [`ResQuery`]s,
/// and queries for entity containers.
/// They must be requested at once in order to prevent dead lock.
/// You can make a request by implementing this trait and put it in a system.
#[allow(private_interfaces, private_bounds)]
pub trait Request: 'static {
    /// Read-only access [`Query`] consisting of [`Filter`]s.
    /// Read-only access helps us execute systems simultaneously.
    ///
    /// [`Filter`]: super::filter::Filter
    type Read: Query;

    /// Writable access [`QueryMut`] consisting of [`Filter`]s.
    /// Writable access always takes exclusive authority over the target.
    ///
    /// [`Filter`]: super::filter::Filter
    type Write: QueryMut;

    /// Read-only access [`ResQuery`] consisting of [`Resource`]s.
    /// Read-only access can help us execute systems simultaneously.
    ///
    /// [`Resource`]: super::super::resource::Resource
    type ResRead: ResQuery;

    /// Writable access [`ResQueryMut`] consisting of [`Resource`]s.
    /// Writable access always takes exclusive authority over the target.
    ///
    /// [`Resource`]: super::super::resource::Resource
    type ResWrite: ResQueryMut;

    /// Writable access [`EntQueryMut`] consisting of [`Entity`].
    /// Writable acess always takes exclusive authority over the target.
    ///
    /// [`Entity`]: super::super::ent::entity::Entity
    type EntWrite: EntQueryMut;

    #[doc(hidden)]
    fn key() -> RequestKey {
        RequestKey::of::<Self>()
    }

    #[doc(hidden)]
    fn get_info_from<S>(stor: &mut S) -> &Arc<RequestInfo>
    where
        S: StoreRequestInfo + ?Sized,
    {
        let key = Self::key();

        if !StoreRequestInfo::contains(stor, &key) {
            let rinfo = Arc::new(Self::info_from(stor));
            StoreRequestInfo::insert(stor, key, rinfo);
        }

        // Safety: Inserted right before.
        unsafe { StoreRequestInfo::get(stor, &key).unwrap_unchecked() }
    }

    #[doc(hidden)]
    fn info_from<S>(stor: &mut S) -> RequestInfo
    where
        S: StoreRequestInfo + ?Sized,
    {
        // TODO: create new()
        RequestInfo {
            name: any::type_name::<Self>(),
            read: (
                Self::Read::key(),
                Arc::clone(Self::Read::get_info_from(stor)),
            ),
            write: (
                Self::Write::key(),
                Arc::clone(Self::Write::get_info_from(stor)),
            ),
            res_read: (
                Self::ResRead::key(),
                Arc::clone(Self::ResRead::get_info_from(stor)),
            ),
            res_write: (
                Self::ResWrite::key(),
                Arc::clone(Self::ResWrite::get_info_from(stor)),
            ),
            ent_write: (
                Self::EntWrite::key(),
                Arc::clone(Self::EntWrite::get_info_from(stor)),
            ),
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

/// This macro declares an empty struct and implements [`Request`] fot it.
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
    fn contains(&self, key: &RequestKey) -> bool;
    fn get(&self, key: &RequestKey) -> Option<&Arc<RequestInfo>>;
    fn insert(&mut self, key: RequestKey, info: Arc<RequestInfo>);
    fn remove(&mut self, key: &RequestKey);
}

/// Unique identifier for a type implementing [`Request`].
pub(crate) type RequestKey = ATypeId<RequestKey_>;
pub(crate) struct RequestKey_;

#[derive(Clone)]
pub(crate) struct RequestInfo {
    read: (QueryKey, Arc<QueryInfo>),
    write: (QueryKey, Arc<QueryInfo>),
    res_read: (ResQueryKey, Arc<ResQueryInfo>),
    res_write: (ResQueryKey, Arc<ResQueryInfo>),
    ent_write: (EntQueryKey, Arc<EntQueryInfo>),
    name: &'static str,
}

impl fmt::Debug for RequestInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RequestInfo")
            .field("name", &self.name())
            .field("read", &self.read())
            .field("write", &self.write())
            .field("res_read", &self.res_read())
            .field("res_write", &self.res_write())
            .field("ent_write", &self.ent_write())
            .finish()
    }
}

impl RequestInfo {
    pub(crate) const fn read(&self) -> &(QueryKey, Arc<QueryInfo>) {
        &self.read
    }

    pub(crate) const fn write(&self) -> &(QueryKey, Arc<QueryInfo>) {
        &self.write
    }

    pub(crate) const fn res_read(&self) -> &(ResQueryKey, Arc<ResQueryInfo>) {
        &self.res_read
    }

    pub(crate) const fn res_write(&self) -> &(ResQueryKey, Arc<ResQueryInfo>) {
        &self.res_write
    }

    pub(crate) const fn ent_write(&self) -> &(EntQueryKey, Arc<EntQueryInfo>) {
        &self.ent_write
    }

    pub(crate) const fn name(&self) -> &'static str {
        self.name
    }

    pub(crate) fn resource_keys(&self) -> impl Iterator<Item = &ResourceKey> {
        let read = self.res_read().1.as_ref();
        let write = self.res_write().1.as_ref();
        read.resource_keys().iter().chain(write.resource_keys())
    }

    pub(crate) fn filters(&self) -> &[(FilterKey, Arc<FilterInfo>)] {
        self.ent_write().1.as_ref().filters()
    }

    /// Determines whether the request info is valid or not in terms of
    /// `Read`, `Write`, `ResRead`, and `ResWrite`.
    /// Request info that meets conditions below is valid.
    /// - Write query selectors are disjoint against other selectors.
    /// - Write resource query doesn't overlap other read or write resource query.
    ///
    /// Note that request info cannot validate `EntWrite` itself.
    /// That must be validated outside.
    pub(crate) fn validate(&self) -> Result<(), String> {
        // 1. Write query contains disjoint selectors only?
        let (_, r_qinfo) = self.read();
        let (_, w_qinfo) = self.write();
        let r_sels = r_qinfo.selectors();
        let w_sels = w_qinfo.selectors();
        for i in 0..w_sels.len() {
            // Doesn't overlap other write?
            for j in i + 1..w_sels.len() {
                if !w_sels[i].1.is_disjoint(&w_sels[j].1) {
                    let errmsg = debug_format!(
                        "`{}` and `{}` are not disjoint in request `{}`",
                        w_sels[i].1.name(),
                        w_sels[j].1.name(),
                        self.name(),
                    );
                    return Err(errmsg);
                }
            }
            // Doesn't overlap read?
            for (_, r_sel) in r_sels.iter() {
                if !w_sels[i].1.is_disjoint(r_sel) {
                    let errmsg = debug_format!(
                        "`{}` and `{}` are not disjoint in request `{}`",
                        w_sels[i].1.name(),
                        r_sel.name(),
                        self.name(),
                    );
                    return Err(errmsg);
                }
            }
        }

        // 2. Write resource query doesn't overlap other resource queries?
        let (_, r_rqinfo) = self.res_read();
        let (_, w_rqinfo) = self.res_write();
        let r_keys = r_rqinfo.resource_keys();
        let w_keys = w_rqinfo.resource_keys();
        for i in 0..w_keys.len() {
            // Doesn't overlap other write?
            for j in i + 1..w_keys.len() {
                if w_keys[i] == w_keys[j] {
                    let errmsg = debug_format!(
                        "duplicate resource query `{:?}` in request `{}`",
                        w_keys[i],
                        self.name(),
                    );
                    return Err(errmsg);
                }
            }
            // Doesn't overlap read?
            for r_key in r_keys.iter() {
                if &w_keys[i] == r_key {
                    let errmsg = debug_format!(
                        "duplicate resource query `{:?}` in request `{}`",
                        w_keys[i],
                        self.name(),
                    );
                    return Err(errmsg);
                }
            }
        }

        Ok(())
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

/// System buffer for its request.
///
/// System request, [`Request`], is composed of requests for read, write,
/// resource read, resource write, and entity write. They are actually pointers
/// to the requesting data. Each requuest means,
/// - Read or write: Read or write requests for specific
///   [`Component`](crate::ecs::ent::component::Component)s.
/// - Resource read or write: Read or write requests for specific
///   [`Resource`](crate::ecs::resource::Resource)s.
/// - Enitty write: Write requests for specific entity containers.
//
// Why buffer for system rather than request?
// Q. Many systems may have the same request, so is they be able to share the
//    same buffer?
// A. Because of borrow check, we need system-individual buffer.
// - We check borrow status, so we need to borrow and release data everytime.
//   * Borrow check helps us avoid running into hidden data race during
//     development.
#[derive(Debug)]
pub struct SystemBuffer {
    /// Buffer for read-only borrowed component arrays for the system's request.
    pub(crate) read: Box<[SelectedRaw]>,

    /// Buffer for writable borrowed component arrays for the system's request.
    pub(crate) write: Box<[SelectedRaw]>,

    /// Buffer for read-only borrowed resources for the system's request.
    pub(crate) res_read: Vec<Borrowed<ManagedConstPtr<u8>>>,

    /// Buffer for writable borrowed resources for the system's request.
    pub(crate) res_write: Vec<Borrowed<ManagedMutPtr<u8>>>,

    /// Buffer for writable borrowed entity container for the system's request.
    pub(crate) ent_write: Box<[FilteredRaw]>,
}

// We're going to send this buffer to other threads with a system implementation.
// So it's needed to be `Send` like `dyn Invoke + Send`.
// Obviously, it includes raw pointers, which are unsafe to be sent.
// But scheduler guarantees there will be no violation.
unsafe impl Send for SystemBuffer {}

impl SystemBuffer {
    pub(crate) fn new() -> Self {
        Self {
            read: [].into(),
            write: [].into(),
            res_read: Vec::new(),
            res_write: Vec::new(),
            ent_write: [].into(),
        }
    }

    pub(crate) fn clear(&mut self) {
        #[cfg(feature = "check")]
        self.clear_force();
    }

    pub(crate) fn clear_force(&mut self) {
        for read in self.read.iter_mut() {
            read.clear();
        }
        for write in self.write.iter_mut() {
            write.clear();
        }
        self.res_read.clear();
        self.res_write.clear();
        for ent_write in self.ent_write.iter_mut() {
            ent_write.clear();
        }
    }
}

impl Default for SystemBuffer {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Response<'buf, Req: Request> {
    pub read: <Req::Read as Query>::Output<'buf>,
    pub write: <Req::Write as QueryMut>::Output<'buf>,
    pub res_read: <Req::ResRead as ResQuery>::Output<'buf>,
    pub res_write: <Req::ResWrite as ResQueryMut>::Output<'buf>,
    pub ent_write: <Req::EntWrite as EntQueryMut>::Output<'buf>,
    _cleaner: BufferCleaner<'buf>,
}

impl<'buf, Req: Request> Response<'buf, Req> {
    pub(crate) fn new(buf: &'buf mut SystemBuffer) -> Self {
        // Safety: Infallible.
        let _cleaner = BufferCleaner {
            buf_ptr: unsafe { NonNull::new_unchecked(buf as *mut _) },
            _marker: PhantomData,
        };

        Self {
            read: <Req::Read as Query>::convert(&mut buf.read),
            write: <Req::Write as QueryMut>::convert(&mut buf.write),
            res_read: <Req::ResRead as ResQuery>::convert(&mut buf.res_read),
            res_write: <Req::ResWrite as ResQueryMut>::convert(&mut buf.res_write),
            ent_write: <Req::EntWrite as EntQueryMut>::convert(&mut buf.ent_write),
            _cleaner,
        }
    }
}

struct BufferCleaner<'buf> {
    buf_ptr: NonNull<SystemBuffer>,
    _marker: PhantomData<&'buf ()>,
}

impl Drop for BufferCleaner<'_> {
    fn drop(&mut self) {
        // Safety: We're actually borrowing `SystemBuffer` via `buf lifetime.
        let buf = unsafe { self.buf_ptr.as_mut() };
        buf.clear();
    }
}
