use super::{
    ent::{
        component::ComponentKey,
        entity::{EntityIndex, EntityKey, EntityKeyKind, EntityTag},
        storage::EntityStorage,
    },
    resource::ResourceStorage,
    sys::{
        filter::{FilterInfo, Filtered},
        query::{EntQueryInfo, QueryInfo, ResQueryInfo},
        request::{RequestBuffer, RequestInfo, RequestKey},
        system::{SystemData, SystemId, SystemInfo},
    },
    wait::{WaitIndices, WaitRetryIndices},
    EcsError, EcsResult,
};
use crate::{ds::prelude::*, util::prelude::*};
use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    hash::BuildHasher,
    ops::{Deref, DerefMut},
    rc::{Rc, Weak},
};

#[derive(Debug)]
pub(super) struct CacheStorage<S> {
    waits: HashMap<RequestKey, Rc<RefCell<WaitIndices>>, S>,

    /// Holds requested and borrowed entities, components, and resources.
    //
    // Cache item must borrow ECS data when it needs that,
    // and then drop the borrowed data when it finished using the data.
    // Because we check the borrow rule using `Borrowed` at run-time to prevent bugs.
    pub(super) items: HashMap<SystemId, CacheItem, S>,

    noti: HashMap<CacheNoti, Vec<(SystemId, Weak<SystemInfo>)>, S>,

    /// Mapping between entities and systems.
    /// Each record means some systems has cached an entity.
    /// So those systems have cache items that contain indices and buffers about the entity.
    //
    // Value type, `Vec<SystemId>`, is kept to sorted and deduplicated.
    ent_imap: HashMap<EntityIndex, Vec<SystemId>, S>,
}

impl<S> CacheStorage<S>
where
    S: Default,
{
    pub(super) fn new() -> Self {
        Self {
            waits: HashMap::default(),
            items: HashMap::default(),
            noti: HashMap::default(),
            ent_imap: HashMap::default(),
        }
    }
}

impl<S> CacheStorage<S>
where
    S: BuildHasher + Default,
{
    /// Caches the system's requests.
    pub(super) fn create(
        &mut self,
        sdata: &SystemData,
        ent: &EntityStorage<S>,
        res: &ResourceStorage<S>,
    ) {
        assert!(!self.items.contains_key(&sdata.id()));

        // TODO: Validation: Conflicts b/w read and write.
        // But, Users may want to insert/remove Entity using EntityContainer
        // while read some columns from the Entity.

        // TODO: Validation: restrict duplicated filter or resource or entity in a single query.

        // Caches read & write queries.
        let noti = &mut self.noti;
        let ent_imap = &mut self.ent_imap;
        let rinfo = sdata.get_request_info();
        let qinfo = rinfo.read().1.as_ref();
        let (wait_read, buf_read) = cache_query(sdata, qinfo, ent, noti, ent_imap);
        let qinfo = rinfo.write().1.as_ref();
        let (wait_write, buf_write) = cache_query(sdata, qinfo, ent, noti, ent_imap);

        // Caches read & write resource queries.
        let rqinfo = rinfo.res_read().1.as_ref();
        let wait_res_read = cache_res_query(rqinfo, res);
        let rqinfo = rinfo.res_write().1.as_ref();
        let wait_res_write = cache_res_query(rqinfo, res);

        // Caches write entity query.
        let eqinfo = rinfo.ent_write().1.as_ref();
        let wait_ent_write = cache_ent_query(sdata, eqinfo, ent, ent_imap);

        // Inserts wait queue indices.
        let rkey = sdata.get_info().request_key();
        let wait = WaitIndices {
            read: wait_read,
            write: wait_write,
            res_read: wait_res_read,
            res_write: wait_res_write,
            ent_write: wait_ent_write,
        };
        let wait = Rc::new(RefCell::new(wait));
        self.waits.insert(rkey, Rc::clone(&wait));

        // Creates and inserts cache item.
        let sid = sdata.id();
        let mut item = CacheItem::new(wait);
        item.buf.read = buf_read;
        item.buf.write = buf_write;
        self.items.insert(sid, item);

        return;

        // === Internal helper functions ===

        /// Creates 'wait indices' and 'buffer for filtered data' for a query.
        ///
        /// - 'Wait indices' are a set of entity and component index pairs.
        ///   We can find specific components requered by the query using them.
        ///   They are a part of [`WaitIndices`].
        ///
        /// - 'Buffer for filtered data' is a set of buffer that hold component data.
        ///   The buffer will eventually be served to a worker.
        ///   They are a part of [`RequestBuffer`].
        ///
        /// Plus, appends some information written below.
        ///
        /// - Adds system id to notification list.
        ///   When a new entity is registered, this system will have a chance
        ///   to update its cache item described above.
        ///
        /// - Adds entity index and system id mapping.
        fn cache_query<S>(
            sdata: &SystemData,
            qinfo: &QueryInfo,
            ent: &EntityStorage<S>,
            noti: &mut HashMap<CacheNoti, Vec<(SystemId, Weak<SystemInfo>)>, S>,
            ent_imap: &mut HashMap<EntityIndex, Vec<SystemId>, S>,
        ) -> (DedupVec<(usize, usize), false>, Box<[Filtered]>)
        where
            S: BuildHasher + Default,
        {
            let mut merged_wait = DedupVec::new();
            let mut merged_filtered = Vec::new();
            for (_, finfo) in qinfo.filters().iter() {
                let (wait, filtered) = FilterUtil::filter_all(ent, finfo);

                // Appends entity-system mapping.
                for ei in wait.iter().map(|(ei, _ci)| ei).cloned() {
                    CacheStorage::add_entity_system_mapping(ei, sdata.id(), ent_imap);
                }

                // Appends system to noti-list.
                noti.entry(CacheNoti::Comp(*finfo.target()))
                    .and_modify(|v| v.push((sdata.id(), sdata.info())))
                    .or_insert(vec![(sdata.id(), sdata.info())]);

                // Gathers `wait indices` and `buffer for filtered data`.
                let wait = wait.into_iter().map(|(ei, ci)| (ei.index(), ci));
                merged_wait.extend(wait);
                merged_filtered.push(filtered);
            }
            (merged_wait, merged_filtered.into_boxed_slice())
        }

        /// Creates 'wait indices' for a resource query.
        ///
        /// - 'Wait indices' are a set of resource indices.
        ///   We can find specific resources requered by the resource query using them.
        ///   They are a part of [`WaitIndices`].
        fn cache_res_query<S>(
            rqinfo: &ResQueryInfo,
            res: &ResourceStorage<S>,
        ) -> DedupVec<usize, false>
        where
            S: BuildHasher + Default,
        {
            // TODO: In here, assumes there is no duplication.
            let mut waits = DedupVec::new();
            for rkey in rqinfo.rkeys() {
                let index = res.get_index(rkey).unwrap();
                waits.push(index);
            }
            waits
        }

        /// Creates 'wait indices' for an entity query.
        ///
        /// - 'Wait indices' are a set of entity indices.
        ///   We can find specific entity contianers requered by the entity query using them.
        ///   They are a part of [`WaitIndices`].
        fn cache_ent_query<S>(
            sdata: &SystemData,
            eqinfo: &EntQueryInfo,
            ent: &EntityStorage<S>,
            ent_imap: &mut HashMap<EntityIndex, Vec<SystemId>, S>,
        ) -> DedupVec<usize, false>
        where
            S: BuildHasher + Default,
        {
            let mut wait = DedupVec::new();
            for ekey in eqinfo.ekeys() {
                let ekey = ent.convert_entity_key(ekey, EntityKeyKind::Index).unwrap();
                let ei = ekey.index();

                // Appends entity-system mapping.
                CacheStorage::add_entity_system_mapping(*ei, sdata.id(), ent_imap);

                wait.push(ei.index());
            }
            wait
        }
    }

    /// Updates cache data for the new entity.
    pub(super) fn update(&mut self, ent: &EntityStorage<S>, ei: EntityIndex) {
        for ckey in ent.get_component_keys(&EntityKey::from(ei)).unwrap() {
            if let Some(noti) = self.noti.get(&CacheNoti::Comp(*ckey)) {
                for (sid, sinfo) in noti {
                    // If the entity-system mapping exists, skips.
                    if Self::has_entity_system_mapping(&ei, sid, &self.ent_imap) {
                        continue;
                    }

                    // Appends entity-system mapping.
                    Self::add_entity_system_mapping(ei, *sid, &mut self.ent_imap);

                    // Updates system's cache for the entity.
                    let sinfo = sinfo.upgrade().unwrap();
                    let rinfo = sinfo.get_request_info();
                    let cache = self.items.get_mut(sid).unwrap();
                    update_read_write_query(ent, ei, rinfo, cache);
                }
            }
        }

        // === Internal helper functions ===

        /// Updates cache's wait indices and buffer for the given entity.
        fn update_read_write_query<S>(
            ent: &EntityStorage<S>,
            ei: EntityIndex,
            rinfo: &RequestInfo,
            cache: &mut CacheItem,
        ) where
            S: BuildHasher + Default,
        {
            let mut borrow_wait = cache.wait.borrow_mut();

            // Read query.
            let qinfo = &rinfo.read().1;
            let wait = &mut borrow_wait.read;
            let buf = &mut cache.buf.read;
            update_query(ent, ei, qinfo, wait, buf);

            // Write query.
            let qinfo = &rinfo.write().1;
            let wait = &mut borrow_wait.write;
            let buf = &mut cache.buf.write;
            update_query(ent, ei, qinfo, wait, buf);
        }

        fn update_query<S>(
            ent: &EntityStorage<S>,
            ei: EntityIndex,
            qinfo: &QueryInfo,
            wait: &mut DedupVec<(usize, usize), false>,
            buf: &mut [Filtered],
        ) where
            S: BuildHasher + Default,
        {
            debug_assert_eq!(qinfo.filters().len(), buf.len());

            for ((_, finfo), filtered) in qinfo.filters().iter().zip(buf.iter_mut()) {
                if let Some((ci, etag)) = FilterUtil::filter(ent, ei, finfo) {
                    wait.push((ei.index(), ci));
                    filtered.add(etag, ci);
                }
            }
        }
    }

    // For future impl.
    #[allow(dead_code)]
    pub(crate) fn remove(&mut self, _sid: &SystemId) -> Option<CacheItem> {
        todo!()
    }

    fn add_entity_system_mapping(
        ei: EntityIndex,
        sid: SystemId,
        ent_imap: &mut HashMap<EntityIndex, Vec<SystemId>, S>,
    ) {
        ent_imap
            .entry(ei)
            .and_modify(|sids| {
                // Keeps sorted and deduplicated state.
                if let Err(i) = sids.binary_search(&sid) {
                    sids.insert(i, sid);
                }
            })
            .or_insert(vec![sid]);
    }

    fn has_entity_system_mapping(
        ei: &EntityIndex,
        sid: &SystemId,
        ent_imap: &HashMap<EntityIndex, Vec<SystemId>, S>,
    ) -> bool {
        if let Some(sids) = ent_imap.get(ei) {
            // `sids` is soreted.
            sids.binary_search(sid).is_ok()
        } else {
            false
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub(super) enum CacheNoti {
    Comp(ComponentKey),
}

struct FilterUtil;
impl FilterUtil {
    /// Filters currently registered entity containers by the given filter.
    fn filter_all<S>(
        ent: &EntityStorage<S>,
        finfo: &FilterInfo,
    ) -> (Vec<(EntityIndex, usize)>, Filtered)
    where
        S: BuildHasher + Default,
    {
        let (wait, (etags, col_idxs)): (Vec<_>, (Vec<_>, Vec<_>)) = ent
            .iter_entity_container()
            .filter_map(|(_, ei, _)| {
                Self::filter(ent, ei, finfo).map(|(ci, etag)| {
                    let wait = (ei, ci);
                    let etag_ci = (etag, ci);
                    (wait, etag_ci)
                })
            })
            .unzip();

        (wait, Filtered::new(etags, col_idxs))
    }

    /// Determines the entity is matched by the filter.
    /// If so, returns matching information.
    fn filter<S>(
        ent: &EntityStorage<S>,
        ei: EntityIndex,
        finfo: &FilterInfo,
    ) -> Option<(usize, EntityTag)>
    where
        S: BuildHasher + Default,
    {
        let ekey = EntityKey::from(ei);
        let cont = ent.get_entity_container(&ekey)?;
        if !finfo.filter(|ckey| cont.contains_column(ckey)) {
            return None;
        }

        // Safety:
        // - ckeys: `ekey` has its container, which means `ekey` can be converted.
        // - etag: Scheduler guarantees arguments won't be dropped or incorrectly aliased
        // while the etag is in use.
        // - ci: It's safe because we've already filtered.
        unsafe {
            let ckeys = ent.get_component_keys(&ekey).unwrap_unchecked();
            let etag = EntityTag::new(ei, cont.name().clone(), ckeys, cont.comp_names());
            let ci = cont.get_column_index(finfo.target()).unwrap_unchecked();
            Some((ci, etag))
        }
    }
}

/// Each system has corresponding cache item.
/// Cache item contains indices to the wait queues for entities, components, and resources.
/// Plus, the item also contains buffer for request data.
#[derive(Debug)]
pub(super) struct CacheItem {
    wait: Rc<RefCell<WaitIndices>>,
    retry: WaitRetryIndices,
    buf: Box<RequestBuffer>,
}

impl CacheItem {
    fn new(wait: Rc<RefCell<WaitIndices>>) -> Self {
        Self {
            wait,
            retry: WaitRetryIndices::new(),
            buf: Box::new(RequestBuffer::new()),
        }
    }

    pub(super) fn get_wait_indices(&self) -> Ref<WaitIndices> {
        self.wait.borrow()
    }

    pub(super) fn get_request_buffer_mut(&mut self) -> &mut RequestBuffer {
        &mut self.buf
    }

    pub(super) fn request_buffer_ptr(&mut self) -> NonNullExt<RequestBuffer> {
        // Safety: Infallible.
        unsafe { NonNullExt::new_unchecked(self.get_request_buffer_mut() as *mut _) }
    }
}

/// Cache storage at a time.
/// This dosen't allow new item to be registered,
/// but you can read or write each item in the cache.
#[derive(Debug)]
pub(super) struct RefreshCacheStorage<'a, S> {
    pub(super) cache: &'a mut HashMap<SystemId, CacheItem, S>,
    pub(super) ent: &'a mut EntityStorage<S>,
    pub(super) res: &'a mut ResourceStorage<S>,
}

impl<'a, S> RefreshCacheStorage<'a, S>
where
    S: BuildHasher,
{
    pub(crate) fn get(&self, sid: &SystemId) -> Option<&CacheItem> {
        self.cache.get(sid)
    }

    pub(crate) fn get_mut(&mut self, sid: &SystemId) -> Option<RefreshCacheItem<S>> {
        self.cache.get_mut(sid).map(|item| RefreshCacheItem {
            inner: item,
            ent: self.ent,
            res: self.res,
        })
    }
}

#[derive(Debug)]
pub(super) struct RefreshCacheItem<'a, S> {
    inner: &'a mut CacheItem,
    ent: &'a mut EntityStorage<S>,
    res: &'a mut ResourceStorage<S>,
}

impl<'a, S> RefreshCacheItem<'a, S>
where
    S: BuildHasher + Default,
{
    pub(super) fn get_wait_retry_indices_mut(
        &mut self,
    ) -> (Ref<WaitIndices>, &mut WaitRetryIndices) {
        (self.inner.wait.borrow(), &mut self.inner.retry)
    }

    /// Refreshes cache item by re-borrowing the data.
    pub(super) fn refresh(self) -> &'a mut CacheItem {
        let Self { inner, ent, res } = self;

        // Updates component buffer.
        for filtered in inner.buf.read.iter_mut() {
            let (etags, col_idxs, query_res) = filtered.take();

            // We call Vec::set_len() instead of Vec::clear() not to call Borrowed::drop() here.
            // When a system finishes its operation, it drops FilteredIter or FilteredIterMut that it received.
            // The iterators drop the items, which are Borrowed, in this vector manually.
            // Therefore, Borrowed is dropped as *soon* as it has been completely used.
            // See FilteredIter::drop() and FilteredIterMut::drop().
            unsafe { query_res.set_len(0) };

            for (ei, ci) in Filtered::iter_index_pair(etags, col_idxs) {
                let cont = ent.get_entity_container(&EntityKey::from(ei)).unwrap();
                let col = cont.borrow_column(ci).unwrap();
                query_res.push(col);
            }
        }
        for filtered in inner.buf.write.iter_mut() {
            let (etags, col_idxs, query_res) = filtered.take();
            unsafe { query_res.set_len(0) };
            for (ei, ci) in Filtered::iter_index_pair(etags, col_idxs) {
                let cont = ent.get_entity_container_mut(&EntityKey::from(ei)).unwrap();
                let col = cont.borrow_column_mut(ci).unwrap();
                query_res.push(col);
            }
        }

        // Updates read resource buffer.
        // The buffer must be consumed by `ResQuery::convert()`.
        debug_assert!(inner.buf.res_read.is_empty());
        for ri in inner.wait.borrow().res_read.iter().cloned() {
            let borrowed = res.borrow(ri).unwrap();
            inner.buf.res_read.push(borrowed);
        }

        // Updates write resource buffer.
        // The buffer must be consumed by `ResQueryMut::convert()`.
        debug_assert!(inner.buf.res_write.is_empty());
        for ri in inner.wait.borrow().res_write.iter().cloned() {
            let borrowed = res.borrow_mut(ri).unwrap();
            inner.buf.res_write.push(borrowed);
        }

        // Updates entity buffer.
        // The buffer must be consumed by `EntQueryMut::convert()`.
        debug_assert!(inner.buf.ent_write.is_empty());
        for ei in inner.wait.borrow().ent_write.iter().cloned() {
            // Safety: We didn't keep the generation of the entity container,
            // But we guarantee that it's fine.
            // TODO: Should we preserve the generation as well?
            let borrowed = unsafe { ent.borrow_unchecked_mut(ei).unwrap() };
            inner.buf.ent_write.push(borrowed);
        }

        inner
    }
}

impl<'a, S> Deref for RefreshCacheItem<'a, S> {
    type Target = CacheItem;

    fn deref(&self) -> &Self::Target {
        self.inner
    }
}

impl<'a, S> DerefMut for RefreshCacheItem<'a, S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.inner
    }
}
