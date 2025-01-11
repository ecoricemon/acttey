use super::{
    ent::{
        component::ComponentKey,
        entity::{EntityIndex, EntityKeyKind, EntityKeyRef, EntityTag},
        storage::EntityStorage,
    },
    resource::{ResourceIndex, ResourceKey, ResourceStorage},
    sys::{
        query::{EntQueryInfo, QueryInfo, ResQueryInfo},
        request::{RequestInfo, SystemBuffer},
        select::{FilterInfo, FilterKey, FilteredRaw, SelectInfo, SelectedRaw},
        system::{SystemData, SystemId, SystemInfo},
    },
    wait::{WaitIndices, WaitRetryIndices},
};
use crate::ds::prelude::*;
use std::{
    cell::{Ref, RefCell},
    collections::{HashMap, HashSet},
    hash::{BuildHasher, Hash},
    ops::{Deref, DerefMut},
    rc::Rc,
    sync::Arc,
};

/// The storage is updated by some events as follows.
///
/// | Events                  | Actions                |
/// | :---                    | :---                   |
/// | System activation       | Creates a cache item   |
/// | System removal          | Removes a cache item   |
/// | Entity registration     | May update cache items |
/// | Entity unregistration   | May update cache items |
/// | Resource registration   | None                   |
/// | Resource unregistration | May remove cache items |
///
/// Do not forget to call proper methods for events.
#[derive(Debug)]
pub(super) struct CacheStorage<S> {
    items: HashMap<SystemId, (CacheItem, Arc<SystemInfo>), S>,
    noti: CacheNotiMap<S>,
}

impl<S> CacheStorage<S>
where
    S: BuildHasher + Default,
{
    pub(super) fn new() -> Self {
        Self {
            items: HashMap::default(),
            noti: CacheNotiMap::new(),
        }
    }

    /// Creates a new cache item for the newly activated system.
    ///
    /// You can call this method before or after you activate the system.
    pub(super) fn create_item(
        &mut self,
        sdata: &SystemData,
        ent_stor: &mut EntityStorage<S>,
        res_stor: &mut ResourceStorage<S>,
    ) {
        assert!(!self.items.contains_key(&sdata.id()));

        // TODO: Validation: Conflicts b/w read and write.
        // But, Users may want to insert/remove Entity using EntityContainer
        // while read some columns from the Entity.

        // TODO: Validation: restrict duplicated filter or resource or entity in a single query.

        // Caches read & write queries.
        let rinfo = sdata.get_request_info();
        let qinfo = rinfo.read().1.as_ref();
        let (wait_read, buf_read) = cache_query(qinfo, ent_stor);
        let qinfo = rinfo.write().1.as_ref();
        let (wait_write, buf_write) = cache_query(qinfo, ent_stor);

        // Caches read & write resource queries.
        let rqinfo = rinfo.res_read().1.as_ref();
        let wait_res_read = cache_res_query(rqinfo, res_stor);
        let rqinfo = rinfo.res_write().1.as_ref();
        let wait_res_write = cache_res_query(rqinfo, res_stor);

        // Caches write entity query.
        let eqinfo = rinfo.ent_write().1.as_ref();
        let (wait_ent_write, buf_ent_write) = cache_ent_query(eqinfo, ent_stor);

        // Inserts wait queue indices.
        let wait = WaitIndices {
            read: wait_read,
            write: wait_write,
            res_read: wait_res_read,
            res_write: wait_res_write,
            ent_write: wait_ent_write,
        };
        let wait = Rc::new(RefCell::new(wait));

        // Creates and inserts cache item.
        let sid = sdata.id();
        let mut item = CacheItem::new(wait);
        item.buf.read = buf_read;
        item.buf.write = buf_write;
        item.buf.ent_write = buf_ent_write;
        item.finish_update(ent_stor, res_stor);
        self.items.insert(sid, (item, sdata.info()));

        // Adds notification for the cache item.
        self.noti.insert(rinfo, sid);

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
        ///   They are a part of [`SystemBuffer`].
        fn cache_query<S>(
            qinfo: &QueryInfo,
            ent_stor: &EntityStorage<S>,
        ) -> (DedupVec<(EntityIndex, usize), false>, Box<[SelectedRaw]>)
        where
            S: BuildHasher + Default,
        {
            let mut merged_wait = DedupVec::new();
            let mut merged_selected = Vec::new();
            for sinfo in qinfo.select_infos() {
                let (wait, selected) = Matcher::collect_selected(ent_stor, sinfo);

                // Gathers `wait indices` and `buffer for selected data`.
                merged_wait.extend(wait);
                merged_selected.push(selected);
            }
            (merged_wait, merged_selected.into_boxed_slice())
        }

        /// Creates 'wait indices' for a resource query.
        ///
        /// - 'Wait indices' are a set of resource indices.
        ///   We can find specific resources requered by the resource query using them.
        ///   They are a part of [`WaitIndices`].
        fn cache_res_query<S>(
            rqinfo: &ResQueryInfo,
            res_stor: &ResourceStorage<S>,
        ) -> DedupVec<ResourceIndex, false>
        where
            S: BuildHasher + Default,
        {
            // TODO: In here, assumes there is no duplication.
            let mut waits = DedupVec::new();
            for rkey in rqinfo.resource_keys() {
                let index = res_stor.index(rkey).unwrap();
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
            eqinfo: &EntQueryInfo,
            ent_stor: &EntityStorage<S>,
        ) -> (DedupVec<EntityIndex, false>, Box<[FilteredRaw]>)
        where
            S: BuildHasher + Default,
        {
            let mut merged_wait = DedupVec::new();
            let mut merged_filtered = Vec::new();
            for (_, finfo) in eqinfo.filters() {
                let (wait, filtered) = Matcher::collect_filtered(ent_stor, finfo);

                // Gathers `wait indices` and `buffer for filtered data`.
                merged_wait.extend(wait);
                merged_filtered.push(filtered);
            }
            (merged_wait, merged_filtered.into_boxed_slice())
        }
    }

    /// Removes system's cache item.
    ///
    /// If the cache item was already removed by other events like resource
    /// unregistration, nothing takes place.
    ///
    /// You can call this method before or after you activate the system.
    pub(crate) fn remove_item(&mut self, sid: SystemId) {
        // Removes cache item.
        if let Some((_item, sinfo)) = self.items.remove(&sid) {
            // Removes notification for the cache item.
            let rinfo = sinfo.get_request_info();
            self.noti.remove(rinfo, sid);
        }
    }

    /// Updates related cache items for the newly registered entity. In this
    /// method, operations below can heppen.
    /// - Read and write queries in cache items can be updated.
    ///
    /// Note that you must call this method after you register the entity.
    pub(super) fn update_by_entity_reg<'r, K>(
        &mut self,
        ekey: K,
        ent_stor: &mut EntityStorage<S>,
        res_stor: &mut ResourceStorage<S>,
    ) where
        K: Into<EntityKeyRef<'r>>,
    {
        inner(self, ekey.into(), ent_stor, res_stor);

        // === Internal helper functions ===

        fn inner<S>(
            this: &mut CacheStorage<S>,
            ekey: EntityKeyRef<'_>,
            ent_stor: &mut EntityStorage<S>,
            res_stor: &mut ResourceStorage<S>,
        ) where
            S: BuildHasher + Default,
        {
            let ei = *ent_stor
                .convert_entity_key(ekey, EntityKeyKind::Index)
                .unwrap()
                .index();

            let mut need_finish: HashSet<SystemId, S> = HashSet::default();
            for ckey in ent_stor.get_component_keys(ekey).unwrap().iter() {
                if let Some(set) = this.noti.get_read(ckey) {
                    for (sid, si) in set.iter() {
                        let (item, sinfo) = this.items.get_mut(sid).unwrap();

                        if try_extend_cache_for_read_write_query(
                            ent_stor,
                            ei,
                            &sinfo.get_request_info().read().1,
                            *si,
                            &mut item.wait.borrow_mut().read,
                            &mut item.buf.read,
                        ) {
                            need_finish.insert(*sid);
                        }
                    }
                }
                if let Some(set) = this.noti.get_write(ckey) {
                    for (sid, si) in set.iter() {
                        let (item, sinfo) = this.items.get_mut(sid).unwrap();

                        if try_extend_cache_for_read_write_query(
                            ent_stor,
                            ei,
                            &sinfo.get_request_info().write().1,
                            *si,
                            &mut item.wait.borrow_mut().write,
                            &mut item.buf.write,
                        ) {
                            need_finish.insert(*sid);
                        }
                    }
                }
            }

            for (_, set) in this.noti.ent_writes() {
                for (sid, fi) in set.iter() {
                    let (item, sinfo) = this.items.get_mut(sid).unwrap();

                    if try_extend_cache_for_entity_write_query(
                        ent_stor,
                        ei,
                        &sinfo.get_request_info().ent_write().1,
                        *fi,
                        &mut item.wait.borrow_mut().ent_write,
                        &mut item.buf.ent_write,
                    ) {
                        need_finish.insert(*sid);
                    }
                }
            }

            for sid in need_finish {
                let (item, _) = this.items.get_mut(&sid).unwrap();
                item.finish_update(ent_stor, res_stor);
            }
        }

        fn try_extend_cache_for_read_write_query<S>(
            ent_stor: &EntityStorage<S>,
            ei: EntityIndex,
            qinfo: &QueryInfo,
            si: usize, // Index to a specific `Select` in a query.
            wait: &mut DedupVec<(EntityIndex, usize), false>,
            buf: &mut [SelectedRaw],
        ) -> bool
        where
            S: BuildHasher + Default,
        {
            debug_assert_eq!(qinfo.select_infos().len(), buf.len());
            debug_assert!(si < buf.len());

            let (_, sinfo) = &qinfo.selectors()[si];
            let selected = &mut buf[si];

            let ekey = EntityKeyRef::from(&ei);
            if let Some((ci, etag)) = Matcher::select(ent_stor, ekey, sinfo) {
                wait.push((ei, ci));
                selected.add(Arc::clone(etag), ci);
                true
            } else {
                false
            }
        }

        fn try_extend_cache_for_entity_write_query<S>(
            ent_stor: &EntityStorage<S>,
            ei: EntityIndex,
            eqinfo: &EntQueryInfo,
            fi: usize, // Index to a specific `Filter` in an entity query.
            wait: &mut DedupVec<EntityIndex, false>,
            buf: &mut [FilteredRaw],
        ) -> bool
        where
            S: BuildHasher + Default,
        {
            debug_assert_eq!(eqinfo.filters().len(), buf.len());
            debug_assert!(fi < buf.len());

            let (_, finfo) = &eqinfo.filters()[fi];
            let filtered = &mut buf[fi];

            let ekey = EntityKeyRef::from(&ei);
            if let Some(etag) = Matcher::filter(ent_stor, ekey, finfo) {
                wait.push(ei);
                filtered.add(Arc::clone(etag));
                true
            } else {
                false
            }
        }
    }

    /// Updates related cache items for the unregistered entity. In this method,
    /// operations below can happen.
    /// - Read and write queries in cache items can be updated.
    /// - Entity write queries in cache items can be updated.
    ///
    /// Note that you must call this method before you unregister the entity.
    pub(super) fn update_by_entity_unreg<'r, K>(
        &mut self,
        ekey: K,
        ent_stor: &mut EntityStorage<S>,
        res_stor: &mut ResourceStorage<S>,
    ) where
        K: Into<EntityKeyRef<'r>>,
    {
        return inner(self, ekey.into(), ent_stor, res_stor);

        // === Internal helper functions ===

        fn inner<S>(
            this: &mut CacheStorage<S>,
            ekey: EntityKeyRef<'_>,
            ent_stor: &mut EntityStorage<S>,
            res_stor: &mut ResourceStorage<S>,
        ) where
            S: BuildHasher + Default,
        {
            let ei = *ent_stor
                .convert_entity_key(ekey, EntityKeyKind::Index)
                .unwrap()
                .index();

            // Updates chche items for the systems that request components
            // belonging to the entity.
            let mut need_finish: HashSet<SystemId, S> = HashSet::default();
            for ckey in ent_stor.get_component_keys(ekey).unwrap().iter() {
                if let Some(set) = this.noti.get_read(ckey) {
                    for (sid, si) in set.iter() {
                        let (item, sinfo) = this.items.get_mut(sid).unwrap();

                        if try_shrink_cache_for_read_write_query(
                            ent_stor,
                            ei,
                            &sinfo.get_request_info().read().1,
                            *si,
                            &mut item.wait.borrow_mut().read,
                            &mut item.buf.read,
                        ) {
                            need_finish.insert(*sid);
                        }
                    }
                }
                if let Some(set) = this.noti.get_write(ckey) {
                    for (sid, si) in set.iter() {
                        let (item, sinfo) = this.items.get_mut(sid).unwrap();

                        if try_shrink_cache_for_read_write_query(
                            ent_stor,
                            ei,
                            &sinfo.get_request_info().write().1,
                            *si,
                            &mut item.wait.borrow_mut().write,
                            &mut item.buf.write,
                        ) {
                            need_finish.insert(*sid);
                        }
                    }
                }
            }

            for (_, set) in this.noti.ent_writes() {
                for (sid, fi) in set.iter() {
                    let (item, sinfo) = this.items.get_mut(sid).unwrap();

                    if try_shrink_cache_for_entity_write_query(
                        ent_stor,
                        ei,
                        &sinfo.get_request_info().ent_write().1,
                        *fi,
                        &mut item.wait.borrow_mut().ent_write,
                        &mut item.buf.ent_write,
                    ) {
                        need_finish.insert(*sid);
                    }
                }
            }

            for sid in need_finish {
                let (item, _) = this.items.get_mut(&sid).unwrap();
                item.finish_update(ent_stor, res_stor);
            }
        }

        fn try_shrink_cache_for_read_write_query<S>(
            ent_stor: &EntityStorage<S>,
            ei: EntityIndex,
            qinfo: &QueryInfo,
            si: usize, // Index to a specific `Select` in a query.
            wait: &mut DedupVec<(EntityIndex, usize), false>,
            buf: &mut [SelectedRaw],
        ) -> bool
        where
            S: BuildHasher + Default,
        {
            debug_assert_eq!(qinfo.select_infos().len(), buf.len());
            debug_assert!(si < buf.len());

            let (_, sinfo) = &qinfo.selectors()[si];
            let selected = &mut buf[si];

            let ekey = EntityKeyRef::from(&ei);
            if let Some((ci, _)) = Matcher::select(ent_stor, ekey, sinfo) {
                let old = wait.remove(&(ei, ci));
                debug_assert_eq!(old, Some((ei, ci)));

                let old = selected.remove(ei, ci);
                debug_assert_eq!(old.unwrap().index(), ei);
                true
            } else {
                false
            }
        }

        fn try_shrink_cache_for_entity_write_query<S>(
            ent_stor: &EntityStorage<S>,
            ei: EntityIndex,
            eqinfo: &EntQueryInfo,
            fi: usize, // Index to a specific `Filter` in an entity query.
            wait: &mut DedupVec<EntityIndex, false>,
            buf: &mut [FilteredRaw],
        ) -> bool
        where
            S: BuildHasher + Default,
        {
            debug_assert_eq!(eqinfo.filters().len(), buf.len());
            debug_assert!(fi < buf.len());

            let (_, finfo) = &eqinfo.filters()[fi];
            let filtered = &mut buf[fi];

            let ekey = EntityKeyRef::from(&ei);
            if Matcher::matches(ent_stor, ekey, finfo) {
                let old = wait.remove(&ei);
                debug_assert_eq!(old, Some(ei));

                let old = filtered.remove(&ei);
                debug_assert_eq!(old.unwrap().index(), ei);
                true
            } else {
                false
            }
        }
    }

    /// Updates related cache items for the unregistered resource. In this
    /// method, operations below can happen.
    /// - Cache items can be removed.
    ///
    /// You can call this method before or after you register the system.
    //
    // Resource registration after system is not allowed. Therefore,
    // 'update_by_resource_reg()' doesn't exist.
    pub(super) fn update_by_resource_unreg<F>(&mut self, rkey: &ResourceKey, mut remove: F)
    where
        F: FnMut(&SystemId),
    {
        let mut sids = Vec::new();

        if let Some(inner_set) = self.noti.get_res_read(rkey) {
            sids.extend(inner_set);
        }
        if let Some(inner_set) = self.noti.get_res_write(rkey) {
            sids.extend(inner_set);
        }

        for sid in sids {
            self.remove_item(sid);
            remove(&sid);
        }
    }
}

#[derive(Debug)]
pub(super) struct CacheNotiMap<S> {
    /// A mapping between systems and their read query targets.
    ///
    /// Key: Target component key that system's read query is requesting.
    /// Value: Pair of system id and index.
    /// - Systems that need to be notified for reg/unreg of the component.
    /// - Index to a specific [`Select`](crate::ecs::sys::select::Select) in a
    ///   read query. For example, in a read query (S0, S1, S2), 0, 1, and 2
    ///   are indices to S0, S1, and S2 respectively.
    read: HashMap<ComponentKey, HashSet<(SystemId, usize), S>, S>,

    /// A mapping between systems and their write query targets.
    ///
    /// Key: Target component key that system's write query is requesting.
    /// Value: Pair of system id and index.
    /// - Systems that need to be notified for reg/unreg of the component.
    /// - Index to a specific [`Select`](crate::ecs::sys::select::Select) in a
    ///   write query. For example, in a write query (S0, S1, S2), 0, 1, and 2
    ///   are indices to S0, S1, and S2 respectively.
    write: HashMap<ComponentKey, HashSet<(SystemId, usize), S>, S>,

    res_read: HashMap<ResourceKey, HashSet<SystemId, S>, S>,
    res_write: HashMap<ResourceKey, HashSet<SystemId, S>, S>,

    #[allow(clippy::type_complexity)]
    ent_write: HashMap<FilterKey, (Arc<FilterInfo>, HashSet<(SystemId, usize), S>), S>,
}

impl<S> CacheNotiMap<S>
where
    S: BuildHasher + Default,
{
    fn new() -> Self {
        Self {
            read: HashMap::default(),
            write: HashMap::default(),
            res_read: HashMap::default(),
            res_write: HashMap::default(),
            ent_write: HashMap::default(),
        }
    }

    pub(super) fn len(&self) -> usize {
        self.read.len()
            + self.write.len()
            + self.res_read.len()
            + self.res_write.len()
            + self.ent_write.len()
    }

    // Allows dead code: Used in test + for future use.
    #[allow(dead_code)]
    pub(super) fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub(super) fn insert(&mut self, rinfo: &RequestInfo, sid: SystemId) {
        // Component notification for system's queries.
        for (i, sinfo) in rinfo.read().1.select_infos().enumerate() {
            self.insert_read(*sinfo.target(), sid, i);
        }
        for (i, sinfo) in rinfo.write().1.select_infos().enumerate() {
            self.insert_write(*sinfo.target(), sid, i);
        }

        // Resource notification for system's resource queries.
        for rkey in rinfo.res_read().1.resource_keys() {
            self.insert_res_read(*rkey, sid);
        }
        for rkey in rinfo.res_write().1.resource_keys() {
            self.insert_res_write(*rkey, sid);
        }

        // Entity notification for system's entity query.
        for (i, (fkey, finfo)) in rinfo.ent_write().1.filters().iter().enumerate() {
            self.insert_ent_write(*fkey, finfo, sid, i);
        }
    }

    pub(super) fn remove(&mut self, rinfo: &RequestInfo, sid: SystemId) {
        // Component notification for system's queries.
        for (i, sinfo) in rinfo.read().1.select_infos().enumerate() {
            self.remove_read(sinfo.target(), sid, i);
        }
        for (i, sinfo) in rinfo.write().1.select_infos().enumerate() {
            self.remove_write(sinfo.target(), sid, i);
        }

        // Resource notification for system's resource queries.
        for rkey in rinfo.res_read().1.resource_keys() {
            self.remove_res_read(rkey, &sid);
        }
        for rkey in rinfo.res_write().1.resource_keys() {
            self.remove_res_write(rkey, &sid);
        }

        // Entity notification for system's entity query.
        for (i, (fkey, _)) in rinfo.ent_write().1.filters().iter().enumerate() {
            self.remove_ent_write(fkey, sid, i);
        }
    }

    pub(super) fn get_read<Q>(&self, key: &Q) -> Option<&HashSet<(SystemId, usize), S>>
    where
        ComponentKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.read.get(key)
    }

    pub(super) fn get_write<Q>(&self, key: &Q) -> Option<&HashSet<(SystemId, usize), S>>
    where
        ComponentKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.write.get(key)
    }

    pub(super) fn get_res_read<Q>(&self, key: &Q) -> Option<&HashSet<SystemId, S>>
    where
        ResourceKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.res_read.get(key)
    }

    pub(super) fn get_res_write<Q>(&self, key: &Q) -> Option<&HashSet<SystemId, S>>
    where
        ResourceKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.res_write.get(key)
    }

    pub(super) fn ent_writes(
        &self,
    ) -> impl Iterator<Item = &(Arc<FilterInfo>, HashSet<(SystemId, usize), S>)> {
        self.ent_write.values()
    }

    fn insert_read(&mut self, key: ComponentKey, sid: SystemId, index: usize) {
        self.read
            .entry(key)
            .and_modify(|set| {
                set.insert((sid, index));
            })
            .or_insert(HashSet::from_iter([(sid, index)]));
    }

    fn insert_write(&mut self, key: ComponentKey, sid: SystemId, index: usize) {
        self.write
            .entry(key)
            .and_modify(|set| {
                set.insert((sid, index));
            })
            .or_insert(HashSet::from_iter([(sid, index)]));
    }

    fn insert_res_read(&mut self, key: ResourceKey, sid: SystemId) {
        self.res_read
            .entry(key)
            .and_modify(|set| {
                set.insert(sid);
            })
            .or_insert(HashSet::from_iter([sid]));
    }

    fn insert_res_write(&mut self, key: ResourceKey, sid: SystemId) {
        self.res_write
            .entry(key)
            .and_modify(|set| {
                set.insert(sid);
            })
            .or_insert(HashSet::from_iter([sid]));
    }

    fn insert_ent_write(
        &mut self,
        key: FilterKey,
        finfo: &Arc<FilterInfo>,
        sid: SystemId,
        index: usize,
    ) {
        self.ent_write
            .entry(key)
            .and_modify(|(_, set)| {
                set.insert((sid, index));
            })
            .or_insert((Arc::clone(finfo), HashSet::from_iter([(sid, index)])));
    }

    fn remove_read<Q>(&mut self, key: &Q, sid: SystemId, index: usize)
    where
        ComponentKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let Some(set) = self.read.get_mut(key) else {
            return;
        };
        set.remove(&(sid, index));
        if set.is_empty() {
            self.read.remove(key);
        }
    }

    fn remove_write<Q>(&mut self, key: &Q, sid: SystemId, index: usize)
    where
        ComponentKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let Some(set) = self.write.get_mut(key) else {
            return;
        };
        set.remove(&(sid, index));
        if set.is_empty() {
            self.write.remove(key);
        }
    }

    fn remove_res_read<Q>(&mut self, key: &Q, sid: &SystemId)
    where
        ResourceKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let Some(set) = self.res_read.get_mut(key) else {
            return;
        };
        set.remove(sid);
        if set.is_empty() {
            self.res_read.remove(key);
        }
    }

    fn remove_res_write<Q>(&mut self, key: &Q, sid: &SystemId)
    where
        ResourceKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let Some(set) = self.res_write.get_mut(key) else {
            return;
        };
        set.remove(sid);
        if set.is_empty() {
            self.res_write.remove(key);
        }
    }

    fn remove_ent_write<Q>(&mut self, key: &Q, sid: SystemId, index: usize)
    where
        FilterKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let Some((_, set)) = self.ent_write.get_mut(key) else {
            return;
        };
        set.remove(&(sid, index));
        if set.is_empty() {
            self.ent_write.remove(key);
        }
    }
}

pub(crate) struct Matcher;
impl Matcher {
    /// Tests if the given selector matches entity containers in the given
    /// entity storage, then collects some data from matched entity containers.
    ///
    /// Return value is composed of,
    /// - Vector of matched entity index and component column index.
    /// - Vector of matched buffer, which is [`SelectedRaw`].
    pub(crate) fn collect_selected<S>(
        ent_stor: &EntityStorage<S>,
        sinfo: &SelectInfo,
    ) -> (Vec<(EntityIndex, usize)>, SelectedRaw)
    where
        S: BuildHasher + Default,
    {
        let (v_ei_ci, (v_etag, v_ci)): (Vec<_>, (Vec<_>, Vec<_>)) = ent_stor
            .iter_entity_container()
            .filter_map(|(_, ei, _)| {
                let ekey = EntityKeyRef::from(&ei);
                Self::select(ent_stor, ekey, sinfo).map(|(ci, etag)| {
                    let ei_ci = (ei, ci);
                    let etag_ci = (Arc::clone(etag), ci);
                    (ei_ci, etag_ci)
                })
            })
            .unzip();

        (v_ei_ci, SelectedRaw::new(v_etag, v_ci))
    }

    pub(crate) fn collect_filtered<S>(
        ent_stor: &EntityStorage<S>,
        finfo: &FilterInfo,
    ) -> (Vec<EntityIndex>, FilteredRaw)
    where
        S: BuildHasher + Default,
    {
        let (v_ei, v_etag): (Vec<_>, Vec<_>) = ent_stor
            .iter_entity_container()
            .filter_map(|(_, ei, _)| {
                let ekey = EntityKeyRef::from(&ei);
                Self::filter(ent_stor, ekey, finfo).map(|etag| (ei, Arc::clone(etag)))
            })
            .unzip();

        (v_ei, FilteredRaw::new(v_etag))
    }

    /// Determines if the given selector matches an entity container specified
    /// by the given entity index, then if they are matched, returns column
    /// index corresponding to selector's target component and entity tag.
    pub(crate) fn select<'s, S>(
        ent_stor: &'s EntityStorage<S>,
        ekey: EntityKeyRef,
        sinfo: &SelectInfo,
    ) -> Option<(usize, &'s Arc<EntityTag>)>
    where
        S: BuildHasher + Default,
    {
        let cont = ent_stor.get_entity_container(ekey)?;
        if sinfo.filter(|ckey| cont.contains_column(ckey), cont.num_columns()) {
            // Safety: Filtered.
            let ci = unsafe { cont.get_column_index(sinfo.target()).unwrap_unchecked() };
            let etag = cont.get_tag();
            Some((ci, etag))
        } else {
            None
        }
    }

    pub(crate) fn filter<'s, S>(
        ent_stor: &'s EntityStorage<S>,
        ekey: EntityKeyRef,
        finfo: &FilterInfo,
    ) -> Option<&'s Arc<EntityTag>>
    where
        S: BuildHasher + Default,
    {
        let cont = ent_stor.get_entity_container(ekey)?;
        if finfo.filter(|ckey| cont.contains_column(ckey), cont.num_columns()) {
            Some(cont.get_tag())
        } else {
            None
        }
    }

    #[cfg(test)]
    pub(crate) fn iter_matched_entity_indices<'s, S>(
        ent_stor: &'s EntityStorage<S>,
        finfo: &'s FilterInfo,
    ) -> impl Iterator<Item = EntityIndex> + 's
    where
        S: BuildHasher + Default,
    {
        ent_stor
            .iter_entity_container()
            .filter_map(|(_, ei, cont)| {
                finfo
                    .filter(|ckey| cont.contains_column(ckey), cont.num_columns())
                    .then_some(ei)
            })
    }

    /// Determines if the given filter matches an entity container specified
    /// by the given entity index.
    pub(crate) fn matches<S>(
        ent_stor: &EntityStorage<S>,
        ekey: EntityKeyRef,
        finfo: &FilterInfo,
    ) -> bool
    where
        S: BuildHasher + Default,
    {
        if let Some(cont) = ent_stor.get_entity_container(ekey) {
            finfo.filter(|ckey| cont.contains_column(ckey), cont.num_columns())
        } else {
            false
        }
    }
}

/// Each system has a dedicated cache item.
/// Cache item contains indices to the wait queues for entities, components, and resources.
/// Plus, the item also contains buffer for request data.
#[derive(Debug)]
pub(super) struct CacheItem {
    wait: Rc<RefCell<WaitIndices>>,
    retry: WaitRetryIndices,
    buf: Box<SystemBuffer>,
}

impl CacheItem {
    fn new(wait: Rc<RefCell<WaitIndices>>) -> Self {
        Self {
            wait,
            retry: WaitRetryIndices::new(),
            buf: Box::new(SystemBuffer::new()),
        }
    }

    pub(super) fn get_wait_indices(&self) -> Ref<WaitIndices> {
        self.wait.borrow()
    }

    pub(super) fn get_request_buffer_mut(&mut self) -> &mut SystemBuffer {
        &mut self.buf
    }

    pub(super) fn request_buffer_ptr(&mut self) -> NonNullExt<SystemBuffer> {
        // Safety: Infallible.
        unsafe { NonNullExt::new_unchecked(self.get_request_buffer_mut() as *mut _) }
    }

    fn refresh<S>(&mut self, _ent_stor: &mut EntityStorage<S>, _res_stor: &mut ResourceStorage<S>)
    where
        S: BuildHasher + Default,
    {
        #[cfg(feature = "check")]
        self._refresh(_ent_stor, _res_stor);
    }

    fn finish_update<S>(
        &mut self,
        _ent_stor: &mut EntityStorage<S>,
        _res_stor: &mut ResourceStorage<S>,
    ) where
        S: BuildHasher + Default,
    {
        #[cfg(not(feature = "check"))]
        {
            self.buf.clear_force();
            self._refresh(_ent_stor, _res_stor);
        }
    }

    fn _refresh<S>(&mut self, ent_stor: &mut EntityStorage<S>, res_stor: &mut ResourceStorage<S>)
    where
        S: BuildHasher + Default,
    {
        // Updates component buffer.
        for selected in self.buf.read.iter_mut() {
            let (etags, col_idxs, query_res) = selected.take();

            // The buffer must be consumed in advance.
            debug_assert!(query_res.is_empty());
            for (ei, ci) in SelectedRaw::iter_index_pair(etags, col_idxs) {
                let cont = ent_stor
                    .get_entity_container(EntityKeyRef::Index(&ei))
                    .unwrap();
                let col = cont.borrow_column(ci).unwrap();
                query_res.push(col);
            }
        }
        for selected in self.buf.write.iter_mut() {
            let (etags, col_idxs, query_res) = selected.take();

            // The buffer must be consumed in advance.
            debug_assert!(query_res.is_empty());
            for (ei, ci) in SelectedRaw::iter_index_pair(etags, col_idxs) {
                let cont = ent_stor
                    .get_entity_container_mut(EntityKeyRef::Index(&ei))
                    .unwrap();
                let col = cont.borrow_column_mut(ci).unwrap();
                query_res.push(col);
            }
        }

        // Updates read resource buffer.
        // The buffer must be consumed in advance.
        debug_assert!(self.buf.res_read.is_empty());
        for ri in self.wait.borrow().res_read.iter().cloned() {
            let borrowed = res_stor.borrow(ri).unwrap();
            self.buf.res_read.push(borrowed);
        }

        // Updates write resource buffer.
        // The buffer must be consumed in advance.
        debug_assert!(self.buf.res_write.is_empty());
        for ri in self.wait.borrow().res_write.iter().cloned() {
            let borrowed = res_stor.borrow_mut(ri).unwrap();
            self.buf.res_write.push(borrowed);
        }

        // Updates entity buffer.
        for filtered in self.buf.ent_write.iter_mut() {
            let (etags, query_res) = filtered.take();

            // The buffer must be consumed in advance.
            debug_assert!(query_res.is_empty());
            for ei in etags.iter().map(|etag| etag.index()) {
                let cont = ent_stor
                    .get_entity_container(EntityKeyRef::Index(&ei))
                    .unwrap();
                let cont_ptr = cont.borrow().unwrap();
                query_res.push(cont_ptr);
            }
        }
    }
}

/// Cache storage at a time.
/// This dosen't allow new item to be registered,
/// but you can read or write each item in the cache.
#[derive(Debug)]
pub(super) struct RefreshCacheStorage<'a, S> {
    pub(super) cache_stor: &'a mut CacheStorage<S>,
    pub(super) ent_stor: &'a mut EntityStorage<S>,
    pub(super) res_stor: &'a mut ResourceStorage<S>,
}

impl<'a, S> RefreshCacheStorage<'a, S>
where
    S: BuildHasher,
{
    pub(crate) fn new(
        cache_stor: &'a mut CacheStorage<S>,
        ent_stor: &'a mut EntityStorage<S>,
        res_stor: &'a mut ResourceStorage<S>,
    ) -> Self {
        Self {
            cache_stor,
            ent_stor,
            res_stor,
        }
    }

    pub(crate) fn get(&self, sid: &SystemId) -> Option<&CacheItem> {
        self.cache_stor.items.get(sid).map(|(item, _sinfo)| item)
    }

    pub(crate) fn get_mut(&mut self, sid: &SystemId) -> Option<RefreshCacheItem<S>> {
        self.cache_stor
            .items
            .get_mut(sid)
            .map(|(item, _sinfo)| RefreshCacheItem {
                item,
                ent_stor: self.ent_stor,
                res_stor: self.res_stor,
            })
    }
}

#[derive(Debug)]
pub(super) struct RefreshCacheItem<'a, S> {
    item: &'a mut CacheItem,
    ent_stor: &'a mut EntityStorage<S>,
    res_stor: &'a mut ResourceStorage<S>,
}

impl<'a, S> RefreshCacheItem<'a, S>
where
    S: BuildHasher + Default,
{
    pub(super) fn get_wait_retry_indices_mut(
        &mut self,
    ) -> (Ref<WaitIndices>, &mut WaitRetryIndices) {
        (self.item.wait.borrow(), &mut self.item.retry)
    }

    /// Refreshes cache item by re-borrowing the data.
    pub(super) fn refresh(self) -> &'a mut CacheItem {
        let Self {
            item,
            ent_stor,
            res_stor,
        } = self;
        item.refresh(ent_stor, res_stor);
        item
    }
}

impl<S> Deref for RefreshCacheItem<'_, S> {
    type Target = CacheItem;

    fn deref(&self) -> &Self::Target {
        self.item
    }
}

impl<S> DerefMut for RefreshCacheItem<'_, S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.item
    }
}

#[cfg(test)]
#[allow(dead_code, non_camel_case_types)]
#[rustfmt::skip]
mod tests {
    use super::*;
    use crate as my_ecs;
    use crate::prelude::*;
    use std::hash::RandomState;

    // To test if addresses are cached correctly, declares structs including 
    // non-ZST types.

    // Components.
    #[derive(Component)] struct Empty;
    #[derive(Component)] struct C0(i32);
    #[derive(Component)] struct C1(i32);
    #[derive(Component)] struct C2(i32);

    // Entities.
    #[derive(Entity)] struct E0_C0 { c: C0 }
    #[derive(Entity)] struct E1_C0 { c: C0, x: Empty }
    #[derive(Entity)] struct E2_C1 { c: C1 }
    #[derive(Entity)] struct E3_C1 { c: C1, x: Empty }
    #[derive(Entity)] struct E4_C0C1 { c0: C0, c1: C1 }
    #[derive(Entity)] struct E5_C2 { c: C2 }

    // Selectors and filters.
    filter!(S0, Target = C0);
    filter!(S1, Target = C1);
    filter!(S2, Target = C2);
    filter!(F0, All = C0);
    filter!(F1, All = C1);

    // Resources.
    #[derive(Resource)] struct R0(i32);
    #[derive(Resource)] struct R1(i32);

    #[test]
    fn test_cachestorage_update() {
        let mut cache_stor = CacheStorage::<RandomState>::new();
        let mut ent_stor = EntityStorage::<RandomState>::new();
        let mut res_stor = ResourceStorage::<RandomState>::new();

        validate_cache_read_update(&mut cache_stor, &mut ent_stor, &mut res_stor);
        validate_cache_write_update(&mut cache_stor, &mut ent_stor, &mut res_stor);
        validate_cache_res_read_update(&mut cache_stor, &mut ent_stor, &mut res_stor);
        validate_cache_res_write_update(&mut cache_stor, &mut ent_stor, &mut res_stor);
        validate_cache_ent_write_update(&mut cache_stor, &mut ent_stor, &mut res_stor);
        validate_cache_mixed_update(&mut cache_stor, &mut ent_stor, &mut res_stor);
    }

    // Registers/Unregisters entities and sees if the cache item will be updated
    // as expected.
    fn validate_cache_read_update<S>(
        cache_stor: &mut CacheStorage<S>,
        ent_stor: &mut EntityStorage<S>,
        res_stor: &mut ResourceStorage<S>,
    ) where
        S: BuildHasher + Default + Clone + 'static,
    {
        // 1. Add E0_C0: Cached 1 C0 by S0
        // 2. Add E1_C0: Cached 2 C0 by S0
        // 3. Add E2_C1: Cached 2 C0 by S0 + 1 C1 by S1
        // 4. Add E3_C1: Cached 2 C0 by S0 + 2 C1 by S1
        // 5. Del E0_C0: Cached 1 C0 by S0 + 2 C1 by S1
        // 6. Del E1_C0: Cached 2 C1 by S1
        // 7. Del E2_C1: Cached 1 C1 by S1
        // 8. Del E3_C1: None
        // 9. Add/Del E5_C2: None

        let sys = FnSystem::from(|_: Read<(S0, S1)>| {});
        let sdata = sys.into_data();
        cache_stor.create_item(&sdata, ent_stor, res_stor);

        validate_entity_reg::<E0_C0, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[1, 0], &[], 0, 0, &[])
        );
        validate_entity_reg::<E1_C0, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[2, 0], &[], 0, 0, &[])
        );
        validate_entity_reg::<E2_C1, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[2, 1], &[], 0, 0, &[])
        );
        validate_entity_reg::<E3_C1, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[2, 2], &[], 0, 0, &[])
        );
        validate_entity_unreg::<E0_C0, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[1, 2], &[], 0, 0, &[])
        );
        validate_entity_unreg::<E1_C0, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[0, 2], &[], 0, 0, &[])
        );
        validate_entity_unreg::<E2_C1, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[0, 1], &[], 0, 0, &[])
        );
        validate_entity_unreg::<E3_C1, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[0, 0], &[], 0, 0, &[])
        );
        validate_entity_reg::<E5_C2, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[0, 0], &[], 0, 0, &[])
        );
        validate_entity_unreg::<E5_C2, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[0, 0], &[], 0, 0, &[])
        );

        cache_stor.remove_item(sdata.id());
        assert!(cache_stor.items.is_empty());
        assert!(cache_stor.noti.is_empty());
    }

    // Registers/Unregisters entities and sees if the cache item will be updated
    // as expected.
    fn validate_cache_write_update<S>(
        cache_stor: &mut CacheStorage<S>,
        ent_stor: &mut EntityStorage<S>,
        res_stor: &mut ResourceStorage<S>,
    ) where
        S: BuildHasher + Default + Clone + 'static,
    {
        // 1. Add E0_C0: Cached 1 C0 by S0
        // 2. Add E1_C0: Cached 2 C0 by S0
        // 3. Add E2_C1: Cached 2 C0 by S0 + 1 C1 by S1
        // 4. Add E3_C1: Cached 2 C0 by S0 + 2 C1 by S1
        // 5. Del E0_C0: Cached 1 C0 by S0 + 2 C1 by S1
        // 6. Del E1_C0: Cached 2 C1 by S1
        // 7. Del E2_C1: Cached 1 C1 by S1
        // 8. Del E3_C1: None
        // 9. Add/Del E5_C2: None

        let sys = FnSystem::from(|_: Write<(S0, S1)>| {});
        let sdata = sys.into_data();
        cache_stor.create_item(&sdata, ent_stor, res_stor);

        validate_entity_reg::<E0_C0, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[], &[1, 0], 0, 0, &[])
        );
        validate_entity_reg::<E1_C0, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[], &[2, 0], 0, 0, &[])
        );
        validate_entity_reg::<E2_C1, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[], &[2, 1], 0, 0, &[])
        );
        validate_entity_reg::<E3_C1, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[], &[2, 2], 0, 0, &[])
        );
        validate_entity_unreg::<E0_C0, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[], &[1, 2], 0, 0, &[])
        );
        validate_entity_unreg::<E1_C0, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[], &[0, 2], 0, 0, &[])
        );
        validate_entity_unreg::<E2_C1, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[], &[0, 1], 0, 0, &[])
        );
        validate_entity_unreg::<E3_C1, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[], &[0, 0], 0, 0, &[])
        );
        validate_entity_reg::<E5_C2, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[], &[0, 0], 0, 0, &[])
        );
        validate_entity_unreg::<E5_C2, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[], &[0, 0], 0, 0, &[])
        );

        cache_stor.remove_item(sdata.id());
        assert!(cache_stor.items.is_empty());
        assert!(cache_stor.noti.is_empty());
    }

    fn validate_cache_res_read_update<S>(
        cache_stor: &mut CacheStorage<S>,
        ent_stor: &mut EntityStorage<S>,
        res_stor: &mut ResourceStorage<S>,
    ) where
        S: BuildHasher + Default + Clone + 'static,
    {
        // 1. With R0 and R1, Del R0: Removed item
        // 2. With R0 and R1, Del R1: Removed item

        // Res: None -> R0, R1
        // Cache: None
        res_stor.register(ResourceDesc::new().with_owned(R0(0))).unwrap();
        res_stor.register(ResourceDesc::new().with_owned(R1(0))).unwrap();
        let sys = FnSystem::from(|_: ResRead<(R0, R1)>| {});
        let sdata = sys.into_data();

        // 1. With R0 and R1, Del R0: Removed item
        // Res: R0, R1 -> R1
        // Cache: None -> Sys -> None
        cache_stor.create_item(&sdata, ent_stor, res_stor);
        cache_stor.update_by_resource_unreg(&ResourceKey::of::<R0>(), |_| {});
        res_stor.unregister(&ResourceKey::of::<R0>()).unwrap();
        assert!(cache_stor.items.is_empty());
        assert!(cache_stor.noti.is_empty());

        // 2. With R0 and R1, Del R1: Removed item
        // Res: R1 -> R0, R1 -> R0
        // Cache: None -> Sys -> None
        res_stor.register(ResourceDesc::new().with_owned(R0(0))).unwrap();
        cache_stor.create_item(&sdata, ent_stor, res_stor);
        cache_stor.update_by_resource_unreg(&ResourceKey::of::<R1>(), |_| {});
        res_stor.unregister(&ResourceKey::of::<R1>()).unwrap();
        assert!(cache_stor.items.is_empty());
        assert!(cache_stor.noti.is_empty());

        // Clean up
        res_stor.unregister(&ResourceKey::of::<R0>());
        res_stor.unregister(&ResourceKey::of::<R1>());
        cache_stor.remove_item(sdata.id());
        assert!(cache_stor.items.is_empty());
        assert!(cache_stor.noti.is_empty());
    }

    fn validate_cache_res_write_update<S>(
        cache_stor: &mut CacheStorage<S>,
        ent_stor: &mut EntityStorage<S>,
        res_stor: &mut ResourceStorage<S>,
    ) where
        S: BuildHasher + Default + Clone + 'static,
    {
        // 1. With R0 and R1, Del R0: Removed item
        // 2. With R0 and R1, Del R1: Removed item

        // Res: None -> R0, R1
        // Cache: None
        res_stor.register(ResourceDesc::new().with_owned(R0(0))).unwrap();
        res_stor.register(ResourceDesc::new().with_owned(R1(0))).unwrap();
        let sys = FnSystem::from(|_: ResWrite<(R0, R1)>| {});
        let sdata = sys.into_data();

        // 1. With R0 and R1, Del R0: Removed item
        // Res: R0, R1 -> R1
        // Cache: None -> Sys -> None
        cache_stor.create_item(&sdata, ent_stor, res_stor);
        cache_stor.update_by_resource_unreg(&ResourceKey::of::<R0>(), |_| {});
        res_stor.unregister(&ResourceKey::of::<R0>()).unwrap();
        assert!(cache_stor.items.is_empty());
        assert!(cache_stor.noti.is_empty());

        // 2. With R0 and R1, Del R1: Removed item
        // Res: R1 -> R0, R1 -> R0
        // Cache: None -> Sys -> None
        res_stor.register(ResourceDesc::new().with_owned(R0(0))).unwrap();
        cache_stor.create_item(&sdata, ent_stor, res_stor);
        cache_stor.update_by_resource_unreg(&ResourceKey::of::<R1>(), |_| {});
        res_stor.unregister(&ResourceKey::of::<R1>()).unwrap();
        assert!(cache_stor.items.is_empty());
        assert!(cache_stor.noti.is_empty());

        // Clean up
        res_stor.unregister(&ResourceKey::of::<R0>());
        res_stor.unregister(&ResourceKey::of::<R1>());
        cache_stor.remove_item(sdata.id());
        assert!(cache_stor.items.is_empty());
        assert!(cache_stor.noti.is_empty());
    }

    fn validate_cache_ent_write_update<S>(
        cache_stor: &mut CacheStorage<S>,
        ent_stor: &mut EntityStorage<S>,
        res_stor: &mut ResourceStorage<S>,
    ) where
        S: BuildHasher + Default + Clone + 'static,
    {
        // 1. Add E0_C0: Cached 1 by F0
        // 2. Add E1_C0: Cached 2 by F0
        // 3. Add E2_C1: Cached 2 by F0 + 1 by F1
        // 4. Add E3_C1: Cached 2 by F0 + 2 by F2
        // 5. Del E0_C0: Cached 1 by F0 + 2 by F2
        // 6. Del E1_C0: Cached 2 by F2
        // 7. Del E2_C1: Cached 1 by F2
        // 8. Del E3_C1: None
        // 9. Add/Del E5_C2: None

        let sys = FnSystem::from(|_: EntWrite<(F0, F1)>| {});
        let sdata = sys.into_data();
        cache_stor.create_item(&sdata, ent_stor, res_stor);

        validate_entity_reg::<E0_C0, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[], &[], 0, 0, &[1, 0])
        );
        validate_entity_reg::<E1_C0, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[], &[], 0, 0, &[2, 0])
        );
        validate_entity_reg::<E2_C1, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[], &[], 0, 0, &[2, 1])
        );
        validate_entity_reg::<E3_C1, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[], &[], 0, 0, &[2, 2])
        );
        validate_entity_unreg::<E0_C0, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[], &[], 0, 0, &[1, 2])
        );
        validate_entity_unreg::<E1_C0, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[], &[], 0, 0, &[0, 2])
        );
        validate_entity_unreg::<E2_C1, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[], &[], 0, 0, &[0, 1])
        );
        validate_entity_unreg::<E3_C1, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[], &[], 0, 0, &[0, 0])
        );
        validate_entity_reg::<E5_C2, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[], &[], 0, 0, &[0, 0])
        );
        validate_entity_unreg::<E5_C2, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[], &[], 0, 0, &[0, 0])
        );

        cache_stor.remove_item(sdata.id());
        assert!(cache_stor.items.is_empty());
        assert!(cache_stor.noti.is_empty());
    }

    fn validate_cache_mixed_update<S>(
        cache_stor: &mut CacheStorage<S>,
        ent_stor: &mut EntityStorage<S>,
        res_stor: &mut ResourceStorage<S>,
    ) where
        S: BuildHasher + Default + Clone + 'static,
    {
        res_stor.register(ResourceDesc::new().with_owned(R0(0))).unwrap();
        res_stor.register(ResourceDesc::new().with_owned(R1(0))).unwrap();
        ent_stor.register(E5_C2::as_entity_descriptor()).unwrap();

        let sys = FnSystem::from(|
            _: Read<S0>, 
            _: Write<S1>,
            _: ResRead<R0>,
            _: ResWrite<R1>,
            _: EntWrite<E5_C2>| {}
        );
        let sdata = sys.into_data();
        cache_stor.create_item(&sdata, ent_stor, res_stor);

        validate_entity_reg::<E4_C0C1, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[1], &[1], 1, 1, &[1])
        );
        validate_entity_unreg::<E4_C0C1, S>(
            &sdata, cache_stor, ent_stor, res_stor, (&[0], &[0], 1, 1, &[1])
        );

        cache_stor.remove_item(sdata.id());
        assert!(cache_stor.items.is_empty());
        assert!(cache_stor.noti.is_empty());
    }

    fn validate_entity_reg<E, S>(
        sdata: &SystemData,
        cache_stor: &mut CacheStorage<S>,
        ent_stor: &mut EntityStorage<S>,
        res_stor: &mut ResourceStorage<S>,
        expect_len: (&[usize], &[usize], usize, usize, &[usize]),
    ) where
        E: AsEntityReg,
        S: BuildHasher + Default + Clone + 'static,
    {
        // Register entity and then updates cache.
        let ei = ent_stor.register(E::as_entity_descriptor()).unwrap();
        cache_stor.update_by_entity_reg(EntityKeyRef::Index(&ei), ent_stor, res_stor);

        // Validates.
        validate_item(&sdata, cache_stor, ent_stor, res_stor, expect_len);
    }

    fn validate_entity_unreg<E, S>(
        sdata: &SystemData,
        cache_stor: &mut CacheStorage<S>,
        ent_stor: &mut EntityStorage<S>,
        res_stor: &mut ResourceStorage<S>,
        expect_len: (&[usize], &[usize], usize, usize, &[usize]),
    ) where
        E: Entity + AsEntityReg,
        S: BuildHasher + Default + Clone + 'static,
    {
        // Update cache and then register entity.
        cache_stor.update_by_entity_unreg(&E::key(), ent_stor, res_stor);
        ent_stor.unregister(&E::key()).unwrap();

        // Validates.
        validate_item(&sdata, cache_stor, ent_stor, res_stor, expect_len);
    }

    fn validate_item<S>(
        sdata: &SystemData,
        cache_stor: &mut CacheStorage<S>,
        ent_stor: &mut EntityStorage<S>,
        res_stor: &mut ResourceStorage<S>,
        expect_len: (&[usize], &[usize], usize, usize, &[usize]),
    ) where
        S: BuildHasher + Default + Clone + 'static,
    {
        let (item, _sinfo) = cache_stor.items.get_mut(&sdata.id()).unwrap();
        item.refresh(ent_stor, res_stor);
        validate_len(item, expect_len);
        validate_item_inner(item, &sdata.get_request_info(), ent_stor, res_stor);
        item.buf.clear();
    }

    fn validate_len(item: &CacheItem, expect_len: (&[usize], &[usize], usize, usize, &[usize])) {
        let validate_query = |buf: &[SelectedRaw], exp_lens: &[usize]| {
            assert_eq!(buf.len(), exp_lens.len());
            for (selected, &exp_len) in buf.iter().zip(exp_lens) {
                assert_eq!(selected.query_res().len(), exp_len);
            }
        };

        let validate_ent_query = |buf: &[FilteredRaw], exp_lens: &[usize]| {
            assert_eq!(buf.len(), exp_lens.len());
            for (filtered, &exp_len) in buf.iter().zip(exp_lens) {
                assert_eq!(filtered.query_res().len(), exp_len);
            }
        };

        validate_query(&item.buf.read, expect_len.0);
        validate_query(&item.buf.write, expect_len.1);
        assert_eq!(item.buf.res_read.len(), expect_len.2);
        assert_eq!(item.buf.res_write.len(), expect_len.3);
        validate_ent_query(&item.buf.ent_write, expect_len.4);
    }

    /// Validates cache item with respect to things described below.
    /// - Does it have valid wait indices?
    /// - Does it have valid buffer addresses?
    fn validate_item_inner<S>(
        item: &CacheItem,
        rinfo: &RequestInfo,
        ent_stor: &EntityStorage<S>,
        res_stor: &ResourceStorage<S>,
    ) where
        S: BuildHasher + Default + Clone + 'static,
    {
        let sys_idxs = SystemIndices::new(rinfo, ent_stor, res_stor);
        validate_wait_indices(&item.wait.borrow(), &sys_idxs);
        validate_buffer_addresses(&item.buf, &sys_idxs, ent_stor, res_stor);
    }

    /// Checks whether or not wait indices are the same as indices gotten from
    /// storages.
    fn validate_wait_indices(waits: &WaitIndices, sys_idxs: &SystemIndices) {
        let WaitIndices {
            read: wait_read,
            write: wait_write,
            res_read: wait_res_read,
            res_write: wait_res_write,
            ent_write: wait_ent_write,
        } = waits;
        let SystemIndices {
            read: sys_read,
            write: sys_write,
            res_read: sys_res_read,
            res_write: sys_res_write,
            ent_write: sys_ent_write,
        } = sys_idxs;

        validate_query(wait_read.iter().cloned(), sys_read.iter().flatten().cloned());
        validate_query(wait_write.iter().cloned(), sys_write.iter().flatten().cloned());
        validate_res_query(wait_res_read.iter().cloned(), sys_res_read.iter().cloned());
        validate_res_query(wait_res_write.iter().cloned(), sys_res_write.iter().cloned());
        validate_ent_query(wait_ent_write.iter().cloned(), sys_ent_write.iter().flatten().cloned());

        // === Internal helper functions ===

        fn validate_query<Iw, Is>(wait: Iw, sys: Is)
        where
            Iw: Iterator<Item = (EntityIndex, usize)>,
            Is: Iterator<Item = (EntityIndex, usize)>,
        {
            let mut wait = wait.collect::<Vec<_>>();
            let mut sys = sys.collect::<Vec<_>>();

            // - Wait indices are basically deduplicated, but not sorted.
            // - Indices we got for testing are neither sorted nor deduplicated.
            wait.sort_unstable();
            sys.sort_unstable();
            sys.dedup();
            assert_eq!(wait, sys);
        }

        fn validate_res_query<Iw, Is>(wait: Iw, sys: Is) 
        where
            Iw: Iterator<Item = ResourceIndex>,
            Is: Iterator<Item = ResourceIndex>,
        {
            let mut wait = wait.collect::<Vec<_>>();
            let mut sys = sys.collect::<Vec<_>>();

            // # Indices for resource and entity queries cannot be duplicated.
            // - Wait indices are not sorted.
            // - Indices we got for testing are not sorted.
            wait.sort_unstable();
            sys.sort_unstable();
            assert_eq!(wait, sys);
        }

        fn validate_ent_query<Iw, Is>(wait: Iw, sys: Is) 
        where
            Iw: Iterator<Item = EntityIndex>,
            Is: Iterator<Item = EntityIndex>,
        {
            let mut wait = wait.collect::<Vec<_>>();
            let mut sys = sys.collect::<Vec<_>>();

            // # Indices for resource and entity queries cannot be duplicated.
            // - Wait indices are not sorted.
            // - Indices we got for testing are not sorted.
            wait.sort_unstable();
            sys.sort_unstable();
            assert_eq!(wait, sys);
        }
    }

    fn validate_buffer_addresses<S>(
        buf: &SystemBuffer,
        sys_idxs: &SystemIndices,
        ent_stor: &EntityStorage<S>,
        res_stor: &ResourceStorage<S>,
    ) where
        S: BuildHasher + Default + Clone + 'static,
    {
        let SystemBuffer {
            read: buf_read,
            write: buf_write,
            res_read: buf_res_read,
            res_write: buf_res_write,
            ent_write: buf_ent_write,
        } = buf;
        let SystemIndices {
            read: sys_read,
            write: sys_write,
            res_read: sys_res_read,
            res_write: sys_res_write,
            ent_write: sys_ent_write,
        } = sys_idxs;

        let validate_query = |buf: &[SelectedRaw], sys: &[Vec<(EntityIndex, usize)>]| {
            assert_eq!(buf.len(), sys.len());
            for (buf_selected, sys_pairs) in buf.iter().zip(sys) {
                let buf_getters = buf_selected.query_res();
                assert_eq!(buf_getters.len(), sys_pairs.len());

                // Note that buf_getters need to be sorted. See example below.
                // Query: (Sa, ...)
                // Sa -> (E0-C0), (E1-C0), ... : OK.
                // Sa -> (E1-C0), (E0-C0), ... : Also OK.

                let mut buf_ptrs = buf_getters
                    .iter()
                    .map(|getter| getter.ptr())
                    .collect::<Vec<_>>();

                let mut sys_ptrs = sys_pairs
                    .iter()
                    .map(|(ei, ci)| unsafe {
                        let cont = ent_stor.get_ptr(ei).unwrap().as_ref();
                        cont.get_column(*ci).unwrap()
                    })
                    .collect::<Vec<_>>();

                buf_ptrs.sort_unstable();
                sys_ptrs.sort_unstable();

                assert_eq!(buf_ptrs, sys_ptrs);
            }
        };

        // Validates read and write queries.
        validate_query(buf_read, sys_read);
        validate_query(buf_write, sys_write);

        // Validates resource read query.
        assert_eq!(buf_res_read.len(), sys_res_read.len());
        for (buf_ptr, ri) in buf_res_read.iter().zip(sys_res_read) {
            let sys_ptr = unsafe { res_stor.get_ptr(*ri).unwrap() };
            assert_eq!(buf_ptr.inner(), sys_ptr);
        }

        // Validates resource write query.
        assert_eq!(buf_res_write.len(), sys_res_write.len());
        for (buf_ptr, ri) in buf_res_write.iter().zip(sys_res_write) {
            let sys_ptr = unsafe { res_stor.get_ptr(*ri).unwrap() };
            assert_eq!(buf_ptr.inner(), sys_ptr);
        }

        // Validates entity write query.
        assert_eq!(buf_ent_write.len(), sys_ent_write.len());
        for (buf_filtered, sys_eis) in buf_ent_write.iter().zip(sys_ent_write) {
            let buf_conts = buf_filtered.query_res();
            assert_eq!(buf_conts.len(), sys_eis.len());

            // Note that buf_conts need to be sorted. See example below.
            // Query: (Fa, ...)
            // Fa -> E0, E1, ... : OK.
            // Fa -> E1, E0, ... : Also OK.

            let mut buf_ptrs = buf_conts
                .iter()
                .map(|cont| cont.as_ptr())
                .collect::<Vec<_>>();

            let mut sys_ptrs = sys_eis
                .iter()
                .map(|ei| unsafe {
                    ent_stor.get_ptr(ei).unwrap().as_ptr()
                })
                .collect::<Vec<_>>();

            buf_ptrs.sort_unstable();
            sys_ptrs.sort_unstable();

            assert_eq!(buf_ptrs, sys_ptrs);
        }
    }

    struct SystemIndices {
        // Query: (Sa, Sb, ...)
        // Indices: Vec of Vec of (ei, ci)
        read: Vec<Vec<(EntityIndex, usize)>>,
        write: Vec<Vec<(EntityIndex, usize)>>,
        // Resource query: (Ra, Rb, ...)
        // Indices: Vec of ri
        res_read: Vec<ResourceIndex>,
        res_write: Vec<ResourceIndex>,
        // Entity query: (Fa, Fb, ...)
        // Indices: Vec of Vec of ei
        ent_write: Vec<Vec<EntityIndex>>,
    }

    impl SystemIndices {
        fn new<S>(
            rinfo: &RequestInfo,
            ent_stor: &EntityStorage<S>,
            res_stor: &ResourceStorage<S>,
        ) -> Self
        where
            S: BuildHasher + Default + Clone + 'static,
        {
            let query_indices = |qinfo: &QueryInfo| {
                qinfo
                    .select_infos()
                    .map(|sinfo| {
                        Matcher::collect_selected(ent_stor, sinfo)
                            .0
                            .into_iter()
                            .map(|(ei, ci)| (ei, ci))
                            .collect::<Vec<_>>()
                    })
                    .collect::<Vec<_>>()
            };

            let res_query_indices = |rqinfo: &ResQueryInfo| {
                rqinfo
                    .resource_keys()
                    .iter()
                    .map(|rkey| res_stor.index(rkey).unwrap())
                    .collect::<Vec<_>>()
            };

            let ent_query_indices = |eqinfo: &EntQueryInfo| {
                eqinfo
                    .filters()
                    .iter()
                    .map(|(_, finfo)| {
                        Matcher::iter_matched_entity_indices(ent_stor, finfo)
                            .map(|ei| ei)
                            .collect::<Vec<_>>()
                    })
                    .collect::<Vec<_>>()
            };

            Self {
                read: query_indices(&rinfo.read().1),
                write: query_indices(&rinfo.write().1),
                res_read: res_query_indices(&rinfo.res_read().1),
                res_write: res_query_indices(&rinfo.res_write().1),
                ent_write: ent_query_indices(&rinfo.ent_write().1),
            }
        }
    }
}
