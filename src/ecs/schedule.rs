use super::{
    borrow_js::JsAtomic,
    entity::{EntityDict, EntityForm, EntityKey, EntityKeyKind, EntityTag},
    filter::{FilterInfo, Filtered},
    predefined::resource::{RenderResource, SceneManager},
    query::{QueryInfo, ResQueryInfo},
    request::{RequestBuffer, RequestInfo, RequestKey},
    resource::{Resource, ResourceKey, ResourcePack},
    system::{SharedInfo, System, SystemData, SystemId, SystemKey, SystemPack},
    wait::{WaitNotifyType, WaitQueuePack, WaitRequest},
};
use crate::{
    ds::{
        borrow::Borrowed,
        set_list::{ListPos, SetValueList},
    },
    worker::{msg::ChMsg, MainChannel, Worker},
};
use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::Debug,
    ptr::NonNull,
    sync::mpsc::TryRecvError,
    thread,
};

#[derive(Debug)]
pub(crate) struct Scheduler {
    /// Systems.
    systems: SystemPack,

    /// Entities and components.
    pub(crate) entities: EntityDict, // TODO: Remove pub(crate).

    /// Resources.
    resources: ResourcePack,

    /// Shared info.
    sh_info: SharedInfo,

    waits: WaitQueuePack,

    /// Caching what to wait and response of the request.
    cache: HashMap<RequestKey, RequestCache, ahash::RandomState>,

    /// This helps systems to be updated when a new entity or resource is registered.
    cache_noti:
        HashMap<WaitNotifyType, HashSet<RequestKey, ahash::RandomState>, ahash::RandomState>,

    /// Dedicated tasks that should be run on main worker because they need JS ownership relative data such as OffscreenCanvas.
    /// It may be moved to a special dedicated sub worker in the future.
    dedi_tasks: HashSet<SystemId, ahash::RandomState>,

    /// Idle worker indices.
    idle_workers: Vec<usize>,

    /// Pending task positions in the [`Self::systems`] due to the data dependency.
    pending_tasks: SetValueList<ListPos>,

    /// Dedicated pending task positions in the [`Self::systems`] due to the data dependency.
    dedi_pending_tasks: VecDeque<ListPos>,
}

impl Scheduler {
    pub(crate) fn new() -> Self {
        let mut sh_info = SharedInfo::new();
        let systems = SystemPack::new(&mut sh_info);

        Self {
            systems,
            entities: EntityDict::new(),
            resources: ResourcePack::new(),
            sh_info,
            waits: WaitQueuePack::new(),
            cache: HashMap::default(),
            cache_noti: HashMap::default(),
            dedi_tasks: HashSet::default(),
            idle_workers: Vec::new(),
            pending_tasks: SetValueList::new(ListPos::dummy()),
            dedi_pending_tasks: VecDeque::new(),
        }
    }

    /// Must be called before [`Self::register_system`].
    #[inline]
    pub(crate) fn register_default_resource(&mut self, rkey: ResourceKey, ptr: NonNull<u8>) {
        let index = self.resources.register_default_resource(rkey, ptr);
        self.waits.init_resource_queue(index);
    }

    pub(crate) fn register_resource(&mut self) {
        todo!()
    }

    #[inline]
    pub(crate) fn register_system<S: System>(&mut self, sys: S) -> bool {
        fn inner(this: &mut Scheduler, skey: SystemKey, sdata: SystemData) -> bool {
            // Determines whether the system is dedicated task or not.
            let (_, rinfo) = sdata.req();
            let (_, read_qinfo) = rinfo.res_read();
            let (_, write_qinfo) = rinfo.res_write();
            let is_dedicated = read_qinfo
                .rkeys()
                .iter()
                .chain(write_qinfo.rkeys().iter())
                .any(|&rkey| 
                    rkey == <RenderResource as Resource>::key()
                    || rkey == <SceneManager as Resource>::key()
                );

            this.create_cache_for_system(&sdata);
            let sid = this.systems.register_system(skey, sdata);

            // Now, the system's id was set by register_system().
            if is_dedicated {
                if let Some(sid) = sid {
                    this.dedi_tasks.insert(sid);
                }
            }

            sid.is_some()
        }

        let sdata = sys.into_data(&mut self.sh_info);
        inner(self, S::key(), sdata)
    }

    pub(crate) fn register_entity(&mut self, reg: EntityForm) -> usize {
        // Registers new entity.
        let enti = self.entities.register_entity(reg);

        // Updates cache for the new entity.
        self.update_cache(enti);

        // Makes wait queue for the entity.
        let cont = unsafe {
            self.entities
                .get_entity_container(EntityKey::Index(enti))
                .unwrap_unchecked()
        };
        self.waits.init_entity_queue(enti, cont.get_column_num());

        enti
    }

    #[inline]
    pub(crate) fn activate_system(&mut self, skey: &SystemKey, after: &SystemKey) -> bool {
        self.systems.activate_system(skey, after)
    }

    #[inline]
    pub(crate) fn activate_system_as_first(&mut self, skey: &SystemKey) -> bool {
        self.systems.activate_system_as_first(skey)
    }

    /// Schedules all systems(tasks).
    pub(crate) fn schedule(&mut self, workers: &[Worker], ch: &MainChannel) {
        /// Reads main worker's channel and returns true if something has been sent from sub workers.
        /// This doesn't block because,
        /// - We have pending tasks that are not ready yet because of data dependencies.
        /// - If we have an idle worker, we see if there's a ready task among pending or new tasks.
        /// - But, during the process, any busy workers can become idle by finishing their tasks.
        /// - Then, it may free some data, which means some dependencies may be released.
        /// - So, we need to go back to the first pending task and check it again.
        /// - As a result, we need to check the channel every time while we traverse tasks.
        fn try_recv(
            ch: &MainChannel,
            cache: &HashMap<RequestKey, RequestCache, ahash::RandomState>,
            waits: &mut WaitQueuePack,
            idle_workers: &mut Vec<usize>,
        ) -> bool {
            match ch.try_recv() {
                Ok(ChMsg::Fin(wid, rkey)) => {
                    let cached = cache.get(&rkey).unwrap();
                    waits.dequeue(&cached.wait);
                    idle_workers.push((wid.into_u32() - 1) as usize);
                    true
                }
                Err(TryRecvError::Empty) => {
                    // Nothing to do.
                    // According to Rust description, thread::park() can be unparked unconditionally.
                    // (https://doc.rust-lang.org/std/thread/fn.park.html)
                    // I'm not sure it may occur on wasm as well, so just let's prepare for it.
                    false
                }
                Ok(msg) => {
                    unreachable!("[E] schedule(): received unknown msg: {:?}", msg);
                }
                Err(TryRecvError::Disconnected) => {
                    unreachable!("[E] channel has been broken");
                }
            }
        }

        /// Determines if the system(task) is runnable,
        /// which means there's no data dependency at the time.
        /// If it's runnable, the system's cached buffer is updated and then returned.
        fn try_update_task<'a>(
            sdata: &mut SystemData,
            cache: &'a mut HashMap<RequestKey, RequestCache, ahash::RandomState>,
            waits: &mut WaitQueuePack,
            entities: &mut EntityDict,
            resources: &mut ResourcePack,
        ) -> Option<&'a mut RequestCache> {
            let (rkey, _) = sdata.req();
            let cached = cache.get_mut(rkey).unwrap();
            if waits.enqueue(&mut cached.wait) {
                // Before we send the data, we need to update(re-borrow) it.
                cached.update_buffer(entities, resources);
                Some(cached)
            } else {
                None
            }
        }

        /// Tries to give the task `sdata` to the `worker`.
        /// If it succeeded, returns true.
        fn try_give(
            sdata: &mut SystemData,
            worker: &Worker,
            cache: &mut HashMap<RequestKey, RequestCache, ahash::RandomState>,
            waits: &mut WaitQueuePack,
            entities: &mut EntityDict,
            resources: &mut ResourcePack,
        ) -> bool {
            if let Some(cached) = try_update_task(sdata, cache, waits, entities, resources) {
                let task = ChMsg::Task(sdata.task_ptr(), cached.buf_ptr());
                worker.send(task).unwrap();
                true
            } else {
                false
            }
        }

        /// Tries to do the dedicated task on the main worker.
        fn try_do_self(
            sdata: &mut SystemData,
            cache: &mut HashMap<RequestKey, RequestCache, ahash::RandomState>,
            waits: &mut WaitQueuePack,
            entities: &mut EntityDict,
            resources: &mut ResourcePack,
        ) -> bool {
            if let Some(cached) = try_update_task(sdata, cache, waits, entities, resources) {
                let invokable = sdata.invokable_mut();
                invokable.invoke(&cached.buf);
                waits.dequeue(&cached.wait);
                true
            } else {
                false
            }
        }

        unsafe fn has_task(
            pending_tasks: &SetValueList<ListPos>,
            dedi_pending_tasks: &VecDeque<ListPos>,
            systems: &SystemPack,
            dedi_tasks: &HashSet<SystemId, ahash::RandomState>,
            cur_sys: ListPos,
        ) -> (bool, bool) {
            let mut has_normal_task = pending_tasks.len_occupied() > 0;
            let mut has_dedicated_task = !dedi_pending_tasks.is_empty();
            if let Some((_, sdata)) = unsafe { systems.iter_next(cur_sys) } {
                let is_dedicated = is_dedicated(sdata, dedi_tasks);
                has_dedicated_task |= is_dedicated;
                has_normal_task |= !is_dedicated;
            }
            (has_normal_task, has_dedicated_task)
        }

        #[inline]
        fn is_dedicated(
            sdata: &SystemData,
            dedi_tasks: &HashSet<SystemId, ahash::RandomState>,
        ) -> bool {
            dedi_tasks.contains(&sdata.id())
        }

        #[inline]
        fn is_worker_working(idle_workers: &[usize], worker_num: usize) -> bool {
            idle_workers.len() < worker_num
        }

        let Self {
            systems,
            entities,
            resources,
            waits,
            cache,
            dedi_tasks,
            idle_workers,
            pending_tasks,
            dedi_pending_tasks,
            ..
        } = self;

        // Opens all workers.
        idle_workers.clear();
        for (wi, worker) in workers.iter().enumerate() {
            worker.open();
            idle_workers.push(wi);
        }

        // TODO: system pointers will be transferred to workers. 
        // That means system must not be moved during **running**. 
        // Can we guarantee that?

        let worker_num = workers.len();
        let mut cur_sys = systems.iter_begin();

        'outer: loop {

            while try_recv(ch, cache, waits, idle_workers) {}

            // Safety: `cur_sys` is controlled to be valid.
            let (has_normal_task, has_dedicated_task) = unsafe { has_task(
                pending_tasks,
                dedi_pending_tasks,
                systems,
                dedi_tasks,
                cur_sys,
            ) };

            match (
                has_normal_task, 
                has_dedicated_task, 
                idle_workers.is_empty(),
                is_worker_working(idle_workers, worker_num),
            ) {
                // Only normal task, but no idle worker.
                (true, false, true, _) => thread::park(),
                // No tasks, no working.
                (false, false, _, false) => break 'outer,
                // Goes on to the next step.
                _ => {}
            }

            if has_normal_task {
                if let Some(wi) = idle_workers.last() {
                    let worker = &workers[*wi];

                    // Tries to give the worker a pending task first.
                    for pos in pending_tasks.iter().cloned() {
                        if try_recv(ch, cache, waits, idle_workers) {
                            continue 'outer;
                        }
                        // Safety: position of a pending task can't be invalid.
                        let (_, sdata) = unsafe { systems.iter_next_mut(pos).unwrap_unchecked() };
                        if try_give(sdata, worker, cache, waits, entities, resources) {
                            idle_workers.pop();
                            pending_tasks.remove(&pos);
                            continue 'outer;
                        }
                    }

                    // Searches not pending tasks after pending tasks.
                    // Safety: cur_sys is controlled to be valid.
                    while let Some((next, sdata)) = unsafe { systems.iter_next_mut(cur_sys) } {
                        if try_recv(ch, cache, waits, idle_workers) {
                            continue 'outer;
                        }
                        if is_dedicated(sdata, dedi_tasks) {
                            if !try_do_self(sdata, cache, waits, entities, resources) {
                                dedi_pending_tasks.push_back(cur_sys);
                            }
                        } else if try_give(sdata, worker, cache, waits, entities, resources) {
                            idle_workers.pop();
                            cur_sys = next;
                            continue 'outer;
                        } else {
                            pending_tasks.push_back(cur_sys);
                        }
                        cur_sys = next;
                    }
                }
            } 

            if has_dedicated_task {
                if try_recv(ch, cache, waits, idle_workers) {
                    continue 'outer;
                }
                if let Some(pos) = dedi_pending_tasks.front() {
                    let (_, sdata) = unsafe { systems.iter_next_mut(*pos).unwrap_unchecked() };
                    if try_do_self(sdata, cache, waits, entities, resources) {
                        dedi_pending_tasks.pop_front();
                    }
                } else if let Some((next, sdata)) = unsafe { systems.iter_next_mut(cur_sys) } {
                    if is_dedicated(sdata, dedi_tasks) {
                        if !try_do_self(sdata, cache, waits, entities, resources) {
                            dedi_pending_tasks.push_back(cur_sys);
                        }
                        cur_sys = next;
                    }
                }
            }
        }

        // Takes O(1).
        debug_assert!(cur_sys.is_end()); // All system must have been done.
        debug_assert_eq!(0, pending_tasks.len_occupied()); // No pending tasks.
        debug_assert!(dedi_pending_tasks.is_empty()); // No dedicated pending tasks.
        debug_assert_eq!(worker_num, idle_workers.len()); // All workers must be idle.

        // Takes O(n).
        debug_assert!(waits.is_all_queue_empty()); // No waiting request.

        // Closes all workers.
        for worker in workers.iter() {
            worker.close().unwrap();
        }

        // crate::log!("[D] schedule() has finished.");
    }

    /// Caches the system's requests.
    fn create_cache_for_system(&mut self, sdata: &SystemData) {
        let (rkey, rinfo) = sdata.req();
        if self.cache.contains_key(rkey) {
            return;
        }

        // Creates a list of `WaitTarget` and `Filtered` for the query.
        // Plus, registers notification for the request.
        let mut cache_query = |qinfo: &QueryInfo| -> (Vec<(usize, usize)>, Box<[Filtered]>) {
            let mut merged_waits = Vec::new();
            let mut merged_filtered = Vec::new();
            for (_, finfo) in qinfo.filters().iter() {
                let (waits, filtered) = Self::filter_all(&self.entities, finfo);
                merged_waits.extend(waits);
                merged_filtered.push(filtered);

                // Adds a notification for this request.
                let noti = WaitNotifyType::Comp(*finfo.target());
                self.cache_noti
                    .entry(noti)
                    .and_modify(|set| {
                        set.insert(*rkey);
                    })
                    .or_insert_with(|| {
                        let mut set = HashSet::default();
                        set.insert(*rkey);
                        set
                    });
            }
            merged_waits.sort_unstable();
            merged_waits.dedup();
            (merged_waits, merged_filtered.into_boxed_slice())
        };

        // Panics if any resource is not registered yet.
        let cache_res_query =
            |qinfo: &ResQueryInfo| -> (Vec<usize>, Vec<Borrowed<NonNull<u8>, JsAtomic>>) {
                // TODO: In here, assumes there is no duplication.
                let waits = qinfo
                    .rkeys()
                    .iter()
                    .map(|rkey| self.resources.get_index(rkey).unwrap())
                    .collect::<Vec<_>>();
                (waits, Vec::new())
            };

        // TODO: Validation: Conflicts b/w read and write.
        // But, Users may want to insert/remove Entity using EntityContainer
        // while read some columns from the Entity.

        // TODO: Validation: restrict duplicated filter or resource or entity in a single query.

        // Makes a new entry to the cache.
        let mut cached = RequestCache::new();
        (cached.wait.read, cached.buf.read) = cache_query(&rinfo.read().1);
        (cached.wait.write, cached.buf.write) = cache_query(&rinfo.write().1);
        (cached.wait.res_read, cached.buf.res_read) = cache_res_query(&rinfo.res_read().1);
        (cached.wait.res_write, cached.buf.res_write) = cache_res_query(&rinfo.res_write().1);

        // TODO: Caches the request's entity write.

        self.cache.insert(*rkey, cached);
    }

    /// Updates cache data for the new entity.
    fn update_cache(&mut self, enti: usize) {
        /// Updates a single cache data for the new entity.
        fn inner(
            entities: &EntityDict,
            enti: usize,
            rinfo: &RequestInfo,
            cached: &mut RequestCache,
        ) {
            let inner2 =
                |qinfo: &QueryInfo, waits: &mut Vec<(usize, usize)>, buf: &mut Box<[Filtered]>| {
                    debug_assert_eq!(qinfo.filters().len(), buf.len());

                    for ((_, finfo), filtered) in qinfo.filters().iter().zip(buf.iter_mut()) {
                        if let Some((enti, coli, etag)) = Scheduler::filter(entities, enti, finfo) {
                            if let Err(i) = waits.binary_search(&(enti, coli)) {
                                waits.insert(i, (enti, coli));
                            }
                            filtered.add(etag, coli);
                        }
                    }
                };

            // Checks the request's read out and update `waits` and `filtered` for it.
            inner2(&rinfo.read().1, &mut cached.wait.read, &mut cached.buf.read);

            // Checks the request's write out and update `waits` and `filtered` for it.
            inner2(
                &rinfo.write().1,
                &mut cached.wait.write,
                &mut cached.buf.write,
            );
        }

        let ekey = self
            .entities
            .convert_entity_key(EntityKey::Index(enti), EntityKeyKind::ComponentKeys)
            .unwrap();
        let ckeys = ekey.comp_keys();

        // For each column in the new entity.
        for ckey in ckeys.iter() {
            // If any requests wait for the column.
            if let Some(rkeys) = self.cache_noti.get(&WaitNotifyType::Comp(*ckey)) {
                // Then, for the requests which was waiting for the column.
                for rkey in rkeys.iter() {
                    // Updates its response.
                    let cached = self.cache.get_mut(rkey).unwrap();
                    let rinfo = self.sh_info.get_request_info(rkey).unwrap();
                    inner(&self.entities, enti, rinfo, cached);
                }
            }
        }
    }

    /// Filters current all entity containers by the given filter.
    /// Note that entity container that will be registered in the future must be checked manually.
    /// Call [`Self::filter_new`] in that case.
    fn filter_all(entities: &EntityDict, finfo: &FilterInfo) -> (Vec<(usize, usize)>, Filtered) {
        let (waits, (ent_tags, col_indices)): (Vec<_>, (Vec<_>, Vec<_>)) = entities
            .iter_entity_container()
            .filter_map(|(_, enti, _)| Self::filter(entities, enti, finfo))
            .map(|(enti, coli, etag)| ((enti, coli), (etag, coli)))
            .unzip();

        (waits, Filtered::new(ent_tags, col_indices))
    }

    /// Determines the entity is matched by the filter.
    /// If so, returns matching information.
    fn filter(
        entities: &EntityDict,
        enti: usize,
        finfo: &FilterInfo,
    ) -> Option<(usize, usize, EntityTag)> {
        let ekey = EntityKey::Index(enti);
        let cont = entities.get_entity_container(ekey)?;
        if !finfo.filter(|ckey| cont.contains_column(ckey.as_type())) {
            return None;
        }

        // Safety: `ekey` has its container, which means `ekey` can be converted.
        let ekey = unsafe {
            entities
                .convert_entity_key(ekey, EntityKeyKind::ComponentKeys)
                .unwrap_unchecked()
        };
        let ckeys = ekey.comp_keys();
        let etag = EntityTag::new(enti, cont.name(), ckeys, cont.comp_names());
        // Safety: It's safe because we've already filtered.
        let coli = unsafe {
            cont.get_column_index(finfo.target().as_type())
                .unwrap_unchecked()
        };
        Some((enti, coli, etag))
    }
}

#[derive(Debug)]
pub(super) struct RequestCache {
    pub(super) wait: WaitRequest,
    pub(super) buf: Box<RequestBuffer>,
}

impl RequestCache {
    pub(super) fn new() -> Self {
        Self {
            wait: WaitRequest::new(),
            buf: Box::new(RequestBuffer::new()),
        }
    }

    pub(super) fn buf_ptr(&mut self) -> NonNull<RequestBuffer> {
        let ptr = self.buf.as_mut() as *mut _;
        // Safety: Infallible.
        unsafe { NonNull::new_unchecked(ptr) }
    }

    // TODO: Adds parameter for updating resource too.
    /// Updates cached buffer by re-borrowing the data.
    pub(super) fn update_buffer(&mut self, ents: &mut EntityDict, res: &mut ResourcePack) {
        // Updates component buffer.
        for filtered in self.buf.read.iter_mut() {
            let (ent_tags, col_indices, query_res) = filtered.destructure();

            // We call Vec::set_len() instead of Vec::clear() not to call Borrowed::drop() here.
            // When a system finishes its operation, it drops FilteredIter or FilteredIterMut that it received.
            // The iterators drop the items, which are Borrowed, in this vector manually.
            // Therefore, Borrowed is dropped as *soon* as it has been completely used.
            // See FilteredIter::drop() and FilteredIterMut::drop().
            unsafe { query_res.set_len(0) };

            for (enti, coli) in Filtered::iter_index_pair(ent_tags, col_indices) {
                let cont = ents.get_entity_container(EntityKey::Index(enti)).unwrap();
                let col = cont.borrow_column(coli).unwrap();
                query_res.push(col);
            }
        }
        for filtered in self.buf.write.iter_mut() {
            let (ent_tags, col_indices, query_res) = filtered.destructure();
            unsafe { query_res.set_len(0) };
            for (enti, coli) in Filtered::iter_index_pair(ent_tags, col_indices) {
                let cont = ents
                    .get_entity_container_mut(EntityKey::Index(enti))
                    .unwrap();
                let col = cont.borrow_column_mut(coli).unwrap();
                query_res.push(col);
            }
        }

        // Updates resource buffer.
        // Borrowed in the resource buffer is also dropped like above.
        // See ResourceRef::drop() and ResourceMut::drop().
        unsafe { self.buf.res_read.set_len(0) };
        for ri in self.wait.res_read.iter().cloned() {
            let borrowed = res.borrow(ri).unwrap();
            self.buf.res_read.push(borrowed);
        }
        unsafe { self.buf.res_write.set_len(0) };
        for ri in self.wait.res_write.iter().cloned() {
            let borrowed = res.borrow(ri).unwrap();
            self.buf.res_write.push(borrowed);
        }

        // Updates entity buffer.
        // TODO
    }
}
