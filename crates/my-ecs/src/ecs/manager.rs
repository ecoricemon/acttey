use super::{
    entity::{
        AsEntityDesc, ContainEntity, Entity, EntityDesc, EntityDict, EntityId, EntityKey,
        EntityKeyKind, EntityTag,
    },
    filter::{FilterInfo, Filtered},
    query::{EntQueryInfo, QueryInfo, ResQueryInfo},
    request::{RequestBuffer, RequestInfo, RequestKey},
    resource::{Resource, ResourceKey, ResourcePack},
    system::{
        FnOnceSystem, FnSystem, Invoke, NonZeroTick, SharedInfo, StructOrFnSystem, System,
        SystemData, SystemId, SystemKey, SystemManager, Tick,
    },
    wait::{WaitNotifyType, WaitQueuePack, WaitRequest},
    worker::{ChMsg, MainChannel, SubChannel, Work, WorkerIndex},
    EcsError,
};
use crate::{debug_format, ds::prelude::*, fname, panic_if_called_twice, util::prelude::*};
use std::{
    any::{self, TypeId},
    collections::{HashMap, HashSet, VecDeque},
    fmt::Debug,
    hash::{BuildHasher, RandomState},
    marker::PhantomData,
    num::NonZeroU32,
    ptr::NonNull,
    sync::{atomic::AtomicI32, mpsc::TryRecvError},
    thread,
    time::Duration,
};

#[derive(Debug)]
pub struct EcsManager<S = RandomState> {
    /// Systems that will be executed in phase A or B.
    sys_mgr: Twin<SystemManager<S>>,

    /// Command type -> System id.
    cmd_handlers: HashMap<TypeId, Vec<SystemId>, S>,

    /// Entities and components.
    pub(crate) entities: EntityDict<S>, // TODO: Remove pub(crate).

    /// Resources.
    resources: ResourcePack<S>,

    /// Resources that need to be accessed by main worker.
    dedi_res: HashSet<ResourceKey, S>,

    /// Shared info.
    sh_info: SharedInfo<S>,

    waits: WaitQueuePack<S>,

    /// Caching what to wait and response for the request.
    cache: HashMap<RequestKey, RequestCache, S>,

    /// This helps systems to be updated when a new entity or resource is registered.
    cache_noti: HashMap<WaitNotifyType, HashSet<RequestKey, S>, S>,

    /// Dedicated tasks that should be run on main worker because they need JS ownership relative data such as OffscreenCanvas.
    /// It may be moved to a special dedicated sub worker in the future.
    dedi_tasks: HashSet<SystemId, S>,

    /// Scheduler.
    sched: Scheduler<S>,

    /// Idle worker indices.
    idle_workers: Vec<usize>,

    /// Pending task positions in the [`Self::sys_mgr`] due to the data dependency.
    pending_tasks: SetValueList<ListPos, S>,

    /// Dedicated pending task positions in the [`Self::sys_mgr`] due to the data dependency.
    dedi_pending_tasks: VecDeque<ListPos>,

    /// System run history.
    sys_run_history: HashSet<SystemId, S>,
}

impl<S> EcsManager<S>
where
    S: BuildHasher + Default,
{
    pub fn new() -> Self {
        let mut sh_info = SharedInfo::new();
        let sys_mgr_a = SystemManager::new(&mut sh_info);
        let sys_mgr_b = SystemManager::new(&mut sh_info);

        Self {
            sys_mgr: Twin::new(sys_mgr_a, sys_mgr_b),
            cmd_handlers: HashMap::default(),
            entities: EntityDict::new(),
            resources: ResourcePack::new(),
            dedi_res: HashSet::default(),
            sh_info,
            waits: WaitQueuePack::new(),
            cache: HashMap::default(),
            cache_noti: HashMap::default(),
            dedi_tasks: HashSet::default(),
            sched: Scheduler::new(),
            idle_workers: Vec::new(),
            pending_tasks: SetValueList::new(ListPos::end()),
            dedi_pending_tasks: VecDeque::new(),
            sys_run_history: HashSet::default(),
        }
    }

    pub fn register_entity<T: AsEntityDesc>(&mut self) -> EntityKey<'static> {
        self.register_entity_raw(T::as_entity_descriptor())
    }

    pub fn register_entity_raw(&mut self, desc: EntityDesc) -> EntityKey<'static> {
        // formisters new entity.
        let ekey = self.entities.register_entity(desc);

        // Updates cache for the new entity.
        self.update_cache(ekey.index());

        // Makes wait queue for the entity.
        let cont = unsafe {
            self.entities
                .get_entity_container(ekey.clone())
                .unwrap_unchecked()
        };
        self.waits
            .initialize_entity_queue(ekey.index(), cont.get_column_num());

        ekey
    }

    pub fn add_entity<E: Entity>(&mut self, enti: usize, value: E) -> Result<EntityId, EcsError> {
        if let Some(cont) = self
            .entities
            .get_entity_container_mut(EntityKey::Index(enti))
        {
            let index = value.move_to(&mut **cont);
            Ok(EntityId::new(enti, index))
        } else {
            let errmsg = debug_format!("{}", any::type_name::<E>());
            Err(EcsError::UnknownEntity(errmsg))
        }
    }

    pub fn register_dedicated_resource(&mut self, rkey: ResourceKey) {
        self.dedi_res.insert(rkey);
    }

    pub fn register_once_system<T, Req, F, Args>(
        &mut self,
        sys: T,
    ) -> Result<(SystemKey, SystemId), EcsError>
    where
        /* To accept FnOnce("variable length arguments") */
        T: Into<FnOnceSystem<Req, F>>,
        /* FnOnceSystem becomes FnMut */
        FnOnceSystem<Req, F>: Into<Box<dyn FnMut(Args)>>,
        /* We can handle FnMut as a System */
        Box<dyn FnMut(Args)>: System,
    {
        // FnOnce system will be removed from memory when it's expired.
        const VOLATILE: bool = true;
        let f: FnOnceSystem<Req, F> = sys.into();
        let boxed: Box<dyn FnMut(Args)> = f.into();
        self.register_system(boxed, VOLATILE)
    }

    pub fn register_system<T, Sys, Req, F>(
        &mut self,
        sys: T,
        volatile: bool,
    ) -> Result<(SystemKey, SystemId), EcsError>
    where
        T: Into<StructOrFnSystem<Sys, Req, F>>,
        Sys: System,
        FnSystem<Req, F>: System,
    {
        fn inner<S>(
            this: &mut EcsManager<S>,
            skey: SystemKey,
            sdata: SystemData,
            volatile: bool,
        ) -> Result<SystemId, EcsError>
        where
            S: BuildHasher + Default,
        {
            // Determines whether the system is dedicated task or not.
            let (_, rinfo) = sdata.req();
            let (_, read_qinfo) = rinfo.res_read();
            let (_, write_qinfo) = rinfo.res_write();
            let is_dedicated = read_qinfo
                .rkeys()
                .iter()
                .chain(write_qinfo.rkeys().iter())
                .any(|rkey| this.dedi_res.contains(rkey));

            this.create_cache_for_system(&sdata)?;
            let sid = this.sys_mgr.register_system(skey, sdata, volatile);

            // Now, the system's id was set by register_system().
            if is_dedicated {
                this.dedi_tasks.insert(sid);
            }

            Ok(sid)
        }

        let sys: StructOrFnSystem<Sys, Req, F> = sys.into();
        let (skey, sdata) = match sys {
            StructOrFnSystem::Struct(s) => {
                let skey = Sys::key();
                let sdata = s.into_data(&mut self.sh_info);
                (skey, sdata)
            }
            StructOrFnSystem::Fn(f) => {
                let skey = f.skey();
                let sdata = f.into_data(&mut self.sh_info);
                (skey, sdata)
            }
        };
        let sid = inner(self, skey, sdata, volatile);
        sid.map(|sid| (skey, sid))
    }

    /// Returns true if the system was successfully activated.
    #[must_use]
    pub fn activate_system(
        &mut self,
        target: &SystemId,
        after: &SystemId,
        mut live: NonZeroTick,
    ) -> bool {
        // It can be in the middle of scheduling,
        // which means there may be no chance to run target system in this time.
        // In that case, we need to add `live` by 1 to guarantee the total number of run.
        if let Some(next) = self.sys_mgr.peek_next(after) {
            if self.sys_run_history.contains(&next.id) {
                live = live.saturating_add(1);
            }
        }
        self.sys_mgr.activate_system(target, after, live)
    }

    /// Returns true if the system was successfully activated.
    #[must_use]
    pub fn activate_system_as_first(&mut self, target: &SystemId, mut live: NonZeroTick) -> bool {
        if !self.sys_run_history.is_empty() {
            live = live.saturating_add(1);
        }
        self.sys_mgr.activate_system_as_first(target, live)
    }

    /// Returns true if the system was successfully activated.
    #[must_use]
    pub fn activate_system_as_last(&mut self, target: &SystemId, live: NonZeroTick) -> bool {
        self.sys_mgr.activate_system_as_last(target, live)
    }

    pub fn append_once_system<T, Req, F, Args>(
        &mut self,
        sys: T,
    ) -> Result<(SystemKey, SystemId), EcsError>
    where
        /* To accept FnOnce("variable length arguments") */
        T: Into<FnOnceSystem<Req, F>>,
        /* FnOnceSystem becomes FnMut */
        FnOnceSystem<Req, F>: Into<Box<dyn FnMut(Args)>>,
        /* We can handle FnMut as a System */
        Box<dyn FnMut(Args)>: System,
    {
        // FnOnce system will be removed from memory when it's expired.
        const VOLATILE: bool = true;
        const LIVE: NonZeroTick = unsafe { NonZeroTick::new_unchecked(1) };
        let f: FnOnceSystem<Req, F> = sys.into();
        let boxed: Box<dyn FnMut(Args)> = f.into();
        self.append_system(boxed, LIVE, VOLATILE)
    }

    pub fn append_system<T, Sys, Req, F>(
        &mut self,
        sys: T,
        live: NonZeroTick,
        volatile: bool,
    ) -> Result<(SystemKey, SystemId), EcsError>
    where
        T: Into<StructOrFnSystem<Sys, Req, F>>,
        Sys: System,
        FnSystem<Req, F>: System,
    {
        let res = self.register_system(sys, volatile);
        if let Ok((_, sid)) = res.as_ref() {
            let must_true = self.activate_system_as_last(sid, live);
            debug_assert!(must_true);
        }
        res
    }

    pub fn register_command_handler<T, Sys, Req, F>(
        &mut self,
        cmd_ty: TypeId,
        sys: T,
    ) -> Result<(), EcsError>
    where
        T: Into<StructOrFnSystem<Sys, Req, F>>,
        Sys: System,
        FnSystem<Req, F>: System,
    {
        debug_assert!(self.sys_mgr.is_a());
        self.sys_mgr.toggle(); // To phase B.

        // Registers the handler to phase B.
        let res = self.register_system(sys, false).map(|(_skey, sid)| {
            self.cmd_handlers
                .entry(cmd_ty)
                .and_modify(|handlers| handlers.push(sid))
                .or_insert(vec![sid]);
        });

        self.sys_mgr.toggle(); // Get back to phase A.
        res
    }

    pub fn unregister_command_handler(&mut self, _cmd_ty: &TypeId, _sid: &SystemId) {
        debug_assert!(self.sys_mgr.is_a());
        self.sys_mgr.toggle(); // To phase B.
        self.sys_mgr.toggle(); // Get back to phase A.
        todo!("TODO");
    }

    // Assumes that this method will be called in phase B only.
    pub fn activate_command_handlers(&mut self, cmd_ty: &TypeId) {
        debug_assert!(self.sys_mgr.is_b());

        if let Some(handlers) = self.cmd_handlers.get(cmd_ty) {
            for sid in handlers.iter() {
                let live = unsafe { NonZeroU32::new_unchecked(1) };
                let must_true = self.sys_mgr.activate_system_as_last(sid, live);
                debug_assert!(must_true);
            }
        }
    }

    pub fn set_main_system_b<T, Sys, Req, F>(
        &mut self,
        sys: T,
    ) -> Result<(SystemKey, SystemId), EcsError>
    where
        T: Into<StructOrFnSystem<Sys, Req, F>>,
        Sys: System,
        FnSystem<Req, F>: System,
    {
        // This method must be called only once.
        #[cfg(debug_assertions)]
        panic_if_called_twice!("{}", fname!(&Self::set_main_system_b::<T, Sys, Req, F>));

        debug_assert!(self.sys_mgr.is_a());
        self.sys_mgr.toggle(); // To phase B

        // Registers and activates the main system.
        const LIVE: NonZeroTick = unsafe { NonZeroTick::new_unchecked(Tick::MAX) };
        let res = self.append_system(sys, LIVE, false);

        self.sys_mgr.toggle(); // Get back to phase A
        res
    }

    // TODO: Can I call this method after appending any system?
    pub fn register_resource(&mut self, rkey: ResourceKey, ptr: NonNull<u8>) {
        let index = self.resources.register(rkey, ptr);
        self.waits.initialize_resource_queue(index);
    }

    pub fn set_scheduler_timeout(&mut self, dur: Duration) {
        self.sched.set_wait_timeout(dur);
    }

    pub fn schedule<W: Work>(&mut self, workers: &mut [W]) {
        Scheduler::<S>::schedule(self, workers);
    }

    /// Caches the system's requests.
    fn create_cache_for_system(&mut self, sdata: &SystemData) -> Result<(), EcsError> {
        let (rkey, rinfo) = sdata.req();
        if self.cache.contains_key(rkey) {
            return Ok(());
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
        #[allow(clippy::type_complexity)]
        let cache_res_query = |rqinfo: &ResQueryInfo| -> Result<
            (Vec<usize>, Vec<Borrowed<NonNull<u8>, AtomicI32>>),
            EcsError,
        > {
            // TODO: In here, assumes there is no duplication.
            let mut waits = Vec::new();
            for rkey in rqinfo.rkeys() {
                if let Some(index) = self.resources.get_index(rkey) {
                    waits.push(index);
                } else {
                    let errmsg =
                        debug_format!("some resources are not registered yet: `{}`", rqinfo.name());
                    return Err(EcsError::UnknownResource(errmsg));
                }
            }
            Ok((waits, Vec::new()))
        };

        // Panics if any entity is not registered yet.
        #[allow(clippy::type_complexity)]
        let cache_ent_query = |eqinfo: &EntQueryInfo| -> Result<
            (
                Vec<usize>,
                Vec<Borrowed<NonNull<dyn ContainEntity>, AtomicI32>>,
            ),
            EcsError,
        > {
            let mut waits = Vec::new();
            for ekey in eqinfo.ekeys() {
                if let Some(ekey) = self
                    .entities
                    .convert_entity_key(ekey.clone(), EntityKeyKind::Index)
                {
                    waits.push(ekey.index());
                } else {
                    let errmsg =
                        debug_format!("some entities are not registered yet: `{}`", eqinfo.name());
                    return Err(EcsError::UnknownEntity(errmsg));
                }
            }
            Ok((waits, Vec::new()))
        };

        // TODO: Validation: Conflicts b/w read and write.
        // But, Users may want to insert/remove Entity using EntityContainer
        // while read some columns from the Entity.

        // TODO: Validation: restrict duplicated filter or resource or entity in a single query.

        // Makes a new entry to the cache.
        let mut cached = RequestCache::new();
        (cached.wait.read, cached.buf.read) = cache_query(&rinfo.read().1);
        (cached.wait.write, cached.buf.write) = cache_query(&rinfo.write().1);
        (cached.wait.res_read, cached.buf.res_read) = cache_res_query(&rinfo.res_read().1)?;
        (cached.wait.res_write, cached.buf.res_write) = cache_res_query(&rinfo.res_write().1)?;
        (cached.wait.ent_write, cached.buf.ent_write) = cache_ent_query(&rinfo.ent_write().1)?;

        self.cache.insert(*rkey, cached);
        Ok(())
    }

    /// Updates cache data for the new entity.
    fn update_cache(&mut self, enti: usize) {
        /// Updates a single cache data for the new entity.
        fn inner<S>(
            entities: &EntityDict<S>,
            enti: usize,
            rinfo: &RequestInfo,
            cached: &mut RequestCache,
        ) where
            S: BuildHasher + Default,
        {
            let inner2 = |qinfo: &QueryInfo,
                          waits: &mut Vec<(usize, usize)>,
                          buf: &mut Box<[Filtered]>| {
                debug_assert_eq!(qinfo.filters().len(), buf.len());

                for ((_, finfo), filtered) in qinfo.filters().iter().zip(buf.iter_mut()) {
                    if let Some((enti, coli, etag)) = EcsManager::<S>::filter(entities, enti, finfo)
                    {
                        if let Err(i) = waits.binary_search(&(enti, coli)) {
                            waits.insert(i, (enti, coli));
                        }
                        filtered.add(etag, coli);
                    }
                }
            };

            // Checks the request's read, and then updates `waits` and `filtered` for it.
            inner2(&rinfo.read().1, &mut cached.wait.read, &mut cached.buf.read);

            // Checks the request's write, and then updates `waits` and `filtered` for it.
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
    fn filter_all(entities: &EntityDict<S>, finfo: &FilterInfo) -> (Vec<(usize, usize)>, Filtered) {
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
        entities: &EntityDict<S>,
        enti: usize,
        finfo: &FilterInfo,
    ) -> Option<(usize, usize, EntityTag)> {
        let ekey = EntityKey::Index(enti);
        let cont = entities.get_entity_container(ekey.clone())?;
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

impl<S> Default for EcsManager<S>
where
    S: BuildHasher + Default,
{
    fn default() -> Self {
        Self::new()
    }
}

struct EcsManagerInner<'a, S> {
    sys_mgr: &'a mut SystemManager<S>,
    entities: &'a mut EntityDict<S>,
    resources: &'a mut ResourcePack<S>,
    waits: &'a mut WaitQueuePack<S>,
    cache: &'a mut HashMap<RequestKey, RequestCache, S>,
    dedi_tasks: &'a mut HashSet<SystemId, S>,
    idle_workers: &'a mut Vec<usize>,
    pending_tasks: &'a mut SetValueList<ListPos, S>,
    dedi_pending_tasks: &'a mut VecDeque<ListPos>,
    sys_run_history: &'a mut HashSet<SystemId, S>,
    sched: &'a mut Scheduler<S>,
}

impl<'a, S> EcsManagerInner<'a, S> {
    fn new(value: &'a mut EcsManager<S>) -> Self {
        Self {
            sys_mgr: &mut value.sys_mgr,
            entities: &mut value.entities,
            resources: &mut value.resources,
            waits: &mut value.waits,
            cache: &mut value.cache,
            dedi_tasks: &mut value.dedi_tasks,
            idle_workers: &mut value.idle_workers,
            pending_tasks: &mut value.pending_tasks,
            dedi_pending_tasks: &mut value.dedi_pending_tasks,
            sys_run_history: &mut value.sys_run_history,
            sched: &mut value.sched,
        }
    }
}

// Scheduler is a part of EcsManager and it has a role of scheduling only.
#[derive(Debug)]
struct Scheduler<S> {
    main_ch: MainChannel,
    sub_chs: Vec<SubChannel>,

    /// Scheduler will stop itself when it needs to wait for workers for this timeout.
    wait_timeout: Duration,
    _marker: PhantomData<S>,
}

impl<S> Scheduler<S> {
    const DEFAULT_WAIT_TIMEOUT: Duration = Duration::from_millis(10);

    fn new() -> Self {
        Self {
            main_ch: MainChannel::new(),
            sub_chs: Vec::new(),
            wait_timeout: Self::DEFAULT_WAIT_TIMEOUT,
            _marker: PhantomData,
        }
    }

    fn set_wait_timeout(&mut self, dur: Duration) {
        self.wait_timeout = dur;
    }
}

impl<S> Scheduler<S>
where
    S: BuildHasher + Default,
{
    /// Schedules all systems(tasks).
    fn schedule<W: Work>(ecs: &mut EcsManager<S>, workers: &mut [W]) {
        // Extends sub-channel array if needed.
        for i in ecs.sched.sub_chs.len()..workers.len() {
            ecs.sched.sub_chs.push(SubChannel::new(
                ecs.sched.main_ch.clone_tx(),
                thread::current(),
                WorkerIndex::new(i),
            ));
        }

        // Opens workers.
        for (widx, worker) in workers.iter_mut().enumerate() {
            let job = ecs.sched.sub_chs[widx].job_ptr();
            // Safety: The pointer won't be aliased and they will be valid during scheduling.
            let job = unsafe { ManagedConstPtr::new(NonNull::new_unchecked(job.cast_mut())) };
            let must_true = worker.unpark(job);
            // TODO: For now, we assumes that workers must be opened.
            assert!(must_true);
            ecs.idle_workers.push(widx);
        }

        // Phase A
        {
            debug_assert!(ecs.sys_mgr.is_a());

            // Runs activated systems.
            Self::schedule_loop(EcsManagerInner::new(ecs), workers);

            // Clears system run history.
            ecs.sys_run_history.clear();

            // Inactivates expired systems.
            ecs.sys_mgr.tick();

            debug_assert!(ecs.sys_mgr.iter_current().is_end()); // All systems must have been done.
            debug_assert_eq!(0, ecs.pending_tasks.len_occupied()); // No pending tasks.
            debug_assert!(ecs.dedi_pending_tasks.is_empty()); // No dedicated pending tasks.
            debug_assert_eq!(workers.len(), ecs.idle_workers.len()); // All workers must be idle.
            debug_assert!(ecs.sys_run_history.is_empty()); // System run history must be empty.
            debug_assert!(ecs.waits.is_all_queue_empty()); // No waiting request. Takes O(n).
        }

        // Phase B
        {
            // Switches systems to B.
            ecs.sys_mgr.toggle();
            debug_assert!(ecs.sys_mgr.is_b());

            // Keeps going if user registered main system of phase B.
            if ecs.sys_mgr.activate_system_num() > 0 {
                for i in 0.. {
                    const MAX_ITER: usize = 10;
                    debug_assert!(i < MAX_ITER, "detected too long command chain");

                    // Runs activated systems.
                    Self::schedule_loop(EcsManagerInner::new(ecs), workers);

                    // Inactivates expired systems.
                    ecs.sys_mgr.tick();

                    // If there's no appended system except main system, escapes the loop.
                    if ecs.sys_mgr.activate_system_num() == 1 {
                        break;
                    }
                }

                // Clears system run history.
                ecs.sys_run_history.clear();
            }
            // Gets back to A.
            ecs.sys_mgr.toggle();
            debug_assert!(ecs.sys_mgr.is_a());
        }

        // Exits message loop.
        for sub_ch in ecs.sched.sub_chs.iter() {
            sub_ch.send(ChMsg::End).unwrap();
        }

        // Closes workers.
        for worker in workers.iter_mut() {
            let must_true = worker.park();
            // TODO: For now, we assumes that workers must be closed.
            assert!(must_true);
        }
        ecs.idle_workers.clear();
    }

    // TODO: system pointers will be transferred to workers.
    // That means system must not be moved during **running**.
    // Can we guarantee that using Rust syntax or validation in debug mode?
    fn schedule_loop<W: Work>(ecs: EcsManagerInner<S>, workers: &mut [W]) {
        let EcsManagerInner {
            sys_mgr,
            entities,
            resources,
            waits,
            cache,
            dedi_tasks,
            idle_workers,
            pending_tasks,
            dedi_pending_tasks,
            sys_run_history,
            sched,
        } = ecs;
        let Self {
            main_ch,
            sub_chs,
            wait_timeout,
            _marker,
        } = sched;

        let num_workers = workers.len();
        sys_mgr.iter_begin();

        'outer: loop {
            while Self::try_recv(main_ch, cache, waits, idle_workers) {}

            // Safety: `cur_sys` is controlled to be valid.
            let (has_normal_task, has_dedicated_task) =
                unsafe { Self::has_task(pending_tasks, dedi_pending_tasks, sys_mgr, dedi_tasks) };

            match (
                has_normal_task,
                has_dedicated_task,
                idle_workers.is_empty(),
                Self::is_worker_working(idle_workers, num_workers),
            ) {
                // Only normal task, but no idle worker.
                (true, false, true, _) => thread::park_timeout(*wait_timeout),
                // No tasks, no working.
                (false, false, _, false) => break 'outer,
                // Goes on to the next step.
                _ => {}
            }

            if has_normal_task {
                if let Some(widx) = idle_workers.last() {
                    let sub_ch = &sub_chs[*widx];

                    // Tries to give the worker a pending task first.
                    for pos in pending_tasks.iter().cloned() {
                        // Safety: Pending system position is valid.
                        let sdata = unsafe { sys_mgr.iter_value_at(pos).unwrap_unchecked() };
                        if Self::try_give(sdata, sub_ch, cache, waits, entities, resources) {
                            idle_workers.pop();
                            pending_tasks.remove(&pos);
                            continue 'outer;
                        }
                        if Self::try_recv(main_ch, cache, waits, idle_workers) {
                            continue 'outer;
                        }
                    }

                    // Searches not pending tasks after pending tasks.
                    while let Some(sdata) = sys_mgr.iter_value_at_current() {
                        // The system's request is definitely going to be inserted into wait queue.
                        Self::record_run_system(sys_run_history, sdata.id);

                        if Self::is_dedicated(sdata, dedi_tasks) {
                            if !Self::try_do_self(sdata, cache, waits, entities, resources) {
                                dedi_pending_tasks.push_back(sys_mgr.iter_current());
                            }
                        } else if Self::try_give(sdata, sub_ch, cache, waits, entities, resources) {
                            idle_workers.pop();
                            sys_mgr.iter_next();
                            continue 'outer;
                        } else {
                            pending_tasks.push_back(sys_mgr.iter_current());
                        }
                        sys_mgr.iter_next();

                        if Self::try_recv(main_ch, cache, waits, idle_workers) {
                            continue 'outer;
                        }
                    }
                }
            }

            if has_dedicated_task {
                if let Some(pos) = dedi_pending_tasks.front() {
                    // Safety: Pending system position is valid.
                    let sdata = unsafe { sys_mgr.iter_value_at(*pos).unwrap_unchecked() };
                    if Self::try_do_self(sdata, cache, waits, entities, resources) {
                        dedi_pending_tasks.pop_front();
                    }
                } else if let Some(sdata) = sys_mgr.iter_value_at_current() {
                    if Self::is_dedicated(sdata, dedi_tasks) {
                        // The system's request is definitely going to be inserted into wait queue.
                        Self::record_run_system(sys_run_history, sdata.id);

                        if !Self::try_do_self(sdata, cache, waits, entities, resources) {
                            dedi_pending_tasks.push_back(sys_mgr.iter_current());
                        }
                        sys_mgr.iter_next();
                    }
                }
                if Self::try_recv(main_ch, cache, waits, idle_workers) {
                    continue 'outer;
                }
            }
        }
    }

    /// Reads the main channel and returns true if something has been sent from workers.
    /// This doesn't block because,
    /// - We have pending tasks that are not ready yet because of data dependencies.
    /// - If we have an idle worker, we'll see if there's a ready task among pending or new tasks.
    /// - But, during the process, any busy workers can become idle by finishing their tasks.
    /// - Then, it may free some data, which means some dependencies may be released.
    /// - So, we need to go back to the first pending task and check it again.
    /// - As a result, we need to check the channel every time while we traverse tasks.
    fn try_recv(
        ch: &MainChannel,
        cache: &HashMap<RequestKey, RequestCache, S>,
        waits: &mut WaitQueuePack<S>,
        idle_workers: &mut Vec<usize>,
    ) -> bool {
        match ch.try_recv() {
            Ok(ChMsg::Fin(widx, rkey)) => {
                let cached = cache.get(&rkey).unwrap();
                waits.dequeue(&cached.wait);
                idle_workers.push(widx.into_inner());
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
        cache: &'a mut HashMap<RequestKey, RequestCache, S>,
        waits: &mut WaitQueuePack<S>,
        entities: &mut EntityDict<S>,
        resources: &mut ResourcePack<S>,
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
        sub_ch: &SubChannel,
        cache: &mut HashMap<RequestKey, RequestCache, S>,
        waits: &mut WaitQueuePack<S>,
        entities: &mut EntityDict<S>,
        resources: &mut ResourcePack<S>,
    ) -> bool {
        if let Some(cached) = Self::try_update_task(sdata, cache, waits, entities, resources) {
            let task = ChMsg::Task(sdata.task_ptr(), cached.buf_ptr());
            sub_ch.send(task).unwrap();
            true
        } else {
            false
        }
    }

    /// Tries to do the dedicated task on the main worker.
    /// Note that a system can modify [`Scheduler`] directly.
    /// That means inner data in the fields of [`Scheduler`] can be moved after invoking the system.
    /// For now, it's fine.
    //
    // TODO: Safer way?
    fn try_do_self(
        sdata: &mut SystemData,
        cache: &mut HashMap<RequestKey, RequestCache, S>,
        waits: &mut WaitQueuePack<S>,
        entities: &mut EntityDict<S>,
        resources: &mut ResourcePack<S>,
    ) -> bool {
        if let Some(cached) = Self::try_update_task(sdata, cache, waits, entities, resources) {
            let invokable = sdata.invokable_mut();

            // NOTE: If `Scheduler` is modified, `cached` could be moved in `invoke()`.
            // Therefore, `dequeue()` must be called before it.
            // It's fine because the system is a dedicated task.
            waits.dequeue(&cached.wait);
            invokable.invoke(&mut cached.buf);
            true
        } else {
            false
        }
    }

    unsafe fn has_task(
        pending_tasks: &SetValueList<ListPos, S>,
        dedi_pending_tasks: &VecDeque<ListPos>,
        systems: &mut SystemManager<S>,
        dedi_tasks: &HashSet<SystemId, S>,
    ) -> (bool, bool) {
        let mut has_normal_task = pending_tasks.len_occupied() > 0;
        let mut has_dedicated_task = !dedi_pending_tasks.is_empty();
        if let Some(sdata) = systems.iter_value_at_current() {
            let is_dedicated = Self::is_dedicated(sdata, dedi_tasks);
            has_dedicated_task |= is_dedicated;
            has_normal_task |= !is_dedicated;
        }
        (has_normal_task, has_dedicated_task)
    }

    fn is_dedicated(sdata: &SystemData, dedi_tasks: &HashSet<SystemId, S>) -> bool {
        dedi_tasks.contains(&sdata.id())
    }

    fn is_worker_working(idle_workers: &[usize], worker_num: usize) -> bool {
        idle_workers.len() < worker_num
    }

    fn record_run_system(sys_run_history: &mut HashSet<SystemId, S>, sid: SystemId) {
        sys_run_history.insert(sid);
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
    pub(super) fn update_buffer<S>(&mut self, ents: &mut EntityDict<S>, res: &mut ResourcePack<S>)
    where
        S: BuildHasher + Default,
    {
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

        // Updates read resource buffer.
        // The buffer must be consumed by `ResQuery::convert()`.
        debug_assert!(self.buf.res_read.is_empty());
        for ri in self.wait.res_read.iter().cloned() {
            let borrowed = res.borrow(ri).unwrap();
            self.buf.res_read.push(borrowed);
        }

        // Updates write resource buffer.
        // The buffer must be consumed by `ResQueryMut::convert()`.
        debug_assert!(self.buf.res_write.is_empty());
        for ri in self.wait.res_write.iter().cloned() {
            let borrowed = res.borrow_mut(ri).unwrap();
            self.buf.res_write.push(borrowed);
        }

        // Updates entity buffer.
        // The buffer must be consumed by `EntQueryMut::convert()`.
        debug_assert!(self.buf.ent_write.is_empty());
        for ei in self.wait.ent_write.iter().cloned() {
            let borrowed = ents.borrow_mut(EntityKey::Index(ei)).unwrap();
            self.buf.ent_write.push(borrowed);
        }
    }
}

impl<S> Resource for EcsManager<S> where S: 'static {}
