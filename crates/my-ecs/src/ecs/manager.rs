use super::{
    cache::{CacheItem, CacheStorage, RefreshCacheStorage},
    ent::{
        component::ComponentKey,
        entity::{Entity, EntityId, EntityIndex, EntityKey, EntityKeyKind},
        storage::{AsEntityDesc, EntityDesc, EntityStorage},
    },
    resource::{MaybeOwned, Resource, ResourceKey, ResourceStorage},
    sys::{
        storage::{SystemStorage, WorkingSystemGroup},
        system::{
            FnOnceSystem, FnSystem, InsertPos, Invoke, NonZeroTick, PrivateSystem,
            StructOrFnSystem, System, SystemCycleIter, SystemData, SystemGroup, SystemId,
            SystemKey,
        },
    },
    wait::WaitQueues,
    worker::{ChMsg, MainChannel, SubChannel, Work, WorkerIndex},
    EcsError, EcsResult,
};
use crate::{default::prelude::*, ds::prelude::*, util::prelude::*};
use std::{
    any,
    collections::{HashSet, VecDeque},
    fmt::Debug,
    hash::BuildHasher,
    sync::mpsc::TryRecvError,
    thread,
    time::Duration,
};

use sched::*;

/// * `S` - Hasher.
/// * `N` - Number of [`SystemGroup`], which operates in a different configurable way from each other.
//
// We know N > 0 due to the validation in `Multi`.
#[derive(Debug)]
pub struct EcsManager<S = std::hash::RandomState, const N: usize = 2> {
    /// System storage.
    sys: SystemStorage<S, N>,

    /// Entity and component storage.
    /// The storage contains all kinds of entities and components.
    ent: EntityStorage<S>,

    /// Resource storage.
    /// The storage contains pointers to resources.
    res: ResourceStorage<S>,

    cache: CacheStorage<S>,

    sched: Scheduler<S>,
}

impl EcsManager<std::hash::RandomState, 2> {
    pub fn new<W: Work>() -> Self {
        let mut this = Self::default();
        this.set_operation(0, sgroup_op_repetitive::<W, std::hash::RandomState>);
        this.set_operation(1, sgroup_op_reactive::<W, std::hash::RandomState>);
        this
    }
}

impl<S, const N: usize> EcsManager<S, N>
where
    S: BuildHasher + Default + 'static,
{
    pub fn set_operation<W: Work>(&mut self, gi: usize, op: fn(&mut SystemGroup<S>, Cycle<W, S>)) {
        self.sys.get_system_group_mut(gi).set_operation(op);
    }

    pub fn get_entity_storage(&self) -> &EntityStorage<S> {
        &self.ent
    }

    pub fn get_entity_storage_mut(&mut self) -> &mut EntityStorage<S> {
        &mut self.ent
    }

    pub fn get_resource_storage(&self) -> &ResourceStorage<S> {
        &self.res
    }

    pub fn get_resource_storage_mut(&mut self) -> &mut ResourceStorage<S> {
        &mut self.res
    }

    pub fn register_system<T, Sys, Req, F>(
        &mut self,
        gi: usize,
        volatile: bool,
        sys: T,
    ) -> EcsResult<SystemId>
    where
        T: Into<StructOrFnSystem<Sys, Req, F>>,
        Sys: System,
        FnSystem<Req, F>: System,
    {
        let sid = self.sys.get_system_group_mut(gi).next_system_id();
        let sys: StructOrFnSystem<Sys, Req, F> = sys.into();
        let (skey, sdata) = match sys {
            StructOrFnSystem::Struct(s) => {
                let skey = Sys::key();
                let sdata = s.into_data(self.sys.request_info_storage(), sid);
                (skey, sdata)
            }
            StructOrFnSystem::Fn(f) => {
                let skey = f.skey();
                let sdata = f.into_data(self.sys.request_info_storage(), sid);
                (skey, sdata)
            }
        };

        validate_request(self, &sdata)?;
        register_system_inner(self, gi, volatile, skey, sdata)?;
        return Ok(sid);

        // === Internal helper functions ===

        fn validate_request<S, const N: usize>(
            this: &EcsManager<S, N>,
            sdata: &SystemData,
        ) -> EcsResult<()>
        where
            S: BuildHasher + Default + 'static,
        {
            // Validates existence of requested resources & entities.
            //
            // When it comes to "resource & entity queries", in contrast to "component queries",
            // They must be known at the time of system registration.
            // Why? assume that clients didn't register required resource or entity.
            // Then, we can't give systems the unregistered resource or entity.
            // But with respect to component, we can give empty iterator.
            for rkey in sdata.get_request_info().resource_keys() {
                if !this.res.contains(rkey) {
                    let errmsg = debug_format!("failed to find a resource `{:?}`", rkey);
                    return Err(EcsError::UnknownResource(errmsg));
                }
            }
            for ekey in sdata.get_request_info().entity_keys() {
                if !this.ent.contains(ekey) {
                    let errmsg = debug_format!("failed to find an entity `{:?}`", ekey);
                    return Err(EcsError::UnknownEntity(errmsg));
                }
            }

            // Validates if each query is unique.
            // For example, queries for the same resource are not valid.
            let contains_comp = |ekey: &EntityKey, ckey: &ComponentKey| {
                let cont = this.ent.get_entity_container(ekey).unwrap();
                cont.contains_column(ckey.get_inner())
            };
            let has_dup = |ekeys: &[EntityKey]| {
                let mut entis = ekeys
                    .iter()
                    .map(|ekey| {
                        let ekey = this
                            .ent
                            .convert_entity_key(ekey, EntityKeyKind::Index)
                            .unwrap();
                        *ekey.index()
                    })
                    .collect::<Vec<_>>();
                entis.sort_unstable();
                for win in entis.windows(2) {
                    if win[0] == win[1] {
                        return true;
                    }
                }
                false
            };
            if !sdata.get_request_info().is_valid(contains_comp, has_dup) {
                let errmsg = debug_format!(
                    "request {} in system {}",
                    sdata.get_request_info().name(),
                    sdata.get_info().name()
                );
                return Err(EcsError::InvalidRequest(errmsg));
            }

            Ok(())
        }

        fn register_system_inner<S, const N: usize>(
            this: &mut EcsManager<S, N>,
            gi: usize,
            volatile: bool,
            skey: SystemKey,
            sdata: SystemData,
        ) -> EcsResult<()>
        where
            S: BuildHasher + Default + 'static,
        {
            this.cache.create(&sdata, &this.ent, &this.res);
            this.sys
                .register_system(gi, skey, sdata, volatile, &this.res)
        }
    }

    pub fn register_once_system<T, Req, F, Args>(
        &mut self,
        gi: usize,
        sys: T,
    ) -> EcsResult<SystemId>
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
        self.register_system(gi, VOLATILE, boxed)
    }

    /// Activates the system. If the system is already active, nothing takes place.
    pub fn activate_system(
        &mut self,
        target: &SystemId,
        at: InsertPos,
        mut live: NonZeroTick,
    ) -> EcsResult<()> {
        // It can be in the middle of scheduling,
        // which means there may be no chance to run target system in this time.
        // In that case, we need to add `live` by 1 to guarantee the total number of run.
        match at {
            InsertPos::After(after) => {
                let sgroup = self.sys.get_system_group_mut(after.group_index() as usize);
                if let Some(next) = sgroup.cycle().peek_next(after) {
                    if self.sched.get_run_history().contains(&next.id()) {
                        live = live.saturating_add(1);
                    }
                }
            }
            InsertPos::Back => { /* target must be run in this time */ }
            InsertPos::Front => {
                if !self.sched.get_run_history().is_empty() {
                    live = live.saturating_add(1);
                }
            }
        }

        // Activates it.
        self.sys.activate_system(target, at, live)
    }

    pub fn append_system<T, Sys, Req, F>(
        &mut self,
        gi: usize,
        live: NonZeroTick,
        volatile: bool,
        sys: T,
    ) -> EcsResult<SystemId>
    where
        T: Into<StructOrFnSystem<Sys, Req, F>>,
        Sys: System,
        FnSystem<Req, F>: System,
    {
        let res = self.register_system(gi, volatile, sys);
        if let Ok(sid) = res.as_ref() {
            let must_ok = self.activate_system(sid, InsertPos::Back, live);
            debug_assert!(must_ok.is_ok());
        }
        res
    }

    pub fn append_once_system<T, Req, F, Args>(&mut self, gi: usize, sys: T) -> EcsResult<SystemId>
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
        let boxed_sys: Box<dyn FnMut(Args)> = f.into();
        self.append_system(gi, LIVE, VOLATILE, boxed_sys)
    }

    pub fn register_entity_of<T: AsEntityDesc>(&mut self) -> EntityIndex {
        self.register_entity(T::as_entity_descriptor())
    }

    pub fn register_entity(&mut self, desc: EntityDesc) -> EntityIndex {
        // Registers entity.
        let ei = self.ent.register_entity(desc);
        self.cache.update(&self.ent, ei);

        // Makes wait queue for the entity.
        let cont = unsafe {
            self.ent
                .get_entity_container(&EntityKey::from(ei))
                .unwrap_unchecked()
        };
        self.sched
            .get_wait_queues()
            .initialize_entity_queue(ei.index(), cont.get_column_num());

        ei
    }

    pub fn add_entity<E: Entity>(&mut self, ei: EntityIndex, value: E) -> EcsResult<EntityId> {
        if let Some(cont) = self.ent.get_entity_container_mut(&EntityKey::from(ei)) {
            let itemi = value.move_to(&mut **cont);
            Ok(EntityId::new(ei, itemi))
        } else {
            let errmsg = debug_format!("{}", any::type_name::<E>());
            Err(EcsError::UnknownEntity(errmsg))
        }
    }

    /// Registers the resource.
    /// If the registration failed, nothing takes place and returns received value.
    /// In other words, the old resouce data won't be dropped.
    pub fn register_resource(
        &mut self,
        key: ResourceKey,
        value: MaybeOwned,
        is_dedicated: bool,
    ) -> Result<(), MaybeOwned> {
        let index = self.res.register(key, value, is_dedicated)?;
        self.sched
            .get_wait_queues()
            .initialize_resource_queue(index);
        Ok(())
    }

    pub fn set_scheduler_timeout(&mut self, dur: Duration) {
        self.sched.set_wait_timeout(dur);
    }

    pub fn schedule<W>(&mut self, workers: &mut [W])
    where
        W: Work,
    {
        let cache = RefreshCacheStorage {
            cache: &mut self.cache.items,
            ent: &mut self.ent,
            res: &mut self.res,
        };
        self.sched
            .schedule(&mut self.sys.sys, workers, cache, &self.sys.dedi_sys);
    }
}

impl<S, const N: usize> Default for EcsManager<S, N>
where
    S: BuildHasher + Default + 'static,
{
    fn default() -> Self {
        Self {
            sys: SystemStorage::new(),
            ent: EntityStorage::new(),
            res: ResourceStorage::new(),
            cache: CacheStorage::new(),
            sched: Scheduler::new(),
        }
    }
}

impl<S: 'static, const N: usize> Resource for EcsManager<S, N> {}

pub mod sched {
    use super::*;

    #[derive(Debug)]
    pub struct Scheduler<S> {
        main_ch: MainChannel,
        sub_chs: Vec<SubChannel>,

        waits: WaitQueues<S>,

        /// Idle worker indices.
        idle: Vec<usize>,

        /// Pending task positions due to the data dependency.
        pending: SetValueList<ListPos, S>,

        /// Dedicated pending task positions due to the data dependency.
        dedi_pending: VecDeque<ListPos>,

        /// System run history.
        run_record: HashSet<SystemId, S>,

        /// Scheduler will pause when it needs to wait for workers for this timeout.
        wait_timeout: Duration,

        /// System group index that's currently running or waiting.
        cur_gi: usize,
    }

    impl<S> Scheduler<S>
    where
        S: BuildHasher + Default,
    {
        const DEFAULT_WAIT_TIMEOUT: Duration = Duration::from_millis(10);

        pub(super) fn new() -> Self {
            Self {
                main_ch: MainChannel::new(),
                sub_chs: Vec::new(),
                waits: WaitQueues::new(),
                idle: Vec::new(),
                pending: SetValueList::new(ListPos::end()),
                dedi_pending: VecDeque::new(),
                run_record: HashSet::default(),
                wait_timeout: Self::DEFAULT_WAIT_TIMEOUT,
                cur_gi: 0,
            }
        }

        pub(super) fn set_wait_timeout(&mut self, dur: Duration) {
            self.wait_timeout = dur;
        }

        pub(super) fn get_run_history(&self) -> &HashSet<SystemId, S> {
            &self.run_record
        }

        pub(super) fn get_wait_queues(&mut self) -> &mut WaitQueues<S> {
            &mut self.waits
        }

        /// Schedules all systems(tasks).
        pub(super) fn schedule<W, const N: usize>(
            &mut self,
            sgroups: &mut Multi<WorkingSystemGroup<S>, N>,
            workers: &mut [W],
            mut cache: RefreshCacheStorage<S>,
            dedi: &HashSet<SystemId, S>,
        ) where
            W: Work,
        {
            // Extends sub-channel array if needed.
            for i in self.sub_chs.len()..workers.len() {
                self.sub_chs.push(SubChannel::new(
                    self.main_ch.clone_tx(),
                    thread::current(),
                    WorkerIndex::new(i),
                ));
            }

            // Opens workers.
            for (widx, worker) in workers.iter_mut().enumerate() {
                let job = self.sub_chs[widx].job_ptr();
                // Safety: The pointer won't be aliased and they will be valid during scheduling.
                let job = unsafe {
                    let ptr = NonNullExt::new_unchecked(job.cast_mut());
                    ManagedConstPtr::new(ptr)
                };
                let must_true = worker.unpark(job);
                assert!(must_true);
                self.idle.push(widx);
            }

            for (i, sgroup) in sgroups.iter_mut().enumerate() {
                self.cur_gi = i;
                unsafe {
                    sgroup.operate_unchecked(Cycle {
                        sched: self,
                        workers,
                        cache: &mut cache,
                        dedi,
                    });
                }

                #[cfg(debug_assertions)]
                self.validate(workers);
            }

            // Clears system run history.
            self.run_record.clear();

            // Exits message loop.
            for sub_ch in self.sub_chs.iter() {
                sub_ch.send(ChMsg::End).unwrap();
            }

            // Closes workers.
            for worker in workers.iter_mut() {
                let must_true = worker.park();
                assert!(must_true);
            }
            self.idle.clear();
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
            waits: &mut WaitQueues<S>,
            idle: &mut Vec<usize>,
            cache: &mut RefreshCacheStorage<S>,
        ) -> bool {
            match ch.try_recv() {
                Ok(ChMsg::Fin(widx, sid)) => {
                    let cache = cache.get(&sid).unwrap();
                    waits.dequeue(&cache.get_wait_indices());
                    idle.push(widx.into_inner());
                    true
                }
                Err(TryRecvError::Empty) => {
                    // According to Rust description,
                    // thread::park() can be unparked unconditionally.
                    // (https://doc.rust-lang.org/std/thread/fn.park.html)
                    // We don't have to do something in that case.
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
        /// which means there's no data dependency at the moment.
        /// If it's runnable, the system's cached buffer is updated and then returned.
        fn try_update_task<'a>(
            sdata: &mut SystemData,
            waits: &mut WaitQueues<S>,
            cache: &'a mut RefreshCacheStorage<S>,
        ) -> Option<&'a mut CacheItem> {
            let sid = sdata.id();
            let mut cache = cache.get_mut(&sid).unwrap();
            let (wait, retry) = cache.get_wait_retry_indices_mut();
            if waits.enqueue(&wait, retry) {
                // Before we send the data, we need to update(re-borrow) it.
                drop(wait);
                Some(cache.refresh())
            } else {
                None
            }
        }

        /// Tries to give the task `sdata` to the `worker`.
        /// If it succeeded, returns true.
        fn try_give(
            sdata: &mut SystemData,
            sub_ch: &SubChannel,
            waits: &mut WaitQueues<S>,
            cache: &mut RefreshCacheStorage<S>,
        ) -> bool {
            if let Some(cache) = Self::try_update_task(sdata, waits, cache) {
                // Safety: Scheduler guarantees those pointers will be used in the worker only.
                let (task_ptr, buf_ptr) = unsafe {
                    (
                        ManagedMutPtr::new(sdata.task_ptr()),
                        ManagedMutPtr::new(cache.request_buffer_ptr()),
                    )
                };
                let task = ChMsg::Task(task_ptr, buf_ptr, sdata.id());
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
            waits: &mut WaitQueues<S>,
            cache: &mut RefreshCacheStorage<S>,
        ) -> bool {
            if let Some(cache) = Self::try_update_task(sdata, waits, cache) {
                crate::log!("@@@ do self {}", sdata.get_info().name());
                let invokable = sdata.invokable_mut();

                // NOTE: If `Scheduler` is modified, `cached` could be moved in `invoke()`.
                // Therefore, `dequeue()` must be called before it.
                // It's fine because the system is a dedicated task.
                waits.dequeue(&cache.get_wait_indices());
                invokable.invoke(cache.get_request_buffer_mut());
                true
            } else {
                false
            }
        }

        fn remains_task(
            pending: &SetValueList<ListPos, S>,
            dedi_pending: &VecDeque<ListPos>,
            cycle: &mut SystemCycleIter<S>,
            dedi: &HashSet<SystemId, S>,
        ) -> (bool, bool) {
            let mut has_normal = pending.len_occupied() > 0;
            let mut has_dedi = !dedi_pending.is_empty();
            if let Some(sdata) = cycle.get() {
                let is_dedi = dedi.contains(&sdata.id());
                has_dedi |= is_dedi;
                has_normal |= !is_dedi;
            }
            (has_normal, has_dedi)
        }

        fn is_worker_working(idle: &[usize], num_workers: usize) -> bool {
            idle.len() < num_workers
        }

        fn record_run_system(run_record: &mut HashSet<SystemId, S>, sid: SystemId) {
            run_record.insert(sid);
        }

        fn validate<W: Work>(&self, workers: &[W]) {
            // No pending tasks.
            assert_eq!(0, self.pending.len_occupied());
            // No dedicated pending tasks.
            assert!(self.dedi_pending.is_empty());
            // All workers must be idle.
            assert_eq!(workers.len(), self.idle.len());
            // No waiting request. Takes O(n).
            assert!(self.waits.is_all_queue_empty());
        }

        fn run_cycle<W>(
            &mut self,
            sys: &mut SystemGroup<S>,
            workers: &mut [W],
            cache: &mut RefreshCacheStorage<S>,
            dedi: &HashSet<SystemId, S>,
        ) {
            let num_workers = workers.len();
            let Self {
                main_ch,
                sub_chs,
                waits,
                idle,
                pending,
                dedi_pending,
                wait_timeout,
                run_record,
                ..
            } = self;
            let mut cycle = sys.cycle().iter_begin();
            let mut last_seen = LastSeen::new();

            'outer: loop {
                while Self::try_recv(main_ch, waits, idle, cache) {}

                let (remains_normal_task, remains_dedicated_task) =
                    Self::remains_task(pending, dedi_pending, &mut cycle, dedi);

                // We have three options.
                // 1. We can do something. Keeps going on.
                // 2. We cannot make progress. Waits.
                // 3. All tasks done. Exits.
                // Match arms below cover those three cases.
                match (
                    remains_normal_task,
                    remains_dedicated_task,
                    idle.is_empty(),
                    Self::is_worker_working(idle, num_workers),
                ) {
                    // 1. We can do something. Keeps going.
                    // - If we have dedicated tasks, keeps proceeding.
                    (_, true, _, _) |
                    // - Or, if we have both normal tasks and workers,
                    // keeps proceeding.
                    (true, _, false, _) => {
                        // But, if we're stuck due to data dependency, waits.
                        if !last_seen.has_changed(pending, dedi_pending, &cycle, waits) {
                            thread::park_timeout(*wait_timeout);
                        }
                    }

                    // 2. We cannot make progress. Waits.
                    // - If we have normal tasks, but no workers, waits.
                    (true, _, true, _) |
                    // - Or, if any worker is working, waits.
                    (_, _, _, true) => thread::park_timeout(*wait_timeout),

                    // 3. All tasks done. Exits.
                    // - If we don't have any tasks, exits.
                    (false, false, _, _) => break 'outer,
                }

                if remains_normal_task {
                    if let Some(widx) = idle.last() {
                        let sub_ch = &sub_chs[*widx];

                        // Tries to give the worker a pending task first.
                        for pos in pending.iter().cloned() {
                            // Safety: Pending system position is valid.
                            let sdata = unsafe { cycle.get_at(pos).unwrap_unchecked() };
                            if Self::try_give(sdata, sub_ch, waits, cache) {
                                idle.pop();
                                pending.remove(&pos);
                                continue 'outer;
                            }
                            if Self::try_recv(main_ch, waits, idle, cache) {
                                continue 'outer;
                            }
                        }

                        // Searches not pending tasks after pending tasks.
                        while let Some(sdata) = cycle.get() {
                            // The system's request is definitely going to be inserted into wait queue.
                            Self::record_run_system(run_record, sdata.id());

                            if dedi.contains(&sdata.id()) {
                                if !Self::try_do_self(sdata, waits, cache) {
                                    dedi_pending.push_back(cycle.position());
                                }
                            } else if Self::try_give(sdata, sub_ch, waits, cache) {
                                idle.pop();
                                cycle.next();
                                continue 'outer;
                            } else {
                                pending.push_back(cycle.position());
                            }
                            cycle.next();

                            if Self::try_recv(main_ch, waits, idle, cache) {
                                continue 'outer;
                            }
                        }
                    }
                }

                if remains_dedicated_task {
                    if let Some(pos) = dedi_pending.front() {
                        // Safety: Pending system position is valid.
                        let sdata = unsafe { cycle.get_at(*pos).unwrap_unchecked() };
                        if Self::try_do_self(sdata, waits, cache) {
                            dedi_pending.pop_front();
                        }
                    } else if let Some(sdata) = cycle.get() {
                        if dedi.contains(&sdata.id()) {
                            // The system's request is definitely going to be inserted into wait queue.
                            Self::record_run_system(run_record, sdata.id());

                            if !Self::try_do_self(sdata, waits, cache) {
                                dedi_pending.push_back(cycle.position());
                            }
                            cycle.next();
                        }
                    }
                    if Self::try_recv(main_ch, waits, idle, cache) {
                        continue 'outer;
                    }
                }
            }

            // === Internal structs and functions ===

            #[derive(Debug, PartialEq, Eq, Clone, Copy)]
            struct LastSeen {
                pending: ListPos,
                dedi_pending: ListPos,
                cycle: ListPos,
                wait_gen: u64,
            }

            impl LastSeen {
                const fn new() -> Self {
                    Self {
                        pending: ListPos::end(),
                        dedi_pending: ListPos::end(),
                        cycle: ListPos::end(),
                        wait_gen: 0,
                    }
                }

                fn has_changed<S>(
                    &mut self,
                    pending: &SetValueList<ListPos, S>,
                    dedi_pending: &VecDeque<ListPos>,
                    cycle: &SystemCycleIter<S>,
                    waits: &WaitQueues<S>,
                ) -> bool
                where
                    S: BuildHasher,
                {
                    let cur = LastSeen {
                        pending: pending.front().cloned().unwrap_or(ListPos::end()),
                        dedi_pending: dedi_pending.front().cloned().unwrap_or(ListPos::end()),
                        cycle: cycle.position(),
                        wait_gen: waits.generation(),
                    };
                    let changed = self != &cur;
                    *self = cur;
                    changed
                }
            }
        }
    }

    pub struct Cycle<'a, 'b: 'a, W, S> {
        sched: &'a mut Scheduler<S>,
        workers: &'a mut [W],
        cache: &'a mut RefreshCacheStorage<'b, S>,
        dedi: &'a HashSet<SystemId, S>,
    }

    impl<'a, 'b, W, S> Cycle<'a, 'b, W, S>
    where
        S: BuildHasher + Default,
    {
        pub fn run_cycle(&mut self, sys: &mut SystemGroup<S>) {
            self.sched
                .run_cycle(sys, self.workers, self.cache, self.dedi);
        }
    }
}
