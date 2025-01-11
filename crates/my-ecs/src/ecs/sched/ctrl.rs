use super::{
    comm::{
        self, CommandReceiver, CommandSender, GlobalSignal, ParkingReceiver, ParkingSender,
        SubComm, WORKER_ID_GEN,
    },
    par::FnContext,
    task::{ParTask, SysTask, Task},
};
use crate::{
    ds::prelude::*,
    ecs::{
        cache::{CacheItem, RefreshCacheStorage},
        cmd::{Command, CommandObject},
        entry::Ecs,
        share::Shared,
        stat,
        sys::system::{RawSystemCycleIter, SystemCycleIter, SystemData, SystemGroup, SystemId},
        wait::WaitQueues,
        worker::{Message, PanicMessage, Work, WorkerId},
        DynResult,
    },
    util::prelude::*,
};
use crossbeam_deque as cb;
use std::{
    any::Any,
    array,
    cell::{Cell, UnsafeCell},
    collections::{HashMap, VecDeque},
    future::Future,
    hash::BuildHasher,
    marker::PhantomPinned,
    mem,
    ops::Deref,
    pin::Pin,
    ptr::NonNull,
    rc::Rc,
    sync::Arc,
    task::Poll,
    thread::{self, Thread},
    time::Duration,
};

#[derive(Debug)]
pub(crate) struct Scheduler<W, S, const N: usize> {
    wgroups: [WorkGroup<W>; N],

    waits: WaitQueues<S>,

    /// System run record.
    record: ScheduleRecord<S>,

    /// A list holding pending tasks due to data dependency.
    nor_pendings: [Pending<S>; N],

    /// A list holding pending tasks due to data dependency.
    dedi_pendings: [Pending<S>; N],

    /// Ready dedicated tasks.
    dedi: VecDeque<Task>,

    /// Channel sending messages to main worker.
    tx_msg: ParkingSender<Message>,

    /// Channel receving messages from sub workers.
    rx_msg: ParkingReceiver<Message>,

    /// Channel receving commands from sub workers.
    rx_cmd: Rc<CommandReceiver>,

    /// Main worker id.
    wid: WorkerId,
}

impl<W, S, const N: usize> Scheduler<W, S, N> {
    /// Returns number of open sub workers.
    pub(crate) fn is_work_groups_exhausted(&self) -> bool {
        self.wgroups.iter().all(|wg| wg.is_exhausted())
    }

    /// Determines whether scheduler doesn't have any uncompleted commands.
    pub(crate) fn has_command(&self) -> bool {
        self.rx_cmd.has()
    }

    pub(crate) fn wait_exhausted(&self) {
        for wg in self.wgroups.iter() {
            wg.wait_exhausted();
        }
    }

    /// Destroys this scheduler and then returns internal worker array.
    //
    // Scheduler cannot exist without workers, so consumes it.
    pub(crate) fn take_workers(mut self) -> Vec<W> {
        self.wgroups.iter_mut().fold(Vec::new(), |mut acc, wgroup| {
            acc.append(&mut wgroup.take_workers());
            acc
        })
    }

    pub(crate) fn get_wait_queues_mut(&mut self) -> &mut WaitQueues<S> {
        &mut self.waits
    }

    fn work_one(&mut self) {
        if let Some(task) = self.dedi.pop_front() {
            // NOTE: Panics can occur here.
            // TODO: Handle panic?
            match task {
                Task::System(task) => {
                    let sid = task.sid();
                    let resp = match task.execute() {
                        Ok(_) => Message::Fin(self.wid, sid),
                        Err(payload) => Message::Panic(PanicMessage {
                            wid: self.wid,
                            sid,
                            payload,
                            unrecoverable: false,
                        }),
                    };

                    // Even if main worker, it needs to send Fin message to
                    // release data dependency.
                    self.tx_msg.send(resp).unwrap();
                }
                _ => {
                    debug_assert!(
                        false,
                        "parallel or async task is not allowed on main worker"
                    );
                }
            }
        }
    }
}

impl<W, S, const N: usize> Scheduler<W, S, N>
where
    W: Work + 'static,
    S: BuildHasher + Default + 'static,
{
    pub(crate) fn new(
        mut workers: Vec<W>,
        nums: [usize; N],
        shared: &Arc<Shared>,
        tx_cmd: &CommandSender,
        rx_cmd: Rc<CommandReceiver>,
    ) -> Self {
        assert_eq!(workers.len(), nums.iter().sum::<usize>());

        let pending_limit: usize = workers.len();
        let nor_pendings = array::from_fn(|_| Pending::new(pending_limit));
        let dedi_pendings = array::from_fn(|_| Pending::new(pending_limit));
        let (tx_msg, rx_msg) = comm::parking_channel(thread::current());

        let wgroups = array::from_fn(|i| {
            // Splits off left piece from entire worker array.
            let mut piece = workers.split_off(nums[i]);
            mem::swap(&mut workers, &mut piece);

            // Creates sub context group and initializes it.
            let mut group = WorkGroup::new(i as u16, piece, &tx_msg, tx_cmd, shared);
            group.initialize(&rx_msg);
            group
        });

        let id = WORKER_ID_GEN.get();
        WORKER_ID_GEN.set(id + 1);
        let wid = WorkerId::new(id, u16::MAX, u16::MAX);

        Self {
            wgroups,
            waits: WaitQueues::new(),
            record: ScheduleRecord::new(),
            nor_pendings,
            dedi_pendings,
            dedi: VecDeque::new(),
            tx_msg,
            rx_msg,
            rx_cmd,
            wid,
        }
    }

    pub(crate) fn execute_all(
        &mut self,
        sgroups: &mut [SystemGroup<S>; N],
        cache: &mut RefreshCacheStorage<S>,
    ) {
        // # Procedures
        // 1. Opens sub workers through `WorkGroup`s.
        // 2. Creates `ScheduleUnit` for each `WorkGroup` and `SystemGroup`,
        //    then runs every `ScheduleUnit`.
        // 3. Cleans up.

        // 1. Opens work groups.
        let mut lives: [bool; N] = array::from_fn(|i| sgroups[i].len_active() > 0);
        let tickables = lives;
        for (i, _) in lives.iter().enumerate().filter(|(_, &live)| live) {
            self.wgroups[i].open();
        }

        // 2. Runs schedule units. This procedure follows the order below.
        // (1) Tries to pull system tasks as many as possible.
        // (2) If there are dedicated system tasks, handles just one. Because
        //     we want to make sub workers busy rather than doing something.
        // (3) If all system tasks are pulled or handled, escapes the loop.
        let mut panicked = Vec::new();
        let mut units: [_; N] = array::from_fn(|i| ScheduleUnit::new(i, self, sgroups, cache));
        loop {
            // (1) Pulls system tasks from cycles and inject them into work
            // groups.
            for (i, live) in lives.iter_mut().enumerate().filter(|(_, live)| **live) {
                let pull_end = units[i].pull_many() == PullRes::Empty;
                let no_pending = !self.has_pending(i);
                if pull_end && no_pending {
                    self.wgroups[i].close();
                    *live = false;
                }
            }

            // (2) If we have dedicated system task, handles it.
            self.work_one();
            if !self.dedi.is_empty() {
                continue;
            }

            // (3) If any cycle is not empty, waits for messages. Otherwise
            // breaks the loop.
            if lives.iter().any(|&live| live) {
                self.wait(&mut units, cache, &mut panicked);
            } else {
                self.consume_messages(cache, &mut panicked);
                break;
            }
        }

        // 3. Gets cleaned up.
        // - Makes panicked systems be poisoned.
        // - Increases tick.
        // - Resets run record.
        drop(units);
        while let Some((sid, payload)) = panicked.pop() {
            sgroups[sid.group_index() as usize]
                .poison(&sid, payload)
                .unwrap();
        }
        for i in 0..N {
            if tickables[i] {
                sgroups[i].tick();
            }
        }
        self.record.clear();

        #[cfg(debug_assertions)]
        self.validate_clean();
    }

    fn wait(
        &mut self,
        units: &mut [ScheduleUnit<'_, W, S, N>; N],
        cache: &mut RefreshCacheStorage<'_, S>,
        panicked: &mut Vec<(SystemId, Box<dyn Any + Send>)>,
    ) {
        // Consumes buffered messages as many as possible.
        if let Ok(msg) = self.rx_msg.recv_timeout(Duration::MAX) {
            self.handle_message(msg, cache, panicked);
        }
        while let Ok(msg) = self.rx_msg.try_recv() {
            self.handle_message(msg, cache, panicked);
        }

        self.pending_to_ready(units, cache);
    }

    fn consume_messages(
        &mut self,
        cache: &mut RefreshCacheStorage<'_, S>,
        panicked: &mut Vec<(SystemId, Box<dyn Any + Send>)>,
    ) {
        while self.record.num_injected() > self.record.num_completed() {
            if let Ok(msg) = self.rx_msg.recv_timeout(Duration::MAX) {
                self.handle_message(msg, cache, panicked);
            }
        }
    }

    fn handle_message(
        &mut self,
        msg: Message,
        cache: &mut RefreshCacheStorage<'_, S>,
        panicked: &mut Vec<(SystemId, Box<dyn Any + Send>)>,
    ) {
        match msg {
            Message::Handle(..) => unreachable!(),
            Message::Fin(_wid, sid) => {
                self.record.insert(sid, RunResult::Finished);
                let cache = cache.get(&sid).unwrap();
                self.waits.dequeue(&cache.get_wait_indices());
            }
            Message::Aborted(_wid, sid) => {
                self.record.insert(sid, RunResult::Aborted);
                let cache = cache.get(&sid).unwrap();
                self.waits.dequeue(&cache.get_wait_indices());
            }
            Message::Panic(msg) => {
                self.record.insert(msg.sid, RunResult::Panicked);
                self.panic_helper(cache, panicked, msg);
            }
        };
    }

    fn pending_to_ready(
        &mut self,
        units: &mut [ScheduleUnit<'_, W, S, N>; N],
        cache: &mut RefreshCacheStorage<'_, S>,
    ) {
        #[allow(clippy::needless_range_loop)] // indexing more than one.
        for i in 0..N {
            // @@@ Safety:
            unsafe {
                // For normal tasks.
                let target = NonNull::new_unchecked(&mut self.wgroups[i] as *mut _);
                Helper::pending_to_ready::<W, S>(
                    Or::A(target),
                    &mut self.nor_pendings[i],
                    &mut self.waits,
                    &mut units[i].cycle(),
                    cache,
                );

                // For dedi tasks.
                let target = NonNull::new_unchecked(&mut self.dedi as &mut _);
                Helper::pending_to_ready::<W, S>(
                    Or::B(target),
                    &mut self.dedi_pendings[i],
                    &mut self.waits,
                    &mut units[i].cycle(),
                    cache,
                );
            }
        }
    }

    fn panic_helper(
        &mut self,
        cache: &mut RefreshCacheStorage<S>,
        panicked: &mut Vec<(SystemId, Box<dyn Any + Send>)>,
        msg: PanicMessage,
    ) {
        if msg.unrecoverable {
            panic!("unrecoverable");
        }

        let cache = {
            #[cfg(not(target_arch = "wasm32"))]
            {
                cache.get(&msg.sid).unwrap()
            }

            // In web, buffer is not released by [`BufferCleaner`] because
            // wasm-bindgen make it abort instead of unwinding stack.
            // So, we need to release it manually.
            #[cfg(target_arch = "wasm32")]
            {
                let mut cache = cache.get_mut(&msg.sid).unwrap();
                let buf = cache.get_request_buffer_mut();
                buf.clear();
                cache
            }
        };

        self.waits.dequeue(&cache.get_wait_indices());
        panicked.push((msg.sid, msg.payload));

        #[cfg(target_arch = "wasm32")]
        {
            // The worker must be closed without notification.
            // Re-open it with `Search` state so that it can
            // empty its local queue.
            debug_assert_eq!(msg.sid.group_index(), msg.wid.group_index());
            let gi = msg.wid.group_index() as usize;
            let wi = msg.wid.worker_index() as usize;

            self.wgroups[gi].insert_search(wi);
        }
    }

    fn has_pending(&self, index: usize) -> bool {
        !self.nor_pendings[index].is_empty() || !self.dedi_pendings[index].is_empty()
    }

    #[cfg(debug_assertions)]
    fn validate_clean(&self) {
        // Validates if there's no waiting requests. It takes O(n).
        assert!(self.waits.is_all_queue_empty());

        // Validates if system record is clean.
        assert!(self.record.is_empty());

        // Validates if there's no pending tasks.
        for i in 0..N {
            assert!(!self.has_pending(i));
        }

        // Validates if there's no uncompleted dedicated system.
        assert!(self.dedi.is_empty());

        // Validates if message channel is empty.
        match self.rx_msg.try_recv() {
            Err(std::sync::mpsc::TryRecvError::Empty) => {}
            Ok(msg) => panic!("unexpected remaining msg in channel: {msg:?}"),
            Err(err) => panic!("unexpected error from channel: {err:?}"),
        }
    }
}

#[derive(Debug)]
struct ScheduleUnit<'s, W, S, const N: usize> {
    // Own data.
    cycle: RawSystemCycleIter<S>,

    // From `Scheduler`.
    wgroup: NonNull<WorkGroup<W>>,
    waits: NonNull<WaitQueues<S>>,
    record: NonNull<ScheduleRecord<S>>,
    nor_pendings: NonNull<[Pending<S>; N]>,
    dedi_pendings: NonNull<[Pending<S>; N]>,
    dedi: NonNull<VecDeque<Task>>,

    // From others.
    cache: NonNull<RefreshCacheStorage<'s, S>>,
}

impl<'s, W, S, const N: usize> ScheduleUnit<'s, W, S, N>
where
    W: Work + 'static,
    S: BuildHasher + Default + 'static,
{
    fn new(
        index: usize,
        sched: &mut Scheduler<W, S, N>,
        sgroups: &mut [SystemGroup<S>; N],
        cache: &mut RefreshCacheStorage<'s, S>,
    ) -> Self {
        let cycle = sgroups[index].get_active_mut().iter_begin().into_raw();

        // Safety: Infallible. Also these pointers will never be dereferenced
        // in a violated way.
        unsafe {
            Self {
                // Own data.
                cycle,
                // From `Scheduler`.
                wgroup: NonNull::new_unchecked(sched.wgroups.get_unchecked_mut(index) as *mut _),
                waits: NonNull::new_unchecked(&mut sched.waits as *mut _),
                record: NonNull::new_unchecked(&mut sched.record as *mut _),
                nor_pendings: NonNull::new_unchecked(&mut sched.nor_pendings as *mut _),
                dedi_pendings: NonNull::new_unchecked(&mut sched.dedi_pendings as *mut _),
                dedi: NonNull::new_unchecked(&mut sched.dedi as *mut _),
                // From others.
                cache: NonNull::new_unchecked(cache as *mut _),
            }
        }
    }

    fn pull_many(&mut self) -> PullRes {
        loop {
            match self.pull_one() {
                PullRes::Empty => return PullRes::Empty,
                PullRes::Success => {}
                PullRes::PendingFull => return PullRes::PendingFull,
            }
        }
    }

    fn pull_one(&mut self) -> PullRes {
        // There are four possible cases here.
        // 1. No tasks to pull      : Does nothing.
        // 2. Data dependency? No   : Appends it onto (unbounded) ready queue.
        // 3. Data dependency? Yes  : Appends it onto pending queue.
        // 4. Pending queue is full : Does nothing.

        // 1. No tasks.
        let mut cycle = self.cycle();
        if cycle.position().is_end() {
            return PullRes::Empty;
        }

        // Takes out one system from the cycle list.
        let sdata = cycle.get().unwrap();
        let sid = sdata.id();
        let gi = sid.group_index() as usize;
        let (pending, target) = if sdata.flags().is_dedi() {
            (&mut self.dedi_pendings()[gi], Or::B(self.dedi))
        } else {
            (&mut self.nor_pendings()[gi], Or::A(self.wgroup))
        };

        // 2. No data dependency : Ready queue.
        if let Some(cache) = Helper::update_task(self.waits(), sdata, self.cache()) {
            self.record().insert(sid, RunResult::Injected);
            Helper::move_ready_system(target, sdata, cache);
            unsafe { self.cycle.next() };
            PullRes::Success
        }
        // 3. Data dependency detected : Pending queue.
        else if pending.push(cycle.position()) {
            self.record().insert(sid, RunResult::Injected);
            unsafe { self.cycle.next() };
            PullRes::Success
        }
        // 4. Pending queue is full : Ignore.
        else {
            PullRes::PendingFull
        }
    }

    // === Pointer helper methods ===

    fn cycle<'o>(&mut self) -> SystemCycleIter<'o, S> {
        // Safety: While this reference exists, `Scheduler` prevents the memory
        // won't be get accessed by other pointer or references.
        unsafe { SystemCycleIter::from_raw(self.cycle) }
    }

    fn waits<'o>(&mut self) -> &'o mut WaitQueues<S> {
        // Safety: While this reference exists, `Scheduler` prevents the memory
        // won't be get accessed by other pointer or references.
        unsafe { self.waits.as_mut() }
    }

    fn record<'o>(&mut self) -> &'o mut ScheduleRecord<S> {
        // Safety: While this reference exists, `Scheduler` prevents the memory
        // won't be get accessed by other pointer or references.
        unsafe { self.record.as_mut() }
    }

    fn nor_pendings<'o>(&mut self) -> &'o mut [Pending<S>; N] {
        // Safety: While this reference exists, `Scheduler` prevents the memory
        // won't be get accessed by other pointer or references.
        unsafe { self.nor_pendings.as_mut() }
    }

    fn dedi_pendings<'o>(&mut self) -> &'o mut [Pending<S>; N] {
        // Safety: While this reference exists, `Scheduler` prevents the memory
        // won't be get accessed by other pointer or references.
        unsafe { self.dedi_pendings.as_mut() }
    }

    fn cache<'o>(&mut self) -> &'o mut RefreshCacheStorage<'s, S> {
        // Safety: While this reference exists, `Scheduler` prevents the memory
        // won't be get accessed by other pointer or references.
        unsafe { self.cache.as_mut() }
    }
}

impl<W, S, const N: usize> Drop for ScheduleUnit<'_, W, S, N> {
    fn drop(&mut self) {
        // We've consumed cycle completely?
        debug_assert!(self.cycle.position().is_end());
    }
}

struct Helper;

impl Helper {
    /// Determines if the system(task) is runnable which means there's no data
    /// dependency at the moment. If it's runnable, the system's cached buffer
    /// is updated and then returned.
    fn update_task<'a, S>(
        waits: &mut WaitQueues<S>,
        sdata: &mut SystemData,
        cache: &'a mut RefreshCacheStorage<S>,
    ) -> Option<&'a mut CacheItem>
    where
        S: BuildHasher + Default + 'static,
    {
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

    fn pending_to_ready<W, S>(
        target: Or<NonNull<WorkGroup<W>>, NonNull<VecDeque<Task>>>,
        pending: &mut Pending<S>,
        waits: &mut WaitQueues<S>,
        cycle: &mut SystemCycleIter<'_, S>,
        cache: &mut RefreshCacheStorage<'_, S>,
    ) where
        S: BuildHasher + Default + 'static,
        W: Work + 'static,
    {
        let mut cur = pending.get_first_position();

        while let Some((next, &cycle_pos)) = pending.iter_next(cur) {
            let sdata = cycle.get_at(cycle_pos).unwrap();
            if let Some(cache) = Self::update_task(waits, sdata, cache) {
                pending.remove(&cycle_pos);
                Self::move_ready_system(target, sdata, cache);
            }
            cur = next;
        }
    }

    fn move_ready_system<W>(
        target: Or<NonNull<WorkGroup<W>>, NonNull<VecDeque<Task>>>,
        sdata: &mut SystemData,
        cache: &mut CacheItem,
    ) where
        W: Work + 'static,
    {
        let sid = sdata.id();

        // Safety:
        // - `invoker` and `buf` is safe because `Scheduler` guarantees that
        //   those pointers will be accessed in a sub worker only.
        // - `target` is safe because `Scheduler` guarantees that its pointer
        //   will be accessed in a single `ScheduleUnit` only at a time.
        unsafe {
            let mut invoker = sdata.task_ptr();
            let buf = ManagedMutPtr::new(cache.request_buffer_ptr());

            // There are three branches here.
            // 1. Private system task   : Main worker handles it.
            // 2. Normal system task    : Appends it onto `wgroup`'s injector.
            // 3. Dedicated system task : Appends it onto `dedi` queue.

            if sdata.flags().is_private() {
                invoker.invoke_private(sid, buf);
            } else {
                let task = Task::System(SysTask::new(invoker, buf, sid));
                match target {
                    Or::A(wgroup) => wgroup.as_ref().inject_task(task),
                    Or::B(mut dedi) => dedi.as_mut().push_back(task),
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PullRes {
    Empty,
    Success,
    PendingFull,
}

#[derive(Debug)]
pub(crate) struct ScheduleRecord<S> {
    record: HashMap<SystemId, RunResult, S>,
    injected: usize,
    finished: usize,
    panicked: usize,
    aborted: usize,
}

impl<S> ScheduleRecord<S>
where
    S: BuildHasher + Default + 'static,
{
    fn new() -> Self {
        Self {
            record: HashMap::default(),
            injected: 0,
            finished: 0,
            panicked: 0,
            aborted: 0,
        }
    }

    #[cfg(debug_assertions)]
    pub(crate) fn len(&self) -> usize {
        self.record.len()
    }

    #[cfg(debug_assertions)]
    pub(crate) fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub(crate) fn clear(&mut self) {
        self.record.clear();
        self.injected = 0;
        self.finished = 0;
        self.panicked = 0;
        self.aborted = 0;
    }

    pub(crate) fn num_injected(&self) -> usize {
        self.injected
    }

    pub(crate) fn num_completed(&self) -> usize {
        self.finished + self.panicked
    }

    fn insert(&mut self, sid: SystemId, state: RunResult) {
        match state {
            RunResult::Injected => self.injected += 1,
            RunResult::Finished => self.finished += 1,
            RunResult::Panicked => self.panicked += 1,
            RunResult::Aborted => self.aborted += 1,
        }
        self.record.insert(sid, state);
    }
}

#[derive(Debug)]
pub(crate) enum RunResult {
    Injected,
    Finished,
    Panicked,
    Aborted,
}

#[derive(Debug)]
struct Pending<S> {
    /// Pending system positions in a system cycle.
    list: SetValueList<ListPos, S>,

    /// Maximum number of pending systems.
    limit: usize,
}

impl<S> Pending<S>
where
    S: BuildHasher + Default,
{
    fn new(limit: usize) -> Self {
        Self {
            list: SetValueList::new(ListPos::end()),
            limit,
        }
    }

    fn is_empty(&self) -> bool {
        self.list.is_occupied_empty()
    }

    fn push(&mut self, pos: ListPos) -> bool {
        if self.list.len_occupied() < self.limit {
            self.list.push_back(pos);
            true
        } else {
            false
        }
    }

    fn remove(&mut self, pos: &ListPos) {
        self.list.remove(pos);
    }
}

// Do not implement `DerefMut` to prevent pusing over the `limit`.
impl<S> Deref for Pending<S> {
    type Target = SetValueList<ListPos, S>;

    fn deref(&self) -> &Self::Target {
        &self.list
    }
}

#[derive(Debug)]
struct WorkGroup<W> {
    /// Sub workers belonging to this group.
    workers: Vec<W>,

    /// Context for each sub worker.
    sub_cxs: Vec<Pin<Box<SubContext>>>,

    /// Signal to wait or be waken by others in this group.
    signal: Arc<GlobalSignal>,

    /// Main worker gives this group tasks through this injector.
    injector: Arc<cb::Injector<Task>>,
}

impl<W> WorkGroup<W> {
    fn len(&self) -> usize {
        self.sub_cxs.len()
    }

    fn take_workers(&mut self) -> Vec<W> {
        mem::take(&mut self.workers)
    }

    fn wait_exhausted(&self) {
        while !self.is_exhausted() {
            self.signal.wait_open_count(0);
        }
    }

    fn is_exhausted(&self) -> bool {
        // If guidance queue is empty, it means that sub workers cannot open
        // themselves again.
        self.sub_cxs
            .iter()
            .all(|cx| cx.guide.is_empty())
        &&
        // Queue is empty, Also there's no open sub workers, they are completely
        // in closed states.
        self.signal.open_count() == 0
    }

    fn inject_task(&self, task: Task) {
        debug_assert!(
            !self.workers.is_empty(),
            "no workers for a non-dedicated task"
        );

        self.injector.push(task);
        self.signal.sub().notify_one();
    }

    #[cfg(debug_assertions)]
    fn validate_clean(&self) {
        // Validates sub contexts.
        for cx in self.sub_cxs.iter() {
            cx.validate_clean();
        }

        // Validates signal.
        assert_eq!(self.signal.open_count(), 0);
        assert_eq!(self.signal.work_count(), 0);
        assert_eq!(self.signal.future_count(), 0);

        // Validates if injector is empty.
        assert!(self.injector.is_empty());
    }
}

impl<W> WorkGroup<W>
where
    W: Work + 'static,
{
    fn new(
        group_index: u16,
        workers: Vec<W>,
        tx_msg: &ParkingSender<Message>,
        tx_cmd: &CommandSender,
        shared: &Arc<Shared>,
    ) -> Self {
        // Creates global queue.
        let injector = Arc::new(cb::Injector::new());

        // This signal will be replaced at `initialize` with sub worker's
        // handles.
        let dummy_signal = Arc::new(GlobalSignal::new(Vec::new()));

        let comms = SubComm::with_len(
            group_index,
            &injector,
            &dummy_signal,
            tx_msg,
            tx_cmd,
            workers.len(),
        );

        // Sub contexts.
        let sub_cxs = comms
            .into_iter()
            .map(|comm| {
                Box::pin(SubContext {
                    guide: sub::SubStateGuide::new(),
                    // Puts in dummy handle for now. Sub workers will overwrite
                    // their handles at their first execution.
                    handle: UnsafeCell::new(thread::current()),
                    comm,
                    need_close: Cell::new(false),
                    shared: Arc::clone(shared),
                    _pin: PhantomPinned,
                })
            })
            .collect();

        Self {
            workers,
            sub_cxs,
            signal: dummy_signal,
            injector,
        }
    }

    fn initialize(&mut self, rx_msg: &ParkingReceiver<Message>) {
        // Sub worker will send thread handle at its first open.
        for i in 0..self.len() {
            self.unpark_one(i);
        }

        let mut remain = self.len();
        while remain > 0 {
            if let Ok(msg) = rx_msg.recv_timeout(Duration::MAX) {
                debug_assert_eq!(
                    mem::discriminant(&msg),
                    mem::discriminant(&Message::Handle(WorkerId::dummy()))
                );
                remain -= 1;
            }
        }

        let handles = self
            .sub_cxs
            .iter()
            .map(|sub_cx| unsafe { (*sub_cx.handle.get()).clone() })
            .collect();

        self.signal = Arc::new(GlobalSignal::new(handles));

        for sub_cx in self.sub_cxs.iter_mut() {
            sub_cx.as_mut().set_flags(Arc::clone(&self.signal));
        }
    }

    fn open(&mut self) {
        for i in 0..self.len() {
            if self.sub_cxs[i].guide.push_open() {
                self.unpark_one(i);
            }
        }
    }

    fn close(&mut self) {
        for i in 0..self.len() {
            self.sub_cxs[i].guide.push_close();
        }
        self.signal.sub().notify_all();
    }

    /// Do not call this method unless panic is detected.
    //
    // Allow dead_code?
    // - This will be compiled for wasm32 target.
    // - For keeping consistency with other methods.
    #[allow(dead_code)]
    fn insert_search(&mut self, index: usize) {
        // Search is only pushed when a sub worker has panicked.
        // It means that the worker was open state, and in turn, the queue
        // has one space at least because 'open' must be popped from it.
        let must_true = self.sub_cxs[index].guide.push_search();
        debug_assert!(must_true);
        self.unpark_one(index);
    }

    fn unpark_one(&mut self, index: usize) {
        let ptr = self.sub_cxs[index].as_ref().get_ref() as *const SubContext;
        // Safety: The pointer won't be aliased and they will be valid
        // during scheduling.
        let ptr = unsafe {
            let ptr = NonNullExt::new_unchecked(ptr.cast_mut());
            ManagedConstPtr::new(ptr)
        };

        let must_true = self.workers[index].unpark(ptr);
        assert!(must_true);
    }
}

impl<W> Drop for WorkGroup<W> {
    fn drop(&mut self) {
        self.signal.set_abort(true);
        while !self.is_exhausted() {
            self.signal.sub().notify_all();
            self.signal.wait_open_count(0);
        }
        self.signal.set_abort(false);

        #[cfg(debug_assertions)]
        self.validate_clean();
    }
}

thread_local! {
    pub(crate) static SUB_CONTEXT: Cell<NonNullExt<SubContext>> = const {
        Cell::new(NonNullExt::dangling())
    };
    pub(crate) static WORKER_ID: Cell<WorkerId> = const {
        Cell::new(WorkerId::dummy())
    };
}

#[derive(Debug)]
pub struct SubContext {
    guide: sub::SubStateGuide,

    /// Thread handle.
    handle: UnsafeCell<Thread>,

    comm: SubComm,

    need_close: Cell<bool>,

    shared: Arc<Shared>,

    _pin: PhantomPinned,
}

impl SubContext {
    /// Use this function in your [`Worker::unpark`] implementation.
    ///
    /// [`Worker::unpark`]: crate::prelude::Worker::unpark
    #[rustfmt::skip]
    pub fn execute(ptr: ManagedConstPtr<Self>) {
        if ptr.comm.worker_id() != WORKER_ID.get() {
            Self::set_handle(ptr);
            return;
        }

        // * Deals with managed pointer for internal debugging.
        // (1) WASM32    : Drops managed pointer for unexpected panics.
        // (2) Otherwise : Drops managed pointer later.
        let this = {
            #[cfg(target_arch = "wasm32")]      { ptr.into_ref() }
            #[cfg(not(target_arch = "wasm32"))] { &*ptr }
        };

        // ** Notifies start of accessing `SubContext`.
        // Main worker cannot release `SubContext` while we're accessing it.
        this.comm.signal().add_open_count(1);

        // Runs state machine.
        let mut cur = this.guide.pop();
        let mut steal = cb::Steal::Empty;
        while let Some(next) = this.execute_by_state(cur, &mut steal) {
            cur = next;
        }

        // * Deals with managed pointer for internal debugging.
        // (2) Drops it here, before we notify end of execution.
        #[cfg(not(target_arch = "wasm32"))]
        let this = ptr.into_ref();

        // ** Notifies end of accessing `SubContext`.
        // Main worker is now free to release `SubContext`.
        // In web, however, this procedure must occur in somewhere during panic.
        // See `web_panic_hook` for more details.
        this.comm.signal().sub_open_count(1);
    }

    pub(crate) fn get_comm(&self) -> &SubComm {
        &self.comm
    }

    pub(crate) fn get_shared(&self) -> &Shared {
        &self.shared
    }

    fn set_handle(ptr: ManagedConstPtr<Self>) {
        let handle = unsafe { &mut *ptr.handle.get() };
        *handle = thread::current();
        SUB_CONTEXT.set(ptr.inner());
        WORKER_ID.set(ptr.comm.worker_id());
        ptr.comm.send_message(Message::Handle(ptr.comm.worker_id()));
    }

    #[inline]
    fn execute_by_state(&self, cur: SubState, steal: &mut cb::Steal<Task>) -> Option<SubState> {
        match cur {
            SubState::Wait => {
                self.comm.wait();
                if self.comm.signal().is_abort() {
                    Some(SubState::Abort)
                } else {
                    Some(SubState::Search)
                }
            }
            SubState::Search => {
                *steal = self.comm.search();
                if steal.is_success() {
                    Some(SubState::Work)
                } else if self.need_close.take() {
                    Some(SubState::Close)
                } else if self.can_close() {
                    // Because search() and can_close() are not atomic, we have
                    // to search once again.
                    self.need_close.set(true);
                    Some(SubState::Search)
                } else {
                    Some(SubState::Wait)
                }
            }
            SubState::Work => {
                self.work(steal);
                Some(SubState::Search)
            }
            SubState::Abort => {
                self.abort();
                Some(SubState::Search)
            }
            SubState::Close => {
                self.comm.signal().sub().notify_all();
                None
            }
        }
    }

    #[inline]
    fn can_close(&self) -> bool {
        // Currently there is no any remaining task, but this sub worker must
        // wait for other siblings for future task. Imagine that a sibling is
        // working on a system task, which produces a future task. If the future
        // task should be run in parallel, this sub worker must join it.
        let fut_cnt = self.comm.signal().future_count();
        let work_cnt = self.comm.signal().work_count();
        if fut_cnt > 0 || work_cnt > 0 {
            return false;
        }

        // It seems likely that siblings and this sub worker are completely
        // free to exit execution. If main worker sent exit signal, we can exit.
        if self.guide.need_close() {
            return true;
        }

        // Main worker has not sent exit signal. Waits for new tasks.
        false
    }

    pub(super) fn work(&self, steal: &mut cb::Steal<Task>) {
        // * Notifies start of working state.
        self.comm.signal().add_work_count(1);

        // Works for tasks as much as possible.
        // NOTE: Panics can occur here.
        loop {
            match mem::replace(steal, cb::Steal::Empty) {
                cb::Steal::Success(cur) => match cur {
                    Task::System(task) => {
                        stat::exec::increase_system_task_count();
                        self.work_for_system_task(task);
                    }
                    Task::Parallel(task) => {
                        // Statistic counter is increased in bridge().
                        self.work_for_parallel_task(task);
                    }
                    Task::Future(task) => {
                        stat::exec::increase_future_task_count();
                        self.work_for_future_task(task);
                    }
                },
                cb::Steal::Empty => break,
                cb::Steal::Retry => {}
            }
            *steal = self.comm.pop();
        }

        // * Notifies end of working state.
        // In web, however, this procedure must occur in somewhere during panic.
        // See `web_panic_hook` for more details.
        self.comm.signal().sub_work_count(1);
    }

    fn work_for_system_task(&self, task: SysTask) {
        let sid = task.sid();

        // In web, panic is normally aborted, but we can detect
        // whether or not panic has been happened through panic hook.
        // We write `WorkId` to thread local variable to be used in
        // panic hook in order to identify where the panic occurred.
        #[cfg(target_arch = "wasm32")]
        {
            use super::comm::{TaskKind, WorkId, WORK_ID};

            WORK_ID.set(WorkId {
                wid: self.comm.worker_id(),
                sid,
                kind: TaskKind::System,
            });
        }

        let resp = match task.execute() {
            Ok(_) => Message::Fin(self.comm.worker_id(), sid),
            Err(payload) => Message::Panic(PanicMessage {
                wid: self.comm.worker_id(),
                sid,
                payload,
                unrecoverable: false,
            }),
        };

        self.comm.send_message(resp);
    }

    fn work_for_parallel_task(&self, task: ParTask) {
        // Like in `work_for_systask()`, sets `WorkId`.
        // But note that panics in parallel tasks cannot be recovered.
        // It will result in panic in main thread.
        #[cfg(target_arch = "wasm32")]
        {
            use super::comm::{TaskKind, WorkId, WORK_ID};

            WORK_ID.set(WorkId {
                wid: self.comm.worker_id(),
                sid: SystemId::dummy(),
                kind: TaskKind::Parallel,
            });
        }

        task.execute(FnContext::MIGRATED);
    }

    fn work_for_future_task(&self, task: UnsafeFuture) {
        // Like in `work_for_systask()`, sets `WorkId`.
        #[cfg(target_arch = "wasm32")]
        {
            use super::comm::{TaskKind, WorkId, WORK_ID};

            WORK_ID.set(WorkId {
                wid: self.comm.worker_id(),
                sid: SystemId::dummy(),
                kind: TaskKind::Future,
            });
        }

        // Future task may be one that this worker stole from another worker.
        // In that case, we'd better replace waker with this context pointer.
        // So that the future will wake this worker up at first.
        //
        // Safety: Waker type is the same as the thing we used, which is
        // `UnsafeWaker`.
        unsafe {
            let waker = UnsafeWaker {
                ptr: self as *const _,
            };
            if !task.will_wake(&waker) {
                task.set_waker(waker);
            }
        }

        // Calls poll on the future.
        //
        // Safety: We're constraining future output type to be
        // `Box<dyn Command>`.
        match unsafe { task.poll() } {
            Poll::Ready(()) => {
                // Safety: The future is just ready and destroying it only
                // occurs in `ReadyFuture::drop`.
                let ready = unsafe { ReadyFuture::new(task) };
                let cmd = CommandObject::Future(ready);

                // Decreases *running* future count and sends the command.
                self.comm.signal().sub_future_count(1);
                self.comm.send_command(cmd);
            }
            Poll::Pending => {}
        }
    }

    /// Cancels out remaining tasks.  
    //
    // NOTE: Future tasks are able to be cancelled at the next await points.
    // So that if any future executor, or runtime, doesn't call poll() on
    // future tasks again, this method will block infinitely.
    fn abort(&self) {
        loop {
            match self.comm.pop() {
                cb::Steal::Success(task) => self.abort_task(task),
                cb::Steal::Empty => {
                    if self.comm.signal().future_count() == 0 {
                        // Before escaping the loop, notifies all other sub
                        // wokers, so that they can escape their loops as well.
                        self.comm.signal().sub().notify_all();
                        break;
                    }

                    // This worker may be supposed to be woken by a future task.
                    self.comm.wait();
                }
                cb::Steal::Retry => {}
            }
        }
    }

    fn abort_task(&self, task: Task) {
        match task {
            // To abort system task, drops it and sends Aborted to main worker.
            Task::System(task) => {
                let wid = self.comm.worker_id();
                let sid = task.sid();
                self.comm.send_message(Message::Aborted(wid, sid));
            }
            // Parallel task cannot be aborted. It should be aborted at system
            // task level.
            Task::Parallel(task) => {
                self.work_for_parallel_task(task);
            }
            // To abort future task, destroys it and reduces running future
            // count.
            Task::Future(task) => {
                // Safety: Uncompleted future task is aborted and deallocated
                // in here only.
                unsafe { task.destroy() };
                self.comm.signal().sub_future_count(1);
            }
        }
    }

    fn set_flags(self: Pin<&mut Self>, flags: Arc<GlobalSignal>) {
        // Safety: We do not move self.
        unsafe {
            let this = self.get_unchecked_mut();
            this.comm.set_signal(flags);
        }
    }

    #[cfg(debug_assertions)]
    fn validate_clean(&self) {
        // Validates if guide is empty.
        assert!(self.guide.is_empty());

        // Validates comm.
        match self.get_comm().search() {
            cb::Steal::Empty => {}
            _ => panic!("validation failed due to remaining task"),
        }
    }
}

impl Drop for SubContext {
    fn drop(&mut self) {
        // Resets static variables.
        SUB_CONTEXT.set(NonNullExt::dangling());
        WORKER_ID.set(WorkerId::dummy());
    }
}

// This module helps us not to access fields of structs in this module directly.
mod sub {
    use super::SubState;
    use crate::ds::prelude::*;
    use std::sync::{
        atomic::{AtomicU32, Ordering},
        Mutex,
    };

    /// Guidance for [`SubContext`] on what state it should start with or need
    /// for closing. You can push or pop some states onto this struct. Allowed
    /// states are as follows.
    ///
    /// - [`SubState::Wait`]   : Normal open request.
    /// - [`SubState::Close`]  : Request for closing.
    /// - [`SubState::Search`] : It has panicked, start with search state.
    ///
    /// States can be buffered at most a fixed size(4 for now).
    ///
    /// [`SubContext`]: super::SubContext
    //
    // Why we need buffering?
    // - Main worker and sub workers are not tightly synchronized.
    //   - Main worker doesn't care of in which states sub workers are.
    //   - Main worker just notifies that there will be no new system tasks to
    //     sub workers by sending Close request to sub workers and waits for
    //     completion of system tasks only, not for future tasks.
    // - We don't want that sub workers are in open states too long.
    //   - Worker may have other roles and we may need to hand over the control
    //     for them. To do that, we need to reach close state from time to time.
    #[derive(Debug)]
    pub(super) struct SubStateGuide {
        /// SPSC queue holding state requests for a [`SubContext`].
        /// - Producer: Main worker.
        /// - Consumer: A sub worker.
        ///
        /// [`SubContext`]: super::SubContext
        queue: Mutex<ArrayDeque<SubState, 4>>,

        /// Close request counter.
        //
        // Helps us not to lock `queue` too frequently.
        close: AtomicU32,

        /// Whether or not open request has been pushed.
        //
        // Prevents from appending 'not paired close' onto the queue.
        #[cfg(debug_assertions)]
        open: std::cell::Cell<bool>,
    }

    impl SubStateGuide {
        pub(super) fn new() -> Self {
            Self {
                queue: Mutex::new(ArrayDeque::new()),
                close: AtomicU32::new(0),
                #[cfg(debug_assertions)]
                open: std::cell::Cell::new(false),
            }
        }

        pub(super) fn is_empty(&self) -> bool {
            let queue = self.queue.lock().unwrap();
            queue.is_empty()
        }

        /// If queue has enough room for open-close request pair, then pushes
        /// an open request, [`SubState::Wait`], then returns true.
        ///
        /// However, queue was full and open request was not actually pushed,
        /// then the last open-close pair in the queue turns into single open
        /// request by popping close request, then returns false.
        ///
        /// You have to unpark worker if and only if true is returned.
        ///
        /// Push operation is allowed for main worker only.
        pub(super) fn push_open(&self) -> bool {
            #[cfg(debug_assertions)]
            {
                // Open cannot follow another open.
                assert!(!self.open.get());
                self.open.set(true);
            }

            // If an open-close pair cannot be appended in a row, unchains the
            // pair by popping close reuqest.
            let mut queue = self.queue.lock().unwrap();
            if queue.capacity() - queue.len() < 2 {
                debug_assert_eq!(queue[queue.len() - 1], SubState::Close);
                debug_assert_eq!(queue[queue.len() - 2], SubState::Wait);

                queue.pop_back();
                return false;
            }

            // We checked the room, so that it must succeed.
            queue.push_back(SubState::Wait);
            true
        }

        /// Pushes a close request onto queue. You must call this method after
        /// [`Self::push_open`].
        ///
        /// Push operation is allowed for main worker only.
        pub(super) fn push_close(&self) {
            #[cfg(debug_assertions)]
            {
                // Close must follow open request.
                assert!(self.open.get());
                self.open.set(false);
            }

            let mut queue = self.queue.lock().unwrap();
            let must_true = queue.push_back(SubState::Close);
            debug_assert!(must_true);
            self.close.fetch_add(1, Ordering::Relaxed);
        }

        /// Push operation is allowed for main worker only.
        //
        // Allow dead_code?
        // - This will be compiled for wasm32 target.
        // - For keeping consistency with other methods.
        #[allow(dead_code)]
        pub(super) fn push_search(&self) -> bool {
            let mut queue = self.queue.lock().unwrap();
            queue.push_front(SubState::Search)
        }

        /// Pop operation is allowed for sub worker only.
        pub(super) fn pop(&self) -> SubState {
            let mut queue = self.queue.lock().unwrap();
            queue.pop_front().unwrap()
        }

        /// Pop operation is allowed for sub worker only.
        pub(super) fn need_close(&self) -> bool {
            if self.close.load(Ordering::Relaxed) == 0 {
                return false;
            }

            let mut queue = self.queue.lock().unwrap();
            if queue.front() != Some(&SubState::Close) {
                return false;
            }

            self.close.fetch_sub(1, Ordering::Relaxed);
            queue.pop_front();
            true
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SubState {
    Wait,
    Search,
    Work,
    Abort,
    Close,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct UnsafeWaker {
    ptr: *const SubContext,
}

unsafe impl Send for UnsafeWaker {}
unsafe impl Sync for UnsafeWaker {}

impl WakeSend for UnsafeWaker {
    fn wake_send(&self, handle: UnsafeFuture) {
        // Safety: Scheduler holds sub contexts while it is executing.
        let comm = unsafe { self.ptr.as_ref().unwrap_unchecked().get_comm() };

        // Pushes the future handle onto local future queue.
        comm.push_future_task(handle);

        // Tries to wake up the worker which called poll() on the future.
        // If it is already awaken, wakes another worker.
        comm.wake_self();
    }
}

pub fn schedule_future<F, R>(future: F)
where
    F: Future<Output = R> + Send + 'static,
    R: Command,
{
    // This function must be called on sub worker context.
    let ptr = SUB_CONTEXT.get();
    assert!(!ptr.is_dangling());

    // Allocates memory for the future.
    let waker = UnsafeWaker { ptr: ptr.as_ptr() };
    let handle = UnsafeFuture::new(future, waker, consume_ready_future::<R>);

    // Pushes the future handle onto local future queue.
    // Safety: Current worker has valid sub context pointer.
    let comm = unsafe { ptr.as_ref().get_comm() };
    comm.push_future_task(handle);

    // Increases future count.
    // Main worker will check this out whenever it needs.
    comm.signal().add_future_count(1);

    // If current worker's local queue is not empty, current worker cannot
    // do the future task promptly, so wakes another worker to steal it.
    if !comm.is_local_empty() {
        comm.signal().sub().notify_one();
    }
}

pub(crate) fn consume_ready_future<T: Command>(mut res: T, ecs: Ecs<'_>) -> DynResult<()> {
    res.command(ecs)
}
