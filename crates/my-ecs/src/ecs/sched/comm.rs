use super::task::{ParTask, Task};
use crate::{
    ds::{Signal, UnsafeFuture},
    ecs::{
        cmd::CommandObject,
        worker::{Message, WorkerId},
    },
};
use crossbeam_deque as cb;
use std::{
    cell::Cell,
    fmt,
    sync::{
        atomic::{AtomicBool, AtomicU32, Ordering},
        mpsc::{self, Receiver, RecvTimeoutError, SendError, Sender, TryRecvError},
        Arc, Mutex,
    },
    thread::{self, Thread},
    time::Duration,
};

thread_local! {
    pub(crate) static WORKER_ID_GEN: Cell<u32> = const { Cell::new(0) };
}

#[derive(Debug)]
pub(crate) struct SubComm {
    /// Global task queue contains [`Task::System`] only.
    /// It works in 'single producer multiple consumers' fashion and push/pop
    /// occurs as follows.
    /// - 'Push' occurs by main worker.
    /// - 'Pop' occurs by all sub workers.
    injector: Arc<cb::Injector<Task>>,

    /// Local task queue contains [`Task::System`], [`Task::Parallel`], and
    /// [`Task::Future`].
    /// It works in 'single producer multiple consumers' fashion and push/pop
    /// occurs as follows.
    /// - 'Push' occurs by sub worker.
    /// - 'Pop' occurs by both this sub worker and siblings via [`cb::Stealer`].
    local: cb::Worker<Task>,

    /// Sibling's pop-only local queues.  
    siblings: Arc<[cb::Stealer<Task>]>,

    /// Local future task queue contains [`Task::Future`] only.
    /// It works in 'multiple producers multiple consumers' fashion and
    /// push/pop occurs as follows.
    /// - 'Push' occurs by a thread that called the most inner future's poll().
    /// - 'Pop' occurs by each sub worker.
    //
    // `crossbeam::Worker<T>` is not a 'multiple producers multiple consumers'
    // queue, so uses another `Injector` instead. But if it causes some kind of
    // performance issue, then consider using another queue.
    futures: Arc<[cb::Injector<Task>]>,

    /// Channel sending messages to main worker.
    tx_msg: ParkingSender<Message>,

    /// Channel sending commands to main worker.
    tx_cmd: CommandSender,

    /// Signal to wake or block workers and some counts.
    signal: Arc<GlobalSignal>,

    /// Index to [`Self::siblings`], [`Self::futures`], and [`Self::signal`].
    wid: WorkerId,
}

impl SubComm {
    pub(super) fn with_len(
        group_index: u16,
        injector: &Arc<cb::Injector<Task>>,
        signal: &Arc<GlobalSignal>,
        tx_msg: &ParkingSender<Message>,
        tx_cmd: &CommandSender,
        len: usize,
    ) -> Vec<Self> {
        // Local queues and stealers.
        let (locals, siblings): (Vec<_>, Vec<_>) = (0..len)
            .map(|_| {
                let local = cb::Worker::<Task>::new_lifo();
                let sibling = local.stealer();
                (local, sibling)
            })
            .unzip();
        let siblings: Arc<[cb::Stealer<Task>]> = siblings.into();

        // Local async queues.
        let asyncs: Arc<[cb::Injector<Task>]> = (0..len).map(|_| cb::Injector::new()).collect();

        locals
            .into_iter()
            .enumerate()
            .map(|(worker_index, local)| {
                let id = WORKER_ID_GEN.get();
                WORKER_ID_GEN.set(id + 1);
                Self {
                    injector: Arc::clone(injector),
                    local,
                    siblings: Arc::clone(&siblings),
                    futures: Arc::clone(&asyncs),
                    tx_msg: tx_msg.clone(),
                    tx_cmd: tx_cmd.clone(),
                    wid: WorkerId::new(id, group_index, worker_index as u16),
                    signal: Arc::clone(signal),
                }
            })
            .collect()
    }

    pub(crate) fn signal(&self) -> &GlobalSignal {
        &self.signal
    }

    pub(crate) const fn worker_id(&self) -> WorkerId {
        self.wid
    }

    pub(super) fn num_siblings(&self) -> usize {
        self.siblings.len()
    }

    pub(super) fn set_signal(&mut self, signal: Arc<GlobalSignal>) {
        self.signal = signal;
    }

    pub(super) fn wait(&self) {
        self.signal.sub().wait(self.wid.worker_index() as usize);
    }

    pub(super) fn wake_self(&self) {
        self.signal.sub().notify(self.wid.worker_index() as usize);
    }

    pub(crate) fn send_message(&self, msg: Message) {
        self.tx_msg.send(msg).unwrap();
    }

    pub(crate) fn send_command(&self, cmd: CommandObject) {
        if let Err(SendError(cmd)) = self.tx_cmd.send(cmd) {
            cmd.cancel();
        }
    }

    pub(super) fn pop(&self) -> cb::Steal<Task> {
        if let Some(task) = self.pop_local() {
            cb::Steal::Success(task)
        } else {
            self.pop_future()
        }
    }

    pub(super) fn pop_local(&self) -> Option<Task> {
        self.local.pop()
    }

    pub(super) fn pop_future(&self) -> cb::Steal<Task> {
        loop {
            let steal = self.futures[self.wid.worker_index() as usize].steal();
            match &steal {
                cb::Steal::Retry => {}
                _ => return steal,
            }
        }
    }

    pub(super) fn push_parallel_task(&self, task: ParTask) {
        self.local.push(Task::Parallel(task));
    }

    pub(super) fn push_future_task(&self, handle: UnsafeFuture) {
        self.futures[self.wid.worker_index() as usize].push(Task::Future(handle));
    }

    pub(super) fn is_local_empty(&self) -> bool {
        self.local.is_empty()
    }

    pub(super) fn search(&self) -> cb::Steal<Task> {
        self.search_injector()
            .or_else(|| self.search_sibling_locals())
            .or_else(|| self.search_futures())
    }

    pub(super) fn search_injector(&self) -> cb::Steal<Task> {
        loop {
            let steal = self.injector.steal_batch_and_pop(&self.local);
            match &steal {
                cb::Steal::Success(_task) => {
                    if !self.local.is_empty() {
                        self.signal.sub().notify_one();
                    }
                    return steal;
                }
                cb::Steal::Empty => break,
                cb::Steal::Retry => {}
            }
        }
        cb::Steal::Empty
    }

    pub(super) fn search_sibling_locals(&self) -> cb::Steal<Task> {
        for sibling in self
            .siblings
            .iter()
            .cycle()
            .skip(self.wid.worker_index() as usize + 1)
            .take(self.siblings.len() - 1)
        {
            loop {
                let steal = sibling.steal_batch_and_pop(&self.local);
                match &steal {
                    cb::Steal::Success(_task) => {
                        if !(self.local.is_empty() && sibling.is_empty()) {
                            self.signal.sub().notify_one();
                        }
                        return steal;
                    }
                    cb::Steal::Empty => break,
                    cb::Steal::Retry => {}
                }
            }
        }
        cb::Steal::Empty
    }

    pub(super) fn search_futures(&self) -> cb::Steal<Task> {
        for sibling in self
            .futures
            .iter()
            .cycle()
            .skip(self.wid.worker_index() as usize)
            .take(self.futures.len())
        {
            loop {
                let steal = sibling.steal_batch_and_pop(&self.local);
                match &steal {
                    cb::Steal::Success(_task) => {
                        if !(self.local.is_empty() && sibling.is_empty()) {
                            self.signal.sub().notify_one();
                        }
                        return steal;
                    }
                    cb::Steal::Empty => break,
                    cb::Steal::Retry => {}
                }
            }
        }
        cb::Steal::Empty
    }
}

#[derive(Debug)]
pub(crate) struct GlobalSignal {
    /// Handle of main worker.  
    /// Sub workers can wake the main worker up through this handle.
    main: Thread,

    /// [`Signal`] for sub workers.  
    /// Main or sub worker can wake up any sub worker through this signal.
    sub: Signal,

    /// Abort flag.  
    /// This flag is written by main worker only.
    /// If this is true, sub workers will be closed soon.
    is_abort: AtomicBool,

    /// Number of open sub workers.  
    /// This count is written by sub workers only.
    /// Main worker will make use of this count for making some decisions.
    open_cnt: AtomicU32,

    /// Number of working sub workers.  
    /// This count is written by sub workers only.
    /// Main worker will make use of this count for making some decisions.
    work_cnt: AtomicU32,

    /// Number of running future tasks.  
    /// This count is written by sub workers only.
    /// Main worker will make use of this count for making some decisions.
    fut_cnt: AtomicU32,
}

impl GlobalSignal {
    /// Some atomic counts are composed of 'target' and 'count' bits.
    /// 'target' bits are located at MSB, and 'count' bits are located at LSB.
    /// This shift is the offset bits of the 'target' bits from LSB.
    const TARGET_SHIFT: u32 = 16;

    /// Mask to filter 'target' bits out, so that we can get 'count' bits only.
    const COUNT_MASK: u32 = (1 << Self::TARGET_SHIFT) - 1;

    /// Mask to filter 'count' bits out, so that we can get 'target' bits only.
    /// Don't forget to r-shift on the filtered value to get 'target' number.
    const TARGET_MASK: u32 = !Self::COUNT_MASK;

    pub(super) fn new(sub_handles: Vec<Thread>) -> Self {
        let mut sub = Signal::default();
        sub.set_handles(sub_handles);

        Self {
            main: thread::current(),
            sub,
            is_abort: AtomicBool::new(false),
            open_cnt: AtomicU32::new(0),
            work_cnt: AtomicU32::new(0),
            fut_cnt: AtomicU32::new(0),
        }
    }

    pub(crate) fn sub(&self) -> &Signal {
        &self.sub
    }

    // === Methods related to ABORT flag ===

    pub(crate) fn is_abort(&self) -> bool {
        self.is_abort.load(Ordering::Relaxed)
    }

    pub(crate) fn set_abort(&self, is_abort: bool) {
        self.is_abort.store(is_abort, Ordering::Relaxed);
    }

    // === Methods related to OPEN count ===
    // OPEN count is wait-wake-able.

    pub(crate) fn open_count(&self) -> u32 {
        self.open_cnt.load(Ordering::Relaxed) & Self::COUNT_MASK
    }

    pub(crate) fn wait_open_count(&self, target: u32) {
        Self::wait_for_target(&self.open_cnt, target);
    }

    pub(crate) fn add_open_count(&self, value: u32) -> u32 {
        let old = self.open_cnt.fetch_add(value, Ordering::Relaxed);
        Self::wake_by_target(&self.main, old, value as i32);
        (old & Self::COUNT_MASK).wrapping_add(value)
    }

    pub(crate) fn sub_open_count(&self, value: u32) -> u32 {
        let old = self.open_cnt.fetch_sub(value, Ordering::Relaxed);
        Self::wake_by_target(&self.main, old, -(value as i32));
        (old & Self::COUNT_MASK).wrapping_sub(value)
    }

    // === Methods related to WORK count ===
    // WORK count is wait-wake-able.

    pub(crate) fn work_count(&self) -> u32 {
        self.work_cnt.load(Ordering::Relaxed) & Self::COUNT_MASK
    }

    pub(crate) fn add_work_count(&self, value: u32) -> u32 {
        let old = self.work_cnt.fetch_add(value, Ordering::Relaxed);
        Self::wake_by_target(&self.main, old, value as i32);
        (old & Self::COUNT_MASK).wrapping_add(value)
    }

    pub(crate) fn sub_work_count(&self, value: u32) -> u32 {
        let old = self.work_cnt.fetch_sub(value, Ordering::Relaxed);
        Self::wake_by_target(&self.main, old, -(value as i32));
        (old & Self::COUNT_MASK).wrapping_sub(value)
    }

    // === Methods related to FUTURE count ===
    // FUTURE count is wait-wake-able.

    pub(crate) fn future_count(&self) -> u32 {
        self.fut_cnt.load(Ordering::Relaxed) & Self::COUNT_MASK
    }

    pub(crate) fn add_future_count(&self, value: u32) -> u32 {
        let old = self.fut_cnt.fetch_add(value, Ordering::Relaxed);
        Self::wake_by_target(&self.main, old, value as i32);
        (old & Self::COUNT_MASK).wrapping_add(value)
    }

    pub(crate) fn sub_future_count(&self, value: u32) -> u32 {
        let old = self.fut_cnt.fetch_sub(value, Ordering::Relaxed);
        Self::wake_by_target(&self.main, old, -(value as i32));
        (old & Self::COUNT_MASK).wrapping_sub(value)
    }

    fn wait_for_target(cnt: &AtomicU32, target: u32) {
        let target_bits = (!target) << Self::TARGET_SHIFT;
        let mut cur = cnt.load(Ordering::Relaxed);

        // Sets target bits or returns if it's not more required.
        loop {
            // If read value reached the target in this loop, returns.
            if cur == target {
                return;
            }

            // Tries to set target bits. If it succeeds, escapes this loop.
            if let Err(old) = cnt.compare_exchange_weak(
                cur,
                cur | target_bits,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                cur = old;
            } else {
                break;
            }
        }

        // We set the target bits. So waits for any sub worker to wake us up.
        let target_target = target_bits | target;
        while cnt.load(Ordering::Relaxed) != target_target {
            thread::park();
        }

        // Resets target bits.
        let mut cur = target_target;
        while (cur & Self::TARGET_MASK) != 0 {
            if let Err(old) = cnt.compare_exchange_weak(
                cur,
                cur & (!Self::TARGET_MASK),
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                cur = old;
            } else {
                break;
            }
        }
    }

    fn wake_by_target(main: &Thread, old: u32, delta: i32) {
        let high = old & Self::TARGET_MASK;
        let low = old & Self::COUNT_MASK;
        let target = (!high) >> Self::TARGET_SHIFT;
        let cur = (low as i32 + delta) as u32;

        if target == cur {
            main.unpark();
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct CommandSender {
    inner: ParkingSender<CommandObject>,
    open: Arc<Mutex<bool>>,
}

impl CommandSender {
    pub(super) fn send(&self, cmd: CommandObject) -> Result<(), SendError<CommandObject>> {
        let guard = self.open.lock().unwrap();
        if *guard {
            self.inner.send(cmd)
        } else {
            Err(SendError(cmd))
        }
    }
}

#[derive(Debug)]
pub(crate) struct CommandReceiver {
    inner: ParkingReceiver<CommandObject>,
    open: Arc<Mutex<bool>>,
}

impl CommandReceiver {
    pub(crate) fn has(&self) -> bool {
        self.inner.has()
    }

    pub(crate) fn try_recv(&self) -> Result<CommandObject, TryRecvError> {
        self.inner.try_recv()
    }

    pub(crate) fn close(&self) {
        *self.open.lock().unwrap() = false;
    }
}

pub(crate) fn command_channel(th: Thread) -> (CommandSender, CommandReceiver) {
    let (tx, rx) = parking_channel(th);
    let open = Arc::new(Mutex::new(true));
    let c_open = Arc::clone(&open);

    (
        CommandSender { inner: tx, open },
        CommandReceiver {
            inner: rx,
            open: c_open,
        },
    )
}

pub(super) fn parking_channel<T>(th: Thread) -> (ParkingSender<T>, ParkingReceiver<T>) {
    let (tx, rx) = mpsc::channel();
    (ParkingSender::new(tx, th.clone()), ParkingReceiver::new(rx))
}

#[derive(Debug)]
pub(crate) struct ParkingSender<T> {
    tx: Sender<T>,
    th: Thread,
}

impl<T> ParkingSender<T> {
    pub(crate) const fn new(tx: Sender<T>, th: Thread) -> Self {
        Self { tx, th }
    }

    pub(crate) fn send(&self, t: T) -> Result<(), SendError<T>> {
        let res = self.tx.send(t);
        self.th.unpark();
        res
    }
}

impl<T> Clone for ParkingSender<T> {
    fn clone(&self) -> Self {
        Self {
            tx: self.tx.clone(),
            th: self.th.clone(),
        }
    }
}

pub(crate) struct ParkingReceiver<T> {
    rx: Receiver<T>,

    /// Takes the next value out of the channel and holds it.
    /// to cooperate with `thread::park_timeout`.
    next: Cell<Option<T>>,
}

impl<T> fmt::Debug for ParkingReceiver<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ParkingReceiver")
            .field("rx", &self.rx)
            .finish_non_exhaustive()
    }
}

impl<T> ParkingReceiver<T> {
    pub(crate) const fn new(rx: Receiver<T>) -> Self {
        Self {
            rx,
            next: Cell::new(None),
        }
    }

    pub(crate) fn has(&self) -> bool {
        if let Ok(value) = self.try_recv() {
            self.next.set(Some(value));
            true
        } else {
            false
        }
    }

    /// May return timeout error spuriously.
    //
    // Why `thread::park_timeout()` instead of `Receiver::recv_timeout()`?
    //
    // In web, we cannot get time, but `Receiver::recv_timeout()` tries
    // to get current time, so it fails to compile.
    // Fortunately, in nightly-2024-06-20, `thread::park_timeout()` is
    // implemented via wasm32::memory_atomic_wait32(), so it works.
    // See "nightly-2024-06-20-.../lib/rustlib/src/rust/library/std/src/sys/pal/wasm/atomics/futex.rs"
    pub(crate) fn recv_timeout(&self, timeout: Duration) -> Result<T, RecvTimeoutError> {
        let cur = if let Some(value) = self.next.take() {
            Ok(value)
        } else {
            thread::park_timeout(timeout);
            self.rx.try_recv().map_err(|err| match err {
                TryRecvError::Empty => RecvTimeoutError::Timeout,
                TryRecvError::Disconnected => RecvTimeoutError::Disconnected,
            })
        };

        self.next.set(self.rx.try_recv().ok());

        cur
    }

    pub(crate) fn try_recv(&self) -> Result<T, TryRecvError> {
        if let Some(value) = self.next.take() {
            Ok(value)
        } else {
            self.rx.try_recv()
        }
    }
}

#[cfg(target_arch = "wasm32")]
pub(crate) use web::*;

#[cfg(target_arch = "wasm32")]
mod web {
    use super::*;
    use crate::ecs::sys::system::SystemId;

    thread_local! {
        /// Per-thread work that the thread is trying to do.
        pub(crate) static WORK_ID: Cell<WorkId> = const {
            Cell::new(WorkId {
                wid: WorkerId::dummy(),
                sid: SystemId::dummy(),
                kind: TaskKind::System,
            })
        };
    }

    #[derive(Debug, Clone, Copy)]
    pub(crate) struct WorkId {
        pub(crate) wid: WorkerId,
        pub(crate) sid: SystemId,
        pub(crate) kind: TaskKind,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub(crate) enum TaskKind {
        System,
        Parallel,
        Future,
    }
}
