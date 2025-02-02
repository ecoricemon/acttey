use std::{
    mem,
    num::NonZeroU32,
    sync::atomic::{AtomicU32, Ordering},
    thread::{self, Thread},
};

const ASLEEP: u32 = 0;
const AWAKE: u32 = 1;
const ALARM: u32 = 2;

/// A data structure that can be used to wake other workers.
///
/// To use this struct, you must register worker handles through
/// [`Signal::set_handles`], then you can make use of sleep/wake methods like
/// so,
/// - [`Signal::wait`] : Sleep indefinitely.
/// - [`Signal::notify`] : Wake a specific worker.
/// - [`Signal::notify_one`] : Wake a random worker.
/// - [`Signal::notify_all`] : Wake all workers.
#[derive(Debug)]
pub struct Signal {
    handles: Vec<Thread>,
    states: Vec<AtomicU32>,
    rand_gen: Xorshift,
}

impl Default for Signal {
    /// Creates a new empty [`Signal`] with default random seed number.
    ///
    /// Note that you cannot do anything with empty `Signal` unless you call
    /// [`Signal::set_handles`] on it.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::Signal;
    ///
    /// let signal = Signal::default();
    /// ```
    fn default() -> Self {
        const DEFAULT_SEED: u32 = 0x7A7A_A7A7;
        // Safety: Infallible.
        unsafe { Self::new(NonZeroU32::new_unchecked(DEFAULT_SEED)) }
    }
}

impl Signal {
    /// Creates a new empty [`Signal`] with the given random seed number.
    ///
    /// Don't forget to call [`Signal::set_handles`] before using the signal.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::Signal;
    /// use std::num::NonZeroU32;
    ///
    /// let signal = Signal::new(NonZeroU32::MIN);
    /// ```
    pub fn new(seed: NonZeroU32) -> Self {
        Self {
            handles: Vec::new(),
            states: Vec::new(),
            rand_gen: Xorshift::new(seed),
        }
    }

    /// Sets worker handles to the signal then returns former worker handles.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::Signal;
    /// use std::thread;
    ///
    /// let join = thread::spawn(|| { /* ... */ });
    /// let handle = join.thread().clone();
    ///
    /// let mut signal = Signal::default();
    /// signal.set_handles(vec![handle]);
    /// ```
    pub fn set_handles(&mut self, handles: Vec<Thread>) -> Vec<Thread> {
        let init_state = if self.states.is_empty() {
            AWAKE
        } else if self
            .states
            .iter()
            .all(|state| state.load(Ordering::Relaxed) == ASLEEP)
        {
            ASLEEP
        } else {
            panic!("cannot replace 'Signal' in awake state");
        };

        let new_len = handles.len();

        // Replaces 'handles'.
        let old = mem::replace(&mut self.handles, handles);

        // Replaces 'states'.
        self.states = (0..new_len).map(|_| AtomicU32::new(init_state)).collect();

        old
    }

    /// Blocks until another worker signals.
    ///
    /// * this_index - Index to the handle of *this worker* in the vector that
    ///   you inserted through [`Signal::set_handles`].
    ///
    /// Current worker cannot be woken up unless another worker wakes the
    /// current worker through [`Signal`] because this method blocks repeatedly
    /// to ignore spurious wakeup. See [`park`](thread::park) for more details.
    ///
    /// Also, note that signaling looks like being buffered in a single slot.
    /// For example, if another worker woke current worker up and current worker
    /// didn't sleep at that time, next call to [`Signal::wait`] will consume
    /// the signal in the single slot buffer then be ignored.
    ///
    /// # Note
    ///
    /// `index` is an index for the current worker handle in the vector you
    /// inserted at [`Signal::set_handles`]. By receiving the index from
    /// caller, this method can avoid unnecessary matching opeartion. But note
    /// that incorrect index causes panic or undefinitely long sleep.
    ///
    /// # Examples
    ///
    /// ```ignore
    /// use my_ecs::ds::Signal;
    ///
    /// let mut signal = Signal::default();
    /// let handle = std::thread::current();
    /// signal.set_handles(vec![handle, /* other handles */]);
    ///
    /// // Current worker will be blocked by this call.
    /// signal.wait(0); // Current handle index is 0.
    /// ```
    pub fn wait(&self, this_index: usize) {
        #[cfg(debug_assertions)]
        {
            let handle = self
                .handles
                .get(this_index)
                .expect("index is out of bounds");
            assert_eq!(
                thread::current().id(),
                handle.id(),
                "incorrect index was given"
            );
        }

        // Skips if ALARM.
        if self.states[this_index]
            .compare_exchange(AWAKE, ASLEEP, Ordering::Relaxed, Ordering::Relaxed)
            .is_ok()
        {
            loop {
                thread::park(); // Acquire op in terms of ordering.
                if self.states[this_index].load(Ordering::Relaxed) != ASLEEP {
                    break;
                }
            }
        }

        // ALARM -> AWAKE.
        self.states[this_index].store(AWAKE, Ordering::Relaxed);
    }

    /// Wakes up a worker for the given target index or another worker.
    ///
    /// * target_index - Index to a handle of the vector that you inserted
    ///   through [`Signal::set_handles`].
    ///
    /// If the target worker is not blocked at the time, tries to wake another
    /// worker instead. If failed to wake any worker, then mark *ALARM* on the
    /// target worker, so that the worker will not be blocked by
    /// [`Signal::wait`] next time.
    ///
    /// # Examples
    ///
    /// ```ignore
    /// use my_ecs::ds::Signal;
    /// use std::thread;
    ///
    /// let mut signal = Signal::default();
    ///
    /// // Worker A.
    /// let join_a = thread::spawn(|| { /* ... */});
    /// let handle_a = join_a.thread().clone();
    ///
    /// // Worker B.
    /// let join_b = thread::spawn(|| { /* ... */});
    /// let handle_b = join_b.thread().clone();
    ///
    /// signal.set_handles(vec![handle_a, handle_b]);
    ///
    /// // Wakes up worker A.
    /// signal.notify(0);
    ///
    /// // Wakes up worker B.
    /// signal.notify(1);
    ///
    /// # join_a.join().unwrap();
    /// # join_b.join().unwrap();
    /// ```
    pub fn notify(&self, target_index: usize) {
        // Tries to find and wake one ASLEEP worker.
        const RETRY: usize = 3;
        let len = self.states.len();
        for i in (0..len).cycle().skip(target_index).take(len * RETRY) {
            if self.states[i]
                .compare_exchange(ASLEEP, ALARM, Ordering::Relaxed, Ordering::Relaxed)
                .is_ok()
            {
                self.handles[i].unpark(); // Release op in terms of ordering.
                return;
            }
        }

        // We've failed to find ASLEEP worker.
        // Then, make sure one worker to be AWAKE later.
        self.states[target_index].store(ALARM, Ordering::Relaxed);
        self.handles[target_index].unpark(); // Release op in terms of ordering.
    }

    /// Wakes up a random worker.
    ///
    /// If failed to wake any worker, then mark *ALARM* on one worker, so that
    /// the worker will not be blocked by [`Signal::wait`] next time.
    ///
    /// # Examples
    ///
    /// ```ignore
    /// use my_ecs::ds::Signal;
    /// use std::thread;
    ///
    /// let mut signal = Signal::default();
    ///
    /// // Worker A.
    /// let join_a = thread::spawn(|| { /* ... */});
    /// let handle_a = join_a.thread().clone();
    ///
    /// // Worker B.
    /// let join_b = thread::spawn(|| { /* ... */});
    /// let handle_b = join_b.thread().clone();
    ///
    /// signal.set_handles(vec![handle_a, handle_b]);
    ///
    /// // Wakes up one random worker.
    /// signal.notify_one();
    ///
    /// # join_a.join().unwrap();
    /// # join_b.join().unwrap();
    /// ```
    pub fn notify_one(&self) {
        let index = self.rand_gen.next() % self.states.len() as u32;
        self.notify(index as usize);
    }

    /// Wakes up all workers.
    ///
    /// Each worker is marked *ALARM* when it's not blocked at the time, so that
    /// the worker will not be blocked by [`Signal::wait`] next time.
    ///
    /// # Examples
    ///
    /// ```ignore
    /// use my_ecs::ds::Signal;
    /// use std::thread;
    ///
    /// let mut signal = Signal::default();
    ///
    /// // Worker A.
    /// let join_a = thread::spawn(|| { /* ... */});
    /// let handle_a = join_a.thread().clone();
    ///
    /// // Worker B.
    /// let join_b = thread::spawn(|| { /* ... */});
    /// let handle_b = join_b.thread().clone();
    ///
    /// signal.set_handles(vec![handle_a, handle_b]);
    ///
    /// // Wakes up all workers, Worker A & B.
    /// signal.notify_all();
    ///
    /// # join_a.join().unwrap();
    /// # join_b.join().unwrap();
    /// ```
    pub fn notify_all(&self) {
        for (state, handle) in self.states.iter().zip(self.handles.iter()) {
            state.store(ALARM, Ordering::Relaxed);
            handle.unpark(); // Release op in terms of ordering.
        }
    }
}

/// A random number generator based on 32bits state.
///
/// The genrator can be shared and generate random numbers across workers but it
/// doesn't provide synchronization.
///
/// # Reference
///  
/// <https://en.wikipedia.org/wiki/Xorshift>
#[derive(Debug)]
#[repr(transparent)]
pub struct Xorshift {
    state: AtomicU32,
}

impl Xorshift {
    /// Creates a new [`Xorshift`].
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::Xorshift;
    /// use std::num::NonZeroU32;
    ///
    /// let gen = Xorshift::new(NonZeroU32::MIN);
    /// ```
    pub const fn new(seed: NonZeroU32) -> Self {
        Self {
            state: AtomicU32::new(seed.get()),
        }
    }

    /// Generates next random number from the current state.
    ///
    /// You can call this method on multiple workers at the same time. The
    /// generator is using atomic variable under the hood, so that it's
    /// guaranteed to generate a random number from different states.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::Xorshift;
    /// use std::{num::NonZeroU32, collections::HashSet};
    ///
    /// let gen = Xorshift::new(NonZeroU32::MIN);
    /// let mut randoms = HashSet::new();
    /// for _ in 0..100 {
    ///     let is_new = randoms.insert(gen.next());
    ///     assert!(is_new);
    /// }
    /// ```
    pub fn next(&self) -> u32 {
        let mut cur = self.state.load(Ordering::Relaxed);
        loop {
            let mut new = cur;
            new ^= new << 13;
            new ^= new >> 17;
            new ^= new << 5;
            if let Err(slot) =
                self.state
                    .compare_exchange_weak(cur, new, Ordering::Relaxed, Ordering::Relaxed)
            {
                cur = slot;
            } else {
                return new;
            }
        }
    }
}
