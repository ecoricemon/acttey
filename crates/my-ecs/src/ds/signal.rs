use std::{
    mem,
    num::NonZeroU32,
    sync::atomic::{AtomicU32, Ordering},
    thread::{self, Thread},
};

const ASLEEP: u32 = 0;
const AWAKE: u32 = 1;
const ALARM: u32 = 2;

#[derive(Debug)]
pub struct Signal {
    handles: Vec<Thread>,
    states: Vec<AtomicU32>,
    rand_gen: Xorshift,
}

impl Default for Signal {
    fn default() -> Self {
        const SEED: u32 = 0x7A7A_A7A7;
        // Safety: Infallible.
        unsafe { Self::new(NonZeroU32::new_unchecked(SEED)) }
    }
}

impl Signal {
    pub fn new(seed: NonZeroU32) -> Self {
        Self {
            handles: Vec::new(),
            states: Vec::new(),
            rand_gen: Xorshift::new(seed),
        }
    }

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

    pub fn wait(&self, index: usize) {
        if self.states[index]
            .compare_exchange(AWAKE, ASLEEP, Ordering::Relaxed, Ordering::Relaxed)
            .is_ok()
        {
            loop {
                thread::park(); // Acquire op in terms of ordering.
                if self.states[index].load(Ordering::Relaxed) != ASLEEP {
                    break;
                }
            }
        }
        self.states[index].store(AWAKE, Ordering::Relaxed);
    }

    pub fn notify(&self, index: usize) {
        // Tries to find and wake one ASLEEP worker.
        const RETRY: usize = 3;
        let len = self.states.len();
        for i in (0..len).cycle().skip(index).take(len * RETRY) {
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
        self.states[index].store(ALARM, Ordering::Relaxed);
        self.handles[index].unpark(); // Release op in terms of ordering.
    }

    // Wakes up one randomly chosen blocked thread.
    pub fn notify_one(&self) {
        let index = self.rand_gen.next() % self.states.len() as u32;
        self.notify(index as usize);
    }

    // Wakes all threads up.
    pub fn notify_all(&self) {
        for (state, handle) in self.states.iter().zip(self.handles.iter()) {
            state.store(ALARM, Ordering::Relaxed);
            handle.unpark(); // Release op in terms of ordering.
        }
    }
}

// ref: https://en.wikipedia.org/wiki/Xorshift
#[derive(Debug)]
#[repr(transparent)]
pub struct Xorshift {
    state: AtomicU32,
}

impl Xorshift {
    pub const fn new(seed: NonZeroU32) -> Self {
        Self {
            state: AtomicU32::new(seed.get()),
        }
    }

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
