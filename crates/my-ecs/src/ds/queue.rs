use std::{
    collections::VecDeque,
    ops::{Deref, RangeBounds},
};

/// Whenever an item is popped, generation grows.
/// This is useful when you're trying to detect number of popped items after you put something in.
#[derive(Debug, Clone)]
pub struct GenQueue<T> {
    queue: VecDeque<T>,
    gen: u64,
}

impl<T> GenQueue<T> {
    pub const fn new() -> Self {
        Self {
            queue: VecDeque::new(),
            gen: 0,
        }
    }

    pub fn gen(&self) -> u64 {
        self.gen
    }

    pub fn front_mut(&mut self) -> Option<&mut T> {
        self.queue.front_mut()
    }

    pub fn back_mut(&mut self) -> Option<&mut T> {
        self.queue.back_mut()
    }

    pub fn push_back(&mut self, value: T) {
        self.queue.push_back(value);
    }

    /// Pops an item from the front of the queue and increases generation by one.
    pub fn pop_front(&mut self) -> Option<T> {
        let old = self.queue.pop_front();
        if old.is_some() {
            self.gen += 1;
        }
        old
    }
}

impl<T> Default for GenQueue<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Deref for GenQueue<T> {
    type Target = VecDeque<T>;

    fn deref(&self) -> &Self::Target {
        &self.queue
    }
}

#[derive(Debug, Clone)]
pub struct LimitedQueue<T> {
    inner: VecDeque<T>,
    limit: usize,
}

impl<T> LimitedQueue<T> {
    pub const DEFAULT_LIMIT: usize = 64;

    pub const fn new() -> Self {
        Self {
            inner: VecDeque::new(),
            limit: Self::DEFAULT_LIMIT,
        }
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self {
            inner: VecDeque::with_capacity(cap),
            limit: cap.max(Self::DEFAULT_LIMIT),
        }
    }

    pub fn with_limit(mut self, limit: usize) -> Self {
        self.set_limit(limit);
        self
    }

    pub fn set_limit(&mut self, limit: usize) {
        self.limit = limit;
        while self.inner.len() > self.limit {
            self.inner.pop_front();
        }
    }

    pub fn get_limit(&self) -> usize {
        self.limit
    }

    pub fn push(&mut self, value: T) -> Option<T> {
        // Pops it out first not to expand capacity.
        let old = if self.inner.len() == self.limit {
            self.inner.pop_front()
        } else {
            None
        };

        self.inner.push_back(value);

        old
    }

    pub fn pop(&mut self) -> Option<T> {
        self.inner.pop_front()
    }

    pub fn drain<R>(&mut self, range: R) -> std::collections::vec_deque::Drain<'_, T>
    where
        R: RangeBounds<usize>,
    {
        self.inner.drain(range)
    }
}

// Do not implement `DerefMut`. We need to monitor and limit length of the vector.
impl<T> Deref for LimitedQueue<T> {
    type Target = VecDeque<T>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> Default for LimitedQueue<T> {
    fn default() -> Self {
        Self::new()
    }
}
