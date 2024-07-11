use std::{collections::VecDeque, ops::Deref};

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
