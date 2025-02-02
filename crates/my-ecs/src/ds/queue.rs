use std::{collections::VecDeque, ops::Deref};

/// A queue with generation.
///
/// Generation increases whenever you take a value out of the queue, so that
/// this would be useful when you need to know how many items are taken out
/// since a specific time.
///
/// # Examples
///
/// ```
/// use my_ecs::ds::GenQueue;
///
/// let mut queue = GenQueue::new();
///
/// // Calls lots of push_back() and pop_front() on the queue...
/// # for _ in 0..10 {
/// #     queue.push_back(0);
/// # }
/// # for _ in 0..5 {
/// #     queue.pop_front();
/// # }
///
/// // We're going to detect when this value will be returned.
/// let value = 123;
/// queue.push_back(value);
/// let expire = queue.generation() + queue.len() as u64;
///
/// // Calls lots of push_back() and pop_front() on the queue...
/// # for _ in 0..10 {
/// #     queue.push_back(0);
/// # }
/// # for _ in 0..5 {
/// #     queue.pop_front();
/// # }
///
/// // `value` will be returned when queue's generation reaches `expire`.
/// while let Some(taken) = queue.pop_front() {
///     if queue.generation() == expire {
///         assert_eq!(taken, value);
///     }
/// }
/// ```
#[derive(Debug, Clone)]
pub struct GenQueue<T> {
    queue: VecDeque<T>,
    generation: u64,
}

impl<T> GenQueue<T> {
    /// Creates a new empty [`GenQueue`].
    ///
    /// # Examples
    ///
    /// See [`GenQueue`] document.
    pub const fn new() -> Self {
        Self {
            queue: VecDeque::new(),
            generation: 0,
        }
    }

    /// Returns current generation of the queue.
    ///
    /// Note that generation increases by 1 whenever you call
    /// [`GenQueue::pop_front`].
    ///
    /// # Examples
    ///
    /// See [`GenQueue`] document.
    pub fn generation(&self) -> u64 {
        self.generation
    }

    /// Returns a mutable reference to the value at the beginning of the queue.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::GenQueue;
    ///
    /// let mut queue = GenQueue::new();
    /// queue.push_back(0);
    /// queue.push_back(1);
    /// assert_eq!(queue.front_mut(), Some(&mut 0));
    /// ```
    pub fn front_mut(&mut self) -> Option<&mut T> {
        self.queue.front_mut()
    }

    /// Returns a mutable reference to the value at the end of the queue.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::GenQueue;
    ///
    /// let mut queue = GenQueue::new();
    /// queue.push_back(0);
    /// queue.push_back(1);
    /// assert_eq!(queue.back_mut(), Some(&mut 1));
    /// ```
    pub fn back_mut(&mut self) -> Option<&mut T> {
        self.queue.back_mut()
    }

    /// Appends the given value to the end of the queue.
    ///
    /// # Examples
    ///
    /// See [`GenQueue`] document.
    pub fn push_back(&mut self, value: T) {
        self.queue.push_back(value);
    }

    /// Removes a value from the beginning of the queue then returns it.
    ///
    /// Note that this operation increases queue's generation by 1.
    ///
    /// # Examples
    ///
    /// See [`GenQueue`] document.
    pub fn pop_front(&mut self) -> Option<T> {
        let old = self.queue.pop_front();
        if old.is_some() {
            self.generation += 1;
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
