/// Simple vector pool.
///
/// The pool holds used vectors while keeping their capacities to avoid frequent
/// memory allocation.
#[derive(Debug)]
pub struct SimpleVecPool<T> {
    bufs: Vec<Vec<T>>,
    free: Vec<usize>,
}

impl<T> SimpleVecPool<T> {
    /// Creates a new empty vector pool.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SimpleVecPool;
    ///
    /// let mut pool = SimpleVecPool::<i32>::new();
    /// ```
    pub const fn new() -> Self {
        Self {
            bufs: Vec::new(),
            free: Vec::new(),
        }
    }

    /// Returns an index to a vector of the pool.
    ///
    /// The pool prefers to reuse vector, therefore caller will receive an index
    /// to a used vector if the pool contains used vectors. In that case, the
    /// vector is completed cleared while keeping its capacity. If the pool
    /// doesn't have any used vectors in it, a new vector is created.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SimpleVecPool;
    ///
    /// let mut pool = SimpleVecPool::<i32>::new();
    ///
    /// let index = pool.request();
    /// let v = pool.get(index);
    /// v.reserve(10);
    /// drop(v);
    ///
    /// pool.release(index);
    /// let index = pool.request(); // pool will return the used vector
    /// let v = pool.get(index);
    /// assert!(v.is_empty());
    /// assert!(v.capacity() >= 10);
    /// ```
    pub fn request(&mut self) -> usize {
        if let Some(index) = self.free.pop() {
            self.bufs[index].clear();
            index
        } else {
            self.bufs.push(Vec::new());
            self.bufs.len() - 1
        }
    }

    /// Lets the pool know the end of use of a vector.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SimpleVecPool;
    ///
    /// let mut pool = SimpleVecPool::<i32>::new();
    /// let index0 = pool.request();
    /// pool.release(index0);
    ///
    /// let index1 = pool.request(); // pool will return the used vector
    /// assert_eq!(index0, index1);
    /// ```
    pub fn release(&mut self, index: usize) {
        // Takes O(n).
        debug_assert!(!self.free.contains(&index));

        self.free.push(index);
    }

    /// Returns a mutable reference to a vector at the given index.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::SimpleVecPool;
    ///
    /// let mut pool = SimpleVecPool::new();
    /// let index = pool.request();
    /// let v = pool.get(index);
    /// v.push(0);
    /// ```
    pub fn get(&mut self, index: usize) -> &mut Vec<T> {
        &mut self.bufs[index]
    }
}

impl<T> Default for SimpleVecPool<T> {
    fn default() -> Self {
        Self::new()
    }
}
