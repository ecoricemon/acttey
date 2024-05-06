#[derive(Debug)]
pub struct SimpleVecPool<T> {
    bufs: Vec<Vec<T>>,
    free: Vec<usize>,
}

impl<T> SimpleVecPool<T> {
    pub const fn new() -> Self {
        Self {
            bufs: Vec::new(),
            free: Vec::new(),
        }
    }

    pub fn request(&mut self) -> usize {
        if let Some(index) = self.free.pop() {
            self.bufs[index].clear();
            index
        } else {
            self.bufs.push(Vec::new());
            self.bufs.len() - 1
        }
    }

    pub fn release(&mut self, index: usize) {
        // Takes O(n).
        debug_assert!(!self.free.contains(&index));

        self.free.push(index);
    }

    #[inline]
    pub fn get(&mut self, index: usize) -> &mut Vec<T> {
        &mut self.bufs[index]
    }

    pub fn get_release(&mut self, index: usize) -> &mut Vec<T> {
        // To release first is fine.
        self.release(index);
        self.get(index)
    }
}

impl<T> Default for SimpleVecPool<T> {
    fn default() -> Self {
        Self::new()
    }
}
