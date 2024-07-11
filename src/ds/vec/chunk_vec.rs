use crate::util::slice;
use my_ecs::util::prelude::*;
use std::{mem, ops};

#[derive(Debug, Clone)]
pub struct ChunkVec<T: 'static> {
    chunks: Vec<Vec<T>>,
    chunk_size: PowerOfTwo,
    len: usize,
}

impl<T: 'static> ChunkVec<T> {
    pub fn new(chunk_size: usize) -> Self {
        let mut instance = Self {
            chunks: vec![],
            chunk_size: PowerOfTwo::new(chunk_size).unwrap(),
            len: 0,
        };
        instance.chunks.push(instance.new_chunk());
        instance
    }

    fn chunk_index(&self, index: usize) -> (usize, usize) {
        (
            self.chunk_size.quotient(index),
            self.chunk_size.remainder(index),
        )
    }

    fn new_chunk(&self) -> Vec<T> {
        Vec::with_capacity(self.chunk_size.get())
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn capacity(&self) -> usize {
        self.chunks.len() * self.chunk_size.get()
    }

    pub fn push(&mut self, value: T) {
        let chunk_size = self.chunk_size.get();

        let r = self.chunk_index(self.len.saturating_sub(1)).0;
        // Safety: There is at least one chunk.
        let last_chunk = unsafe { self.chunks.get_unchecked_mut(r) };

        // We can put the value in the `last_chunk`.
        if last_chunk.len() < chunk_size {
            last_chunk.push(value);
        }
        // We can put the value in the next chunk after the `last_chunk`.
        else if let Some(next_chunk) = self.chunks.get_mut(r + 1) {
            next_chunk.push(value);
        }
        // We should allocate a new chunk and put the value in it.
        else {
            let mut new_chunk = self.new_chunk();
            new_chunk.push(value);
            self.chunks.push(new_chunk);
        }
        self.len += 1;
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.is_empty() {
            None
        } else {
            let (r, _) = self.chunk_index(self.len - 1);
            // Safety: `r` and `c` are valid.
            let last_chunk = unsafe { self.chunks.get_unchecked_mut(r) };

            self.len -= 1;
            last_chunk.pop()
        }
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        // Safetey: `index` is valid.
        (index < self.len).then(|| unsafe { self.get_unchecked(index) })
    }

    unsafe fn get_unchecked(&self, index: usize) -> &T {
        let (r, c) = self.chunk_index(index);
        self.chunks.get_unchecked(r).get_unchecked(c)
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        // Safetey: index is valid.
        (index < self.len).then(|| unsafe { self.get_unchecked_mut(index) })
    }

    unsafe fn get_unchecked_mut(&mut self, index: usize) -> &mut T {
        let (r, c) = self.chunk_index(index);
        self.chunks.get_unchecked_mut(r).get_unchecked_mut(c)
    }

    pub fn swap_remove(&mut self, index: usize) -> T {
        assert!(index < self.len);

        // Safety: Valid index guarantees following unsafe blocks are safe.
        let (old_r, old_c) = self.chunk_index(index);
        let (last_r, last_c) = self.chunk_index(self.len - 1);

        self.len -= 1;

        // Swaps.
        if old_r != last_r {
            let [old_chunk, last_chunk] = slice::split_mut(&mut self.chunks, [old_r, last_r]);
            unsafe {
                mem::swap(
                    old_chunk.get_unchecked_mut(old_c),
                    last_chunk.get_unchecked_mut(last_c),
                );
                last_chunk.pop().unwrap_unchecked()
            }
        } else {
            let last_chunk = unsafe { self.chunks.get_unchecked_mut(last_r) };
            if old_c != last_c {
                let [old_v, last_v] = slice::split_mut(last_chunk, [old_c, last_c]);
                mem::swap(old_v, last_v);
            }
            unsafe { last_chunk.pop().unwrap_unchecked() }
        }
    }

    /// Drops redundant chunks.
    pub fn shrink_to_fit(&mut self) {
        // Doesn't drop the first one even if it's empty.
        let chunk_num = self.chunks.len();
        for i in (1..chunk_num).rev() {
            if self.chunks[i].is_empty() {
                self.chunks.pop();
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ChunkVecIter<'a, T: 'static> {
    data: &'a ChunkVec<T>,
    index: usize,
}

impl<'a, T> ChunkVecIter<'a, T> {
    pub fn new(data: &'a ChunkVec<T>) -> Self {
        Self { data, index: 0 }
    }
}

impl<'a, T> Iterator for ChunkVecIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let res = self.data.get(self.index);
        self.index += 1;
        res
    }
}

#[derive(Debug)]
pub struct ChunkVecIterMut<'a, T: 'static> {
    data: &'a mut ChunkVec<T>,
    index: usize,
}

impl<'a, T> ChunkVecIterMut<'a, T> {
    pub fn new(data: &'a mut ChunkVec<T>) -> Self {
        Self { data, index: 0 }
    }
}

impl<'a, T> Iterator for ChunkVecIterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        (self.index < self.data.len()).then(|| {
            let (r, c) = self.data.chunk_index(self.index);
            // Safety: `index` is valid so that r and c are valid as well.
            // And we take a single piece of mutable borrow in here.
            // Other parts can't be borrowed because we borrowed the whole one mutably.
            let item = unsafe {
                let chunk = self.data.chunks.get_unchecked_mut(r);
                let ptr = chunk.as_mut_ptr().add(c);
                &mut *ptr
            };
            self.index += 1;
            item
        })
    }
}

impl<T> ops::Index<usize> for ChunkVec<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl<T> ops::IndexMut<usize> for ChunkVec<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_bindgen_test::*;

    #[wasm_bindgen_test]
    fn test_chunkvec_push_pop() {
        let chunk_num = 2;
        let chunk_size = 2;
        let mut v = ChunkVec::new(chunk_size);
        for r in 0..chunk_num {
            let mut expect = vec![];
            for c in 0..chunk_size {
                let value = r * chunk_size + c;
                v.push(value);
                expect.push(value);

                let index = r * chunk_size + c;
                assert_eq!(expect, v.chunks[r]);
                assert_eq!(expect[c], v[index]);
                assert_eq!(index + 1, v.len());
            }
            assert_eq!(expect.len(), v.chunks[r].len());
        }

        for r in (0..chunk_num).rev() {
            let mut expect = v.chunks[r].clone();
            for _c in 0..chunk_size {
                assert_eq!(expect, v.chunks[r]);

                let expect_val = expect.pop();
                let popped_val = v.pop();

                assert_eq!(expect_val, popped_val);
            }
        }

        assert_eq!(chunk_num * chunk_size, v.capacity());
        assert_eq!(
            v.capacity(),
            v.chunks.iter().map(|chunk| chunk.capacity()).sum()
        );

        v.shrink_to_fit();
        assert_eq!(chunk_size, v.capacity());
    }

    #[wasm_bindgen_test]
    fn test_chunkvec_swapremove() {
        let chunk_size = 8;
        let item_num = 13;
        let chunk_num = (item_num as f32 / chunk_size as f32).ceil() as usize;
        let mut v = ChunkVec::new(chunk_size);
        let mut expect = vec![];
        for i in 0..item_num {
            v.push(i);
            expect.push(i);
        }

        enum Pos {
            Start,
            Middle,
            End,
        }

        let mut pos = Pos::Start;
        for _i in 0..item_num {
            let index = match pos {
                Pos::Start => {
                    pos = Pos::Middle;
                    0
                }
                Pos::Middle => {
                    pos = Pos::End;
                    v.len() / 2
                }
                Pos::End => {
                    pos = Pos::Start;
                    v.len() - 1
                }
            };
            let expect_val = expect.swap_remove(index);
            let popped_val = v.swap_remove(index);

            assert_eq!(expect_val, popped_val);
            assert_eq!(expect.len(), v.len());
            for j in 0..v.len() {
                assert_eq!(expect[j], v[j]);
            }
        }

        assert!(v.is_empty());
        assert_eq!(chunk_num * chunk_size, v.capacity());

        v.shrink_to_fit();
        assert_eq!(chunk_size, v.capacity());
        assert_eq!(
            v.capacity(),
            v.chunks.iter().map(|chunk| chunk.capacity()).sum()
        );
    }
}
