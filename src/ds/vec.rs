use crate::{
    ty,
    util::{split_mut, PowerOfTwo},
};
use std::{
    any::{Any, TypeId},
    cell::RefCell,
    mem::swap,
    ops::{Index, IndexMut},
    rc::Rc,
};

// TODO: Use ChunkVec.
// And ecs::query::QueryIter and QueryIterMut had better returns Iter<Indexable> not Iter<&[]>.
// Provide sort method instead.
// impl Downcast for SparseSet is also related to this point.
pub struct AnyVec {
    type_id: TypeId,
    raw: *mut dyn Any,
    // Prevent inner Vec to be dropped and be used to call some functions without generic type
    fn_len: Box<dyn Fn() -> usize>,
    fn_capacity: Box<dyn Fn() -> usize>,
    fn_swap_remove_no_return: Box<dyn Fn(usize)>,
    fn_grow: Box<dyn Fn(usize)>,
}

impl AnyVec {
    pub fn new<T: 'static>() -> Self {
        AnyVec::_new(Vec::<T>::new())
    }

    pub fn with_capacity<T: 'static>(capacity: usize) -> Self {
        AnyVec::_new(Vec::<T>::with_capacity(capacity))
    }

    fn _new<T: 'static>(data: Vec<T>) -> Self {
        let data = Rc::new(RefCell::new(data));
        let raw = data.as_ptr() as *mut dyn Any;

        let data_len = data.clone();
        let fn_len = Box::new(move || data_len.borrow().len());

        let data_capacity = data.clone();
        let fn_capacity = Box::new(move || data_capacity.borrow().capacity());

        let data_swap_remove_no_return = data.clone();
        let fn_swap_remove_no_return = Box::new(move |index: usize| {
            data_swap_remove_no_return.borrow_mut().swap_remove(index);
        });

        let data_grow = data.clone();
        let fn_grow = Box::new(move |new_len: usize| {
            let mut data = data_grow.borrow_mut();
            debug_assert!(data.len() <= new_len);
            if data.capacity() < new_len {
                data.reserve_exact(new_len);
            }
            unsafe {
                data.set_len(new_len);
            }
        });

        Self {
            type_id: ty!(T),
            raw,
            fn_len,
            fn_capacity,
            fn_swap_remove_no_return,
            fn_grow,
        }
    }

    #[inline]
    pub fn is_type_of(&self, ty: &TypeId) -> bool {
        *ty == self.type_id
    }

    #[inline]
    pub fn len(&self) -> usize {
        (self.fn_len)()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        (self.fn_capacity)()
    }

    #[inline]
    pub fn swap_remove_no_return(&mut self, index: usize) {
        (self.fn_swap_remove_no_return)(index);
    }

    /// Sets the length to `new_len`.
    ///
    /// # Safety
    ///
    /// The elements at old_len..new_len must be initialized.
    ///
    /// # Panics
    ///
    /// In **DEBUG** mode, smaller `new_len` than current `len()` causes the panic.
    #[inline]
    pub unsafe fn grow(&mut self, new_len: usize) {
        (self.fn_grow)(new_len);
    }

    /// # Panics
    ///
    /// Calling this method with incorrect type causes panic.
    #[inline]
    pub fn get<T: 'static>(&self, index: usize) -> Option<&T> {
        assert_eq!(self.type_id, ty!(T));
        unsafe { self.get_type_unchecked::<T>(index) }
    }

    /// # Safety
    ///
    /// Calling this method with incorrect type causes *undefined behavior*.
    ///
    /// # Panics
    ///
    /// In **DEBUG** mode, safety conditions are checked and cause panic.
    #[inline]
    pub unsafe fn get_type_unchecked<T: 'static>(&self, index: usize) -> Option<&T> {
        debug_assert_eq!(self.type_id, ty!(T));
        unsafe { (*(self.raw as *mut Vec<T>)).get(index) }
    }

    /// # Safety
    ///
    /// Calling this method with incorrect type or out of bound `index`` causes *undefined behavior*.
    ///
    /// # Panics
    ///
    /// In **DEBUG** mode, safety conditions are checked and cause panic when they are met.
    #[inline]
    pub unsafe fn get_unchecked<T: 'static>(&self, index: usize) -> &T {
        debug_assert_eq!(self.type_id, ty!(T));
        debug_assert!(index < self.len());
        unsafe { (*(self.raw as *mut Vec<T>)).get_unchecked(index) }
    }

    /// # Panics
    ///
    /// Calling this method with incorrect type causes panic.
    #[inline]
    pub fn get_mut<T: 'static>(&self, index: usize) -> Option<&mut T> {
        assert_eq!(self.type_id, ty!(T));
        unsafe { self.get_type_unchecked_mut::<T>(index) }
    }

    /// # Safety
    ///
    /// Calling this method with incorrect type causes *undefined behavior*.
    ///
    /// # Panics
    ///
    /// In **DEBUG** mode, safety conditions are checked and cause panic.
    #[inline]
    pub unsafe fn get_type_unchecked_mut<T: 'static>(&self, index: usize) -> Option<&mut T> {
        debug_assert_eq!(self.type_id, ty!(T));
        unsafe { (*(self.raw as *mut Vec<T>)).get_mut(index) }
    }

    /// # Safety
    ///
    /// Calling this method with incorrect type or out of bound `index`` causes *undefined behavior*.
    ///
    /// # Panics
    ///
    /// In **DEBUG** mode, safety conditions are checked and cause panic when they are met.
    #[inline]
    pub unsafe fn get_unchecked_mut<T: 'static>(&mut self, index: usize) -> &mut T {
        debug_assert_eq!(self.type_id, ty!(T));
        debug_assert!(index < self.len());
        unsafe { (*(self.raw as *mut Vec<T>)).get_unchecked_mut(index) }
    }

    /// # Panics
    ///
    /// Calling this method with incorrect type causes panic.
    #[inline]
    pub fn push<T: 'static>(&mut self, value: T) {
        assert_eq!(self.type_id, ty!(T));
        unsafe {
            self.push_type_unchecked(value);
        }
    }

    /// # Safety
    ///
    /// Calling this method with incorrect type causes *undefined behavior*.
    ///
    /// # Panics
    ///
    /// In **DEBUG** mode, safety condition is checked and causes panic when that is met.
    #[inline]
    pub unsafe fn push_type_unchecked<T: 'static>(&mut self, value: T) {
        debug_assert_eq!(self.type_id, ty!(T));
        unsafe {
            (*(self.raw as *mut Vec<T>)).push(value);
        }
    }

    /// # Panics
    ///
    /// Calling this method with incorrect type causes panic.
    #[inline]
    pub fn pop<T: 'static>(&mut self) -> Option<T> {
        assert_eq!(self.type_id, ty!(T));
        unsafe { self.pop_type_unchecked() }
    }

    /// # Safety
    ///
    /// Calling this method with incorrect type causes *undefined behavior*.
    ///
    /// # Panics
    ///
    /// In **DEBUG** mode, safety condition is checked and causes panic when that is met.
    #[inline]
    pub unsafe fn pop_type_unchecked<T: 'static>(&mut self) -> Option<T> {
        debug_assert_eq!(self.type_id, ty!(T));
        unsafe { (*(self.raw as *mut Vec<T>)).pop() }
    }

    /// # Panics
    ///
    /// Calling this method with incorrect type or out of bound `index` causes panic.
    #[inline]
    pub fn swap_remove<T: 'static>(&mut self, index: usize) -> T {
        assert_eq!(self.type_id, ty!(T));
        assert!(index < self.len());
        unsafe { self.swap_remove_type_unchecked(index) }
    }

    /// # Safety
    ///
    /// Calling this method with incorrect type causes *undefined behavior*.
    ///
    /// # Panics
    ///
    /// In **DEBUG** mode, safety condition is checked and causes panic when that is met.
    #[inline]
    pub unsafe fn swap_remove_type_unchecked<T: 'static>(&mut self, index: usize) -> T {
        debug_assert_eq!(self.type_id, ty!(T));
        unsafe { (*(self.raw as *mut Vec<T>)).swap_remove(index) }
    }

    /// # Safety
    ///
    /// Calling this method with incorrect type causes *undefined behavior*.
    ///
    /// # Panics
    ///
    /// In **DEBUG** mode, safety condition is checked and causes panic when that is met.
    #[inline]
    pub unsafe fn resize_with<F, T: 'static>(&mut self, new_len: usize, f: F)
    where
        F: FnMut() -> T,
    {
        debug_assert_eq!(self.type_id, ty!(T));
        unsafe { (*(self.raw as *mut Vec<T>)).resize_with(new_len, f) }
    }
}

impl<'a, T: 'static> From<&'a AnyVec> for &'a Vec<T> {
    /// # Panics
    ///
    /// Calling this method with incorrect type causes panic.
    fn from(value: &'a AnyVec) -> Self {
        assert_eq!(value.type_id, ty!(T));
        unsafe { &*(value.raw as *const Vec<T>) }
    }
}

impl<'a, T: 'static> From<&'a AnyVec> for &'a [T] {
    /// # Panics
    ///
    /// Calling this method with incorrect type causes panic.
    fn from(value: &'a AnyVec) -> Self {
        <&Vec<T>>::from(value)
    }
}

impl<'a, T: 'static> From<&'a mut AnyVec> for &'a mut Vec<T> {
    /// # Panics
    ///
    /// Calling this method with incorrect type causes panic.
    fn from(value: &'a mut AnyVec) -> Self {
        assert_eq!(value.type_id, ty!(T));
        unsafe { &mut *(value.raw as *mut Vec<T>) }
    }
}

impl<'a, T: 'static> From<&'a mut AnyVec> for &'a mut [T] {
    /// # Panics
    ///
    /// Calling this method with incorrect type causes panic.
    fn from(value: &'a mut AnyVec) -> Self {
        <&mut Vec<T>>::from(value)
    }
}

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

    #[inline(always)]
    fn chunk_index(&self, index: usize) -> (usize, usize) {
        (
            self.chunk_size.quotient(index),
            self.chunk_size.remainder(index),
        )
    }

    #[inline(always)]
    fn new_chunk(&self) -> Vec<T> {
        Vec::with_capacity(self.chunk_size.value)
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.chunks.len() * self.chunk_size.value
    }

    #[inline]
    pub fn push(&mut self, value: T) {
        let chunk_size = self.chunk_size.value;

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

    #[inline]
    pub fn pop(&mut self) -> Option<T> {
        if self.is_empty() {
            None
        } else {
            let (r, c) = self.chunk_index(self.len - 1);
            // Safety: `r` and `c` are valid.
            let last_chunk = unsafe { self.chunks.get_unchecked_mut(r) };

            self.len -= 1;
            last_chunk.pop()
        }
    }

    #[inline]
    pub fn get(&self, index: usize) -> Option<&T> {
        // Safetey: `index` is valid.
        (index < self.len).then(|| unsafe { self.get_unchecked(index) })
    }

    #[inline]
    unsafe fn get_unchecked(&self, index: usize) -> &T {
        let (r, c) = self.chunk_index(index);
        self.chunks.get_unchecked(r).get_unchecked(c)
    }

    #[inline]
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        // Safetey: index is valid.
        (index < self.len).then(|| unsafe { self.get_unchecked_mut(index) })
    }

    #[inline]
    unsafe fn get_unchecked_mut(&mut self, index: usize) -> &mut T {
        let (r, c) = self.chunk_index(index);
        self.chunks.get_unchecked_mut(r).get_unchecked_mut(c)
    }

    #[inline]
    pub fn swap_remove(&mut self, index: usize) -> T {
        assert!(index < self.len);

        // Safety: Valid index guarantees following unsafe blocks are safe.
        let (old_r, old_c) = self.chunk_index(index);
        let (last_r, last_c) = self.chunk_index(self.len - 1);

        self.len -= 1;

        // Swaps.
        if old_r != last_r {
            let [old_chunk, last_chunk] = split_mut(&mut self.chunks, [old_r, last_r]);
            unsafe {
                swap(
                    old_chunk.get_unchecked_mut(old_c),
                    last_chunk.get_unchecked_mut(last_c),
                );
                last_chunk.pop().unwrap_unchecked()
            }
        } else {
            let last_chunk = unsafe { self.chunks.get_unchecked_mut(last_r) };
            if old_c != last_c {
                let [old_v, last_v] = split_mut(last_chunk, [old_c, last_c]);
                swap(old_v, last_v);
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

pub struct ChunkVecIter<'a, T: 'static> {
    data: &'a ChunkVec<T>,
    index: usize,
}

impl<'a, T> ChunkVecIter<'a, T> {
    fn new(data: &'a ChunkVec<T>) -> Self {
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

pub struct ChunkVecIterMut<'a, T: 'static> {
    data: &'a mut ChunkVec<T>,
    index: usize,
}

impl<'a, T> ChunkVecIterMut<'a, T> {
    fn new(data: &'a mut ChunkVec<T>) -> Self {
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

impl<T> Index<usize> for ChunkVec<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl<T> IndexMut<usize> for ChunkVec<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_bindgen_test::*;

    wasm_bindgen_test_configure!(run_in_browser);

    #[derive(PartialEq, Debug, Clone, Copy, Default)]
    struct SA {
        x: [usize; 2],
    }

    #[derive(PartialEq, Debug, Clone, Copy, Default)]
    struct SB {
        x: [usize; 2],
        y: [f32; 2],
    }

    #[wasm_bindgen_test]
    fn test_anyvec_push_pop() {
        let mut a = AnyVec::new::<SA>();
        assert_eq!(true, a.is_empty());

        a.push(SA { x: [0, 1] });
        assert_eq!(1, a.len());
        assert!(a.capacity() >= 1);
        assert_eq!(false, a.is_empty());

        a.push(SA { x: [2, 3] });
        assert_eq!(2, a.len());
        assert!(a.capacity() >= 2);
        assert_eq!(false, a.is_empty());

        assert_eq!(Some(SA { x: [2, 3] }), a.pop::<SA>());
        assert_eq!(1, a.len());
        assert!(a.capacity() >= 1);
        assert_eq!(false, a.is_empty());

        assert_eq!(Some(SA { x: [0, 1] }), a.pop::<SA>());
        assert_eq!(0, a.len());
        assert_eq!(true, a.is_empty());

        assert_eq!(None, a.pop::<SA>());
    }

    #[wasm_bindgen_test]
    fn test_anyvec_remove() {
        let mut a = AnyVec::new::<SA>();

        a.push(SA { x: [0, 1] });
        a.push(SA { x: [2, 3] });
        a.push(SA { x: [4, 5] });
        a.push(SA { x: [6, 7] });

        let removed = a.swap_remove(1);
        assert_eq!(SA { x: [2, 3] }, removed);
        assert_eq!(3, a.len());
        assert_eq!(Some(&SA { x: [0, 1] }), a.get(0));
        assert_eq!(Some(&SA { x: [6, 7] }), a.get(1));
        assert_eq!(Some(&SA { x: [4, 5] }), a.get(2));
    }

    #[wasm_bindgen_test]
    #[should_panic]
    fn test_anyvec_push_incorrect_type_panic() {
        let mut a = AnyVec::new::<SA>();
        a.push(SB {
            x: [0, 1],
            y: [0.1, 0.2],
        });
    }

    #[wasm_bindgen_test]
    #[should_panic]
    fn test_anyvec_pop_incorrect_type_panic() {
        let mut a = AnyVec::new::<SB>();
        a.push(SB {
            x: [0, 1],
            y: [0.1, 0.2],
        });
        let _ = a.pop::<SA>();
    }

    #[wasm_bindgen_test]
    fn test_anyvec_into_vec_push_pop() {
        let mut a = AnyVec::new::<SA>();
        let v: &mut Vec<SA> = (&mut a).into();
        v.push(SA { x: [0, 1] });
        v.push(SA { x: [2, 3] });
        assert_eq!(Some(SA { x: [2, 3] }), v.pop());
        assert_eq!(Some(SA { x: [0, 1] }), a.pop::<SA>());
        assert_eq!(None, a.pop::<SA>());

        let v: &mut Vec<SA> = (&mut a).into();
        v.push(SA { x: [0, 1] });
        v.push(SA { x: [2, 3] });
        let v_imm: &Vec<SA> = (&a).into();
        assert_eq!(Some(&SA { x: [0, 1] }), v_imm.get(0));
        assert_eq!(Some(&SA { x: [2, 3] }), v_imm.get(1));
    }

    #[wasm_bindgen_test]
    #[should_panic]
    fn test_anyvec_into_incorrect_type_panic() {
        let mut a = AnyVec::new::<SA>();
        let _: &mut Vec<SB> = (&mut a).into();
    }

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
