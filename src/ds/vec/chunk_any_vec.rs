use super::{
    super::common::{FnCloneRaw, FnDropRaw, TypeInfo},
    AnyVec,
};
use crate::util::PowerOfTwo;
use std::{
    any::TypeId,
    cmp, mem,
    ptr::{self, NonNull},
};

#[derive(Debug, Clone)]
pub struct ChunkAnyVec {
    /// The very first chunk is used for special purpose.
    /// If the type is zero sized type, only the first chunk is used.
    /// Otherwise, that is used for the cloning.
    chunks: Vec<AnyVec>,

    /// Maximum length and initial capacity of each chunk.
    /// Zero is allowed and it's considered as usize::MAX + 1.
    chunk_len: PowerOfTwo,

    /// Number of items in all chunks.
    len: usize,

    /// Current capacity.
    cap: usize,

    /// If the type is zero sized, this value is 1 due to the first special chunk.
    /// Otherwise, this value is 0.
    chunk_offset: u8,

    /// Vector can grow as long as this value.
    max_cap: usize,
}

impl ChunkAnyVec {
    /// If the type size is zero, then `chunk_len` is ignored.
    pub fn new(tinfo: TypeInfo, chunk_len: usize) -> Self {
        let mut v = Self {
            chunks: vec![AnyVec::new(tinfo)],
            chunk_len: PowerOfTwo::new(chunk_len).unwrap(),
            len: 0,
            cap: 0,
            max_cap: 0,
            chunk_offset: 1,
        };

        // Same limitation with `Vec` or `AnyVec`.
        v.max_cap = isize::MAX as usize / v.chunks[0].padded_item_size();

        // If the type is zero sized, we won't allocate any memory for the vector.
        // But, adjust capacity like `Vec`.
        if v.is_zst() {
            v.chunk_len = PowerOfTwo::new(0).unwrap();
            v.cap = usize::MAX;
            v.max_cap = usize::MAX;
            v.chunk_offset = 0;
        } else if v.chunk_len() > isize::MAX as usize {
            // We need to restrict isize::MAX + 1.
            panic!("chunk_len must be less than isize::MAX");
        }

        v
    }

    #[inline]
    pub fn type_info(&self) -> &TypeInfo {
        self.chunks[0].type_info()
    }

    #[inline]
    pub fn type_id(&self) -> TypeId {
        self.chunks[0].type_id()
    }

    #[inline]
    pub fn type_name(&self) -> &'static str {
        self.chunks[0].type_name()
    }

    #[inline]
    pub fn item_size(&self) -> usize {
        self.chunks[0].item_size()
    }

    #[inline]
    pub fn is_zst(&self) -> bool {
        self.chunks[0].is_zst()
    }

    #[inline]
    pub fn align(&self) -> usize {
        self.chunks[0].align()
    }

    #[inline]
    pub fn fn_drop(&self) -> FnDropRaw {
        self.chunks[0].fn_drop()
    }

    #[inline]
    pub fn fn_clone(&self) -> FnCloneRaw {
        self.chunks[0].fn_clone()
    }

    #[inline]
    pub fn is_type_of(&self, ty: &TypeId) -> bool {
        self.chunks[0].is_type_of(ty)
    }

    #[inline]
    pub const fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline]
    pub const fn capacity(&self) -> usize {
        self.cap
    }

    #[inline]
    pub const fn chunk_len(&self) -> usize {
        self.chunk_len.get()
    }

    #[inline]
    pub fn reserve(&mut self, add_num: usize) {
        self.reserve_exact(add_num);
    }

    pub fn reserve_exact(&mut self, add_num: usize) {
        let need_cap = self.len.saturating_add(add_num);
        if self.capacity() < need_cap {
            if need_cap > self.max_capacity() {
                panic!(
                    "can't allocate {need_cap} x {} bytes",
                    self.padded_item_size()
                );
            }

            // Rounds up the target capacity to be a multiple of chunk length.
            // This operation doesn't overflow because cap and len are restricted.
            let new_cap = (need_cap + self.chunk_len() - 1) & !(self.chunk_len() - 1);

            // If the new_cap is clamped by this operation, the last chunk length is same with the other length - 1.
            let new_cap = cmp::min(new_cap, self.max_capacity());

            let mut remain = new_cap - self.capacity();
            while remain > 0 {
                let partial = cmp::min(self.chunk_len(), remain);
                let chunk = self.new_chunk(partial);
                self.chunks.push(chunk);

                remain -= partial;
            }

            self.cap = new_cap;
        }
    }

    /// # Safety
    ///
    /// - `new_len` must be less than or equal to [`Self::capacity`].
    /// - Caller must initialized extended items.
    #[inline]
    pub unsafe fn set_len(&mut self, new_len: usize) {
        debug_assert!(new_len <= self.capacity());

        self.len = new_len;
    }

    /// Caller should make sure data pointed by `ptr` not to be dropped.
    /// To make it not to be dropped, call [`std::mem::forget`].
    ///
    /// # Safety
    ///
    /// `ptr` must point to valid data type.
    pub unsafe fn push_raw(&mut self, ptr: *const u8) {
        self.reserve(1);

        let (ci, _) = self.index_2d(self.len());
        self.chunks[ci].push_raw(ptr);

        // Safety: Infallible.
        unsafe { self.set_len(self.len().checked_add(1).unwrap()) };
    }

    #[inline]
    pub fn push<T: 'static>(&mut self, value: T) {
        debug_assert!(self.is_type_of(&TypeId::of::<T>()));

        // Safety:: Infallible.
        unsafe { self.push_raw(&value as *const T as *const u8) }
        mem::forget(value);
    }

    /// # Safety
    ///
    /// - `index` must be in bound.
    /// - `ptr` must point to valid data type.
    #[inline]
    pub unsafe fn update_unchecked(&mut self, index: usize, ptr: *const u8) {
        let (ci, ii) = self.index_2d(index);
        self.chunks[ci].update_unchecked(ii, ptr);
    }

    /// Don't forget to call destructor.
    ///
    /// # Safety
    ///
    /// `buf` must valid for writes of [`Self::item_size`] bytes.
    pub unsafe fn pop_raw(&mut self, buf: *mut u8) -> Option<()> {
        if self.is_empty() {
            None
        } else {
            // Safety: Vector is not empty.
            let chunk = self._pop();
            chunk.pop_raw(buf);
            Some(())
        }
    }

    pub fn pop<T: 'static>(&mut self) -> Option<T> {
        if self.is_empty() {
            None
        } else {
            // Safety: Vector is not empty.
            let chunk = unsafe { self._pop() };
            chunk.pop()
        }
    }

    pub fn pop_drop(&mut self) -> Option<()> {
        if self.is_empty() {
            None
        } else {
            // Safety: Vector is not empty.
            let chunk = unsafe { self._pop() };
            chunk.pop_drop()
        }
    }

    /// Reduces length by 1 and returns the last chunk.
    /// Don't forget to call pop from the chunk.
    ///
    /// # Safety
    ///
    /// Length of the vector must not be zero.
    unsafe fn _pop(&mut self) -> &mut AnyVec {
        // Safety: Decreasing is safe.
        self.set_len(self.len() - 1);

        let (ci, _) = self.index_2d(self.len());

        // Safety: `r` is valid.
        unsafe { self.chunks.get_unchecked_mut(ci) }
    }

    /// Don't forget to call destructor.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound..
    ///
    /// # Safety
    ///
    /// `buf` must valid for writes of [`Self::item_size`] bytes.
    pub unsafe fn swap_remove_raw(&mut self, index: usize, buf: *mut u8) {
        // len - 1 can overflow but it causes panic in swap().
        self.swap(index, self.len() - 1);
        self.pop_raw(buf);
    }

    /// # Panics
    ///
    /// Panics if `index` is out of bound..
    pub fn swap_remove<T: 'static>(&mut self, index: usize) -> T {
        // len - 1 can overflow but it causes panic in swap().
        self.swap(index, self.len() - 1);
        self.pop().unwrap()
    }

    pub fn swap_remove_drop(&mut self, index: usize) {
        // len - 1 can overflow but it causes panic in swap().
        self.swap(index, self.len() - 1);
        self.pop_drop();
    }

    pub fn swap(&mut self, index0: usize, index1: usize) {
        assert!(index0 < self.len());
        assert!(index1 < self.len());

        unsafe {
            let (ci0, ii0) = self.index_2d(index0);
            let (ci1, ii1) = self.index_2d(index1);
            let ptr0 = self.chunks[ci0].get_ptr(ii0);
            let ptr1 = self.chunks[ci1].get_ptr(ii1);
            if ptr0 != ptr1 {
                ptr::swap_nonoverlapping(ptr0, ptr1, self.item_size());
            }
        }
    }

    #[inline]
    pub fn get_raw(&self, index: usize) -> Option<NonNull<u8>> {
        if index < self.len() {
            unsafe { Some(self.get_raw_unchecked(index)) }
        } else {
            None
        }
    }

    #[inline]
    pub unsafe fn get_raw_unchecked(&self, index: usize) -> NonNull<u8> {
        let (ci, ii) = self.index_2d(index);
        self.chunks[ci].get_raw_unchecked(ii)
    }

    #[inline]
    pub fn get<T: 'static>(&self, index: usize) -> Option<&T> {
        if index < self.len() {
            let (ci, ii) = self.index_2d(index);
            // Type is checked here.
            self.chunks[ci].get(ii)
        } else {
            None
        }
    }

    #[inline]
    pub fn get_mut<T: 'static>(&mut self, index: usize) -> Option<&mut T> {
        if index < self.len() {
            let (ci, ii) = self.index_2d(index);
            // Type is checked here.
            self.chunks[ci].get_mut(ii)
        } else {
            None
        }
    }

    pub fn resize_with<T, F>(&mut self, new_len: usize, mut f: F)
    where
        T: 'static,
        F: FnMut() -> T,
    {
        debug_assert!(self.is_type_of(&TypeId::of::<T>()));

        if new_len > self.len() {
            let mut remain = new_len - self.len();

            self.reserve(remain);

            while remain > 0 {
                let (ci, _) = self.index_2d(self.len() - 1);
                let clen = self.chunks[ci].len();
                let partial = cmp::min(self.chunk_len() - clen, remain);
                self.chunks[ci].resize_with(clen + partial, &mut f);

                remain -= partial;
            }

            unsafe {
                self.set_len(new_len);
            }
        } else {
            self.truncate(new_len);
        }
    }

    pub fn truncate(&mut self, len: usize) {
        if len >= self.len() {
            return;
        }

        let mut remain = self.len() - len;
        while remain > 0 {
            let (ci, _) = self.index_2d(self.len() - 1);
            let clen = self.chunks[ci].len();
            let partial = cmp::min(clen, remain);
            self.chunks[ci].truncate(clen - partial);

            remain -= partial;
        }

        unsafe {
            self.set_len(len);
        }
    }

    /// Drops redundant chunks such that the capacity is close to the twice the current length.
    /// If the capacity is less than that, this method doens't do anything.
    pub fn shrink(&mut self) {
        if self.is_zst() {
            return;
        }

        let target = self.len() * 2;
        if target > isize::MAX as usize {
            return;
        }

        let target = (target + self.chunk_len() - 1) & !(self.chunk_len() - 1);

        if let Some(redundant) = self.capacity().checked_sub(target) {
            let preserve = self.chunks.len() - redundant / self.chunk_len();
            self.chunks.truncate(preserve);
            self.cap = target;
        }
    }

    /// See [`AnyVec::padded_item_size`].
    #[inline]
    pub(crate) fn padded_item_size(&self) -> usize {
        self.chunks[0].padded_item_size()
    }

    /// Creates a new chunk and allocate memory for the chunk.
    #[inline]
    fn new_chunk(&self, cap: usize) -> AnyVec {
        let mut chunk = self.chunks[0].clone();
        chunk.reserve_exact(cap);
        chunk
    }

    /// Converts 1D index into 2D index.
    #[inline]
    const fn index_2d(&self, index: usize) -> (usize, usize) {
        (
            self.chunk_len.quotient(index) + self.chunk_offset as usize,
            self.chunk_len.remainder(index),
        )
    }

    #[inline]
    const fn max_capacity(&self) -> usize {
        self.max_cap
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_bindgen_test::*;
    use crate::tinfo;

    #[wasm_bindgen_test]
    fn test_chunkanyvec_push_pop() {
        let chunk_num = 4 * 4;
        let chunk_len = 2;
        let mut v = ChunkAnyVec::new(tinfo!(usize), chunk_len);
        for r in 0..chunk_num {
            for c in 0..chunk_len {
                let index = r * chunk_len + c;
                let value = r * chunk_len + c;

                v.push::<usize>(value);

                assert_eq!(index + 1, v.len());
                assert_eq!(Some(&value), v.get(index));
                assert_eq!((r + 1) * chunk_len, v.capacity());
            }
        }

        assert_eq!(chunk_num * chunk_len, v.capacity());
        assert_eq!(
            v.capacity(),
            v.chunks.iter().map(|chunk| chunk.capacity()).sum()
        );

        for r in (chunk_num / 4..chunk_num).rev() {
            for c in (0..chunk_len).rev() {
                let value = r * chunk_len + c;
                assert_eq!(Some(value), v.pop());
            }
        }

        v.shrink();
        assert_eq!(chunk_num / 4 * chunk_len, v.len());
        assert_eq!(v.len() * 2, v.capacity());
    }

    #[wasm_bindgen_test]
    fn test_chunkanyvec_swapremove() {
        let chunk_size = 8;
        let item_num = 13;
        let chunk_num = (item_num as f32 / chunk_size as f32).ceil() as usize;
        let mut v = ChunkAnyVec::new(tinfo!(i32), chunk_size);
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
                assert_eq!(expect.get(j), v.get(j));
            }
        }

        assert!(v.is_empty());
        assert_eq!(chunk_num * chunk_size, v.capacity());
    }
}
