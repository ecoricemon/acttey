use super::super::common::{FnCloneRaw, FnDropRaw, TypeInfo};
use std::{
    alloc::{self, Layout},
    any::TypeId,
    mem, ops,
    ptr::{self, NonNull},
    slice,
};

// TODO: Not tested in boundary conditions.
/// Vector containing same type of data without generic parameter.
#[derive(Debug)]
pub struct AnyVec {
    tinfo: TypeInfo,

    ptr: NonNull<u8>,
    len: usize,
    cap: usize,
}

impl AnyVec {
    /// You can use [`BasicTyped::type_info`] for uncloneable types or [`CloneableTyped::type_info`] to make `tinfo`.
    pub fn new(tinfo: TypeInfo) -> Self {
        let mut v = Self {
            tinfo,
            ptr: Self::aligned_dangling(tinfo.align),
            len: 0,
            cap: 0,
        };

        // If the type is zero sized, we won't allocate any memory for the vector.
        // But, adjust capacity like `Vec`.
        if v.is_zst() {
            v.cap = usize::MAX;
        }
        v
    }

    #[inline]
    pub const fn type_info(&self) -> &TypeInfo {
        &self.tinfo
    }

    #[inline]
    pub const fn type_id(&self) -> TypeId {
        self.tinfo.id
    }

    #[inline]
    pub const fn type_name(&self) -> &'static str {
        self.tinfo.name
    }

    #[inline]
    pub const fn item_size(&self) -> usize {
        self.tinfo.size
    }

    #[inline]
    pub const fn is_zst(&self) -> bool {
        self.item_size() == 0
    }

    #[inline]
    pub const fn align(&self) -> usize {
        self.tinfo.align
    }

    #[inline]
    pub const fn fn_drop(&self) -> FnDropRaw {
        self.tinfo.fn_drop
    }

    #[inline]
    pub const fn fn_clone(&self) -> FnCloneRaw {
        self.tinfo.fn_clone
    }

    #[inline]
    pub fn is_type_of(&self, ty: &TypeId) -> bool {
        &self.tinfo.id == ty
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

    pub fn reserve(&mut self, add_num: usize) {
        let need_cap = self.len().saturating_add(add_num);
        if self.capacity() < need_cap {
            let max_cap = self.max_capacity();
            if need_cap > max_cap {
                panic!(
                    "can't allocate {need_cap} x {} bytes",
                    self.padded_item_size()
                );
            }

            // self.capacity() * 2 doesn't overflow.
            // If sized, self.capacity() is less than or equal to isize::MAX.
            // otherwise, self.capacity() is usize::MAX, so that it can't reach.
            let new_cap = (self.capacity() * 2).clamp(need_cap, max_cap);

            // Safety: Infallible.
            unsafe { self._reserve(new_cap) };
        }
    }

    pub fn reserve_exact(&mut self, add_num: usize) {
        let need_cap = self.len().saturating_add(add_num);
        if self.capacity() < need_cap {
            if need_cap > self.max_capacity() {
                panic!(
                    "can't allocate {need_cap} x {} bytes",
                    self.padded_item_size()
                );
            }

            // Safety: Infallible.
            unsafe { self._reserve(need_cap) };
        }
    }

    /// # Safety
    ///
    /// - This method assumes the type is not zero sized type.
    /// - `new_cap` x [`Self::padded_item_size`] must not exceed isize::MAX.
    unsafe fn _reserve(&mut self, new_cap: usize) {
        let padded_item_size = self.padded_item_size();

        let new_size = new_cap * padded_item_size;
        let ptr = if self.capacity() == 0 {
            let layout = Layout::from_size_align(new_size, self.align()).unwrap();
            let ptr = unsafe { alloc::alloc(layout) };
            if ptr.is_null() {
                alloc::handle_alloc_error(layout);
            }
            ptr
        } else {
            let old_size = self.capacity() * padded_item_size;
            let old_layout = Layout::from_size_align(old_size, self.align()).unwrap();
            let ptr = unsafe { alloc::realloc(self.ptr.as_mut(), old_layout, new_size) };
            if ptr.is_null() {
                let layout = Layout::from_size_align(new_size, self.align()).unwrap();
                alloc::handle_alloc_error(layout);
            }
            ptr
        };
        self.ptr = unsafe { NonNull::new_unchecked(ptr) };
        self.cap = new_cap;
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
    pub unsafe fn push_raw(&mut self, ptr: NonNull<u8>) {
        if !self.is_zst() {
            self.reserve(1);

            // Safety: index is valid.
            self.update_unchecked(self.len(), ptr);
        }

        // Safety: Infallible.
        unsafe { self.set_len(self.len().checked_add(1).unwrap()) };
    }

    #[inline]
    pub fn push<T: 'static>(&mut self, mut value: T) {
        debug_assert!(self.is_type_of(&TypeId::of::<T>()));

        // Safety: Infallible.
        unsafe {
            let ptr = NonNull::new_unchecked(&mut value as *mut T as *mut u8);
            self.push_raw(ptr);
        }
        mem::forget(value);
    }

    /// # Safety
    ///
    /// - `lindex` must be in bound.
    /// - `ptr` must point to valid data type.
    #[inline]
    pub unsafe fn update_unchecked(&mut self, index: usize, ptr: NonNull<u8>) {
        let dst = self.get_ptr(index);
        ptr::copy_nonoverlapping(ptr.as_ptr(), dst, self.item_size());
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
            let ptr = self._pop();
            ptr::copy_nonoverlapping(ptr, buf, self.item_size());
            Some(())
        }
    }

    pub fn pop<T: 'static>(&mut self) -> Option<T> {
        debug_assert!(self.is_type_of(&TypeId::of::<T>()));

        if self.is_empty() {
            None
        } else {
            // Safety: Vector is not empty.
            let ptr = unsafe { self._pop() as *mut T };
            let value = unsafe { ptr.read() };
            Some(value)
        }
    }

    pub fn pop_drop(&mut self) -> Option<()> {
        if self.is_empty() {
            None
        } else {
            // Safety: Vector is not empty.
            unsafe { (self.fn_drop())(self._pop()) };
            Some(())
        }
    }

    /// # Safety
    ///
    /// Length of the vector must not be zero.
    unsafe fn _pop(&mut self) -> *mut u8 {
        // Safety: Decreasing is safe.
        self.set_len(self.len() - 1);

        // Safety: We're using `Layout::from_size_align` which restricts size to be under the limit.
        self.get_ptr(self.len())
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
            let ptr0 = self.get_ptr(index0);
            let ptr1 = self.get_ptr(index1);
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

    /// # Safety
    ///
    /// `index` must be inbound and result address must not overflow `isize`.
    #[inline]
    pub unsafe fn get_raw_unchecked(&self, index: usize) -> NonNull<u8> {
        NonNull::new_unchecked(self.get_ptr(index))
    }

    /// `index` is an index of T, not u8.
    ///
    /// # Safety
    ///
    /// `index` must be inbound and result address must not overflow `isize`.
    #[inline]
    pub const unsafe fn get_ptr(&self, index: usize) -> *mut u8 {
        let offset = index * self.padded_item_size();
        self.ptr.as_ptr().add(offset)
    }

    #[inline]
    pub fn get<T: 'static>(&self, index: usize) -> Option<&T> {
        debug_assert!(self.is_type_of(&TypeId::of::<T>()));

        self.get_raw(index)
            .map(|ptr| unsafe { (ptr.as_ptr() as *const T).as_ref().unwrap_unchecked() })
    }

    #[inline]
    pub fn get_mut<T: 'static>(&mut self, index: usize) -> Option<&mut T> {
        debug_assert!(self.is_type_of(&TypeId::of::<T>()));

        self.get_raw(index)
            .map(|ptr| unsafe { (ptr.as_ptr() as *mut T).as_mut().unwrap_unchecked() })
    }

    pub fn resize_with<T, F>(&mut self, new_len: usize, mut f: F)
    where
        T: 'static,
        F: FnMut() -> T,
    {
        debug_assert!(self.is_type_of(&TypeId::of::<T>()));

        if new_len > self.len() {
            self.reserve(new_len - self.len());

            let (mut offset, stride) = self.get_ptr_offset(self.len());

            let range = self.len()..new_len;
            for _ in range {
                let ptr = unsafe { self.ptr.as_ptr().add(offset) } as *mut T;
                unsafe { ptr.write(f()) };
                offset += stride;
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

        let (mut offset, stride) = self.get_ptr_offset(len);

        let range = len..self.len();
        for _ in range {
            unsafe {
                let ptr = self.ptr.as_ptr().add(offset);
                (self.fn_drop())(ptr);
            }
            offset += stride;
        }

        unsafe {
            self.set_len(len);
        }
    }

    pub fn as_vec_mut<T: 'static>(&mut self) -> TypedAnyVec<'_, T> {
        debug_assert!(self.is_type_of(&TypeId::of::<T>()));

        let typed = unsafe {
            Vec::from_raw_parts(self.ptr.as_ptr() as *mut T, self.len(), self.capacity())
        };
        TypedAnyVec { typed, any: self }
    }

    #[inline]
    pub fn as_slice<T: 'static>(&self) -> &[T] {
        debug_assert!(self.is_type_of(&TypeId::of::<T>()));

        unsafe { slice::from_raw_parts(self.ptr.as_ptr() as *const T, self.len()) }
    }

    #[inline]
    pub fn as_slice_mut<T: 'static>(&mut self) -> &mut [T] {
        debug_assert!(self.is_type_of(&TypeId::of::<T>()));

        unsafe { slice::from_raw_parts_mut(self.ptr.as_ptr() as *mut T, self.len()) }
    }

    pub(crate) const fn padded_item_size(&self) -> usize {
        // Our alignment is calculated by `mem::align_of::<T>()`,
        // which is used in `std::Layout::array()`.
        // I believe the alignment is a common divisor of `mem::size_of::<T>()`
        // so, no padding exists. Otherwise we may calculate padded size like below
        // instead of using `self.item_size` alone.
        // e.g. (self.item_size + self.align - 1) & (!(self.align - 1))

        self.item_size()
    }

    /// Converts start index into start pointer offset from the beginning of the vector and stride in bytes.
    /// If the type is zero sized, it will return all zeros.
    /// So, you must not use offset as loop counter.
    /// And caller must provide valid index.
    /// This method assumes that, therefore doesn't check either index validity and arithmetic overflow.
    #[inline]
    const fn get_ptr_offset(&self, index: usize) -> (usize, usize) {
        if self.is_zst() {
            (0, 0)
        } else {
            let padded_item_size = self.padded_item_size();
            // Valid pointer offset can't exceed isize::MAX.
            (index * padded_item_size, padded_item_size)
        }
    }

    /// Mimics [`NonNull::dangling`].
    /// This helps to use lots of ptr module's APIs because they request aligned pointer even if the type is zero sized.
    #[inline]
    fn aligned_dangling(align: usize) -> NonNull<u8> {
        NonNull::new(align as *mut u8).unwrap()
    }

    /// Maximum capacity only if the type is not zero sized.
    #[inline]
    const fn max_capacity(&self) -> usize {
        isize::MAX as usize / self.padded_item_size()
    }
}

impl Clone for AnyVec {
    fn clone(&self) -> Self {
        let ptr = if self.is_zst() || self.is_empty() {
            self.ptr
        } else {
            let padded_item_size = self.padded_item_size();

            let size = self.len() * padded_item_size;
            let layout = Layout::from_size_align(size, self.align()).unwrap();
            let ptr = unsafe { alloc::alloc(layout) };
            if ptr.is_null() {
                alloc::handle_alloc_error(layout);
            }

            let mut offset = 0;
            while offset < size {
                unsafe {
                    let src = self.ptr.as_ptr().add(offset);
                    let dst = ptr.add(offset);
                    // If data type doesn't support clone, panics here.
                    (self.fn_clone())(src, dst);
                }
                offset += padded_item_size;
            }

            NonNull::new(ptr).unwrap()
        };

        Self {
            tinfo: self.tinfo,
            ptr,
            len: self.len(),
            cap: self.len(),
        }
    }
}

impl Drop for AnyVec {
    fn drop(&mut self) {
        // Calls every drop method.
        self.truncate(0);

        // Releases the memory.
        if !self.is_zst() && self.capacity() > 0 {
            let size = self.capacity() * self.padded_item_size();
            let layout = Layout::from_size_align(size, self.align()).unwrap();
            unsafe { alloc::dealloc(self.ptr.as_ptr(), layout) };
        }
    }
}

/// [`Vec`]-like typed vector you can get from [`AnyVec`].
/// When this is dropped, any changes you've made reflect to the `AnyVec`.
pub struct TypedAnyVec<'a, T> {
    typed: Vec<T>,
    any: &'a mut AnyVec,
}

impl<T> ops::Deref for TypedAnyVec<'_, T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.typed
    }
}

impl<T> ops::DerefMut for TypedAnyVec<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.typed
    }
}

impl<T> Drop for TypedAnyVec<'_, T> {
    fn drop(&mut self) {
        let ptr = self.typed.as_mut_ptr() as *mut u8;
        self.any.ptr = NonNull::new(ptr).unwrap();
        self.any.len = self.typed.len();
        self.any.cap = self.typed.capacity();
        let v = mem::take(&mut self.typed);
        mem::forget(v);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_bindgen_test::*;

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
    fn test_anyvec_clone() {
        let mut a = AnyVec::new(crate::tinfo!(SA));
        let mut b = a.clone();

        a.push(SA { x: [0, 0] });
        b.push(SA { x: [1, 1] });
        let c = a.clone();
        let d = b.clone();

        assert_eq!(a.len(), c.len());
        assert_eq!(a.get::<SA>(0), c.get::<SA>(0));
        assert_eq!(b.len(), d.len());
        assert_eq!(b.get::<SA>(0), d.get::<SA>(0));
    }

    #[wasm_bindgen_test]
    #[should_panic]
    fn test_anyvec_uncloneable_panic() {
        struct S(i32);
        let mut a = AnyVec::new(crate::tinfo!(S));
        a.push(S(0));
        let _ = a.clone();
    }

    #[wasm_bindgen_test]
    fn test_anyvec_push_pop() {
        let mut a = AnyVec::new(crate::tinfo!(SA));
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
        let mut a = AnyVec::new(crate::tinfo!(SA));

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

        a.swap_remove_drop(1);
        assert_eq!(2, a.len());
        assert_eq!(Some(&SA { x: [0, 1] }), a.get(0));
        assert_eq!(Some(&SA { x: [4, 5] }), a.get(1));
    }

    #[wasm_bindgen_test]
    #[should_panic]
    fn test_anyvec_push_incorrect_type_panic() {
        let mut a = AnyVec::new(crate::tinfo!(SA));
        a.push(SB {
            x: [0, 1],
            y: [0.1, 0.2],
        });
    }

    #[wasm_bindgen_test]
    #[should_panic]
    fn test_anyvec_pop_incorrect_type_panic() {
        let mut a = AnyVec::new(crate::tinfo!(SB));
        a.push(SB {
            x: [0, 1],
            y: [0.1, 0.2],
        });
        let _ = a.pop::<SA>();
    }

    #[wasm_bindgen_test]
    fn test_anyvec_into_vec_push_pop() {
        let mut a = AnyVec::new(crate::tinfo!(SA));
        {
            let mut v = (&mut a).as_vec_mut::<SA>();
            v.push(SA { x: [0, 1] });
            v.push(SA { x: [2, 3] });
            assert_eq!(Some(SA { x: [2, 3] }), v.pop());
        }
        assert_eq!(Some(SA { x: [0, 1] }), a.pop::<SA>());
        assert_eq!(None, a.pop::<SA>());

        {
            let mut v = (&mut a).as_vec_mut::<SA>();
            v.push(SA { x: [0, 1] });
            v.push(SA { x: [2, 3] });
        }
        let v_imm = a.as_slice::<SA>();
        assert_eq!(Some(&SA { x: [0, 1] }), v_imm.get(0));
        assert_eq!(Some(&SA { x: [2, 3] }), v_imm.get(1));
    }
}
