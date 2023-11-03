use crate::ty;
use std::{
    any::{Any, TypeId},
    cell::RefCell,
    rc::Rc,
};

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
            type_id: ty!(<T>),
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
        assert_eq!(self.type_id, ty!(<T>));
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
        debug_assert_eq!(self.type_id, ty!(<T>));
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
        debug_assert_eq!(self.type_id, ty!(<T>));
        debug_assert!(index < self.len());
        unsafe { (*(self.raw as *mut Vec<T>)).get_unchecked(index) }
    }

    /// # Panics
    ///
    /// Calling this method with incorrect type causes panic.
    #[inline]
    pub fn get_mut<T: 'static>(&self, index: usize) -> Option<&mut T> {
        assert_eq!(self.type_id, ty!(<T>));
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
        debug_assert_eq!(self.type_id, ty!(<T>));
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
        debug_assert_eq!(self.type_id, ty!(<T>));
        debug_assert!(index < self.len());
        unsafe { (*(self.raw as *mut Vec<T>)).get_unchecked_mut(index) }
    }

    /// # Panics
    ///
    /// Calling this method with incorrect type causes panic.
    #[inline]
    pub fn push<T: 'static>(&mut self, value: T) {
        assert_eq!(self.type_id, ty!(<T>));
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
        debug_assert_eq!(self.type_id, ty!(<T>));
        unsafe {
            (*(self.raw as *mut Vec<T>)).push(value);
        }
    }

    /// # Panics
    ///
    /// Calling this method with incorrect type causes panic.
    #[inline]
    pub fn pop<T: 'static>(&mut self) -> Option<T> {
        assert_eq!(self.type_id, ty!(<T>));
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
        debug_assert_eq!(self.type_id, ty!(<T>));
        unsafe { (*(self.raw as *mut Vec<T>)).pop() }
    }

    /// # Panics
    ///
    /// Calling this method with incorrect type or out of bound `index` causes panic.
    #[inline]
    pub fn swap_remove<T: 'static>(&mut self, index: usize) -> T {
        assert_eq!(self.type_id, ty!(<T>));
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
        debug_assert_eq!(self.type_id, ty!(<T>));
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
        debug_assert_eq!(self.type_id, ty!(<T>));
        unsafe { (*(self.raw as *mut Vec<T>)).resize_with(new_len, f) }
    }
}

impl<'a, T: 'static> From<&'a AnyVec> for &'a Vec<T> {
    /// # Panics
    ///
    /// Calling this method with incorrect type causes panic.
    fn from(value: &'a AnyVec) -> Self {
        assert_eq!(value.type_id, ty!(<T>));
        unsafe { &*(value.raw as *const Vec<T>) }
    }
}

impl<'a, T: 'static> From<&'a mut AnyVec> for &'a mut Vec<T> {
    /// # Panics
    ///
    /// Calling this method with incorrect type causes panic.
    fn from(value: &'a mut AnyVec) -> Self {
        assert_eq!(value.type_id, ty!(<T>));
        unsafe { &mut *(value.raw as *mut Vec<T>) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_bindgen_test::*;

    wasm_bindgen_test_configure!(run_in_browser);

    #[derive(PartialEq, Debug, Clone, Copy)]
    struct SA {
        x: [usize; 2],
    }

    #[derive(PartialEq, Debug, Clone, Copy)]
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
}
