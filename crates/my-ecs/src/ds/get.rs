use std::{marker::PhantomData, ptr::NonNull};

#[derive(Debug, Clone)]
pub struct RawGetter {
    me: NonNull<u8>,
    len: usize,
    fn_get: unsafe fn(me: NonNull<u8>, index: usize) -> NonNull<u8>,
}

impl RawGetter {
    /// # Safety
    ///
    /// Pointer must be valid while this is alive.
    pub const unsafe fn new(
        me: NonNull<u8>,
        len: usize,
        fn_get: unsafe fn(me: NonNull<u8>, index: usize) -> NonNull<u8>,
    ) -> Self {
        Self { me, len, fn_get }
    }

    pub fn get(&self, index: usize) -> Option<NonNull<u8>> {
        if index < self.len {
            unsafe { Some(self.get_unchecked(index)) }
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// Undefined behavior if `index` is out of bound.
    pub unsafe fn get_unchecked(&self, index: usize) -> NonNull<u8> {
        // We assume that self.me is valid and not aliased.
        (self.fn_get)(self.me, index)
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.len
    }
}

/// Method wrapper of [`RawGetter`] for the `T`.
#[derive(Debug)]
pub struct Getter<T> {
    raw_getter: RawGetter,
    _marker: PhantomData<T>,
}

impl<T> Getter<T> {
    /// # Safety
    ///
    /// Undefined behavior if
    /// - [`RawGetter`] is invalid. See it's constructor.
    /// - Type is incorrect.
    pub const unsafe fn new(raw_getter: RawGetter) -> Self {
        Self {
            raw_getter,
            _marker: PhantomData,
        }
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.raw_getter.len()
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        if index < self.len() {
            // Safety: We checked the length.
            unsafe { Some(self.get_unchecked(index)) }
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// Undefined behavior if `index` is out of bound.
    pub unsafe fn get_unchecked(&self, index: usize) -> &T {
        let ptr = self.raw_getter.get_unchecked(index).cast();
        ptr.as_ref()
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        if index < self.len() {
            // Safety: `index` is in bounds.
            unsafe { Some(self.get_unchecked_mut(index)) }
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// Undefined behavior if `index` is out of bound.
    pub unsafe fn get_unchecked_mut(&mut self, index: usize) -> &mut T {
        let mut ptr = self.raw_getter.get_unchecked(index).cast();
        ptr.as_mut()
    }

    pub fn iter(&self) -> GetterIter<T> {
        // Safety: Warning in the constructor.
        unsafe { GetterIter::new(self.raw_getter.clone()) }
    }

    pub fn iter_mut(&mut self) -> GetterIterMut<T> {
        // Safety: Warning in the constructor.
        unsafe { GetterIterMut::new(self.raw_getter.clone()) }
    }
}

#[derive(Debug, Clone)]
pub struct GetterIter<'a, T>(GetterIterMut<'a, T>);

impl<'a, T> GetterIter<'a, T> {
    /// # Safety
    ///
    /// Undefined behavior if
    /// - [`RawGetter`] is invalid. See it's constructor.
    /// - Type is incorrect.
    pub const unsafe fn new(getter: RawGetter) -> Self {
        Self(GetterIterMut::new(getter))
    }
}

impl<'a, T: 'a> Iterator for GetterIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|item| &*item)
    }
}

#[derive(Debug, Clone)]
pub struct GetterIterMut<'a, T> {
    getter: RawGetter,
    cur: usize,
    _marker: PhantomData<&'a T>,
}

impl<'a, T> GetterIterMut<'a, T> {
    /// # Safety
    ///
    /// Undefined behavior if
    /// - [`RawGetter`] is invalid. See it's constructor.
    /// - Type is incorrect.
    pub const unsafe fn new(getter: RawGetter) -> Self {
        Self {
            getter,
            cur: 0,
            _marker: PhantomData,
        }
    }
}

impl<'a, T: 'a> Iterator for GetterIterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cur < self.getter.len() {
            // Safety: Index is in bounds.
            let res = unsafe { self.getter.get_unchecked(self.cur) };
            self.cur += 1;
            // Safety: Warning in the constroctor.
            unsafe { Some(res.cast().as_mut()) }
        } else {
            None
        }
    }
}
