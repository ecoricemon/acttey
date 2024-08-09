use std::{marker::PhantomData, ops::Deref, ptr::NonNull};

// `RawGetter` consists of pointers and integer. We can copy it.
#[derive(Debug, Clone, Copy)]
pub struct RawGetter {
    this: NonNull<u8>,
    len: usize,
    fn_get: unsafe fn(this: NonNull<u8>, index: usize) -> NonNull<u8>,
}

impl RawGetter {
    /// # Safety
    ///
    /// Pointer must be valid while this is alive.
    pub const unsafe fn new(
        this: NonNull<u8>,
        len: usize,
        fn_get: unsafe fn(me: NonNull<u8>, index: usize) -> NonNull<u8>,
    ) -> Self {
        Self { this, len, fn_get }
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
        (self.fn_get)(self.this, index)
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.len
    }
}

/// A wrapper of [`RawGetter`] for `&T`.
#[derive(Debug, Clone, Copy)]
pub struct Getter<T> {
    pub(crate) raw_getter: RawGetter,
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

    pub fn iter(&self) -> GetterIter<T> {
        GetterIter::new(self)
    }
}

/// A wapper of [`RawGetter`] for the `&mut T`.
//
// Unlike `Getter`, this is a kind of mutable reference.
// So this is not clonable.
#[derive(Debug)]
#[repr(transparent)]
pub struct GetterMut<T>(Getter<T>);

impl<T> GetterMut<T> {
    /// # Safety
    ///
    /// Undefined behavior if
    /// - [`RawGetter`] is invalid. See it's constructor.
    /// - Type is incorrect.
    pub const unsafe fn new(raw_getter: RawGetter) -> Self {
        Self(Getter::new(raw_getter))
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

    pub fn iter_mut(&mut self) -> GetterIterMut<T> {
        GetterIterMut::new(self)
    }
}

impl<T> Deref for GetterMut<T> {
    type Target = Getter<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone)]
pub struct GetterIter<'a, T: 'a> {
    raw_getter: RawGetter,
    cur: usize,
    _marker: PhantomData<&'a T>,
}

impl<'a, T> GetterIter<'a, T> {
    // Borrows `Getter` through explicit 'a lifetime.
    pub const fn new(getter: &'a Getter<T>) -> Self {
        Self::new_raw(getter.raw_getter)
    }

    // Follows lifetime defined by caller.
    pub const fn new_raw(raw_getter: RawGetter) -> Self {
        Self {
            raw_getter,
            cur: 0,
            _marker: PhantomData,
        }
    }
}

impl<'a, T> Iterator for GetterIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let value = self.raw_getter.get(self.cur)?;
        self.cur += 1;

        // Safety: Iterator is borrowing the getter.
        // So it's safe to use the pointer as reference.
        Some(unsafe { value.cast().as_ref() })
    }
}

#[derive(Debug)]
pub struct GetterIterMut<'a, T: 'a> {
    raw_getter: RawGetter,
    cur: usize,
    _marker: PhantomData<&'a mut T>,
}

impl<'a, T> GetterIterMut<'a, T> {
    // Borrows `GetterMut` mutably through explicit 'a lifetime.
    pub fn new(getter: &'a mut GetterMut<T>) -> Self {
        Self::new_raw(getter.raw_getter)
    }

    // Follows lifetime defined by caller.
    pub fn new_raw(raw_getter: RawGetter) -> Self {
        Self {
            raw_getter,
            cur: 0,
            _marker: PhantomData,
        }
    }
}

impl<'a, T> Iterator for GetterIterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        let value = self.raw_getter.get(self.cur)?;
        self.cur += 1;

        // Safety: Iterator is borrowing the getter.
        // So it's safe to use the pointer as reference.
        Some(unsafe { value.cast().as_mut() })
    }
}
