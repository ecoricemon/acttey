use std::{marker::PhantomData, ptr::NonNull};

#[derive(Debug)]
pub struct RawGetter {
    pub(crate) me: *mut u8,
    pub(crate) len: usize,
    pub(crate) fn_get: unsafe fn(me: *mut u8, index: usize) -> NonNull<u8>,
}

impl RawGetter {
    #[inline]
    pub fn get(&self, index: usize) -> Option<NonNull<u8>> {
        if index < self.len {
            unsafe { Some(self.get_unchecked(index)) }
        } else {
            None
        }
    }

    #[inline]
    pub unsafe fn get_unchecked(&self, index: usize) -> NonNull<u8> {
        (self.fn_get)(self.me, index)
    }

    #[inline]
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.len
    }
}

/// Method warpper of [`RawGetter`] for the `&T`.
/// `&RawGetter` and `Getter` are interchangeable by the [`From`].
#[derive(Debug)]
pub struct Getter<'a, T> {
    inner: &'a RawGetter,
    _marker: PhantomData<T>,
}

impl<'a, T> Getter<'a, T> {
    #[inline]
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
    #[inline]
    pub unsafe fn get_unchecked(&self, index: usize) -> &T {
        let ptr = self.inner.get_unchecked(index).cast();

        // Safety: `Storage` is responsible for giving data type of `T`.
        unsafe { ptr.as_ref() }
    }

    #[inline]
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    #[inline]
    pub fn iter(&self) -> GetterIter<T> {
        GetterIter {
            getter: self,
            remain: self.len(),
        }
    }
}

impl<'a, T> From<&'a RawGetter> for Getter<'a, T> {
    #[inline]
    fn from(value: &'a RawGetter) -> Self {
        Self {
            inner: value,
            _marker: PhantomData,
        }
    }
}

impl<'a, T> From<Getter<'a, T>> for &'a RawGetter {
    #[inline]
    fn from(value: Getter<'a, T>) -> Self {
        value.inner
    }
}

#[derive(Debug, Clone)]
pub struct GetterIter<'a, T> {
    getter: &'a Getter<'a, T>,
    remain: usize,
}

impl<'a, T> Iterator for GetterIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(remain) = self.remain.checked_sub(1) {
            self.remain = remain;
            // Safety: Infallible.
            unsafe { Some(self.getter.get_unchecked(remain)) }
        } else {
            None
        }
    }
}

/// Method warpper of [`RawGetter`] for the `&mut T`.
/// `&mut RawGetter` and `GetterMut` are interchangeable by the [`From`].
#[derive(Debug)]
pub struct GetterMut<'a, T> {
    inner: &'a mut RawGetter,
    _marker: PhantomData<T>,
}

impl<'a, T> GetterMut<'a, T> {
    #[inline]
    pub fn get(&mut self, index: usize) -> Option<&mut T> {
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
    #[inline]
    pub unsafe fn get_unchecked(&mut self, index: usize) -> &mut T {
        let mut ptr = self.inner.get_unchecked(index).cast();

        // Safety: `Storage` is responsible for giving data type of `T`.
        unsafe { ptr.as_mut() }
    }

    #[inline]
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    #[inline]
    pub fn iter(&mut self) -> GetterIterMut<T> {
        // Safety: Infallible.
        GetterIterMut {
            getter: unsafe { NonNull::new_unchecked(self as *mut GetterMut<T>) },
            remain: self.len(),
        }
    }
}

impl<'a, T> From<&'a mut RawGetter> for GetterMut<'a, T> {
    #[inline]
    fn from(value: &'a mut RawGetter) -> Self {
        Self {
            inner: value,
            _marker: PhantomData,
        }
    }
}

impl<'a, T> From<GetterMut<'a, T>> for &'a mut RawGetter {
    #[inline]
    fn from(value: GetterMut<'a, T>) -> Self {
        value.inner
    }
}

#[derive(Debug, Clone)]
pub struct GetterIterMut<'a, T> {
    getter: NonNull<GetterMut<'a, T>>,
    remain: usize,
}

impl<'a, T: 'a> Iterator for GetterIterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(remain) = self.remain.checked_sub(1) {
            self.remain = remain;
            // Safety: Infallible.
            unsafe { Some(self.getter.as_mut().get_unchecked(remain)) }
        } else {
            None
        }
    }
}
