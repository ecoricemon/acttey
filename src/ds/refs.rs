use crate::ds::DsError;
use smallvec::SmallVec;
use std::{
    cell::{Ref, RefCell, RefMut},
    ops::{Deref, DerefMut},
    rc::{Rc, Weak},
};

/// Value with reference counter.
/// This helps you track usage of value itself.
/// [`super::generational::GenIndexRc`] is similar, but its target is index, not value.
/// But notice that this size is greater than normal value because of reference counter.
/// So that using this as items in a vector will be a trade-off.
///
/// You can use inner value using deref coercion.  
#[derive(Debug)]
pub struct RcValue<T> {
    value: T,
    _ref: Rc<()>,
}

impl<T> RcValue<T> {
    #[inline]
    pub fn new(value: T) -> Self {
        Self {
            value,
            _ref: Rc::new(()),
        }
    }

    #[inline]
    pub fn strong_count(&self) -> usize {
        Rc::strong_count(&self._ref)
    }

    #[inline]
    pub fn weak_count(&self) -> usize {
        Rc::weak_count(&self._ref)
    }

    #[inline]
    pub fn into_inner(self) -> T {
        self.value
    }

    /// Clones inner reference only.
    #[inline]
    pub fn clone_ref(&self) -> Rc<()> {
        Rc::clone(&self._ref)
    }
}

impl<T: Default> Default for RcValue<T> {
    #[inline]
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl<T: Clone> Clone for RcValue<T> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
            _ref: self.clone_ref(),
        }
    }
}

impl<T> Deref for RcValue<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for RcValue<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<T> From<T> for RcValue<T> {
    #[inline]
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

/// For use of smallvec with const generic N.
pub struct Array<T, const N: usize>(pub [T; N]);

unsafe impl<T, const N: usize> smallvec::Array for Array<T, N> {
    type Item = T;

    fn size() -> usize {
        N
    }
}

#[derive(Debug)]
pub struct Weaks<T, const N: usize> {
    pub weaks: SmallVec<Array<Weak<T>, N>>,
    pub strongs: SmallVec<Array<Rc<T>, N>>,
}

impl<T, const N: usize> Weaks<T, N> {
    pub fn new() -> Self {
        Self {
            weaks: smallvec::smallvec![],
            strongs: smallvec::smallvec![],
        }
    }

    pub fn push(&mut self, value: &Rc<T>) {
        self.weaks.push(Rc::downgrade(value));
        self.strongs.clear();
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Weak<T>> {
        self.weaks.iter()
    }

    /// Don't forget to grab the return value because it will release upgraded references
    /// when it is dropped.
    pub fn upgrade(&mut self) -> Result<UpgradedWeaks<T, N>, DsError> {
        self.upgrade_lasting()?;
        Ok(UpgradedWeaks(self))
    }

    pub fn upgrade_lasting(&mut self) -> Result<(), DsError> {
        self.drop_upgraded();
        for weak in self.weaks.iter() {
            if let Some(strong) = weak.upgrade() {
                self.strongs.push(strong);
            } else {
                self.drop_upgraded();
                return Err(DsError::WeakUpgradeFail);
            }
        }
        Ok(())
    }

    pub fn drop_upgraded(&mut self) {
        self.strongs.clear();
    }
}

impl<T, const N: usize> Default for Weaks<T, N> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct UpgradedWeaks<'a, T, const N: usize>(&'a mut Weaks<T, N>);

impl<'a, T, const N: usize> UpgradedWeaks<'a, T, N> {
    pub fn get(&self) -> &[Rc<T>] {
        &self.0.strongs
    }
}

impl<'a, T, const N: usize> Drop for UpgradedWeaks<'a, T, N> {
    fn drop(&mut self) {
        self.0.drop_upgraded();
    }
}

#[derive(Debug)]
/// Rc<RefCell\<T\>>
pub struct RCell<T> {
    value: Rc<RefCell<T>>,
    tag: &'static str,
}

impl<T> RCell<T> {
    pub fn new(value: T) -> Self {
        Self {
            value: Rc::new(RefCell::new(value)),
            tag: "",
        }
    }

    pub fn with_tag(mut self, tag: &'static str) -> Self {
        self.tag = tag;
        self
    }

    #[cfg_attr(print_rcell, track_caller)]
    #[inline(always)]
    pub fn borrow(&self) -> Ref<'_, T> {
        #[cfg(print_rcell)]
        {
            if !self.tag.is_empty() {
                crate::log!(
                    "[RCell][{}_{}] borrow() at tid: {}, code: {}",
                    self.tag,
                    self.strong,
                    current_thread_id(),
                    Location::caller(),
                );
            }
        }

        self.value.borrow()
    }

    #[cfg_attr(print_rcell, track_caller)]
    #[inline(always)]
    pub fn borrow_mut(&self) -> RefMut<'_, T> {
        #[cfg(print_rcell)]
        {
            if !self.tag.is_empty() {
                crate::log!(
                    "[RCell][{}_{}] borrow_mut() at tid: {}, code: {}",
                    self.tag,
                    self.strong,
                    current_thread_id(),
                    Location::caller(),
                );
            }
        }

        self.value.borrow_mut()
    }
}

impl<T> Clone for RCell<T> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            value: Rc::clone(&self.value),
            tag: self.tag,
        }
    }
}

impl<T> Drop for RCell<T> {
    #[cfg_attr(print_rcell, track_caller)]
    fn drop(&mut self) {
        #[cfg(print_rcell)]
        {
            crate::log!(
                "[RCell][{}_{}] dropped at tid: {}, code: {}, strong count is now {}",
                self.tag,
                self.strong,
                current_thread_id(),
                Location::caller(),
                Rc::strong_count(&self.value) - 1,
            );
        }
    }
}
