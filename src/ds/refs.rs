use std::{
    hash::{Hash, Hasher},
    ops::{Deref, DerefMut},
    rc::Rc,
};

pub trait AsRc {
    fn strong_count(&self) -> usize;
    fn get_reference_counter(&self) -> &Rc<()>;
}

#[derive(Debug, Clone)]
pub struct WithRc<T> {
    value: T,
    _ref: Rc<()>,
}

impl<T> WithRc<T> {
    pub fn new(value: T) -> Self {
        Self {
            value,
            _ref: Rc::new(()),
        }
    }

    pub fn into_inner(self) -> T {
        self.value
    }

    pub fn get_inner(&self) -> &T {
        &self.value
    }
}

impl<T> Deref for WithRc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for WithRc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<T> AsRc for WithRc<T> {
    fn strong_count(&self) -> usize {
        Rc::strong_count(&self._ref)
    }

    fn get_reference_counter(&self) -> &Rc<()> {
        &self._ref
    }
}

impl<T: PartialEq> PartialEq for WithRc<T> {
    fn eq(&self, other: &Self) -> bool {
        // Is the same value?
        self.get_inner() == other.get_inner()
    }
}

impl<T: Eq> Eq for WithRc<T> {}

impl<T: Hash> Hash for WithRc<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.get_inner().hash(state);
    }
}
