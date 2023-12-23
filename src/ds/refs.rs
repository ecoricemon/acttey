use crate::util::current_thread_id;
use std::{
    cell::{Ref, RefCell, RefMut},
    panic::Location,
    rc::{Rc, Weak},
};

pub(crate) struct Weaks<T> {
    weaks: Vec<Weak<T>>,
    strongs: Vec<Rc<T>>,
}

impl<T> Weaks<T> {
    pub(crate) fn new() -> Self {
        Self {
            weaks: vec![],
            strongs: vec![],
        }
    }

    pub(crate) fn push(&mut self, value: &Rc<T>) {
        self.weaks.push(Rc::downgrade(value));
        self.strongs.clear();
    }

    pub(crate) fn iter(&self) -> std::slice::Iter<'_, Weak<T>> {
        self.weaks.iter()
    }

    #[inline]
    pub(crate) fn upgrade_lasting(&mut self) {
        if self.strongs.is_empty() {
            self.strongs.reserve_exact(self.weaks.len());
            for weak in self.weaks.iter() {
                self.strongs.push(weak.upgrade().unwrap());
            }
        }
    }

    #[inline]
    pub(crate) fn get_upgraded(&self) -> &[Rc<T>] {
        &self.strongs
    }

    pub(crate) fn drop_upgraded(&mut self) {
        self.strongs.clear();
    }
}

/// Rc<RefCell\<T\>>
pub(crate) struct RCell<T> {
    value: Rc<RefCell<T>>,
    tag: &'static str,
    strong: usize,
}

impl<T> RCell<T> {
    pub(crate) fn new(value: T) -> Self {
        Self {
            value: Rc::new(RefCell::new(value)),
            tag: "",
            strong: 1,
        }
    }

    pub(crate) fn with_tag(mut self, tag: &'static str) -> Self {
        self.tag = tag;
        self
    }

    #[cfg_attr(print_rcell, track_caller)]
    #[inline(always)]
    pub(crate) fn borrow(&self) -> Ref<'_, T> {
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
    pub(crate) fn borrow_mut(&self) -> RefMut<'_, T> {
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
            strong: Rc::strong_count(&self.value),
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
