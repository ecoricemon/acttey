use crate::{
    ds::{
        borrow::{BorrowError, Borrowed, SimpleHolder},
        vec::OptVec,
    },
    ecs::borrow_js::JsAtomic,
};
use std::{
    any::{Any, TypeId},
    collections::HashMap,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

// TODO: More safe way to have pointers?
/// There are two types of resources.
/// First one is static resource which is defined internally.
/// The other one is user resource which is defined by users.
/// This structure has pointers to those resources and dosen't update it once it's set.
/// Because, resource is a kind of unique data storage, so it makes sense.
///
/// [`AppState`](crate::top::app::AppState) owns all kinds of static resources and it guarantees that pointers are valid over the entire application's lifetime.
/// On the other hand, this structure owns user resources for the sake of pointer validity.
#[derive(Debug)]
pub struct ResourcePack {
    /// [`ResourceKey`] -> index.
    imap: HashMap<ResourceKey, usize, ahash::RandomState>,

    /// Owned user resources.
    _user: HashMap<ResourceKey, Box<dyn Any>>,

    /// Raw pointers to resources.  
    /// This field grabs just pointers for performance reason.
    /// So, you must guarantee the pointer's validity.
    /// In other words, they must properly aligned and must not be moved or dropped.
    /// See [`AppState`](crate::top::app::AppState), the owner of all kinds of basic resources, for more details.
    /// Plus, [`Self::user`] guarantees user defined resources to be valid.
    ptrs: OptVec<SimpleHolder<NonNull<u8>, JsAtomic>>,
}

impl ResourcePack {
    pub fn new() -> Self {
        Self {
            imap: HashMap::default(),
            _user: HashMap::default(),
            ptrs: OptVec::new(),
        }
    }

    pub fn register_default_resource(&mut self, rkey: ResourceKey, ptr: NonNull<u8>) -> usize {
        let holder = SimpleHolder::new(ptr);
        let index = self.ptrs.add(holder);
        self.imap.insert(rkey, index);
        index
    }

    #[inline]
    pub fn get_index(&self, rkey: &ResourceKey) -> Option<usize> {
        self.imap.get(rkey).cloned()
    }

    pub fn borrow(&self, index: usize) -> Result<Borrowed<NonNull<u8>, JsAtomic>, BorrowError> {
        if let Some(holder) = self.ptrs.get(index) {
            holder.borrow()
        } else {
            Err(BorrowError::OutOfBound)
        }
    }

    pub fn borrow_mut(
        &mut self,
        index: usize,
    ) -> Result<Borrowed<NonNull<u8>, JsAtomic>, BorrowError> {
        if let Some(holder) = self.ptrs.get_mut(index) {
            holder.borrow_mut()
        } else {
            Err(BorrowError::OutOfBound)
        }
    }
}

impl Default for ResourcePack {
    fn default() -> Self {
        Self::new()
    }
}

/// Unique data over entire application.
pub trait Resource: 'static {
    fn key() -> ResourceKey {
        ResourceKey::new(TypeId::of::<Self>())
    }

    fn rkey(&self) -> ResourceKey {
        Self::key()
    }
}

/// [`TypeId`] for [`Resource`].
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct ResourceKey(TypeId);

impl ResourceKey {
    pub const fn new(ty: TypeId) -> Self {
        Self(ty)
    }
}

#[derive(Debug)]
pub struct ResourceRef<T> {
    /// Pointer to a [`Borrowed`] of the resource buffer in the [`RequestBuffer`](super::request::RequestBuffer).
    /// When this structure is dropped, `Borrowed` is also dropped, and it stops borrowing.
    ptr: NonNull<Borrowed<NonNull<u8>, JsAtomic>>,

    _marker: PhantomData<T>,
}

impl<T> ResourceRef<T> {
    pub fn new(ptr: NonNull<Borrowed<NonNull<u8>, JsAtomic>>) -> Self {
        Self {
            ptr,
            _marker: PhantomData,
        }
    }
}

impl<T> Deref for ResourceRef<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // Safety: Pointers to resource are valid by the `Scheduler`.
        unsafe {
            let borrowed = self.ptr.as_ref();
            borrowed.cast().as_ref()
        }
    }
}

impl<T> Drop for ResourceRef<T> {
    fn drop(&mut self) {
        // Safety: Pointers to resource are valid by the `Scheduler`.
        unsafe {
            self.ptr.as_ptr().drop_in_place();
        }
    }
}

#[derive(Debug)]
pub struct ResourceMut<T> {
    /// Pointer to a [`Borrowed`] of the resource buffer in the [`RequestBuffer`](super::request::RequestBuffer).
    /// When this structure is dropped, `Borrowed` is also dropped, and it stops borrowing.
    ptr: NonNull<Borrowed<NonNull<u8>, JsAtomic>>,

    _marker: PhantomData<T>,
}

impl<T> ResourceMut<T> {
    pub fn new(ptr: NonNull<Borrowed<NonNull<u8>, JsAtomic>>) -> Self {
        Self {
            ptr,
            _marker: PhantomData,
        }
    }
}

impl<T> Deref for ResourceMut<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // Safety: Pointers to resource are valid by the `Scheduler`.
        unsafe {
            let borrowed = self.ptr.as_ref();
            borrowed.cast().as_ref()
        }
    }
}

impl<T> DerefMut for ResourceMut<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // Safety: Pointers to resource are valid by the `Scheduler`.
        unsafe {
            let borrowed = self.ptr.as_mut();
            borrowed.cast().as_mut()
        }
    }
}

impl<T> Drop for ResourceMut<T> {
    fn drop(&mut self) {
        // Safety: Pointers to resource are valid by the `Scheduler`.
        unsafe {
            self.ptr.as_ptr().drop_in_place();
        }
    }
}
