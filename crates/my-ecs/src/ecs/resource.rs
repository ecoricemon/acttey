use crate::ds::{
    borrow::{BorrowError, Borrowed, SimpleHolder},
    vec::OptVec,
};
use std::{
    any::{Any, TypeId},
    collections::HashMap,
    hash::BuildHasher,
    ptr::NonNull,
    sync::atomic::AtomicI32,
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
pub struct ResourcePack<S> {
    /// [`ResourceKey`] -> index.
    imap: HashMap<ResourceKey, usize, S>,

    /// Owned user resources.
    _user: HashMap<ResourceKey, Box<dyn Any>, S>,

    /// Raw pointers to resources.  
    /// This field holds just pointers for performance reason.
    /// So, you must guarantee the pointer's validity.
    /// In other words, they must properly aligned and must not be moved or dropped.
    /// See [`AppState`](crate::top::app::AppState), the owner of all kinds of basic resources, for more details.
    /// Plus, [`Self::user`] guarantees user defined resources to be valid.
    ptrs: OptVec<SimpleHolder<NonNull<u8>, AtomicI32>, S>,
}

impl<S> ResourcePack<S>
where
    S: Default,
{
    pub fn new() -> Self {
        Self {
            imap: HashMap::default(),
            _user: HashMap::default(),
            ptrs: OptVec::new(),
        }
    }
}

impl<S> ResourcePack<S>
where
    S: BuildHasher + Default,
{
    pub fn register(&mut self, rkey: ResourceKey, ptr: NonNull<u8>) -> usize {
        let holder = SimpleHolder::new(ptr);
        let index = self.ptrs.add(holder);
        self.imap.insert(rkey, index);
        index
    }

    pub fn get_index(&self, rkey: &ResourceKey) -> Option<usize> {
        self.imap.get(rkey).cloned()
    }

    pub fn borrow(&self, index: usize) -> Result<Borrowed<NonNull<u8>, AtomicI32>, BorrowError> {
        if let Some(holder) = self.ptrs.get(index) {
            holder.borrow()
        } else {
            Err(BorrowError::OutOfBound)
        }
    }

    pub fn borrow_mut(
        &mut self,
        index: usize,
    ) -> Result<Borrowed<NonNull<u8>, AtomicI32>, BorrowError> {
        if let Some(holder) = self.ptrs.get_mut(index) {
            holder.borrow_mut()
        } else {
            Err(BorrowError::OutOfBound)
        }
    }
}

impl<S> Default for ResourcePack<S>
where
    S: Default,
{
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
#[repr(transparent)]
pub struct ResourceKey(TypeId);

impl ResourceKey {
    pub const fn new(ty: TypeId) -> Self {
        Self(ty)
    }
}

impl From<TypeId> for ResourceKey {
    fn from(value: TypeId) -> Self {
        Self::new(value)
    }
}
