use crate::{ds::prelude::*, util::prelude::*};
use std::{
    any::Any,
    cmp,
    collections::HashMap,
    hash::{BuildHasher, Hash},
    ptr::NonNull,
    sync::atomic::AtomicI32,
};

/// There are two types of resources.
/// First one is static resource which is defined internally.
/// The other one is user resource which is defined by users.
/// This structure has pointers to those resources and dosen't update it once it's set.
/// Because, resource is a kind of unique data storage, so it makes sense.
#[derive(Debug)]
pub struct ResourceStorage<S> {
    /// Owned resources.
    owned: HashMap<ResourceKey, Box<dyn Any>, S>,

    /// Raw pointers to resources.
    /// Pointers to owned resources are guaranteed to be valid by the struct.
    /// Other pointers must be kept to be valid by client code.
    /// They must be well aligned, not aliased, and alive.
    ptrs: OptVec<SimpleHolder<NonNullExt<u8>, AtomicI32>, S>,

    /// [`ResourceKey`] -> index in `Self::ptrs`.
    imap: HashMap<ResourceKey, usize, S>,

    /// Grouped indices to `Self::ptrs` by [`ResourceKind`].
    kind_idxs: HashMap<ResourceKind, Vec<usize>, S>,

    /// Dedicated resources, which are not allowed to be sent to other workers.
    /// So they must be handled by main worker.
    /// For example, in web environment, we must send JS objects through postMessage().
    /// That means objects that are not posted can't be accessed from other workers.
    /// Plus, ecs objects will be dedicated resource in most cases.
    dedi_idxs: Vec<bool>,
}

impl<S> ResourceStorage<S>
where
    S: Default,
{
    pub(super) fn new() -> Self {
        Self {
            owned: HashMap::default(),
            ptrs: OptVec::new(),
            imap: HashMap::default(),
            kind_idxs: HashMap::default(),
            dedi_idxs: Vec::new(),
        }
    }
}

impl<S> ResourceStorage<S>
where
    S: BuildHasher + Default,
{
    /// Registers the resource.
    /// If the registration succeeded, returns its resource index.
    /// Otherwise, nothing takes place and returns received value.
    /// In other words, the old resouce data won't be dropped.
    pub(super) fn register(
        &mut self,
        key: ResourceKey,
        value: MaybeOwned,
        is_dedicated: bool,
    ) -> Result<usize, MaybeOwned> {
        if self.imap.contains_key(&key) {
            return Err(value);
        }

        let ptr = match value {
            MaybeOwned::A(mut owned) => {
                // Safety: Infallible.
                let ptr = unsafe { NonNull::new_unchecked(&mut *owned as *mut dyn Any as *mut u8) };
                let must_none = self.owned.insert(key, owned);
                debug_assert!(must_none.is_none());
                ptr
            }
            MaybeOwned::B(ptr) => ptr,
        };

        // Attaches ResourceKey's type info to the pointer for debug.
        let ptr = NonNullExt::from(ptr).with_type(**key.get_inner());

        // Adds the pointer.
        let holder = SimpleHolder::new(ptr);
        let index = self.ptrs.add(holder);

        // Adds the index to the pointer list.
        self.imap.insert(key, index);

        // Adds resource kind mapping.
        let kind = key.kind();
        self.kind_idxs
            .entry(kind)
            .and_modify(|idxs| idxs.push(index))
            .or_insert(vec![index]);

        // Adds dedicated mapping.
        if self.dedi_idxs.len() < index + 1 {
            self.dedi_idxs.resize(index + 1, false);
        }
        self.dedi_idxs[index] = is_dedicated;

        Ok(index)
    }

    pub fn contains<Q>(&self, key: &Q) -> bool
    where
        ResourceKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.imap.contains_key(key)
    }

    pub fn get_index<Q>(&self, key: &Q) -> Option<usize>
    where
        ResourceKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.imap.get(key).cloned()
    }

    pub fn is_dedicated(&self, index: usize) -> bool {
        self.dedi_idxs[index]
    }

    pub fn is_dedicated2<Q>(&self, key: &Q) -> bool
    where
        ResourceKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        if let Some(index) = self.imap.get(key) {
            self.is_dedicated(*index)
        } else {
            false
        }
    }

    pub fn borrow(&self, index: usize) -> BorrowResult<ManagedConstPtr<u8>, AtomicI32> {
        if let Some(holder) = self.ptrs.get(index) {
            holder
                .borrow()
                .map(|borrowed| borrowed.map(|ptr| unsafe { ManagedConstPtr::new(ptr) }))
        } else {
            Err(BorrowError::OutOfBound)
        }
    }

    pub fn borrow2<Q>(&self, key: &Q) -> BorrowResult<ManagedConstPtr<u8>, AtomicI32>
    where
        ResourceKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        if let Some(index) = self.get_index(key) {
            self.borrow(index)
        } else {
            Err(BorrowError::NotFound)
        }
    }

    pub fn borrow_mut(&mut self, index: usize) -> BorrowResult<ManagedMutPtr<u8>, AtomicI32> {
        if let Some(holder) = self.ptrs.get_mut(index) {
            holder
                .borrow_mut()
                .map(|borrowed| borrowed.map(|ptr| unsafe { ManagedMutPtr::new(ptr) }))
        } else {
            Err(BorrowError::OutOfBound)
        }
    }

    pub fn borrow_mut2<Q>(&mut self, key: &Q) -> BorrowResult<ManagedMutPtr<u8>, AtomicI32>
    where
        ResourceKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        if let Some(index) = self.get_index(key) {
            self.borrow_mut(index)
        } else {
            Err(BorrowError::NotFound)
        }
    }

    pub fn borrow_kind<Q>(&self, kind: &Q) -> Option<ResourceIter<S>>
    where
        ResourceKind: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        Some(ResourceIter {
            ptrs: &self.ptrs,
            idxs: self.kind_idxs.get(kind)?,
            cur: 0,
        })
    }

    pub fn borrow_kind_mut<Q>(&mut self, kind: &Q) -> Option<ResourceIterMut<S>>
    where
        ResourceKind: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        Some(ResourceIterMut {
            ptrs: &mut self.ptrs,
            idxs: self.kind_idxs.get(kind)?,
            cur: 0,
        })
    }
}

impl<S> Default for ResourceStorage<S>
where
    S: Default,
{
    fn default() -> Self {
        Self::new()
    }
}

pub struct ResourceIter<'a, S> {
    ptrs: &'a OptVec<SimpleHolder<NonNullExt<u8>, AtomicI32>, S>,
    idxs: &'a [usize],
    cur: usize,
}

impl<'a, S> Iterator for ResourceIter<'a, S>
where
    S: BuildHasher,
{
    type Item = BorrowResult<ManagedConstPtr<u8>, AtomicI32>;

    fn next(&mut self) -> Option<Self::Item> {
        let index = self.idxs.get(self.cur)?;
        self.cur += 1;

        // Safety: We picked the index up among valid indices `idxs`.
        let holder = unsafe { self.ptrs.get_unchecked(*index) };

        let borrowed = holder
            .borrow()
            .map(|borrowed| borrowed.map(|ptr| unsafe { ManagedConstPtr::new(ptr) }));

        Some(borrowed)
    }
}

impl<'a, S> ExactSizeIterator for ResourceIter<'a, S>
where
    S: BuildHasher,
{
    fn len(&self) -> usize {
        self.idxs.len() - self.cur
    }
}

pub struct ResourceIterMut<'a, S> {
    ptrs: &'a mut OptVec<SimpleHolder<NonNullExt<u8>, AtomicI32>, S>,
    idxs: &'a [usize],
    cur: usize,
}

impl<'a, S> Iterator for ResourceIterMut<'a, S>
where
    S: BuildHasher,
{
    type Item = BorrowResult<ManagedMutPtr<u8>, AtomicI32>;

    fn next(&mut self) -> Option<Self::Item> {
        let index = self.idxs.get(self.cur)?;
        self.cur += 1;

        // Safety: We picked the index up among valid indices `idxs`.
        let holder = unsafe { self.ptrs.get_unchecked_mut(*index) };

        let borrowed = holder
            .borrow_mut()
            .map(|borrowed| borrowed.map(|ptr| unsafe { ManagedMutPtr::new(ptr) }));

        Some(borrowed)
    }
}

impl<'a, S> ExactSizeIterator for ResourceIterMut<'a, S>
where
    S: BuildHasher,
{
    fn len(&self) -> usize {
        self.idxs.len() - self.cur
    }
}

/// Unique data over entire application.
pub trait Resource: 'static {
    fn key() -> ResourceKey {
        ResourceKey {
            inner: ResourceKeyInner::of::<Self>(),
            kind: ResourceKind::Etc("unknown"),
        }
    }

    fn rkey(&self) -> ResourceKey {
        Self::key()
    }
}

/// [`TypeId`](std::any::TypeId) of a [`Resource`].
#[derive(Debug, Clone, Copy)]
pub struct ResourceKey {
    inner: ResourceKeyInner,
    kind: ResourceKind,
}

impl ResourceKey {
    pub fn of<T: 'static>(kind: ResourceKind) -> Self {
        Self {
            inner: ResourceKeyInner::of::<T>(),
            kind,
        }
    }

    pub const fn get_inner(&self) -> &ResourceKeyInner {
        &self.inner
    }

    pub const fn kind(&self) -> ResourceKind {
        self.kind
    }

    pub fn is_kind_of(&self, kind: ResourceKind) -> bool {
        self.kind == kind
    }
}

impl PartialEq for ResourceKey {
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl Eq for ResourceKey {}

impl PartialOrd for ResourceKey {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ResourceKey {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.inner.cmp(&other.inner)
    }
}

impl Hash for ResourceKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state)
    }
}

type ResourceKeyInner = ATypeId<ResourceKey>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResourceKind {
    EventQueue,
    Etc(&'static str),
}

pub type MaybeOwned = Or<Box<dyn Any>, NonNull<u8>>;
