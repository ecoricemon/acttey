use crate::{ds::prelude::*, ecs::EcsError, util::prelude::*};
use std::{
    any::Any,
    collections::HashMap,
    fmt,
    hash::{BuildHasher, Hash},
    ptr::NonNull,
};

/// Unique data over entire application.
#[allow(private_interfaces)]
pub trait Resource: Send + 'static {
    #[doc(hidden)]
    fn key() -> ResourceKey {
        ResourceKey::of::<Self>()
    }
}

/// There are two types of resources.
/// First one is static resource which is defined internally.
/// The other one is user resource which is defined by users.
/// This struct has pointers to those resources and dosen't update it once it's set.
/// Because, resource is a kind of unique data storage, so it makes sense.
#[derive(Debug)]
pub(super) struct ResourceStorage<S> {
    /// Owned resources.
    owned: HashMap<ResourceKey, Box<dyn Any>, S>,

    /// Raw pointers to resources.
    /// Pointers to owned resources are guaranteed to be valid by the struct.
    /// Other pointers must be kept to be valid by client code.
    /// They must be well aligned, not aliased, and alive.
    ptrs: OptVec<SimpleHolder<NonNullExt<u8>>, S>,

    /// [`ResourceKey`] -> index in `Self::ptrs`.
    imap: HashMap<ResourceKey, ResourceIndex, S>,

    /// Dedicated resources, which are not allowed to be sent to other workers.
    /// So they must be handled by main worker.
    /// For example, in web environment, we must send JS objects through postMessage().
    /// That means objects that are not posted can't be accessed from other workers.
    /// Plus, ecs objects will be dedicated resource in most cases.
    is_dedi: Vec<bool>,

    /// Generation of each resource. The generation is when the resource is
    /// registered to this storage.
    res_gens: Vec<u64>,

    /// Generation that will be assigned to the next registered resource.
    gen: u64,
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
            is_dedi: Vec::new(),
            res_gens: Vec::new(),
            gen: 1,
        }
    }
}

impl<S> ResourceStorage<S>
where
    S: BuildHasher + Default,
{
    /// Registers a resource.
    ///
    /// If it succeeded, returns resource index for the resource. Otherwise,
    /// nothing takes place and returns error with the descriptor.
    pub(super) fn register(
        &mut self,
        desc: ResourceDesc,
    ) -> Result<ResourceIndex, EcsError<ResourceDesc>> {
        if self.imap.contains_key(&desc.key) {
            let reason = debug_format!("detected duplicated resource `{:?}`", desc.key);
            return Err(EcsError::DupResource(reason, desc));
        }

        let ResourceDesc {
            dedicated,
            key,
            data,
        } = desc;

        let ptr = match data {
            Or::A(mut owned) => {
                // Safety: Infallible.
                let ptr = unsafe { NonNull::new_unchecked(&mut *owned as *mut dyn Any as *mut u8) };
                let must_none = self.owned.insert(key, owned);
                debug_assert!(must_none.is_none());
                ptr
            }
            Or::B(ptr) => ptr,
        };

        // Attaches ResourceKey's type info to the pointer for the sake of
        // debugging.
        let ptr = NonNullExt::from_nonnull(ptr).with_type(*key.get_inner());

        // Adds the pointer.
        let holder = SimpleHolder::new(ptr);
        let index = self.ptrs.add(holder);
        let ri = ResourceIndex::new(index, self.gen);
        self.gen += 1;
        while self.res_gens.len() <= index {
            self.res_gens.push(0);
        }
        self.res_gens[index] = ri.generation();

        // Adds the index to the pointer list.
        self.imap.insert(key, ri);

        // Adds dedicated mapping.
        if self.is_dedi.len() < index + 1 {
            self.is_dedi.resize(index + 1, false);
        }
        self.is_dedi[index] = dedicated;

        Ok(ri)
    }

    pub(super) fn unregister(
        &mut self,
        rkey: &ResourceKey,
    ) -> Option<Or<Box<dyn Any>, NonNull<u8>>> {
        // Removes the resource from `self.owned`, `self.ptrs`, and `self.imap`.
        // But we don't have to remove `self.is_dedi`.
        if let Some(ri) = self.imap.remove(rkey) {
            let data = self.owned.remove(rkey);
            let ptr = self.ptrs.take(ri.index());

            // Safety: Pointer must exist.
            debug_assert!(ptr.is_some());
            let holder = unsafe { ptr.unwrap_unchecked() };
            let ptr = *holder.into_value();

            Some(if let Some(data) = data {
                Or::A(data)
            } else {
                Or::B(ptr)
            })
        } else {
            None
        }
    }

    pub(super) fn contains<Q>(&self, key: &Q) -> bool
    where
        ResourceKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.imap.contains_key(key)
    }

    pub(super) fn index<Q>(&self, key: &Q) -> Option<ResourceIndex>
    where
        ResourceKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.imap.get(key).cloned()
    }

    // For consistency
    #[allow(dead_code)]
    pub(super) fn is_dedicated(&self, ri: ResourceIndex) -> Option<bool> {
        if self.is_valid_index(&ri) {
            Some(self.is_dedi[ri.index()])
        } else {
            None
        }
    }

    pub(super) fn is_dedicated2<Q>(&self, key: &Q) -> Option<bool>
    where
        ResourceKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.imap.get(key).map(|ri| self.is_dedi[ri.index()])
    }

    pub(super) fn borrow(&self, ri: ResourceIndex) -> BorrowResult<ManagedConstPtr<u8>> {
        if self.is_valid_index(&ri) {
            if let Some(holder) = self.ptrs.get(ri.index()) {
                return holder
                    .borrow()
                    .map(|borrowed| borrowed.map(|ptr| unsafe { ManagedConstPtr::new(ptr) }));
            }
        }
        Err(BorrowError::OutOfBound)
    }

    pub(super) fn borrow2<Q>(&self, key: &Q) -> BorrowResult<ManagedConstPtr<u8>>
    where
        ResourceKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        if let Some(index) = self.index(key) {
            self.borrow(index)
        } else {
            Err(BorrowError::NotFound)
        }
    }

    pub(super) fn borrow_mut(&mut self, ri: ResourceIndex) -> BorrowResult<ManagedMutPtr<u8>> {
        if self.is_valid_index(&ri) {
            if let Some(holder) = self.ptrs.get_mut(ri.index()) {
                return holder
                    .borrow_mut()
                    .map(|borrowed| borrowed.map(|ptr| unsafe { ManagedMutPtr::new(ptr) }));
            }
        }
        Err(BorrowError::OutOfBound)
    }

    pub(super) fn borrow_mut2<Q>(&mut self, key: &Q) -> BorrowResult<ManagedMutPtr<u8>>
    where
        ResourceKey: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        if let Some(index) = self.index(key) {
            self.borrow_mut(index)
        } else {
            Err(BorrowError::NotFound)
        }
    }

    /// # Safety
    ///
    /// Undefine behavior if exclusive borrow happend before.
    //
    // Allows dead_code for test.
    #[cfg(test)]
    pub(super) unsafe fn get_ptr(&self, ri: ResourceIndex) -> Option<NonNullExt<u8>> {
        if self.is_valid_index(&ri) {
            self.ptrs
                .get(ri.index())
                .map(|holder| *holder.get_unchecked())
        } else {
            None
        }
    }

    fn is_valid_index(&self, ri: &ResourceIndex) -> bool {
        if let Some(gen) = self.res_gens.get(ri.index()).cloned() {
            gen == ri.generation()
        } else {
            false
        }
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

#[derive(Debug)]
pub struct ResourceDesc {
    pub dedicated: bool,
    pub(crate) key: ResourceKey,
    pub data: Or<Box<dyn Any>, NonNull<u8>>,
}

impl ResourceDesc {
    pub fn new() -> Self {
        struct Dummy;
        impl Resource for Dummy {}

        Self {
            dedicated: false,
            key: Dummy::key(),
            data: Or::B(NonNull::dangling()),
        }
    }

    pub fn with_dedicated(mut self, is_dedicated: bool) -> Self {
        self.dedicated = is_dedicated;
        self
    }

    pub fn with_owned<R: Resource>(mut self, data: R) -> Self {
        self.key = R::key();
        self.data = Or::A(Box::new(data));
        self
    }

    /// # Safety
    ///
    /// If caller registered the resource to ecs and accesses memory at the data
    /// address while ecs is working, it's undefined behavior.
    pub unsafe fn with_ptr<R: Resource>(mut self, data: *mut R) -> Self {
        self.key = R::key();
        self.data = Or::B(NonNull::new(data as *mut u8).unwrap());
        self
    }
}

impl Default for ResourceDesc {
    fn default() -> Self {
        Self::new()
    }
}

impl<R: Resource> From<R> for ResourceDesc {
    fn from(value: R) -> Self {
        ResourceDesc::new().with_owned(value)
    }
}

/// Unique identifier for a type implementing [`Resource`].
pub(crate) type ResourceKey = ATypeId<ResourceKey_>;
pub(crate) struct ResourceKey_;

/// A specific resource item identifier.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ResourceId {
    /// Index to a specific resource.
    ri: ResourceIndex,

    /// Index to a resource item in a type of resource container.
    ///
    /// To store resources of the same type, clients need to register a resource
    /// container for the type and then put resource items in it. Here, index to
    /// a resource item points to a specific item in the container by `usize`
    /// index and additional `u64` generation like [`ResourceIndex`].
    ii: With<usize, u64>,
}

impl ResourceId {
    pub const fn new(ri: ResourceIndex, ii: With<usize, u64>) -> Self {
        Self { ri, ii }
    }

    pub const fn resource_index(&self) -> ResourceIndex {
        self.ri
    }

    pub const fn item_index(&self) -> With<usize, u64> {
        self.ii
    }
}

/// Index to a specific resource.
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
#[repr(transparent)]
pub struct ResourceIndex(With<usize, u64>);

impl ResourceIndex {
    const DUMMY: Self = Self(With::new(usize::MAX, u64::MAX));

    pub const fn new(index: usize, gen: u64) -> Self {
        Self(With::new(index, gen))
    }

    pub const fn dummy() -> Self {
        Self::DUMMY
    }

    pub fn is_dummy(&self) -> bool {
        *self == Self::dummy()
    }

    pub fn index(&self) -> usize {
        self.0.value
    }

    pub fn generation(&self) -> u64 {
        self.0.with
    }
}

impl Default for ResourceIndex {
    fn default() -> Self {
        Self::dummy()
    }
}

impl fmt::Display for ResourceIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
