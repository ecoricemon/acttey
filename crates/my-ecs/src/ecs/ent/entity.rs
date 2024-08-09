use super::component::ComponentKey;
use crate::{ds::prelude::*, util::prelude::*};
use std::{
    any::TypeId,
    fmt,
    ptr::NonNull,
    slice,
    sync::{atomic::AtomicI32, Arc},
};

pub trait Entity: 'static {
    /// Provided.
    fn key() -> EntityKey {
        EntityKey::Type(EntityTypeId::of::<Self>())
    }

    /// Required.
    fn move_to<T: AddEntity + ?Sized>(self, cont: &mut T) -> usize;
}

/// A trait for collecting heterogeneous static types.
/// In this trait, each type is gathered in each column and all columns have the same length like 2d matrix.
///
/// When it comes to in & out types, this trait has intentionally raw pointer parameters not to use generic for object safety.
/// So that you can provide your own data structure.
/// There's built-in implementation [`SparseSet`](super::sparse_set::SparseSet) which is based on [`ChunkAnyVec`](crate::ds::vec::ChunkAnyVec).
#[allow(clippy::len_without_is_empty)]
pub trait ContainEntity: RegisterComponent + BorrowComponent + AddEntity {
    fn new_from_this(&self) -> Box<dyn ContainEntity>;

    /// * `ci` - Index of the column(component container).
    /// * `ri` - Index of the item.
    ///   This may be different from index in a column.
    ///   In other words, you can't use the same `ri` as an index for columns that you get from
    ///   [`BorrowComponent::borrow_column`] or [`BorrowComponent::borrow_column_mut`].
    ///   For example, if an implementation underlying the trait is sparse set,
    ///   `ri` must be an index in sparse layer, and index in a column may be an index in dense layer.
    fn get_item_mut(&mut self, ci: usize, ri: usize) -> Option<NonNull<u8>>;

    /// Returnes the number of items.
    fn len(&self) -> usize;
}

// Need to be object safe.
pub trait RegisterComponent {
    fn add_column(&mut self, tinfo: TypeInfo) -> Option<usize>;
    fn remove_column(&mut self, ci: usize) -> Option<TypeInfo>;
    fn contains_column(&self, ty: &TypeId) -> bool;
    fn get_column_index(&self, ty: &TypeId) -> Option<usize>;
    fn get_column_info(&self, ci: usize) -> Option<&TypeInfo>;
    fn get_column_num(&self) -> usize;
}

// Need to be object safe.
pub trait BorrowComponent {
    /// Borrows a column(component container) from the entity container.
    /// You can access an item through its index from 0 to the number of items.
    //
    // Implementation must guarantee that all items are able to referenced by index
    // from 0 to RawGetter::len().
    fn borrow_column(&self, ci: usize) -> BorrowResult<RawGetter, AtomicI32>;

    /// Borrows a column(component container) from the entity container.
    /// You can access an item through its index from 0 to the number of items.
    //
    // Implementation must guarantee that all items are able to referenced by index
    // from 0 to RawGetter::len().
    fn borrow_column_mut(&mut self, ci: usize) -> BorrowResult<RawGetter, AtomicI32>;
}

// Need to be object safe.
pub trait AddEntity {
    /// Prepares to add a new row.
    fn begin_add_row(&mut self);

    /// # Safety
    ///
    /// Undefined behavior if `ptr` is invalid.
    unsafe fn add_value(&mut self, ci: usize, ptr: NonNull<u8>);

    /// Finishes to add a row.
    fn end_add_row(&mut self) -> usize;

    /// Removes the row by row index.
    fn remove_row_by_outer_index(&mut self, ri: usize) -> bool;

    /// Removes the row by inner index which is an index used in column.
    /// * `index` - Index in a column, not index of the item.
    fn remove_row_by_inner_index(&mut self, index: usize);
}

/// A specific entity identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EntityId {
    /// Entity container index.
    ei: EntityIndex,

    /// Item index.
    itemi: usize,
}

impl EntityId {
    pub const fn new(ei: EntityIndex, itemi: usize) -> Self {
        Self { ei, itemi }
    }

    pub const fn container_index(&self) -> EntityIndex {
        self.ei
    }

    pub const fn item_index(&self) -> usize {
        self.itemi
    }
}

/// Key for the map [`EntityStorage`] in order to get value [`EntityContainer`].
/// `EntityStorage` provides some access ways shown below.
/// - Index: Entity container index and generation when the container is generated.
/// - Name: Unique name for the entity. Each entity must have its name.
/// - Type: If the entity is declared statically, it has its own type.
#[derive(Debug, Clone)]
pub enum EntityKey {
    /// Unique name for the entity.
    /// All entities have their name.
    Name(EntityName),

    /// Entity container index.
    /// Registered entities to [`EntityStorage`] have their indices to the dictionary.
    Index(EntityIndex),

    /// Type id of the entity.
    /// Statically defined entities have their type ids.
    Type(EntityTypeId),
}

impl EntityKey {
    pub fn name(&self) -> &EntityName {
        if let Self::Name(name) = self {
            name
        } else {
            panic!("{:?} is not Name", self);
        }
    }

    pub fn index(&self) -> &EntityIndex {
        if let Self::Index(index) = self {
            index
        } else {
            panic!("{:?} is not Name", self);
        }
    }

    pub fn ty(&self) -> &EntityTypeId {
        if let Self::Type(ty) = self {
            ty
        } else {
            panic!("{:?} is not Type", self);
        }
    }
}

impl_from_for_enum!(EntityKey, Name, EntityName);
impl_from_for_enum!(EntityKey, Index, EntityIndex);
impl_from_for_enum!(EntityKey, Type, EntityTypeId);

/// [`Arc<str>`] of entity.
/// An entity must have its unique name.
#[derive(Hash, PartialEq, Eq, Clone, Debug)]
#[repr(transparent)]
pub struct EntityName(Arc<str>);

impl EntityName {
    pub const fn new(name: Arc<str>) -> Self {
        Self(name)
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for EntityName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// [`GenIndex`] of entity container.
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
#[repr(transparent)]
pub struct EntityIndex(GenIndex<u32, u32>);

impl EntityIndex {
    pub const fn new(index: GenIndex<u32, u32>) -> Self {
        Self(index)
    }

    pub fn generation(&self) -> u32 {
        *self.0.get_generation()
    }

    pub fn index(&self) -> usize {
        *self.0.get_index() as usize
    }
}

impl fmt::Display for EntityIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// [`TypeId`] of an entity.
pub type EntityTypeId = ATypeId<EntityTypeId_>;
pub struct EntityTypeId_;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum EntityKeyKind {
    /// Corresponds to [`EntityKey::Name`].
    Name,
    /// Corresponds to [`EntityKey::Index`].
    Index,
    /// Corresponds to [`EntityKey::Type`].
    Type,
}

impl From<&EntityKey> for EntityKeyKind {
    fn from(value: &EntityKey) -> Self {
        match value {
            EntityKey::Name(..) => EntityKeyKind::Name,
            EntityKey::Index(..) => EntityKeyKind::Index,
            EntityKey::Type(..) => EntityKeyKind::Type,
        }
    }
}

/// A piece of information about an entity such as entity index, name, and its components.
#[derive(Debug, Eq)]
pub struct EntityTag {
    /// Corresponds to [`EntityKey::Index`].
    index: EntityIndex,

    /// Corresponds to [`EntityKey::Name`].
    name: EntityName,

    /// Component keys that belong to the entity.
    comp_keys: ManagedConstPtr<ComponentKey>,

    /// Corresponds to [`EntityContainer::comp_names`].
    comp_names: ManagedConstPtr<&'static str>,

    /// Length of [`Self::comp_keys`] slice and [`Self::comp_names`] slice.
    comp_len: usize,
}

impl EntityTag {
    /// Creates a tag including pointers to the given arguments.
    /// So caller must guarantee validity for those pointers.
    ///
    /// # Safety
    ///
    /// Undefined behavior if `name`, `comp_keys` or `comp_names` are invalidated.
    /// For example, it's undfeind if one of them is dropped or aliased
    /// as mutable reference while the tag lives.
    pub unsafe fn new(
        index: EntityIndex,
        name: EntityName,
        comp_keys: &[ComponentKey],
        comp_names: &[&'static str],
    ) -> Self {
        debug_assert_eq!(comp_keys.len(), comp_names.len());

        let comp_len = comp_keys.len();
        let comp_keys = NonNull::new_unchecked(comp_keys.as_ptr().cast_mut());
        let comp_keys = ManagedConstPtr::new(comp_keys.into());
        let comp_names = NonNull::new_unchecked(comp_names.as_ptr().cast_mut());
        let comp_names = ManagedConstPtr::new(comp_names.into());

        Self {
            index,
            name,
            comp_keys,
            comp_names,
            comp_len,
        }
    }

    pub fn index(&self) -> EntityIndex {
        self.index
    }

    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn comp_keys(&self) -> &[ComponentKey] {
        let ptr = self.comp_keys.as_ptr();

        // Safety: Caller who called `Self::new()` guarantees validity of the pointers.
        unsafe { slice::from_raw_parts(ptr, self.comp_len) }
    }

    pub fn comp_names(&self) -> &[&'static str] {
        let ptr = self.comp_names.as_ptr();

        // Safety: Caller who called `Self::new()` guarantees validity of the pointers.
        unsafe { slice::from_raw_parts(ptr, self.comp_len) }
    }
}

impl PartialEq for EntityTag {
    fn eq(&self, other: &Self) -> bool {
        self.index() == other.index()
    }
}
