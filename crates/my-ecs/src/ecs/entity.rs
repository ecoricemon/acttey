use super::{
    component::{Component, ComponentKey},
    sparse_set::SparseSet,
};
use crate::ds::{
    borrow::{BorrowError, Borrowed, SimpleHolder},
    get::{Getter, RawGetter},
    map::{DescribeGroup, GroupMap},
    types::TypeInfo,
};
use std::{
    any::TypeId,
    borrow::Cow,
    collections::HashMap,
    fmt::Debug,
    hash::BuildHasher,
    marker::PhantomData,
    mem::MaybeUninit,
    ops::{Deref, DerefMut},
    ptr::{self, NonNull},
    slice,
    sync::{atomic::AtomicI32, Arc},
};

pub trait Entity: 'static {
    /// Provided.
    fn key() -> EntityKey<'static> {
        EntityKey::Type(EntityKeyType(TypeId::of::<Self>()))
    }

    /// Required.
    fn move_to<T: AddEntity + ?Sized>(self, cont: &mut T) -> usize;
}

/// A specific entity identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EntityId {
    key: usize,
    index: usize,
}

impl EntityId {
    pub const fn new(key: usize, index: usize) -> Self {
        Self { key, index }
    }

    pub const fn key(&self) -> EntityKey {
        EntityKey::Index(self.key)
    }

    pub const fn key_inner(&self) -> usize {
        self.key
    }

    pub const fn index(&self) -> usize {
        self.index
    }
}

/// Key for the map [`EntityDict`] in order to get value [`EntityContainer`].
/// `EntityDict` provides some access ways shown below.
/// - Index: Index of the `EntityContainer`.
/// - Component keys: [`ComponentKey`]s of components that forms an entity.
/// - Name: Unique name for the entity.
/// - Type: If the entity is declared statically, it has its own type which is used as a key.
#[derive(Debug, Clone)]
pub enum EntityKey<'a> {
    /// Access by index is recommended in terms of performance.
    Index(usize),

    /// Component's keys that belong to entity can be used as a key to find [`EntityContainer`].
    ComponentKeys(Cow<'a, [ComponentKey]>),

    /// Unique name for the entity.
    Name(Cow<'a, str>),

    /// If the entity is defined statically.
    Type(EntityKeyType),
}

impl<'a> EntityKey<'a> {
    pub fn index(&self) -> usize {
        if let Self::Index(index) = self {
            *index
        } else {
            panic!("{:?} is not Index", self);
        }
    }

    pub fn comp_keys(&self) -> &[ComponentKey] {
        if let Self::ComponentKeys(ckeys) = self {
            ckeys
        } else {
            panic!("{:?} is not ComponentKey", self);
        }
    }

    pub fn name(&self) -> &str {
        if let Self::Name(name) = self {
            name
        } else {
            panic!("{:?} is not Name", self);
        }
    }

    pub fn ty(&self) -> &EntityKeyType {
        if let Self::Type(ty) = self {
            ty
        } else {
            panic!("{:?} is not Type", self);
        }
    }
}

impl<'a> From<usize> for EntityKey<'a> {
    fn from(value: usize) -> Self {
        Self::Index(value)
    }
}

impl<'a> From<&'a [ComponentKey]> for EntityKey<'a> {
    fn from(value: &'a [ComponentKey]) -> Self {
        Self::ComponentKeys(Cow::Borrowed(value))
    }
}

impl<'a> From<&'a str> for EntityKey<'a> {
    fn from(value: &'a str) -> Self {
        Self::Name(Cow::Borrowed(value))
    }
}

impl<'a> From<TypeId> for EntityKey<'a> {
    fn from(value: TypeId) -> Self {
        Self::Type(EntityKeyType::new(value))
    }
}

/// [`TypeId`] for statically defined entity.
#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
#[repr(transparent)]
pub struct EntityKeyType(TypeId);

impl EntityKeyType {
    pub const fn new(ty: TypeId) -> Self {
        Self(ty)
    }

    pub const fn as_type(&self) -> &TypeId {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum EntityKeyKind {
    /// Corresponds to [`EntityKey::Index`].
    Index,
    /// Corresponds to [`EntityKey::ComponentKeys`].
    ComponentKeys,
    /// Corresponds to [`EntityKey::name`].
    Name,
    /// Corresponds to [`EntityKey::Type`].
    Type,
}

impl<'a> From<&EntityKey<'a>> for EntityKeyKind {
    fn from(value: &EntityKey) -> Self {
        match value {
            EntityKey::Index(..) => EntityKeyKind::Index,
            EntityKey::ComponentKeys(..) => EntityKeyKind::ComponentKeys,
            EntityKey::Name(..) => EntityKeyKind::Name,
            EntityKey::Type(..) => EntityKeyKind::Type,
        }
    }
}

/// A piece of information about an entity such as entity index, name, and its components.
#[derive(Debug, Eq)]
pub struct EntityTag {
    /// Corresponds to [`EntityKey::Index`].
    index: usize,

    /// Corresponds to [`EntityKey::Name`].
    name: NonNull<str>,

    /// Corresponds to [`EntityKey::ComponentKeys`].
    comp_keys: NonNull<ComponentKey>,

    /// Corresponds to [`EntityContainer::comp_names`].
    comp_names: NonNull<&'static str>,

    /// Length of [`Self::comp_keys`] slice and [`Self::comp_names`] slice.
    comp_len: usize,
}

impl EntityTag {
    pub fn new(
        index: usize,
        name: &Arc<str>,
        comp_keys: &[ComponentKey],
        comp_names: &[&'static str],
    ) -> Self {
        debug_assert_eq!(comp_keys.len(), comp_names.len());

        // Safety: Infallible.
        let ptr_comp_kyes = unsafe { NonNull::new_unchecked(comp_keys.as_ptr().cast_mut()) };
        let ptr_comp_names = unsafe { NonNull::new_unchecked(comp_names.as_ptr().cast_mut()) };
        let comp_len = comp_keys.len();

        // Safety: Infallible.
        let name = unsafe { NonNull::new_unchecked(Arc::as_ptr(name).cast_mut()) };

        Self {
            index,
            name,
            comp_keys: ptr_comp_kyes,
            comp_names: ptr_comp_names,
            comp_len,
        }
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn name(&self) -> &str {
        // Safety: We got this pointer from `ArcStr`, and it won't be dropped until `EntityContainer` is dropped.
        unsafe { self.name.as_ref() }
    }

    pub fn comp_keys(&self) -> &[ComponentKey] {
        // Safety: We got this pointer from `Vec<TypeId>`, and it won't be dropped until `EntityDict` removes the element.
        // The element is guaranteed to alive while it's been using.
        let ptr = self.comp_keys.as_ptr().cast_const();
        unsafe { slice::from_raw_parts(ptr, self.comp_len) }
    }

    pub fn comp_names(&self) -> &[&'static str] {
        // Safety: We got this pointer from `Vec<&'static str>`, and it won't be dropped until `EntityContainer` is dropped.
        let ptr = self.comp_names.as_ptr().cast_const();
        unsafe { slice::from_raw_parts(ptr, self.comp_len) }
    }
}

impl PartialEq for EntityTag {
    fn eq(&self, other: &Self) -> bool {
        self.index() == other.index()
    }
}

/// Entity dictionary that you can find static information about entities and components
/// such as names, types, and their relationships.
/// Plus the dictionary has component data for each entity in [`EntityContainer`].
/// Each `EntityContainer` has all component data related to the entity.
///
/// You can get or remove entries from their indices or keys.
/// Using indices may be faster than using keys in most cases.
#[derive(Debug)]
pub struct EntityDict<S> {
    /// Data.
    data: GroupMap<Vec<ComponentKey>, EntityContainer, ComponentKey, TypeInfo, S>,

    /// Name -> index of the [`EntityContainer`] in the [`Self::data`].
    /// Each `EntityContainer` has its corresponding index.
    name_to_index: HashMap<Arc<str>, usize, S>,

    /// Type -> index of the [`EntityContainer`] in the [`Self::data`].
    /// This is optional, only statically declared entity has its type.
    type_to_index: HashMap<EntityKeyType, usize, S>,
}

impl<S> EntityDict<S>
where
    S: Default,
{
    pub fn new() -> Self {
        Self {
            data: GroupMap::new(),
            name_to_index: HashMap::default(),
            type_to_index: HashMap::default(),
        }
    }
}

impl<S> EntityDict<S>
where
    S: BuildHasher + Default,
{
    /// Turns `ekey` into another type of it according to `to`.
    pub fn convert_entity_key(&self, ekey: EntityKey, to: EntityKeyKind) -> Option<EntityKey> {
        let index = self.get_entity_index(ekey)?;
        let res = match to {
            EntityKeyKind::Index => EntityKey::Index(index),
            EntityKeyKind::ComponentKeys => {
                // Safety: Infallible.
                EntityKey::ComponentKeys(unsafe {
                    Cow::Borrowed(self.data.get_group_key(index).unwrap_unchecked())
                })
            }
            EntityKeyKind::Name => {
                // Safety: Infallible.
                EntityKey::Name(unsafe {
                    Cow::Borrowed(self.data.get_group(index).unwrap_unchecked().0.name())
                })
            }
            EntityKeyKind::Type => {
                // Safety: Infallible.
                let ty = unsafe { self.data.get_group(index).unwrap_unchecked().0.ty() };
                EntityKey::Type(*ty.as_ref()?)
            }
        };
        Some(res)
    }

    pub fn get_entity_container(&self, ekey: EntityKey) -> Option<&EntityContainer> {
        let index = self.get_entity_index(ekey)?;
        // Safety: Infallible.
        let cont = unsafe { self.data.get_group(index).unwrap_unchecked().0 };
        Some(cont)
    }

    pub fn get_entity_container_mut(&mut self, ekey: EntityKey) -> Option<&mut EntityContainer> {
        let index = self.get_entity_index(ekey)?;
        // Safety: Infallible.
        let cont = unsafe { self.data.get_group_mut(index).unwrap_unchecked().0 };
        Some(cont)
    }

    pub fn get_component_info(&self, index: usize) -> Option<&TypeInfo> {
        let tinfo = &self.data.get_item(index)?.0;
        Some(tinfo)
    }

    /// Registers new entity and its components information and returns entity index.
    /// If you want to change entity information, you must remove if first. See [`Self::unregister_entity`].
    /// Also, this method doesn't overwrite component information.
    ///
    /// # Panics
    ///
    /// - Panics if `desc` doesn't have component information at all.
    /// - Panics if entity name conflicts.
    /// - Panics if the dictionary has had entity information already.
    pub fn register_entity(&mut self, desc: EntityDesc) -> EntityKey<'static> {
        let name = Arc::clone(&desc.cont.name);
        let ent_ty = *desc.cont.ty();
        let index = self.data.add_group(desc);
        assert!(
            !self.name_to_index.contains_key(&name),
            "entity '{}' conflicts",
            name
        );

        // Adds mapping.
        self.name_to_index.insert(name, index);
        if let Some(ty) = ent_ty {
            self.type_to_index.insert(ty, index);
        }

        EntityKey::Index(index)
    }

    /// Unregister entity and tries to unregister corresponding components as well.
    /// But components that are linked to another entity won't be unregistered.
    pub fn unregister_entity(&mut self, ekey: EntityKey) -> Option<EntityContainer> {
        let index = self.get_entity_index(ekey)?;
        let old = self.data.remove_group(index);

        // Removes mapping.
        if let Some(old) = old.as_ref() {
            self.name_to_index.remove(old.name());
            if let Some(ty) = old.ty() {
                self.type_to_index.remove(ty);
            }
        }

        old
    }

    pub fn iter_entity_container(
        &self,
    ) -> impl Iterator<Item = (&Vec<ComponentKey>, usize, &EntityContainer)> {
        self.data.iter_group()
    }

    pub fn borrow(
        &self,
        ekey: EntityKey,
    ) -> Result<Borrowed<NonNull<dyn ContainEntity>, AtomicI32>, BorrowError> {
        if let Some(cont) = self.get_entity_container(ekey) {
            cont.borrow()
        } else {
            Err(BorrowError::OutOfBound)
        }
    }

    pub fn borrow_mut(
        &mut self,
        ekey: EntityKey,
    ) -> Result<Borrowed<NonNull<dyn ContainEntity>, AtomicI32>, BorrowError> {
        if let Some(cont) = self.get_entity_container_mut(ekey) {
            cont.borrow_mut()
        } else {
            Err(BorrowError::OutOfBound)
        }
    }

    fn get_entity_index(&self, ekey: EntityKey) -> Option<usize> {
        match ekey {
            EntityKey::Index(index) => self.data.contains_group(index).then_some(index),
            EntityKey::ComponentKeys(keys) => self.data.get_group_index(&*keys),
            EntityKey::Name(name) => self.name_to_index.get(&*name).cloned(),
            EntityKey::Type(ty) => self.type_to_index.get(&ty).cloned(),
        }
    }
}

impl<S> Default for EntityDict<S>
where
    S: Default,
{
    fn default() -> Self {
        Self::new()
    }
}

pub trait AsEntityDesc {
    fn as_entity_descriptor() -> EntityDesc;
}

/// A registration descriptor of an entity for [`EntityDict`].
pub struct EntityDesc {
    cont: EntityContainer,
    comps: Vec<(ComponentKey, TypeInfo)>,
}

impl EntityDesc {
    /// You can pass your own empty component container `cont`, otherwise [`SparseSet`] is used.
    pub fn new(name: Arc<str>, ty: Option<EntityKeyType>, cont: Box<dyn ContainEntity>) -> Self {
        Self {
            cont: EntityContainer::new(name, ty, cont, Vec::new()),
            comps: Vec::new(),
        }
    }

    pub fn new_with_default_container<S>(name: Arc<str>, ty: Option<EntityKeyType>) -> Self
    where
        S: BuildHasher + Default + Clone + 'static,
    {
        Self::new(name, ty, Box::new(SparseSet::<S>::new()))
    }

    pub fn add_component(&mut self, tinfo: TypeInfo) {
        if !self.cont.cont.contains_column(&tinfo.id) {
            self.cont.cont.add_column(tinfo);
        }
        self.comps.push((ComponentKey::new(tinfo.id), tinfo));
        self.cont.comp_names.push(tinfo.name);
    }
}

impl DescribeGroup<Vec<ComponentKey>, EntityContainer, ComponentKey, TypeInfo> for EntityDesc {
    fn into_group_and_items(
        self,
    ) -> (
        Vec<ComponentKey>,
        EntityContainer,
        Vec<(ComponentKey, TypeInfo)>,
    ) {
        let Self { cont, comps } = self;

        let ckeys = comps.iter().map(|(ty, _)| *ty).collect::<Vec<_>>();

        (ckeys, cont, comps)
    }
}

/// A structure including entity information and its container.
/// The container holds component data without concrete type information.
pub struct EntityContainer {
    /// Unique name of the entity, which will be used as a key to find this container.
    /// See [`EntityKey::Name`].
    name: Arc<str>,

    /// Optional type of the entity.
    /// Statically declared entities have this property.
    ty: Option<EntityKeyType>,

    /// Container that including components for the entity.
    cont: Box<dyn ContainEntity>,

    /// Pointer to the `dyn ContainEntity`.
    cont_ptr: SimpleHolder<NonNull<dyn ContainEntity>, AtomicI32>,

    /// Included component names just for users.
    comp_names: Vec<&'static str>,
}

impl Debug for EntityContainer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EntityContainer")
            .field("name", &self.name)
            .field("ty", &self.ty)
            .field("cont_ptr", &self.cont_ptr)
            .field("comp_names", &self.comp_names)
            .finish_non_exhaustive()
    }
}

impl EntityContainer {
    pub(crate) fn new(
        name: Arc<str>,
        ty: Option<EntityKeyType>,
        mut cont: Box<dyn ContainEntity>,
        comp_names: Vec<&'static str>,
    ) -> Self {
        // Safety: Infallible
        let ptr = unsafe { NonNull::new_unchecked(&mut *cont as *mut _) };

        Self {
            name,
            ty,
            cont,
            cont_ptr: SimpleHolder::new(ptr),
            comp_names,
        }
    }

    pub fn name(&self) -> &Arc<str> {
        &self.name
    }

    pub fn ty(&self) -> &Option<EntityKeyType> {
        &self.ty
    }

    pub fn comp_names(&self) -> &Vec<&'static str> {
        &self.comp_names
    }

    pub fn borrow(&self) -> Result<Borrowed<NonNull<dyn ContainEntity>, AtomicI32>, BorrowError> {
        self.cont_ptr.borrow()
    }

    pub fn borrow_mut(
        &mut self,
    ) -> Result<Borrowed<NonNull<dyn ContainEntity>, AtomicI32>, BorrowError> {
        self.cont_ptr.borrow_mut()
    }
}

impl Deref for EntityContainer {
    type Target = dyn ContainEntity;

    fn deref(&self) -> &Self::Target {
        &*self.cont
    }
}

impl DerefMut for EntityContainer {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.cont
    }
}

pub struct TypedEntityContainer<E> {
    borrowed: Borrowed<NonNull<dyn ContainEntity>, AtomicI32>,
    _marker: PhantomData<E>,
}

impl<E: Entity> TypedEntityContainer<E> {
    /// # Safety
    ///
    /// Undefined behavior if
    /// - pointer is not valid.
    /// - type is incorrect.
    /// The pointer must be valid while this instance lives.
    pub(crate) unsafe fn new(borrowed: Borrowed<NonNull<dyn ContainEntity>, AtomicI32>) -> Self {
        Self {
            borrowed,
            _marker: PhantomData,
        }
    }

    /// Constructs instance by copying `borrowed` in bit level.
    /// Don't forget to not drop `borrowed` because it's copied to this structure.
    ///
    /// # Safety
    ///
    /// Undefined behavior if
    /// - pointer is not valid.
    /// - type is incorrect.
    /// - `borrowed` is dropped after call to this method.
    /// The pointer must be valid while this instance lives.
    pub(crate) unsafe fn new_copy(
        borrowed: &Borrowed<NonNull<dyn ContainEntity>, AtomicI32>,
    ) -> Self {
        let mut uninit: MaybeUninit<Borrowed<NonNull<dyn ContainEntity>, AtomicI32>> =
            MaybeUninit::uninit();
        // Safety: Infallible.
        unsafe { ptr::copy_nonoverlapping(borrowed as *const _, uninit.as_mut_ptr(), 1) };
        Self::new(uninit.assume_init())
    }

    /// Returns the number of items.
    pub fn len(&self) -> usize {
        self.as_ref().len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn get_component_mut<C: Component>(&mut self) -> Option<Borrowed<Getter<C>, AtomicI32>> {
        let ci = self.as_ref().get_column_index(&TypeId::of::<C>())?;
        if let Ok(borrowed) = self.as_ref().borrow_column(ci) {
            // Safety: We got the column index from the type, so the type is correct.
            Some(borrowed.map(|raw_getter| unsafe { Getter::new(raw_getter) }))
        } else {
            None
        }
    }

    pub fn add_entity(&mut self, value: E) {
        value.move_to(self.as_mut());
    }

    /// * `index` - Index in a component array.
    //
    // Index used in `AddEntity::remove_by_inner_index()`.
    pub fn remove_entity(&mut self, index: usize) {
        self.as_mut().remove_row_by_inner_index(index);
    }

    fn as_ref(&self) -> &dyn ContainEntity {
        // Safety: Warning in the constructor.
        unsafe { self.borrowed.as_ref() }
    }

    fn as_mut(&mut self) -> &mut dyn ContainEntity {
        // Safety: Warning in the constructor.
        unsafe { self.borrowed.as_mut() }
    }
}

/// A trait for collecting heterogeneous static types.
/// In this trait, each type is gathered in each column and all columns have the same length like 2d matrix.
///
/// When it comes to in & out types, this trait has intentionally raw pointer parameters not to use generic for object safety.
/// So that you can provide your own data structure.
/// There's built-in implementation [`SparseSet`](super::sparse_set::SparseSet) which is based on [`ChunkAnyVec`](crate::ds::vec::ChunkAnyVec).
pub trait ContainEntity: RegisterComponent + BorrowComponent + AddEntity {
    fn new_from_this(&self) -> Box<dyn ContainEntity>;

    /// Returnes the number of items.
    fn len(&self) -> usize;

    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// * `ci` - Index of the column(component container).
    /// * `ri` - Index of the item.  
    ///   This may be different from index in a column.
    ///   In other words, you can't use the same `ri` as an index for columns that you get from
    ///   [`BorrowComponent::borrow_column`] or [`BorrowComponent::borrow_column_mut`].  
    ///   For example, if an implementation underlying the trait is sparse set,
    ///   `ri` must be an index in sparse layer, and index in a column may be an index in dense layer.
    fn get_item_mut(&mut self, ci: usize, ri: usize) -> Option<NonNull<u8>>;
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
    fn borrow_column(&self, ci: usize) -> Result<Borrowed<RawGetter, AtomicI32>, BorrowError>;

    /// Borrows a column(component container) from the entity container.
    /// You can access an item through its index from 0 to the number of items.
    //
    // Implementation must guarantee that all items are able to referenced by index
    // from 0 to RawGetter::len().
    fn borrow_column_mut(
        &mut self,
        ci: usize,
    ) -> Result<Borrowed<RawGetter, AtomicI32>, BorrowError>;
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
