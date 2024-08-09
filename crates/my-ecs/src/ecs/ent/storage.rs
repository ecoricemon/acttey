use super::{
    component::{Component, ComponentKey},
    entity::{
        ContainEntity, Entity, EntityIndex, EntityKey, EntityKeyKind, EntityName, EntityTypeId,
    },
    sparse_set::SparseSet,
};
use crate::ds::prelude::*;
use std::{
    any::TypeId,
    collections::HashMap,
    fmt,
    hash::BuildHasher,
    marker::PhantomData,
    mem::MaybeUninit,
    ops::{Deref, DerefMut},
    ptr::{self, NonNull},
    sync::atomic::AtomicI32,
};

pub trait AsEntityDesc {
    fn as_entity_descriptor() -> EntityDesc;
}

/// Storage where you can find static entity and component information
/// such as names, types, and their relationships.
/// Plus the dictionary has component data for each entity in [`EntityContainer`].
/// Each `EntityContainer` has all component data related to the entity.
///
/// You can get or remove entries from their indices or keys.
/// Using indices may be faster than using keys in most cases.
#[derive(Debug)]
pub struct EntityStorage<S> {
    /// Data.
    data: GroupMap<Vec<ComponentKey>, EntityContainer, ComponentKey, TypeInfo, S>,

    /// Name -> index of the [`EntityContainer`] in the [`Self::data`].
    /// Each `EntityContainer` has its corresponding index.
    name_to_index: HashMap<EntityName, usize, S>,

    /// Type -> index of the [`EntityContainer`] in the [`Self::data`].
    /// This is optional, only statically declared entity has its type.
    type_to_index: HashMap<EntityTypeId, usize, S>,

    /// Generation of each entity container.
    /// The generation is when the container is registered to the dictionary.
    ent_gens: Vec<u32>,

    /// Current generation.
    gen: u32,
}

impl<S> EntityStorage<S>
where
    S: Default,
{
    pub(crate) fn new() -> Self {
        Self {
            data: GroupMap::new(),
            name_to_index: HashMap::default(),
            type_to_index: HashMap::default(),
            ent_gens: Vec::new(),
            gen: 1,
        }
    }
}

impl<S> EntityStorage<S>
where
    S: BuildHasher + Default,
{
    pub fn contains(&self, ekey: &EntityKey) -> bool {
        self.get_container_index(ekey).is_some()
    }

    /// Turns `ekey` into another type of it according to `to`.
    pub fn convert_entity_key(&self, ekey: &EntityKey, to: EntityKeyKind) -> Option<EntityKey> {
        let index = self.get_container_index(ekey)?;
        let res = match to {
            EntityKeyKind::Name => {
                // Safety: Infallible.
                let name = unsafe { self.data.get_group(index).unwrap_unchecked().0.name() };
                EntityKey::Name(name.clone())
            }
            EntityKeyKind::Index => {
                let ei = EntityIndex::new(GenIndex::new(
                    index as u32,
                    // Safety: Infallible.
                    unsafe { *self.ent_gens.get_unchecked(index) },
                ));
                EntityKey::Index(ei)
            }
            EntityKeyKind::Type => {
                // Safety: Infallible.
                let ty = unsafe { self.data.get_group(index).unwrap_unchecked().0.ty()? };
                EntityKey::Type(*ty)
            }
        };
        Some(res)
    }

    pub fn get_component_keys(&self, ekey: &EntityKey) -> Option<&Vec<ComponentKey>> {
        let index = self.get_container_index(ekey)?;
        // Safety: Infallible.
        let ckeys = unsafe { self.data.get_group_key(index).unwrap_unchecked() };
        Some(ckeys)
    }

    pub fn get_entity_container(&self, ekey: &EntityKey) -> Option<&EntityContainer> {
        let index = self.get_container_index(ekey)?;
        // Safety: Infallible.
        let cont = unsafe { self.data.get_group(index).unwrap_unchecked().0 };
        Some(cont)
    }

    pub fn get_entity_container_mut(&mut self, ekey: &EntityKey) -> Option<&mut EntityContainer> {
        let index = self.get_container_index(ekey)?;
        // Safety: Infallible.
        let cont = unsafe { self.data.get_group_mut(index).unwrap_unchecked().0 };
        Some(cont)
    }

    pub fn get_component_info(&self, index: usize) -> Option<&TypeInfo> {
        let tinfo = &self.data.get_item(index)?.0;
        Some(tinfo)
    }

    pub fn iter_entity_container(
        &self,
    ) -> impl Iterator<Item = (&Vec<ComponentKey>, EntityIndex, &EntityContainer)> {
        self.data.iter_group().map(|(ckeys, index, cont)| {
            (
                ckeys,
                EntityIndex::new(GenIndex::new(index as u32, self.ent_gens[index])),
                cont,
            )
        })
    }

    pub fn borrow(&self, ekey: &EntityKey) -> BorrowResult<NonNull<dyn ContainEntity>, AtomicI32> {
        if let Some(cont) = self.get_entity_container(ekey) {
            cont.borrow()
        } else {
            Err(BorrowError::OutOfBound)
        }
    }

    /// Borrows entity container without checking generation.
    ///
    /// # Safety
    ///
    /// Undefined behavior if the index is out of bound.
    pub unsafe fn borrow_unchecked(
        &self,
        ei: usize,
    ) -> BorrowResult<NonNull<dyn ContainEntity>, AtomicI32> {
        self.data.get_group(ei).unwrap_unchecked().0.borrow()
    }

    pub fn borrow_mut(
        &mut self,
        ekey: &EntityKey,
    ) -> BorrowResult<NonNull<dyn ContainEntity>, AtomicI32> {
        if let Some(cont) = self.get_entity_container_mut(ekey) {
            cont.borrow_mut()
        } else {
            Err(BorrowError::OutOfBound)
        }
    }

    /// Borrows entity container without checking generation.
    ///
    /// # Safety
    ///
    /// Undefined behavior if the index is out of bound.
    pub unsafe fn borrow_unchecked_mut(
        &mut self,
        ei: usize,
    ) -> BorrowResult<NonNull<dyn ContainEntity>, AtomicI32> {
        self.data
            .get_group_mut(ei)
            .unwrap_unchecked()
            .0
            .borrow_mut()
    }

    /// Registers new entity and its components information and returns entity container index.
    /// If you want to change entity information, you must remove if first. See [`Self::unregister_entity`].
    /// Also, this method doesn't overwrite component information.
    ///
    /// # Panics
    ///
    /// - Panics if `desc` doesn't have component information at all.
    /// - Panics if entity name conflicts.
    /// - Panics if the dictionary has had entity information already.
    pub(crate) fn register_entity(&mut self, desc: EntityDesc) -> EntityIndex {
        let name = desc.cont.name.clone();
        let ent_ty = desc.cont.ty().cloned();
        let index = match self.data.add_group(desc) {
            Ok(index) => index,
            Err(_) => {
                panic!("couldn't register entity '{name}', it may conflict or have no components")
            }
        };

        // Adds mapping.
        self.name_to_index.insert(name, index);
        if let Some(ty) = ent_ty {
            self.type_to_index.insert(ty, index);
        }

        // Writes current generation on the slot.
        while self.ent_gens.len() <= index {
            self.ent_gens.push(0);
        }
        self.ent_gens[index] = self.gen;
        let res = EntityIndex::new(GenIndex::new(index as u32, self.gen));
        self.gen += 1;

        res
    }

    /// Unregister entity and tries to unregister corresponding components as well.
    /// But components that are linked to another entity won't be unregistered.
    //
    // for future use.
    #[allow(dead_code)]
    pub(crate) fn unregister_entity(&mut self, ekey: &EntityKey) -> Option<EntityContainer> {
        let index = self.get_container_index(ekey)?;
        let old = self.data.remove_group(index);

        // Removes mapping.
        if let Some(old) = old.as_ref() {
            self.name_to_index.remove(old.name());
            if let Some(ty) = old.ty() {
                self.type_to_index.remove(ty);
            }
        }

        // In contrast to `register_entity()`, we don't need to reset generation in the slot
        // because we know that it doesn't exist.

        old
    }

    fn get_container_index(&self, ekey: &EntityKey) -> Option<usize> {
        match ekey {
            EntityKey::Index(ei) => {
                let index = ei.index();
                (self.data.contains_group(index) && ei.generation() == self.ent_gens[index])
                    .then_some(index)
            }
            EntityKey::Name(name) => self.name_to_index.get(name).cloned(),
            EntityKey::Type(ty) => self.type_to_index.get(ty).cloned(),
        }
    }
}

impl<S> Default for EntityStorage<S>
where
    S: Default,
{
    fn default() -> Self {
        Self::new()
    }
}

/// A registration descriptor of an entity for [`EntityStorage`].
#[derive(Debug)]
pub struct EntityDesc {
    cont: EntityContainer,
    comps: Vec<(ComponentKey, TypeInfo)>,
}

impl EntityDesc {
    /// You can pass your own empty component container `cont`, otherwise [`SparseSet`] is used.
    pub fn new(name: EntityName, ty: Option<EntityTypeId>, cont: Box<dyn ContainEntity>) -> Self {
        Self {
            cont: EntityContainer::new(name, ty, cont, Vec::new()),
            comps: Vec::new(),
        }
    }

    pub fn new_with_default_container<S>(name: EntityName, ty: Option<EntityTypeId>) -> Self
    where
        S: BuildHasher + Default + Clone + 'static,
    {
        Self::new(name, ty, Box::new(SparseSet::<S>::new()))
    }

    pub fn add_component(&mut self, tinfo: TypeInfo) {
        if !self.cont.cont.contains_column(&tinfo.ty) {
            self.cont.cont.add_column(tinfo);
        }
        self.comps.push((ComponentKey::from(&tinfo), tinfo));
        self.cont.comp_names.push(tinfo.name);
    }
}

impl DescribeGroup<Vec<ComponentKey>, EntityContainer, ComponentKey, TypeInfo> for EntityDesc {
    fn into_group_and_items(
        self,
    ) -> GroupDesc<Vec<ComponentKey>, EntityContainer, ComponentKey, TypeInfo> {
        let Self { cont, comps } = self;

        let group_key = comps.iter().map(|(ty, _)| *ty).collect::<Vec<_>>();

        GroupDesc {
            group_key,
            group_value: cont,
            items: comps,
        }
    }
}

/// A structure including entity information and its container.
/// The container holds component data without concrete type information.
pub struct EntityContainer {
    /// Unique name of the entity, which will be used as a key to find this container.
    /// See [`EntityKey::Name`].
    name: EntityName,

    /// Optional type of the entity.
    /// Statically declared entities have this property.
    ty: Option<EntityTypeId>,

    /// Container that including components for the entity.
    cont: Box<dyn ContainEntity>,

    /// Pointer to the `dyn ContainEntity`.
    cont_ptr: SimpleHolder<NonNull<dyn ContainEntity>, AtomicI32>,

    /// Included component names just for users.
    comp_names: Vec<&'static str>,
}

impl fmt::Debug for EntityContainer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
        name: EntityName,
        ty: Option<EntityTypeId>,
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

    pub fn name(&self) -> &EntityName {
        &self.name
    }

    pub fn ty(&self) -> Option<&EntityTypeId> {
        self.ty.as_ref()
    }

    pub fn comp_names(&self) -> &Vec<&'static str> {
        &self.comp_names
    }

    pub fn borrow(&self) -> BorrowResult<NonNull<dyn ContainEntity>, AtomicI32> {
        self.cont_ptr.borrow()
    }

    pub fn borrow_mut(&mut self) -> BorrowResult<NonNull<dyn ContainEntity>, AtomicI32> {
        self.cont_ptr.borrow_mut()
    }

    pub fn borrow_column_of<C: Component>(&self) -> BorrowResult<Getter<C>, AtomicI32> {
        let ci = self
            .get_column_index(&TypeId::of::<C>())
            .ok_or(BorrowError::NotFound)?;
        let borrowed = unsafe {
            // Borrows the target column.
            // Safety: We found the column index.
            let borrowed = self.borrow_column(ci).unwrap_unchecked();

            // Converts `RawGetter` into `Getter<C>`.
            // Safety: `raw_getter` and the type `C` are all valid.
            borrowed.map(|raw_getter| Getter::<C>::new(raw_getter))
        };
        Ok(borrowed)
    }

    pub fn borrow_column_of_mut<C: Component>(&mut self) -> BorrowResult<GetterMut<C>, AtomicI32> {
        let ci = self
            .get_column_index(&TypeId::of::<C>())
            .ok_or(BorrowError::NotFound)?;
        let borrowed = unsafe {
            // Borrows the target column.
            // Safety: We found the column index.
            let borrowed = self.borrow_column_mut(ci).unwrap_unchecked();

            // Converts `RawGetter` into `GetterMut<C>`.
            // Safety: `raw_getter` and the type `C` are all valid.
            borrowed.map(|raw_getter| GetterMut::<C>::new(raw_getter))
        };
        Ok(borrowed)
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

#[derive(Debug)]
#[repr(transparent)]
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
    ///   The pointer must be valid while this instance lives.
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
    ///   The pointer must be valid while this instance lives.
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

    pub fn get_component_mut<C: Component>(&mut self) -> Option<Borrowed<GetterMut<C>, AtomicI32>> {
        let ci = self.as_ref().get_column_index(&TypeId::of::<C>())?;
        if let Ok(borrowed) = self.as_ref().borrow_column(ci) {
            // Safety: We got the column index from the type, so the type is correct.
            Some(borrowed.map(|raw_getter| unsafe { GetterMut::new(raw_getter) }))
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
