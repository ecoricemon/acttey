use super::{
    component::{Component, ComponentKey},
    entity::{
        ContainEntity, Entity, EntityId, EntityIndex, EntityKey, EntityKeyKind, EntityKeyRef,
        EntityName, EntityTag,
    },
};
use crate::{
    ds::{
        BorrowResult, Borrowed, DescribeGroup, Getter, GetterMut, GroupDesc, GroupMap,
        SimpleHolder, TypeInfo,
    },
    ecs::EcsError,
    util::{macros::debug_format, With},
};
use std::{
    any::TypeId,
    collections::HashMap,
    fmt,
    hash::BuildHasher,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr::NonNull,
    sync::Arc,
};

/// A trait for generating [`EntityReg`].
pub trait AsEntityReg {
    fn entity_descriptor() -> EntityReg;
}

/// A storage where you can find both entity and component data and static
/// information about them such as names, types, and their relationships.
///
/// Each container is basically identified by its component keys. In other
/// words, unique combination of components is the key of an entity container.
/// So you cannot register two entities that has the same components. Entity
/// containers are also identified by their indices they get when they are
/// registered to this storage. It's recommended accessing using entity index
/// instead of component keys if possible because it would be faster.
///
/// Optionally, entity name or type may be able to be used as an identification,
/// but it's not guaranteed because it must be provided by clients. If not so,
/// searching an entity container via entity name or type fails. See
/// [`EntityKeyRef`].
//
// TODO: Write this on ent module doc as well.
// Why entities of the same component combination are not allowed?
// - If it's allowed, something like below is possible.
//   EntA: (CompA, CompB), EntB: (CompA), EntC: (CompA)
// - Imagine clients are removing `CompB` from some items in EntA's container.
//   In that case, they must be moved into `EntB` or `EntC`, but we cannot
//   specify which container they should go.
#[derive(Debug)]
pub(crate) struct EntityStorage<S> {
    /// A map holding entity containers and relationships to components.
    ///
    /// Key: Index or slice of [`ComponentKey`]s
    map: GroupMap<Arc<[ComponentKey]>, EntityContainer, ComponentKey, TypeInfo, S>,

    /// Optional mapping from [`EntityName`] to [`EntityIndex`].
    name_to_index: HashMap<EntityName, EntityIndex, S>,

    /// Generation of each entity container. The generation is when the
    /// container is registered to this storage.
    ent_gens: Vec<u64>,

    /// Generation that will be assigned to the next registered entity
    /// container.
    generation: u64,
}

impl<S> EntityStorage<S>
where
    S: Default,
{
    pub(crate) fn new() -> Self {
        Self {
            map: GroupMap::new(),
            name_to_index: HashMap::default(),
            ent_gens: Vec::new(),
            generation: 1,
        }
    }
}

impl<S> EntityStorage<S>
where
    S: BuildHasher + Default,
{
    /// Turns `ekey` into another type of it according to `to`.
    pub(crate) fn convert_entity_key<'r, K>(&self, key: K, to: EntityKeyKind) -> Option<EntityKey>
    where
        K: Into<EntityKeyRef<'r>>,
    {
        return inner(self, key.into(), to);

        // === Internal helper functions ===

        fn inner<S>(
            this: &EntityStorage<S>,
            key: EntityKeyRef<'_>,
            to: EntityKeyKind,
        ) -> Option<EntityKey>
        where
            S: BuildHasher + Default,
        {
            let ei = this.entity_index(key)?;
            let res = match to {
                EntityKeyKind::Name => {
                    // Safety: Infallible.
                    let (cont, _) = unsafe { this.map.get_group(ei).unwrap_unchecked() };
                    let name = cont.get_tag().get_name()?.clone();
                    EntityKey::Name(name)
                }
                EntityKeyKind::Index => {
                    let ei = EntityIndex::new(With::new(
                        ei,
                        // Safety: Infallible.
                        unsafe { *this.ent_gens.get_unchecked(ei) },
                    ));
                    EntityKey::Index(ei)
                }
                EntityKeyKind::Ckeys => {
                    // Safety: Infallible.
                    let ckeys = unsafe { this.map.get_group_key(ei).unwrap_unchecked() };
                    EntityKey::Ckeys(Arc::clone(ckeys))
                }
            };
            Some(res)
        }
    }

    pub(crate) fn get_component_keys<'r, K>(&self, key: K) -> Option<&Arc<[ComponentKey]>>
    where
        K: Into<EntityKeyRef<'r>>,
    {
        return inner(self, key.into());

        // === Internal helper functions ===

        fn inner<'this, S>(
            this: &'this EntityStorage<S>,
            key: EntityKeyRef<'_>,
        ) -> Option<&'this Arc<[ComponentKey]>>
        where
            S: BuildHasher + Default,
        {
            let ei = this.entity_index(key)?;
            // Safety: Infallible.
            let ckeys = unsafe { this.map.get_group_key(ei).unwrap_unchecked() };
            Some(ckeys)
        }
    }

    pub(crate) fn get_entity_container<'r, K>(&self, key: K) -> Option<&EntityContainer>
    where
        K: Into<EntityKeyRef<'r>>,
    {
        return inner(self, key.into());

        // === Internal helper functions ===

        fn inner<'this, S>(
            this: &'this EntityStorage<S>,
            key: EntityKeyRef<'_>,
        ) -> Option<&'this EntityContainer>
        where
            S: BuildHasher + Default,
        {
            let ei = this.entity_index(key)?;
            // Safety: Infallible.
            let cont = unsafe { this.map.get_group(ei).unwrap_unchecked().0 };
            Some(cont)
        }
    }

    pub(crate) fn get_entity_container_mut<'r, K>(&mut self, key: K) -> Option<&mut EntityContainer>
    where
        K: Into<EntityKeyRef<'r>>,
    {
        return inner(self, key.into());

        // === Internal helper functions ===

        fn inner<'this, S>(
            this: &'this mut EntityStorage<S>,
            key: EntityKeyRef<'_>,
        ) -> Option<&'this mut EntityContainer>
        where
            S: BuildHasher + Default,
        {
            let ei = this.entity_index(key)?;
            // Safety: Infallible.
            let cont = unsafe { this.map.get_group_mut(ei).unwrap_unchecked().0 };
            Some(cont)
        }
    }

    pub(crate) fn iter_entity_container(
        &self,
    ) -> impl Iterator<Item = (&Arc<[ComponentKey]>, EntityIndex, &EntityContainer)> {
        self.map.iter_group().map(|(ckeys, index, cont)| {
            (
                ckeys,
                EntityIndex::new(With::new(index, self.ent_gens[index])),
                cont,
            )
        })
    }

    /// Registers new entity and its components information and returns entity
    /// container index. If you want to change entity information, you must
    /// remove if first. See [`Self::unregister_entity`]. Also, this method
    /// doesn't overwrite component information.
    pub(crate) fn register(&mut self, mut desc: EntityReg) -> Result<EntityIndex, EcsError> {
        if desc.is_empty() {
            let reason = debug_format!(
                "failed to register an entity: `{:?}` has no components",
                desc.get_name()
            );
            return Err(EcsError::InvalidEntity(reason, ()));
        }

        let gkey = desc
            .get_key_info_pairs()
            .iter()
            .map(|(ckey, _)| *ckey)
            .collect::<Vec<_>>();
        let index = self.map.next_index(&*gkey);
        let ei = EntityIndex::new(With::new(index, self.generation));
        desc.set_index(ei);

        let ename = desc.get_name().cloned();
        match self.map.add_group(desc) {
            Ok(i) => {
                debug_assert_eq!(i, index);
                self.generation += 1;

                // Adds mapping.
                if let Some(name) = ename {
                    self.name_to_index.insert(name, ei);
                }

                // Writes current generation on the slot.
                while self.ent_gens.len() <= index {
                    self.ent_gens.push(0);
                }
                self.ent_gens[index] = ei.generation();

                Ok(ei)
            }
            Err(desc) => {
                let (_cont, _) = self.map.get_group2(&desc.group_key).unwrap();
                let reason = debug_format!(
                    "failed to register an entity: two entities `{:?}` and `{:?}` are the same",
                    ename,
                    _cont.get_tag().get_name()
                );
                Err(EcsError::InvalidEntity(reason, ()))
            }
        }
    }

    /// Unregister entity and tries to unregister corresponding components as well.
    /// But components that are linked to another entity won't be unregistered.
    pub(crate) fn unregister<'r, K>(
        &mut self,
        key: K,
    ) -> Option<(Arc<[ComponentKey]>, EntityContainer)>
    where
        K: Into<EntityKeyRef<'r>>,
    {
        return inner(self, key.into());

        // === Internal helper functions ===

        fn inner<S>(
            this: &mut EntityStorage<S>,
            key: EntityKeyRef<'_>,
        ) -> Option<(Arc<[ComponentKey]>, EntityContainer)>
        where
            S: BuildHasher + Default,
        {
            let index = this.entity_index(key)?;
            let ckeys_cont = this.map.remove_group(index);
            debug_assert!(ckeys_cont.is_some());

            // Removes mapping.
            if let Some((_ckeys, cont)) = ckeys_cont.as_ref() {
                if let Some(name) = cont.get_tag().get_name() {
                    this.name_to_index.remove(name);
                }
            }

            // In contrast to `register_entity()`, we don't need to reset generation in the slot
            // because we know that it doesn't exist.

            ckeys_cont
        }
    }

    pub(crate) fn entity_index<'r, K>(&self, key: K) -> Option<usize>
    where
        K: Into<EntityKeyRef<'r>>,
    {
        return inner(self, key.into());

        // === Internal helper functions ===

        fn inner<S>(this: &EntityStorage<S>, key: EntityKeyRef<'_>) -> Option<usize>
        where
            S: BuildHasher + Default,
        {
            match key {
                EntityKeyRef::Ckeys(ckeys) => this.map.get_group_index(ckeys),
                EntityKeyRef::Index(ei) => this.is_valid_index(ei).then_some(ei.index()),
                EntityKeyRef::Name(name) => {
                    let ei = this.name_to_index.get(name)?;
                    this.is_valid_index(ei).then_some(ei.index())
                }
            }
        }
    }

    /// # Safety
    ///
    /// Undefined behavior if exclusive borrow happened before.
    //
    // Allows dead_code for test.
    #[allow(dead_code)]
    pub(crate) unsafe fn get_ptr(&self, ei: &EntityIndex) -> Option<NonNull<dyn ContainEntity>> {
        let ekey = EntityKeyRef::from(ei);
        let cont = self.get_entity_container(ekey)?;
        let ptr = unsafe { cont.cont_ptr.get_unchecked() };
        Some(*ptr)
    }

    fn is_valid_index(&self, ei: &EntityIndex) -> bool {
        self.map.contains_group(ei.index()) && ei.generation() == self.ent_gens[ei.index()]
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

/// A descriptor for registration of an entity storage.
pub struct EntityReg {
    name: Option<EntityName>,
    index: Option<EntityIndex>,
    cont: Box<dyn ContainEntity>,
    key_info_pairs: Vec<(ComponentKey, TypeInfo)>,
}

impl fmt::Debug for EntityReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("EntityReg")
            .field("name", &self.get_name())
            .field("index", &self.get_index())
            .field("key_info_pairs", &self.get_key_info_pairs())
            .finish_non_exhaustive()
    }
}

impl EntityReg {
    pub fn new(name: Option<EntityName>, mut cont: Box<dyn ContainEntity>) -> Self {
        // Removes remaining columns.
        for ci in (0..cont.num_columns()).rev() {
            cont.remove_column(ci);
        }

        Self {
            name,
            index: None,
            cont,
            key_info_pairs: Vec::new(),
        }
    }

    pub(crate) const fn get_name(&self) -> Option<&EntityName> {
        self.name.as_ref()
    }

    pub(crate) const fn get_index(&self) -> Option<&EntityIndex> {
        self.index.as_ref()
    }

    pub(crate) const fn get_key_info_pairs(&self) -> &Vec<(ComponentKey, TypeInfo)> {
        &self.key_info_pairs
    }

    pub fn add_component_of<C: Component>(&mut self) {
        self.add_component(C::type_info());
    }

    pub fn add_component(&mut self, tinfo: TypeInfo) {
        let pair = (ComponentKey::from(&tinfo), tinfo);
        self.key_info_pairs.push(pair);
    }

    fn is_empty(&self) -> bool {
        self.key_info_pairs.is_empty()
    }

    fn set_index(&mut self, index: EntityIndex) {
        self.index = Some(index);
    }

    fn finish(self) -> GroupDesc<Arc<[ComponentKey]>, EntityContainer, ComponentKey, TypeInfo> {
        let Self {
            name,
            index,
            mut cont,
            mut key_info_pairs,
        } = self;
        let index = index.unwrap();

        // Sorts by component key.
        let old_len = key_info_pairs.len();
        key_info_pairs.sort_unstable_by_key(|(key, _)| *key);
        key_info_pairs.dedup_by_key(|(key, _)| *key);
        assert_eq!(
            key_info_pairs.len(),
            old_len,
            "entity cannot have duplicated components"
        );

        // Puts sorted component columns in the container.
        let mut ckeys = Vec::new();
        let mut cnames = Vec::new();
        for (ckey, tinfo) in key_info_pairs.iter() {
            cnames.push(tinfo.name);
            ckeys.push(*ckey);
            cont.add_column(*tinfo);
        }
        let ckeys: Arc<[ComponentKey]> = ckeys.into();
        let cnames: Box<[&'static str]> = cnames.into();

        // Creates `EntityContainer` which is more info-rich.
        let tag = EntityTag::new(index, name, Arc::clone(&ckeys), cnames);
        let tag = Arc::new(tag);
        let cont = EntityContainer::new(tag, cont);

        GroupDesc {
            group_key: ckeys,
            group_value: cont,
            items: key_info_pairs,
        }
    }
}

impl DescribeGroup<Arc<[ComponentKey]>, EntityContainer, ComponentKey, TypeInfo> for EntityReg {
    fn into_group_and_items(
        self,
    ) -> GroupDesc<Arc<[ComponentKey]>, EntityContainer, ComponentKey, TypeInfo> {
        self.finish()
    }
}

/// A wrapper of an entity container.
///
/// The entity container is held as a trait object, and this wrapper provides
/// access to the entity container with some information such as entity name.
pub(crate) struct EntityContainer {
    tag: Arc<EntityTag>,

    /// Container that including components for the entity.
    cont: Box<dyn ContainEntity>,

    /// Pointer to the `dyn ContainEntity`.
    cont_ptr: SimpleHolder<NonNull<dyn ContainEntity>>,
}

impl fmt::Debug for EntityContainer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("EntityContainer")
            .field("tag", &self.get_tag())
            .field("cont_ptr", &self.cont_ptr)
            .finish_non_exhaustive()
    }
}

impl EntityContainer {
    pub(crate) fn new(tag: Arc<EntityTag>, mut cont: Box<dyn ContainEntity>) -> Self {
        // Safety: Infallible
        let cont_ptr = unsafe { NonNull::new_unchecked(&mut *cont as *mut _) };
        let cont_ptr = SimpleHolder::new(cont_ptr);

        Self {
            tag,
            cont,
            cont_ptr,
        }
    }

    pub(crate) fn into_inner(self) -> Box<dyn ContainEntity> {
        self.cont
    }

    pub(crate) const fn get_tag(&self) -> &Arc<EntityTag> {
        &self.tag
    }

    pub(crate) fn borrow(&self) -> BorrowResult<NonNull<dyn ContainEntity>> {
        self.cont_ptr.borrow()
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

pub struct EntityContainerRef<'buf, T> {
    etag: &'buf EntityTag,
    cont: &'buf mut dyn ContainEntity,
    _marker: PhantomData<&'buf mut T>,
}

impl<T> fmt::Debug for EntityContainerRef<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("EntityContainerRef")
            .field("etag", &self.get_entity_tag())
            .finish_non_exhaustive()
    }
}

impl<'buf, T> EntityContainerRef<'buf, T> {
    pub(crate) fn new(etag: &'buf EntityTag, cont: &'buf mut dyn ContainEntity) -> Self {
        Self {
            etag,
            cont,
            _marker: PhantomData,
        }
    }

    pub const fn get_entity_tag(&self) -> &EntityTag {
        self.etag
    }

    /// Returns number of items.
    pub fn len(&self) -> usize {
        self.cont.len()
    }

    /// Returns `true` if the entity container is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns capacity if the entity container supports gauging capacity.
    /// Otherwise, returns number of items, which is equal to
    /// [`EntityContainerRef::len`].
    pub fn capacity(&self) -> usize {
        self.cont.capacity()
    }

    /// Returns number of component columns.
    pub fn num_columns(&self) -> usize {
        self.cont.num_columns()
    }

    /// Returns `true` if all components in the entity container are [`Default`].
    pub fn is_default(&self) -> bool {
        (0..self.num_columns()).all(|ci| {
            // Safety: Infallible.
            let tinfo = unsafe { self.cont.get_column_info(ci).unwrap_unchecked() };
            tinfo.is_default
        })
    }

    /// Returns `true` if all components in the entity container are [`Clone`].
    pub fn is_clone(&self) -> bool {
        (0..self.num_columns()).all(|ci| {
            // Safety: Infallible.
            let tinfo = unsafe { self.cont.get_column_info(ci).unwrap_unchecked() };
            tinfo.is_clone
        })
    }

    /// Reserves extra `additional` capacity if the entity container supports to
    /// do so. Otherwise, nothing takes place.
    pub fn reserve(&mut self, additional: usize) {
        self.cont.reserve(additional);
    }

    pub fn shrink_to_fit(&mut self) {
        self.cont.shrink_to_fit();
    }

    /// Retrieves borrowed getter for a certain component column.
    ///
    /// If the component type doesn't belong to the entity or the column was
    /// borrowed mutably in the past and not returned yet, returns None.
    pub fn get_column_of<C: Component>(&self) -> Option<Borrowed<Getter<'_, C>>> {
        let ci = self.cont.get_column_index(&TypeId::of::<C>())?;
        self.cont.borrow_column(ci).ok().map(|col| {
            // Safety: We got the column index from the type, so the type is correct.
            col.map(|raw_getter| unsafe { Getter::from_raw(raw_getter) })
        })
    }

    /// Retrieves borrowed mutable getter for a certain component column.
    ///
    /// If the component type doesn't belong to the entity or the column was
    /// borrowed in the past and not returned yet, returns None.
    pub fn get_column_mut_of<C: Component>(&mut self) -> Option<Borrowed<GetterMut<'_, C>>> {
        let ci = self.cont.get_column_index(&TypeId::of::<C>())?;
        self.cont.borrow_column_mut(ci).ok().map(|col| {
            // Safety: We got the column index from the type, so the type is correct.
            col.map(|raw_getter| unsafe { GetterMut::from_raw(raw_getter) })
        })
    }

    /// Removes an entity for the given entity id from the entity container.
    pub fn remove(&mut self, eid: &EntityId) {
        if eid.container_index() == self.get_entity_tag().index() {
            if let Some(vi) = self.cont.to_value_index(eid.row_index()) {
                self.remove_by_value_index(vi);
            }
        }
    }

    /// Removes an entity for the given value index from the entity container.
    ///
    /// # Panics
    ///
    /// Panics if the given value index is out of bounds.
    pub fn remove_by_value_index(&mut self, vi: usize) {
        self.cont.remove_row_by_value_index(vi);
    }
}

impl<T> EntityContainerRef<'_, T>
where
    T: Entity,
{
    /// Returns a struct holding shared references to components that belong
    /// to an entity for the given entity id.
    ///
    /// If it failed to find an entity using the given entity id, returns
    /// `None`. See [`EntityContainerRef::get_by_value_index`] for more
    /// details.
    ///
    /// # Examples
    ///
    /// ```
    /// # use my_ecs::prelude::*;
    ///
    /// #[derive(Entity)]
    /// struct Ea {
    ///     ca: Ca,
    /// }
    /// #[derive(Component, Debug)]
    /// struct Ca(i32);
    ///
    /// fn system(ew: EntWrite<Ea>) {
    ///     let mut ew = ew.take_recur();
    ///     let eid = ew.add(Ea { ca: Ca(0) });
    ///     println!("{:?}", ew.get(&eid).unwrap());
    /// }
    /// ```
    pub fn get(&self, eid: &EntityId) -> Option<T::Ref<'_>> {
        if eid.container_index() == self.get_entity_tag().index() {
            let vi = self.cont.to_value_index(eid.row_index())?;
            Some(self.get_by_value_index(vi))
        } else {
            None
        }
    }

    /// Returns a struct holding mutable references to components that belong
    /// to an entity for the given entity id.
    ///
    /// If it failed to find an entity using the given entity id, returns
    /// `None`. See [`EntityContainerRef::get_mut_by_value_index`] for more
    /// details.
    ///
    /// # Examples
    ///
    /// ```
    /// # use my_ecs::prelude::*;
    ///
    /// #[derive(Entity)]
    /// struct Ea {
    ///     ca: Ca,
    /// }
    /// #[derive(Component, Debug)]
    /// struct Ca(i32);
    ///
    /// fn system(ew: EntWrite<Ea>) {
    ///     let mut ew = ew.take_recur();
    ///     let eid = ew.add(Ea { ca: Ca(0) });
    ///     let ea = ew.get_mut(&eid).unwrap();
    ///     *ea.ca = Ca(42);
    ///     println!("{:?}", ew.get(&eid).unwrap());
    /// }
    /// ```
    pub fn get_mut(&mut self, eid: &EntityId) -> Option<T::Mut<'_>> {
        if eid.container_index() == self.get_entity_tag().index() {
            let vi = self.cont.to_value_index(eid.row_index())?;
            Some(self.get_mut_by_value_index(vi))
        } else {
            None
        }
    }

    /// Returns a struct holding shared references to components that belong
    /// to an entity for the given value index.
    ///
    /// Note, however, that entity is not stored as it is. It is split up into
    /// its components then stored in each component container. That means
    /// collecting those references like this function is inefficient because it
    /// requires random access to memory.
    ///
    /// # Panics
    ///
    /// Panics if the given value index is out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// # use my_ecs::prelude::*;
    ///
    /// #[derive(Entity)]
    /// struct Ea {
    ///     ca: Ca,
    /// }
    /// #[derive(Component, Debug)]
    /// struct Ca(i32);
    ///
    /// fn system(ew: EntWrite<Ea>) {
    ///     let ew = ew.take_recur();
    ///     for vi in 0..ew.len() {
    ///         println!("{:?}", ew.get_by_value_index(vi));
    ///     }
    /// }
    /// ```
    pub fn get_by_value_index(&self, vi: usize) -> T::Ref<'_> {
        // Panics here.
        T::get_ref_from(self.cont, vi)
    }

    /// Returns a struct holding mutable references to components that belong
    /// to an entity for the given value index.
    ///
    /// Note, however, that entity is not stored as it is. It is split up into
    /// its components then stored in each component container. That means
    /// collecting those references like this function is inefficient because it
    /// requires random access to memory.
    ///
    /// # Panics
    ///
    /// Panics if the given value index is out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// # use my_ecs::prelude::*;
    ///
    /// #[derive(Entity)]
    /// struct Ea {
    ///     ca: Ca,
    /// }
    /// #[derive(Component, Debug)]
    /// struct Ca(i32);
    ///
    /// fn system(ew: EntWrite<Ea>) {
    ///     let mut ew = ew.take_recur();
    ///     for vi in 0..ew.len() {
    ///         let ea = ew.get_mut_by_value_index(vi);
    ///         *ea.ca = Ca(42);
    ///         println!("{:?}", ea);
    ///     }
    /// }
    /// ```
    pub fn get_mut_by_value_index(&mut self, vi: usize) -> T::Mut<'_> {
        // Panics here.
        T::get_mut_from(self.cont, vi)
    }

    /// Inserts an entity to the entity container then returns entity id of the
    /// inserted entity.
    ///
    /// It's encouraged to use combination of [`EntityContainerRef::resize`]
    /// and [`EntityContainerRef::get_column_mut_of`] when you need to add
    /// lots of entities at once. It's more cache-friendly so it would be faster
    /// than calling to this method many times.
    pub fn add(&mut self, value: T) -> EntityId {
        let ei = self.get_entity_tag().index();
        let ri = value.move_to(self.cont);
        EntityId::new(ei, ri)
    }

    /// Removes an entity for the given entity id from the entity container
    /// then returns the entity.
    pub fn take(&mut self, eid: &EntityId) -> Option<T> {
        if eid.container_index() == self.get_entity_tag().index() {
            let vi = self.cont.to_value_index(eid.row_index())?;
            Some(self.take_by_value_index(vi))
        } else {
            None
        }
    }

    /// Removes an entity for the given value index from the entity container
    /// then returns the entity.
    ///
    /// # Panics
    ///
    /// Panics if the given value index is out of bounds.
    pub fn take_by_value_index(&mut self, vi: usize) -> T {
        T::take_from(self.cont, vi)
    }

    /// Resizes the entity container to the new length by cloning the given
    /// value.
    ///
    /// Resizing occurs column by column.
    ///
    /// # Panics
    ///
    /// Panics if any components of the entity are not [`Clone`].
    pub fn resize(&mut self, new_len: usize, value: T) {
        for ci in 0..T::num_components() {
            // Safety: All columns are resized.
            unsafe {
                self.cont
                    .resize_column(ci, new_len, value.component_ptr(ci));
            }
        }
    }
}
