use super::{
    borrow_js::JsAtomic,
    traits::{Getter, Together},
};
use crate::{
    ds::{
        borrow::Borrowed,
        common::TypeInfo,
        map::{DescribeGroup, GroupMap},
    },
    ecs::{
        query::{Filter, FilterInfo, QueryIter, QueryIterMut},
        sparse_set::SparseSet,
        system::SystemInfo,
        FilterKey, QueryKey, SystemKey,
    },
    ty,
};
use std::{
    any::TypeId,
    borrow::Cow,
    collections::HashMap,
    fmt::Debug,
    iter,
    ops::{Deref, DerefMut},
};

/// Storage containing all components.  
#[derive(Default)]
pub struct Storage {
    // TODO: Use a synchronized queue to add/remove entities.
    // and make central thread to drain the queue and fill the entities.
    // TODO: Remove pub(crate).
    /// Entity and component information and its containers.
    pub(crate) ents: EntityDict,

    cache: HashMap<FilterKey, Filtered, ahash::RandomState>,

    /// This will be used for scheduling threads in the future.
    sinfos: HashMap<SystemKey, SystemInfo>,
}

impl Storage {
    pub fn new() -> Self {
        Default::default()
    }

    #[inline]
    pub fn register_entity(&mut self, reg: EntityReg) -> usize {
        self.ents.register_entity(reg)
    }

    #[inline]
    pub fn register_system(&mut self, skey: SystemKey, sinfo: SystemInfo) {
        self.sinfos.insert(skey, sinfo);
    }

    #[inline]
    pub fn query<'o, F: Filter>(&mut self, qkey: QueryKey) -> QueryIter<'o, F::Target> {
        QueryIter::new(self._query::<F>(qkey))
    }

    #[inline]
    pub fn query_mut<'o, F: Filter>(&mut self, qkey: QueryKey) -> QueryIterMut<'o, F::Target> {
        QueryIterMut::new(self._query::<F>(qkey))
    }

    fn _query<F: Filter>(&mut self, qkey: QueryKey) -> &Vec<Borrowed<Getter, JsAtomic>> {
        let fkey = FilterKey::new(ty!(F), qkey);
        let Self { cache, ents, .. } = self;
        let filtered = cache
            .entry(fkey)
            .and_modify(|filtered| {
                Self::update_query_result(filtered, ents);
            })
            .or_insert(Self::filter(ents, F::info(qkey)));
        &filtered.query_res
    }

    fn update_query_result(filtered: &mut Filtered, ents: &EntityDict) {
        // We call Vec::set_len() instead of Vec::clear() not to call Borrowed::drop() here.
        // When a system ends its operation, it drops QueryIter or QueryIterMut that it received.
        // The iterator drops the items, which are Borrowed, in this vector manually.
        // Therefore, Borrowed is dropped as soon as it's used.
        // See QueryIter::drop() and QueryIterMut::drop().
        // Safety: Infallible.
        unsafe { filtered.query_res.set_len(0) };

        filtered
            .indices
            .iter()
            .map(|(enti, coli)| {
                let cont = ents.get_entity_container(*enti).unwrap();
                cont.borrow_column(*coli).unwrap()
            })
            .for_each(|borrowed| filtered.query_res.push(borrowed));
    }

    // TODO: Once someone add/remove entity or filter, we need to update it to cache.
    fn filter(ents: &EntityDict, finfo: FilterInfo) -> Filtered {
        let indices = ents
            .iter_entity_container()
            .filter_map(|(_, enti, cont)| {
                (finfo.all.iter().all(|ty| cont.contains_column(ty))
                    && iter::once(&finfo.target)
                        .chain(finfo.any.iter())
                        .any(|ty| cont.contains_column(ty))
                    && !finfo.none.iter().any(|ty| cont.contains_column(ty)))
                .then_some((enti, cont.get_column_index(&finfo.target).unwrap()))
            })
            .collect::<Vec<_>>();

        Filtered {
            indices,
            query_res: Vec::new(),
            finfo,
        }
    }
}

#[derive(Debug)]
pub struct Filtered {
    /// Index pair of entity and component for a filter.
    indices: Vec<(usize, usize)>,

    /// Temporary buffer for the query result.
    /// Content will be replaced for every query, but we can reuse the capacity.
    /// Notice that this doesn't actually own [`Borrowed`] because this is just a temporary buffer.
    /// Real user, system, owns it and will drop it after using it.
    query_res: Vec<Borrowed<Getter, JsAtomic>>,

    /// Filter information.
    finfo: FilterInfo,
}

/// Entity dictionary that you can find static information about entities and components
/// such as names, types, and their relationships.
/// Plus the dictionary has component data for each entity in [`EntityContainer`].
/// Each `EntityContainer` has all component data related to the entity.
///
/// You can get or remove entries from their indices or keys.
/// Using indices may be faster than using keys in most cases.
#[derive(Debug)]
pub struct EntityDict(GroupMap<Vec<TypeId>, EntityContainer, TypeId, TypeInfo>);

impl EntityDict {
    pub fn new() -> Self {
        Self(GroupMap::new())
    }

    #[inline]
    pub fn get_entity_container(&self, index: usize) -> Option<&EntityContainer> {
        self.0.get_group(index).map(|(ent, _)| ent)
    }

    #[inline]
    pub fn get_entity_container2(&self, key: &[TypeId]) -> Option<&EntityContainer> {
        self.0.get_group2(key).map(|(ent, _)| ent)
    }

    #[inline]
    pub fn get_entity_container_mut(&mut self, index: usize) -> Option<&mut EntityContainer> {
        self.0.get_group_mut(index).map(|(ent, _)| ent)
    }

    #[inline]
    pub fn get_entity_key(&self, index: usize) -> Option<&Vec<TypeId>> {
        self.0.get_group_key(index)
    }

    #[inline]
    pub fn get_component_info(&self, index: usize) -> Option<&TypeInfo> {
        self.0.get_item(index).map(|(comp, _)| comp)
    }

    #[inline]
    pub fn get_component_info2(&self, key: &TypeId) -> Option<&TypeInfo> {
        self.0.get_item2(key).map(|(comp, _)| comp)
    }

    /// Registers new entity and its components information and returns entity index.
    /// If you want to change entity information, you must remove if first. See [`Self::unregister_entity`].
    /// Also, this method doesn't overwrite component information.
    ///
    /// # Panics
    ///
    /// - Panics if `form` doesn't have component information at all.
    /// - Panics if the dictionary has had entity information already.
    #[inline]
    pub fn register_entity(&mut self, reg: EntityReg) -> usize {
        self.0.add_group(reg)
    }

    /// Unregister entity and tries to unregister corresponding components as well.
    /// But components that are linked to another entity won't be unregistered.
    #[inline]
    pub fn unregister_entity(&mut self, index: usize) -> Option<EntityContainer> {
        self.0.remove_group(index)
    }

    #[inline]
    pub fn unregister_entity2(&mut self, key: &[TypeId]) -> Option<EntityContainer> {
        self.0.remove_group2(key)
    }

    #[inline]
    pub fn iter_entity_container(
        &self,
    ) -> impl Iterator<Item = (&Vec<TypeId>, usize, &EntityContainer)> {
        self.0.iter_group()
    }
}

impl Default for EntityDict {
    fn default() -> Self {
        Self::new()
    }
}

/// A registration form of an entity for [`EntCompDict`].
pub struct EntityReg {
    ent: EntityContainer,
    comps: Vec<(TypeId, TypeInfo)>,
}

impl EntityReg {
    /// You can pass your own empty component container `cont`, otherwise [`SparseSet`] is used.
    #[inline]
    pub fn new(
        name: impl Into<Cow<'static, str>>,
        ty: Option<TypeId>,
        cont: Option<Box<dyn Together>>,
    ) -> Self {
        fn _new(
            name: Cow<'static, str>,
            ty: Option<TypeId>,
            cont: Option<Box<dyn Together>>,
        ) -> EntityReg {
            EntityReg {
                ent: EntityContainer {
                    name,
                    ty,
                    cont: cont.unwrap_or(Box::new(SparseSet::new())),
                },
                comps: Vec::new(),
            }
        }

        _new(name.into(), ty, cont)
    }

    pub fn add_component(&mut self, tinfo: TypeInfo) {
        if !self.ent.cont.contains_column(&tinfo.id) {
            self.ent.cont.add_column(tinfo);
        }
        self.comps.push((tinfo.id, tinfo));
    }
}

impl DescribeGroup<Vec<TypeId>, EntityContainer, TypeId, TypeInfo> for EntityReg {
    fn into_group_and_items(self) -> (Vec<TypeId>, EntityContainer, Vec<(TypeId, TypeInfo)>) {
        let Self { ent, comps } = self;

        let comp_ids = comps.iter().map(|(ty, _)| *ty).collect::<Vec<_>>();

        (comp_ids, ent, comps)
    }
}

/// A structure including entity information and its container.
/// That container contains compoent data without concrete type.
#[derive(Debug)]
pub struct EntityContainer {
    /// Optional name of the entity.
    name: Cow<'static, str>,

    /// Optional [`TypeId`] of the entity.
    ty: Option<TypeId>,

    /// Container that including components for the entity.
    cont: Box<dyn Together>,
}

impl Deref for EntityContainer {
    type Target = dyn Together;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &*self.cont
    }
}

impl DerefMut for EntityContainer {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.cont
    }
}
