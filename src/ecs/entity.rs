use super::{borrow_js::JsAtomic, component::ComponentKey, sparse_set::SparseSet};
use crate::ds::{
    borrow::{BorrowError, Borrowed},
    common::TypeInfo,
    get::RawGetter,
    map::{DescribeGroup, GroupMap},
};
use std::{
    any::TypeId,
    borrow::Cow,
    collections::HashMap,
    ops::{Deref, DerefMut},
    ptr::NonNull,
    slice,
    sync::Arc,
};

pub trait Entity {
    fn add_to(&self, cont: &mut EntityContainer) -> usize;
}

#[macro_export]
macro_rules! impl_entity {
    (1) => {const _: () = {
        use $crate::ecs::{
            entity::{Entity, EntityContainer},
            component::Component,
        };
        use std::{any::TypeId, ptr::NonNull};

        // Implements `Entity` for A0.
        impl<A0: Component> Entity for A0 {
            fn add_to(&self, cont: &mut EntityContainer) -> usize {
                debug_assert_eq!(
                    1, cont.get_column_num(),
                    "{} doesn't have 1 column",
                    cont.name()
                );
                debug_assert_eq!(
                    0, cont.get_column_index(&TypeId::of::<A0>()).unwrap(),
                    "{} is not at index 0 in {}",
                    std::any::type_name::<A0>(), cont.name()
                );

                cont.begin_add_item();
                unsafe {
                    let ptr = self as *const _ as *const u8;
                    let ptr = NonNull::new_unchecked(ptr.cast_mut());
                    cont.add_item(0, ptr);
                }
                cont.end_add_item()
            }
        }
    };};
    ($n:expr, $($i:expr),+) => {const _: () = {
        use $crate::ecs::{
            entity::{Entity, EntityContainer},
            component::Component,
        };
        use std::{any::TypeId, ptr::NonNull};
        use paste::paste;

        // Implements `Entity` for (A0, A1, ...).
        paste! {
            impl<$([<A $i>]: Component),+> Entity for ( $([<A $i>]),+ ) {
                fn add_to(&self, cont: &mut EntityContainer) -> usize {
                    debug_assert_eq!(
                        $n, cont.get_column_num(),
                        "{} doesn't have 1 column",
                        cont.name()
                    );

                    cont.begin_add_item();
                    $(
                        debug_assert_eq!(
                            $i, cont.get_column_index(&TypeId::of::<[<A $i>]>()).unwrap(),
                            "{} is not at index {} in {}",
                            std::any::type_name::<[<A $i>]>(), $i, cont.name()
                        );
                        unsafe {
                            let ptr = &self.[<$i>] as *const _ as *const u8;
                            let ptr = NonNull::new_unchecked(ptr.cast_mut());
                            cont.add_item($i, ptr);
                        }
                    )+
                    cont.end_add_item()
                }
            }
        }
    };};
}

impl_entity!(1);
impl_entity!(2, 0, 1);
impl_entity!(3, 0, 1, 2);
impl_entity!(4, 0, 1, 2, 3);
impl_entity!(5, 0, 1, 2, 3, 4);
impl_entity!(6, 0, 1, 2, 3, 4, 5);
impl_entity!(7, 0, 1, 2, 3, 4, 5, 6);
impl_entity!(8, 0, 1, 2, 3, 4, 5, 6, 7);

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
    #[inline]
    fn from(value: usize) -> Self {
        Self::Index(value)
    }
}

impl<'a> From<&'a [ComponentKey]> for EntityKey<'a> {
    #[inline]
    fn from(value: &'a [ComponentKey]) -> Self {
        Self::ComponentKeys(Cow::Borrowed(value))
    }
}

impl<'a> From<&'a str> for EntityKey<'a> {
    #[inline]
    fn from(value: &'a str) -> Self {
        Self::Name(Cow::Borrowed(value))
    }
}

impl<'a> From<TypeId> for EntityKey<'a> {
    #[inline]
    fn from(value: TypeId) -> Self {
        Self::Type(EntityKeyType::new(value))
    }
}

/// [`TypeId`] for statically defined entity.
#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
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

    #[inline]
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
    #[inline]
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
pub struct EntityDict {
    /// Data.
    data: GroupMap<Vec<ComponentKey>, EntityContainer, ComponentKey, TypeInfo>,

    /// Name -> index of the [`EntityContainer`] in the [`Self::map`].
    /// Each `EntityContainer` has its corresponding index.
    name_to_index: HashMap<Arc<str>, usize, ahash::RandomState>,

    /// Type -> index of the [`EntityContainer`] in the [`Self::map`].
    /// This is optional, only statically declared entity has its type.
    type_to_index: HashMap<EntityKeyType, usize, ahash::RandomState>,
}

impl EntityDict {
    pub fn new() -> Self {
        Self {
            data: GroupMap::new(),
            name_to_index: HashMap::default(),
            type_to_index: HashMap::default(),
        }
    }

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
    /// - Panics if `reg` doesn't have component information at all.
    /// - Panics if entity name conflicts.
    /// - Panics if the dictionary has had entity information already.
    #[inline]
    pub fn register_entity(&mut self, reg: EntityForm) -> EntityKey<'static> {
        let name = Arc::clone(&reg.cont.name);
        let ent_ty = *reg.cont.ty();
        let index = self.data.add_group(reg);
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

    #[inline]
    pub fn iter_entity_container(
        &self,
    ) -> impl Iterator<Item = (&Vec<ComponentKey>, usize, &EntityContainer)> {
        self.data.iter_group()
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

impl Default for EntityDict {
    fn default() -> Self {
        Self::new()
    }
}

/// A registration form of an entity for [`EntityDict`].
pub struct EntityForm {
    cont: EntityContainer,
    comps: Vec<(ComponentKey, TypeInfo)>,
}

impl EntityForm {
    /// You can pass your own empty component container `cont`, otherwise [`SparseSet`] is used.
    #[inline]
    pub fn new(name: Arc<str>, ty: Option<EntityKeyType>, cont: Option<Box<dyn Together>>) -> Self {
        Self {
            cont: EntityContainer {
                name,
                ty,
                cont: cont.unwrap_or(Box::new(SparseSet::new())),
                comp_names: Vec::new(),
            },
            comps: Vec::new(),
        }
    }

    pub fn add_component(&mut self, tinfo: TypeInfo) {
        if !self.cont.cont.contains_column(&tinfo.id) {
            self.cont.cont.add_column(tinfo);
        }
        self.comps.push((ComponentKey::new(tinfo.id), tinfo));
        self.cont.comp_names.push(tinfo.name);
    }
}

impl DescribeGroup<Vec<ComponentKey>, EntityContainer, ComponentKey, TypeInfo> for EntityForm {
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
/// That container contains compoent data without concrete type.
#[derive(Debug)]
pub struct EntityContainer {
    /// Unique name of the entity, which can be used as a key to find `EntityContainer` itself.
    /// See [`EntityKey::Name`].
    name: Arc<str>,

    /// Optional type of the entity.
    /// Statically declared entities have this property.
    ty: Option<EntityKeyType>,

    /// Container that including components for the entity.
    cont: Box<dyn Together>,

    /// Included component names for information.
    comp_names: Vec<&'static str>,
}

impl EntityContainer {
    #[inline]
    pub fn name(&self) -> &Arc<str> {
        &self.name
    }

    #[inline]
    pub fn ty(&self) -> &Option<EntityKeyType> {
        &self.ty
    }

    #[inline]
    pub fn cont(&self) -> &dyn Together {
        &*self.cont
    }

    #[inline]
    pub fn comp_names(&self) -> &Vec<&'static str> {
        &self.comp_names
    }
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

/// Functionalities for collecting heterogeneous static types of data.
/// In this trait, each types of data are gathered in each column and all columns have the same length like 2d matrix.
///
/// When it comes to in/out types, this trait has intentionally raw pointer parameters.
/// Although it's quite dangerous, it gives us object safety, so that we can have another heterogeneous trait implementations.  
/// For instance, there's built-in implementation [`SparseSet`](super::sparse_set::SparseSet) which is deeply based on `Vec`.
/// You can implement your own data structure like hash map, then you can keep them in a buffer with `dyn Together` type.
#[allow(clippy::len_without_is_empty)]
pub trait Together: std::fmt::Debug {
    fn add_column(&mut self, tinfo: TypeInfo) -> Option<usize>;
    fn remove_column(&mut self, ci: usize) -> Option<TypeInfo>;
    fn new_from_this(&self) -> Box<dyn Together>;
    fn contains_column(&self, ty: &TypeId) -> bool;
    fn get_column_index(&self, ty: &TypeId) -> Option<usize>;
    fn get_column_info(&self, ci: usize) -> Option<&TypeInfo>;
    fn get_column_num(&self) -> usize;

    fn len(&self) -> usize;

    fn begin_add_item(&mut self);
    /// # Safety
    ///
    /// Undefined behavior if `ptr` is invalid.
    unsafe fn add_item(&mut self, ci: usize, ptr: NonNull<u8>);
    fn end_add_item(&mut self) -> usize;
    fn remove_item(&mut self, ri: usize) -> bool;

    fn get_item(&self, ci: usize, ri: usize) -> Option<NonNull<u8>>;
    fn get_item_mut(&mut self, ci: usize, ri: usize) -> Option<NonNull<u8>>;

    fn borrow_column(&self, ci: usize) -> Result<Borrowed<RawGetter, JsAtomic>, BorrowError>;
    fn borrow_column_mut(
        &mut self,
        ci: usize,
    ) -> Result<Borrowed<RawGetter, JsAtomic>, BorrowError>;
}
