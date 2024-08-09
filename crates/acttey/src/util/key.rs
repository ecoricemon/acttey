use crate::{common::AppHasher, util::string::ToStr};
use std::{
    any,
    borrow::Cow,
    collections::HashMap,
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
    marker::PhantomData,
    ops::{Deref, DerefMut},
    sync::{Arc, LazyLock, LockResult, OnceLock, RwLock, RwLockReadGuard, RwLockWriteGuard},
};

pub trait EnumObjectKey {
    fn set_to_global() -> Result<(), ()>;
}

pub trait IntoObjectKey {
    /// Required.
    fn into_key(self) -> ObjectKey;

    /// Provided.
    fn into_typed_key<T>(self) -> AnObjectKey<T>
    where
        Self: Sized,
    {
        AnObjectKey::new(Self::into_key(self))
    }
}

impl<T> IntoObjectKey for T
where
    T: Into<u32> + 'static,
{
    fn into_key(self) -> ObjectKey {
        #[cfg(debug_assertions)]
        let errmsg = { &format!("{} is not set to ObjectKeyMap", any::type_name::<T>()) };
        #[cfg(not(debug_assertions))]
        let errmsg = { "something is not set to ObjectKeyMap" };

        // Panics if `self` is not set to the global map yet.
        let id: u32 = self.into();
        ObjectKeyMap::get(&id).expect(errmsg)
    }
}

impl IntoObjectKey for ObjectKey {
    fn into_key(self) -> ObjectKey {
        self
    }
}

/// Globally accessable map of [`ObjectKey`] for sharing object names typed `Arc<str>`.
pub struct ObjectKeyMap;

impl ObjectKeyMap {
    pub fn get(id: &u32) -> Option<ObjectKey> {
        let guard = Self::read().ok()?;
        let name = guard.get(id)?;
        Some(ObjectKey::new(*id, Arc::clone(name)))
    }

    pub fn get_name<'o>(id: &u32) -> Option<&'o str> {
        let guard = Self::read().ok()?;
        let name = guard.get(id)?;
        // Safety: ptr is valid and it's read-only.
        unsafe { Arc::as_ptr(name).as_ref() }
    }

    pub fn get_or_init<F>(id: &u32, f: F) -> ObjectKey
    where
        F: FnOnce() -> Arc<str>,
    {
        let res = Self::get(id);
        if let Some(res) = res {
            res
        } else {
            Self::set(ObjectKey::new(*id, f()));
            // Safety: We just put the `id` in the map.
            unsafe { Self::get(id).unwrap_unchecked() }
        }
    }

    /// Returns old value.
    pub fn set(key: ObjectKey) -> Option<ObjectKey> {
        let mut guard = Self::write().unwrap();
        guard
            .insert(key.id, key.name)
            .map(|name| ObjectKey::new(key.id, name))
    }

    pub fn remove(id: &u32) -> Option<ObjectKey> {
        let mut guard = Self::write().unwrap();
        guard.remove(id).map(|name| ObjectKey::new(*id, name))
    }

    pub fn read() -> LockResult<RwLockReadGuard<'static, ObjectKeyMapType>> {
        Self::get_map().read()
    }

    pub fn write() -> LockResult<RwLockWriteGuard<'static, ObjectKeyMapType>> {
        Self::get_map().write()
    }

    fn get_map() -> &'static RwLock<ObjectKeyMapType> {
        static OBJECT_KEYS: LazyLock<RwLock<HashMap<u32, Arc<str>, AppHasher>>> =
            LazyLock::new(|| RwLock::new(HashMap::default()));

        &OBJECT_KEYS
    }
}

pub type ObjectKeyMapType = HashMap<u32, Arc<str>, AppHasher>;

/// [`ObjectKey`] with a salt type to distinguish [`TypeId`](std::any::TypeId).
/// Consider using this when you need new type for the `ObjectKey`.
#[repr(transparent)]
pub struct AnObjectKey<Salt> {
    inner: ObjectKey,
    _marker: PhantomData<Salt>,
}

impl<Salt> Debug for AnObjectKey<Salt> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AnObjectKey")
            .field("inner", &self.inner)
            .finish()
    }
}

impl<Salt> Display for AnObjectKey<Salt> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner.name())
    }
}

impl<Salt> AnObjectKey<Salt> {
    pub const fn new(inner: ObjectKey) -> Self {
        Self {
            inner,
            _marker: PhantomData,
        }
    }

    pub fn from<T>(inner: T) -> Self
    where
        T: IntoObjectKey,
    {
        Self::new(inner.into_key())
    }

    pub fn into_inner(self) -> ObjectKey {
        self.inner
    }
}

impl<Salt> Deref for AnObjectKey<Salt> {
    type Target = ObjectKey;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<Salt> DerefMut for AnObjectKey<Salt> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<Salt> Clone for AnObjectKey<Salt> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            _marker: PhantomData,
        }
    }
}

impl<Salt> PartialEq for AnObjectKey<Salt> {
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl<Salt> Eq for AnObjectKey<Salt> {}

impl<Salt> Hash for AnObjectKey<Salt> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.hash(state)
    }
}

impl<Salt> IntoObjectKey for AnObjectKey<Salt> {
    fn into_key(self) -> ObjectKey {
        self.into_inner()
    }
}

/// Id for distinguishing objects such as colors, meshes, or graphics pipelines.
#[derive(Debug, Clone)]
pub struct ObjectKey {
    /// Unique ID.
    //
    // TODO: MSB might be used for crate-internal resources in the future.
    id: u32,

    /// Optional name. Use default() if you don't want to hold name.
    name: Arc<str>,
}

impl ObjectKey {
    const DUMMY_ID: u32 = u32::MAX;

    pub const fn new(id: u32, name: Arc<str>) -> Self {
        Self { id, name }
    }

    pub fn dummy() -> Self {
        static DUMMY: OnceLock<ObjectKey> = OnceLock::new();
        DUMMY
            .get_or_init(|| Self::new(Self::DUMMY_ID, "dummy".into()))
            .clone()
    }

    pub const fn is_dummy(&self) -> bool {
        self.id == Self::DUMMY_ID
    }

    pub const fn id(&self) -> u32 {
        self.id
    }

    pub const fn name(&self) -> &Arc<str> {
        &self.name
    }
}

impl PartialEq for ObjectKey {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for ObjectKey {}

impl Hash for ObjectKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl From<&ObjectKey> for ObjectKey {
    fn from(value: &ObjectKey) -> Self {
        value.clone()
    }
}

impl ToStr for ObjectKey {
    fn to_str(&self) -> Cow<str> {
        Cow::Borrowed(&self.name)
    }
}

impl Default for ObjectKey {
    fn default() -> Self {
        Self::dummy()
    }
}
