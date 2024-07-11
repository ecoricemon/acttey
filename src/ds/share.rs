use crate::ds::refs::AsRc;
use my_ecs::ds::prelude::*;
use std::{
    any::Any,
    cell::Cell,
    collections::HashMap,
    hash::{BuildHasher, Hash, Hasher},
    marker::{Copy, PhantomData, PhantomPinned},
    ops::{Deref, DerefMut},
    pin::Pin,
    ptr::{self, NonNull},
    rc::Rc,
    thread::LocalKey,
};

pub type SharedVecOrigin<T, S> = Pin<Box<UnsafeShared<OptVec<T, S>>>>;

#[derive(Debug)]
pub struct SharedVec<T, S> {
    /// Pointer to the [`OptVec`].
    /// This pointer will never change unless modification to pointer inside [`SharedVecOrigin`] occurs.
    ptr: UnsafeSharedPtr<OptVec<T, S>>,
}

macro_rules! impl_optvec_method {
    (
        $( [$unsafe:ident] )?
        $id:ident,
        in=( $($in_param:ident: $in_ty:ty),* ),
        out=( $($out_ty:ty)? )
    ) => {
        #[doc = "See [`OptVec::"]
        #[doc = stringify!($id)]
        #[doc = "`]"]
        #[allow(clippy::missing_safety_doc)]
        pub $( $unsafe )? fn $id(
            &mut self,
            $($in_param: $in_ty),*
        ) $( -> $out_ty )? {
            // Safety: Method is unable to move the vec.
            let vec = unsafe { self.ptr.as_mut() };
            vec.$id( $($in_param),* )
        }
    };
}

impl<T, S> SharedVec<T, S>
where
    S: Default,
{
    pub fn new() -> (Self, SharedVecOrigin<T, S>) {
        let mut vec = UnsafeShared::new(OptVec::new());
        let ptr = unsafe { vec.as_ptr() };
        (Self { ptr }, vec)
    }
}

impl<T, S> SharedVec<T, S> {
    pub fn as_ptr(&self) -> NonNull<UnsafeShared<OptVec<T, S>>> {
        self.ptr.as_ptr()
    }
}

impl<T, S> SharedVec<T, S>
where
    S: BuildHasher,
{
    // Implements OptVec's mutable methods.
    impl_optvec_method!(as_slice_mut, in=(), out=(&mut [Option<T>]));
    impl_optvec_method!([unsafe] get_unchecked_mut, in=(index: usize), out=(&mut T));
    impl_optvec_method!(set, in=(index: usize, value: Option<T>), out=(Option<T>));
    impl_optvec_method!(add, in=(value: T), out=(usize));
    impl_optvec_method!(push, in=(value: Option<T>), out=());
    impl_optvec_method!(iter_occupied_mut, in=(), out=(impl Iterator<Item = (usize, &mut T)>));
    impl_optvec_method!(values_occupied_mut, in=(), out=(impl Iterator<Item = &mut T>));
    // We implemented remove() instead of take().
    impl_optvec_method!(swap_occupied, in=(a: usize, b: usize), out=());
    impl_optvec_method!(truncate, in=(len: usize), out=());
    impl_optvec_method!(shrink_to_fit, in=(), out=());
    impl_optvec_method!(extend_set, in=(index: usize, value: T), out=(Option<T>));
}

impl<T, S> SharedVec<T, S>
where
    T: AsRc,
    S: BuildHasher,
{
    pub fn strong_count(&self, index: usize) -> Option<usize> {
        // Safety: We don't move it.
        let vec = unsafe { self.ptr.as_ref() };
        let value = vec.get(index)?;
        Some(value.strong_count())
    }

    /// Removes item and returns it if and only if it's currently not referenced.
    /// You can check it out by calling to [`Self::strong_count`] in advance.
    /// Or you'll be given Err(item).
    pub fn remove(&mut self, item: SharedVecItem<T, S>) -> Result<T, SharedVecItem<T, S>> {
        let SharedVecItem { vec, index, _ref } = item;
        drop(_ref);
        self.remove_by_index(index)
            .map_err(|_| SharedVecItem::new(vec, index))
    }

    /// Removes item and returns it if and only if it's currently not referenced.
    /// You can check it out by calling to [`Self::strong_count`] in advance.
    /// Or you'll be given Err(count).
    ///
    /// # Panics
    ///
    /// Panics if
    /// - index is out of bound.
    /// - item is None.
    pub fn remove_by_index(&mut self, index: usize) -> Result<T, usize> {
        // Safety: We don't move it.
        let vec = unsafe { self.ptr.as_mut() };
        let old = vec.take(index).unwrap();

        let strong_count = old.strong_count();
        if strong_count == 1 {
            Ok(old)
        } else {
            Err(strong_count)
        }
    }
}

impl<T, S> Clone for SharedVec<T, S> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T, S> Copy for SharedVec<T, S> {}

// `SharedVec` doesn't implement `DerefMut` not to allow move.
impl<T, S> Deref for SharedVec<T, S> {
    type Target = OptVec<T, S>;

    fn deref(&self) -> &Self::Target {
        // Safety: Unable to move.
        unsafe { self.ptr.as_ref() }
    }
}

/// [`SharedVecItem`] with debug function for undesirable mutable access.
pub type DebugVecItem<T, S> = DebugLock<SharedVecItem<T, S>, T>;

#[derive(Debug)]
pub struct SharedVecItem<T, S> {
    /// Pointer to vector.
    vec: SharedVec<T, S>,

    /// Index to the vector.
    index: usize,

    /// Shared reference counter to the item.
    /// Due to this counter, this structure has ownership for the item.
    /// So that [`SharedVec`] can't remove it.
    /// Check [`SharedVec::remove`] out for more details.
    //
    // Who has the counter
    // - Actual item in the vector has the counter due to T: AsRc.
    // - This structure has the counter.
    _ref: Rc<()>,
}

impl<T, S> SharedVecItem<T, S> {
    pub fn index(&self) -> usize {
        self.index
    }

    pub fn get_reference_counter(&self) -> &Rc<()> {
        &self._ref
    }

    pub fn strong_count(&self) -> usize {
        Rc::strong_count(&self._ref)
    }
}

impl<T, S> SharedVecItem<T, S>
where
    T: AsRc,
    S: BuildHasher,
{
    /// Creates vector item accessor from the vector and index.
    ///
    /// # Panics
    ///
    /// Panics if index is not valid.
    //
    // Users can't make item directly due to its validity.
    pub(crate) fn new(vec: SharedVec<T, S>, index: usize) -> Self {
        let item = vec.get(index).unwrap();
        let _ref = Rc::clone(item.get_reference_counter());
        debug_assert!(Rc::strong_count(&_ref) > 1); // At least 2(Original's + _ref's).

        Self { vec, index, _ref }
    }

    /// Drops itself and tries to remove original item as well.
    /// If it failed, then returns Err with current strong count of the item.
    pub fn destroy(self) -> Result<T, usize> {
        let mut vec = self.vec;
        vec.remove(self).map_err(|item| {
            let count = item.strong_count();
            drop(item);
            count - 1
        })
    }

    /// Determines whether both are same address or not.
    pub fn addr_eq(&self, other: &Self) -> bool {
        self.vec.as_ptr() == other.vec.as_ptr() && self.index == other.index
    }
}

impl<T, S> Clone for SharedVecItem<T, S> {
    fn clone(&self) -> Self {
        Self {
            vec: self.vec,
            index: self.index,
            _ref: Rc::clone(&self._ref),
        }
    }
}

impl<T, S> AsRef<T> for SharedVecItem<T, S>
where
    S: BuildHasher,
{
    fn as_ref(&self) -> &T {
        // Safety: `SharedVec` can't remove item at index until `SharedVecItem` holds the reference.
        unsafe { self.vec.get_unchecked(self.index) }
    }
}

impl<T, S> AsMut<T> for SharedVecItem<T, S>
where
    S: BuildHasher,
{
    fn as_mut(&mut self) -> &mut T {
        // Safety: `SharedVec` can't remove item at index until `SharedVecItem` holds the reference.
        unsafe { self.vec.get_unchecked_mut(self.index) }
    }
}

impl<T, S> PartialEq for SharedVecItem<T, S>
where
    T: PartialEq,
    S: BuildHasher,
{
    fn eq(&self, other: &Self) -> bool {
        // Compares values.
        self.as_ref() == other.as_ref()
    }
}

impl<T, S> Eq for SharedVecItem<T, S>
where
    T: Eq,
    S: BuildHasher,
{
}

impl<T, S> Hash for SharedVecItem<T, S>
where
    T: Hash,
    S: BuildHasher,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state);
    }
}

pub type SharedMapOrigin<K, V, S> = Pin<Box<UnsafeShared<HashMap<K, V, S>>>>;

#[derive(Debug)]
pub struct SharedMap<K, V, S> {
    /// Pointer to the [`HashMap`].
    /// This pointer will never change unless modification to pointer inside [`SharedMapOrigin`] occurs.
    ptr: UnsafeSharedPtr<HashMap<K, V, S>>,
}

macro_rules! impl_hashmap_method {
    (
        $( [$unsafe:ident] )?
        $id:ident,
        $(gen=$gen:ident,)?
        in=( $($in_param:ident: $in_ty:ty),* ),
        out=( $($out_ty:ty)? )
    ) => {
        #[doc = "See [`HashMap::"]
        #[doc = stringify!($id)]
        #[doc = "`]"]
        #[allow(clippy::missing_safety_doc)]
        pub $( $unsafe )? fn $id $( <$gen> )? (
            &mut self,
            $($in_param: $in_ty),*
        ) $( -> $out_ty )?
        $(
        where
            K: std::borrow::Borrow<$gen>,
            $gen: Hash + Eq + ?Sized,
        )?
        {
            // Safety: Method is unable to move the map.
            let map = unsafe { self.ptr.as_mut() };
            map.$id( $($in_param),* )
        }
    };
}

impl<K, V, S> SharedMap<K, V, S>
where
    K: Hash + Eq,
    S: Default,
{
    pub fn new() -> (Self, SharedMapOrigin<K, V, S>) {
        let mut map = UnsafeShared::new(HashMap::default());
        let ptr = unsafe { map.as_ptr() };
        (Self { ptr }, map)
    }
}

impl<K, V, S> SharedMap<K, V, S>
where
    K: Hash + Eq,
    S: BuildHasher,
{
    // Implements HashMap's mutable methods.
    impl_hashmap_method!(get_mut, gen=Q, in=(k: &Q), out=(Option<&mut V>));
}

impl<K, V, S> SharedMap<K, V, S>
where
    K: Hash + Eq,
    V: AsRc,
    S: BuildHasher,
{
    pub fn strong_count<Q>(&self, key: &Q) -> Option<usize>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        // Safety: We don't move it.
        let map = unsafe { self.ptr.as_ref() };
        let value = map.get(key)?;
        Some(value.strong_count())
    }

    /// Removes item and returns it if and only if it's currently not referenced.
    /// You can check it out by calling to [`Self::strong_count`] in advance.
    /// Or you'll be given Err(item).
    pub fn remove(&mut self, item: SharedMapItem<K, V, S>) -> Result<V, SharedMapItem<K, V, S>> {
        let SharedMapItem { map, key, _ref } = item;
        drop(_ref);
        self.remove_by_key(&key)
            .map(
                |value| value.unwrap(), /* item must exist because users can't make it */
            )
            .map_err(|_| SharedMapItem::new(map, key))
    }

    /// Removes item and returns it if and only if it's currently not referenced.
    /// You can check it out by calling to [`Self::strong_count`] in advance.
    /// Or you'll be given Err(count).
    pub fn remove_by_key<Q>(&mut self, key: &Q) -> Result<Option<V>, usize>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        // Safety: We don't move it.
        let map = unsafe { self.ptr.as_mut() };
        let old = map.remove(key);
        if let Some(old) = old.as_ref() {
            let strong_count = old.strong_count();
            if strong_count != 1 {
                return Err(strong_count);
            }
        }
        Ok(old)
    }
}

impl<K, V, S> Clone for SharedMap<K, V, S> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<K, V, S> Copy for SharedMap<K, V, S> {}

// `SharedMap` doesn't implement `DerefMut` not to allow move.
impl<K, V, S> Deref for SharedMap<K, V, S> {
    type Target = HashMap<K, V, S>;

    fn deref(&self) -> &Self::Target {
        // Safety: Unable to move.
        unsafe { self.ptr.as_ref() }
    }
}

#[derive(Debug)]
pub struct SharedMapItem<K, V, S> {
    /// Pointer to map.
    map: SharedMap<K, V, S>,

    /// Key to the map.
    key: K,

    /// Shared reference counter to the item.
    /// Due to this counter, this structure has ownership for the item.
    /// So that [`SharedMap`] can't remove it.
    /// Check [`SharedMap::remove`] out for more details.
    //
    // Who has the counter
    // - Actual item in the map has the counter due to V: AsRc.
    // - This structure has the counter.
    _ref: Rc<()>,
}

impl<K, V, S> SharedMapItem<K, V, S> {
    pub fn pointer(&self) -> SharedMap<K, V, S> {
        self.map
    }

    pub fn get_reference_counter(&self) -> &Rc<()> {
        &self._ref
    }

    pub fn strong_count(&self) -> usize {
        Rc::strong_count(&self._ref)
    }
}

impl<K, V, S> SharedMapItem<K, V, S>
where
    K: Hash + Eq,
    V: AsRc,
    S: BuildHasher,
{
    /// Creates map item accessor from the map and key.
    ///
    /// # Panics
    ///
    /// Panics if there's no value corresponding to the key.
    //
    // Users can't make item directly due to its validity.
    pub(crate) fn new(map: SharedMap<K, V, S>, key: K) -> Self {
        let item = map.get(&key).unwrap();
        let _ref = Rc::clone(item.get_reference_counter());
        debug_assert!(Rc::strong_count(&_ref) > 1); // At least 2(Original's + _ref's).

        Self { map, key, _ref }
    }
}

impl<K, V, S> AsRef<V> for SharedMapItem<K, V, S>
where
    K: Hash + Eq,
    S: BuildHasher,
{
    fn as_ref(&self) -> &V {
        self.map.get(&self.key).unwrap()
    }
}

impl<K, V, S> AsMut<V> for SharedMapItem<K, V, S>
where
    K: Hash + Eq,
    S: BuildHasher,
{
    fn as_mut(&mut self) -> &mut V {
        self.map.get_mut(&self.key).unwrap()
    }
}

#[derive(Debug)]
pub struct SharedAnyMap<K, S>(HashMap<K, Rc<dyn Any>, S>);

impl<K, S> SharedAnyMap<K, S>
where
    K: Hash + Eq,
    S: Default,
{
    pub fn new() -> Self {
        Self(HashMap::default())
    }
}

impl<K, S> SharedAnyMap<K, S>
where
    K: Hash + Eq,
    S: BuildHasher,
{
    pub fn insert<V: 'static>(&mut self, key: K, value: V) -> Rc<V> {
        let value = Rc::new(value);
        let cloned = Rc::clone(&value);

        // Inserts the value but prohibits removal by insertion.
        let old = self.0.insert(key, value);
        assert!(
            old.is_none(),
            "SharedAnyMap doesn't allow removal by insertion"
        );

        cloned
    }

    pub fn get_item<Q, V: 'static>(&self, key: &Q) -> Option<Rc<V>>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.0.get(key).map(|value| {
            let cloned = Rc::clone(value);
            cloned.downcast().unwrap()
        })
    }

    pub fn strong_count<Q>(&self, key: &Q) -> Option<usize>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.0.get(key).map(Rc::strong_count)
    }

    /// Removes item and returns it if and only if it's currently not referenced.
    /// You can check it out by calling to [`Self::strong_count`] in advance.
    /// Or you'll be given Err(count).
    pub fn remove<Q>(&mut self, key: &Q) -> Result<Option<Rc<dyn Any>>, usize>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        if let Some(strong_count) = self.strong_count(key) {
            if strong_count != 1 {
                return Err(strong_count);
            }
        }
        Ok(self.0.remove(key))
    }
}

#[derive(Debug)]
pub struct UnsafeSharedAnyMap<K, S>(HashMap<K, (Box<dyn Any>, Rc<()>), S>);

impl<K, S> UnsafeSharedAnyMap<K, S>
where
    K: Hash + Eq,
    S: Default,
{
    pub fn new() -> Self {
        Self(HashMap::default())
    }
}

impl<K, S> UnsafeSharedAnyMap<K, S>
where
    K: Hash + Eq,
    S: BuildHasher,
{
    pub fn insert<V: 'static>(&mut self, key: K, value: V) -> UnsafeSharedItem<V> {
        let mut value = Box::new(value);
        let _ref = Rc::new(());
        let item = UnsafeSharedItem::from_boxed(&mut value, Rc::clone(&_ref));

        // Inserts the value but prohibits removal by insertion.
        let old = self.0.insert(key, (value, _ref));
        assert!(
            old.is_none(),
            "UnsafeSharedAnyMap doesn't allow removal by insertion"
        );

        item
    }

    pub fn get_item<Q, V: 'static>(&mut self, key: &Q) -> Option<UnsafeSharedItem<V>>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.0.get_mut(key).map(|(value, _ref)| {
            let value: &mut V = value.downcast_mut().unwrap();
            let ptr = value as *mut _;
            // Infallible.
            let ptr = unsafe { NonNull::new_unchecked(ptr) };
            UnsafeSharedItem::new(ptr, Rc::clone(_ref))
        })
    }

    pub fn strong_count<Q>(&mut self, key: &Q) -> Option<usize>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.0.get(key).map(|(_, _ref)| Rc::strong_count(_ref))
    }

    /// Removes item and returns it if and only if it's currently not referenced.
    /// You can check it out by calling to [`Self::strong_count`] in advance.
    ///
    /// # Panics
    ///
    /// Panics if reference count is greater than 1 (debug mode only).
    pub fn remove<Q>(&mut self, key: &Q) -> Result<Option<Box<dyn Any>>, usize>
    where
        K: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        if let Some(strong_count) = self.strong_count(key) {
            if strong_count != 1 {
                return Err(strong_count);
            }
        }
        Ok(self.0.remove(key).map(|(value, _)| value))
    }
}

impl<K, S> Deref for UnsafeSharedAnyMap<K, S>
where
    K: Hash + Eq,
{
    type Target = HashMap<K, (Box<dyn Any>, Rc<()>), S>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug)]
pub struct UnsafeShared<T> {
    value: T,
    _marker: PhantomPinned,
}

impl<T> UnsafeShared<T> {
    pub fn new(value: T) -> Pin<Box<Self>> {
        let inst = Self {
            value,
            _marker: PhantomPinned,
        };
        Box::pin(inst)
    }

    pub unsafe fn as_ptr(self: &mut Pin<Box<Self>>) -> UnsafeSharedPtr<T> {
        let ptr = self.as_mut().get_unchecked_mut() as *mut _;
        UnsafeSharedPtr(NonNull::new_unchecked(ptr))
    }
}

impl<T> Deref for UnsafeShared<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for UnsafeShared<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

#[derive(Debug)]
pub struct UnsafeSharedPtr<T>(NonNull<UnsafeShared<T>>);

impl<T> UnsafeSharedPtr<T> {
    pub const fn new(ptr: NonNull<UnsafeShared<T>>) -> Self {
        Self(ptr)
    }

    pub fn as_ptr(&self) -> NonNull<UnsafeShared<T>> {
        self.0
    }

    pub unsafe fn as_ref<'a>(&self) -> &'a T {
        self.0.as_ref()
    }

    pub unsafe fn as_mut<'a>(&mut self) -> &'a mut T {
        self.0.as_mut()
    }
}

impl<T> Clone for UnsafeSharedPtr<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for UnsafeSharedPtr<T> {}

/// Owned pointer.
#[derive(Debug)]
pub struct UnsafeSharedItem<T> {
    ptr: NonNull<T>,
    _ref: Rc<()>,
}

impl<T> UnsafeSharedItem<T> {
    // Users can't make item directly for its validity.
    pub(crate) const fn new(ptr: NonNull<T>, _ref: Rc<()>) -> Self {
        Self { ptr, _ref }
    }

    // Users can't make item directly for its validity.
    pub(crate) fn from_boxed(value: &mut Box<T>, _ref: Rc<()>) -> Self {
        let ptr = value.as_mut() as *mut _;
        // Safety: Infallible.
        let ptr = unsafe { NonNull::new_unchecked(ptr) };
        Self::new(ptr, _ref)
    }

    pub unsafe fn as_ref(&self) -> &T {
        self.ptr.as_ref()
    }

    pub unsafe fn as_mut(&mut self) -> &mut T {
        self.ptr.as_mut()
    }

    pub fn pointer(&self) -> NonNull<T> {
        self.ptr
    }

    pub fn get_reference_counter(&self) -> &Rc<()> {
        &self._ref
    }

    pub fn strong_count(&self) -> usize {
        Rc::strong_count(&self._ref)
    }
}

impl<T> Clone for UnsafeSharedItem<T> {
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr,
            _ref: Rc::clone(&self._ref),
        }
    }
}

/// A structure helping you block some modifications on shared variables in a specific timing.
/// This structure recieves thread local counter for checking current borrow and freezing status.
/// When you try to borrow the value mutably, if the counter is in freezing status, it panics.
/// But it only works in debug mode not to add unnecessary operations in release mode.
//
// For now, this structure is desined for types that implement AsRef and AsMut.
#[derive(Debug)]
pub struct DebugLock<T, IT> {
    value: T,

    /// Globally shared mutable borrow counter in single thread scenarios.
    /// This counter has three states.
    /// - Positive number: Multiple mutable borrows have occurred on variables sharing this counter.
    /// - Zero: Currently no one borrowed varaibles sharing this counter.
    /// - Negative number: Freezing status, so any mutable borrow is not allowed.
    ///
    /// Note that this counter may be shared across multiple values.
    /// So that the counter may be increased or decreased by multiple values.
    #[cfg(debug_assertions)]
    debug_cnt: &'static LocalKey<Cell<i32>>,

    _marker: PhantomData<IT>,
}

impl<T: AsMut<IT>, IT> DebugLock<T, IT> {
    pub const fn new(
        value: T,
        #[cfg(debug_assertions)] debug_cnt: &'static LocalKey<Cell<i32>>,
    ) -> Self {
        Self {
            value,
            #[cfg(debug_assertions)]
            debug_cnt,
            _marker: PhantomData,
        }
    }

    pub fn borrow_mut(&mut self) -> UnlockedValue<'_, IT> {
        UnlockedValue::new(
            self.value.as_mut(),
            #[cfg(debug_assertions)]
            self.debug_cnt,
        )
    }

    pub fn into_inner(self) -> T {
        self.value
    }
}

impl<T, IT> Deref for DebugLock<T, IT> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T: Clone, IT> Clone for DebugLock<T, IT> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
            #[cfg(debug_assertions)]
            debug_cnt: self.debug_cnt,
            _marker: PhantomData,
        }
    }
}

impl<T: PartialEq, IT> PartialEq for DebugLock<T, IT> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<T: Eq, IT> Eq for DebugLock<T, IT> {}

impl<T: Hash, IT> Hash for DebugLock<T, IT> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.hash(state)
    }
}

#[derive(Debug)]
pub struct UnlockedValue<'a, T> {
    borrowed: &'a mut T,

    #[cfg(debug_assertions)]
    debug_cnt: &'static LocalKey<Cell<i32>>,
}

impl<'a, T> UnlockedValue<'a, T> {
    pub fn new(
        borrowed: &'a mut T,
        #[cfg(debug_assertions)] debug_cnt: &'static LocalKey<Cell<i32>>,
    ) -> Self {
        #[cfg(debug_assertions)]
        {
            debug_cnt.with(|cnt| {
                let cur_cnt = cnt.get();
                assert!(cur_cnt >= 0);
                cnt.set(cur_cnt + 1);
            });
        }

        Self {
            borrowed,
            #[cfg(debug_assertions)]
            debug_cnt,
        }
    }
}

impl<'a, T> Drop for UnlockedValue<'a, T> {
    fn drop(&mut self) {
        #[cfg(debug_assertions)]
        {
            self.debug_cnt.with(|cnt| {
                cnt.set(cnt.get() - 1);
            })
        }
    }
}

impl<'a, T> Deref for UnlockedValue<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.borrowed
    }
}

impl<'a, T> DerefMut for UnlockedValue<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.borrowed
    }
}

#[derive(Debug)]
pub struct DebugLockKey(&'static LocalKey<Cell<i32>>);

impl DebugLockKey {
    #[must_use]
    pub fn lock(key: &'static LocalKey<Cell<i32>>) -> Self {
        assert_eq!(0, key.get());
        key.set(-1);
        Self(key)
    }
}

impl Drop for DebugLockKey {
    fn drop(&mut self) {
        debug_assert_eq!(-1, self.0.get());
        self.0.set(0);
    }
}
