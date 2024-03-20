use super::borrow_js::JsAtomic;
use crate::{
    ds::{
        borrow::{BorrowError, Borrowed},
        common::TypeInfo,
    },
    ecs::storage::Storage,
    ty,
};
use erased_generic_trait::erase_generic;
use std::{
    any::{Any, TypeId},
    fmt::Debug,
    marker::PhantomData,
    ptr::NonNull,
};

/// Granular data such as position, speed, etc.
pub trait Component: 'static {}

/// Unique data over entire app such as event manager.
pub trait Resource: 'static {}

/// Entity must have distinct components in it.
/// If you need the same types of data, then define different `Component`s using the types.
pub trait Entity: 'static {
    /// DO NOT call me!
    /// This is an internal validation function.
    /// TODO: Validate component identity as well. use uuid?
    /// TODO: Please fix me with better way.
    fn validate();

    /// Notifies types to the collector.
    /// This implementation calls the collector's functions to hand types over such as inner field's types.
    /// And can't recive trait object based on `Collect` because types must be known beforehand becoming a trait object (See `erased-generic-trait`)
    fn notify_types<T>(collector: &mut T, storage: &mut Storage)
    where
        T: CollectGeneric + Downcast;

    // fn notify_types(storage: &mut Storage, col: Option<Box<dyn Collect>>);

    /// Moves itself out into the collector.
    fn moves(self, collector: &mut Box<dyn Collect>, key: usize);
}

// TODO: `Collect` has object safe `CollectErased` as a supertrait,
// which is declared and implemented by `erased-generic-trait` crate.
// Using that, we can have different types of collectors in a single storage,
// that means users can define their own collector.
// But downside of `erased-generic-trait` is performance penalty by a number of indirection.
// Please see `erased-generic-trait` documentation for more details.
#[erase_generic(CollectErased)]
pub trait CollectGeneric {
    /// Collects a single type.
    fn collect_type<T: 'static>(&mut self);

    /// Collects an item of the given type.
    /// Please make sure that collecting all types at once.
    /// In other words, call collect() as many as the number of types you put in
    /// between begin_collect() and end_collect().
    fn collect<T: 'static>(&mut self, key: usize, value: &mut T);

    /// Copies an item of the given type and key to the `value`.
    fn copy<T: 'static>(&mut self, key: usize, value: &mut T) -> Option<()>;
}

pub trait CollectNonGeneric {
    /// Determines if the given `TypeId` has been collected.
    fn contains_type(&self, ty: &TypeId) -> bool;

    /// Prepares collecting items.
    fn begin_collect(&mut self, key: usize);

    /// Finishes collecting items.
    fn end_collect(&mut self, key: usize);

    /// Removes items having the given key.
    fn remove(&mut self, key: usize);

    /// Retrieves how many items have been collected.
    fn len(&self) -> usize;

    /// Determines this is empty.
    fn is_empty(&self) -> bool;

    /// Returns a mutable raw pointer of a single type container.
    fn values_as_any(&mut self, ty: &TypeId) -> &mut dyn Any;
}

/// Object safe trait for `CollectGeneric` and `CollectNonGeneric`.
pub trait Collect: 'static + CollectErased + CollectNonGeneric {}

/// Blanket impl of `Collect`.
impl<T: 'static + CollectErased + CollectNonGeneric> Collect for T {}

/// This allows us to use CollectGeneric's methods on a Box<dyn Collect>.
/// It's exactly same with the impl by `erased-generic-trait`.
impl CollectGeneric for dyn Collect {
    #[inline]
    fn collect_type<T: 'static>(&mut self) {
        self.erased_collect_type(&ty!(T))
    }

    #[inline]
    fn collect<T: 'static>(&mut self, key: usize, value: &mut T) {
        self.erased_collect(&ty!(T), key, value)
    }

    #[inline]
    fn copy<T: 'static>(&mut self, key: usize, value: &mut T) -> Option<()> {
        self.erased_copy(&ty!(T), key, value)
    }
}

pub trait Downcast {
    fn downcast<T: 'static>(any: &mut dyn Any) -> &mut [T];
}

#[derive(Debug)]
pub struct Getter {
    pub(crate) me: *mut u8,
    pub(crate) len: usize,
    pub(crate) fn_get: unsafe fn(me: *mut u8, index: usize) -> NonNull<u8>,
}

impl Getter {
    #[inline]
    pub fn get(&self, index: usize) -> Option<NonNull<u8>> {
        if index < self.len {
            unsafe { Some(self.get_unchecked(index)) }
        } else {
            None
        }
    }

    #[inline]
    pub unsafe fn get_unchecked(&self, index: usize) -> NonNull<u8> {
        (self.fn_get)(self.me, index)
    }

    #[inline]
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.len
    }
}

/// Method warpper of [`Getter`] for the `&T`.
/// `&Getter` and `TypedGetter` are interchangable by the [`From`].
#[derive(Debug)]
pub struct TypedGetter<'a, T> {
    inner: &'a Getter,
    _marker: PhantomData<T>,
}

impl<'a, T> TypedGetter<'a, T> {
    /// # Safety
    ///
    /// Boundary is checked, but this method is still unsafe.
    /// See [`NonNull::as_ref`].
    #[inline]
    pub unsafe fn get(&self, index: usize) -> Option<&T> {
        if index < self.len() {
            // Safety:
            unsafe { Some(self.get_unchecked(index)) }
        } else {
            None
        }
    }

    #[inline]
    pub unsafe fn get_unchecked(&self, index: usize) -> &T {
        self.inner.get_unchecked(index).cast().as_ref()
    }

    #[inline]
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.inner.len()
    }
}

impl<'a, T> From<&'a Getter> for TypedGetter<'a, T> {
    #[inline]
    fn from(value: &'a Getter) -> Self {
        Self {
            inner: value,
            _marker: PhantomData,
        }
    }
}

impl<'a, T> From<TypedGetter<'a, T>> for &'a Getter {
    #[inline]
    fn from(value: TypedGetter<'a, T>) -> Self {
        value.inner
    }
}

/// Method warpper of [`Getter`] for the `&mut T`.
/// `&mut Getter` and `TypedGetterMut` are interchangable by the [`From`].
#[derive(Debug)]
pub struct TypedGetterMut<'a, T> {
    inner: &'a mut Getter,
    _marker: PhantomData<T>,
}

impl<'a, T> TypedGetterMut<'a, T> {
    /// # Safety
    ///
    /// Boundary is checked, but this method is still unsafe.
    /// See [`NonNull::as_mut`].
    #[inline]
    pub unsafe fn get(&mut self, index: usize) -> Option<&mut T> {
        if index < self.len() {
            unsafe { Some(self.get_unchecked(index)) }
        } else {
            None
        }
    }

    #[inline]
    pub unsafe fn get_unchecked(&mut self, index: usize) -> &mut T {
        self.inner.get_unchecked(index).cast().as_mut()
    }

    #[inline]
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.inner.len()
    }
}

impl<'a, T> From<&'a mut Getter> for TypedGetterMut<'a, T> {
    #[inline]
    fn from(value: &'a mut Getter) -> Self {
        Self {
            inner: value,
            _marker: PhantomData,
        }
    }
}

impl<'a, T> From<TypedGetterMut<'a, T>> for &'a mut Getter {
    #[inline]
    fn from(value: TypedGetterMut<'a, T>) -> Self {
        value.inner
    }
}

/// Functionalities for collecting heterogeneous static types of data.
/// In this trait, each types of data are gathered in each column and all columns have the same length like 2d matrix.
///
/// When it comes to in/out types, this trait has intentionally raw pointer parameters.
/// Although it's quite dangerous, it gives us object safety, so that we can have and manage another heterogeneous trait implementations.  
/// For instance, there's built-in implementation [`SparseSet`](super::sparse_set::SparseSet) which is deeply based on Vec.
/// You can implement your own data structure like hash map, then you can keep them in a buffer with dyn Together type.
#[allow(clippy::len_without_is_empty)]
pub trait Together: Debug {
    fn add_column(&mut self, tinfo: TypeInfo) -> Option<usize>;
    fn remove_column(&mut self, ci: usize) -> Option<TypeInfo>;
    fn new_from_this(&self) -> Box<dyn Together>;
    fn contains_column(&self, ty: &TypeId) -> bool;
    fn get_column_index(&self, ty: &TypeId) -> Option<usize>;
    fn get_column_info(&self, ci: usize) -> Option<&TypeInfo>;
    fn get_column_num(&self) -> usize;

    fn len(&self) -> usize;

    fn begin_add_item(&mut self);
    unsafe fn add_item(&mut self, ci: usize, ptr: *const u8);
    fn end_add_item(&mut self) -> usize;
    fn remove_item(&mut self, ri: usize) -> bool;

    fn get_item(&self, ci: usize, ri: usize) -> Option<NonNull<u8>>;
    fn get_item_mut(&mut self, ci: usize, ri: usize) -> Option<NonNull<u8>>;

    fn borrow_column(&self, ci: usize) -> Result<Borrowed<Getter, JsAtomic>, BorrowError>;
    fn borrow_column_mut(&mut self, ci: usize) -> Result<Borrowed<Getter, JsAtomic>, BorrowError>;
}
