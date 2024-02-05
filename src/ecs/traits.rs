use crate::{ecs::storage::Storage, ty};
use erased_generic_trait::erase_generic;
use std::any::{Any, TypeId};

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
    /// TODO: Please fix me with better way :)
    fn validate();

    /// Notifies types to the collector.
    /// This implementation calls the collector's functions to hand types over such as inner field's types.
    /// And can't recive trait object based on `Collect` because types must be known beforehand becoming a trait object (See `erased-generic-trait`)
    fn notify_types<T>(collector: &mut T, storage: &mut Storage)
    where
        T: CollectGeneric + Downcast;

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
    fn downcast<C: 'static>(any: &mut dyn Any) -> &mut [C];
}
