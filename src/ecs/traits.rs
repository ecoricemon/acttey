use erased_generic_trait::erase_generic;
use std::any::TypeId;

pub trait Component: 'static {}

pub trait Entity: 'static {
    /// DO NOT call me!
    /// This is an internal validation function.
    fn validate();

    /// Notifies types to the collector.
    /// This implementation calls the collector's functions to hand types over such as inner field's types.
    fn notify_types(collector: &mut impl Collect);

    /// Moves itself out into the collector.
    fn moves(self, collector: &mut Box<dyn ErasedCollect>, key: usize);
}

#[erase_generic(ErasedCollect)]
pub trait Collect {
    /// Collects a single type.
    fn collect_type<V: 'static>(&mut self);

    /// Determines if the given `TypeId` has been collected.
    fn contains_type(&self, ty_id: &TypeId) -> bool;

    /// Prepares collecting items.
    fn begin_collect(&mut self, key: usize);

    /// Finishes collecting items.
    fn end_collect(&mut self, key: usize);

    /// Collects an item of the given type.
    /// Please make sure that collecting all types at once.
    /// In other words, call collect() as many as the number of types you put in
    /// between begin_collect() and end_collect().
    fn collect<V: 'static>(&mut self, key: usize, value: &mut V);

    /// Copies an item.
    fn copy<V: 'static>(&mut self, key: usize, value: &mut V) -> Option<()>;

    /// Removes items having the given key.
    fn remove(&mut self, key: usize);

    /// Retrieves how many items have been collected.
    fn len(&self) -> usize;

    /// Determines this is empty.
    fn is_empty(&self) -> bool;
}
