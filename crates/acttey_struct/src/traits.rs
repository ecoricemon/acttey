pub trait Notify {
    /// Notifies types to the collector.
    /// This implementation calls the collector's functions to hand types over such as inner field's types.
    fn notify_types(&self, collector: &mut impl Collect);

    /// Moves itself out into the collector.
    fn moves(self, collector: &mut impl Collect, key: usize);
}

pub trait Collect {
    /// Prepares collecting types.
    fn begin_collect_type(&mut self);

    /// Finishes collecting types.
    fn end_collect_type(&mut self);

    /// Collects a type.
    /// This implementation catches the type with the generic parameter.
    fn collect_type<V: 'static + Default>(&mut self);

    /// Prepares collecting items.
    fn begin_collect_item(&mut self, key: usize);

    /// Finishes collecting items.
    fn end_collect_item(&mut self, key: usize);

    /// Collects an item.
    fn collect_item<V: 'static + Default>(&mut self, key: usize, value: V);
}
