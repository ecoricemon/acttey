pub trait NotifyType {
    /// Notifies types to the collector.
    /// This implementation calls the collector's functions to hand types over such as inner field's types.
    fn notify_types(&self, collector: &mut impl CollectType);

    /// Moves itself out into the collector.
    fn moves(self, collector: &mut impl CollectType, key: usize);
}

pub trait CollectType {
    /// Prepares collecting types.
    fn begin_collect_type(&mut self);

    /// Finishes collecting types.
    fn end_collect_type(&mut self);

    /// Collects a type.
    /// This implementation catches the type with the generic parameter.
    fn collect_type<T: 'static + Default>(&mut self);

    /// Prepares collecting items.
    fn begin_collect_item(&mut self, _key: usize);

    /// Finishes collecting items.
    fn end_collect_item(&mut self);

    /// Collects an item.
    fn collect_item<T: 'static + Default>(&mut self, key: usize, value: T);
}
