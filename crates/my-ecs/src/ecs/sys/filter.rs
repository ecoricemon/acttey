use crate::ecs::ent::{
    component::{Component, ComponentKey},
    entity::{EntityIndex, EntityTag},
};
use crate::{ds::prelude::*, util::prelude::*};
use std::{
    any, iter,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr::NonNull,
    rc::Rc,
    sync::atomic::AtomicI32,
};

#[macro_export]
macro_rules! filter {
    (
        $vis:vis $id:ident
        $(, Target=( $($target:ty),+ ))?
        $(, All=( $($all:ty),+ ))?
        $(, Any=( $($any:ty),+ ))?
        $(, None=( $($none:ty),+ ))?
    ) => {
        #[derive(Debug)]
        $vis struct $id;
        impl $crate::ecs::sys::filter::Filter for $id {
            #[allow(unused_parens)]
            type Target = ( $( $($target),+ )? );
            #[allow(unused_parens)]
            type All = ( $( $($all),+ )? );
            #[allow(unused_parens)]
            type Any = ( $( $($any),+ )? );
            #[allow(unused_parens)]
            type None = ( $( $($none),+ )? );
        }
    };
}

/// A filter to select slices of [`Component`].
/// You should fill out this form of filter.
///
/// - `Target` is what `Component` you want. You will receive slices of this `Target`.
/// - `FilterAll` is a tuple of `Component`s to select entities that have all these `Component`s. Empty tuple means selecting all entities.
/// - `FilterAny` is a tuple of `Component`s to select entities that have any of these `Component`s. `Target` automatically belongs to this, so that empty tuple means selecting entities including `Target`.
/// - `FilterNone` is a tuple of `Component`s not to select entities that have any of these `Component`s. Empty tuple means selecting all entities.
pub trait Filter: 'static {
    type Target: Component;
    type All: AsTypes;
    type Any: AsTypes;
    type None: AsTypes;

    fn ids() -> [Box<[ComponentKey]>; 3] {
        let all = <Self::All as AsTypes>::types();
        let all = all
            .map(ComponentKey::new)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let any = <Self::Any as AsTypes>::types();
        let any = any
            .map(ComponentKey::new)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let none = <Self::None as AsTypes>::types();
        let none = none
            .map(ComponentKey::new)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        [all, any, none]
    }

    fn key() -> FilterKey {
        FilterKey::of::<Self>()
    }

    fn info<S>(info_stor: &mut S) -> Rc<FilterInfo>
    where
        S: StoreFilterInfo,
    {
        let key = Self::key();
        if let Some(info) = info_stor.get(&key) {
            info
        } else {
            let [all, any, none] = Self::ids();
            let info = Rc::new(FilterInfo {
                name: any::type_name::<Self>(),
                target: ComponentKey::of::<Self::Target>(),
                all,
                any,
                none,
            });
            info_stor.insert(key, &info);
            info
        }
    }
}

pub trait StoreFilterInfo {
    fn get(&self, key: &FilterKey) -> Option<Rc<FilterInfo>>;
    fn insert(&mut self, key: FilterKey, info: &Rc<FilterInfo>);
}

/// [`TypeId`] of a [`Filter`].
pub type FilterKey = ATypeId<FilterKey_>;
pub struct FilterKey_;

/// A Type that implemnts [`Filter`] can generate [`FilterInfo`],
/// which is a [`TypeId`] package of associated types of the `Filter`.
#[derive(Debug, Clone)]
pub struct FilterInfo {
    name: &'static str,
    target: ComponentKey,
    all: Box<[ComponentKey]>,
    any: Box<[ComponentKey]>,
    none: Box<[ComponentKey]>,
}

impl FilterInfo {
    pub fn name(&self) -> &'static str {
        self.name
    }

    pub fn target(&self) -> &ComponentKey {
        &self.target
    }

    pub fn all(&self) -> &[ComponentKey] {
        &self.all
    }

    pub fn any(&self) -> &[ComponentKey] {
        &self.any
    }

    pub fn none(&self) -> &[ComponentKey] {
        &self.none
    }

    pub fn filter<F>(&self, mut contains: F) -> bool
    where
        F: FnMut(&ComponentKey) -> bool,
    {
        let pass_any = iter::once(self.target())
            .chain(self.any())
            .any(&mut contains);

        let pass_any_all = pass_any && self.all().iter().all(&mut contains);

        let pass_any_all_none = pass_any_all && !self.none().iter().any(contains);

        pass_any_all_none
    }
}

/// Cached data by filtering.
#[derive(Debug)]
pub struct Filtered {
    /// [`EntityTag`] searched by the filter.
    etags: Vec<EntityTag>,

    /// Column(Component) index searched by the filter.
    col_idxs: Vec<usize>,

    /// Temporary buffer for the query result.
    /// Content will be replaced for every query, but we can reuse the capacity.
    /// Notice that this doesn't actually own [`Borrowed`] because this is just a temporary buffer.
    /// Real user, system, owns it and will drop it after using it.
    query_res: Vec<Borrowed<RawGetter, AtomicI32>>,
}

impl Filtered {
    pub const fn new(etags: Vec<EntityTag>, col_idxs: Vec<usize>) -> Self {
        Self {
            etags,
            col_idxs,
            query_res: Vec::new(),
        }
    }

    pub fn take(
        &mut self,
    ) -> (
        &Vec<EntityTag>,
        &Vec<usize>,
        &mut Vec<Borrowed<RawGetter, AtomicI32>>,
    ) {
        (&self.etags, &self.col_idxs, &mut self.query_res)
    }

    // By putting `etag` and `ci` in together, `Self::etags` and `Self::col_idxs` have the same length.
    pub fn add(&mut self, etag: EntityTag, ci: usize) {
        self.etags.push(etag);
        self.col_idxs.push(ci);
    }

    pub fn entity_tags(&self) -> &Vec<EntityTag> {
        &self.etags
    }

    pub fn column_indices(&self) -> &Vec<usize> {
        &self.col_idxs
    }

    pub fn query_res(&self) -> &Vec<Borrowed<RawGetter, AtomicI32>> {
        &self.query_res
    }

    pub fn query_res_mut(&mut self) -> &mut Vec<Borrowed<RawGetter, AtomicI32>> {
        &mut self.query_res
    }

    /// Retrieve an iterator that traverses over entity and column index pair.
    pub fn iter_index_pair<'a>(
        etags: &'a [EntityTag],
        col_idxs: &'a [usize],
    ) -> impl Iterator<Item = (EntityIndex, usize)> + 'a {
        // Self::add() guarantees that etags and col_idxs have the same length.
        etags
            .iter()
            .map(|etag| etag.index())
            .zip(col_idxs.iter().cloned())
    }
}

#[derive(Debug)]
pub struct RawFiltered<T> {
    /// Pointer to a slice of [`Borrowed`] of the [`RawGetter`].
    /// We can get each component in a column of an entity container through the `RawGetter`.
    /// `RawGetter`s are borrowed, they are wraped in `Borrowed` as you can see,
    /// so they will be released when they are dropped.
    raw_getters: NonNull<Borrowed<RawGetter, AtomicI32>>,

    /// Pointer to a slice of [`EntityTag`].
    /// `EntityTag` is a kind of metadata about an entity such as its name.
    etags: NonNull<EntityTag>,

    /// Length of two slices above.
    len: usize,

    _marker: PhantomData<T>,
}

impl<T> RawFiltered<T> {
    /// Creates iterator traversing over `getters` and `etags`.
    /// But the iterator holds pointers to them instead of their references.
    /// So caller must guarantee validity for those pointers.
    ///
    /// # Safety
    ///
    /// Undefined behavior if `getters` or `etags` are invalidated.
    /// For example, it's undfeind if one of them is dropped or aliased
    /// as mutable reference while the iterator lives.
    pub unsafe fn new(getters: &[Borrowed<RawGetter, AtomicI32>], etags: &[EntityTag]) -> Self {
        debug_assert_eq!(getters.len(), etags.len());

        let raw_getters = NonNull::new_unchecked(getters.as_ptr().cast_mut());
        let etags = NonNull::new_unchecked(etags.as_ptr().cast_mut());
        let len = getters.len();

        Self {
            raw_getters,
            etags,
            len,
            _marker: PhantomData,
        }
    }

    pub fn iter(&self) -> FilteredIter<T> {
        FilteredIter::new(self)
    }
}

impl<T> Clone for RawFiltered<T> {
    fn clone(&self) -> Self {
        Self {
            raw_getters: self.raw_getters,
            etags: self.etags,
            len: self.len,
            _marker: self._marker,
        }
    }
}

impl<T> Drop for RawFiltered<T> {
    fn drop(&mut self) {
        // Drops all `Borrowed`s.
        for i in 0..self.len {
            unsafe {
                let ptr = self.raw_getters.add(i);
                ptr.drop_in_place();
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct FilteredIter<'a, T: 'a> {
    /// See [`RawFiltered::raw_getters`].
    raw_getters: NonNull<Borrowed<RawGetter, AtomicI32>>,

    /// See [`RawFiltered::etags`].
    etags: NonNull<EntityTag>,

    /// Length of two slices above.
    len: usize,
    cur: usize,
    _marker: PhantomData<&'a T>,
}

impl<'a, T> FilteredIter<'a, T> {
    // Borrows `RawFiltered`.
    fn new(src: &'a RawFiltered<T>) -> Self {
        Self {
            raw_getters: src.raw_getters,
            etags: src.etags,
            len: src.len,
            cur: 0,
            _marker: PhantomData,
        }
    }
}

impl<'a, T> Iterator for FilteredIter<'a, T> {
    type Item = TaggedGetter<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cur < self.len {
            // Safety: Caller who called `Self::new()` guarantees validity of the pointers.
            unsafe {
                let raw_getter_ptr = self.raw_getters.add(self.cur);
                let etag_ptr = self.etags.add(self.cur);
                self.cur += 1;

                let raw_getter: RawGetter = **raw_getter_ptr.as_ref();
                let etag = ManagedConstPtr::new(etag_ptr.into());
                Some(TaggedGetter {
                    getter: Getter::new(raw_getter),
                    etag,
                    _marker: PhantomData,
                })
            }
        } else {
            None
        }
    }
}

impl<'a, T> ExactSizeIterator for FilteredIter<'a, T> {
    fn len(&self) -> usize {
        self.len - self.cur
    }
}

// This should not be clonable due to iter_mut().
#[derive(Debug)]
#[repr(transparent)]
pub struct RawFilteredMut<T>(RawFiltered<T>);

impl<T> RawFilteredMut<T> {
    /// Creates iterator traversing over `getters` and `etags`.
    /// But the iterator holds pointers to them instead of their references.
    /// So caller must guarantee validity for those pointers.
    ///
    /// # Safety
    ///
    /// Undefined behavior if `getters` or `etags` are invalidated.
    /// For example, it's undfeind if one of them is dropped or aliased
    /// while the iterator lives.
    pub unsafe fn new(getters: &mut [Borrowed<RawGetter, AtomicI32>], etags: &[EntityTag]) -> Self {
        Self(RawFiltered::new(getters, etags))
    }

    pub fn iter(&self) -> FilteredIter<T> {
        FilteredIter::new(&self.0)
    }

    pub fn iter_mut(&mut self) -> FilteredIterMut<T> {
        FilteredIterMut::new(self.iter())
    }
}

// Mutable iterator is not clonable.
#[derive(Debug)]
#[repr(transparent)]
pub struct FilteredIterMut<'a, T>(FilteredIter<'a, T>);

impl<'a, T> FilteredIterMut<'a, T> {
    const fn new(inner: FilteredIter<'a, T>) -> Self {
        Self(inner)
    }
}

impl<'a, T> Iterator for FilteredIterMut<'a, T> {
    type Item = TaggedGetterMut<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(
            |TaggedGetter {
                 getter,
                 etag,
                 _marker,
             }| {
                TaggedGetterMut {
                    // Safety: `GetterMut` is made from `Getter`,
                    // which proves it's `RawGetter` and type are valid.
                    getter: unsafe { GetterMut::new(getter.raw_getter) },
                    etag,
                    _marker: PhantomData,
                }
            },
        )
    }
}

impl<'a, T> ExactSizeIterator for FilteredIterMut<'a, T> {
    fn len(&self) -> usize {
        self.0.len()
    }
}

#[derive(Debug, Clone)]
pub struct TaggedGetter<'a, T> {
    pub getter: Getter<T>,
    pub etag: ManagedConstPtr<EntityTag>,
    _marker: PhantomData<&'a T>,
}

impl<'a, T> Deref for TaggedGetter<'a, T> {
    type Target = Getter<T>;

    fn deref(&self) -> &Self::Target {
        &self.getter
    }
}

impl<'a, T> IntoIterator for TaggedGetter<'a, T> {
    type Item = &'a T;
    type IntoIter = GetterIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        GetterIter::new_raw(self.getter.raw_getter)
    }
}

// Unlike `TaggedGetter`, this is a kind of mutable reference.
// So this is not clonable.
#[derive(Debug)]
pub struct TaggedGetterMut<'a, T> {
    pub getter: GetterMut<T>,
    pub etag: ManagedConstPtr<EntityTag>,
    _marker: PhantomData<&'a mut T>,
}

impl<'a, T> Deref for TaggedGetterMut<'a, T> {
    type Target = GetterMut<T>;

    fn deref(&self) -> &Self::Target {
        &self.getter
    }
}

impl<'a, T> DerefMut for TaggedGetterMut<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.getter
    }
}

impl<'a, T> IntoIterator for TaggedGetterMut<'a, T> {
    type Item = &'a mut T;
    type IntoIter = GetterIterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        GetterIterMut::new_raw(self.getter.raw_getter)
    }
}
