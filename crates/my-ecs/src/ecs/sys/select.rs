use crate::{
    ds::prelude::*,
    ecs::ent::{
        component::{Component, ComponentKey, Components},
        entity::{ContainEntity, Entity, EntityIndex, EntityName, EntityTag},
        storage::EntityContainerRef,
    },
    impl_into_iterator_for_parallel, impl_parallel_iterator, impl_unindexed_producer,
    util::TakeRecur,
};
use rayon::iter::{plumbing::Producer, IntoParallelIterator};
use std::{
    any, fmt, iter,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr::NonNull,
    sync::Arc,
};

pub(crate) trait StoreSelectInfo: StoreFilterInfo {
    fn contains(&self, key: &SelectKey) -> bool;
    fn get(&self, key: &SelectKey) -> Option<&Arc<SelectInfo>>;
    fn insert(&mut self, key: SelectKey, info: Arc<SelectInfo>);
}

pub(crate) trait StoreFilterInfo {
    fn contains(&self, key: &FilterKey) -> bool;
    fn get(&self, key: &FilterKey) -> Option<&Arc<FilterInfo>>;
    fn insert(&mut self, key: FilterKey, info: Arc<FilterInfo>);
}

/// A trait for selecting a certain [`Target`](Select::Target) from entities
/// that meet [`All`](Filter::All), [`Any`](Filter::Any), and
/// [`None`](Filter::None) conditions.
///
/// # Example
///
/// ```ignore
/// # use my_ecs::prelude::*;
///
/// #[derive(Component)] struct Ca;
/// #[derive(Component)] struct Cb;
/// #[derive(Component)] struct Cc;
/// #[derive(Component)] struct Cd;
///
/// struct Sa;
/// impl Select for Sa {
///     type Target = Ca;
///     type Filter = Fa;
/// }
/// struct Fa;
/// impl Filter for Fa {
///     type All = Cb;
///     type Any = (Cc, Cd);
///     type None = ();
/// }
///
/// // Or simply
/// filter!(Sb, Target = Ca, All = Cb, Any = (Cc, Cd));
/// ```
#[allow(private_interfaces, private_bounds)]
pub trait Select: 'static {
    type Target: Component;
    type Filter: Filter;

    #[doc(hidden)]
    fn key() -> SelectKey {
        SelectKey::of::<Self>()
    }

    #[doc(hidden)]
    fn get_info_from<S>(stor: &mut S) -> &Arc<SelectInfo>
    where
        S: StoreSelectInfo + StoreFilterInfo + ?Sized,
    {
        let key = Self::key();

        if !StoreSelectInfo::contains(stor, &key) {
            let sinfo = Arc::new(Self::info_from(stor));
            StoreSelectInfo::insert(stor, key, sinfo);
        }

        // Safety: Inserted right before.
        unsafe { StoreSelectInfo::get(stor, &key).unwrap_unchecked() }
    }

    #[doc(hidden)]
    fn info_from<S>(stor: &mut S) -> SelectInfo
    where
        S: StoreFilterInfo + ?Sized,
    {
        let target = ComponentKey::of::<Self::Target>();
        let finfo = Arc::clone(Self::Filter::get_info_from(stor));
        let name = any::type_name::<Self>();
        SelectInfo::new(target, finfo, name)
    }
}

/// A trait for selecting certain entities that meet [`All`](Filter::All),
/// [`Any`](Filter::Any), and [`None`](Filter::None) conditions.
///
/// # Example
///
/// ```ignore
/// # use my_ecs::prelude::*;
///
/// #[derive(Component)] struct Ca;
/// #[derive(Component)] struct Cb;
/// #[derive(Component)] struct Cc;
/// #[derive(Component)] struct Cd;
///
/// /// Filtering using All, Any, and None.
/// struct Fa;
/// impl Filter for Fa {
///     type All = Ca;
///     type Any = (Cb, Cc);
///     type None = Cd;
///     type Exact = ();
/// }
///
/// // Or simply
/// filter!(Fb, All = Ca, Any = (Cb, Cc));
///
/// /// Filtering using Exact.
/// struct Fb;
/// impl Filter for Fc {
///     type All = ();
///     type Any = ();
///     type None = ();
///     type Exact = (Ca, Cb);
/// }
///
/// // Or simply
/// filter!(Fd, Exact = (Ca, Cb));
/// ```
#[allow(private_interfaces, private_bounds)]
pub trait Filter: 'static {
    /// A [`Component`] group to select entities that contains all components in
    /// this group. It's something like *AND* condition. But if `All` is empty,
    /// then any entities won't be rejected.
    type All: Components;

    /// A [`Component`] group to select entities that contains any components in
    /// this group. It's something like *OR* condition. But if `Any` is empty,
    /// then any entities won't be rejected.
    type Any: Components;

    /// A [`Component`] group to select entities that don't contain any
    /// components in this group. It's something like *NOR* condition. Buf if
    /// `None` is empty, then any entities won't be rejected.
    type None: Components;

    /// A [`Component`] group to select a specific entity that consists of
    /// components in this group exactly.
    type Exact: Components;

    #[doc(hidden)]
    fn key() -> FilterKey {
        FilterKey::of::<Self>()
    }

    #[doc(hidden)]
    fn all_any_none() -> [Box<[ComponentKey]>; 3] {
        let all: Box<[ComponentKey]> = Self::All::keys().as_ref().into();
        let any: Box<[ComponentKey]> = Self::Any::keys().as_ref().into();
        let none: Box<[ComponentKey]> = Self::None::keys().as_ref().into();
        [all, any, none]
    }

    #[doc(hidden)]
    fn get_info_from<S>(stor: &mut S) -> &Arc<FilterInfo>
    where
        S: StoreFilterInfo + ?Sized,
    {
        let key = Self::key();

        if !stor.contains(&key) {
            let sinfo = Arc::new(Self::info());
            stor.insert(key, sinfo);
        }

        // Safety: Inserted right before.
        unsafe { stor.get(&key).unwrap_unchecked() }
    }

    #[doc(hidden)]
    fn info() -> FilterInfo {
        let [all, any, none] = Self::all_any_none();
        let exact = Self::Exact::keys().as_ref().into();
        let name = any::type_name::<Self>();
        FilterInfo::new(all, any, none, exact, name)
    }
}

/// An [`Entity`] is an exact [`Filter`].
impl<T: Entity> Filter for T {
    type All = ();
    type Any = ();
    type None = ();
    type Exact = T;
}

/// Unique identifier for a type implementing [`Select`].
pub(crate) type SelectKey = ATypeId<SelectKey_>;
pub(crate) struct SelectKey_;

/// Unique identifier for a type implementing [`Filter`].
pub(crate) type FilterKey = ATypeId<FilterKey_>;
pub(crate) struct FilterKey_;

#[derive(Clone)]
pub(crate) struct SelectInfo {
    target: ComponentKey,
    finfo: Arc<FilterInfo>,
    name: &'static str,
}

impl fmt::Debug for SelectInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SelectInfo")
            .field("name", &self.name())
            .field("target", &self.target())
            .field("finfo", &self.filter_info())
            .finish()
    }
}

impl SelectInfo {
    const fn new(target: ComponentKey, finfo: Arc<FilterInfo>, name: &'static str) -> Self {
        Self {
            target,
            finfo,
            name,
        }
    }

    pub(crate) const fn target(&self) -> &ComponentKey {
        &self.target
    }

    pub(crate) const fn filter_info(&self) -> &Arc<FilterInfo> {
        &self.finfo
    }

    pub(crate) const fn name(&self) -> &'static str {
        self.name
    }

    pub(crate) fn filter<F>(&self, contains: F, num_columns: usize) -> bool
    where
        F: Fn(&ComponentKey) -> bool,
    {
        contains(self.target()) && self.finfo.filter(contains, num_columns)
    }

    /// Determines that the given selector is disjoint with this selector.
    ///
    /// Disjoint filters mean that two filters don't overlap at all.
    /// Table below shows the disjoint conditions.
    /// - Two have diffrent targets.
    /// - Two are disjoint filters.
    pub(crate) fn is_disjoint(&self, rhs: &Self) -> bool {
        (self.target != rhs.target) || self.finfo.is_disjoint(&rhs.finfo)
    }

    pub(crate) fn is_disjoint2(&self, rhs: &FilterInfo) -> bool {
        self.finfo.is_disjoint(rhs)
    }
}

#[derive(Clone)]
pub(crate) struct FilterInfo {
    all: Box<[ComponentKey]>,
    any: Box<[ComponentKey]>,
    none: Box<[ComponentKey]>,
    exact: Box<[ComponentKey]>,
    name: &'static str,
}

impl fmt::Debug for FilterInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FilterInfo")
            .field("name", &self.name())
            .field("all", &self.all())
            .field("any", &self.any())
            .field("none", &self.none())
            .field("exact", &self.exact())
            .finish()
    }
}

impl FilterInfo {
    const fn new(
        all: Box<[ComponentKey]>,
        any: Box<[ComponentKey]>,
        none: Box<[ComponentKey]>,
        exact: Box<[ComponentKey]>,
        name: &'static str,
    ) -> Self {
        Self {
            all,
            any,
            none,
            exact,
            name,
        }
    }

    pub(crate) const fn all(&self) -> &[ComponentKey] {
        &self.all
    }

    pub(crate) const fn any(&self) -> &[ComponentKey] {
        &self.any
    }

    pub(crate) const fn none(&self) -> &[ComponentKey] {
        &self.none
    }

    pub(crate) const fn exact(&self) -> &[ComponentKey] {
        &self.exact
    }

    pub(crate) const fn name(&self) -> &'static str {
        self.name
    }

    pub(crate) fn filter<F>(&self, contains: F, num_columns: usize) -> bool
    where
        F: Fn(&ComponentKey) -> bool,
    {
        // empty iter.all() -> returns true.
        // empty iter.any() -> returns false.

        if !self.exact.is_empty() {
            self.exact.len() == num_columns && self.exact.iter().all(&contains)
        } else {
            self.all.iter().all(&contains)
                && !self.none.iter().any(&contains)
                && if self.any.is_empty() {
                    true
                } else {
                    self.any.iter().any(contains)
                }
        }
    }

    /// Determines that the given filter is disjoint with this filter.
    ///
    /// Disjoint filters mean that two filters don't overlap at all.
    /// Disjoint conditions are as follows.
    pub(crate) fn is_disjoint(&self, rhs: &Self) -> bool {
        let is_self_general = self.exact.is_empty();
        let is_rhs_general = rhs.exact.is_empty();
        match (is_self_general, is_rhs_general) {
            // Exact and Exact
            (false, false) => self.is_disjoint_exact_exact(rhs),
            // Exact and General
            (false, true) => rhs.is_disjoint_general_exact(self),
            // General and Exact
            (true, false) => self.is_disjoint_general_exact(rhs),
            // General and General
            (true, true) => self.is_disjoint_general_general(rhs),
        }
    }

    /// # General vs. General disjoint conditions
    /// - X's All intersects Y's None or vice versa.
    /// - X's Any is a subset of Y's None or vice versa.
    ///
    /// | Filter | All   | None     | Any   |
    /// | :---:  | :---: | :---:    | :---: |
    /// | FA     | A, B  | C, D     | E, F  |
    /// | FB     | ..    | A, ..    | ..    | 1. FA::All intersects FB::None
    /// | FB     | ..    | B, ..    | ..    | 1. FA::All intersects FB::None
    /// | FB     | C, .. | ..       | ..    | 2. FA::None intersects FB::All
    /// | FB     | D, .. | ..       | ..    | 2. FA::None intersects FB::All
    /// | FB     | ..    | ..       | C     | 3. FB::Any is a subset of FA::None
    /// | FB     | ..    | ..       | D     | 3. FB::Any is a subset of FA::None
    /// | FB     | ..    | ..       | C, D  | 3. FB::Any is a subset of FA::None
    /// | FB     | ..    | E, F, .. | ..    | 4. FA::Any is a subset of FB::None
    fn is_disjoint_general_general(&self, rhs: &Self) -> bool {
        let (a_all, a_any, a_none) = (&self.all, &self.any, &self.none);
        let (b_all, b_any, b_none) = (&rhs.all, &rhs.any, &rhs.none);

        // 1. FA::All intersects FB::None
        if a_all.iter().any(|a| b_none.contains(a)) {
            return true;
        }

        // 2. FA::None intersects FA::All
        if a_none.iter().any(|a| b_all.contains(a)) {
            return true;
        }

        // 3. FB::Any is a subset of FA::None
        if !b_any.is_empty() && b_any.iter().all(|b| a_none.contains(b)) {
            return true;
        }

        // 4. FA::Any is a subset of FB::None
        if !a_any.is_empty() && a_any.iter().all(|a| b_none.contains(a)) {
            return true;
        }

        false
    }

    /// # Exact vs. filter disjoint conditions
    ///
    /// - Not the same one
    fn is_disjoint_exact_exact(&self, rhs: &Self) -> bool {
        if self.exact.len() != rhs.exact.len() {
            return false;
        }

        self.exact.iter().any(|l| !rhs.exact.contains(l))
    }

    /// # General vs. filter disjoint conditions
    ///
    /// | Filter | All   | None  | Any   | Exact           |
    /// | :---:  | :---: | :---: | :---: | :--:            |
    /// | FA     | A, B  | C, D  | E, F  |                 |
    /// | FB     |       |       |       | not A, ...      | // Case 1
    /// | FB     |       |       |       | not B, ...      | // Case 1
    /// | FB     |       |       |       | C, ...          | // Case 2
    /// | FB     |       |       |       | D, ...          | // Case 2
    /// | FB     |       |       |       | ... except E, F | // Case 3
    fn is_disjoint_general_exact(&self, rhs: &Self) -> bool {
        let (a_all, a_any, a_none) = (&self.all, &self.any, &self.none);
        let b_exact = &rhs.exact;

        // Case 1. `FB` includes one not beloning `FA's All`.
        if b_exact.iter().any(|b| !a_all.contains(b)) {
            return true;
        }

        // Case 2. `FB` includes one of `FA's None`.
        if b_exact.iter().any(|b| a_none.contains(b)) {
            return true;
        }

        // Case 3. `FB` doesn't include any of `FA's Any`.
        if a_any.iter().all(|a| !b_exact.contains(a)) {
            return true;
        }

        false
    }
}

/// Shared references to [`Select::Target`] componenet arrays from multiple
/// entities. You can get an iterator traversing over each component array via
/// [`iter`](Self::iter). A component array belongs to a specific entity.
#[derive(Debug)]
pub struct Selected<'cont, Comp: 'cont> {
    /// A struct holding borrowed component arrays and their entity tags.
    raw: &'cont mut SelectedRaw,

    /// Holds component type.
    _marker: PhantomData<Comp>,
}

impl<'cont, Comp: 'cont> Selected<'cont, Comp> {
    /// Creates [`Selected`] from a mutable reference to a [`SelectedRaw`].
    /// [`SelectedRaw`] is not a container, but it's borrowing container's data,
    /// and holding them inside [`Borrowed`]s. So we can think lifetime to the
    /// '&mut [`SelectedRaw`]' is as if container's.
    pub(crate) fn new(raw: &'cont mut SelectedRaw) -> Self {
        Self {
            raw,
            _marker: PhantomData,
        }
    }

    pub fn iter(&self) -> SelectedIter<'cont, Comp> {
        SelectedIter::new(self)
    }

    pub fn par_iter(&self) -> ParSelectedIter<'cont, Comp> {
        ParSelectedIter(self.iter())
    }

    pub(crate) fn as_raw(&self) -> &SelectedRaw {
        self.raw
    }
}

/// Mutable references to [`Select::Target`] component arrays from multiple
/// entities.  You can get an iterator traversing over each component array via
/// [`iter`](Self::iter) or [`iter_mut`](Self::iter_mut). A component array
/// belongs to a specific entity.
//
// `Selected` has mutable reference to a `SelectedRaw` in it.
// So we can make use of it and expose mutable methods to clients here.
#[derive(Debug)]
#[repr(transparent)]
pub struct SelectedMut<'cont, Comp>(Selected<'cont, Comp>);

impl<'cont, Comp: 'cont> SelectedMut<'cont, Comp> {
    pub(crate) fn new(filtered: &'cont mut SelectedRaw) -> Self {
        Self(Selected::new(filtered))
    }

    pub fn iter(&self) -> SelectedIter<'cont, Comp> {
        self.0.iter()
    }

    pub fn iter_mut(&mut self) -> SelectedIterMut<'cont, Comp> {
        SelectedIterMut(self.iter())
    }

    pub fn par_iter(&self) -> ParSelectedIter<'cont, Comp> {
        self.0.par_iter()
    }

    pub fn par_iter_mut(&mut self) -> ParSelectedIterMut<'cont, Comp> {
        ParSelectedIterMut(self.iter_mut())
    }
}

#[derive(Debug)]
pub struct FilteredMut<'stor, T: 'stor> {
    raw: &'stor mut FilteredRaw,
    _marker: PhantomData<T>,
}

impl<'stor, T: 'stor> FilteredMut<'stor, T> {
    pub(crate) fn new(raw: &'stor mut FilteredRaw) -> Self {
        Self {
            raw,
            _marker: PhantomData,
        }
    }

    pub fn iter_mut(&mut self) -> FilteredIterMut<'stor, T> {
        FilteredIterMut::new(self)
    }

    pub(crate) fn as_raw(&self) -> &FilteredRaw {
        self.raw
    }
}

impl<'stor, T: Entity + 'stor> TakeRecur for FilteredMut<'stor, T> {
    type Inner = Option<EntityContainerRef<'stor, T>>;

    fn take_recur(mut self) -> Self::Inner {
        self.iter_mut().next()
    }
}

/// Selected component arryas by a [`Select`].  
///
// This struct contains borrowed `Select::Target` arrays. But, this struct
// doesn't bring lifetime constraint into inside the struct although it
// borrows component arrays. Instead, borrowed data are encapsulated by
// `Borrowed`, which is a run-time borrow checker. In other words, component
// arrays must be borrowed and released everytime.
//
// This struct is intended to be used as a cache without lifetime.
// Cache is a data storage which lives as long as system data.
// But system data will live indefinitely,
// so removing lifetime helps to keep things simple.
#[derive(Debug)]
pub(crate) struct SelectedRaw {
    /// [EntityTag] searched by the filter.
    //
    // Each system owns `SelectedRaw`, so `etags` and `col_idxs` will be
    // duplicated between systems.
    etags: Vec<Arc<EntityTag>>,

    /// Column(Component) index searched by the filter.
    //
    // Each system owns `SelectedRaw`, so `etags` and `col_idxs` will be
    // duplicated between systems.
    col_idxs: Vec<usize>,

    /// Temporary buffer for the query result.
    /// Content will be replaced for every query, but we can reuse the capacity.
    /// Notice that this doesn't actually own [Borrowed] because this is just a temporary buffer.
    /// Real user, system, owns it and will drop it after using it.
    //
    // See `request::BufferCleaner` for more details.
    query_res: Vec<Borrowed<RawGetter>>,
}

impl SelectedRaw {
    pub(crate) const fn new(etags: Vec<Arc<EntityTag>>, col_idxs: Vec<usize>) -> Self {
        Self {
            etags,
            col_idxs,
            query_res: Vec::new(),
        }
    }

    pub(crate) fn take(
        &mut self,
    ) -> (
        &Vec<Arc<EntityTag>>,
        &Vec<usize>,
        &mut Vec<Borrowed<RawGetter>>,
    ) {
        (&self.etags, &self.col_idxs, &mut self.query_res)
    }

    // `etags` and `col_idxs` always have the same length.
    pub(crate) fn add(&mut self, etag: Arc<EntityTag>, ci: usize) {
        self.etags.push(etag);
        self.col_idxs.push(ci);
    }

    // `etags` and `col_idxs` always have the same length.
    pub(crate) fn remove(&mut self, ei: EntityIndex, ci: usize) -> Option<Arc<EntityTag>> {
        let ei_iter = self.etags.iter().map(|etag| etag.index());
        let ei_ci_iter = ei_iter.zip(&self.col_idxs);

        if let Some((i, _)) = ei_ci_iter
            .enumerate()
            .find(|(_, (item_ei, item_ci))| *item_ei == ei && **item_ci == ci)
        {
            let old = self.etags.swap_remove(i);
            self.col_idxs.swap_remove(i);
            Some(old)
        } else {
            None
        }
    }

    /// Retrieve an iterator that traverses over entity and column index pair.
    pub(crate) fn iter_index_pair<'a>(
        etags: &'a [Arc<EntityTag>],
        col_idxs: &'a [usize],
    ) -> impl Iterator<Item = (EntityIndex, usize)> + 'a {
        // Self::add() guarantees that etags and col_idxs have the same length.
        etags
            .iter()
            .map(|etag| etag.index())
            .zip(col_idxs.iter().cloned())
    }

    pub(crate) fn clear(&mut self) {
        self.query_res.clear();
    }

    pub(crate) const fn query_res(&self) -> &Vec<Borrowed<RawGetter>> {
        &self.query_res
    }

    const fn entity_tags(&self) -> &Vec<Arc<EntityTag>> {
        &self.etags
    }
}

#[derive(Debug)]
pub(crate) struct FilteredRaw {
    etags: Vec<Arc<EntityTag>>,
    query_res: Vec<Borrowed<NonNull<dyn ContainEntity>>>,
}

impl FilteredRaw {
    pub(crate) const fn new(etags: Vec<Arc<EntityTag>>) -> Self {
        Self {
            etags,
            query_res: Vec::new(),
        }
    }

    #[allow(clippy::type_complexity)]
    pub(crate) fn take(
        &mut self,
    ) -> (
        &Vec<Arc<EntityTag>>,
        &mut Vec<Borrowed<NonNull<dyn ContainEntity>>>,
    ) {
        (&self.etags, &mut self.query_res)
    }

    pub(crate) fn add(&mut self, etag: Arc<EntityTag>) {
        self.etags.push(etag);
    }

    pub(crate) fn remove(&mut self, ei: &EntityIndex) -> Option<Arc<EntityTag>> {
        if let Some((i, _)) = self
            .etags
            .iter()
            .enumerate()
            .find(|(_, x)| &x.index() == ei)
        {
            let old = self.etags.swap_remove(i);
            Some(old)
        } else {
            None
        }
    }

    pub(crate) fn clear(&mut self) {
        self.query_res.clear();
    }

    pub(crate) const fn query_res(&self) -> &Vec<Borrowed<NonNull<dyn ContainEntity>>> {
        &self.query_res
    }

    const fn entity_tags(&self) -> &Vec<Arc<EntityTag>> {
        &self.etags
    }
}

#[derive(Debug, Clone)]
pub struct SelectedIter<'cont, Comp: 'cont> {
    getter_cur: SendSyncPtr<Borrowed<RawGetter>>,

    getter_end: SendSyncPtr<Borrowed<RawGetter>>,

    etag_cur: SendSyncPtr<Arc<EntityTag>>,

    etag_end: SendSyncPtr<Arc<EntityTag>>,

    _marker: PhantomData<&'cont Comp>,
}

impl<'cont, Comp: 'cont> SelectedIter<'cont, Comp> {
    // Borrows `Selected`.
    fn new(raw: &Selected<'cont, Comp>) -> Self {
        // Safety: `Selected` guarantees we're good to access those vecs.
        unsafe {
            let getter_range = raw.as_raw().query_res().as_ptr_range();
            let getter_cur = NonNull::new_unchecked(getter_range.start.cast_mut());
            let getter_end = NonNull::new_unchecked(getter_range.end.cast_mut());

            let etag_range = raw.as_raw().entity_tags().as_ptr_range();
            let etag_cur = NonNull::new_unchecked(etag_range.start.cast_mut());
            let etag_end = NonNull::new_unchecked(etag_range.end.cast_mut());

            Self {
                getter_cur: SendSyncPtr::new(getter_cur),
                getter_end: SendSyncPtr::new(getter_end),
                etag_cur: SendSyncPtr::new(etag_cur),
                etag_end: SendSyncPtr::new(etag_end),
                _marker: PhantomData,
            }
        }
    }

    #[inline]
    pub const fn len(&self) -> usize {
        let cur = self.getter_cur.as_ptr();
        let end = self.getter_end.as_ptr();
        unsafe { end.offset_from(cur) as usize }
    }

    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline]
    fn split_at(self, index: usize) -> (Self, Self) {
        let l_getter_cur = self.getter_cur;
        let l_getter_end = unsafe { self.getter_cur.add(index) };
        let r_getter_cur = l_getter_end;
        let r_getter_end = self.getter_end;

        let l_etag_cur = self.etag_cur;
        let l_etag_end = unsafe { self.etag_cur.add(index) };
        let r_etag_cur = l_etag_end;
        let r_etag_end = self.etag_end;

        let l = SelectedIter {
            getter_cur: l_getter_cur,
            getter_end: l_getter_end,
            etag_cur: l_etag_cur,
            etag_end: l_etag_end,
            _marker: PhantomData,
        };
        let r = SelectedIter {
            getter_cur: r_getter_cur,
            getter_end: r_getter_end,
            etag_cur: r_etag_cur,
            etag_end: r_etag_end,
            _marker: PhantomData,
        };
        (l, r)
    }

    unsafe fn create_item(
        getter: SendSyncPtr<Borrowed<RawGetter>>,
        etag: SendSyncPtr<Arc<EntityTag>>,
    ) -> TaggedGetter<'cont, Comp> {
        let raw = **getter.as_ref();
        let getter = Getter::from_raw(raw);

        let etag = Arc::as_ptr(etag.as_ref()).cast_mut();
        let etag = NonNullExt::new(etag).unwrap_unchecked();
        let etag = ManagedConstPtr::new(etag);

        TaggedGetter { getter, etag }
    }
}

impl<'cont, Comp: 'cont> Iterator for SelectedIter<'cont, Comp> {
    type Item = TaggedGetter<'cont, Comp>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.getter_cur < self.getter_end {
            let getter = self.getter_cur;
            let etag = self.etag_cur;
            unsafe {
                self.getter_cur = self.getter_cur.add(1);
                self.etag_cur = self.etag_cur.add(1);
                Some(Self::create_item(getter, etag))
            }
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = Self::len(self);
        (len, Some(len))
    }
}

impl<'cont, Comp: 'cont> iter::FusedIterator for SelectedIter<'cont, Comp> {}

impl<'cont, Comp: 'cont> ExactSizeIterator for SelectedIter<'cont, Comp> {
    fn len(&self) -> usize {
        Self::len(self)
    }
}

impl<'cont, Comp: 'cont> DoubleEndedIterator for SelectedIter<'cont, Comp> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.getter_cur < self.getter_end {
            unsafe {
                self.getter_end = self.getter_cur.sub(1);
                self.etag_end = self.etag_cur.sub(1);
                Some(Self::create_item(self.getter_end, self.etag_end))
            }
        } else {
            None
        }
    }
}

/// Parallel [`SelectedIter`].
//
// `Iterator` and `ParallelIterator` have the same signature methods,
// So clients have to write fully-qualified syntax to specify methods.
// This new type helps clients avoid it.
#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct ParSelectedIter<'cont, Comp>(pub SelectedIter<'cont, Comp>);

impl<'cont, Comp: 'cont> ParSelectedIter<'cont, Comp> {
    #[inline]
    pub const fn into_seq(self) -> SelectedIter<'cont, Comp> {
        self.0
    }

    #[inline]
    pub const fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<'cont, Comp: Send + Sync + 'cont> Producer for ParSelectedIter<'cont, Comp> {
    type Item = TaggedGetter<'cont, Comp>;
    type IntoIter = SelectedIter<'cont, Comp>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.into_seq()
    }

    #[inline]
    fn split_at(self, index: usize) -> (Self, Self) {
        let (l, r) = SelectedIter::split_at(self.0, index);
        (ParSelectedIter(l), ParSelectedIter(r))
    }
}

impl_into_iterator_for_parallel!(
    "lifetimes" = 'cont; "bounds" = Comp: {'cont};
    "for" = ParSelectedIter; "to" = SelectedIter<'cont, Comp>;
    "item" = TaggedGetter<'cont, Comp>;
);
impl_parallel_iterator!(
    "lifetimes" = 'cont; "bounds" = Comp: {Send + Sync + 'cont};
    "for" = ParSelectedIter; "item" = TaggedGetter<'cont, Comp>;
);
impl_unindexed_producer!(
    "lifetimes" = 'cont; "bounds" = Comp: {Send + Sync + 'cont};
    "for" = ParSelectedIter; "item" = TaggedGetter<'cont, Comp>;
);

// Mutable iterator is not clonable.
#[derive(Debug)]
#[repr(transparent)]
pub struct SelectedIterMut<'cont, Comp>(pub SelectedIter<'cont, Comp>);

impl<'cont, Comp: 'cont> SelectedIterMut<'cont, Comp> {
    #[inline]
    pub const fn into_seq(self) -> SelectedIter<'cont, Comp> {
        self.0
    }

    #[inline]
    pub const fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[inline]
    fn split_at(self, index: usize) -> (Self, Self) {
        let (l, r) = SelectedIter::split_at(self.0, index);
        (Self(l), Self(r))
    }
}

impl<'cont, Comp> Iterator for SelectedIterMut<'cont, Comp> {
    type Item = TaggedGetterMut<'cont, Comp>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|TaggedGetter { getter, etag }| {
            TaggedGetterMut {
                // Safety: `GetterMut` is made from `Getter`,
                // which proves it's `RawGetter` and type are valid.
                getter: unsafe { GetterMut::from_raw(getter.into_raw()) },
                etag,
            }
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'cont, Comp: 'cont> iter::FusedIterator for SelectedIterMut<'cont, Comp> {}

impl<'cont, Comp: 'cont> ExactSizeIterator for SelectedIterMut<'cont, Comp> {
    fn len(&self) -> usize {
        Self::len(self)
    }
}

impl<'cont, Comp: 'cont> DoubleEndedIterator for SelectedIterMut<'cont, Comp> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(|TaggedGetter { getter, etag }| {
            TaggedGetterMut {
                // Safety: `GetterMut` is made from `Getter`,
                // which proves it's `RawGetter` and type are valid.
                getter: unsafe { GetterMut::from_raw(getter.into_raw()) },
                etag,
            }
        })
    }
}

/// Parallel [`SelectedIterMut`].
//
// `Iterator` and `ParallelIterator` have the same signature methods,
// So clients have to write fully-qualified syntax to specify methods.
// This new type helps clients avoid it.
#[derive(Debug)]
#[repr(transparent)]
pub struct ParSelectedIterMut<'cont, Comp>(pub SelectedIterMut<'cont, Comp>);

impl<'cont, Comp: 'cont> ParSelectedIterMut<'cont, Comp> {
    #[inline]
    pub const fn into_seq(self) -> SelectedIterMut<'cont, Comp> {
        self.0
    }

    #[inline]
    pub const fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<'cont, Comp: Send + Sync> Producer for ParSelectedIterMut<'cont, Comp> {
    type Item = TaggedGetterMut<'cont, Comp>;
    type IntoIter = SelectedIterMut<'cont, Comp>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.into_seq()
    }

    #[inline]
    fn split_at(self, index: usize) -> (Self, Self) {
        let (l, r) = SelectedIterMut::split_at(self.0, index);
        (ParSelectedIterMut(l), ParSelectedIterMut(r))
    }
}

impl_into_iterator_for_parallel!(
    "lifetimes" = 'cont; "bounds" = Comp: {'cont};
    "for" = ParSelectedIterMut; "to" = SelectedIterMut<'cont, Comp>;
    "item" = TaggedGetterMut<'cont, Comp>;
);
impl_parallel_iterator!(
    "lifetimes" = 'cont; "bounds" = Comp: {Send + Sync + 'cont};
    "for" = ParSelectedIterMut; "item" = TaggedGetterMut<'cont, Comp>;
);
impl_unindexed_producer!(
    "lifetimes" = 'cont; "bounds" = Comp: {Send + Sync + 'cont};
    "for" = ParSelectedIterMut; "item" = TaggedGetterMut<'cont, Comp>;
);

#[derive(Debug)]
pub struct FilteredIterMut<'stor, T: 'stor> {
    cont_cur: NonNull<Borrowed<NonNull<dyn ContainEntity>>>,
    cont_end: NonNull<Borrowed<NonNull<dyn ContainEntity>>>,
    etag_cur: NonNull<Arc<EntityTag>>,
    etag_end: NonNull<Arc<EntityTag>>,
    _marker: PhantomData<&'stor mut T>,
}

impl<'stor, T: 'stor> FilteredIterMut<'stor, T> {
    fn new(raw: &mut FilteredMut<'stor, T>) -> Self {
        // Safety: `FilteredMut` guarantees we're good to access those vecs.
        unsafe {
            let cont_range = raw.as_raw().query_res().as_ptr_range();
            let cont_cur = NonNull::new_unchecked(cont_range.start.cast_mut());
            let cont_end = NonNull::new_unchecked(cont_range.end.cast_mut());

            let etag_range = raw.as_raw().entity_tags().as_ptr_range();
            let etag_cur = NonNull::new_unchecked(etag_range.start.cast_mut());
            let etag_end = NonNull::new_unchecked(etag_range.end.cast_mut());

            Self {
                cont_cur,
                cont_end,
                etag_cur,
                etag_end,
                _marker: PhantomData,
            }
        }
    }

    #[inline]
    pub const fn len(&self) -> usize {
        let cur = self.cont_cur.as_ptr();
        let end = self.cont_end.as_ptr();
        unsafe { end.offset_from(cur) as usize }
    }

    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    unsafe fn create_item(
        mut cont: NonNull<Borrowed<NonNull<dyn ContainEntity>>>,
        etag: NonNull<Arc<EntityTag>>,
    ) -> EntityContainerRef<'stor, T> {
        let etag = &**etag.as_ref();
        let cont = cont.as_mut().as_mut();
        EntityContainerRef::new(etag, cont)
    }
}

impl<'stor, T: 'stor> Iterator for FilteredIterMut<'stor, T> {
    type Item = EntityContainerRef<'stor, T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cont_cur < self.cont_end {
            let getter = self.cont_cur;
            let etag = self.etag_cur;
            unsafe {
                self.cont_cur = self.cont_cur.add(1);
                self.etag_cur = self.etag_cur.add(1);
                Some(Self::create_item(getter, etag))
            }
        } else {
            None
        }
    }
}

impl<'stor, T: 'stor> iter::FusedIterator for FilteredIterMut<'stor, T> {}

impl<'stor, T: 'stor> ExactSizeIterator for FilteredIterMut<'stor, T> {
    fn len(&self) -> usize {
        Self::len(self)
    }
}

impl<'stor, T: 'stor> DoubleEndedIterator for FilteredIterMut<'stor, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.cont_cur < self.cont_end {
            unsafe {
                self.cont_end = self.cont_cur.sub(1);
                self.etag_end = self.etag_cur.sub(1);
                Some(Self::create_item(self.cont_end, self.etag_end))
            }
        } else {
            None
        }
    }
}

/// Component getter with entity tag.
///
/// * Component getter  
///   A component getter corresponds to a component array.
///   You can get each component inside component array via getter.
///   See [`Getter`] for more details.
///
/// * Entity tag  
///   Many entities may contain the same component type.
///   So, it's needed to know what entity this component belongs to.
///   Entity tag has entity identification such as entity name.
#[derive(Debug, Clone)]
pub struct TaggedGetter<'cont, Comp: 'cont> {
    getter: Getter<'cont, Comp>,
    etag: ManagedConstPtr<EntityTag>,
}

impl<Comp> TaggedGetter<'_, Comp> {
    pub fn entity_index(&self) -> EntityIndex {
        self.etag.index()
    }

    pub fn entity_name(&self) -> Option<&EntityName> {
        self.etag.get_name()
    }

    pub fn component_names(&self) -> &[&'static str] {
        self.etag.get_component_names()
    }
}

impl<'cont, Comp> Deref for TaggedGetter<'cont, Comp> {
    type Target = Getter<'cont, Comp>;

    fn deref(&self) -> &Self::Target {
        &self.getter
    }
}

impl<'cont, Comp> IntoIterator for TaggedGetter<'cont, Comp> {
    type Item = &'cont Comp;
    type IntoIter = FlatIter<'cont, Comp>;

    fn into_iter(self) -> Self::IntoIter {
        self.getter.into_iter()
    }
}

impl<'cont, Comp: Send + Sync> IntoParallelIterator for TaggedGetter<'cont, Comp> {
    type Item = &'cont Comp;
    type Iter = ParFlatIter<'cont, Comp>;

    fn into_par_iter(self) -> Self::Iter {
        self.getter.into_par_iter()
    }
}

/// Component getter with entity tag.
///
/// * Component getter  
///   A component getter corresponds to a component array.
///   You can get each component inside component array via getter.
///   See [`GetterMut`] for more details.
///
/// * Entity tag  
///   Many entities may contain the same component type.
///   So, it's needed to know what entity this component belongs to.
///   Entity tag has entity identification such as entity name.
#[derive(Debug)]
pub struct TaggedGetterMut<'cont, Comp: 'cont> {
    getter: GetterMut<'cont, Comp>,
    etag: ManagedConstPtr<EntityTag>,
}

impl<Comp> TaggedGetterMut<'_, Comp> {
    pub fn entity_index(&self) -> EntityIndex {
        self.etag.index()
    }

    pub fn entity_name(&self) -> Option<&EntityName> {
        self.etag.get_name()
    }

    pub fn component_names(&self) -> &[&'static str] {
        self.etag.get_component_names()
    }
}

impl<'cont, Comp> Deref for TaggedGetterMut<'cont, Comp> {
    type Target = GetterMut<'cont, Comp>;

    fn deref(&self) -> &Self::Target {
        &self.getter
    }
}

impl<Comp> DerefMut for TaggedGetterMut<'_, Comp> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.getter
    }
}

impl<'cont, Comp> IntoIterator for TaggedGetterMut<'cont, Comp> {
    type Item = &'cont mut Comp;
    type IntoIter = FlatIterMut<'cont, Comp>;

    fn into_iter(self) -> Self::IntoIter {
        self.getter.into_iter()
    }
}

impl<'cont, Comp: Send + Sync> IntoParallelIterator for TaggedGetterMut<'cont, Comp> {
    type Item = &'cont mut Comp;
    type Iter = ParFlatIterMut<'cont, Comp>;

    fn into_par_iter(self) -> Self::Iter {
        self.getter.into_par_iter()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    #[rustfmt::skip]
    fn test_my_ecs_macros_filter() {
        use crate as my_ecs;
        use crate::prelude::*;
        use crate::ecs::{
            sys::select::Select,
            ent::component::ComponentKey,
        };

        #[derive(Component)] struct Ca;
        #[derive(Component)] struct Cb;
        #[derive(Component)] struct Cc;
        #[derive(Component)] struct Cd;
        #[derive(Component)] struct Ce;
        #[derive(Component)] struct Cf;

        // Target only.
        filter!(F0, Target = Ca);
        let [all, any, none] = F0::all_any_none();
        assert_eq!(<F0 as Select>::Target::key(), Ca::key());
        assert!(all.is_empty());
        assert!(any.is_empty());
        assert!(none.is_empty());

        // All only.
        filter!(F1, All = Ca);
        let [all, any, none] = F1::all_any_none();
        validate_slice(&all, &[Ca::key()]);
        assert!(any.is_empty());
        assert!(none.is_empty());

        // Any only.
        filter!(F2, Any = Ca);
        let [all, any, none] = F2::all_any_none();
        assert!(all.is_empty());
        validate_slice(&any, &[Ca::key()]);
        assert!(none.is_empty());

        // None only.
        filter!(F3, None = Ca);
        let [all, any, none] = F3::all_any_none();
        assert!(all.is_empty());
        assert!(any.is_empty());
        validate_slice(&none, &[Ca::key()]);

        // All + Any + None.
        filter!(F4, All = (Ca, Cb), Any = (Cc, Cd), None = (Ce, Cf));
        let [all, any, none] = F4::all_any_none();
        validate_slice(&all, &[Ca::key(), Cb::key()]);
        validate_slice(&any, &[Cc::key(), Cd::key()]);
        validate_slice(&none, &[Ce::key(), Cf::key()]);

        // Target + All + Any + None.
        filter!(F5, Target = Ca, All = Cb, Any = Cc, None = Cd);
        let [all, any, none] = F5::all_any_none();
        assert_eq!(<F5 as Select>::Target::key(), Ca::key());
        validate_slice(&all, &[Cb::key()]);
        validate_slice(&any, &[Cc::key()]);
        validate_slice(&none, &[Cd::key()]);

        fn validate_slice(a: &[ComponentKey], b: &[ComponentKey]) {
            assert_eq!(a.len(), b.len());
            for (va, vb) in a.iter().zip(b) {
                assert_eq!(va, vb);
            }
        }
    }
}
