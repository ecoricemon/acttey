use super::{
    component::{Component, ComponentKey},
    entity::EntityTag,
};
use crate::ds::{
    borrow::Borrowed,
    get::{Getter, RawGetter},
};
use std::{
    any::{type_name, TypeId},
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr::NonNull,
    sync::{atomic::AtomicI32, Arc},
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
        impl $crate::ecs::filter::Filter for $id {
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

/// A filter to select slices of `Component`.
/// You should fill out this form of filter.
///
/// - `Target` is what `Component` you want. You will receive slices of this `Target`.
/// - `FilterAll` is a tuple of `Component`s to select entities that have all these `Component`s. Empty tuple means selecting all entities.
/// - `FilterAny` is a tuple of `Component`s to select entities that have any of these `Component`s. `Target` automatically belongs to this, so that empty tuple means selecting entities including `Target`.
/// - `FilterNone` is a tuple of `Component`s not to select entities that have any of these `Component`s. Empty tuple means selecting all entities.
pub trait Filter: 'static {
    type Target: Component;
    type All: Identify;
    type Any: Identify;
    type None: Identify;

    fn ids() -> [Box<[ComponentKey]>; 3] {
        let all = <Self::All as Identify>::ids();
        let all = all
            .iter()
            .map(|&ty| ComponentKey::new(ty))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let any = <Self::Any as Identify>::ids();
        let any = any
            .iter()
            .map(|&ty| ComponentKey::new(ty))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let none = <Self::None as Identify>::ids();
        let none = none
            .iter()
            .map(|&ty| ComponentKey::new(ty))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        [all, any, none]
    }

    fn key() -> FilterKey {
        FilterKey::new(TypeId::of::<Self>())
    }

    fn info<S>(info_stor: &mut S) -> Arc<FilterInfo>
    where
        S: StoreFilterInfo,
    {
        let key = Self::key();
        if let Some(info) = info_stor.get(&key) {
            Arc::clone(info)
        } else {
            let [all, any, none] = Self::ids();
            let info = Arc::new(FilterInfo {
                name: type_name::<Self>(),
                target: ComponentKey::new(TypeId::of::<Self::Target>()),
                all,
                any,
                none,
            });
            info_stor.insert(key, Arc::clone(&info));
            info
        }
    }
}

pub trait StoreFilterInfo {
    fn get(&self, key: &FilterKey) -> Option<&Arc<FilterInfo>>;
    fn insert(&mut self, key: FilterKey, info: Arc<FilterInfo>);
    fn remove(&mut self, key: &FilterKey) -> Option<Arc<FilterInfo>>;
}

/// [`TypeId`] for [`Filter`].
#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
#[repr(transparent)]
pub struct FilterKey(TypeId);

impl FilterKey {
    pub const fn new(ty: TypeId) -> Self {
        Self(ty)
    }

    pub const fn as_type(&self) -> &TypeId {
        &self.0
    }
}

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
        /* any including target */
        std::iter::once(self.target())
                .chain(self.any().iter())
                .any(&mut contains)
        /* all */ && self.all().iter().all(&mut contains)
        /* none */ && !self.none().iter().any(contains)
    }
}

/// A trait to get [`TypeId`]s of elements inside a tuple.
pub trait Identify {
    type Output;

    fn ids() -> Vec<TypeId>;
}

/// Implements the trait [`Identify`] for a anonymous tuple of [`Component`]s.
#[macro_export]
macro_rules! impl_identify {
    (0) => {const _: () = {
        use $crate::ecs::filter::Identify;
        use std::any::TypeId;

        impl Identify for () {
            type Output = [TypeId; 0];

            fn ids() -> Vec<TypeId> {
                vec![]
            }
        }
    };};
    (1, $id:ident) => {const _: () = {
        use $crate::ecs::{component::Component, filter::Identify};
        use std::any::TypeId;

        impl<$id: Component> Identify for $id {
            type Output = [TypeId; 1];

            fn ids() -> Vec<TypeId> {
                vec![TypeId::of::<$id>()]
            }
        }
    };};
    ($n:expr, $($id:ident),+) => {const _: () = {
        use $crate::ecs::{component::Component, filter::Identify};
        use std::any::TypeId;

        impl<$($id: Component),+> Identify for ( $($id),+ ) {
            type Output = [TypeId; $n];

            fn ids() -> Vec<TypeId> {
                vec![$( TypeId::of::<$id>() ),+]
            }
        }
    };};
}
impl_identify!(0);
impl_identify!(1, A);
impl_identify!(2, A, B);
impl_identify!(3, A, B, C);
impl_identify!(4, A, B, C, D);
impl_identify!(5, A, B, C, D, E);
impl_identify!(6, A, B, C, D, E, F);
impl_identify!(7, A, B, C, D, E, F, G);
impl_identify!(8, A, B, C, D, E, F, G, H);

/// Cached data by filtering.
#[derive(Debug)]
pub struct Filtered {
    /// [`EntityTag`] searched by the filter.
    ent_tags: Vec<EntityTag>,

    /// Column(Component) index searched by the filter.
    col_indices: Vec<usize>,

    /// Temporary buffer for the query result.
    /// Content will be replaced for every query, but we can reuse the capacity.
    /// Notice that this doesn't actually own [`Borrowed`] because this is just a temporary buffer.
    /// Real user, system, owns it and will drop it after using it.
    query_res: Vec<Borrowed<RawGetter, AtomicI32>>,
}

impl Filtered {
    pub const fn new(ent_tags: Vec<EntityTag>, col_indices: Vec<usize>) -> Self {
        Self {
            ent_tags,
            col_indices,
            query_res: Vec::new(),
        }
    }

    pub fn destructure(
        &mut self,
    ) -> (
        &Vec<EntityTag>,
        &Vec<usize>,
        &mut Vec<Borrowed<RawGetter, AtomicI32>>,
    ) {
        (&self.ent_tags, &self.col_indices, &mut self.query_res)
    }

    // By putting `etag` and `coli` in together, `Self::ent_tags` and `Self::col_indices` have the same length.
    pub fn add(&mut self, etag: EntityTag, coli: usize) {
        self.ent_tags.push(etag);
        self.col_indices.push(coli);
    }

    pub fn ent_tags(&self) -> &Vec<EntityTag> {
        &self.ent_tags
    }

    pub fn col_indices(&self) -> &Vec<usize> {
        &self.col_indices
    }

    pub fn query_res(&self) -> &Vec<Borrowed<RawGetter, AtomicI32>> {
        &self.query_res
    }

    pub fn query_res_mut(&mut self) -> &mut Vec<Borrowed<RawGetter, AtomicI32>> {
        &mut self.query_res
    }

    /// Retrieve an iterator that traverses over entity and column index pair.
    pub fn iter_index_pair<'a>(
        ent_tags: &'a [EntityTag],
        col_indices: &'a [usize],
    ) -> impl Iterator<Item = (usize, usize)> + 'a {
        // Self::add() guarantees that ent_tags and col_indices have the same length.
        ent_tags
            .iter()
            .map(|etag| etag.index())
            .zip(col_indices.iter().cloned())
    }
}

#[derive(Debug, Clone)]
pub struct FilteredIter<T> {
    /// Pointer to a slice of [`Borrowed`] of the [`RawGetter`].
    /// `RawGetter` is an access path to the filtered entity columns.
    /// `Borrowed` has a role of RAII.
    /// When the iterator is dropped, `Borrowed`s are dropped too, then it stops borrowing.
    ptr_getter: NonNull<Borrowed<RawGetter, AtomicI32>>,

    /// Pointer to a slice of [`EntityTag`].
    ptr_etag: NonNull<EntityTag>,

    /// Length of two slices above.
    len: usize,

    /// Current position on the slice.
    cur: usize,

    _marker: PhantomData<T>,
}

impl<T> FilteredIter<T> {
    pub fn new(getters: &[Borrowed<RawGetter, AtomicI32>], etags: &[EntityTag]) -> Self {
        debug_assert_eq!(getters.len(), etags.len());

        let ptr_getter = unsafe { NonNull::new_unchecked(getters.as_ptr().cast_mut()) };
        let ptr_etag = unsafe { NonNull::new_unchecked(etags.as_ptr().cast_mut()) };
        let len = getters.len();

        Self {
            ptr_getter,
            ptr_etag,
            len,
            cur: 0,
            _marker: PhantomData,
        }
    }

    pub fn empty() -> Self {
        Self {
            ptr_getter: NonNull::dangling(),
            ptr_etag: NonNull::dangling(),
            len: 0,
            cur: 0,
            _marker: PhantomData,
        }
    }
}

impl<T> Iterator for FilteredIter<T> {
    type Item = TaggedGetter<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cur < self.len {
            unsafe {
                let borrowed = self.ptr_getter.as_ptr().add(self.cur);
                let raw_getter = (*borrowed).clone();
                let etag = self.ptr_etag.as_ptr().add(self.cur);
                let etag = NonNull::new(etag).unwrap();
                self.cur += 1;
                Some(TaggedGetter {
                    getter: Getter::new(raw_getter),
                    etag,
                })
            }
        } else {
            None
        }
    }
}

impl<T> ExactSizeIterator for FilteredIter<T> {
    fn len(&self) -> usize {
        self.len - self.cur
    }
}

impl<T> Drop for FilteredIter<T> {
    fn drop(&mut self) {
        for i in 0..self.len {
            unsafe {
                let ptr = self.ptr_getter.as_ptr().add(i);
                ptr.drop_in_place();
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct FilteredIterMut<T> {
    /// Pointer to a slice of [`Borrowed`] of the [`RawGetter`].
    /// `RawGetter` is an access path to the filtered entity columns.
    /// `Borrowed` has a role of something similar to RAII.
    /// When the iterator is dropped, `Borrowed`s are dropped too, and it stops borrowing.
    ptr_getter: NonNull<Borrowed<RawGetter, AtomicI32>>,

    /// Pointer to a slice of [`EntityTag`].
    ptr_etag: NonNull<EntityTag>,

    /// Length of two slices above.
    len: usize,

    /// Current position on the slice.
    cur: usize,

    _marker: PhantomData<T>,
}

impl<T> FilteredIterMut<T> {
    pub fn new(getters: &mut [Borrowed<RawGetter, AtomicI32>], etags: &[EntityTag]) -> Self {
        debug_assert_eq!(getters.len(), etags.len());

        let ptr_getter = unsafe { NonNull::new_unchecked(getters.as_mut_ptr()) };
        let ptr_etag = unsafe { NonNull::new_unchecked(etags.as_ptr().cast_mut()) };
        let len = getters.len();

        Self {
            ptr_getter,
            ptr_etag,
            len,
            cur: 0,
            _marker: PhantomData,
        }
    }

    pub fn empty() -> Self {
        Self {
            ptr_getter: NonNull::dangling(),
            ptr_etag: NonNull::dangling(),
            len: 0,
            cur: 0,
            _marker: PhantomData,
        }
    }
}

impl<T> Iterator for FilteredIterMut<T> {
    type Item = TaggedGetter<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cur < self.len {
            unsafe {
                let borrowed = self.ptr_getter.as_ptr().add(self.cur);
                let raw_getter = (*borrowed).clone();
                let etag = self.ptr_etag.as_ptr().add(self.cur);
                let etag = NonNull::new(etag).unwrap();
                self.cur += 1;
                Some(TaggedGetter {
                    getter: Getter::new(raw_getter),
                    etag,
                })
            }
        } else {
            None
        }
    }
}

impl<T> ExactSizeIterator for FilteredIterMut<T> {
    fn len(&self) -> usize {
        self.len - self.cur
    }
}

impl<T> Drop for FilteredIterMut<T> {
    fn drop(&mut self) {
        for i in 0..self.len {
            unsafe {
                let ptr = self.ptr_getter.as_ptr().add(i);
                ptr.drop_in_place();
            }
        }
    }
}

#[derive(Debug)]
pub struct TaggedGetter<T> {
    pub getter: Getter<T>,
    pub etag: NonNull<EntityTag>,
}

impl<T> Deref for TaggedGetter<T> {
    type Target = Getter<T>;

    fn deref(&self) -> &Self::Target {
        &self.getter
    }
}

impl<T> DerefMut for TaggedGetter<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.getter
    }
}
