use super::component::{ComponentKey, Components};
use crate::{
    ds::{BorrowResult, RawGetter, TypeInfo},
    util::{macros::impl_from_for_enum, With},
};
use std::{any::TypeId, fmt, mem, mem::MaybeUninit, ops::Deref, ptr::NonNull, sync::Arc};

/// A set of components.
///
/// Implementing this trait is not mandatory, but by doing so, the crate can
/// provide you more easy to use APIs. Plus, there is a derive macro that have
/// the same name, which help you implement this trait. As a consequence, it's
/// encouraged to implement this trait by the derive macro about entity types
/// that you know.
#[allow(private_interfaces)]
pub trait Entity: Components + Send + 'static {
    type Ref<'cont>;
    type Mut<'cont>;

    /// Offsets in bytes of each field.
    ///
    /// See example below.
    /// ```ignore
    /// struct Entity {
    ///     a: i32, // offset may be 0
    ///     b: i8,  // offset may be 6
    ///     c: i16, // offset may be 4
    /// }
    ///
    /// // Implementors must define offsets like this.
    /// // OFFSETS_BY_FIELD_INDEX = &[0, 6, 4]
    /// ```
    ///
    /// # Safety
    ///
    /// Must be implemented correctly. Other methods depend on this offset with
    /// unsafe blocks.
    const OFFSETS_BY_FIELD_INDEX: &'static [usize];

    /// Turns field index into column index.
    ///
    /// This function could be called frequently, so that it's recommended to
    /// cache the mapping from field index to column index in a way.
    ///
    /// # Field index
    ///
    /// Field index is an index assigned to a field in the order it is declared.
    ///
    /// # Column index
    ///
    /// Column index is a terminology used in [`ContainEntity`]. In short, it is
    /// an index sorted by [`ComponentKey`].
    ///
    /// # Safety
    ///
    /// Must be implemented correctly. Other methods depend on this offset with
    /// unsafe blocks.
    fn field_to_column_index(fi: usize) -> usize;

    /// Turns column index into field index.
    ///
    /// This function would be called infrequently, so that simple
    /// implementations would be good enough.
    fn column_to_field_index(ci: usize) -> usize {
        // Safety: Field index matches column index 1 by 1.
        unsafe {
            (0..Self::num_components())
                .find(|&fi| Self::field_to_column_index(fi) == ci)
                .unwrap_unchecked()
        }
    }

    /// Returns a struct holding shared references to components that belong
    /// to an entity for the given value index.
    ///
    /// Note, however, that entity is not stored as it is. It is split up into
    /// its components then stored in each component container. That means
    /// collecting those references like this function is inefficient because it
    /// requires random access to memory.
    ///
    /// `derive(Entity)` macro gives us a similar struct to `Self`. You can
    /// access each field via dot operator. Plus, it implements [`Debug`], so
    /// that you can see how the entity looks like. But it will show some
    /// components only that implement `Debug`. See examples below.
    ///
    /// # Panics
    ///
    /// Panics if the given value index is out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    /// use std::hash::RandomState;
    ///
    /// #[derive(Entity, Debug, PartialEq)]
    /// struct Ea {
    ///     ca: Ca,
    ///     cb: Cb,
    /// }
    /// #[derive(Component, Debug, PartialEq)]
    /// struct Ca(i32);
    /// #[derive(Component, Debug, PartialEq)]
    /// struct Cb(String);
    ///
    /// let mut cont: SparseSet<RandomState> = SparseSet::new();
    /// Ea::register_to(&mut cont);
    /// Ea { ca: Ca(42), cb: Cb("cb".to_owned()) }.move_to(&mut cont);
    ///
    /// let e = Ea::get_ref_from(&cont, 0);
    /// println!("{e:?}");
    /// assert_eq!(e.ca, &Ca(42));
    /// assert_eq!(e.cb, &Cb("cb".to_owned()));
    /// ```
    fn get_ref_from<Cont: ContainEntity + ?Sized>(cont: &Cont, vi: usize) -> Self::Ref<'_>;

    /// Returns a struct holding mutable references to components that belong
    /// to an entity for the given value index.
    ///
    /// Note, however, that entity is not stored as it is. It is split up into
    /// its components then stored in each component container. That means
    /// collecting those references like this function is inefficient because it
    /// requires random access to memory.
    ///
    /// `derive(Entity)` macro gives us a similar struct to `Self`. You can
    /// access each field via dot operator. Plus, it implements [`Debug`], so
    /// that you can see how the entity looks like. But it will show some
    /// components only that implement `Debug`. See examples below.
    ///
    /// # Panics
    ///
    /// Panics if the given value index is out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    /// use std::hash::RandomState;
    ///
    /// #[derive(Entity, Debug, PartialEq)]
    /// struct Ea {
    ///     ca: Ca,
    ///     cb: Cb,
    /// }
    /// #[derive(Component, Debug, PartialEq)]
    /// struct Ca(i32);
    /// #[derive(Component, Debug, PartialEq)]
    /// struct Cb(String);
    ///
    /// let mut cont: SparseSet<RandomState> = SparseSet::new();
    /// Ea::register_to(&mut cont);
    /// Ea { ca: Ca(1), cb: Cb("2".to_owned()) }.move_to(&mut cont);
    ///
    /// let e = Ea::get_mut_from(&mut cont, 0);
    /// println!("{e:?}");
    /// assert_eq!(e.ca, &Ca(1));
    /// assert_eq!(e.cb, &Cb("2".to_owned()));
    ///
    /// *e.ca = Ca(3);
    /// *e.cb = Cb("4".to_owned());
    ///
    /// let e = Ea::get_ref_from(&cont, 0);
    /// assert_eq!(e.ca, &Ca(3));
    /// assert_eq!(e.cb, &Cb("4".to_owned()));
    /// ```
    fn get_mut_from<Cont: ContainEntity + ?Sized>(cont: &mut Cont, vi: usize) -> Self::Mut<'_>;

    /// Returns entity key of the entity type.
    //
    // TODO: Arc is not shared with the Arc inside of entity container.
    // But we need to generate sorted component keys as an entity key.
    #[doc(hidden)]
    fn key() -> EntityKey {
        let ckeys: Arc<[ComponentKey]> = (0..Self::num_components())
            .map(|ci| {
                let fi = Self::column_to_field_index(ci);
                Self::keys().as_ref()[fi]
            })
            .collect();
        EntityKey::Ckeys(ckeys)
    }

    /// Returns number of components.
    fn num_components() -> usize {
        Self::LEN
    }

    /// Returns a pointer to a component in this entity for the given field
    /// index.
    ///
    /// # Panics
    ///
    /// Panics if the given index is out of bounds.
    fn component_ptr(&self, fi: usize) -> NonNull<u8> {
        let base_ptr = (self as *const Self as *const u8).cast_mut();
        // Safety: Calculated from OFFSETS_BY_FIELD_INDEX.
        unsafe {
            let ptr = base_ptr.add(Self::OFFSETS_BY_FIELD_INDEX[fi]);
            NonNull::new_unchecked(ptr)
        }
    }

    /// Registers components in this entity to an entity container.
    ///
    /// # Panics
    ///
    /// Panics if the given entity container has registered columns in it.
    fn register_to<Cont: ContainEntity + ?Sized>(cont: &mut Cont) {
        assert_eq!(cont.num_columns(), 0);

        // Registers component column in the sorted order by component key.
        for ci in 0..Self::num_components() {
            let fi = Self::column_to_field_index(ci);
            let tinfo = Self::infos().as_ref()[fi];
            cont.add_column(tinfo);
        }
    }

    /// Moves the entity to an entity container then returns row index to the
    /// moved entity.
    ///
    /// # How to move
    ///
    /// Use [`AddEntity::begin_add_row`], [`AddEntity::add_value`], and
    /// [`AddEntity::end_add_row`], then forget self.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    /// use std::hash::RandomState;
    ///
    /// #[derive(Entity, Debug, PartialEq)]
    /// struct Ea {
    ///     ca: Ca,
    ///     cb: Cb,
    /// }
    /// #[derive(Component, Debug, PartialEq)]
    /// struct Ca(i32);
    /// #[derive(Component, Debug, PartialEq)]
    /// struct Cb(String);
    ///
    /// let mut cont: SparseSet<RandomState> = SparseSet::new();
    /// Ea::register_to(&mut cont);
    /// Ea { ca: Ca(42), cb: Cb("cb".to_owned()) }.move_to(&mut cont);
    /// assert_eq!(cont.len(), 1);
    /// ```
    fn move_to<Cont: ContainEntity + ?Sized>(self, cont: &mut Cont) -> usize
    where
        Self: Sized,
    {
        cont.begin_add_row();

        for fi in 0..Self::num_components() {
            let ci = Self::field_to_column_index(fi);
            // Safety:
            // - Column index and value pointer are gotten client impl.
            // - We're going to forget `self`.
            unsafe { cont.add_value(ci, self.component_ptr(fi)) };
        }

        #[allow(clippy::forget_non_drop)]
        mem::forget(self);

        // Safety: Inserted all columns.
        unsafe { cont.end_add_row() }
    }

    /// Removes an entity for the given value index from an entity container
    /// then returns the entity.
    ///
    /// See [`ContainEntity`] document when you need to know what value index
    /// is.
    ///
    /// # Panics
    ///
    /// Panics if the given value index is out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    /// use std::hash::RandomState;
    ///
    /// #[derive(Entity, Debug, PartialEq)]
    /// struct Ea {
    ///     ca: Ca,
    ///     cb: Cb,
    /// }
    /// #[derive(Component, Debug, PartialEq)]
    /// struct Ca(i32);
    /// #[derive(Component, Debug, PartialEq)]
    /// struct Cb(String);
    ///
    /// let mut cont: SparseSet<RandomState> = SparseSet::new();
    /// Ea::register_to(&mut cont);
    /// Ea { ca: Ca(42), cb: Cb("cb".to_owned()) }.move_to(&mut cont);
    /// let e = Ea::take_from(&mut cont, 0);
    /// assert_eq!(e, Ea { ca: Ca(42), cb: Cb("cb".to_owned()) });
    /// ```
    fn take_from<Cont: ContainEntity + ?Sized>(cont: &mut Cont, vi: usize) -> Self
    where
        Self: Sized,
    {
        assert!(vi < cont.len());

        let mut this: MaybeUninit<Self> = MaybeUninit::uninit();
        let base_ptr = this.as_mut_ptr() as *mut u8;

        unsafe {
            cont.begin_remove_row_by_value_index(vi);
            for fi in 0..Self::num_components() {
                let comp_ptr = base_ptr.add(Self::OFFSETS_BY_FIELD_INDEX[fi]);
                let comp_ptr = NonNull::new_unchecked(comp_ptr);
                let ci = Self::field_to_column_index(fi);
                cont.remove_value_by_value_index(ci, vi, comp_ptr);
            }
            cont.end_remove_row_by_value_index(vi);

            this.assume_init()
        }
    }
}

/// A trait for collecting heterogeneous component types.
///
/// In this trait, each component type is gathered in each component column and
/// all columns have the same length so that it looks like 2d matrix.
///
/// When it comes to in & out types, this trait has intentionally raw pointer
/// parameters not to use generic for object safety. So that you can hold
/// various sorts of entity container in a single variable.
///
/// # Index system
///
/// There are three index systems for this trait.
///
/// The first one is 'column index' which is for pointing a certain component
/// column. When you add or remove component column from an entity container,
/// you will get or need this column index. Column indices starts with 0 and
/// increases by 1 as you put in a component column. To avoid confusion, column
/// must be added in a sorted order by [`ComponentKey`].
///
/// The second and third index systems are related to pointing a certain entity
/// in an entity container. In other words, you need one of those index systems
/// when you need access to a single entity.
///
/// Second one is 'row index' which is a kind of outer index for each entity.
/// You will get this row index when you put your entity in an entity container.
/// Also, you can remove an entity using the row index. Row indices may not be
/// in order and not consecutive.
///
/// The last one is 'value index' which is inner index for each entity. In
/// contrast to former one, value indices are in order and consecutive like
/// indices on a slice. Plus, all component columns follows the same value
/// indices in an entity container.
///
/// Take a look at example below.
///
/// ```text
///              column index      0        1
/// --------------------------------------------
/// | row index | value index | comp_a | comp_b |
/// |     0     |      0      |    .   |    .   |
/// |     4     |      1      |    .   |    .   |
/// |     2     |      2      |    .   |    .   |
/// ```
///
/// # Examples
///
/// ```
/// use my_ecs::prelude::*;
/// use std::{hash::RandomState, ptr::NonNull};
///
/// #[derive(Entity)]
/// struct Entity {
///     a: Ca,
///     b: Cb,
/// }
/// #[derive(Component)]
/// struct Ca(i32);
/// #[derive(Component)]
/// struct Cb(i32);
///
/// let mut cont: SparseSet<RandomState> = SparseSet::new();
///
/// // Adds component columns.
/// let ci_a = cont.add_column(tinfo!(Ca)).unwrap();
/// let ci_b = cont.add_column(tinfo!(Cb)).unwrap();
///
/// // Adds component values for the entity.
/// cont.begin_add_row();
/// let ri = unsafe {
///     let ptr = NonNull::new(&mut Ca(4) as *mut Ca as *mut u8).unwrap();
///     cont.add_value(ci_a, ptr);
///     let ptr = NonNull::new(&mut Cb(2) as *mut Cb as *mut u8).unwrap();
///     cont.add_value(ci_b, ptr);
///     cont.end_add_row()
/// };
/// assert_eq!(cont.len(), 1);
///
/// // Borrows component columns and test them if they are as we expected.
/// let col_a = cont.borrow_column(ci_a).unwrap();
/// let col_b = cont.borrow_column(ci_b).unwrap();
/// unsafe {
///     let ptr = col_a.get(0).unwrap();
///     assert_eq!(*ptr.as_ref(), 4);
///     let ptr = col_b.get(0).unwrap();
///     assert_eq!(*ptr.as_ref(), 2);
/// }
/// drop(col_a);
/// drop(col_b);
///
/// // Removes the entity we just put.
/// let is_removed = cont.remove_row(ri);
/// assert!(is_removed);
/// assert_eq!(cont.len(), 0);
/// ```
//
// Must object safe.
#[allow(clippy::len_without_is_empty)]
pub trait ContainEntity: RegisterComponent + BorrowComponent + AddEntity {
    /// Creates a new entity container that has the same component types without
    /// component values.
    ///
    /// In other words, the copied container doesn't have any entities in it. So
    /// it's empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    /// use std::{hash::RandomState, ptr::NonNull, any::TypeId};
    ///
    /// #[derive(Entity)]
    /// struct Entity {
    ///     a: Ca,
    /// }
    /// #[derive(Component)]
    /// struct Ca(i32);
    ///
    /// let mut cont: SparseSet<RandomState> = SparseSet::new();
    /// let ci = cont.add_column(tinfo!(Ca)).unwrap();
    ///
    /// // Adds component values for an entity.
    /// cont.begin_add_row();
    /// unsafe {
    ///     let ptr = NonNull::new(&mut Ca(0) as *mut Ca as *mut u8).unwrap();
    ///     cont.add_value(ci, ptr);
    ///     cont.end_add_row();
    /// }
    /// assert_eq!(cont.len(), 1);
    ///
    /// let twin = cont.create_twin();
    /// assert_eq!(twin.len(), 0);
    /// assert!(twin.contains_column(&TypeId::of::<Ca>()));
    /// ```
    fn create_twin(&self) -> Box<dyn ContainEntity>;

    /// Retrieves an entity for the given component column index and row index
    /// from the entity container.
    ///
    /// If one of two indices is out of bounds, returns `None`.
    fn get_item_mut(&mut self, ci: usize, ri: usize) -> Option<NonNull<u8>>;

    /// Returns number of entities in the entity container.
    ///
    /// # Examples
    ///
    /// See [`ContainEntity`] document.
    fn len(&self) -> usize;

    /// Returns capacity of the entity container.
    ///
    /// But if entity container doesn't support getting capacity, it returns
    /// number of entities instead.
    ///
    /// # Examples
    ///
    /// See [`ContainEntity::reserve`] document.
    fn capacity(&self) -> usize;

    /// May reserve at least `additional` extra capacity.
    ///
    /// This method doesn't guarantee definite extension of capacity. It depends
    /// on implementations.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    /// use std::{hash::RandomState, ptr::NonNull, any::TypeId};
    ///
    /// #[derive(Entity)]
    /// struct Entity {
    ///     a: Ca,
    /// }
    /// #[derive(Component)]
    /// struct Ca(i32);
    ///
    /// // SparseSet supports capacity.
    /// let mut cont: SparseSet<RandomState> = SparseSet::new();
    /// cont.add_column(tinfo!(Ca)).unwrap();
    /// assert_eq!(cont.capacity(), 0);
    ///
    /// cont.reserve(10);
    /// assert!(cont.capacity() >= 10);
    ///
    /// cont.shrink_to_fit();
    /// assert_eq!(cont.capacity(), 0);
    /// ```
    fn reserve(&mut self, additional: usize);

    /// May shrink capacity of the entity container as much as possible.
    ///
    /// This method doesn't guarantee definite removal of extra capacity. It
    /// depends on implementations.
    ///
    /// # Examples
    ///
    /// See [`ContainEntity::reserve`] document.
    fn shrink_to_fit(&mut self);

    /// # Panics
    ///
    /// Panics if
    /// - Column index `ci` is out of bounds.
    /// - Column type is not [`Clone`].
    ///
    /// # Safety
    ///
    /// Undefined behavior if
    /// - Pointer to a value `val_ptr` is not a valid pointer for the column's
    ///   type.
    /// - After calling this method on columns in an entity container, any of
    ///   columns doesn't have the same length.
    unsafe fn resize_column(&mut self, ci: usize, new_len: usize, val_ptr: NonNull<u8>);
}

/// A trait for adding or removing component types from an entity container.
///
/// See [`ContainEntity`] for more information.
//
// Must object safe.
pub trait RegisterComponent {
    /// Adds a component column to the entity container then returns column
    /// index.
    ///
    /// But the entity container failed to add new entity for some reason,
    /// returns `None`. You can get [`TypeInfo`] from any static types using
    /// [`tinfo`](crate::tinfo) macro.
    ///
    /// Column index is guaranteed to be increased one by one from zero
    /// whenever you call this method, which means you can get column index
    /// from order you added components.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    /// use std::{hash::RandomState, any::TypeId};
    ///
    /// #[derive(Component)]
    /// struct Ca;
    ///
    /// let mut cont: SparseSet<RandomState> = SparseSet::new();
    ///
    /// cont.add_column(tinfo!(Ca));
    /// assert!(cont.contains_column(&TypeId::of::<Ca>()));
    ///
    /// // Duplicated component columns are not allowed.
    /// let ret = cont.add_column(tinfo!(Ca));
    /// assert!(ret.is_none());
    /// ```
    fn add_column(&mut self, tinfo: TypeInfo) -> Option<usize>;

    /// Removes the component column from the entity container.
    ///
    /// If removal is successful, returns [`TypeInfo`] of the removed component
    /// column. But if the entity container doesn't have component column for
    /// the given column index, returns `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    /// use std::{hash::RandomState, any::TypeId};
    ///
    /// #[derive(Component)]
    /// struct Ca;
    ///
    /// let mut cont: SparseSet<RandomState> = SparseSet::new();
    ///
    /// let col_idx = cont.add_column(tinfo!(Ca)).unwrap();
    /// assert!(cont.contains_column(&TypeId::of::<Ca>()));
    ///
    /// let tinfo = cont.remove_column(col_idx);
    /// assert_eq!(tinfo, Some(tinfo!(Ca)));
    /// assert!(!cont.contains_column(&TypeId::of::<Ca>()));
    /// ```
    fn remove_column(&mut self, ci: usize) -> Option<TypeInfo>;

    /// Retrieves column index for the given component type.
    ///
    /// If there is not the component column in the entity container, returns
    /// `None`
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    /// use std::{hash::RandomState, any::TypeId};
    ///
    /// #[derive(Component)]
    /// struct Ca;
    /// #[derive(Component)]
    /// struct Cb;
    ///
    /// let mut cont: SparseSet<RandomState> = SparseSet::new();
    ///
    /// let col_idx = cont.add_column(tinfo!(Ca)).unwrap();
    /// assert_eq!(cont.get_column_index(&TypeId::of::<Ca>()).unwrap(), col_idx);
    ///
    /// assert!(cont.get_column_index(&TypeId::of::<Cb>()).is_none());
    /// ```
    fn get_column_index(&self, ty: &TypeId) -> Option<usize>;

    /// Retrieves [`TypeInfo`] of the component for the given column index.
    ///
    /// If there is not the component column in the entity container, returns
    /// `None`
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    /// use std::hash::RandomState;
    ///
    /// #[derive(Component)]
    /// struct Ca;
    ///
    /// let mut cont: SparseSet<RandomState> = SparseSet::new();
    ///
    /// let col_idx = cont.add_column(tinfo!(Ca)).unwrap();
    /// let tinfo = cont.get_column_info(col_idx);
    /// assert_eq!(tinfo, Some(&tinfo!(Ca)));
    /// ```
    fn get_column_info(&self, ci: usize) -> Option<&TypeInfo>;

    /// Retrieves number of component columns in the entity container.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    /// use std::hash::RandomState;
    ///
    /// #[derive(Component)]
    /// struct Ca;
    /// #[derive(Component)]
    /// struct Cb;
    ///
    /// let mut cont: SparseSet<RandomState> = SparseSet::new();
    /// assert_eq!(cont.num_columns(), 0);
    ///
    /// cont.add_column(tinfo!(Ca));
    /// assert_eq!(cont.num_columns(), 1);
    /// cont.add_column(tinfo!(Cb));
    /// assert_eq!(cont.num_columns(), 2);
    /// ```
    fn num_columns(&self) -> usize;

    /// Returns true if the entity container contains given component type.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    /// use std::{hash::RandomState, any::TypeId};
    ///
    /// #[derive(Component)]
    /// struct Ca;
    ///
    /// let mut cont: SparseSet<RandomState> = SparseSet::new();
    /// assert!(!cont.contains_column(&TypeId::of::<Ca>()));
    ///
    /// cont.add_column(tinfo!(Ca));
    /// assert!(cont.contains_column(&TypeId::of::<Ca>()));
    /// ```
    fn contains_column(&self, ty: &TypeId) -> bool {
        self.get_column_index(ty).is_some()
    }
}

/// A trait for borrowing a component column from an entity container.
///
/// See [`ContainEntity`] for more information.
//
// Must object safe.
pub trait BorrowComponent {
    /// Borrows component column for the given column index.
    ///
    /// If borrow is successful, returns [`RawGetter`] of the component column.
    /// Otherwise, returns [`BorrowError`](crate::ds::BorrowError).
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    /// use std::{hash::RandomState, any::TypeId};
    ///
    /// #[derive(Component)]
    /// struct Ca;
    ///
    /// let mut cont: SparseSet<RandomState> = SparseSet::new();
    /// cont.add_column(tinfo!(Ca));
    ///
    /// let col_idx = cont.get_column_index(&TypeId::of::<Ca>()).unwrap();
    /// let getter = cont.borrow_column(col_idx).unwrap();
    /// ```
    fn borrow_column(&self, ci: usize) -> BorrowResult<RawGetter>;

    /// Borrows component column mutably for the given column index.
    ///
    /// If borrow is successful, returns [`RawGetter`] of the component column.
    /// Otherwise, returns [`BorrowError`](crate::ds::BorrowError).
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    /// use std::{hash::RandomState, any::TypeId};
    ///
    /// #[derive(Component)]
    /// struct Ca;
    ///
    /// let mut cont: SparseSet<RandomState> = SparseSet::new();
    /// cont.add_column(tinfo!(Ca));
    ///
    /// let col_idx = cont.get_column_index(&TypeId::of::<Ca>()).unwrap();
    /// let getter = cont.borrow_column_mut(col_idx).unwrap();
    /// ```
    fn borrow_column_mut(&mut self, ci: usize) -> BorrowResult<RawGetter>;

    /// Retrieves component column pointer for the given column index.
    ///
    /// If there is not the component column in the entity container, returns
    /// `None`
    ///
    /// # Safety
    ///
    /// Undefined behavior if exclusive borrow happened before.
    unsafe fn get_column(&self, ci: usize) -> Option<NonNull<u8>>;
}

/// A trait for adding or removing component values from an entity container.
///
/// See [`ContainEntity`] for more information.
//
// Must object safe.
pub trait AddEntity: RegisterComponent {
    /// Converts given row index to value index.
    fn to_value_index(&self, ri: usize) -> Option<usize>;

    /// Starts inserting component values of an entity to the entity container.
    ///
    /// # Panics
    ///
    /// May panic if any component columns were borrowed and not returned yet.
    /// But implementations must guarantee that one of
    /// [`AddEntity::begin_add_row`], [`AddEntity::add_value`], and
    /// [`AddEntity::end_add_row`] panics if borrowed component column is
    /// detected.
    ///
    /// # Safety
    ///
    /// This method must be followed by [`AddEntity::add_value`] and
    /// [`AddEntity::end_add_row`].
    ///
    /// # Examples
    ///
    /// See [`ContainEntity`] document.
    fn begin_add_row(&mut self);

    /// Inserts a component value of an entity in the entity container.
    ///
    /// This method creates a bitwise copy of the value, so that caller must not
    /// access the value in any ways including its drop procedure after calling
    /// this method.
    ///
    /// # Panics
    ///
    /// See [`AddEntity::begin_add_row`] document.
    ///
    /// # Safety
    ///
    /// Caller must guarantee
    /// - This method must be called between [`AddEntity::begin_add_row`] and
    ///   [`AddEntity::end_add_row`] for all components.
    /// - Column index `ci` is not out of bounds.
    /// - Value pointer `val_ptr` is valid for the component column type.
    /// - Value must not be accessed after calling this method even `drop()`.
    ///
    /// # Examples
    ///
    /// See [`ContainEntity`] document.
    unsafe fn add_value(&mut self, ci: usize, val_ptr: NonNull<u8>);

    /// Finishes inserting component values of an entity in the entity
    /// container then returns row index to the inserted entity.
    ///
    /// # Panics
    ///
    /// See [`AddEntity::begin_add_row`] document.
    ///
    /// # Safety
    ///
    /// Caller must have called [`AddEntity::begin_add_row`] once and
    /// [`AddEntity::add_value`] number of component columns times before
    /// calling to this method for just one entity.
    ///
    /// # Examples
    ///
    /// See [`ContainEntity`] document.
    unsafe fn end_add_row(&mut self) -> usize;

    /// Retrieves a pointer to a component value for the given column and value
    /// indices.
    ///
    /// If the given index is out of bounds, returns None.
    fn value_ptr_by_value_index(&self, ci: usize, vi: usize) -> Option<NonNull<u8>>;

    /// Removes an entity for the given row index from the entity container.
    ///
    /// If removal is successful, returns true. Otherwise, for instance index
    /// is out of bounds, returns false.
    ///
    /// # Examples
    ///
    /// See [`ContainEntity`] document.
    fn remove_row(&mut self, ri: usize) -> bool {
        if let Some(vi) = self.to_value_index(ri) {
            self.remove_row_by_value_index(vi);
            true
        } else {
            false
        }
    }

    /// Removes an entity for the given value index from the entity container.
    ///
    /// # Panics
    ///
    /// Panics if the given value index is out of bounds.
    fn remove_row_by_value_index(&mut self, vi: usize) {
        unsafe {
            self.begin_remove_row_by_value_index(vi);
            for ci in 0..self.num_columns() {
                self.drop_value_by_value_index(ci, vi);
            }
            self.end_remove_row_by_value_index(vi);
        }
    }

    /// Starts removing component values of an entity from the entity container.
    ///
    /// # Panics
    ///
    /// May panic if any component columns were borrowed and not returned yet.
    /// But implementations must guarantee that one of methods below panic if
    /// borrowed component column is detected.
    /// - [`AddEntity::begin_remove_row_by_value_index`]
    /// - [`AddEntity::remove_value_by_value_index`]
    /// - [`AddEntity::drop_value_by_value_index`]
    /// - [`AddEntity::forget_value_by_value_index`]
    /// - [`AddEntity::end_remove_row_by_value_index`]
    ///
    /// # Safety
    ///
    /// Caller must guarantee
    /// - This method must be followed by
    ///   [`AddEntity::remove_value_by_value_index`] and
    ///   [`AddEntity::end_remove_row_by_value_index`].
    ///   [`AddEntity::remove_value_by_value_index`] can be replaced by
    ///   [`AddEntity::drop_value_by_value_index`] or
    ///   [`AddEntity::forget_value_by_value_index`].
    /// - Value index `vi` is not out of bounds.
    unsafe fn begin_remove_row_by_value_index(&mut self, vi: usize);

    /// Removes a component value of an entity in the entity container then
    /// write it to the given buffer.
    ///
    /// Caller must choose one of methods below to take a component value out.
    /// - [`AddEntity::remove_value_by_value_index`].
    /// - [`AddEntity::drop_value_by_value_index`].
    /// - [`AddEntity::forget_value_by_value_index`].
    ///
    /// # Panics
    ///
    /// See [`AddEntity::begin_remove_row_by_value_index`] document.
    ///
    /// # Safety
    ///
    /// Caller must guarantee
    /// - This method must be called between
    ///   [`AddEntity::remove_value_by_value_index`] and
    ///   [`AddEntity::end_remove_row_by_value_index`] for all components.
    /// - Column index `ci` is not out of bounds.
    /// - Value index `vi` is not out of bounds.
    /// - Buffer `buf` has sufficient capacity for the component value.
    unsafe fn remove_value_by_value_index(&mut self, ci: usize, vi: usize, buf: NonNull<u8>);

    /// Drops a component value of an entity in the entity container.
    ///
    /// Caller must choose one of methods below to take a component value out.
    /// - [`AddEntity::remove_value_by_value_index`].
    /// - [`AddEntity::drop_value_by_value_index`].
    /// - [`AddEntity::forget_value_by_value_index`].
    ///
    /// # Panics
    ///
    /// See [`AddEntity::begin_remove_row_by_value_index`] document.
    ///
    /// # Safety
    ///
    /// Caller must guarantee
    /// - This method must be called between
    ///   [`AddEntity::remove_value_by_value_index`] and
    ///   [`AddEntity::end_remove_row_by_value_index`] for all components.
    /// - Column index `ci` is not out of bounds.
    /// - Value index `vi` is not out of bounds.
    unsafe fn drop_value_by_value_index(&mut self, ci: usize, vi: usize);

    /// Removes and forgets a component value of an entity in the entity
    /// container.
    ///
    /// Caller must choose one of methods below to take a component value out.
    /// - [`AddEntity::remove_value_by_value_index`].
    /// - [`AddEntity::drop_value_by_value_index`].
    /// - [`AddEntity::forget_value_by_value_index`].
    ///
    /// # Panics
    ///
    /// See [`AddEntity::begin_remove_row_by_value_index`] document.
    ///
    /// # Safety
    ///
    /// See [`AddEntity::drop_value_by_value_index`].
    unsafe fn forget_value_by_value_index(&mut self, ci: usize, vi: usize);

    /// Finishes removing component values of an entity in the entity container.
    ///
    /// # Panics
    ///
    /// See [`AddEntity::begin_remove_row_by_value_index`] document.
    ///
    /// # Safety
    ///
    /// Caller must guarantee
    /// - Caller must have called [`AddEntity::begin_remove_row_by_value_index`]
    ///   once and [`AddEntity::remove_value_by_value_index`] number of
    ///   component columns times before calling to this method for just one
    ///   entity.
    /// - Value index `vi` is not out of bounds.
    unsafe fn end_remove_row_by_value_index(&mut self, vi: usize);
}

/// A specific entity identifier.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct EntityId {
    /// Index to a specific entity container.
    ei: EntityIndex,

    /// Row index to an entity in an entity container.
    ///
    /// Row index is defined in [`ContainEntity`] document.
    ri: usize,
}

impl EntityId {
    pub const fn new(ei: EntityIndex, ri: usize) -> Self {
        Self { ei, ri }
    }

    pub const fn container_index(&self) -> EntityIndex {
        self.ei
    }

    pub const fn row_index(&self) -> usize {
        self.ri
    }
}

impl fmt::Display for EntityId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.ei.index(), self.ri)
    }
}

impl fmt::Debug for EntityId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("EntityId")
            .field("gen", &self.ei.generation())
            .field("ei", &self.ei.index())
            .field("ri", &self.ri)
            .finish()
    }
}

/// Key for the map [`EntityStorage`] in order to get value [`EntityContainer`].
/// `EntityStorage` provides some access ways shown below.
/// - Index: Entity container index and generation when the container is generated.
/// - Name: Unique name for the entity. Each entity must have its name.
/// - Type: If the entity is declared statically, it has its own type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum EntityKey {
    /// Component keys, another type of [`TypeId`], that belong to an entity.
    ///
    /// Searching an entity container using component keys always succeeds if
    /// the keys are valid. In searching, component keys must be sorted and
    /// deduplicated.
    Ckeys(Arc<[ComponentKey]>),

    /// Index to an entity container.
    ///
    /// Searching an entity container using entity index always succeeds if
    /// the index is valid.
    Index(EntityIndex),

    /// Name of an entity container.
    ///
    /// Entity container may not have its name. In this casa, it fails to search
    /// an entity container using entity name.
    Name(EntityName),
}

impl_from_for_enum!("outer" = EntityKey; "var" = Ckeys; "inner" = Arc<[ComponentKey]>);
impl_from_for_enum!("outer" = EntityKey; "var" = Index; "inner" = EntityIndex);
impl_from_for_enum!("outer" = EntityKey; "var" = Name; "inner" = EntityName);

impl EntityKey {
    pub(crate) fn index(&self) -> &EntityIndex {
        self.try_into().unwrap()
    }

    pub(crate) fn get_ref(&self) -> EntityKeyRef<'_> {
        match self {
            Self::Index(ei) => EntityKeyRef::Index(ei),
            Self::Ckeys(ckeys) => EntityKeyRef::Ckeys(ckeys),
            Self::Name(name) => EntityKeyRef::Name(name),
        }
    }
}

impl<'r> From<&'r EntityKey> for EntityKeyRef<'r> {
    fn from(value: &'r EntityKey) -> Self {
        value.get_ref()
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum EntityKeyRef<'r> {
    Ckeys(&'r [ComponentKey]),
    Index(&'r EntityIndex),
    Name(&'r str),
}

impl_from_for_enum!(
    "lifetimes" = 'r;
    "outer" = EntityKeyRef; "var" = Ckeys; "inner" = &'r [ComponentKey]
);
impl_from_for_enum!(
    "lifetimes" = 'r;
    "outer" = EntityKeyRef; "var" = Index; "inner" = &'r EntityIndex
);
impl_from_for_enum!(
    "lifetimes" = 'r;
    "outer" = EntityKeyRef; "var" = Name; "inner" = &'r str
);

/// Index to a specific entity container.
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
#[repr(transparent)]
pub struct EntityIndex(With<usize, u64>);

impl EntityIndex {
    const DUMMY: Self = Self(With::new(usize::MAX, u64::MAX));

    pub(crate) const fn new(index: With<usize, u64>) -> Self {
        Self(index)
    }

    pub(crate) const fn dummy() -> Self {
        Self::DUMMY
    }

    pub fn index(&self) -> usize {
        self.0.value
    }

    pub fn generation(&self) -> u64 {
        self.0.with
    }
}

impl Default for EntityIndex {
    fn default() -> Self {
        Self::dummy()
    }
}

impl fmt::Display for EntityIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// Unique entity name.
///
/// An entity container can be distinguished by its name, so in other words, it
/// must be unique.
#[derive(Hash, PartialEq, Eq, Clone, Debug)]
#[repr(transparent)]
pub struct EntityName(Arc<str>);

impl EntityName {
    pub const fn new(name: Arc<str>) -> Self {
        Self(name)
    }
}

impl Deref for EntityName {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::borrow::Borrow<str> for EntityName {
    fn borrow(&self) -> &str {
        self
    }
}

impl fmt::Display for EntityName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub(crate) enum EntityKeyKind {
    /// Corresponds to [`EntityKey::Index`].
    Index,
    /// Corresponds to [`EntityKey::Ckeys`].
    Ckeys,
    /// Corresponds to [`EntityKey::Name`].
    Name,
}

impl From<&EntityKey> for EntityKeyKind {
    fn from(value: &EntityKey) -> Self {
        match value {
            EntityKey::Index(..) => Self::Index,
            EntityKey::Ckeys(..) => Self::Ckeys,
            EntityKey::Name(..) => Self::Name,
        }
    }
}

/// A piece of information about an entity such as entity index, name, and its components.
#[derive(Eq)]
pub struct EntityTag {
    index: EntityIndex,

    /// Optional entity name.
    name: Option<EntityName>,

    /// Sorted component keys.
    ///
    /// Note that this is sorted, so that index may be different with colum
    /// index used in [`Self::cont`].
    ckeys: Arc<[ComponentKey]>,

    /// Corresponding component names to component keys.
    cnames: Box<[&'static str]>,
}

impl fmt::Debug for EntityTag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("EntityTag")
            .field("index", &self.index())
            .field("name", &self.get_name())
            .field("ckeys", &self.get_component_keys())
            .field("cnames", &self.get_component_names())
            .finish()
    }
}

impl EntityTag {
    pub(crate) fn new(
        index: EntityIndex,
        name: Option<EntityName>,
        ckeys: Arc<[ComponentKey]>,
        cnames: Box<[&'static str]>,
    ) -> Self {
        assert_eq!(ckeys.len(), ckeys.len());

        Self {
            index,
            name,
            ckeys,
            cnames,
        }
    }

    pub(crate) const fn index(&self) -> EntityIndex {
        self.index
    }

    pub const fn get_name(&self) -> Option<&EntityName> {
        self.name.as_ref()
    }

    pub(crate) const fn get_component_keys(&self) -> &Arc<[ComponentKey]> {
        &self.ckeys
    }

    pub const fn get_component_names(&self) -> &[&'static str] {
        &self.cnames
    }
}

impl PartialEq for EntityTag {
    fn eq(&self, other: &Self) -> bool {
        self.index() == other.index()
    }
}
