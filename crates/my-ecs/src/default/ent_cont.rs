//! Provides [`ContainEntity`] implementations.
//!
//! Currently, there are two options.
//!
//! - [`SparseSet`] based on [`AnyVec`], which stores data serially. It's good
//!   for traversing items, but requires copy when the vector needs to extend
//!   its capacity.
//!
//! - [`ChunkSparseSet`] based on [`ChunkAnyVec`], which stores data in chunks.
//!   Chunk is a fixed sized memory, and the vector appends a chunk when it
//!   needs more capacity. Therefore, `ChunkSparseSet` is good for frequent
//!   insertion or removal, but not good as mush as `SparseSet` in terms of
//!   traversing items.

use crate::{
    ds::{
        AnyVec, AsFlatRawIter, BorrowError, BorrowResult, ChunkAnyVec, FlatRawIter, Holder, OptVec,
        RawGetter, TypeInfo,
    },
    ecs::ent::entity::{AddEntity, BorrowComponent, ContainEntity, RegisterComponent},
};
use std::{
    any::TypeId, cmp, collections::HashMap, fmt::Debug, hash::BuildHasher, mem, ptr::NonNull,
};

/// Two-dimensional storage containing heterogeneous data types based on
/// [`ChunkAnyVec`].
///
/// Unlike `SparseSet`, this struct stores data in chunks. Chunk is a fixed
/// sized memory, and it's appended when extra capacity is needed. This chunk
/// based capacity control removes data copy that is seen from normal vector,
/// but note that it brings inefficiency in terms of iteration.
///
/// See [`SparseSet`] for more details.
#[derive(Debug)]
pub struct ChunkSparseSet<S> {
    sparse: OptVec<usize, S>,
    deref: Vec<usize>,
    cols: Vec<Holder<ChunkAnyVec, RawGetter, RawGetter>>,
    map: HashMap<TypeId, usize, S>,
}

impl<S> ChunkSparseSet<S>
where
    S: Default,
{
    const CHUNK_SIZE: usize = 4 * 1024;
    const MIN_CHUNK_LEN: usize = 8;

    /// Creates a new empty [`ChunkSparseSet`].
    ///
    /// # Examples
    ///
    /// ```
    /// # use my_ecs::prelude::*;
    ///
    /// let mut cont = ChunkSparseSet::<std::hash::RandomState>::new();
    /// ```
    pub fn new() -> Self {
        Self {
            sparse: OptVec::new(),
            deref: Vec::new(),
            cols: Vec::new(),
            map: HashMap::default(),
        }
    }
}

impl<S> ContainEntity for ChunkSparseSet<S>
where
    S: BuildHasher + Default + Clone + 'static,
{
    fn create_twin(&self) -> Box<dyn ContainEntity> {
        // Creates new `Holder`s for the same types.
        let cols = self
            .cols
            .iter()
            .map(|col| {
                let (fn_imm, fn_mut) = (col.get_fn_imm(), col.get_fn_mut());
                let col = col.get().unwrap();
                let value = ChunkAnyVec::new(*col.type_info(), col.default_chunk_capacity());
                // Safety: It's safe to create the same `Holder`. The safety
                // must have been guaranteed by source `Holder`.
                unsafe { Holder::new(value, fn_imm, fn_mut) }
            })
            .collect::<Vec<_>>();

        // We can reuse mapping information.
        let map = self.map.clone();

        // Makes empty instance.
        let this = Self {
            sparse: OptVec::new(),
            deref: Vec::new(),
            cols,
            map,
        };
        Box::new(this)
    }

    fn get_item_mut(&mut self, ci: usize, ri: usize) -> Option<NonNull<u8>> {
        let key = ri;

        let index = *self.sparse.get(key)?;
        let col = self.cols.get_mut(ci)?;
        let col = col.get_mut().unwrap();
        col.get_raw(index)
    }

    fn len(&self) -> usize {
        // All columns must have the same length.
        self.cols
            .first()
            .map(|col| unsafe { col.get_unchecked().len() })
            .unwrap_or_default()
    }

    fn capacity(&self) -> usize {
        // All columns must have the same length.
        self.cols
            .first()
            .map(|col| unsafe { col.get_unchecked().capacity() })
            .unwrap_or_default()
    }

    fn reserve(&mut self, additional: usize) {
        for col in self.cols.iter_mut() {
            col.get_mut().unwrap().reserve(additional);
        }
    }

    fn shrink_to_fit(&mut self) {
        for col in self.cols.iter_mut() {
            col.get_mut().unwrap().shrink_to_fit();
        }
    }

    unsafe fn resize_column(&mut self, ci: usize, new_len: usize, val_ptr: NonNull<u8>) {
        let mut col = self.cols[ci].get_mut().unwrap();
        assert!(col.is_clone());
        unsafe { col.resize_raw(new_len, val_ptr) }
    }
}

impl<S> RegisterComponent for ChunkSparseSet<S>
where
    S: BuildHasher + Default + Clone + 'static,
{
    /// Adds a new column and returns column index.
    fn add_column(&mut self, tinfo: TypeInfo) -> Option<usize> {
        let ty = tinfo.ty;
        if self.len() > 0 || self.get_column_index(&ty).is_some() {
            return None;
        }

        // Holder's immutable access function.
        fn fn_imm(col: &ChunkAnyVec) -> RawGetter {
            // Safety: Infallible.
            let this = unsafe { NonNull::new_unchecked((col as *const _ as *const u8).cast_mut()) };
            let len = col.len();
            unsafe fn fn_get(this: NonNull<u8>, index: usize) -> NonNull<u8> {
                unsafe { this.cast::<ChunkAnyVec>().as_ref().get_raw_unchecked(index) }
            }
            unsafe fn fn_iter(this: NonNull<u8>) -> FlatRawIter {
                unsafe { this.cast::<ChunkAnyVec>().as_ref().iter() }
            }
            // Safety: The pointer is valid.
            let getter = unsafe { RawGetter::new(this, len, fn_get) };
            getter.with_iter(fn_iter)
        }

        // Holder's mutable access function.
        fn fn_mut(col: &mut ChunkAnyVec) -> RawGetter {
            fn_imm(col)
        }

        // Adds column wrapped with Holder.
        let chunk_len = if tinfo.size != 0 {
            cmp::max(
                (Self::CHUNK_SIZE / tinfo.size).next_power_of_two(),
                Self::MIN_CHUNK_LEN,
            )
        } else {
            0 // Has no effect
        };
        let value = ChunkAnyVec::new(tinfo, chunk_len);
        // Safety: `fn_imm` and `fn_mut` don't return lifetime-constrained type.
        let holder = unsafe { Holder::new(value, fn_imm, fn_mut) };
        self.cols.push(holder);

        let ci = self.cols.len() - 1;
        self.map.insert(ty, ci);
        Some(ci)
    }

    fn remove_column(&mut self, ci: usize) -> Option<TypeInfo> {
        if ci >= self.num_columns() || self.cols[ci].borrow_count() != 0 {
            return None;
        }

        let old = self.cols.remove(ci);

        // Does re-mapping.
        for i in ci..self.cols.len() {
            let col = self.cols[i].get().unwrap();
            let ty = col.type_id();
            *self.map.get_mut(&ty).unwrap() = i;
        }

        // If empty, initialize self completely.
        if self.cols.is_empty() {
            mem::take(self);
        }

        let old = old.get().unwrap();
        let tinfo = old.type_info();
        Some(*tinfo)
    }

    fn get_column_index(&self, ty: &TypeId) -> Option<usize> {
        self.map.get(ty).cloned()
    }

    fn get_column_info(&self, ci: usize) -> Option<&TypeInfo> {
        self.cols.get(ci).map(|col| {
            let col = col.get().unwrap();
            col.type_info()
        })
    }

    fn num_columns(&self) -> usize {
        self.cols.len()
    }

    fn contains_column(&self, ty: &TypeId) -> bool {
        self.map.contains_key(ty)
    }
}

impl<S> BorrowComponent for ChunkSparseSet<S>
where
    S: BuildHasher + Default + Clone + 'static,
{
    fn borrow_column(&self, ci: usize) -> BorrowResult<RawGetter> {
        #[cfg(feature = "check")]
        let this_len = self.len();

        if let Some(col) = self.cols.get(ci) {
            let borrowed = col.borrow();

            // Validates length.
            #[cfg(feature = "check")]
            {
                if let Ok(borrowed) = borrowed.as_ref() {
                    assert_eq!(
                        this_len,
                        borrowed.len(),
                        "borrowed column must have the same length as entity container's"
                    );
                }
            }

            borrowed
        } else {
            Err(BorrowError::OutOfBound)
        }
    }

    fn borrow_column_mut(&mut self, ci: usize) -> BorrowResult<RawGetter> {
        #[cfg(feature = "check")]
        let this_len = self.len();

        if let Some(col) = self.cols.get_mut(ci) {
            let borrowed = col.borrow_mut();

            // Validates length.
            #[cfg(feature = "check")]
            {
                if let Ok(borrowed) = borrowed.as_ref() {
                    assert_eq!(
                        this_len,
                        borrowed.len(),
                        "borrowed column must have the same length as entity container's"
                    );
                }
            }

            borrowed
        } else {
            Err(BorrowError::OutOfBound)
        }
    }

    unsafe fn get_column(&self, ci: usize) -> Option<NonNull<u8>> {
        self.cols.get(ci).map(|col| unsafe {
            let ptr = col.get_unchecked() as *const _ as *const u8;
            NonNull::new_unchecked(ptr.cast_mut())
        })
    }
}

impl<S> AddEntity for ChunkSparseSet<S>
where
    S: BuildHasher + Default + Clone + 'static,
{
    fn to_value_index(&self, ri: usize) -> Option<usize> {
        let key = ri;

        self.sparse.get(key).cloned()
    }

    fn begin_add_row(&mut self) {}

    unsafe fn add_value(&mut self, ci: usize, val_ptr: NonNull<u8>) {
        // Panics if holder has been borrowed and not returned yet.
        unsafe {
            let holder = self.cols.get_unchecked_mut(ci);
            let mut col = holder.get_mut().unwrap_unchecked();
            col.push_raw(val_ptr);
        }
    }

    unsafe fn end_add_row(&mut self) -> usize {
        let vi = self.deref.len();
        let ri = self.sparse.add(vi);
        self.deref.push(ri);
        ri
    }

    fn value_ptr_by_value_index(&self, ci: usize, vi: usize) -> Option<NonNull<u8>> {
        // Safety: Getting pointer is ok.
        let col = unsafe { self.cols.get(ci)?.get_unchecked() };
        col.get_raw(vi)
    }

    fn remove_row(&mut self, ri: usize) -> bool {
        if let Some(vi) = self.to_value_index(ri) {
            self.remove_row_by_value_index(vi);
            true
        } else {
            false
        }
    }

    fn remove_row_by_value_index(&mut self, vi: usize) {
        unsafe {
            self.begin_remove_row_by_value_index(vi);
            for ci in 0..self.num_columns() {
                self.drop_value_by_value_index(ci, vi);
            }
            self.end_remove_row_by_value_index(vi);
        }
    }

    unsafe fn begin_remove_row_by_value_index(&mut self, vi: usize) {
        assert!(vi < self.len());
    }

    unsafe fn remove_value_by_value_index(&mut self, ci: usize, vi: usize, buf: NonNull<u8>) {
        let holder = unsafe { self.cols.get_unchecked_mut(ci) };
        let mut col = holder.get_mut().unwrap();
        unsafe { col.swap_remove_raw(vi, buf.as_ptr()) };
    }

    unsafe fn drop_value_by_value_index(&mut self, ci: usize, vi: usize) {
        let holder = unsafe { self.cols.get_unchecked_mut(ci) };
        let mut col = holder.get_mut().unwrap();
        col.swap_remove_drop(vi);
    }

    unsafe fn forget_value_by_value_index(&mut self, ci: usize, vi: usize) {
        let holder = unsafe { self.cols.get_unchecked_mut(ci) };
        let mut col = holder.get_mut().unwrap();
        col.swap_remove_forget(vi);
    }

    unsafe fn end_remove_row_by_value_index(&mut self, vi: usize) {
        let ri = self.deref.swap_remove(vi);
        let _vi = self.sparse.take(ri);
        debug_assert_eq!(_vi, Some(vi));
        if vi < self.deref.len() {
            let moved_key = self.deref[vi];
            *self.sparse.get_mut(moved_key).unwrap() = vi;
        }
    }
}

impl<S> Default for ChunkSparseSet<S>
where
    S: Default,
{
    fn default() -> Self {
        Self::new()
    }
}

/// Two-dimensional storage containing heterogeneous data types based on
/// [`AnyVec`].
///
/// This struct is composed of "Sparse" and "Dense" layers. Sparse layer is
/// literally sparse, so it can contain vacant slots in it, while dense layer
/// doesn't. Dense layer contains real items and the items can be accessed
/// through the sparse layer. Each dense is identified by its item's [`TypeId`].
/// But you are encouraged to access each dense by its index, not `TypeId` for
/// the performance.
///
/// We call each dense layer a column, and all columns have the same length.
/// So it looks like a 2D matrix as shown below.
///
/// ```text
/// Index  Sparse  Dense  Dense
///                  A      B
///   0      0 _____ .      .
///   1      2 _   _ .      .
///   2      x  \_/_ .      .
///   3      1 __/   
///
/// , where '.' is item and 'x' is vacant slot.
/// ```
#[derive(Debug)]
pub struct SparseSet<S> {
    sparse: OptVec<usize, S>,
    deref: Vec<usize>,
    cols: Vec<Holder<AnyVec, RawGetter, RawGetter>>,
    map: HashMap<TypeId, usize, S>,
}

impl<S> SparseSet<S>
where
    S: Default,
{
    /// Creates a new empty [`SparseSet`].
    ///
    /// # Examples
    ///
    /// ```
    /// # use my_ecs::prelude::*;
    ///
    /// let mut cont = SparseSet::<std::hash::RandomState>::new();
    /// ```
    pub fn new() -> Self {
        Self {
            sparse: OptVec::new(),
            deref: Vec::new(),
            cols: Vec::new(),
            map: HashMap::default(),
        }
    }
}

impl<S> ContainEntity for SparseSet<S>
where
    S: BuildHasher + Default + Clone + 'static,
{
    fn create_twin(&self) -> Box<dyn ContainEntity> {
        // Creates new `Holder`s for the same types.
        let cols = self
            .cols
            .iter()
            .map(|col| {
                let (fn_imm, fn_mut) = (col.get_fn_imm(), col.get_fn_mut());
                let col = col.get().unwrap();
                let value = AnyVec::new(*col.type_info());
                // Safety: It's safe to create the same `Holder`. The safety
                // must have been guaranteed by source `Holder`.
                unsafe { Holder::new(value, fn_imm, fn_mut) }
            })
            .collect::<Vec<_>>();

        // We can reuse mapping information.
        let map = self.map.clone();

        // Makes empty instance.
        let this = Self {
            sparse: OptVec::new(),
            deref: Vec::new(),
            cols,
            map,
        };
        Box::new(this)
    }

    fn get_item_mut(&mut self, ci: usize, ri: usize) -> Option<NonNull<u8>> {
        let key = ri;

        let index = *self.sparse.get(key)?;
        let col = self.cols.get_mut(ci)?;
        let col = col.get_mut().unwrap();
        col.get_raw(index)
    }

    fn len(&self) -> usize {
        // All columns must have the same length.
        self.cols
            .first()
            .map(|col| unsafe { col.get_unchecked().len() })
            .unwrap_or_default()
    }

    fn capacity(&self) -> usize {
        // All columns must have the same length.
        self.cols
            .first()
            .map(|col| unsafe { col.get_unchecked().capacity() })
            .unwrap_or_default()
    }

    fn reserve(&mut self, additional: usize) {
        for col in self.cols.iter_mut() {
            col.get_mut().unwrap().reserve(additional);
        }
    }

    fn shrink_to_fit(&mut self) {
        for col in self.cols.iter_mut() {
            col.get_mut().unwrap().shrink_to_fit();
        }
    }

    unsafe fn resize_column(&mut self, ci: usize, new_len: usize, val_ptr: NonNull<u8>) {
        let mut col = self.cols[ci].get_mut().unwrap();
        assert!(col.is_clone());
        unsafe { col.resize_raw(new_len, val_ptr) };
    }
}

impl<S> RegisterComponent for SparseSet<S>
where
    S: BuildHasher + Default + Clone + 'static,
{
    fn add_column(&mut self, tinfo: TypeInfo) -> Option<usize> {
        let ty = tinfo.ty;
        if self.len() > 0 || self.get_column_index(&ty).is_some() {
            return None;
        }

        // Holder's immutable access function.
        fn fn_imm(col: &AnyVec) -> RawGetter {
            // Safety: Infallible.
            let this = unsafe { NonNull::new_unchecked((col as *const _ as *const u8).cast_mut()) };
            let len = col.len();
            unsafe fn fn_get(this: NonNull<u8>, index: usize) -> NonNull<u8> {
                unsafe { this.cast::<AnyVec>().as_ref().get_raw_unchecked(index) }
            }
            unsafe fn fn_iter(this: NonNull<u8>) -> FlatRawIter {
                let this = unsafe { this.cast::<AnyVec>().as_ref() };
                this.iter2()
            }
            // Safety: The pointer is valid.
            let getter = unsafe { RawGetter::new(this, len, fn_get) };
            getter.with_iter(fn_iter)
        }

        // Holder's mutable access function.
        fn fn_mut(col: &mut AnyVec) -> RawGetter {
            fn_imm(col)
        }

        // Adds column wrapped with Holder.
        let value = AnyVec::new(tinfo);
        // Safety: `fn_imm` and `fn_mut` don't return lifetime-constrained type.
        let holder = unsafe { Holder::new(value, fn_imm, fn_mut) };
        self.cols.push(holder);

        let ci = self.cols.len() - 1;
        self.map.insert(ty, ci);
        Some(ci)
    }

    fn remove_column(&mut self, ci: usize) -> Option<TypeInfo> {
        if ci >= self.num_columns() || self.cols[ci].borrow_count() != 0 {
            return None;
        }

        let old = self.cols.remove(ci);

        // Does re-mapping.
        for i in ci..self.cols.len() {
            let col = self.cols[i].get().unwrap();
            let ty = col.type_id();
            *self.map.get_mut(&ty).unwrap() = i;
        }

        // If empty, initialize self completely.
        if self.cols.is_empty() {
            mem::take(self);
        }

        let old = old.get().unwrap();
        let tinfo = old.type_info();
        Some(*tinfo)
    }

    fn get_column_index(&self, ty: &TypeId) -> Option<usize> {
        self.map.get(ty).cloned()
    }

    fn get_column_info(&self, ci: usize) -> Option<&TypeInfo> {
        self.cols.get(ci).map(|col| {
            let col = col.get().unwrap();
            col.type_info()
        })
    }

    fn num_columns(&self) -> usize {
        self.cols.len()
    }

    fn contains_column(&self, ty: &TypeId) -> bool {
        self.map.contains_key(ty)
    }
}

impl<S> BorrowComponent for SparseSet<S>
where
    S: BuildHasher + Default + Clone + 'static,
{
    fn borrow_column(&self, ci: usize) -> BorrowResult<RawGetter> {
        #[cfg(feature = "check")]
        let this_len = self.len();

        if let Some(col) = self.cols.get(ci) {
            let borrowed = col.borrow();

            // Validates length.
            #[cfg(feature = "check")]
            {
                if let Ok(borrowed) = borrowed.as_ref() {
                    assert_eq!(
                        this_len,
                        borrowed.len(),
                        "borrowed column must have the same length as entity container's"
                    );
                }
            }

            borrowed
        } else {
            Err(BorrowError::OutOfBound)
        }
    }

    fn borrow_column_mut(&mut self, ci: usize) -> BorrowResult<RawGetter> {
        #[cfg(feature = "check")]
        let this_len = self.len();

        if let Some(col) = self.cols.get_mut(ci) {
            let borrowed = col.borrow_mut();

            // Validates length.
            #[cfg(feature = "check")]
            {
                if let Ok(borrowed) = borrowed.as_ref() {
                    assert_eq!(
                        this_len,
                        borrowed.len(),
                        "borrowed column must have the same length as entity container's"
                    );
                }
            }

            borrowed
        } else {
            Err(BorrowError::OutOfBound)
        }
    }

    unsafe fn get_column(&self, ci: usize) -> Option<NonNull<u8>> {
        self.cols.get(ci).map(|col| unsafe {
            let ptr = col.get_unchecked() as *const _ as *const u8;
            NonNull::new_unchecked(ptr.cast_mut())
        })
    }
}

impl<S> AddEntity for SparseSet<S>
where
    S: BuildHasher + Default + Clone + 'static,
{
    fn to_value_index(&self, ri: usize) -> Option<usize> {
        let key = ri;

        self.sparse.get(key).cloned()
    }

    fn begin_add_row(&mut self) {}

    unsafe fn add_value(&mut self, ci: usize, val_ptr: NonNull<u8>) {
        unsafe {
            // Panics if holder has been borrowed and not returned yet.
            let holder = self.cols.get_unchecked_mut(ci);
            let mut col = holder.get_mut().unwrap_unchecked();
            col.push_raw(val_ptr);
        }
    }

    unsafe fn end_add_row(&mut self) -> usize {
        let vi = self.deref.len();
        let ri = self.sparse.add(vi);
        self.deref.push(ri);
        ri
    }

    fn value_ptr_by_value_index(&self, ci: usize, vi: usize) -> Option<NonNull<u8>> {
        // Safety: Getting pointer is ok.
        let col = unsafe { self.cols.get(ci)?.get_unchecked() };
        col.get_raw(vi)
    }

    fn remove_row(&mut self, ri: usize) -> bool {
        if let Some(vi) = self.to_value_index(ri) {
            self.remove_row_by_value_index(vi);
            true
        } else {
            false
        }
    }

    fn remove_row_by_value_index(&mut self, vi: usize) {
        unsafe {
            self.begin_remove_row_by_value_index(vi);
            for ci in 0..self.num_columns() {
                self.drop_value_by_value_index(ci, vi);
            }
            self.end_remove_row_by_value_index(vi);
        }
    }

    unsafe fn begin_remove_row_by_value_index(&mut self, vi: usize) {
        assert!(vi < self.len());
    }

    unsafe fn remove_value_by_value_index(&mut self, ci: usize, vi: usize, buf: NonNull<u8>) {
        unsafe {
            let holder = self.cols.get_unchecked_mut(ci);
            let mut col = holder.get_mut().unwrap();
            col.swap_remove_raw(vi, buf.as_ptr());
        }
    }

    unsafe fn drop_value_by_value_index(&mut self, ci: usize, vi: usize) {
        let holder = unsafe { self.cols.get_unchecked_mut(ci) };
        let mut col = holder.get_mut().unwrap();
        col.swap_remove_drop(vi);
    }

    unsafe fn forget_value_by_value_index(&mut self, ci: usize, vi: usize) {
        let holder = unsafe { self.cols.get_unchecked_mut(ci) };
        let mut col = holder.get_mut().unwrap();
        col.swap_remove_forget(vi);
    }

    unsafe fn end_remove_row_by_value_index(&mut self, vi: usize) {
        let ri = self.deref.swap_remove(vi);
        let _vi = self.sparse.take(ri);
        debug_assert_eq!(_vi, Some(vi));
        if vi < self.deref.len() {
            let moved_key = self.deref[vi];
            *self.sparse.get_mut(moved_key).unwrap() = vi;
        }
    }
}

impl<S> Default for SparseSet<S>
where
    S: Default,
{
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use crate as my_ecs;
    use crate::prelude::*;
    use std::{hash::RandomState, sync::Arc};

    #[test]
    fn test_move_entity_to_entity_container() {
        inner(SparseSet::<RandomState>::new());
        inner(ChunkSparseSet::<RandomState>::new());

        fn inner<T: ContainEntity>(mut cont: T) {
            #![allow(unused)]

            #[derive(Entity)]
            struct Ea {
                ca: Ca,
                cb: Cb,
            }
            #[derive(Component)]
            struct Ca(i32);
            #[derive(Component)]
            struct Cb(Arc<String>);

            Ea::register_to(&mut cont);

            // Tests `Entity::move_to`.
            let b = Arc::new("0".to_owned());

            let ri_0 = Ea {
                ca: Ca(0),
                cb: Cb(Arc::clone(&b)),
            }
            .move_to(&mut cont);
            assert_eq!(Arc::strong_count(&b), 2);
            assert_eq!(cont.len(), 1);

            let ri_1 = Ea {
                ca: Ca(1),
                cb: Cb(Arc::clone(&b)),
            }
            .move_to(&mut cont);
            assert_eq!(Arc::strong_count(&b), 3);
            assert_eq!(cont.len(), 2);

            // Correctly moved? checks it by removing.
            cont.remove_row(ri_0);
            assert_eq!(Arc::strong_count(&b), 2);
            cont.remove_row(ri_1);
            assert_eq!(Arc::strong_count(&b), 1);
        }
    }

    #[test]
    fn test_reserve_entity_container() {
        fn inner<T: ContainEntity>(mut cont: T) {
            #![allow(unused)]

            #[derive(Entity)]
            struct Ea {
                ca: Ca,
            }
            #[derive(Component)]
            struct Ca(i32);

            Ea::register_to(&mut cont);

            assert_eq!(cont.capacity(), 0);
            assert_eq!(cont.len(), 0);

            let reserve_size = ChunkSparseSet::<RandomState>::CHUNK_SIZE + 1;
            cont.reserve(reserve_size);

            assert!(cont.capacity() >= reserve_size);
            assert_eq!(cont.len(), 0);
        }
    }
}
