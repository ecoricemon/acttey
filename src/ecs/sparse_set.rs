use super::{
    borrow_js::JsAtomic,
    traits::{Getter, Together},
};
use crate::ds::{
    borrow::{BorrowError, Borrowed, Holder},
    common::TypeInfo,
    vec::{ChunkAnyVec, OptVec},
};
use std::{any::TypeId, cmp, collections::HashMap, mem, ptr::NonNull};

/// Two dimensional storage containing heterogeneous types of data.
/// This structure is composed of "Sparse" and "Dense" layers.
/// Sparse layer is literally sparse, so it has vacant slots in it, while dense layer doesn't.
/// Dense layer has items and they can be accessed through the sparse layer.
/// Each dense is identified by its item's [`TypeId`].
/// But you are encouraged to access each dense by its index, not TypeId for the performance.
///
/// We call each dense layer a column, and all columns have the same length.
/// So it looks like a 2D matrix.
#[derive(Debug)]
pub struct SparseSet {
    sparse: OptVec<usize>,
    deref: Vec<usize>,
    cols: Vec<Holder<ChunkAnyVec, Getter, Getter, JsAtomic>>,
    map: HashMap<TypeId, usize, ahash::RandomState>,
}

impl SparseSet {
    const CHUNK_SIZE: usize = 4 * 1024;
    const MIN_CHUNK_LEN: usize = 8;

    pub fn new() -> Self {
        Self {
            sparse: OptVec::new(0),
            deref: Vec::new(),
            cols: Vec::new(),
            map: HashMap::default(),
        }
    }
}

impl Together for SparseSet {
    /// Adds a new column and returns column index.
    fn add_column(&mut self, tinfo: TypeInfo) -> Option<usize> {
        let ty = tinfo.id;
        if self.len() > 0 || self.get_column_index(&ty).is_some() {
            return None;
        }

        let chunk_len = (Self::CHUNK_SIZE / tinfo.size).next_power_of_two();
        let chunk_len = cmp::max(chunk_len, Self::MIN_CHUNK_LEN);

        // Adds column wapped with Holder.
        let value = ChunkAnyVec::new(tinfo, chunk_len);
        let fn_imm = |col: &ChunkAnyVec| Getter {
            me: col as *const ChunkAnyVec as *mut u8,
            len: col.len(),
            fn_get: |me: *mut u8, index: usize| unsafe {
                let me = &*(me as *const ChunkAnyVec);
                me.get_raw_unchecked(index)
            },
        };
        let fn_mut = |col: &mut ChunkAnyVec| Getter {
            me: col as *mut ChunkAnyVec as *mut u8,
            len: col.len(),
            fn_get: |me: *mut u8, index: usize| unsafe {
                let me = &*(me as *const ChunkAnyVec);
                me.get_raw_unchecked(index)
            },
        };
        let holder = Holder::new(value, fn_imm, fn_mut);
        self.cols.push(holder);

        let ci = self.cols.len() - 1;
        self.map.insert(ty, ci);
        Some(ci)
    }

    fn remove_column(&mut self, ci: usize) -> Option<TypeInfo> {
        if ci >= self.get_column_num() || self.cols[ci].borrow_count() != 0 {
            return None;
        }

        let old = self.cols.remove(ci);

        // Does re-mapping.
        for i in ci..self.cols.len() {
            let ty = self.cols[i].get().type_id();
            *self.map.get_mut(&ty).unwrap() = i;
        }

        // If empty, initialize self completely.
        if self.cols.is_empty() {
            mem::take(self);
        }

        Some(*old.get().type_info())
    }

    fn new_from_this(&self) -> Box<dyn Together> {
        // Creates new Holders keeping same type information.
        let cols = self
            .cols
            .iter()
            .map(|col| {
                let (fn_imm, fn_mut) = (col.get_fn_imm(), col.get_fn_mut());
                let col = col.get();
                let value = ChunkAnyVec::new(*col.type_info(), col.chunk_len());
                Holder::new(value, fn_imm, fn_mut)
            })
            .collect::<Vec<_>>();

        // We can reuse mapping information.
        let map = self.map.clone();

        // Makes empty instance.
        let sset = Self {
            sparse: OptVec::new(0),
            deref: Vec::new(),
            cols,
            map,
        };
        Box::new(sset)
    }

    #[inline]
    fn contains_column(&self, ty: &TypeId) -> bool {
        self.map.contains_key(ty)
    }

    #[inline]
    fn get_column_index(&self, ty: &TypeId) -> Option<usize> {
        self.map.get(ty).cloned()
    }

    #[inline]
    fn get_column_info(&self, ci: usize) -> Option<&TypeInfo> {
        self.cols.get(ci).map(|v| v.get().type_info())
    }

    #[inline]
    fn get_column_num(&self) -> usize {
        self.cols.len()
    }

    fn len(&self) -> usize {
        self.cols
            .first()
            .map(|v| v.get().len())
            .unwrap_or_default()
    }

    /// Nothing to do.
    #[inline]
    fn begin_add_item(&mut self) {}

    /// # Panics
    ///
    /// Panics if other threads borrowed any column.
    #[inline]
    unsafe fn add_item(&mut self, ci: usize, ptr: *const u8) {
        let mut col = self.cols[ci].get_mut().unwrap();
        col.push_raw(ptr);
    }

    /// # Panics
    ///
    /// Panics if you haven't added items to all columns.
    fn end_add_item(&mut self) -> usize {
        let len = self.deref.len() + 1;
        assert!(self.cols.iter().all(|v| v.get().len() == len));

        let key = self.sparse.add(len - 1);
        self.deref.push(key);
        key
    }

    /// # Panics
    ///
    /// Panics if other threads borrowed any column.
    fn remove_item(&mut self, ri: usize) -> bool {
        let key = ri;

        if let Some(index) = self.sparse.take(key) {
            for col in self.cols.iter_mut() {
                let mut col = col.get_mut().unwrap();
                col.swap_remove_drop(index);
            }

            self.deref.swap_remove(index);

            if index < self.deref.len() {
                let moved_key = self.deref[index];
                *self.sparse.get_mut(moved_key).unwrap() = index;
            }

            true
        } else {
            false
        }
    }

    fn get_item(&self, ci: usize, ri: usize) -> Option<NonNull<u8>> {
        let key = ri;

        let index = *self.sparse.get(key)?;
        let col = self.cols.get(ci)?;
        col.get().get_raw(index)
    }

    /// # Panics
    ///
    /// Panics if other threads borrowed any column.
    fn get_item_mut(&mut self, ci: usize, ri: usize) -> Option<NonNull<u8>> {
        let key = ri;

        let index = *self.sparse.get(key)?;
        let col = self.cols.get_mut(ci)?;
        let col = col.get_mut().unwrap();
        col.get_raw(index)
    }

    fn borrow_column(&self, ci: usize) -> Result<Borrowed<Getter, JsAtomic>, BorrowError> {
        if let Some(col) = self.cols.get(ci) {
            col.borrow()
        } else {
            Err(BorrowError::OutOfBound)
        }
    }

    fn borrow_column_mut(&mut self, ci: usize) -> Result<Borrowed<Getter, JsAtomic>, BorrowError> {
        if let Some(col) = self.cols.get_mut(ci) {
            col.borrow_mut()
        } else {
            Err(BorrowError::OutOfBound)
        }
    }
}

impl Default for SparseSet {
    fn default() -> Self {
        Self::new()
    }
}
