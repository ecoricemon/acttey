use crate::{
    ntimes, ty,
    util::{split_mut, PowerOfTwo, Window},
};
use ahash::{AHashMap, AHashSet};
use std::{
    any::{Any, TypeId},
    cell::RefCell,
    hash::Hash,
    mem::swap,
    ops::{Deref, DerefMut, Index, IndexMut, Range},
    rc::Rc,
    iter::repeat,
};

// TODO: Use ChunkVec.
// And ecs::query::QueryIter and QueryIterMut had better returns Iter<Indexable> not Iter<&[]>.
// Provide sort method instead.
// impl Downcast for SparseSet is also related to this point.
pub struct AnyVec {
    type_id: TypeId,
    raw: *mut dyn Any,
    // Prevent inner Vec to be dropped and be used to call some functions without generic type
    fn_len: Box<dyn Fn() -> usize>,
    fn_capacity: Box<dyn Fn() -> usize>,
    fn_swap_remove_no_return: Box<dyn Fn(usize)>,
    fn_grow: Box<dyn Fn(usize)>,
}

impl AnyVec {
    pub fn new<T: 'static>() -> Self {
        AnyVec::_new(Vec::<T>::new())
    }

    pub fn with_capacity<T: 'static>(capacity: usize) -> Self {
        AnyVec::_new(Vec::<T>::with_capacity(capacity))
    }

    fn _new<T: 'static>(data: Vec<T>) -> Self {
        let data = Rc::new(RefCell::new(data));
        let raw = data.as_ptr() as *mut dyn Any;

        let data_len = data.clone();
        let fn_len = Box::new(move || data_len.borrow().len());

        let data_capacity = data.clone();
        let fn_capacity = Box::new(move || data_capacity.borrow().capacity());

        let data_swap_remove_no_return = data.clone();
        let fn_swap_remove_no_return = Box::new(move |index: usize| {
            data_swap_remove_no_return.borrow_mut().swap_remove(index);
        });

        let data_grow = data.clone();
        let fn_grow = Box::new(move |new_len: usize| {
            let mut data = data_grow.borrow_mut();
            debug_assert!(data.len() <= new_len);
            if data.capacity() < new_len {
                data.reserve_exact(new_len);
            }
            unsafe {
                data.set_len(new_len);
            }
        });

        Self {
            type_id: ty!(T),
            raw,
            fn_len,
            fn_capacity,
            fn_swap_remove_no_return,
            fn_grow,
        }
    }

    #[inline]
    pub fn is_type_of(&self, ty: &TypeId) -> bool {
        *ty == self.type_id
    }

    #[inline]
    pub fn len(&self) -> usize {
        (self.fn_len)()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        (self.fn_capacity)()
    }

    #[inline]
    pub fn swap_remove_no_return(&mut self, index: usize) {
        (self.fn_swap_remove_no_return)(index);
    }

    /// Sets the length to `new_len`.
    ///
    /// # Safety
    ///
    /// The elements at old_len..new_len must be initialized.
    ///
    /// # Panics
    ///
    /// In **DEBUG** mode, smaller `new_len` than current `len()` causes the panic.
    #[inline]
    pub unsafe fn grow(&mut self, new_len: usize) {
        (self.fn_grow)(new_len);
    }

    /// # Panics
    ///
    /// Calling this method with incorrect type causes panic.
    #[inline]
    pub fn get<T: 'static>(&self, index: usize) -> Option<&T> {
        assert_eq!(self.type_id, ty!(T));
        unsafe { self.get_type_unchecked::<T>(index) }
    }

    /// # Safety
    ///
    /// Calling this method with incorrect type causes *undefined behavior*.
    ///
    /// # Panics
    ///
    /// In **DEBUG** mode, safety conditions are checked and cause panic.
    #[inline]
    pub unsafe fn get_type_unchecked<T: 'static>(&self, index: usize) -> Option<&T> {
        debug_assert_eq!(self.type_id, ty!(T));
        unsafe { (*(self.raw as *mut Vec<T>)).get(index) }
    }

    /// # Safety
    ///
    /// Calling this method with incorrect type or out of bound `index`` causes *undefined behavior*.
    ///
    /// # Panics
    ///
    /// In **DEBUG** mode, safety conditions are checked and cause panic when they are met.
    #[inline]
    pub unsafe fn get_unchecked<T: 'static>(&self, index: usize) -> &T {
        debug_assert_eq!(self.type_id, ty!(T));
        debug_assert!(index < self.len());
        unsafe { (*(self.raw as *mut Vec<T>)).get_unchecked(index) }
    }

    /// # Panics
    ///
    /// Calling this method with incorrect type causes panic.
    #[inline]
    pub fn get_mut<T: 'static>(&self, index: usize) -> Option<&mut T> {
        assert_eq!(self.type_id, ty!(T));
        unsafe { self.get_type_unchecked_mut::<T>(index) }
    }

    /// # Safety
    ///
    /// Calling this method with incorrect type causes *undefined behavior*.
    ///
    /// # Panics
    ///
    /// In **DEBUG** mode, safety conditions are checked and cause panic.
    #[inline]
    pub unsafe fn get_type_unchecked_mut<T: 'static>(&self, index: usize) -> Option<&mut T> {
        debug_assert_eq!(self.type_id, ty!(T));
        unsafe { (*(self.raw as *mut Vec<T>)).get_mut(index) }
    }

    /// # Safety
    ///
    /// Calling this method with incorrect type or out of bound `index`` causes *undefined behavior*.
    ///
    /// # Panics
    ///
    /// In **DEBUG** mode, safety conditions are checked and cause panic when they are met.
    #[inline]
    pub unsafe fn get_unchecked_mut<T: 'static>(&mut self, index: usize) -> &mut T {
        debug_assert_eq!(self.type_id, ty!(T));
        debug_assert!(index < self.len());
        unsafe { (*(self.raw as *mut Vec<T>)).get_unchecked_mut(index) }
    }

    /// # Panics
    ///
    /// Calling this method with incorrect type causes panic.
    #[inline]
    pub fn push<T: 'static>(&mut self, value: T) {
        assert_eq!(self.type_id, ty!(T));
        unsafe {
            self.push_type_unchecked(value);
        }
    }

    /// # Safety
    ///
    /// Calling this method with incorrect type causes *undefined behavior*.
    ///
    /// # Panics
    ///
    /// In **DEBUG** mode, safety condition is checked and causes panic when that is met.
    #[inline]
    pub unsafe fn push_type_unchecked<T: 'static>(&mut self, value: T) {
        debug_assert_eq!(self.type_id, ty!(T));
        unsafe {
            (*(self.raw as *mut Vec<T>)).push(value);
        }
    }

    /// # Panics
    ///
    /// Calling this method with incorrect type causes panic.
    #[inline]
    pub fn pop<T: 'static>(&mut self) -> Option<T> {
        assert_eq!(self.type_id, ty!(T));
        unsafe { self.pop_type_unchecked() }
    }

    /// # Safety
    ///
    /// Calling this method with incorrect type causes *undefined behavior*.
    ///
    /// # Panics
    ///
    /// In **DEBUG** mode, safety condition is checked and causes panic when that is met.
    #[inline]
    pub unsafe fn pop_type_unchecked<T: 'static>(&mut self) -> Option<T> {
        debug_assert_eq!(self.type_id, ty!(T));
        unsafe { (*(self.raw as *mut Vec<T>)).pop() }
    }

    /// # Panics
    ///
    /// Calling this method with incorrect type or out of bound `index` causes panic.
    #[inline]
    pub fn swap_remove<T: 'static>(&mut self, index: usize) -> T {
        assert_eq!(self.type_id, ty!(T));
        assert!(index < self.len());
        unsafe { self.swap_remove_type_unchecked(index) }
    }

    /// # Safety
    ///
    /// Calling this method with incorrect type causes *undefined behavior*.
    ///
    /// # Panics
    ///
    /// In **DEBUG** mode, safety condition is checked and causes panic when that is met.
    #[inline]
    pub unsafe fn swap_remove_type_unchecked<T: 'static>(&mut self, index: usize) -> T {
        debug_assert_eq!(self.type_id, ty!(T));
        unsafe { (*(self.raw as *mut Vec<T>)).swap_remove(index) }
    }

    /// # Safety
    ///
    /// Calling this method with incorrect type causes *undefined behavior*.
    ///
    /// # Panics
    ///
    /// In **DEBUG** mode, safety condition is checked and causes panic when that is met.
    #[inline]
    pub unsafe fn resize_with<F, T: 'static>(&mut self, new_len: usize, f: F)
    where
        F: FnMut() -> T,
    {
        debug_assert_eq!(self.type_id, ty!(T));
        unsafe { (*(self.raw as *mut Vec<T>)).resize_with(new_len, f) }
    }
}

impl<'a, T: 'static> From<&'a AnyVec> for &'a Vec<T> {
    /// # Panics
    ///
    /// Calling this method with incorrect type causes panic.
    fn from(value: &'a AnyVec) -> Self {
        assert_eq!(value.type_id, ty!(T));
        unsafe { &*(value.raw as *const Vec<T>) }
    }
}

impl<'a, T: 'static> From<&'a AnyVec> for &'a [T] {
    /// # Panics
    ///
    /// Calling this method with incorrect type causes panic.
    fn from(value: &'a AnyVec) -> Self {
        <&Vec<T>>::from(value)
    }
}

impl<'a, T: 'static> From<&'a mut AnyVec> for &'a mut Vec<T> {
    /// # Panics
    ///
    /// Calling this method with incorrect type causes panic.
    fn from(value: &'a mut AnyVec) -> Self {
        assert_eq!(value.type_id, ty!(T));
        unsafe { &mut *(value.raw as *mut Vec<T>) }
    }
}

impl<'a, T: 'static> From<&'a mut AnyVec> for &'a mut [T] {
    /// # Panics
    ///
    /// Calling this method with incorrect type causes panic.
    fn from(value: &'a mut AnyVec) -> Self {
        <&mut Vec<T>>::from(value)
    }
}

#[derive(Debug, Clone)]
pub struct ChunkVec<T: 'static> {
    chunks: Vec<Vec<T>>,
    chunk_size: PowerOfTwo,
    len: usize,
}

impl<T: 'static> ChunkVec<T> {
    pub fn new(chunk_size: usize) -> Self {
        let mut instance = Self {
            chunks: vec![],
            chunk_size: PowerOfTwo::new(chunk_size).unwrap(),
            len: 0,
        };
        instance.chunks.push(instance.new_chunk());
        instance
    }

    #[inline(always)]
    fn chunk_index(&self, index: usize) -> (usize, usize) {
        (
            self.chunk_size.quotient(index),
            self.chunk_size.remainder(index),
        )
    }

    #[inline(always)]
    fn new_chunk(&self) -> Vec<T> {
        Vec::with_capacity(self.chunk_size.value)
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        self.chunks.len() * self.chunk_size.value
    }

    #[inline]
    pub fn push(&mut self, value: T) {
        let chunk_size = self.chunk_size.value;

        let r = self.chunk_index(self.len.saturating_sub(1)).0;
        // Safety: There is at least one chunk.
        let last_chunk = unsafe { self.chunks.get_unchecked_mut(r) };

        // We can put the value in the `last_chunk`.
        if last_chunk.len() < chunk_size {
            last_chunk.push(value);
        }
        // We can put the value in the next chunk after the `last_chunk`.
        else if let Some(next_chunk) = self.chunks.get_mut(r + 1) {
            next_chunk.push(value);
        }
        // We should allocate a new chunk and put the value in it.
        else {
            let mut new_chunk = self.new_chunk();
            new_chunk.push(value);
            self.chunks.push(new_chunk);
        }
        self.len += 1;
    }

    #[inline]
    pub fn pop(&mut self) -> Option<T> {
        if self.is_empty() {
            None
        } else {
            let (r, _) = self.chunk_index(self.len - 1);
            // Safety: `r` and `c` are valid.
            let last_chunk = unsafe { self.chunks.get_unchecked_mut(r) };

            self.len -= 1;
            last_chunk.pop()
        }
    }

    #[inline]
    pub fn get(&self, index: usize) -> Option<&T> {
        // Safetey: `index` is valid.
        (index < self.len).then(|| unsafe { self.get_unchecked(index) })
    }

    #[inline]
    unsafe fn get_unchecked(&self, index: usize) -> &T {
        let (r, c) = self.chunk_index(index);
        self.chunks.get_unchecked(r).get_unchecked(c)
    }

    #[inline]
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        // Safetey: index is valid.
        (index < self.len).then(|| unsafe { self.get_unchecked_mut(index) })
    }

    #[inline]
    unsafe fn get_unchecked_mut(&mut self, index: usize) -> &mut T {
        let (r, c) = self.chunk_index(index);
        self.chunks.get_unchecked_mut(r).get_unchecked_mut(c)
    }

    #[inline]
    pub fn swap_remove(&mut self, index: usize) -> T {
        assert!(index < self.len);

        // Safety: Valid index guarantees following unsafe blocks are safe.
        let (old_r, old_c) = self.chunk_index(index);
        let (last_r, last_c) = self.chunk_index(self.len - 1);

        self.len -= 1;

        // Swaps.
        if old_r != last_r {
            let [old_chunk, last_chunk] = split_mut(&mut self.chunks, [old_r, last_r]);
            unsafe {
                swap(
                    old_chunk.get_unchecked_mut(old_c),
                    last_chunk.get_unchecked_mut(last_c),
                );
                last_chunk.pop().unwrap_unchecked()
            }
        } else {
            let last_chunk = unsafe { self.chunks.get_unchecked_mut(last_r) };
            if old_c != last_c {
                let [old_v, last_v] = split_mut(last_chunk, [old_c, last_c]);
                swap(old_v, last_v);
            }
            unsafe { last_chunk.pop().unwrap_unchecked() }
        }
    }

    /// Drops redundant chunks.
    pub fn shrink_to_fit(&mut self) {
        // Doesn't drop the first one even if it's empty.
        let chunk_num = self.chunks.len();
        for i in (1..chunk_num).rev() {
            if self.chunks[i].is_empty() {
                self.chunks.pop();
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ChunkVecIter<'a, T: 'static> {
    data: &'a ChunkVec<T>,
    index: usize,
}

impl<'a, T> ChunkVecIter<'a, T> {
    pub fn new(data: &'a ChunkVec<T>) -> Self {
        Self { data, index: 0 }
    }
}

impl<'a, T> Iterator for ChunkVecIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let res = self.data.get(self.index);
        self.index += 1;
        res
    }
}

#[derive(Debug)]
pub struct ChunkVecIterMut<'a, T: 'static> {
    data: &'a mut ChunkVec<T>,
    index: usize,
}

impl<'a, T> ChunkVecIterMut<'a, T> {
    pub fn new(data: &'a mut ChunkVec<T>) -> Self {
        Self { data, index: 0 }
    }
}

impl<'a, T> Iterator for ChunkVecIterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        (self.index < self.data.len()).then(|| {
            let (r, c) = self.data.chunk_index(self.index);
            // Safety: `index` is valid so that r and c are valid as well.
            // And we take a single piece of mutable borrow in here.
            // Other parts can't be borrowed because we borrowed the whole one mutably.
            let item = unsafe {
                let chunk = self.data.chunks.get_unchecked_mut(r);
                let ptr = chunk.as_mut_ptr().add(c);
                &mut *ptr
            };
            self.index += 1;
            item
        })
    }
}

impl<T> Index<usize> for ChunkVec<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl<T> IndexMut<usize> for ChunkVec<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}

/// This doesn't allow duplication and support looking up values in O(1) like hash set.
/// Plus this guarantees inserted order like vector.
/// Similar to [`GenVec`](crate::ds::generational::GenVec), but this doesn't have generation.
/// This is based on [`OptVec`] so that it's also recommended to use NonZero series if it's possible.
#[derive(Debug, Clone)]
pub struct SetVec<T> {
    /// Optional values.
    values: OptVec<T>,

    /// Inverse map.  
    /// Vacant slots are not in here.
    imap: AHashMap<T, usize>,
}

impl<T: Clone + Hash + PartialEq + Eq> SetVec<T> {
    pub fn new() -> Self {
        Self {
            values: OptVec::new(0),
            imap: AHashMap::new(),
        }
    }

    #[inline]
    pub fn contains<Q>(&self, value: &Q) -> bool
    where
        T: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.imap.contains_key(value)
    }

    #[inline]
    pub fn get_index<Q>(&self, value: &Q) -> Option<usize>
    where
        T: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.imap.get(value).cloned()
    }

    pub fn insert(&mut self, value: T) -> usize {
        if let Some(index) = self.get_index(&value) {
            index
        } else {
            let index = self.values.insert(value.clone());
            self.imap.insert(value, index);
            index
        }
    }

    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    pub fn take_by_index(&mut self, index: usize) -> Option<T> {
        let old = self.values.take(index);
        if old.is_some() {
            // Safety: Infallible.
            unsafe {
                self.imap.remove(old.as_ref().unwrap_unchecked());
            }
        }
        old
    }

    pub fn take_by_value<Q>(&mut self, value: &Q) -> Option<T>
    where
        T: std::borrow::Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.take_by_index(self.get_index(value)?)
    }
}

// Do not implement DerefMut because we need to synchronize imap.
impl<T: Clone + Hash + PartialEq + Eq> Deref for SetVec<T> {
    type Target = OptVec<T>;

    fn deref(&self) -> &Self::Target {
        &self.values
    }
}

/// Vector with optional values.  
/// If you don't want to change indices after insert or remove operations,
/// it's a good signal to concern use of this.
/// Plus, this helps you to reuse vacant slots.  
/// Note that it's recommended to use NonZero series as generic argument
/// becuase you can save memery thanks to Rust optimization.
#[derive(Debug, Clone, Default)]
pub struct OptVec<T> {
    /// Optionable values.
    values: Vec<Option<T>>,

    /// Vacant slot list.
    vacancies: AHashSet<usize>,

    offset: usize,
}

impl<T> OptVec<T> {
    /// Slots under `offset` will be filled with None.
    pub fn new(offset: usize) -> Self {
        // values with fixed vacant slots.
        let mut values = Vec::new();
        ntimes!(offset, values.push(None));

        Self {
            values,
            vacancies: AHashSet::new(),
            offset,
        }
    }

    /// Gets total number of slots including vacant slot.
    #[inline]
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Gets number of *occupied* slots.
    /// This is equivalent to [`Self::len`] - [`Self::len_vacant`].
    #[inline]
    pub fn len_occupied(&self) -> usize {
        self.len() - self.len_vacant()
    }

    /// Gets number of vacant slots.
    #[inline]
    pub fn len_vacant(&self) -> usize {
        self.vacancies.len()
    }

    /// Determins whether there's no slots at all.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Determines the slot is None, which means no value in the slot.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    #[inline]
    pub fn is_vacant(&self, index: usize) -> bool {
        self.values[index].is_none()
    }

    /// Determines the slot is Some, which means there's a value in the slot.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    #[inline]
    pub fn is_occupied(&self, index: usize) -> bool {
        self.values[index].is_some()
    }

    #[inline]
    pub fn as_slice(&self) -> &[Option<T>] {
        &self.values
    }

    /// Do not modify Some or None status.
    #[inline]
    pub fn as_slice_mut(&mut self) -> &mut [Option<T>] {
        &mut self.values
    }

    /// Retrieves value located at the `index`.
    /// It can be None if the slot is vacant or `index` is out of bound.
    #[inline]
    pub fn get(&self, index: usize) -> Option<&T> {
        self.values.get(index)?.as_ref()
    }

    /// # Safety
    ///
    /// Undefine behavior if `index` is out of bound or
    /// the slot is vacant.
    pub unsafe fn get_unchecked(&self, index: usize) -> &T {
        self.values.get_unchecked(index).as_ref().unwrap_unchecked()
    }

    /// Gets a value located at the `index`.
    /// It can be None if `index` is out of bound or the slot is vacant.
    #[inline]
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.values.get_mut(index)?.as_mut()
    }

    /// # Safety
    ///
    /// Undefine behavior if `index` is out of bound or
    /// the slot is vacant.
    pub unsafe fn get_unchecked_mut(&mut self, index: usize) -> &mut T {
        self.values
            .get_unchecked_mut(index)
            .as_mut()
            .unwrap_unchecked()
    }

    /// Sets value wrapped with Option itself.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    pub fn set(&mut self, index: usize, value: Option<T>) -> Option<T> {
        if value.is_some() {
            self.vacancies.remove(&index);
        } else {
            self.vacancies.insert(index);
        }
        std::mem::replace(&mut self.values[index], value)
    }

    /// Inserts the value to a vacant slot if it's possible or
    /// put it at the end of values.
    pub fn insert(&mut self, value: T) -> usize {
        if let Some(index) = self.vacancies.iter().next() {
            let index = *index;
            self.vacancies.remove(&index);
            self.values[index] = Some(value);
            index
        } else {
            self.values.push(Some(value));
            self.values.len() - 1
        }
    }

    /// Appends the *Optional* value at the end of the vector.
    pub fn push(&mut self, value: Option<T>) {
        if value.is_none() {
            self.vacancies.insert(self.values.len());
        }
        self.values.push(value);
    }

    /// Takes the value out from the slot located at index, then returns the value.  
    /// It'd be None if the slot was vacant.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bound.
    pub fn take(&mut self, index: usize) -> Option<T> {
        let old = self.values[index].take()?; 
        if index >= self.offset {
            self.vacancies.insert(index);
        }
        Some(old)
    }

    /// Swaps two occupied items.
    ///
    /// # Panics
    ///
    /// Panics if `a` or `b` are out of bounds or vacant.
    pub fn swap_occupied(&mut self, a: usize, b: usize) {
        assert!(self.is_occupied(a) && self.is_occupied(b));
        self.values.swap(a, b);
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (usize, &Option<T>)> {
        self.values.iter().enumerate()
    }

    pub fn iter_occupied(&self) -> impl Iterator<Item = (usize, &T)> {
        self.iter().filter_map(|(i, v)| v.as_ref().map(|v| (i, v)))
    }

    pub fn iter_occupied_mut(&mut self) -> impl Iterator<Item = (usize, &mut T)> {
        self.values
            .iter_mut()
            .enumerate()
            .filter_map(|(i, v)| v.as_mut().map(|v| (i, v)))
    }

    // `Option<T>` is not supposed to be modified without concerning vacancies.
    // That's why no iter_mut().
    /// Returns an iterator traversing over all types of slots.
    #[inline]
    pub fn values(&self) -> std::slice::Iter<'_, Option<T>> {
        self.values.iter()
    }

    /// Returns an iterator traversing only *occupied* slots.
    /// Use [`Self::values`] to iterate over all slots including vacant slots.
    pub fn values_occupied(&self) -> impl Iterator<Item = &T> + Clone {
        self.values().filter_map(|v| v.as_ref())
    }

    // This operation is allowed because it can't take inner value inside Option.
    /// Returns an iterator traversing only *occupied* slots.
    /// Use [`Self::values`] to iterate over all slots including vacant slots.
    pub fn values_occupied_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.values.iter_mut().filter_map(|v| v.as_mut())
    }

    /// Removes some slots from the end, so that `len` slots will remain after that.
    /// It does nothing if `len` is equal to or grater than currenet length.
    /// Note that you can't truncate less than offset you set.
    pub fn truncate(&mut self, len: usize) {
        for index in len.max(self.offset)..self.values.len() {
            if self.values.pop().is_none() {
                self.vacancies.remove(&index);
            }
        }
    }

    /// Shrinks the capacity.
    #[inline]
    pub fn shrink_to_fit(&mut self) {
        self.values.shrink_to_fit();
        self.vacancies.shrink_to_fit();
    }
}

// Do not implement IndexMut because we need to modify vacancies if users take the value from the slot.
impl<T> Index<usize> for OptVec<T> {
    type Output = Option<T>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.values[index]
    }
}

// TODO: Test
// TODO: Alignment?, default is 1
// TODO: Automatical defragmmentation
/// Vector containing varying sized chunks.
/// All chunks lay on a single vector.
/// When you put an item in the buffer, only the corresponding chunk may move,
/// not making move all follwing items. Therefore, there can be fragmentations.
#[derive(Debug, Clone)]
pub struct VarChunkVec<T> {
    /// Chunk views. You can find chunk using the [`ChunkView`].
    /// It's guaranteed that index of each view won't change.
    views: OptVec<ChunkView>,

    /// Fragment list.
    /// Assumes that there's no much fragments.
    frags: Vec<usize>,

    /// Buffer index to view index, which is used to find next view.
    /// It always points to the valid [`ChunkView`].
    deref: AHashMap<usize, usize>,

    /// Raw data.
    buf: Vec<T>,
}

impl<T> VarChunkVec<T> {
    #[inline]
    pub fn new() -> Self {
        Self::with_capacity(0)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            views: OptVec::new(0),
            frags: Vec::new(),
            deref: AHashMap::new(),
            buf: Vec::with_capacity(capacity),
        }
    }

    /// Retrieves length of the buffer, which contains all chunks and fragments.
    #[inline]
    pub fn len(&self) -> usize {
        self.buf.len()
    }

    /// Determines the buffer is empty or not.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns iterator visiting all chunks and fragments.
    /// Use [`Self::chunks_occupied`] if you want to skip fragments.
    pub fn iter(&self) -> impl Iterator<Item = &[T]> {
        self.views.values_occupied().map(|view| &self.buf[view.range()])
    }

    /// Returns iterator visiting all chunks only, not fragments.
    pub fn chunks(&self) -> impl Iterator<Item = &[T]> {
        self.views
            .values_occupied()
            .filter_map(|view| (!view.is_fragment()).then_some(&self.buf[view.range()]))
    }

    /// Gets all range of buffer as a slice.
    /// Note that the slice includes not only chunks but fragments as well.
    /// If you want a specific chunk slice, use [`Self::as_chunk`].
    #[inline]
    pub fn as_slice(&self) -> &[T] {
        &self.buf
    }

    /// Retrieves chunk by chunk index.
    ///
    /// # Panics
    ///
    /// - `ci` is out of bound.
    /// - `ci` points to a vacant slot.
    /// - In debug mode only, `ci` points to a fragment.
    #[inline]
    pub fn as_chunk(&self, ci: usize) -> &[T] {
        let view = self.views.get(ci).unwrap();
        debug_assert!(!view.is_fragment());
        &self.buf[view.range()]
    }

    /// Retrieves chunk by chunk index.
    ///
    /// # Panics
    ///
    /// - `ci` is out of bound.
    /// - `ci` points to a vacant slot.
    /// - In debug mode only, `ci` points to a fragment.
    #[inline]
    pub fn as_chunk_mut(&mut self, ci: usize) -> &mut [T] {
        let view = self.views.get(ci).unwrap();
        debug_assert!(!view.is_fragment());
        &mut self.buf[view.range()]
    }

    /// Retrieves chunk length.  
    /// But if you put in a fragment index, it'll be length of the fragment.
    /// 
    /// # Panics
    ///
    /// - `ci` is out of bound.
    /// - `ci` points to a vacant slot.
    #[inline]
    pub fn chunk_len(&self, ci: usize) -> usize {
        self.views.get(ci).unwrap().len
    }

    /// Retrieves item by chunk and item indices.
    ///
    /// # Panics
    ///
    /// Panics if any indices are out of bounds.
    #[inline]
    pub fn get_item(&self, ci: usize, ii: usize) -> &T {
        &self.as_chunk(ci)[ii]
    }

    /// Retrieves item by chunk and item indices.
    ///
    /// # Panics
    ///
    /// Panics if any indices are out of bounds.
    #[inline]
    pub fn get_item_mut(&mut self, ci: usize, ii: usize) -> &mut T {
        &mut self.as_chunk_mut(ci)[ii]
    }

    /// Creates a chunk and adds `value` immediately.
    /// Returns the chunk index.
    pub fn push_item(&mut self, value: T) -> usize {
        if let Some(fi) = self.find_fragment(1) {
            // Unregisters `fi` from fragment list.
            self.unregister_fragment(fi);

            // Edits this view from fragment to chunk.
            let view = unsafe { self.views.get_unchecked_mut(fi) };
            view.frag = false;
            self.buf[view.offset] = value;
            unsafe { self.cut_chunk_unchecked(fi, 1) };

            fi
        } else {
            self.buf.push(value);
            self.create_view(self.buf.len() - 1, 1, false)
        }
    }

    /// Cuts the chunk out of two chunks such that left one will have `left` items,
    /// and right one will have left itmes as a fragment.
    /// But if `left` is greater or equal to the current chunk size, nothing takes place.
    ///
    /// # Safety
    ///
    /// Undefined behavior if `ci` is out of bound or the view is vacant.
    unsafe fn cut_chunk_unchecked(&mut self, ci: usize, left: usize) {
        let view = self.views.get_unchecked_mut(ci);
        if view.len > left {
            let roff = view.offset + left;
            let rlen = view.len - left;
            view.len = left;
            self.create_view(roff, rlen, true);
        }
    }

    fn create_view(&mut self, offset: usize, len: usize, frag: bool) -> usize {
        let fi = self.views.insert(ChunkView::new(offset, len, frag));
        self.deref.insert(offset, fi);
        if frag {
            self.register_fragment(fi);
        }
        fi
    }

    #[inline]
    fn register_fragment(&mut self, fi: usize) {
        self.frags.push(fi);
    }

    fn unregister_fragment(&mut self, fi: usize) {
        for i in 0..self.frags.len() {
            if self.frags[i] == fi {
                self.frags.swap_remove(i);
                break;
            }
        }
    }

    /// Retrieves the next chunk index in buffer perspective.
    fn get_next_chunk_index(&self, ci: usize) -> Option<usize> {
        let view = self.views.get(ci)?;
        self.deref.get(&view.end()).cloned()
    }

    /// Retrieves a fragment that has enough space using linear search.
    fn find_fragment(&self, len: usize) -> Option<usize> {
        // Safety: `self.frags` always has occupied indices.
        self.frags
            .iter()
            .filter(|&&fi| unsafe { self.views.get(fi).unwrap_unchecked().len >= len })
            .next()
            .cloned()
    }

    /// Shrinks fragment from left side by `amount`.
    /// Note that this assumes you put in a fragment index.
    ///
    /// # Panics
    ///
    /// Panics if `amount` is greater than the current fragement size.
    ///
    /// # Safety
    ///
    /// Undefined behavior if `fi` is out of bound or the slot is vacant.
    unsafe fn shrink_rev_unchecked(&mut self, fi: usize, amount: usize) {
        let view = self.views.get_unchecked_mut(fi);
        self.deref.remove(&view.offset);
        view.shrink_rev(amount);
        if view.is_empty() {
            self.views.take(fi);
            self.unregister_fragment(fi);
        } else {
            self.deref.insert(view.offset + amount, fi);
        }
    }

    /// Appends the `value` to the chunk pointed by `ci`.
    ///
    /// # Safety
    ///
    /// Undefined behavior if `ci` is out of bound or the slot is vacant.
    unsafe fn push_item_unchecked(&mut self, ci: usize, value: T) {
        let view = unsafe { self.views.get_unchecked_mut(ci) };
        view.len += 1;
        view.frag = false;
        if view.end() <= self.buf.len() {
            self.buf[view.end() - 1] = value;
        } else {
            self.buf.push(value);
        }
    }

    /// Tries to append the `value` using the next fragment's space.
    /// If it succedded, returns true.
    fn push_item_using_fragment(&mut self, ci: usize, value: T) -> bool {
        if let Some(nci) = self.get_next_chunk_index(ci) {
            // Safety: `ci` and `nci` are valid, so corresponding views are valid as well.
            unsafe {
                let nview = self.views.get_unchecked_mut(nci);
                if nview.is_fragment() && !nview.is_empty() {
                    self.shrink_rev_unchecked(nci, 1);
                    self.push_item_unchecked(ci, value);
                }
            };
            true
        } else {
            false
        }
    }
}

impl<T: Default + Copy> VarChunkVec<T> {
    /// Appends the `value` at the end of the chunk.
    /// 
    /// # Panics
    ///
    /// Panics if `ci` is out of bound or points to a fragment or vacant slot.
    pub fn push_item_to_chunk(&mut self, ci: usize, value: T) {
        // Checks.
        let view = *self.views.get(ci).unwrap();
        assert!(!view.frag);

        // First, tries to use the next fragment.
        if self.push_item_using_fragment(ci, value) {
            return;
        }

        // If it failed, tries to use enough large fragment.
        let (fi, ii) = if let Some(fi) = self.find_fragment(view.len + 1) {
            // Safety: `fi` is valid.
            let frag_offset = unsafe { self.views.get(fi).unwrap_unchecked().offset };
            self.buf.copy_within(view.range(), frag_offset);
            (fi, frag_offset + view.len)
        } else {
            // If it failed, makes new fragment.
            let fi = self.create_view(self.buf.len(), view.len + 1, true);
            self.extend_buffer(view.len + 1, view.range());
            (fi, self.buf.len() - 1)
        };

        // Appends `value`.
        self.buf[ii] = value;

        // Swap views.
        self.views.swap_occupied(ci, fi);

        // Adjust length.
        unsafe { self.cut_chunk_unchecked(ci, view.len + 1) };
    }

    /// # Panics
    ///
    /// Panics if `src` range is greater than `len`.
    fn extend_buffer(&mut self, len: usize, src: Range<usize>) {
        let diff = len - src.len();
        self.buf.reserve_exact(len);
        self.buf.extend_from_within(src);
        let defaults = repeat(T::default()).take(diff);
        self.buf.extend(defaults);
    }

    // /// It's recommended to put all data in at once to avoid fragmentations.
    // /// To do so, you can reserve some space first.
    // /// If the `key` is new one or `len` is greater than old one,
    // /// only newly extended slots are filled with default values.
    // /// Call [`Self::get_chunk_mut`] to set actual data to the chunk.
    // pub fn reserve_chunk(&mut self, key: K, len: usize) {
    //     // Prevents empty values.
    //     assert!(len > 0);

    //     if let Some(vi) = self.map.get(&key).cloned() {
    //         // Expands chunk.
    //     } else {
    //         self.map.insert(key, self.views.len());
    //         self.views.push(ChunkView::new_occupied(self.buf.len(), len));
    //         self.extend_buffer(len, 0..0);
    //     }
    // }

    // pub fn insert_item<Q>(&mut self, key: &Q, value: V) -> Option<usize>
    // where
    //     K: Borrow<Q>,
    //     Q: Hash + Eq + ?Sized,
    // {
    //     if let Some(vi) = self.map.get(key).cloned() {
    //         let new_len = self.views[vi].len + 1;
    //         self.resize_chunk(vi, new_len);
    //         let last = self.views[vi].offset + new_len - 1;
    //         self.buf[last] = value;
    //         Some(new_len - 1)
    //     } else {
    //         None
    //     }
    // }

    // /// Removes all fragments by moving occupied chunks to the front of the buffer.
    // pub fn defragment(&mut self) {
    //     // Brings all occupied chunks to the front.
    //     let n = self.views.len();
    //     let mut cur = 0;
    //     let mut l = 0;
    //     for r in 0..n {
    //         match &mut self.views[r] {
    //             ChunkView::Occupied(view) => {
    //                 if cur < view.offset {
    //                     self.buf.copy_within(view.range(), cur);
    //                     view.offset = cur;
    //                 }
    //                 cur += view.len;
    //                 self.views.swap(l, r);
    //                 l += 1;
    //             },
    //             ChunkView::Fragment(_) => {},
    //         }
    //     }

    //     // Removes fragments.
    //     self.views.truncate(l);

    //     // Reduces the length, but keeps the capacity.
    //     self.buf.truncate(cur);

    //     self.frag_size = 0;
    // }

    // /// Resizes the chunk such that the chunk has `len` items within it.
    // /// If `len` is greater than the currrent length, there can be filled with garbage values.
    // /// Otherwise, length is shrinked, but items may not be dropped.
    // fn resize_chunk(&mut self, vi: usize, len: usize) {
    //     let cur_len = self.views[vi].len;
    //     if cur_len < len {
    //         let diff = len - cur_len;
    //         match self.views.get(vi + 1) {
    //             // Am I the last one? then extend myself only.
    //             None => {
    //                 self.views[vi].len = len;
    //                 self.extend_buffer(diff, 0..0);
    //             }
    //             // Can use the next fragment? then adjust views only.
    //             Some(ChunkView::Fragment(next)) if diff <= next.len => {
    //                 self.views[vi].len = len;
    //                 self.views[vi + 1].shrink_rev(diff);
    //             },
    //             // Hard to extend in-place. Move it.
    //             _ => {
    //                 let mut view = self.views[vi].into_fragment();
    //                 view.reset(self.buf.len(), len);
    //                 self.views.push(view);
    //                 self.extend_buffer(len, self.views[vi].range());
    //             }
    //         }
    //     } else { // Shrink or do nothing
    //         todo!()
    //     }
    // }
}

impl<'a, T: bytemuck::Pod> From<&'a VarChunkVec<T>> for &'a [u8] {
    fn from(value: &'a VarChunkVec<T>) -> Self {
        bytemuck::cast_slice(value.as_slice())
    }
}

#[derive(Debug, Clone, Copy)]
struct ChunkView {
    /// A range of buffer.
    win: Window,

    /// If true, it means this chunk is a fragmment.
    frag: bool,
}

impl ChunkView {
    /// Creates a new view.
    #[inline]
    pub fn new(offset: usize, len: usize, frag: bool) -> Self {
        Self {
            win: Window::new(offset, len),
            frag,
        }
    }

    /// Determines whether this is a fragment or not.
    #[inline]
    pub fn is_fragment(&self) -> bool {
        self.frag
    }
}

impl Deref for ChunkView {
    type Target = Window;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.win
    }
}

impl DerefMut for ChunkView {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.win
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_bindgen_test::*;

    wasm_bindgen_test_configure!(run_in_browser);

    #[derive(PartialEq, Debug, Clone, Copy, Default)]
    struct SA {
        x: [usize; 2],
    }

    #[derive(PartialEq, Debug, Clone, Copy, Default)]
    struct SB {
        x: [usize; 2],
        y: [f32; 2],
    }

    #[wasm_bindgen_test]
    fn test_anyvec_push_pop() {
        let mut a = AnyVec::new::<SA>();
        assert_eq!(true, a.is_empty());

        a.push(SA { x: [0, 1] });
        assert_eq!(1, a.len());
        assert!(a.capacity() >= 1);
        assert_eq!(false, a.is_empty());

        a.push(SA { x: [2, 3] });
        assert_eq!(2, a.len());
        assert!(a.capacity() >= 2);
        assert_eq!(false, a.is_empty());

        assert_eq!(Some(SA { x: [2, 3] }), a.pop::<SA>());
        assert_eq!(1, a.len());
        assert!(a.capacity() >= 1);
        assert_eq!(false, a.is_empty());

        assert_eq!(Some(SA { x: [0, 1] }), a.pop::<SA>());
        assert_eq!(0, a.len());
        assert_eq!(true, a.is_empty());

        assert_eq!(None, a.pop::<SA>());
    }

    #[wasm_bindgen_test]
    fn test_anyvec_remove() {
        let mut a = AnyVec::new::<SA>();

        a.push(SA { x: [0, 1] });
        a.push(SA { x: [2, 3] });
        a.push(SA { x: [4, 5] });
        a.push(SA { x: [6, 7] });

        let removed = a.swap_remove(1);
        assert_eq!(SA { x: [2, 3] }, removed);
        assert_eq!(3, a.len());
        assert_eq!(Some(&SA { x: [0, 1] }), a.get(0));
        assert_eq!(Some(&SA { x: [6, 7] }), a.get(1));
        assert_eq!(Some(&SA { x: [4, 5] }), a.get(2));
    }

    #[wasm_bindgen_test]
    #[should_panic]
    fn test_anyvec_push_incorrect_type_panic() {
        let mut a = AnyVec::new::<SA>();
        a.push(SB {
            x: [0, 1],
            y: [0.1, 0.2],
        });
    }

    #[wasm_bindgen_test]
    #[should_panic]
    fn test_anyvec_pop_incorrect_type_panic() {
        let mut a = AnyVec::new::<SB>();
        a.push(SB {
            x: [0, 1],
            y: [0.1, 0.2],
        });
        let _ = a.pop::<SA>();
    }

    #[wasm_bindgen_test]
    fn test_anyvec_into_vec_push_pop() {
        let mut a = AnyVec::new::<SA>();
        let v: &mut Vec<SA> = (&mut a).into();
        v.push(SA { x: [0, 1] });
        v.push(SA { x: [2, 3] });
        assert_eq!(Some(SA { x: [2, 3] }), v.pop());
        assert_eq!(Some(SA { x: [0, 1] }), a.pop::<SA>());
        assert_eq!(None, a.pop::<SA>());

        let v: &mut Vec<SA> = (&mut a).into();
        v.push(SA { x: [0, 1] });
        v.push(SA { x: [2, 3] });
        let v_imm: &Vec<SA> = (&a).into();
        assert_eq!(Some(&SA { x: [0, 1] }), v_imm.get(0));
        assert_eq!(Some(&SA { x: [2, 3] }), v_imm.get(1));
    }

    #[wasm_bindgen_test]
    #[should_panic]
    fn test_anyvec_into_incorrect_type_panic() {
        let mut a = AnyVec::new::<SA>();
        let _: &mut Vec<SB> = (&mut a).into();
    }

    #[wasm_bindgen_test]
    fn test_chunkvec_push_pop() {
        let chunk_num = 2;
        let chunk_size = 2;
        let mut v = ChunkVec::new(chunk_size);
        for r in 0..chunk_num {
            let mut expect = vec![];
            for c in 0..chunk_size {
                let value = r * chunk_size + c;
                v.push(value);
                expect.push(value);

                let index = r * chunk_size + c;
                assert_eq!(expect, v.chunks[r]);
                assert_eq!(expect[c], v[index]);
                assert_eq!(index + 1, v.len());
            }
            assert_eq!(expect.len(), v.chunks[r].len());
        }

        for r in (0..chunk_num).rev() {
            let mut expect = v.chunks[r].clone();
            for _c in 0..chunk_size {
                assert_eq!(expect, v.chunks[r]);

                let expect_val = expect.pop();
                let popped_val = v.pop();

                assert_eq!(expect_val, popped_val);
            }
        }

        assert_eq!(chunk_num * chunk_size, v.capacity());
        assert_eq!(
            v.capacity(),
            v.chunks.iter().map(|chunk| chunk.capacity()).sum()
        );

        v.shrink_to_fit();
        assert_eq!(chunk_size, v.capacity());
    }

    #[wasm_bindgen_test]
    fn test_chunkvec_swapremove() {
        let chunk_size = 8;
        let item_num = 13;
        let chunk_num = (item_num as f32 / chunk_size as f32).ceil() as usize;
        let mut v = ChunkVec::new(chunk_size);
        let mut expect = vec![];
        for i in 0..item_num {
            v.push(i);
            expect.push(i);
        }

        enum Pos {
            Start,
            Middle,
            End,
        }

        let mut pos = Pos::Start;
        for _i in 0..item_num {
            let index = match pos {
                Pos::Start => {
                    pos = Pos::Middle;
                    0
                }
                Pos::Middle => {
                    pos = Pos::End;
                    v.len() / 2
                }
                Pos::End => {
                    pos = Pos::Start;
                    v.len() - 1
                }
            };
            let expect_val = expect.swap_remove(index);
            let popped_val = v.swap_remove(index);

            assert_eq!(expect_val, popped_val);
            assert_eq!(expect.len(), v.len());
            for j in 0..v.len() {
                assert_eq!(expect[j], v[j]);
            }
        }

        assert!(v.is_empty());
        assert_eq!(chunk_num * chunk_size, v.capacity());

        v.shrink_to_fit();
        assert_eq!(chunk_size, v.capacity());
        assert_eq!(
            v.capacity(),
            v.chunks.iter().map(|chunk| chunk.capacity()).sum()
        );
    }
}
