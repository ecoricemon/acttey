use std::{fmt, hash};

// Main purpose of this triat is to determine data type in `DedupVec`
// by generic parameter `ON` based on associted types.
// Methods, on the other hand, help us not to make mistakes defining
// different methods from impl to impl.
pub trait AsDedupVec {
    type Item;
    type Container;

    fn new() -> Self;
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
    fn contains(&self, value: &Self::Item) -> bool;
    fn push(&mut self, value: Self::Item);
    fn remove(&mut self, value: &Self::Item) -> Option<Self::Item>;
    fn extend(&mut self, iter: impl IntoIterator<Item = Self::Item>);
    fn iter(&self) -> impl Iterator<Item = &Self::Item>;
}

// ON = true means deduplication is activated.
// In this case, we keep the vector sorted and deduplicated whenever
// insertion or removal occurs.
impl<T> AsDedupVec for DedupVec<T, true>
where
    T: Ord,
{
    type Item = T;
    type Container = Vec<T>;

    fn new() -> Self {
        DedupVec { inner: Vec::new() }
    }

    fn len(&self) -> usize {
        self.inner.len()
    }

    fn contains(&self, value: &Self::Item) -> bool {
        self.inner.binary_search(value).is_ok()
    }

    fn push(&mut self, value: Self::Item) {
        if let Err(i) = self.inner.binary_search(&value) {
            self.inner.insert(i, value);
        }
    }

    fn remove(&mut self, value: &Self::Item) -> Option<Self::Item> {
        if let Ok(i) = self.inner.binary_search(value) {
            Some(self.inner.remove(i))
        } else {
            None
        }
    }

    fn extend(&mut self, iter: impl IntoIterator<Item = Self::Item>) {
        self.inner.extend(iter);
        self.inner.sort_unstable();
        self.inner.dedup();
    }

    fn iter(&self) -> impl Iterator<Item = &Self::Item> {
        self.inner.iter()
    }
}

// ON = false means deduplication is not activated.
// In release mode, we don't check duplication at all.
// Clients have responsibility for keeping deduplicated state.
#[cfg(not(debug_assertions))]
impl<T> AsDedupVec for DedupVec<T, false>
where
    T: hash::Hash + Eq + Clone,
{
    type Item = T;
    type Container = Vec<T>;

    fn new() -> Self {
        Self { inner: Vec::new() }
    }

    fn len(&self) -> usize {
        self.inner.len()
    }

    fn contains(&self, value: &Self::Item) -> bool {
        self.inner.iter().any(|v| v == value)
    }

    fn push(&mut self, value: Self::Item) {
        self.inner.push(value);
    }

    fn remove(&mut self, value: &Self::Item) -> Option<Self::Item> {
        if let Some((i, _)) = self.inner.iter().enumerate().find(|(_, x)| *x == value) {
            Some(self.inner.swap_remove(i))
        } else {
            None
        }
    }

    fn extend(&mut self, iter: impl IntoIterator<Item = Self::Item>) {
        self.inner.extend(iter);
    }

    fn iter(&self) -> impl Iterator<Item = &Self::Item> {
        self.inner.iter()
    }
}

// ON = false means deduplication is not activated.
// However, in debug mode, panics if the container gets duplicate items in it.
#[cfg(debug_assertions)]
impl<T> AsDedupVec for DedupVec<T, false>
where
    T: hash::Hash + Eq + Default + Clone,
{
    type Item = T;
    type Container = crate::ds::list::SetValueList<T, hash::RandomState>;

    fn new() -> Self {
        Self {
            inner: crate::ds::list::SetValueList::new_with_default(),
        }
    }

    fn len(&self) -> usize {
        self.inner.len_occupied()
    }

    fn contains(&self, value: &Self::Item) -> bool {
        self.inner.contains_key(value)
    }

    fn push(&mut self, value: Self::Item) {
        assert!(
            !self.contains(&value),
            "duplicated items detected while DedupVec is inactive"
        );

        self.inner.push_back(value);
    }

    fn remove(&mut self, value: &Self::Item) -> Option<Self::Item> {
        self.inner.remove(value)
    }

    fn extend(&mut self, iter: impl IntoIterator<Item = Self::Item>) {
        for value in iter {
            self.push(value);
        }
    }

    fn iter(&self) -> impl Iterator<Item = &Self::Item> {
        self.inner.values()
    }
}

/// Deduplicated vector.
///
/// - If ON is true, then the vector keeps sorted and deduplicated items in it.
///   It means the vector will conduct sorting and binary search to keep the state.
///   In this case, [`Vec`] is used as data container with automatic sorting.
///
/// - If ON is false, on the other hand, the vector acts differently according to build mode.  
///   In *debug mode*, the vector will panic when duplication detected.
///   [`SetValueList`] is used as data container in this case.  
///   In contrast, in *release mode*, the vector is just [`Vec`] without
///   duplication checks. So it's totally client's role to keep the deduplicated state.  
///   Note that it's unsorted, which means items are placed in inserted order
///   regardless of build mode.
///
/// If your purpose is debugging and you keeps deduplicated state, then turn it off.
/// You can turn it on when you want an always deduplicated vector.
#[repr(transparent)]
pub struct DedupVec<T, const ON: bool = true>
where
    Self: AsDedupVec,
{
    inner: <Self as AsDedupVec>::Container,
}

impl<T, const ON: bool> fmt::Debug for DedupVec<T, ON>
where
    Self: AsDedupVec,
    <Self as AsDedupVec>::Container: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "DedupVec({:?})", self.inner)
    }
}

impl<T> From<DedupVec<T, true>> for Vec<T>
where
    T: Ord,
{
    fn from(value: DedupVec<T, true>) -> Self {
        value.inner
    }
}

#[cfg(not(debug_assertions))]
impl<T> From<DedupVec<T, false>> for Vec<T>
where
    T: hash::Hash + Eq + Clone,
{
    fn from(value: DedupVec<T, false>) -> Self {
        value.inner
    }
}

#[cfg(debug_assertions)]
impl<T> From<DedupVec<T, false>> for Vec<T>
where
    T: hash::Hash + Eq + Default + Clone,
{
    fn from(value: DedupVec<T, false>) -> Self {
        value.inner.into()
    }
}
