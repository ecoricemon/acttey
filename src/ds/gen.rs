use super::vec::OptVec;
use crate::util::With;
use std::hash::BuildHasher;

#[derive(Debug, Clone, Default)]
pub struct GenVec<T, S> {
    values: OptVec<With<T, u64>, S>,
    gen: u64,
}

impl<T, S> GenVec<T, S>
where
    S: Default,
{
    pub fn new() -> Self {
        Self {
            values: OptVec::new(),
            gen: 1,
        }
    }
}

impl<T, S> GenVec<T, S>
where
    S: BuildHasher,
{
    pub fn add(&mut self, value: T) -> With<usize, u64> {
        let value_gen = self.gen;
        self.gen += 1;
        let value = With::new(value, value_gen);
        let index = self.values.add(value);
        With::new(index, value_gen)
    }

    pub fn remove(&mut self, index: With<usize, u64>) -> Option<T> {
        if self.is_valid_index(index) {
            // Safety: Checked.
            let with_gen = unsafe { self.values.take(index.value).unwrap_unchecked() };
            Some(with_gen.value)
        } else {
            None
        }
    }

    pub fn get(&self, index: With<usize, u64>) -> Option<&T> {
        if self.is_valid_index(index) {
            // Safety: Checked.
            let with_gen = unsafe { self.values.get(index.value).unwrap_unchecked() };
            Some(&with_gen.value)
        } else {
            None
        }
    }

    pub fn get_mut(&mut self, index: With<usize, u64>) -> Option<&mut T> {
        if self.is_valid_index(index) {
            // Safety: Checked.
            let with_gen = unsafe { self.values.get_mut(index.value).unwrap_unchecked() };
            Some(&mut with_gen.value)
        } else {
            None
        }
    }

    fn is_valid_index(&self, index: With<usize, u64>) -> bool {
        let With { value: i, with: g } = index;
        matches!(self.values.get(i), Some(With { with, .. }) if *with == g)
    }
}
