use std::sync::atomic::{AtomicI32, Ordering};

pub trait Atomic<T> {
    fn new(value: T) -> Self;
    fn load(&self, order: Ordering) -> T;
    fn add(&self, value: T, order: Ordering) -> T;
    fn sub(&self, value: T, order: Ordering) -> T;
    fn compare_exchange(
        &self,
        expect: T,
        replace: T,
        success: Ordering,
        failure: Ordering,
    ) -> Result<T, T>;
}

impl Atomic<i32> for AtomicI32 {
    fn new(value: i32) -> Self {
        Self::new(value)
    }

    fn load(&self, order: Ordering) -> i32 {
        self.load(order)
    }

    fn add(&self, value: i32, order: Ordering) -> i32 {
        self.fetch_add(value, order)
    }

    fn sub(&self, value: i32, order: Ordering) -> i32 {
        self.fetch_sub(value, order)
    }

    fn compare_exchange(
        &self,
        expect: i32,
        replace: i32,
        success: Ordering,
        failure: Ordering,
    ) -> Result<i32, i32> {
        self.compare_exchange(expect, replace, success, failure)
    }
}
