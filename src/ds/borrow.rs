use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut},
    process,
    ptr::NonNull,
    sync::atomic::{
        self, AtomicI32,
        Ordering::{self, Acquire, Relaxed, Release},
    },
    thread,
};

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
    ) -> Result<i32, i32>;
}

impl Atomic<i32> for AtomicI32 {
    #[inline]
    fn new(value: i32) -> Self {
        Self::new(value)
    }

    #[inline]
    fn load(&self, order: Ordering) -> i32 {
        self.load(order)
    }

    #[inline]
    fn add(&self, value: i32, order: Ordering) -> i32 {
        self.fetch_add(value, order)
    }

    #[inline]
    fn sub(&self, value: i32, order: Ordering) -> i32 {
        self.fetch_sub(value, order)
    }

    #[inline]
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

#[derive(Debug)]
pub enum BorrowError {
    /// Exclusive borrowing is only allowed when no one has borrowed it.
    ExclusiveFailed,

    /// For detecting anomaly, there exist limit you can immutably borrow.
    TooManyBorrow,

    /// If someone tried to borrow with out of bound index.
    OutOfBound,
}

/// Holding a thing within this and borrow it as [`Borrowed`].
/// Multiple immutable borrowing is allowed, but mutable borrowing is exclusive.
/// This detects dropping `Borrowed`, then this causes panic or abort if this is dropped while any `Borrowed` is still alive.
/// You can check it out using [`Holder::borrow_count`] to see there's any `Borrowed`.
#[derive(Debug)]
pub struct Holder<V, BI, BM, A: Atomic<i32> = atomic::AtomicI32> {
    value: V,
    atomic_cnt: NonNull<A>,
    fn_imm: fn(&V) -> BI,
    fn_mut: fn(&mut V) -> BM,
    _marker: PhantomData<(BI, BM)>,
}

impl<V, BI, BM, A: Atomic<i32>> Holder<V, BI, BM, A> {
    const CNT_INIT: i32 = 0;
    const CNT_EXC: i32 = -1;
    const CNT_MAX: i32 = 1024;

    pub fn new(value: V, fn_imm: fn(&V) -> BI, fn_mut: fn(&mut V) -> BM) -> Self {
        Self {
            value,
            atomic_cnt: NonNull::new(Box::leak(Box::new(A::new(Self::CNT_INIT)))).unwrap(),
            fn_imm,
            fn_mut,
            _marker: PhantomData,
        }
    }

    pub fn get_fn_imm(&self) -> fn(&V) -> BI {
        self.fn_imm
    }

    pub fn get_fn_mut(&self) -> fn(&mut V) -> BM {
        self.fn_mut
    }

    pub fn borrow(&self) -> Result<Borrowed<BI, A>, BorrowError> {
        let atomic = unsafe { self.atomic_cnt.as_ref() };
        let old = atomic.add(1, Relaxed);
        if old > Self::CNT_MAX {
            atomic.sub(1, Relaxed);
            Err(BorrowError::TooManyBorrow)
        } else if old == Self::CNT_EXC {
            atomic.sub(1, Relaxed);
            Err(BorrowError::ExclusiveFailed)
        } else {
            Ok(Borrowed {
                borrow_value: (self.fn_imm)(&self.value),
                exclusive: false,
                atomic_cnt: self.atomic_cnt,
            })
        }
    }

    pub fn borrow_mut(&mut self) -> Result<Borrowed<BM, A>, BorrowError> {
        self._borrow_mut()?;
        Ok(Borrowed {
            borrow_value: (self.fn_mut)(&mut self.value),
            exclusive: true,
            atomic_cnt: self.atomic_cnt,
        })
    }

    /// # Notice
    ///
    /// Inner value may be in the middle of modification in other threads.
    #[inline]
    pub fn get(&self) -> &V {
        &self.value
    }

    pub fn get_mut(&mut self) -> Result<Borrowed<&mut V, A>, BorrowError> {
        self._borrow_mut()?;
        Ok(Borrowed {
            borrow_value: &mut self.value,
            exclusive: true,
            atomic_cnt: self.atomic_cnt,
        })
    }

    /// Retrieves current reference count.
    /// If it is greater than zero, it means there exist immutable [`Borrowed`].
    /// If it is [`Self::CNT_EXC`], it means there exist mutable `Borrowed`.
    /// Otherwise, in other words, it's zero, there's no `Borrowed`.
    pub fn borrow_count(&self) -> i32 {
        let atomic = unsafe { self.atomic_cnt.as_ref() };
        atomic.load(Relaxed)
    }

    fn _borrow_mut(&mut self) -> Result<(), BorrowError> {
        let atomic = unsafe { self.atomic_cnt.as_ref() };
        atomic
            .compare_exchange(Self::CNT_INIT, Self::CNT_EXC, Relaxed, Relaxed)
            .map_err(|_| BorrowError::ExclusiveFailed)?;
        Ok(())
    }
}

impl<V, BI, BM, A: Atomic<i32>> Drop for Holder<V, BI, BM, A> {
    fn drop(&mut self) {
        // Borrowed is dropped with Release ordering and it's synchronized with this Acquire.
        // Therefore, this thread can observe modification happend before when Borrowed is dropped.
        //
        // Dev note. Can we test whether it fails or not if we used Relaxed here?
        let atomic = unsafe { self.atomic_cnt.as_ref() };
        let cnt = atomic.load(Acquire);
        if cnt == 0 {
            // Deallocate atomic counter.
            let ptr = self.atomic_cnt.as_ptr();
            unsafe {
                std::alloc::dealloc(ptr.cast(), std::alloc::Layout::for_value(&*ptr));
            }
        } else {
            // Holder may be dropped while some threads are using Borrowed.
            // It's definitely unintended behavior.
            if thread::panicking() {
                process::abort();
            } else {
                panic!("Holder was dropped while someone is still using Borrowed");
            }
        }
    }
}

#[derive(Debug)]
pub struct Borrowed<B, A: Atomic<i32>> {
    borrow_value: B,
    exclusive: bool,
    atomic_cnt: NonNull<A>,
}

unsafe impl<B, A: Atomic<i32>> Send for Borrowed<B, A> {}

impl<B, A: Atomic<i32>> Deref for Borrowed<B, A> {
    type Target = B;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.borrow_value
    }
}

impl<B, A: Atomic<i32>> DerefMut for Borrowed<B, A> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.borrow_value
    }
}

impl<B, A: Atomic<i32>> Drop for Borrowed<B, A> {
    fn drop(&mut self) {
        let atomic = unsafe { self.atomic_cnt.as_ref() };
        if self.exclusive {
            atomic.add(1, Release);
        } else {
            atomic.sub(1, Release);
        }
    }
}
