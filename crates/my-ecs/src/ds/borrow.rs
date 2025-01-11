use std::{
    marker::PhantomData,
    mem,
    ops::{Deref, DerefMut},
    ptr,
};

pub type BorrowResult<B> = Result<Borrowed<B>, BorrowError>;

#[derive(Debug)]
pub enum BorrowError {
    /// Exclusive borrowing is only allowed when no one has borrowed it.
    ExclusiveFailed,

    /// For detecting anomaly, there exist limit you can immutably borrow.
    TooManyBorrow,

    /// If someone tried to borrow with out of bound index.
    OutOfBound,

    /// Failed to find the target.
    NotFound,
}

/// A shallow wrapper of [`Holder`].
#[derive(Debug)]
pub struct SimpleHolder<V>(Holder<V, V, V>);

impl<V: Clone> SimpleHolder<V> {
    pub fn new(value: V) -> Self {
        let fn_imm = |value: &V| -> V { value.clone() };
        let fn_mut = |value: &mut V| -> V { value.clone() };
        let inner = Holder::new(value, fn_imm, fn_mut);
        Self(inner)
    }

    pub fn into_value(self) -> V {
        self.0.into_value()
    }
}

impl<V> Deref for SimpleHolder<V> {
    type Target = Holder<V, V, V>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<V> DerefMut for SimpleHolder<V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Holding a thing within this struct and allows to borrow inner data as a
/// [`Borrowed`]. Multiple immutable borrowing is allowed, but mutable borrowing
/// is exclusive. If this struct is dropped while any `Borrowed` is alive, it
/// causes panic. You can check it out using [`Holder::borrow_count`] to see
/// there's any `Borrowed`.
///
/// Note, however, that this struct doesn't provide memory synchronization
/// over threads. If you want it, use [`Arc`] or something like that.
///
/// [`Arc`]: std::sync::Arc
#[derive(Debug)]
pub struct Holder<V, BI, BM> {
    value: V,
    #[cfg(feature = "check")]
    cnt: std::sync::Arc<std::sync::atomic::AtomicI32>,
    fn_imm: fn(&V) -> BI,
    fn_mut: fn(&mut V) -> BM,
    _marker: PhantomData<(BI, BM)>,
}

impl<V, BI, BM> Holder<V, BI, BM> {
    #[cfg(feature = "check")]
    const CNT_INIT: i32 = 0;
    #[cfg(feature = "check")]
    const CNT_EXC: i32 = -1;
    #[cfg(feature = "check")]
    const CNT_MAX: i32 = 1024;

    pub fn new(value: V, fn_imm: fn(&V) -> BI, fn_mut: fn(&mut V) -> BM) -> Self {
        Self {
            value,
            #[cfg(feature = "check")]
            cnt: std::sync::Arc::new(std::sync::atomic::AtomicI32::new(Self::CNT_INIT)),
            fn_imm,
            fn_mut,
            _marker: PhantomData,
        }
    }

    pub fn into_value(self) -> V {
        // Due to the drop impl, we cannot take `value` out. So checking on drop
        // occurs here instead.

        #[cfg(feature = "check")]
        assert!(self.cnt.load(std::sync::atomic::Ordering::Relaxed) == 0);

        // Safety: Pointer to self.value is valid.
        let value = unsafe { ptr::read(&self.value as *const _) };
        mem::forget(self);
        value
    }

    pub fn get_fn_imm(&self) -> fn(&V) -> BI {
        self.fn_imm
    }

    pub fn get_fn_mut(&self) -> fn(&mut V) -> BM {
        self.fn_mut
    }

    pub fn borrow(&self) -> BorrowResult<BI> {
        self.count_ref()?;
        let value = (self.fn_imm)(&self.value);

        #[cfg(feature = "check")]
        {
            let exclusive = false;
            let cnt = std::sync::Arc::clone(&self.cnt);
            Ok(Borrowed::new(value, exclusive, cnt))
        }

        #[cfg(not(feature = "check"))]
        Ok(Borrowed::new(value))
    }

    pub fn borrow_mut(&mut self) -> BorrowResult<BM> {
        self.count_mut()?;
        let value = (self.fn_mut)(&mut self.value);

        #[cfg(feature = "check")]
        {
            let exclusive = true;
            let cnt = std::sync::Arc::clone(&self.cnt);
            Ok(Borrowed::new(value, exclusive, cnt))
        }

        #[cfg(not(feature = "check"))]
        Ok(Borrowed::new(value))
    }

    pub fn get(&self) -> BorrowResult<&V> {
        self.count_ref()?;
        let value = &self.value;

        #[cfg(feature = "check")]
        {
            let exclusive = false;
            let cnt = std::sync::Arc::clone(&self.cnt);
            Ok(Borrowed::new(value, exclusive, cnt))
        }

        #[cfg(not(feature = "check"))]
        Ok(Borrowed::new(value))
    }

    pub fn get_mut(&mut self) -> BorrowResult<&mut V> {
        self.count_mut()?;
        let value = &mut self.value;

        #[cfg(feature = "check")]
        {
            let exclusive = true;
            let cnt = std::sync::Arc::clone(&self.cnt);
            Ok(Borrowed::new(value, exclusive, cnt))
        }

        #[cfg(not(feature = "check"))]
        Ok(Borrowed::new(value))
    }

    /// Returns shared reference to inner value without borrow check.
    ///
    /// # Safety
    ///
    /// Undefine behavior if exclusive borrow happend before.
    pub unsafe fn get_unchecked(&self) -> &V {
        &self.value
    }

    /// Returns mutable reference to inner value without borrow check.
    ///
    /// # Safety
    ///
    /// Undefine behavior if exclusive borrow happend before.
    pub unsafe fn get_unchecked_mut(&mut self) -> &mut V {
        &mut self.value
    }

    /// Retrieves current reference count. If it is greater than zero, it means
    /// there exist immutable [`Borrowed`]. If it is [`Self::CNT_EXC`], it means
    /// there exist mutable `Borrowed`. Otherwise, in other words, it's zero,
    /// there's no `Borrowed`.
    pub fn borrow_count(&self) -> i32 {
        #[cfg(feature = "check")]
        {
            self.cnt.load(std::sync::atomic::Ordering::Relaxed)
        }

        #[cfg(not(feature = "check"))]
        {
            0
        }
    }

    fn count_ref(&self) -> Result<(), BorrowError> {
        #[cfg(feature = "check")]
        {
            use std::sync::atomic::Ordering;

            let old = self.cnt.fetch_add(1, Ordering::Relaxed);
            if old > Self::CNT_MAX {
                self.cnt.fetch_sub(1, Ordering::Relaxed);
                Err(BorrowError::TooManyBorrow)
            } else if old == Self::CNT_EXC {
                self.cnt.fetch_sub(1, Ordering::Relaxed);
                Err(BorrowError::ExclusiveFailed)
            } else {
                Ok(())
            }
        }

        #[cfg(not(feature = "check"))]
        Ok(())
    }

    fn count_mut(&mut self) -> Result<(), BorrowError> {
        #[cfg(feature = "check")]
        {
            use std::sync::atomic::Ordering;

            self.cnt
                .compare_exchange(
                    Self::CNT_INIT,
                    Self::CNT_EXC,
                    Ordering::Relaxed,
                    Ordering::Relaxed,
                )
                .map_err(|_| BorrowError::ExclusiveFailed)?;
            Ok(())
        }

        #[cfg(not(feature = "check"))]
        Ok(())
    }
}

impl<V, BI, BM> Drop for Holder<V, BI, BM> {
    fn drop(&mut self) {
        #[cfg(feature = "check")]
        {
            let cnt = self.cnt.load(std::sync::atomic::Ordering::Relaxed);
            if cnt != 0 {
                // Holder may be dropped while some threads are using Borrowed.
                // It's definitely unintended behavior.
                if std::thread::panicking() {
                    std::process::abort();
                } else {
                    panic!("Holder was dropped while someone is still using Borrowed");
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct Borrowed<B> {
    value: B,
    #[cfg(feature = "check")]
    exclusive: bool,
    #[cfg(feature = "check")]
    cnt: std::sync::Arc<std::sync::atomic::AtomicI32>,
}

impl<B> Borrowed<B> {
    #[cfg(feature = "check")]
    pub const fn new(
        value: B,
        exclusive: bool,
        cnt: std::sync::Arc<std::sync::atomic::AtomicI32>,
    ) -> Self {
        Self {
            value,
            exclusive,
            cnt,
        }
    }

    #[cfg(not(feature = "check"))]
    pub const fn new(value: B) -> Self {
        Self { value }
    }

    pub fn map<T>(mut self, f: impl FnOnce(B) -> T) -> Borrowed<T> {
        // Safety: We're going to call forget() on `self`.
        let res = unsafe { self.map_copy(f) };
        mem::forget(self);
        res
    }

    /// Turns inner type into another type, then returns instance.  But `self`
    /// will leave as it was, so do not use it any longer.  That means you must
    /// not call drop on it as well. Consider using mem::forget() or something
    /// like that on `self` after calling this method.
    ///
    /// # Safety
    ///
    /// Undefined behavior if `self` is used again.
    pub unsafe fn map_copy<T>(&mut self, f: impl FnOnce(B) -> T) -> Borrowed<T> {
        let value: B = ptr::read(&self.value as *const _);

        #[cfg(feature = "check")]
        {
            let cnt = ptr::read(&self.cnt as *const _);
            Borrowed::new(f(value), self.exclusive, cnt)
        }

        #[cfg(not(feature = "check"))]
        Borrowed::new(f(value))
    }

    pub fn map_ref<T>(&self, f: impl FnOnce(&B) -> &T) -> &T {
        f(&self.value)
    }

    pub fn map_mut<T>(&mut self, f: impl FnOnce(&mut B) -> &mut T) -> &mut T {
        f(&mut self.value)
    }
}

impl<B> Deref for Borrowed<B> {
    type Target = B;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<B> DerefMut for Borrowed<B> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<B> Drop for Borrowed<B> {
    fn drop(&mut self) {
        #[cfg(feature = "check")]
        {
            use std::sync::atomic::Ordering;

            if self.exclusive {
                self.cnt.fetch_add(1, Ordering::Relaxed);
            } else {
                self.cnt.fetch_sub(1, Ordering::Relaxed);
            }
        }
    }
}
