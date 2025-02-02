use std::{
    marker::PhantomData,
    mem,
    ops::{Deref, DerefMut},
    ptr,
};

/// A [`Holder`] that lends its inner value itself by cloning.
#[derive(Debug)]
pub struct SimpleHolder<V>(Holder<V, V, V>);

impl<V: Clone> SimpleHolder<V> {
    pub fn new(value: V) -> Self {
        let fn_imm = |value: &V| -> V { value.clone() };
        let fn_mut = |value: &mut V| -> V { value.clone() };
        // Safety: `fn_imm` and `fn_mut` clones the value so that they don't
        // require borrow check.
        let inner = unsafe { Holder::new(value, fn_imm, fn_mut) };
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

/// A type that holds a value and allows us borrow the value.
///
/// When you want to borrow inner value you can call [`Holder::borrow`] or
/// [`Holder::borrow_mut`]. They return the inner value wrapped in [`Borrowed`]
/// which tracks borrow status.
///
/// Multiple immutable borrowing is allowed, but mutable borrowing is exclusive.
/// If this struct is dropped while any borrowed value is alive, it causes
/// panic. You can check it out using [`Holder::borrow_count`] to see there's
/// any borrowed value.
///
/// # Features
///
/// Borrow check is enabled if and only if `check` feature is enabled.
/// Otherwise, this struct does nothing special.
#[derive(Debug)]
pub struct Holder<V, BI, BM> {
    /// The inner value holder holds.
    value: V,

    /// Reference count to check borrow status at runtime.
    #[cfg(feature = "check")]
    cnt: std::sync::Arc<std::sync::atomic::AtomicI32>,

    /// Function that borrows the inner value immutably.
    fn_imm: fn(&V) -> BI,

    /// Function that borrows the inner value mutably.
    fn_mut: fn(&mut V) -> BM,

    _marker: PhantomData<(BI, BM)>,
}

impl<V, BI, BM> Holder<V, BI, BM> {
    /// Initial reference count.
    #[cfg(feature = "check")]
    const CNT_INIT: i32 = 0;

    /// Reference count for mutable borrow.
    #[cfg(feature = "check")]
    const CNT_EXC: i32 = -1;

    /// Maximum number of immutable borrow.
    #[cfg(feature = "check")]
    const CNT_MAX: i32 = 1024;

    /// Creates a [`Holder`] with the given value.
    ///
    /// `Holder` checks borrow rule at runtime by enabled feature `check`. But
    /// it requires additional operations, so that if you are confident, you can
    /// use the `Holder` without the borrow check.
    ///
    /// * `fn_imm` - Function that borrows the value and returns borrowed
    ///   immutable value.
    /// * `fn_mut` - Function that borrows the value and returns borrowed
    ///   mutable value.
    ///
    /// # Safety
    ///
    /// `Holder` disables Rust default borrow checker. Borrow rule is not
    /// checked even if the `fn_imm` and `fn_mut` return references to the inner
    /// value. However, `Holder` checks the borrow rule at runtime by enabled
    /// feature `check`. If the feature is not enabled, this function is unsafe
    /// due to the disabled runtime borrow check.
    ///
    /// If the `fn_imm` and `fn_mut` returns static type instead of reference,
    /// it's safe regardless of enabling the feature because they don't need
    /// borrow check.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::Holder;
    ///
    /// let mut holder: Holder<i32, &i32, &mut i32> = unsafe {
    ///     Holder::new(0, |v| v, |v| v)
    /// };
    /// let a = holder.borrow_mut().unwrap();
    /// holder.borrow_mut().unwrap(); // No panic without `check` feature
    /// println!("{a:?}");
    /// ```
    //
    // Lifetime `v` allows us to put something in like example above.
    pub unsafe fn new<'v>(value: V, fn_imm: fn(&'v V) -> BI, fn_mut: fn(&'v mut V) -> BM) -> Self {
        let (fn_imm, fn_mut) = unsafe {
            (
                mem::transmute::<fn(&'v V) -> BI, fn(&V) -> BI>(fn_imm),
                mem::transmute::<fn(&'v mut V) -> BM, fn(&mut V) -> BM>(fn_mut),
            )
        };

        Self {
            value,
            #[cfg(feature = "check")]
            cnt: std::sync::Arc::new(std::sync::atomic::AtomicI32::new(Self::CNT_INIT)),
            fn_imm,
            fn_mut,
            _marker: PhantomData,
        }
    }

    /// Unwraps the holder and returns inner value.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::Holder;
    ///
    /// let holder = unsafe { Holder::new(0, |v| v, |v| v) };
    /// assert_eq!(holder.into_value(), 0);
    /// ```
    pub fn into_value(self) -> V {
        // Due to the drop impl, we cannot take `value` out. So checking on drop
        // occurs here instead.

        #[cfg(feature = "check")]
        assert_eq!(self.cnt.load(std::sync::atomic::Ordering::Relaxed), 0);

        // Safety: Pointer to self.value is valid.
        let value = unsafe { ptr::read(&self.value as *const _) };
        mem::forget(self);
        value
    }

    /// Returns immutable borrow function that you inserted at [`Holder::new`].
    pub fn get_fn_imm(&self) -> fn(&V) -> BI {
        self.fn_imm
    }

    /// Returns mutable borrow function that you inserted at [`Holder::new`].
    pub fn get_fn_mut(&self) -> fn(&mut V) -> BM {
        self.fn_mut
    }

    /// Borrows inner value.
    ///
    /// If `check` feature is enabled, runtime borrow checker works and blocks
    /// try borrowing value after mutable borrow.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::Holder;
    ///
    /// let holder = unsafe { Holder::new(0, |v| v, |v| v) };
    /// assert_eq!(*holder.borrow().unwrap(), &0);
    /// ```
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

    /// Borrows inner value mutably.
    ///
    /// If `check` feature is enabled, runtime borrow checker works and blocks
    /// try borrowing value after mutable or immutable borrow.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::Holder;
    ///
    /// let mut holder = unsafe { Holder::new(0, |v| v, |v| v) };
    /// **holder.borrow_mut().unwrap() = 1;
    /// assert_eq!(*holder.borrow().unwrap(), &1);
    /// ```
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

    /// Returns shared reference to the inner value without calling immutable
    /// borrow function that you inserted at [`Holder::new`].
    ///
    /// If `check` feature is enabled, runtime borrow checker works and blocks
    /// try borrowing value after mutable borrow.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::Holder;
    ///
    /// let holder = unsafe { Holder::new(0, |v| *v + 1, |v| *v + 1) };
    /// let value = holder.get().unwrap();
    /// assert_eq!(*value, &0); // Due to bypassed borrow function.
    /// ```
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

    /// Returns mutable reference to the inner value without calling mutable
    /// borrow function that you inserted at [`Holder::new`].
    ///
    /// If `check` feature is enabled, runtime borrow checker works and blocks
    /// try borrowing value after mutable or immutable borrow.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::Holder;
    ///
    /// let mut holder = unsafe { Holder::new(0, |v| *v + 1, |v| *v + 1) };
    /// let value = holder.get_mut().unwrap();
    /// assert_eq!(*value, &mut 0); // Due to bypassed borrow function.
    /// ```
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
    /// Undefined behavior if exclusive borrow happened before.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::Holder;
    ///
    /// let holder = unsafe { Holder::new(0, |v| v, |v| v) };
    /// let value = unsafe { holder.get_unchecked() };
    /// assert_eq!(value, &0);
    /// ```
    pub unsafe fn get_unchecked(&self) -> &V {
        &self.value
    }

    /// Returns mutable reference to inner value without borrow check.
    ///
    /// # Safety
    ///
    /// Undefined behavior if exclusive borrow happened before.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::Holder;
    ///
    /// let mut holder = unsafe { Holder::new(0, |v| v, |v| v) };
    /// let value = unsafe { holder.get_unchecked_mut() };
    /// assert_eq!(value, &0);
    /// ```
    pub unsafe fn get_unchecked_mut(&mut self) -> &mut V {
        &mut self.value
    }

    /// Returns current reference count.
    ///
    /// Returning reference count will be
    /// - Greater than zero meaning there exist immutable borrow at the time.
    /// - Lesser than zero meaning there exist mutable borrow at the time.
    /// - Zero meaning no borrow at the time or `check` feature is disabled.
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

/// A type generated by [`Holder`] that wraps a value in it shallowly.
///
/// This struct implements [`Deref`] and [`DerefMut`] for the inner value so
/// that clients can use this struct like the inner value.
///
/// If `check` feature is enabled, this struct tracks borrow status at runtime,
/// and will warn you if borrow rule was violated.
#[derive(Debug)]
pub struct Borrowed<B> {
    value: B,
    #[cfg(feature = "check")]
    exclusive: bool,
    #[cfg(feature = "check")]
    cnt: std::sync::Arc<std::sync::atomic::AtomicI32>,
}

impl<B> Borrowed<B> {
    const fn new(
        value: B,
        #[cfg(feature = "check")] exclusive: bool,
        #[cfg(feature = "check")] cnt: std::sync::Arc<std::sync::atomic::AtomicI32>,
    ) -> Self {
        Self {
            value,
            #[cfg(feature = "check")]
            exclusive,
            #[cfg(feature = "check")]
            cnt,
        }
    }

    /// Converts inner value through the given function while maintaining borrow
    /// state.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::Holder;
    ///
    /// let holder = unsafe { Holder::new(0, |v| v, |v| v) };
    /// let value = holder.borrow().unwrap();
    /// let value = value.map(|v| v.to_string());
    /// ```
    pub fn map<T>(mut self, f: impl FnOnce(B) -> T) -> Borrowed<T> {
        // Safety: self is not used again due to forget.
        let res = unsafe { self.map_copy(f) };
        mem::forget(self);
        res
    }

    /// Turns inner value into another type, then returns converted value
    /// wrapped in [`Borrowed`] while keeping borrow status.
    ///
    /// The operation looks like `move` with type-conversion. But `self` will
    /// leave as it was, so caller must not use it any longer. That means caller
    /// must not call even drop function on it. Consider using
    /// [`forget`](mem::forget) or something like that to `self` after calling
    /// this method.
    ///
    /// # Safety
    ///
    /// Undefined behavior if `self` is used again.
    unsafe fn map_copy<T>(&mut self, f: impl FnOnce(B) -> T) -> Borrowed<T> {
        let value: B = unsafe { ptr::read(&self.value as *const B) };

        #[cfg(feature = "check")]
        {
            let cnt = unsafe { ptr::read(&self.cnt as *const _) };
            Borrowed::new(f(value), self.exclusive, cnt)
        }

        #[cfg(not(feature = "check"))]
        Borrowed::new(f(value))
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

/// Result type for [`Borrowed`] with [`BorrowError`] for error propagation.
pub type BorrowResult<B> = Result<Borrowed<B>, BorrowError>;

/// An error type associated with borrow.
///
/// This error type will be commonly seen with [`Holder`], which can borrow
/// its inner value to you.
#[derive(Debug)]
#[non_exhaustive]
pub enum BorrowError {
    /// Exclusive borrowing is only allowed when no one has borrowed it.
    ExclusiveFailed,

    /// For detecting anomaly, there exist the limit how many you can immutably
    /// borrow.
    TooManyBorrow,

    /// Borrow failed because of index out of bounds.
    OutOfBound,

    /// Borrow failed because of not found key.
    NotFound,
}
