use super::types::TypeIdExt;
use std::{
    cmp, fmt, hash,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

#[derive(Debug)]
#[repr(transparent)]
pub struct SendSyncPtr<T: ?Sized>(NonNull<T>);

unsafe impl<T: ?Sized> Send for SendSyncPtr<T> {}
unsafe impl<T: ?Sized> Sync for SendSyncPtr<T> {}

impl<T: ?Sized> SendSyncPtr<T> {
    // TODO: Can we detect Send or Sync requirements violation?
    #[inline]
    pub const fn new(ptr: NonNull<T>) -> Self {
        Self(ptr)
    }

    #[inline]
    pub const fn dangling() -> Self
    where
        T: Sized,
    {
        Self::new(NonNull::dangling())
    }

    #[inline]
    pub const fn as_nonnull(self) -> NonNull<T> {
        self.0
    }

    #[inline]
    pub const fn as_ptr(self) -> *mut T {
        self.0.as_ptr()
    }

    /// # Safety
    ///
    /// See [`NonNull::as_ref`].
    #[inline]
    pub const unsafe fn as_ref<'a>(&self) -> &'a T {
        self.0.as_ref()
    }

    /// # Safety
    ///
    /// See [`NonNull::as_mut`].
    #[inline]
    pub unsafe fn as_mut<'a>(&mut self) -> &'a mut T {
        self.0.as_mut()
    }

    /// # Safety
    ///
    /// See [`NonNull::add`].
    #[inline]
    pub const unsafe fn add(self, count: usize) -> Self
    where
        T: Sized,
    {
        Self::new(self.0.add(count))
    }

    /// # Safety
    ///
    /// See [`NonNull::sub`].
    #[inline]
    pub const unsafe fn sub(self, count: usize) -> Self
    where
        T: Sized,
    {
        Self::new(self.0.sub(count))
    }

    #[inline]
    pub const fn cast<U>(self) -> SendSyncPtr<U> {
        // Safety: Nothing has changed except `T` -> `U`.
        SendSyncPtr::new(self.0.cast())
    }
}

impl<T: ?Sized> PartialEq for SendSyncPtr<T> {
    #[allow(ambiguous_wide_pointer_comparisons)]
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl<T: ?Sized> Eq for SendSyncPtr<T> {}

impl<T: ?Sized> PartialOrd for SendSyncPtr<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: ?Sized> Ord for SendSyncPtr<T> {
    #[allow(ambiguous_wide_pointer_comparisons)]
    #[inline]
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.as_ptr().cmp(&other.as_ptr())
    }
}

impl<T: ?Sized> hash::Hash for SendSyncPtr<T> {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<T: ?Sized> Clone for SendSyncPtr<T> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> Copy for SendSyncPtr<T> {}

pub struct NonNullExt<T: ?Sized> {
    inner: NonNull<T>,
    #[cfg(feature = "check")]
    ty_or_name: crate::util::Or<TypeIdExt, &'static str>,
}

impl<T: ?Sized> fmt::Debug for NonNullExt<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[cfg(not(feature = "check"))]
        {
            self.inner.fmt(f)
        }

        #[cfg(feature = "check")]
        {
            write!(f, "NonNullExt({:?})", self.ty_or_name)
        }
    }
}

impl<T: ?Sized> NonNullExt<T> {
    #[inline]
    pub fn new(ptr: *mut T) -> Option<Self> {
        // clippy yells for adding `unsafe` to function,
        // because input argument `ptr` seems to be dereferenced in unsafe block below.
        // But we're not dereferencing it. I have no idea why it's clippy error.
        // But if we assign `ptr` to local variable instead of using `ptr`,
        // the clippy error disappears somehow.
        let x = ptr;
        if !x.is_null() {
            // Safety: It's not null.
            Some(unsafe { Self::new_unchecked(x) })
        } else {
            None
        }
    }

    #[inline]
    pub const fn from_nonnull(ptr: NonNull<T>) -> Self {
        Self {
            inner: ptr,
            #[cfg(feature = "check")]
            ty_or_name: crate::util::Or::B(""),
        }
    }

    /// # Safety
    ///
    /// Undefined behavior if the pointer is null.
    #[inline]
    pub const unsafe fn new_unchecked(ptr: *mut T) -> Self {
        Self {
            inner: NonNull::new_unchecked(ptr),
            #[cfg(feature = "check")]
            ty_or_name: crate::util::Or::B(""),
        }
    }

    #[inline]
    pub const fn dangling() -> Self
    where
        T: Sized,
    {
        Self {
            inner: NonNull::dangling(),
            #[cfg(feature = "check")]
            ty_or_name: crate::util::Or::B(""),
        }
    }

    #[inline]
    pub fn is_dangling(&self) -> bool
    where
        T: Sized,
    {
        self == &Self::dangling()
    }

    /// It's noop in release mode.
    #[inline]
    pub fn with_type(self, _ty: TypeIdExt) -> Self {
        #[cfg(not(feature = "check"))]
        {
            self
        }

        #[cfg(feature = "check")]
        {
            let mut this = self;
            this.ty_or_name = crate::util::Or::A(_ty);
            this
        }
    }

    /// It's noop in release mode.
    #[inline]
    pub fn with_name(self, _name: &'static str) -> Self {
        #[cfg(not(feature = "check"))]
        {
            self
        }

        #[cfg(feature = "check")]
        {
            let mut this = self;
            this.ty_or_name = crate::util::Or::B(_name);
            this
        }
    }

    #[inline]
    pub fn get_type(&self) -> Option<&TypeIdExt> {
        #[cfg(not(feature = "check"))]
        {
            None
        }

        #[cfg(feature = "check")]
        {
            match &self.ty_or_name {
                crate::util::Or::A(ty) => Some(ty),
                crate::util::Or::B(_name) => None,
            }
        }
    }

    #[inline]
    pub fn get_name(&self) -> Option<&str> {
        #[cfg(not(feature = "check"))]
        {
            None
        }

        #[cfg(feature = "check")]
        {
            match &self.ty_or_name {
                crate::util::Or::A(_ty) => None,
                crate::util::Or::B(name) => Some(name),
            }
        }
    }

    #[inline]
    pub fn cast<U>(self) -> NonNullExt<U> {
        let Self {
            inner,
            #[cfg(feature = "check")]
            ty_or_name,
        } = self;
        NonNullExt {
            inner: inner.cast(),
            #[cfg(feature = "check")]
            ty_or_name,
        }
    }

    /// # Safety
    ///
    /// See [`NonNull::add`].
    #[inline]
    pub unsafe fn add(self, count: usize) -> Self
    where
        T: Sized,
    {
        Self {
            inner: self.inner.add(count),
            #[cfg(feature = "check")]
            ty_or_name: self.ty_or_name,
        }
    }

    /// # Safety
    ///
    /// See [`NonNull::sub`].
    #[inline]
    pub unsafe fn sub(self, count: usize) -> Self
    where
        T: Sized,
    {
        Self {
            inner: self.inner.sub(count),
            #[cfg(feature = "check")]
            ty_or_name: self.ty_or_name,
        }
    }
}

impl<T: ?Sized> Deref for NonNullExt<T> {
    type Target = NonNull<T>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T: ?Sized> DerefMut for NonNullExt<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T: ?Sized> PartialEq for NonNullExt<T> {
    #[allow(ambiguous_wide_pointer_comparisons)]
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl<T: ?Sized> Eq for NonNullExt<T> {}

impl<T: ?Sized> PartialOrd for NonNullExt<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: ?Sized> Ord for NonNullExt<T> {
    #[allow(ambiguous_wide_pointer_comparisons)]
    #[inline]
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.as_ptr().cmp(&other.as_ptr())
    }
}

impl<T: ?Sized> hash::Hash for NonNullExt<T> {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state)
    }
}

impl<T: ?Sized> Clone for NonNullExt<T> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> Copy for NonNullExt<T> {}

pub struct ManagedConstPtr<T: ?Sized> {
    inner: NonNullExt<T>,
    #[cfg(feature = "check")]
    debug: bool,
}

impl<T: ?Sized> fmt::Debug for ManagedConstPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

unsafe impl<T: ?Sized + Send> Send for ManagedConstPtr<T> {}

impl<T: ?Sized> ManagedConstPtr<T> {
    /// # Safety
    ///
    /// The pointer must be valid and not aliased mutably while the instance is in use.
    #[inline]
    pub unsafe fn new(ptr: NonNullExt<T>) -> Self {
        #[cfg(feature = "check")]
        {
            debug::insert_const_ptr(ptr); // non-const function.
        }

        Self {
            inner: ptr,
            #[cfg(feature = "check")]
            debug: true,
        }
    }

    /// # Safety
    ///
    /// The pointer must be valid and not aliased mutably while the instance is in use.
    #[inline]
    pub const unsafe fn new_nocheck(ptr: NonNullExt<T>) -> Self {
        Self {
            inner: ptr,
            #[cfg(feature = "check")]
            debug: false,
        }
    }

    #[inline]
    pub const fn dangling() -> Self
    where
        T: Sized,
    {
        // Safety: Intentional dangling pointer.
        unsafe { Self::new_nocheck(NonNullExt::dangling()) }
    }

    #[inline]
    pub fn inner(&self) -> NonNullExt<T> {
        self.inner
    }

    #[inline]
    pub fn as_nonnull(&self) -> NonNull<T> {
        self.inner.inner
    }

    #[inline]
    pub fn as_ptr(&self) -> *const T {
        self.inner.as_ptr().cast_const()
    }

    #[inline]
    pub fn into_ref<'a>(self) -> &'a T {
        let inner = self.inner();

        #[cfg(feature = "check")]
        drop(self);

        // Safety: Managed pointer.
        unsafe { inner.as_ref() }
    }

    #[inline]
    pub fn cast<U>(self) -> ManagedConstPtr<U> {
        let inner = self.inner();

        #[cfg(feature = "check")]
        drop(self);

        // Safety: Nothing has changed except `T` -> `U`.
        unsafe { ManagedConstPtr::new(inner.cast()) }
    }

    /// # Safety
    ///
    /// See [`NonNull::add`].
    #[inline]
    pub unsafe fn add(self, count: usize) -> Self
    where
        T: Sized,
    {
        #[cfg(not(feature = "check"))]
        {
            Self {
                inner: self.inner.add(count),
            }
        }

        #[cfg(feature = "check")]
        {
            Self {
                inner: self.inner.add(count),
                debug: self.debug,
            }
        }
    }

    /// # Safety
    ///
    /// See [`NonNull::sub`].
    #[inline]
    pub unsafe fn sub(self, count: usize) -> Self
    where
        T: Sized,
    {
        #[cfg(not(feature = "check"))]
        {
            Self {
                inner: self.inner.sub(count),
            }
        }

        #[cfg(feature = "check")]
        {
            Self {
                inner: self.inner.sub(count),
                debug: self.debug,
            }
        }
    }
}

#[cfg(feature = "check")]
impl<T: ?Sized> Drop for ManagedConstPtr<T> {
    fn drop(&mut self) {
        if self.debug {
            debug::remove_ptr(*self.inner);
        }
    }
}

impl<T: ?Sized> Deref for ManagedConstPtr<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        // Safety: We assume that the pointer is valid by the constructor.
        unsafe { self.inner.as_ref() }
    }
}

impl<T: ?Sized> PartialEq for ManagedConstPtr<T> {
    #[allow(ambiguous_wide_pointer_comparisons)]
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl<T: ?Sized> Eq for ManagedConstPtr<T> {}

impl<T: ?Sized> PartialOrd for ManagedConstPtr<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: ?Sized> Ord for ManagedConstPtr<T> {
    #[allow(ambiguous_wide_pointer_comparisons)]
    #[inline]
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.as_ptr().cmp(&other.as_ptr())
    }
}

impl<T: ?Sized> hash::Hash for ManagedConstPtr<T> {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state)
    }
}

impl<T: ?Sized> Clone for ManagedConstPtr<T> {
    #[inline]
    fn clone(&self) -> Self {
        #[cfg(feature = "check")]
        {
            Self {
                inner: self.inner,
                debug: self.debug,
            }
        }

        #[cfg(not(feature = "check"))]
        *self
    }
}

// It's pointer. We can copy it regardless of T.
#[cfg(not(feature = "check"))]
impl<T: ?Sized> Copy for ManagedConstPtr<T> {}

pub struct ManagedMutPtr<T: ?Sized> {
    inner: NonNullExt<T>,
    #[cfg(feature = "check")]
    debug: bool,
}

impl<T: ?Sized> fmt::Debug for ManagedMutPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

unsafe impl<T: ?Sized + Send> Send for ManagedMutPtr<T> {}

impl<T: ?Sized> ManagedMutPtr<T> {
    /// # Safety
    ///
    /// The pointer must be valid and not aliased while the instance is in use.
    #[inline]
    pub unsafe fn new(ptr: NonNullExt<T>) -> Self {
        #[cfg(feature = "check")]
        {
            debug::insert_mut_ptr(ptr); // non-const function.
        }

        Self {
            inner: ptr,
            #[cfg(feature = "check")]
            debug: true,
        }
    }

    /// # Safety
    ///
    /// The pointer must be valid and not aliased while the instance is in use.
    #[inline]
    pub const unsafe fn new_nocheck(ptr: NonNullExt<T>) -> Self {
        Self {
            inner: ptr,
            #[cfg(feature = "check")]
            debug: false,
        }
    }

    #[inline]
    pub const fn dangling() -> Self
    where
        T: Sized,
    {
        // Safety: Intentional dangling pointer.
        unsafe { Self::new_nocheck(NonNullExt::dangling()) }
    }

    #[inline]
    pub fn is_dangling(&self) -> bool
    where
        T: Sized,
    {
        self == &Self::dangling()
    }

    #[inline]
    pub fn inner(&self) -> NonNullExt<T> {
        self.inner
    }

    #[inline]
    pub fn as_nonnull(&self) -> NonNull<T> {
        self.inner.inner
    }

    #[inline]
    pub fn as_ptr(&self) -> *mut T {
        self.inner.as_ptr()
    }

    #[inline]
    pub fn into_mut<'a>(self) -> &'a mut T {
        let mut inner = self.inner();

        #[cfg(feature = "check")]
        drop(self);

        // Safety: Managed pointer.
        unsafe { inner.as_mut() }
    }

    #[inline]
    pub fn cast<U>(self) -> ManagedMutPtr<U> {
        let inner = self.inner();

        #[cfg(feature = "check")]
        drop(self);

        // Safety: Nothing has changed except `T` -> `U`.
        unsafe { ManagedMutPtr::new(inner.cast()) }
    }

    #[inline]
    pub fn cast_const(self) -> ManagedConstPtr<T> {
        let inner = self.inner();

        #[cfg(feature = "check")]
        drop(self);

        // Safety: Nothing has changed except `Mut` -> `Const`.
        unsafe { ManagedConstPtr::new(inner) }
    }

    /// # Safety
    ///
    /// See [`NonNull::add`].
    #[inline]
    pub unsafe fn add(self, count: usize) -> Self
    where
        T: Sized,
    {
        #[cfg(not(feature = "check"))]
        {
            Self {
                inner: self.inner.add(count),
            }
        }

        #[cfg(feature = "check")]
        {
            Self {
                inner: self.inner.add(count),
                debug: self.debug,
            }
        }
    }

    /// # Safety
    ///
    /// See [`NonNull::sub`].
    #[inline]
    pub unsafe fn sub(self, count: usize) -> Self
    where
        T: Sized,
    {
        #[cfg(not(feature = "check"))]
        {
            Self {
                inner: self.inner.sub(count),
            }
        }

        #[cfg(feature = "check")]
        {
            Self {
                inner: self.inner.sub(count),
                debug: self.debug,
            }
        }
    }
}

#[cfg(feature = "check")]
impl<T: ?Sized> Drop for ManagedMutPtr<T> {
    fn drop(&mut self) {
        if self.debug {
            debug::remove_ptr(*self.inner);
        }
    }
}

impl<T: ?Sized> Deref for ManagedMutPtr<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        // Safety: We assume that the pointer is valid by the constructor.
        unsafe { self.inner.as_ref() }
    }
}

impl<T: ?Sized> DerefMut for ManagedMutPtr<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        // Safety: We assume that the pointer is valid by the constructor.
        unsafe { self.inner.as_mut() }
    }
}

impl<T: ?Sized> PartialEq for ManagedMutPtr<T> {
    #[allow(ambiguous_wide_pointer_comparisons)]
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl<T: ?Sized> Eq for ManagedMutPtr<T> {}

impl<T: ?Sized> PartialOrd for ManagedMutPtr<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: ?Sized> Ord for ManagedMutPtr<T> {
    #[allow(ambiguous_wide_pointer_comparisons)]
    #[inline]
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.as_ptr().cmp(&other.as_ptr())
    }
}

impl<T: ?Sized> hash::Hash for ManagedMutPtr<T> {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state)
    }
}

// ManagedMutPtr is not a pure pointer unlike NonNull.
// It can be dereferenced as shared or mutable reference without need for unsafe block.
// That means it's more like a mutable reference.
// So, we don't implement Clone and Copy for it.
// impl<T: ?Sized> Clone for ManagedMutPtr<T> {..}
// impl<T: ?Sized> Copy for ManagedMutPtr<T> {..}

#[cfg(feature = "check")]
mod debug {
    use super::*;
    use dashmap::DashMap;
    use std::sync::LazyLock;

    enum RefCount {
        Shared(u16),
        Exclusive,
    }

    static RC_MAP: LazyLock<DashMap<[usize; 2], RefCount>> = LazyLock::new(DashMap::new);

    const MAX_RC: u16 = 256;

    fn create_key<T: ?Sized>(ptr: NonNull<T>) -> [usize; 2] {
        const PTR_SIZE: usize = std::mem::size_of::<*const ()>();
        // TODO: Wide pointer size may change in the future.
        // See https://doc.rust-lang.org/reference/type-layout.html#pointers-and-references-layout
        const WIDE_PTR_SIZE: usize = PTR_SIZE * 2;

        const _: () = {
            assert!(PTR_SIZE == std::mem::size_of::<usize>());
            assert!(WIDE_PTR_SIZE == std::mem::size_of::<[usize; 2]>());
        };

        match size_of_val(&ptr) {
            PTR_SIZE => [0, ptr.as_ptr() as *mut () as usize],
            WIDE_PTR_SIZE => {
                // Safety: We checked the size.
                unsafe { std::mem::transmute_copy(&ptr) }
            }
            _ => unimplemented!(),
        }
    }

    /// Inserts the pointer to the global map.
    /// In the map, shared pointer is allowed to be inserted multiple times.
    ///
    /// # Panics
    ///
    /// - Panics if insertion count is greater than the limit (256).
    /// - Panics if the map contained the pointer as exclusive pointer.
    pub(super) fn insert_const_ptr<T: ?Sized>(ptr: NonNullExt<T>) {
        let key = create_key(*ptr);
        RC_MAP
            .entry(key)
            .and_modify(|rc| match rc {
                RefCount::Shared(cnt) => {
                    *cnt += 1;
                    assert!(*cnt <= MAX_RC, "too many ManagedConstPtr");
                }
                RefCount::Exclusive => {
                    panic!("`{ptr:?}` cannot become managed shared pointer because it's already managed as a mutable pointer");
                }
            })
            .or_insert(RefCount::Shared(1));
    }

    /// Inserts the pointer to the global map.
    /// In the map, exclusive pointer is not allowed to be inserted multiple times.
    /// To insert the same pointer, you must remove the pointer first.
    ///
    /// Note, however, that zero-sized `T` is considered shared pointer.
    /// It means the pointer is allowed to be inserted many times.
    ///
    /// # Panics
    ///
    /// Panics if the map contained the pointer.
    pub(super) fn insert_mut_ptr<T: ?Sized>(ptr: NonNullExt<T>) {
        // If `T` is a zero-sized type, then it's good to be allowed to have
        // the same pointers because they do not mutate the same data.
        // It can be considered as a const pointer.
        //
        // Safety: Even if it's aliased, we do not read the data here.
        if unsafe { size_of_val(ptr.as_ref()) } == 0 {
            insert_const_ptr(ptr);
            return;
        }

        let key = create_key(*ptr);
        RC_MAP
            .entry(key)
            .and_modify(|rc| match rc {
                RefCount::Shared(_) => {
                    panic!("`{ptr:?}` cannot become managed mutable pointer because it's already managed as a shared pointer: {:#0x}", ptr.as_ptr() as *mut u8 as usize);
                }
                RefCount::Exclusive => {
                    panic!("`{ptr:?}` cannot become managed mutable pointer because it's already managed as a mutable pointer: {:#0x}", ptr.as_ptr() as *mut u8 as usize);
                }
            })
            .or_insert(RefCount::Exclusive);
    }

    pub(super) fn remove_ptr<T: ?Sized>(ptr: NonNull<T>) {
        let key = create_key(ptr);
        assert!(
            RC_MAP.contains_key(&key),
            "cannot find pointer in the RC_MAP"
        );
        RC_MAP.remove_if_mut(&key, |_, rc| match rc {
            RefCount::Shared(cnt) => {
                *cnt -= 1;
                *cnt == 0
            }
            RefCount::Exclusive => true,
        });
    }
}
