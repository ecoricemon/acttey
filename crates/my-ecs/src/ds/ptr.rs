use super::types::TypeIdExt;
use crate::util::Or;
use std::{
    borrow::Cow,
    fmt, hash, mem,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

#[cfg_attr(not(debug_assertions), repr(transparent))]
pub struct NonNullExt<T: ?Sized> {
    inner: NonNull<T>,
    #[cfg(debug_assertions)]
    ty_or_name: Or<TypeIdExt, Cow<'static, str>>,
}

impl<T: ?Sized> fmt::Debug for NonNullExt<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[cfg(not(debug_assertions))]
        {
            self.inner.fmt(f)
        }

        #[cfg(debug_assertions)]
        {
            write!(f, "NonNullExt({:?})", self.ty_or_name)
        }
    }
}

impl<T: ?Sized> NonNullExt<T> {
    pub fn new(ptr: *mut T) -> Option<Self> {
        // clippy yells for adding `unsafe` to function
        // due to unsafety of dereferencing `ptr` at Self::new_unchecked(ptr).
        // But it's not dereferencing. I have no idea why it's clippy error.
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

    /// # Safety
    ///
    /// Undefined behavior if the pointer is null.
    pub const unsafe fn new_unchecked(ptr: *mut T) -> Self {
        Self {
            inner: NonNull::new_unchecked(ptr),
            #[cfg(debug_assertions)]
            ty_or_name: Or::B(Cow::Borrowed("")),
        }
    }

    /// It's noop in release mode.
    pub fn with_type(mut self, ty: TypeIdExt) -> Self {
        #[cfg(debug_assertions)]
        {
            self.ty_or_name = Or::A(ty)
        }
        self
    }

    /// It's noop in release mode.
    pub fn with_name(mut self, name: impl Into<Cow<'static, str>>) -> Self {
        #[cfg(debug_assertions)]
        {
            self.ty_or_name = Or::B(name.into())
        }
        self
    }

    pub fn get_type(&self) -> Option<&TypeIdExt> {
        #[cfg(not(debug_assertions))]
        {
            None
        }

        #[cfg(debug_assertions)]
        {
            match &self.ty_or_name {
                Or::A(ty) => Some(ty),
                Or::B(_name) => None,
            }
        }
    }

    pub fn get_name(&self) -> Option<&str> {
        #[cfg(not(debug_assertions))]
        {
            None
        }

        #[cfg(debug_assertions)]
        {
            match &self.ty_or_name {
                Or::A(_ty) => None,
                Or::B(name) => Some(name),
            }
        }
    }

    pub fn cast<U>(self) -> NonNullExt<U> {
        let Self {
            inner,
            #[cfg(debug_assertions)]
            ty_or_name,
        } = self;
        NonNullExt {
            inner: inner.cast(),
            #[cfg(debug_assertions)]
            ty_or_name,
        }
    }
}

impl<T: ?Sized> Deref for NonNullExt<T> {
    type Target = NonNull<T>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T: ?Sized> DerefMut for NonNullExt<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T: ?Sized> PartialEq for NonNullExt<T> {
    #[allow(ambiguous_wide_pointer_comparisons)]
    fn eq(&self, other: &Self) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl<T: ?Sized> Eq for NonNullExt<T> {}

impl<T: ?Sized> hash::Hash for NonNullExt<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state)
    }
}

impl<T: ?Sized> Clone for NonNullExt<T> {
    fn clone(&self) -> Self {
        #[cfg(not(debug_assertions))]
        {
            *self
        }

        #[cfg(debug_assertions)]
        {
            Self {
                inner: self.inner,
                ty_or_name: self.ty_or_name.clone(),
            }
        }
    }
}

#[cfg(not(debug_assertions))]
impl<T: ?Sized> Copy for NonNullExt<T> {}

impl<T: ?Sized> From<NonNull<T>> for NonNullExt<T> {
    fn from(value: NonNull<T>) -> Self {
        // Safety: It's not null.
        unsafe { Self::new_unchecked(value.as_ptr()) }
    }
}

#[derive(PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ManagedConstPtr<T: ?Sized>(NonNullExt<T>);

impl<T: ?Sized> fmt::Debug for ManagedConstPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// The pointer is managed to be valid and not aliased mutably.
unsafe impl<T: ?Sized> Send for ManagedConstPtr<T> {}

impl<T: ?Sized> ManagedConstPtr<T> {
    /// # Safety
    ///
    /// The pointer must be valid and not aliased mutably while the instance is in use.
    pub unsafe fn new(ptr: NonNullExt<T>) -> Self {
        #[cfg(debug_assertions)]
        {
            debug::insert_const_ptr(*ptr); // non-const function.
        }

        Self(ptr)
    }

    pub fn into_inner(self) -> NonNullExt<T> {
        self.0.clone()
    }

    pub fn get_inner(&self) -> &NonNullExt<T> {
        &self.0
    }

    pub fn as_ptr(&self) -> *const T {
        self.0.as_ptr().cast_const()
    }

    pub fn cast<U>(self) -> ManagedConstPtr<U> {
        #[cfg(not(debug_assertions))]
        {
            unsafe { ManagedConstPtr::new(self.into_inner().cast()) }
        }

        #[cfg(debug_assertions)]
        {
            // We don't need to call drop() and new().
            let inner = self.0.clone().cast();
            std::mem::forget(self);
            ManagedConstPtr(inner)
        }
    }
}

#[cfg(debug_assertions)]
impl<T: ?Sized> Drop for ManagedConstPtr<T> {
    fn drop(&mut self) {
        debug::remove_ptr(*self.0);
    }
}

impl<T: ?Sized> Deref for ManagedConstPtr<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // Safety: We assume that the pointer is valid by the constructor.
        unsafe { self.0.as_ref() }
    }
}

impl<T: ?Sized> Clone for ManagedConstPtr<T> {
    fn clone(&self) -> Self {
        // Safety: Inner pointer would be safe to access. See constructor.
        unsafe { Self::new(self.0.clone()) }
    }
}

// It's pointer. We can copy it regardless of T.
#[cfg(not(debug_assertions))]
impl<T: ?Sized> Copy for ManagedConstPtr<T> {}

#[derive(PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ManagedMutPtr<T: ?Sized>(NonNullExt<T>);

impl<T: ?Sized> fmt::Debug for ManagedMutPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// The pointer is managed to be valid and not aliased.
unsafe impl<T: ?Sized> Send for ManagedMutPtr<T> {}

impl<T: ?Sized> ManagedMutPtr<T> {
    /// # Safety
    ///
    /// The pointer must be valid and not aliased while the instance is in use.
    pub unsafe fn new(ptr: NonNullExt<T>) -> Self {
        #[cfg(debug_assertions)]
        {
            debug::insert_mut_ptr(*ptr); // non-const function.
        }

        Self(ptr)
    }

    pub fn into_inner(self) -> NonNullExt<T> {
        self.0.clone()
    }

    pub fn get_inner(&self) -> &NonNullExt<T> {
        &self.0
    }

    pub fn as_ptr(&self) -> *mut T {
        self.0.as_ptr()
    }

    pub fn cast<U>(self) -> ManagedMutPtr<U> {
        #[cfg(not(debug_assertions))]
        {
            unsafe { ManagedMutPtr::new(self.into_inner().cast()) }
        }

        #[cfg(debug_assertions)]
        {
            // We don't need to call drop() and new().
            let inner = self.0.clone().cast();
            std::mem::forget(self);
            ManagedMutPtr(inner)
        }
    }

    pub fn cast_const(self) -> ManagedConstPtr<T> {
        // *debug mode*
        // Unlike cast(), we need to call drop() and new() here
        // because it's turned into const from mut.
        let inner = self.into_inner();
        unsafe { ManagedConstPtr::new(inner) }
    }
}

#[cfg(debug_assertions)]
impl<T: ?Sized> Drop for ManagedMutPtr<T> {
    fn drop(&mut self) {
        debug::remove_ptr(*self.0);
    }
}

impl<T: ?Sized> Deref for ManagedMutPtr<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // Safety: We assume that the pointer is valid by the constructor.
        unsafe { self.0.as_ref() }
    }
}

impl<T: ?Sized> DerefMut for ManagedMutPtr<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // Safety: We assume that the pointer is valid by the constructor.
        unsafe { self.0.as_mut() }
    }
}

// ManagedMutPtr is not a pure pointer unlike NonNull.
// It can be dereferenced as shared or mutable reference without need for unsafe block.
// That means it's more like a mutable reference.
// So, we don't implement Clone and Copy for it.
// impl<T: ?Sized> Clone for ManagedMutPtr<T> {..}
// impl<T: ?Sized> Copy for ManagedMutPtr<T> {..}

#[cfg(debug_assertions)]
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
        const PTR_SIZE: usize = mem::size_of::<*const ()>();
        const WIDE_PTR_SIZE: usize = PTR_SIZE * 2;

        const _: () = {
            assert!(PTR_SIZE == mem::size_of::<usize>());
            assert!(WIDE_PTR_SIZE == mem::size_of::<[usize; 2]>());
        };

        match size_of_val(&ptr) {
            PTR_SIZE => [0, ptr.as_ptr() as *mut () as usize],
            WIDE_PTR_SIZE => {
                // Safety: We checked the size.
                unsafe { mem::transmute_copy(&ptr) }
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
    pub(super) fn insert_const_ptr<T: ?Sized>(ptr: NonNull<T>) {
        let key = create_key(ptr);
        RC_MAP
            .entry(key)
            .and_modify(|rc| match rc {
                RefCount::Shared(cnt) => {
                    *cnt += 1;
                    assert!(*cnt <= MAX_RC, "too many ManagedConstPtr");
                }
                RefCount::Exclusive => {
                    panic!("ManagedConstPtr cannot be shared with ManagedMutPtr");
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
    pub(super) fn insert_mut_ptr<T: ?Sized>(ptr: NonNull<T>) {
        // If `T` is a zero-sized type, then it's good to be allowed to have
        // the same pointers because they do not mutate the same data.
        // It can be considered as a const pointer.
        //
        // Safety: Even if it's aliased, we do not read the data here.
        if unsafe { size_of_val(ptr.as_ref()) } == 0 {
            insert_const_ptr(ptr);
            return;
        }

        let key = create_key(ptr);
        RC_MAP
            .entry(key)
            .and_modify(|rc| match rc {
                RefCount::Shared(_) => {
                    panic!("ManagedMutPtr cannot be shared with ManagedConstPtr");
                }
                RefCount::Exclusive => {
                    panic!("ManagedMutPtr cannot be shared with ManagedMutPtr");
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
