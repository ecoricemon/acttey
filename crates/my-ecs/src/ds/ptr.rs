use std::{
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

#[derive(Debug)]
#[repr(transparent)]
pub struct ManagedConstPtr<T: ?Sized>(NonNull<T>);

/// The pointer is managed to be valid and not aliased.
unsafe impl<T> Send for ManagedConstPtr<T> {}

impl<T: ?Sized> ManagedConstPtr<T> {
    /// # Safety
    ///
    /// The pointer must be valid and not aliased mutably while the instance is in use.
    pub const unsafe fn new(ptr: NonNull<T>) -> Self {
        Self(ptr)
    }

    pub const fn into_inner(self) -> NonNull<T> {
        self.0
    }
}

impl<T: ?Sized> Deref for ManagedConstPtr<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // Safety: We assume that the pointer is valid by the constructor.
        unsafe { self.0.as_ref() }
    }
}

/// The pointer is managed to be valid and not aliased.
unsafe impl<T> Send for ManagedMutPtr<T> {}

#[derive(Debug)]
#[repr(transparent)]
pub struct ManagedMutPtr<T: ?Sized>(NonNull<T>);

impl<T: ?Sized> ManagedMutPtr<T> {
    /// # Safety
    ///
    /// The pointer must be valid and not aliased while the instance is in use.
    pub const unsafe fn new(ptr: NonNull<T>) -> Self {
        Self(ptr)
    }

    pub const fn into_inner(self) -> NonNull<T> {
        self.0
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
