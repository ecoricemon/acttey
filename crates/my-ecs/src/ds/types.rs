use std::{
    any::{self, TypeId},
    borrow, cmp, fmt,
    hash::Hash,
    marker::PhantomData,
    mem,
    ops::Deref,
    panic::UnwindSafe,
    ptr,
};

/// Type information such as [`TypeId`], name, size, and alignment.
///
/// Also, the struct contains additional information shown below.
/// - Type erased drop function.
/// - Whether the type is [`Send`] or not.
/// - Whether the type is [`Sync`] or not.
/// - Whether the type is [`Clone`] or not and type erased clone function.
/// - Whether the type is [`Default`] or not and type erased default function.
///
/// It's highly encouraged to use [`tinfo`] macro to construct this struct to
/// avoid incorrect construction.
///
/// # Examples
///
/// ```
/// use my_ecs::tinfo;
///
/// let x = tinfo!(i32);
/// assert!(x.is_send && x.is_sync && x.is_default && x.is_clone);
///
/// let x = tinfo!(std::rc::Rc<i32>);
/// assert!(!x.is_send && !x.is_sync && x.is_default && x.is_clone);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeInfo {
    /// Type id.
    pub ty: TypeId,

    /// Type name.
    ///
    /// This field may differ from rust version to version.
    pub name: &'static str,

    /// Type size in bytes.
    ///
    /// Size must be a multiple of `align` including zero.
    pub size: usize,

    /// Type alignment in bytes.
    ///
    /// Alignment must be a power of two, at lease one.
    pub align: usize,

    /// Raw [`Drop::drop`] function pointer for the item type.
    pub fn_drop: FnDropRaw,

    /// Whether the type is [`Send`] or not.
    pub is_send: bool,

    /// Whether the type is [`Sync`] or not.
    pub is_sync: bool,

    /// Whether the type is [`Default`] or not.
    pub is_default: bool,

    /// Raw [`Default::default`] function pointer for the item type.
    ///
    /// If the item type is not [`Default`], calling this function causes panic.
    pub fn_default: FnDefaultRaw,

    /// Whether the type is [`Clone`] or not.
    pub is_clone: bool,

    /// Raw [`Clone::clone`] function pointer for the item type.
    ///
    /// If the item type is not [`Clone`], calling this function causes panic.
    pub fn_clone: FnCloneRaw,
}

impl TypeInfo {
    /// Creates a [`TypeInfo`] for the given type.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::ds::{TypeInfo, TypeHelper};
    ///
    /// #[derive(Clone)]
    /// struct X;
    ///
    /// // Creates `TypeInfo` for the `X`.
    /// let is_send = true;
    /// let is_sync = true;
    /// let fn_default = None;
    /// let fn_clone = Some(TypeHelper::<X>::FN_CLONE);
    /// let info = TypeInfo::new::<X>(is_send, is_sync, fn_default, fn_clone);
    /// ```
    pub fn new<T: 'static>(
        is_send: bool,
        is_sync: bool,
        fn_default: Option<FnDefaultRaw>,
        fn_clone: Option<FnCloneRaw>,
    ) -> Self {
        /// Calls [`drop_in_place`](ptr::drop_in_place) on the given pointer.
        ///
        /// # Safety
        ///
        /// `ptr` must be a properly aligned and nonnull pointer of a certain
        /// type `T`.
        /// `ptr` must be valid for writes.
        /// `ptr` must be valid for dropping.
        unsafe fn drop<T>(ptr: *mut u8) {
            // Safety: Reflected in the function.
            unsafe { (ptr as *mut T).drop_in_place() }
        }

        let (is_default, fn_default) = if let Some(fn_default) = fn_default {
            (true, fn_default)
        } else {
            (false, unimpl_default as FnDefaultRaw)
        };
        let (is_clone, fn_clone) = if let Some(fn_clone) = fn_clone {
            (true, fn_clone)
        } else {
            (false, unimpl_clone as FnCloneRaw)
        };

        TypeInfo {
            ty: TypeId::of::<T>(),
            name: any::type_name::<T>(),
            size: size_of::<T>(),
            align: align_of::<T>(),
            fn_drop: drop::<T>,
            is_send,
            is_sync,
            is_default,
            fn_default,
            is_clone,
            fn_clone,
        }
    }

    /// Returns true if the type information is about the given type.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// let tinfo = tinfo!(i32);
    /// assert!(tinfo.is_type_of::<i32>());
    /// ```
    pub fn is_type_of<T: 'static>(&self) -> bool {
        self.ty == TypeId::of::<T>()
    }
}

/// Type-erased raw [`Drop::drop`] function pointer type.
pub type FnDropRaw = unsafe fn(*mut u8);

/// A helper struct used to determine whether a type implements traits like
/// [`Send`], [`Sync`], and [`Clone`] by cooperating with helper traits like
/// [`NotSend`] , [`NotSync`], and [`NotClone`].
///
/// Helper traits basically have associated constants meaning whether a type
/// imeplements a trait such as `Send`. They are set to `false` by default, and
/// all types implement the helper types by blanket implementation. In other
/// words, all types are not `Send`, `Sync` by the helper traits, etc. But
/// [`TypeHelper`] can overwrite it for types that actually implement those
/// traits thanks to its trait bound. As a result, clients can be aware of
/// whether a type impelments a certain trait through `TypeHelper`. This is
/// especially useful when you make a library that receives anonymous type and
/// you need such type information.
///
/// # How it works
///
/// If a struct has function `foo<T: Send>` and it also implement `Foo` which
/// has the same signature function `foo<T>`, then rust will look for callable
/// function in the order below.
/// - Inherent function
/// - Trait function
///
/// So if the type is `Send`, then rust chooses inherent function due to the
/// search order.  But rust will choose trait function if the type is not `Send`
/// due to the `T: Send` bound.
///
/// See <https://doc.rust-lang.org/reference/expressions/method-call-expr.html>
/// (Document describes about methods, but I believe the same rule is applied
/// to associated functions as well)
///
/// Here, more specific rules are written.
/// 1. <https://rust-lang.github.io/rfcs/0195-associated-items.html#via-an-id_segment-prefix>
/// 2. <https://rust-lang.github.io/rfcs/0195-associated-items.html#via-a-type_segment-prefix>
///
/// - `1` tells starting with ID_SEGMENT is equivalent to starting with
///   TYPE_SEGMENT. 'A::b' is equivalent to '\<A\>::b'
/// - `2` tells inherent members are prioritized over in-scope traits.
pub struct TypeHelper<T: ?Sized>(PhantomData<T>);

// === TypeHelper for `Send` ===

/// A helper trait for [`TypeHelper`] to detect not [`Send`] types.
///
/// See [`TypeHelper`] documentation for more details.
pub trait NotSend {
    const IS_SEND: bool = false;
}

impl<T: ?Sized> NotSend for TypeHelper<T> {}

impl<T: ?Sized + Send> TypeHelper<T> {
    pub const IS_SEND: bool = true;
}

// === TypeHelper for `Sync` ===

/// A helper trait for [`TypeHelper`] to detect not [`Sync`] types.
///
/// See [`TypeHelper`] documentation for more details.
pub trait NotSync {
    const IS_SYNC: bool = false;
}

impl<T: ?Sized> NotSync for TypeHelper<T> {}

impl<T: ?Sized + Sync> TypeHelper<T> {
    pub const IS_SYNC: bool = true;
}

// === TypeHelper for `UnwindSafe` ===

/// A helper trait for [`TypeHelper`] to detect not [`UnwindSafe`] types.
///
/// See [`TypeHelper`] documentation for more details.
pub trait NotUnwindSafe {
    const IS_UNWIND_SAFE: bool = false;
}

impl<T: ?Sized> NotUnwindSafe for TypeHelper<T> {}

impl<T: ?Sized + UnwindSafe> TypeHelper<T> {
    pub const IS_UNWIND_SAFE: bool = true;
}

// === TypeHelper for `Debug` ===

/// A helper trait for [`TypeHelper`] to detect not [`Debug`](fmt::Debug) types.
///
/// See [`TypeHelper`] documentation for more details.
pub trait NotDebug {
    const IS_DEBUG: bool = false;
    const FN_FMT: FnFmtRaw = unimpl_fmt;
}

impl<T: ?Sized> NotDebug for TypeHelper<T> {}

impl<T: fmt::Debug> TypeHelper<T> {
    pub const IS_DEBUG: bool = true;
    pub const FN_FMT: FnFmtRaw = Self::fn_fmt();

    /// Returns a raw function pointer of [`Debug::fmt`](fmt::Debug::fmt) for
    /// the given type.
    ///
    /// But the given type is not [`Debug`](fmt::Debug), then calling the
    /// returned function will cause panic.
    pub const fn fn_fmt() -> FnFmtRaw {
        unsafe fn fmt<T: fmt::Debug>(
            this: *const u8,
            f: &mut fmt::Formatter<'_>,
        ) -> Result<(), fmt::Error> {
            let this = this.cast::<T>();
            // Safety: Reflected in the `FnFmtRaw`.
            unsafe { (*this).fmt(f) }
        }

        fmt::<T>
    }
}

/// Type-erased raw [`Debug::fmt`](fmt::Debug::fmt) function pointer type.
///
/// To get a function pointer from a certain type, you can call
/// [`TypeHelper::fn_fmt`]. Also, there is a helper type [`DebugHelper`]. It
/// allows you to use the raw function pointer in `{:?}` patterns.
///
/// # Panics
///
/// If the function pointer was generated from a type that is not
/// [`Debug`](fmt::Debug), calling the function causes panic.
///
/// # Safety
///
/// - `src` must be a properly aligned and non-null pointer of a certain type
///   `T`.
/// - `src` must be valid for read of `T`.
pub type FnFmtRaw = unsafe fn(src: *const u8, &mut fmt::Formatter<'_>) -> Result<(), fmt::Error>;

pub(crate) unsafe fn unimpl_fmt(
    _: *const u8,
    _: &mut fmt::Formatter<'_>,
) -> Result<(), fmt::Error> {
    unimplemented!("type is not `Debug`");
}

/// A helper type for the [`FnFmtRaw`].
///
/// This type implements [`Deubg`](fmt::Debug), therefore it's useful when you
/// want to print something out using the `FnFmtRaw`.
///
/// # Examples
///
/// ```
/// use my_ecs::ds::{TypeHelper, DebugHelper};
///
/// let value = 123_i32;
/// let fn_fmt = TypeHelper::<i32>::fn_fmt();
/// let helper = DebugHelper {
///     f: fn_fmt,
///     ptr: &value as *const i32 as *const u8
/// };
/// println!("{helper:?}");
/// ```
pub struct DebugHelper {
    pub f: FnFmtRaw,
    pub ptr: *const u8,
}

impl fmt::Debug for DebugHelper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe { (self.f)(self.ptr, f) }
    }
}

// === TypeHelper for `Default` ===

/// A helper trait for [`TypeHelper`] to detect not [`Default`] types.
///
/// See [`TypeHelper`] documentation for more details.
pub trait NotDefault {
    const IS_DEFAULT: bool = false;
    const FN_DEFAULT: FnDefaultRaw = unimpl_default;
}

impl<T: ?Sized> NotDefault for TypeHelper<T> {}

impl<T: Default> TypeHelper<T> {
    pub const IS_DEFAULT: bool = true;
    pub const FN_DEFAULT: FnDefaultRaw = Self::fn_default();

    /// Returns raw function pointer of [`Default::default`] for the given type.
    ///
    /// But the given type is not [`Default`], then calling the returned
    /// function will cause panic.
    pub const fn fn_default() -> FnDefaultRaw {
        unsafe fn default<T: Default>(dst: *mut u8) {
            let src = <T as Default>::default();
            let dst = dst.cast::<T>();

            // Safety: Reflected in the `FnDefaultRaw`.
            unsafe { ptr::copy_nonoverlapping(&src as *const T, dst, 1) };
            mem::forget(src);
        }

        default::<T>
    }
}

/// Type-erased raw [`Default::default`] function pointer type.
///
/// Call [`TypeHelper::fn_default`] to get the function pointer.
///
/// # Panics
///
/// If the function pointer was generated from a type that is not [`Default`],
/// calling the function causes panic.
///
/// # Safety
///
/// - `dst` must be a properly aligned and nonnull pointer of a certain type
///   `T`.
/// - `dst` must be valid for write of `T`.
pub type FnDefaultRaw = unsafe fn(dst: *mut u8);

pub(crate) unsafe fn unimpl_default(_: *mut u8) {
    unimplemented!("type is not `Default`");
}

// === TypeHelper for `Clone` ===

/// A helper trait for [`TypeHelper`] to detect not [`Clone`] types.
///
/// See [`TypeHelper`] documentation for more details.
pub trait NotClone {
    const IS_CLONE: bool = false;
    const FN_CLONE: FnCloneRaw = unimpl_clone;
}

impl<T: ?Sized> NotClone for TypeHelper<T> {}

impl<T: Clone> TypeHelper<T> {
    pub const IS_CLONE: bool = true;
    pub const FN_CLONE: FnCloneRaw = Self::fn_clone();

    /// Returns raw function pointer of [`Clone::clone`] for the given type.
    ///
    /// But the given type is not [`Clone`], then calling the returned function
    /// will cause panic.
    pub const fn fn_clone() -> FnCloneRaw {
        unsafe fn clone<T: Clone>(src: *const u8, dst: *mut u8) {
            let src = src.cast::<T>();
            let dst = dst.cast::<T>();

            // Safety: Reflected in the `FnCloneRaw`.
            unsafe {
                let src_cloned = (*src).clone();
                ptr::copy_nonoverlapping(&src_cloned as *const T, dst, 1);
                mem::forget(src_cloned);
            }
        }

        clone::<T>
    }
}

/// Type-erased raw [`Clone::clone`] function pointer type.
///
/// Call [`TypeHelper::fn_clone`] to get the function pointer.
///
/// # Panics
///
/// If the function pointer was generated from a type that is not [`Clone`],
/// calling the function causes panic.
///
/// # Safety
///
/// This function calls [`copy_nonoverlapping`](ptr::copy_nonoverlapping)
/// internally. Therefore, function follows the same safety conditions like
/// below.
///
/// - Both `src` and `dst` must be properly aligned and nonnull pointers of a
///   certain type `T`.
/// - `src` must be valid for read of `T`.
/// - `dst` must be valid for write of `T`.
/// - Region of `src` memory must not overlap with region of `dst` memory.
pub type FnCloneRaw = unsafe fn(src: *const u8, dst: *mut u8);

pub(crate) unsafe fn unimpl_clone(_: *const u8, _: *mut u8) {
    unimplemented!("type is not `Clone`");
}

// === TypeHelper for EqualType ===

/// A helper trait for [`TypeHelper`] to detect not equal types.
///
/// See [`TypeHelper`] documentation for more details.
pub trait NotEqualType {
    const IS_EQUAL_TYPE: bool = false;
}

impl<T> NotEqualType for TypeHelper<T> {}

impl<T> TypeHelper<(T, T)> {
    pub const IS_EQUAL_TYPE: bool = true;
}

/// Creates [`TypeInfo`] from the given type and reflects whether the type
/// implements [`Send`], [`Sync`], [`Default`], and [`Clone`] to the TypeInfo.
///
/// If you want your own type name, you can call this macro like
/// `tinfo!(T, "new-name")`.
///
/// This macro exploits Rust's function look-up procedures to determine if the
/// type implements the traits. See [`TypeHelper`] for more details.
///
/// # Examples
///
/// ```
/// use my_ecs::prelude::*;
///
/// // - Clone detection
///
/// struct A;
/// struct B;
/// #[derive(Clone)]
/// struct C;
/// #[derive(Clone)]
/// struct D;
///
/// let a = tinfo!(A); // for non-cloneable type A.
/// let b = tinfo!(B); // for non-cloneable type B.
/// let c = tinfo!(C); // for cloneable type C.
/// let d = tinfo!(D); // for cloneable type D.
///
/// assert_eq!(a.fn_clone, b.fn_clone); // A and B have the same dummy clone function.
/// assert_ne!(a.fn_clone, c.fn_clone); // But C has its own clone function.
/// assert_ne!(a.fn_clone, d.fn_clone); // And so does D.
/// assert_ne!(c.fn_clone, d.fn_clone);
///
/// // - Send & Sync detection
///
/// struct SendSync(u8); // Both Send and Sync.
/// struct NotSendSync(*mut u8); // Neither Send nor Sync.
///
/// let send_sync = tinfo!(SendSync);
/// let not_send_sync = tinfo!(NotSendSync);
///
/// assert!(send_sync.is_send);
/// assert!(send_sync.is_sync);
/// assert!(!not_send_sync.is_send);
/// assert!(!not_send_sync.is_sync);
///
/// // Incorrect usage
///
/// fn is_clone<T: 'static>() -> bool {
///     // The macro doesn't work if the type is passed through generic
///     // parameter.
///     tinfo!(T).is_clone
/// }
///
/// // assert!(is_clone::<C>());
/// # assert!(!is_clone::<C>());
///
/// ```
#[macro_export]
macro_rules! tinfo {
    ($ty:ty) => {{
        #[allow(unused_imports)]
        use $crate::ds::{NotClone, NotDefault, NotSend, NotSync, TypeHelper, TypeInfo};

        TypeInfo::new::<$ty>(
            TypeHelper::<$ty>::IS_SEND,
            TypeHelper::<$ty>::IS_SYNC,
            TypeHelper::<$ty>::IS_DEFAULT.then_some(TypeHelper::<$ty>::FN_DEFAULT),
            TypeHelper::<$ty>::IS_CLONE.then_some(TypeHelper::<$ty>::FN_CLONE),
        )
    }};
    ($ty:ty, $name:literal) => {{
        #[allow(unused_imports)]
        use $crate::ds::types::{NotClone, NotDefault, NotSend, NotSync, TypeHelper, TypeInfo};

        let mut tinfo = TypeInfo::new::<$ty>(
            TypeHelper::<$ty>::IS_SEND,
            TypeHelper::<$ty>::IS_SYNC,
            TypeHelper::<$ty>::IS_DEFAULT.then_some(TypeHelper::<$ty>::FN_DEFAULT),
            TypeHelper::<$ty>::IS_CLONE.then_some(TypeHelper::<$ty>::FN_CLONE),
        );
        tinfo.name = $name;
        tinfo
    }};
}

// The macro is exported and will be shown up in crate level. So we can use the
// macro like 'crate::tinfo!(..)'. In doc comments, however, the link to the
// macro is generated something like 'crate::ds::types::tinfo'. Re-exporting
// like this helps us address the issue.
pub use crate::tinfo;

/// Represents an extended [`TypeId`] with type name.
///
/// This would be useful for enriching debug messages, but the type name is only
/// included when `check` feature is enabled.
#[cfg_attr(not(feature = "check"), repr(transparent))]
pub struct TypeIdExt {
    inner: TypeId,
    #[cfg(feature = "check")]
    name: &'static str,
}

#[cfg(feature = "check")]
impl fmt::Debug for TypeIdExt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[cfg(not(feature = "check"))]
        {
            self.inner.fmt(f)
        }

        #[cfg(feature = "check")]
        {
            write!(f, "TypeIdExt({})", self.name)
        }
    }
}

impl TypeIdExt {
    /// Creates a new [`TypeIdExt`] from the given [`TypeId`].
    ///
    /// If `check` feature is enabled, type name is set to blank string. To set
    /// a proper name, call [`TypeIdExt::with`].
    pub const fn new(ty: TypeId) -> Self {
        Self {
            inner: ty,
            #[cfg(feature = "check")]
            name: "",
        }
    }

    /// Creates a new [`TypeIdExt`] with the given name.
    ///
    /// If `check` feature is disabled, this is no-op.
    #[cfg_attr(not(feature = "check"), allow(unused_variables))]
    pub const fn with(self, name: &'static str) -> Self {
        #[cfg(feature = "check")]
        {
            let mut this = self;
            this.name = name;
        }
        self
    }

    /// Creates a new [`TypeIdExt`] from the given type.
    pub fn of<T: ?Sized + 'static>() -> Self {
        Self {
            inner: TypeId::of::<T>(),
            #[cfg(feature = "check")]
            name: any::type_name::<T>(),
        }
    }

    /// Returns type name.
    #[cfg(feature = "check")]
    pub const fn name(&self) -> &'static str {
        self.name
    }
}

impl Deref for TypeIdExt {
    type Target = TypeId;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl Clone for TypeIdExt {
    fn clone(&self) -> Self {
        *self
    }
}

impl Copy for TypeIdExt {}

impl PartialEq<Self> for TypeIdExt {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl PartialEq<TypeId> for TypeIdExt {
    fn eq(&self, other: &TypeId) -> bool {
        &self.inner == other
    }
}

impl Eq for TypeIdExt {}

impl PartialOrd<Self> for TypeIdExt {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialOrd<TypeId> for TypeIdExt {
    fn partial_cmp(&self, other: &TypeId) -> Option<cmp::Ordering> {
        self.inner.partial_cmp(other)
    }
}

impl Ord for TypeIdExt {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.inner.cmp(&other.inner)
    }
}

impl Hash for TypeIdExt {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state)
    }
}

impl borrow::Borrow<TypeId> for TypeIdExt {
    fn borrow(&self) -> &TypeId {
        &self.inner
    }
}

impl From<&TypeInfo> for TypeIdExt {
    fn from(value: &TypeInfo) -> Self {
        Self::new(value.ty).with(value.name)
    }
}

/// A [`TypeIdExt`] with a salt type.
///
/// By adding a salt type, this type can be different from each other. For
/// instance, if we have `ATypeId<i32>` and `ATypeId<u32>`, they are completely
/// different types from perspective of Rust.
#[repr(transparent)]
pub struct ATypeId<Salt> {
    inner: TypeIdExt,
    _marker: PhantomData<Salt>,
}

impl<Salt> fmt::Debug for ATypeId<Salt> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl<Salt> ATypeId<Salt> {
    /// Creates a new [`ATypeId`] from the given [`TypeIdExt`].
    pub const fn new(ty: TypeIdExt) -> Self {
        Self {
            inner: ty,
            _marker: PhantomData,
        }
    }

    /// Creates a new [`ATypeId`] from the given type.
    pub fn of<T: ?Sized + 'static>() -> Self {
        Self {
            inner: TypeIdExt::of::<T>(),
            _marker: PhantomData,
        }
    }

    /// Converts [`ATypeId`] into [`TypeIdExt`] by unwraping self.
    pub fn into_inner(self) -> TypeIdExt {
        self.inner
    }

    /// Returns a shared reference to inner [`TypeIdExt`].
    pub fn get_inner(&self) -> &TypeIdExt {
        &self.inner
    }
}

impl<Salt> Deref for ATypeId<Salt> {
    type Target = TypeIdExt;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<Salt> Clone for ATypeId<Salt> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<Salt> Copy for ATypeId<Salt> {}

impl<Salt> PartialEq for ATypeId<Salt> {
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl<Salt> Eq for ATypeId<Salt> {}

impl<Salt> PartialOrd for ATypeId<Salt> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<Salt> Ord for ATypeId<Salt> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.inner.cmp(&other.inner)
    }
}

impl<Salt> Hash for ATypeId<Salt> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state)
    }
}

impl<Salt> From<&TypeInfo> for ATypeId<Salt> {
    fn from(value: &TypeInfo) -> Self {
        Self {
            inner: TypeIdExt::from(value),
            _marker: PhantomData,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[allow(unused)]
    #[rustfmt::skip]
    fn test_type_helper() {
        use std::alloc::{self, Layout};

        // Detects `Debug`.
        #[derive(Debug)]
        struct DebugTrue(i32);
        struct DebugFalse(i32);
        const _: () = {
            assert!(TypeHelper::<DebugTrue>::IS_DEBUG);
            assert!(!TypeHelper::<DebugFalse>::IS_DEBUG);
        };

        // Validates `TypeHelper::fn_fmt`.
        let v = DebugTrue(42);
        let helper = DebugHelper {
            f: TypeHelper::<DebugTrue>::fn_fmt(),
            ptr: &v as *const _ as *const u8
        };
        let formatted = format!("{helper:?}");
        assert_eq!(&formatted, "DebugTrue(42)");

        // Detects `Default`.
        struct DefaultInner(i32);
        impl Default for DefaultInner { fn default() -> Self { Self(42) } }
        #[derive(Default)]
        struct DefaultTrue(DefaultInner);
        struct DefaultFalse;
        const _: () = {
            assert!(TypeHelper::<DefaultTrue>::IS_DEFAULT);
            assert!(!TypeHelper::<DefaultFalse>::IS_DEFAULT);
        };

        // Validates `TypeHelper::fn_default`.
        let layout = Layout::new::<DefaultTrue>();
        let fn_default = TypeHelper::<DefaultTrue>::fn_default();
        unsafe {
            let buf = alloc::alloc(layout);
            fn_default(buf);
            let v = &*buf.cast::<DefaultTrue>();
            assert_eq!(v.0.0, 42);
            alloc::dealloc(buf, layout);
        }

        // Detects `Clone`.
        #[derive(Clone)]
        struct CloneTrue(i32);
        struct CloneFalse(i32);
        const _: () = {
            assert!(TypeHelper::<CloneTrue>::IS_CLONE);
            assert!(!TypeHelper::<CloneFalse>::IS_CLONE);
        };

        // Validates `TypeHelper::fn_clone`.
        let (src, mut dst) = (CloneTrue(42), CloneTrue(0));
        let fn_clone = TypeHelper::<CloneTrue>::fn_clone();
        unsafe {
            let src_ptr = &src as *const _ as *const u8;
            let dst_ptr = &mut dst as *mut _ as *mut u8;
            fn_clone(src_ptr, dst_ptr);
        }
        assert_eq!(src.0, dst.0);

        // Detects `Send`.
        struct SendTrue;
        struct SendFalse(*const ());
        const _: () = {
            assert!(TypeHelper::<SendTrue>::IS_SEND);
            assert!(!TypeHelper::<SendFalse>::IS_SEND);
        };

        // Detects `Sync`.
        struct SyncTrue;
        struct SyncFalse(*const ());
        const _: () = {
            assert!(TypeHelper::<SyncTrue>::IS_SYNC);
            assert!(!TypeHelper::<SyncFalse>::IS_SYNC);
        };

        // Detects `EqualType`.
        struct A;
        struct B;
        const _: () = {
            assert!(TypeHelper::<(A, A)>::IS_EQUAL_TYPE);
            assert!(!TypeHelper::<(A, B)>::IS_EQUAL_TYPE);
        };
    }
}
