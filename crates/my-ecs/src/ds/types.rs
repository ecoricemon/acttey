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

pub type FnDropRaw = unsafe fn(*mut u8);
pub type FnDefaultRaw = unsafe fn(*mut u8);
pub type FnCloneRaw = unsafe fn(*const u8, *mut u8);

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

    /// Raw drop function.
    pub fn_drop: FnDropRaw,

    /// Whether the type is [`Send`] or not.
    pub is_send: bool,

    /// Whether the type is [`Sync`] or not.
    pub is_sync: bool,

    /// Whether the type is [`Default`] or not.
    pub is_default: bool,

    /// Raw [`Default`] function.
    ///
    /// If the type doesn't support `Default`, this causes panic.
    pub fn_default: FnDefaultRaw,

    /// Whether the type is [`Clone`] or not.
    pub is_clone: bool,

    /// Raw [`Clone`] function.
    ///
    /// If the type doesn't support `Clone`, this causes panic.
    pub fn_clone: FnCloneRaw,
}

impl TypeInfo {
    pub fn is_type_of<T: 'static>(&self) -> bool {
        self.ty == TypeId::of::<T>()
    }

    pub fn set_additional(
        &mut self,
        is_send: bool,
        is_sync: bool,
        fn_default: Option<FnDefaultRaw>,
        fn_clone: Option<FnCloneRaw>,
    ) {
        self.is_send = is_send;
        self.is_sync = is_sync;
        if let Some(fn_default) = fn_default {
            self.is_default = true;
            self.fn_default = fn_default;
        }
        if let Some(fn_clone) = fn_clone {
            self.is_clone = true;
            self.fn_clone = fn_clone;
        }
    }
}

pub trait AsTypeInfo {
    fn as_type_info() -> TypeInfo;
}

impl<T: 'static> AsTypeInfo for T {
    fn as_type_info() -> TypeInfo {
        unsafe fn drop<T>(ptr: *mut u8) {
            (ptr as *mut T).drop_in_place();
        }

        TypeInfo {
            ty: TypeId::of::<T>(),
            name: any::type_name::<T>(),
            size: mem::size_of::<T>(),
            align: mem::align_of::<T>(),
            fn_drop: drop::<T>,
            is_send: false,
            is_sync: false,
            is_default: false,
            fn_default: unimpl_default,
            is_clone: false,
            fn_clone: unimpl_clone,
        }
    }
}

/// When someone calls [`TypeHelper::is_send`], rust will look for
/// callable function in the order below
/// - Inherent function
/// - Trait function
///
/// So if the type is `Send`, then rust chooses inherent function
/// due to the search order.  
/// But rust will choose trait function if the type is not `Send`
/// due to the `T: Send` bound.
///
/// See https://doc.rust-lang.org/reference/expressions/method-call-expr.html
/// (Document describes about methods, but I believe the same rule is applied
/// to associated functions as well)
///
/// Here, more specific rules are written.
/// 1. https://rust-lang.github.io/rfcs/0195-associated-items.html#via-an-id_segment-prefix
/// 2. https://rust-lang.github.io/rfcs/0195-associated-items.html#via-a-type_segment-prefix
///
/// 1: tells starting with ID_SEGEMENT is equivalent to starting with TYPE_SEGMENT.
///    'A::b' is equivalent to '<A>::b'
/// 2: tells inherent members are priortized over in-scope traits.
pub struct TypeHelper<T: ?Sized>(PhantomData<T>);

// === TypeHelper for `Send` ===
pub trait NotSend {
    const IS_SEND: bool = false;
}

impl<T: ?Sized> NotSend for TypeHelper<T> {}

impl<T: ?Sized + Send> TypeHelper<T> {
    pub const IS_SEND: bool = true;
}

// === TypeHelper for `Sync` ===
pub trait NotSync {
    const IS_SYNC: bool = false;
}

impl<T: ?Sized> NotSync for TypeHelper<T> {}

impl<T: ?Sized + Sync> TypeHelper<T> {
    pub const IS_SYNC: bool = true;
}

// === TypeHelper for `UnwindSafe` ===
pub trait NotUnwindSafe {
    const IS_UNWIND_SAFE: bool = false;
}

impl<T: ?Sized> NotUnwindSafe for TypeHelper<T> {}

impl<T: ?Sized + UnwindSafe> TypeHelper<T> {
    pub const IS_UNWIND_SAFE: bool = true;
}

// === TypeHelper for `Debug` ===

pub trait NotDebug {
    const IS_DEBUG: bool = false;
    const FN_FMT: FnFmtRaw = unimpl_fmt;
}

impl<T: ?Sized> NotDebug for TypeHelper<T> {}

impl<T: fmt::Debug> TypeHelper<T> {
    pub const IS_DEBUG: bool = true;
    pub const FN_FMT: FnFmtRaw = Self::fn_fmt();

    pub const fn fn_fmt() -> FnFmtRaw {
        unsafe fn fmt<T: fmt::Debug>(
            this: *const u8,
            f: &mut fmt::Formatter<'_>,
        ) -> Result<(), fmt::Error> {
            let this: &T = (this as *const T).as_ref().unwrap();
            write!(f, "{this:?}")
        }

        fmt::<T>
    }
}

pub struct DebugHelper {
    pub f: FnFmtRaw,
    pub ptr: *const u8,
}

impl fmt::Debug for DebugHelper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe { (self.f)(self.ptr, f) }
    }
}

type FnFmtRaw = unsafe fn(*const u8, &mut fmt::Formatter<'_>) -> Result<(), fmt::Error>;

pub(crate) unsafe fn unimpl_fmt(
    _: *const u8,
    _: &mut fmt::Formatter<'_>,
) -> Result<(), fmt::Error> {
    unimplemented!("type doesn't implement Debug");
}

// === TypeHelper for `Default` ===

pub trait NotDefault {
    const IS_DEFAULT: bool = false;
    const FN_DEFAULT: FnDefaultRaw = unimpl_default;
}

impl<T: ?Sized> NotDefault for TypeHelper<T> {}

impl<T: Default> TypeHelper<T> {
    pub const IS_DEFAULT: bool = true;
    pub const FN_DEFAULT: FnDefaultRaw = Self::fn_default();

    pub const fn fn_default() -> FnDefaultRaw {
        unsafe fn default<T: Default>(dst: *mut u8) {
            let src = <T as Default>::default();
            let dst = dst as *mut T;

            ptr::copy_nonoverlapping(&src as *const T, dst, 1);

            mem::forget(src);
        }

        default::<T>
    }
}

pub(crate) unsafe fn unimpl_default(_: *mut u8) {
    unimplemented!("type doesn't implement Default");
}

// === TypeHelper for `Clone` ===

pub trait NotClone {
    const IS_CLONE: bool = false;
    const FN_CLONE: FnCloneRaw = unimpl_clone;
}

impl<T: ?Sized> NotClone for TypeHelper<T> {}

impl<T: Clone> TypeHelper<T> {
    pub const IS_CLONE: bool = true;
    pub const FN_CLONE: FnCloneRaw = Self::fn_clone();

    pub const fn fn_clone() -> FnCloneRaw {
        unsafe fn clone<T: Clone>(src: *const u8, dst: *mut u8) {
            let src = src as *const T;
            let dst = dst as *mut T;

            let src_clone = (*src).clone();
            let src_ptr = &src_clone as *const T;
            ptr::copy_nonoverlapping(src_ptr, dst, 1);

            mem::forget(src_clone);
        }

        clone::<T>
    }
}

pub(crate) unsafe fn unimpl_clone(_: *const u8, _: *mut u8) {
    unimplemented!("type doesn't implement Clone");
}

// === TypeHelper for EqualType ===

pub trait NotEqualType {
    const IS_EQUAL_TYPE: bool = false;
}

impl<T> NotEqualType for TypeHelper<T> {}

impl<T> TypeHelper<(T, T)> {
    pub const IS_EQUAL_TYPE: bool = true;
}

/// Creates [`TypeInfo`] from the given type and reflects whether or not
/// the type implements [`Clone`], [`Send`], and [`Sync`] to the TypeInfo.
/// This macro exploits Rust's function look-up procedures to determine
/// if the type implenets `Clone`. See [`TypeHelper`] for more details.
///
/// Plus, you can re-assign type's name by putting yours in
/// like `tinfo!(T, "new-name")`.
///
/// # Examples
///
/// ```
/// # use my_ecs::tinfo;
/// // - Clone detection
///
/// struct A;
/// struct B;
/// #[derive(Clone)]
/// struct C;
/// #[derive(Clone)]
/// struct D;
///
/// let a = tinfo!(A); // for uncloneable type A.
/// let b = tinfo!(B); // for uncloneable type B.
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
/// struct Good(u8); // Both Send and Sync.
/// struct NotGood(*mut u8); // Neither Send nor Sync.
///
/// let good = tinfo!(Good);
/// let not_good = tinfo!(NotGood);
///
/// assert!(good.is_send);
/// assert!(good.is_sync);
/// assert!(!not_good.is_send);
/// assert!(!not_good.is_sync);
/// ```
#[macro_export]
macro_rules! tinfo {
    ($ty:ty) => {{
        #[allow(unused_imports)]
        use $crate::ds::types::{AsTypeInfo, NotClone, NotDefault, NotSend, NotSync, TypeHelper};

        let mut tinfo = <$ty as AsTypeInfo>::as_type_info();
        tinfo.set_additional(
            TypeHelper::<$ty>::IS_SEND,
            TypeHelper::<$ty>::IS_SYNC,
            TypeHelper::<$ty>::IS_DEFAULT.then_some(TypeHelper::<$ty>::FN_DEFAULT),
            TypeHelper::<$ty>::IS_CLONE.then_some(TypeHelper::<$ty>::FN_CLONE),
        );
        tinfo
    }};
    ($ty:ty, $name:literal) => {{
        #[allow(unused_imports)]
        use $crate::ds::types::{AsTypeInfo, NotClone, NotDefault, NotSend, NotSync, TypeHelper};

        let mut tinfo = <$ty as AsTypeInfo>::as_type_info();
        tinfo.set_additional(
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
    pub const fn new(ty: TypeId) -> Self {
        Self {
            inner: ty,
            #[cfg(feature = "check")]
            name: "",
        }
    }

    pub const fn with(self, _name: &'static str) -> Self {
        #[cfg(feature = "check")]
        {
            let mut this = self;
            this.name = _name;
        }
        self
    }

    pub fn of<T: ?Sized + 'static>() -> Self {
        Self {
            inner: TypeId::of::<T>(),
            #[cfg(feature = "check")]
            name: any::type_name::<T>(),
        }
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

/// [`TypeId`] with a salt type.
/// Consider using this when you need new type for the `TypeId`.
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
    pub const fn new(ty: TypeIdExt) -> Self {
        Self {
            inner: ty,
            _marker: PhantomData,
        }
    }

    pub fn of<T: ?Sized + 'static>() -> Self {
        Self {
            inner: TypeIdExt::of::<T>(),
            _marker: PhantomData,
        }
    }

    pub fn into_inner(self) -> TypeIdExt {
        self.inner
    }

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
