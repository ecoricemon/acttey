use std::{
    any::{self, TypeId},
    borrow, cmp, fmt,
    hash::Hash,
    marker::PhantomData,
    mem,
    ops::Deref,
    ptr,
};

pub type FnDropRaw = unsafe fn(*mut u8);
pub type FnCloneRaw = unsafe fn(*const u8, *mut u8);

#[derive(Debug, Clone, Copy)]
pub struct TypeInfo {
    /// Type id.
    pub ty: TypeId,

    /// Type name.
    /// This field may differ from rust version to version.
    pub name: &'static str,

    /// Type size in bytes.
    pub size: usize,

    /// Type alignment in bytes.
    pub align: usize,

    /// Raw level drop function.
    pub fn_drop: FnDropRaw,

    /// Raw level clone function. If the type doesn't support clone, this must cause panic.
    pub fn_clone: FnCloneRaw,
}

pub trait AsTypeInfo {
    fn as_type_info() -> TypeInfo;
}

impl<T: 'static> AsTypeInfo for T {
    fn as_type_info() -> TypeInfo {
        unsafe fn drop<T>(ptr: *mut u8) {
            (ptr as *mut T).drop_in_place();
        }

        unsafe fn clone(_: *const u8, _: *mut u8) {
            panic!("type doesn't implement Clone");
        }

        TypeInfo {
            ty: TypeId::of::<T>(),
            name: any::type_name::<T>(),
            size: mem::size_of::<T>(),
            align: mem::align_of::<T>(),
            fn_drop: drop::<T>,
            fn_clone: clone,
        }
    }
}

pub trait Uncloneable {
    fn type_info() -> Option<TypeInfo>;
}

impl<T: 'static> Uncloneable for T {
    fn type_info() -> Option<TypeInfo> {
        None
    }
}

pub struct CloneDetector<T: 'static>(PhantomData<T>);

impl<T: 'static + Clone> CloneDetector<T> {
    /// [`CloneDetector::type_info`] has the same name as the [`Uncloneable::type_info`].
    /// In this case, inherent function has priority over the one in the trait.
    /// See https://github.com/rust-lang/rust/issues/26007.
    /// (But if the function has receiver, the other rule defines priority. See https://doc.rust-lang.org/reference/expressions/method-call-expr.html)
    /// As a result, calling "CloneDetector::\<T\>::type_info" with clonable T invokes this method due to the higher priority, while calling it with uncloneable T invokes trait function due to the CloneDetector's bounds.
    pub fn type_info() -> Option<TypeInfo> {
        unsafe fn clone<T: Clone>(src: *const u8, dst: *mut u8) {
            let src = src as *const T;
            let dst = dst as *mut T;

            let src_clone = (*src).clone();
            let src_ptr = &src_clone as *const T;
            ptr::copy_nonoverlapping(src_ptr, dst, 1);

            mem::forget(src_clone);
        }

        let mut tinfo = <T as AsTypeInfo>::as_type_info();
        tinfo.fn_clone = clone::<T>;
        Some(tinfo)
    }
}

/// Creates [`TypeInfo`] from the given type and reflects whether or not the type implements [`Clone`] to the TypeInfo.
/// This macro exploits Rust's function look-up procedures to determine if the type implenets `Clone`.
/// See [`CloneDetector::type_info`] for more details.
///
/// Plus, you can re-assign type's name by putting yours in like `tinfo!(T, "new-name")`.
///
/// # Examples
///
/// ```
/// # use my_ecs::tinfo;
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
/// ```
#[macro_export]
macro_rules! tinfo {
    ($ty:ty) => {{
        #[allow(unused)]
        use $crate::ds::types::{AsTypeInfo, CloneDetector, Uncloneable};

        if let Some(info) = CloneDetector::<$ty>::type_info() {
            info
        } else {
            <$ty as AsTypeInfo>::as_type_info()
        }
    }};
    ($ty:ty, $name:literal) => {{
        #[allow(unused)]
        use $crate::ds::common::{AstypeInfo, CloneDetector, Uncloneable};

        let mut info = if let Some(info) = CloneDetector::<$ty>::type_info() {
            info
        } else {
            <$ty as AsTypeInfo>::as_type_info()
        };
        info.name = $name;
        info
    }};
}

#[cfg_attr(not(debug_assertions), repr(transparent), derive(Debug))]
pub struct TypeIdExt {
    inner: TypeId,
    #[cfg(debug_assertions)]
    name: &'static str,
}

#[cfg(debug_assertions)]
impl fmt::Debug for TypeIdExt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[cfg(not(debug_assertions))]
        {
            self.inner.fmt(f)
        }

        #[cfg(debug_assertions)]
        {
            write!(f, "TypeIdExt({})", self.name)
        }
    }
}

impl TypeIdExt {
    pub const fn new(ty: TypeId) -> Self {
        Self {
            inner: ty,
            #[cfg(debug_assertions)]
            name: "",
        }
    }

    pub const fn with(mut self, name: &'static str) -> Self {
        #[cfg(debug_assertions)]
        {
            self.name = name;
        }
        self
    }

    pub fn of<T: ?Sized + 'static>() -> Self {
        Self {
            inner: TypeId::of::<T>(),
            #[cfg(debug_assertions)]
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
