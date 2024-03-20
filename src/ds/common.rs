use std::{
    any::{self, TypeId},
    marker::PhantomData,
    mem, ptr,
};

pub type FnDropRaw = unsafe fn(*mut u8);
pub type FnCloneRaw = unsafe fn(*const u8, *mut u8);

#[derive(Debug, Clone, Copy)]
pub struct TypeInfo {
    /// Type id.
    pub id: TypeId,

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
            id: TypeId::of::<T>(),
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
    /// [`CloneDetector::type_info`] is same name with [`Uncloneable::type_info`].
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
/// # Examples
///
/// ```
/// # use acttey::tinfo;
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
        use $crate::ds::common::Uncloneable;

        if let Some(info) = $crate::ds::common::CloneDetector::<$ty>::type_info() {
            info
        } else {
            <$ty as $crate::ds::common::AsTypeInfo>::as_type_info()
        }
    }};
}
