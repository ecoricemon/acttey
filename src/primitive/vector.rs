use std::ops::Deref;

// TODO: Enough small number?
const F32_EPS: f32 = 1e-6;
const U8_EPS: u8 = 1;
const U16_EPS: u16 = 1;

pub trait Number {
    type Output;
    fn zero() -> Self::Output;
    fn one() -> Self::Output;
    fn max() -> Self::Output;
    fn _sqrt(self) -> Self::Output;
    fn _acos(self) -> Self::Output;
    fn from_f32(v: f32) -> Self::Output;
    fn from_f64(v: f64) -> Self::Output;
    fn eps() -> Self::Output;
}

impl Number for u8 {
    type Output = u8;

    #[inline]
    fn zero() -> Self::Output {
        0
    }

    #[inline]
    fn one() -> Self::Output {
        1
    }

    #[inline]
    fn max() -> Self::Output {
        u8::MAX
    }

    #[inline]
    fn _sqrt(self) -> Self::Output {
        panic!("Oops! There's no sqrt() for u8")
    }

    #[inline]
    fn _acos(self) -> Self::Output {
        panic!("Oops! There's no acos() for u8")
    }

    #[inline]
    fn from_f32(v: f32) -> Self::Output {
        v as Self::Output
    }

    #[inline]
    fn from_f64(v: f64) -> Self::Output {
        v as Self::Output
    }

    #[inline]
    fn eps() -> Self::Output {
        U8_EPS
    }
}

impl Number for u16 {
    type Output = u16;

    #[inline]
    fn zero() -> Self::Output {
        0
    }

    #[inline]
    fn one() -> Self::Output {
        1
    }

    #[inline]
    fn max() -> Self::Output {
        u16::MAX
    }

    #[inline]
    fn _sqrt(self) -> Self::Output {
        panic!("Oops! There's no sqrt() for u16")
    }

    #[inline]
    fn _acos(self) -> Self::Output {
        panic!("Oops! There's no acos() for u16")
    }

    #[inline]
    fn from_f32(v: f32) -> Self::Output {
        v as Self::Output
    }

    #[inline]
    fn from_f64(v: f64) -> Self::Output {
        v as Self::Output
    }

    #[inline]
    fn eps() -> Self::Output {
        U16_EPS
    }
}

impl Number for f32 {
    type Output = f32;

    #[inline]
    fn zero() -> Self::Output {
        0.0
    }

    #[inline]
    fn one() -> Self::Output {
        1.0
    }

    #[inline]
    fn max() -> Self::Output {
        f32::MAX
    }

    #[inline]
    fn _sqrt(self) -> Self::Output {
        self.sqrt()
    }

    #[inline]
    fn _acos(self) -> Self::Output {
        self.acos()
    }

    #[inline]
    fn from_f32(v: f32) -> Self::Output {
        v as Self::Output
    }

    #[inline]
    fn from_f64(v: f64) -> Self::Output {
        v as Self::Output
    }

    #[inline]
    fn eps() -> Self::Output {
        F32_EPS
    }
}

/// A shallow wrapper of [`Vector`].
#[derive(Debug, PartialEq, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(transparent)]
pub struct UnitVector<T, const D: usize>(Vector<T, D>);

impl UnitVector<f32, 3> {
    pub fn new(v: Vector<f32, 3>) -> Self {
        assert!(v.is_unit());
        unsafe { Self::new_unchecked(v) }
    }

    pub const unsafe fn new_unchecked(v: Vector<f32, 3>) -> Self {
        Self(v)
    }

    pub const fn into_inner(self) -> Vector<f32, 3> {
        self.0
    }
}

impl Deref for UnitVector<f32, 3> {
    type Target = Vector<f32, 3>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl UnitVector<f32, 4> {
    pub fn new(v: Vector<f32, 4>) -> Self {
        assert!(v.is_unit());
        unsafe { Self::new_unchecked(v) }
    }

    pub const unsafe fn new_unchecked(v: Vector<f32, 4>) -> Self {
        Self(v)
    }

    pub const fn into_inner(self) -> Vector<f32, 4> {
        self.0
    }
}

impl Deref for UnitVector<f32, 4> {
    type Target = Vector<f32, 4>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, PartialEq, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(transparent)]
pub struct Vector<T, const D: usize>(pub [T; D]);

macro_rules! strip_first_op {
    (+ $($tail:tt)+) => {
        $($tail)+
    };
    (- $($tail:tt)+) => {
        $($tail)+
    };
    (* $($tail:tt)+) => {
        $($tail)+
    };
    (/ $($tail:tt)+) => {
        $($tail)+
    };
}

macro_rules! cross_product_helper {
    (1, $($tail:tt)*) => {
        panic!("No cross_product() for Vector<T, 1>")
    };
    (2, $($tail:tt)*) => {
        panic!("No cross_product() for Vector<T, 2>")
    };
    (3, $x:expr, $y:expr, $z:expr, $w:expr) => {
        [$x, $y, $z]
    };
    (4, $x:expr, $y:expr, $z:expr, $w:expr) => {
        [$x, $y, $z, $w]
    };
}

macro_rules! impl_vector {
    ($d:tt, $({$field:ident:$index:tt}),+) => {
        impl<T> Vector<T, $d>
        where
            T: Copy
                + std::ops::Add<Output = T>
                + std::ops::Sub<Output = T>
                + std::ops::Mul<Output = T>
                + std::ops::Div<Output = T>
                + std::ops::DivAssign
                + std::ops::AddAssign
                + Number<Output = T>
                + PartialEq
                + PartialOrd
                + 'static
        {
            #[inline]
            pub const fn new($($field: T),+) -> Self {
                Self([$($field),+])
            }

            /// Creates a vector with zeros.
            #[inline]
            pub fn zeros() -> Self {
                T::zero().into()
            }

            /// Creates a vector with ones.
            #[inline]
            pub fn ones() -> Self {
                T::one().into()
            }

            #[inline]
            pub fn from_arr_f32<const L: usize>(arr: [f32; L], default: [f32; 4]) -> Self {
                Self([$(
                    if $index < L { T::from_f32(arr[$index]) }
                    else { T::from_f32(default[$index]) }
                ),+])
            }

            #[inline]
            pub fn from_vec_f32<const L: usize>(v: Vector<f32, L>, default: [f32; 4]) -> Self {
                Self([$(
                    if $index < L { T::from_f32(v.0[$index]) }
                    else { T::from_f32(default[$index]) }
                ),+])
            }

            #[inline]
            pub fn get_type() -> Option<&'static str> {
                use std::any::TypeId;
                match TypeId::of::<T>() {
                    x if x == TypeId::of::<u8>() => Some("u8"),
                    x if x == TypeId::of::<u16>() => Some("u16"),
                    x if x == TypeId::of::<f32>() => Some("f32"),
                    _ => None
                }
            }

            #[inline]
            pub const fn size() -> usize {
                std::mem::size_of::<Self>()
            }

            #[inline]
            pub const fn get_size(&self) -> usize {
                Self::size()
            }

            #[inline]
            pub const fn dim() -> usize {
                $d
            }

            #[inline]
            pub const fn get_dim(&self) -> usize {
                Self::dim()
            }

            #[inline]
            pub fn get_max() -> T {
                T::max()
            }

            $(
                /// Getter
                #[inline]
                pub const fn $field(&self) -> T {
                    self.0[$index]
                }

                paste::item! {
                    /// Setter
                    #[inline]
                    pub fn [<set_ $field>](&mut self, v: T) {
                        self.0[$index] = v;
                    }

                    #[inline]
                    pub fn [<add_ $field>](&mut self, v: T) {
                        self.0[$index] += v;
                    }
                }
            )+

            /// Setter
            #[inline]
            pub fn set(&mut self, $($field: T),+) {
                $(
                    self.0[$index] = $field;
                )+
            }

            #[inline]
            pub fn iter(&self) -> core::slice::Iter<T> {
                self.0.iter()
            }

            #[inline]
            pub fn norm_l2(&self) -> T {
                strip_first_op!(
                    $(+ self.0[$index] * self.0[$index])+
                )._sqrt()
            }

            #[inline]
            pub fn is_unit(&self) -> bool {
                self.norm_l2() < T::eps()
            }

            #[inline]
            pub fn normalize(&mut self) {
                let norm = self.norm_l2();
                match norm != T::zero() {
                    true => {$(self.0[$index] /= norm);+}
                    false => (),
                }
            }

            #[inline]
            #[must_use]
            pub fn into_unit(self) -> Self {
                let norm = self.norm_l2();
                match norm != T::zero() {
                    true => {Self::new($(self.0[$index] / norm),+)},
                    false => self,
                }
            }

            #[inline]
            pub fn dist(self, rhs: Self) -> T {
                (self - rhs).norm_l2()
            }

            #[inline]
            pub fn dot(self, rhs: Self) -> T {
                strip_first_op!(
                    $(+ self.0[$index] * rhs.0[$index])+
                )
            }

            #[inline]
            #[allow(unused)]
            pub fn cross(self, rhs: Self) -> Self {
                Self(
                    cross_product_helper!($d,
                        self.0[1] * rhs.0[2] - self.0[2] * rhs.0[1],
                        self.0[2] * rhs.0[0] - self.0[0] * rhs.0[2],
                        self.0[0] * rhs.0[1] - self.0[1] * rhs.0[0],
                        self.0[3]
                    )
                )
            }

            #[inline]
            pub fn normal(self, rhs: Self) -> Self {
                self.cross(rhs).into_unit()
            }

            #[inline]
            pub fn angle(self, rhs: Self) -> T {
                (self.dot(rhs) / self.norm_l2() / rhs.norm_l2())._acos()
            }

            #[inline]
            pub fn random(max: f64, range: std::ops::Range<usize>) -> Self {
                let gen = || T::from_f64(js_sys::Math::random() * max);
                let mut v = Self::zeros();
                range.for_each(|i| v.0[i] = gen());
                v
            }
        }

        impl<T: Copy> From<T> for Vector<T, $d> {
            #[inline]
            fn from(value: T) -> Self {
                Self([value; $d])
            }
        }

        impl<T> From<[T; $d]> for Vector<T, $d> {
            #[inline]
            fn from(value: [T; $d]) -> Self {
                Self(value)
            }
        }

        macro_rules! impl_op {
            ($trait:ident, $fname:ident, $op:tt) => {
                impl<T> std::ops::$trait for Vector<T, $d>
                where
                    T: std::ops::$trait<Output = T>
                        + Copy
                {
                    type Output = Self;

                    #[inline]
                    #[must_use]
                    fn $fname(self, rhs: Self) -> Self {
                        Self([
                            $(self.0[$index] $op rhs.0[$index]),+
                        ])
                    }
                }

                impl<'a, 'b, T> std::ops::$trait<&'b Vector<T, $d>> for &'a Vector<T, $d>
                where
                    T: std::ops::$trait<Output = T>
                        + Copy
                {
                    type Output = Vector<T, $d>;

                    #[inline]
                    #[must_use]
                    fn $fname(self, rhs: &'b Vector<T, $d>) -> Self::Output {
                        Vector::<T, $d>([
                            $(self.0[$index] $op rhs.0[$index]),+
                        ])
                    }
                }

                impl<T> std::ops::$trait<T> for Vector<T, $d>
                where
                    T: std::ops::$trait<Output = T>
                        + Copy
                {
                    type Output = Self;

                    #[inline]
                    #[must_use]
                    fn $fname(self, rhs: T) -> Self {
                        Self([
                            $(self.0[$index] $op rhs),+
                        ])
                    }
                }
            }
        }

        macro_rules! impl_op_assign {
            ($trait:ident, $fname:ident, $op:tt) => {
                impl<T> std::ops::$trait for Vector<T, $d>
                where
                    T: std::ops::$trait
                        + Copy
                {
                    #[inline]
                    fn $fname(&mut self, rhs: Self) {
                        $(self.0[$index] $op rhs.0[$index]);+
                    }
                }

                impl<'a, T> std::ops::$trait<&'a Vector<T, $d>> for Vector<T, $d>
                where
                    T: std::ops::$trait
                        + Copy
                {
                    #[inline]
                    fn $fname(&mut self, rhs: &'a Vector<T, $d>) {
                        $(self.0[$index] $op rhs.0[$index]);+
                    }
                }

                impl<T> std::ops::$trait<T> for Vector<T, $d>
                where
                    T: std::ops::$trait
                        + Copy
                {
                    #[inline]
                    fn $fname(&mut self, rhs: T) {
                        $(self.0[$index] $op rhs);+
                    }
                }
            }
        }

        impl_op!(Add, add, +);
        impl_op!(Sub, sub, -);
        impl_op!(Mul, mul, *);
        impl_op!(Div, div, /);
        impl_op_assign!(AddAssign, add_assign, +=);
        impl_op_assign!(SubAssign, sub_assign, -=);
        impl_op_assign!(MulAssign, mul_assign, *=);
        impl_op_assign!(DivAssign, div_assign, /=);
    }
}

impl_vector!(1, {x: 0});
impl_vector!(2, {x: 0}, {y: 1});
impl_vector!(3, {x: 0}, {y: 1}, {z: 2});
impl_vector!(4, {x: 0}, {y: 1}, {z: 2}, {w: 3});

/// Common normalized vector conversion.
/// Converts from [u8; 3] to [f32; 3].
impl From<Vector<u8, 3>> for Vector<f32, 3> {
    /// 0..255 -> 0..1
    fn from(value: Vector<u8, 3>) -> Self {
        Self::new(
            value.x() as f32 / 255_f32,
            value.y() as f32 / 255_f32,
            value.z() as f32 / 255_f32,
        )
    }
}

/// Common normalized vector conversion.
/// Converts from [u8; 3] to [f32; 4], the last element is filled with 1.0.
impl From<Vector<u8, 3>> for Vector<f32, 4> {
    /// 0..255 -> 0..1
    fn from(value: Vector<u8, 3>) -> Self {
        Self::new(
            value.x() as f32 / 255_f32,
            value.y() as f32 / 255_f32,
            value.z() as f32 / 255_f32,
            1.0,
        )
    }
}

/// Unit length quaternion.
/// Inner vector is used to represent quaternion, which's x, y, and z are used for
/// quaternion's vector part, known as i, j, and k, and w is used for the scalar part.
#[derive(Debug, Clone)]
pub struct Quaternion(UnitVector<f32, 4>);

impl Quaternion {
    pub const fn unit() -> Self {
        let v = Vector::<f32, 4>::new(0.0, 0.0, 0.0, 1.0);
        // Safety: Infallible.
        unsafe { Self(UnitVector::<f32, 4>::new_unchecked(v)) }
    }

    /// Creates quaternion from 4D unit vector.
    /// Make sure that the vector's x, y, and z are the vector and w is the scalar of quaternion.
    ///
    /// # Panics
    ///
    /// In debug mode only, panics if the vector is not a unit vector.
    pub fn new(v: UnitVector<f32, 4>) -> Self {
        Self(v)
    }

    /// Creates quaternion from axis and angle in radian.
    ///
    /// # Panics
    ///
    /// In debug mode only, panics if the vector is not a unit vector.
    pub fn from_axis(axis: UnitVector<f32, 3>, angle: f32) -> Self {
        let mut axis = axis.into_inner();
        let (sin, cos) = (angle * 0.5).sin_cos();
        axis *= sin;
        let v = Vector::<f32, 4>::new(axis.x(), axis.y(), axis.z(), cos);
        // Safety: v is unit vector.
        unsafe { Self(UnitVector::<f32, 4>::new_unchecked(v)) }
    }

    /// Creates quaternion from rotation `angle` radians around x-axis.
    pub fn from_rotation_x(angle: f32) -> Self {
        let (sin, cos) = (angle * 0.5).sin_cos();
        let v = Vector::<f32, 4>::new(sin, 0.0, 0.0, cos);
        // Safety: v is unit vector.
        unsafe { Self(UnitVector::<f32, 4>::new_unchecked(v)) }
    }

    /// Creates quaternion from rotation `angle` radians around y-axis.
    pub fn from_rotation_y(angle: f32) -> Self {
        let (sin, cos) = (angle * 0.5).sin_cos();
        let v = Vector::<f32, 4>::new(0.0, sin, 0.0, cos);
        // Safety: v is unit vector.
        unsafe { Self(UnitVector::<f32, 4>::new_unchecked(v)) }
    }

    /// Creates quaternion from rotation `angle` radians around z-axis.
    pub fn from_rotation_z(angle: f32) -> Self {
        let (sin, cos) = (angle * 0.5).sin_cos();
        let v = Vector::<f32, 4>::new(0.0, 0.0, sin, cos);
        // Safety: v is unit vector.
        unsafe { Self(UnitVector::<f32, 4>::new_unchecked(v)) }
    }
}

impl Deref for Quaternion {
    type Target = UnitVector<f32, 4>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_bindgen_test::*;

    const EPS: f32 = 1e-6;

    type V1u8 = Vector<u8, 1>;
    type V2u8 = Vector<u8, 2>;
    type V3u8 = Vector<u8, 3>;
    type V4u8 = Vector<u8, 4>;
    type V1f32 = Vector<f32, 1>;
    type V2f32 = Vector<f32, 2>;
    type V3f32 = Vector<f32, 3>;
    type V4f32 = Vector<f32, 4>;

    #[wasm_bindgen_test]
    fn test_new() {
        assert_eq!(Vector([1_u8]), V1u8::new(1));
        assert_eq!(Vector([1_u8, 2]), V2u8::new(1, 2));
        assert_eq!(Vector([1_u8, 2, 3]), V3u8::new(1, 2, 3));
        assert_eq!(Vector([1_u8, 2, 3, 4]), V4u8::new(1, 2, 3, 4));
        assert_eq!(Vector([0.1_f32]), V1f32::new(0.1));
        assert_eq!(Vector([0.1_f32, 0.2]), V2f32::new(0.1, 0.2));
        assert_eq!(Vector([0.1_f32, 0.2, 0.3]), V3f32::new(0.1, 0.2, 0.3));
        assert_eq!(
            Vector([0.1_f32, 0.2, 0.3, 0.4]),
            V4f32::new(0.1, 0.2, 0.3, 0.4)
        );
    }

    #[wasm_bindgen_test]
    fn test_from_vec_f32() {
        let default = [0.1, 0.2, 0.3, 0.4];
        assert_eq!(
            V4f32::new(1.0, 0.2, 0.3, 0.4),
            V4f32::from_vec_f32(Vector([1.0]), default)
        );
        assert_eq!(
            V4f32::new(1.0, 2.0, 0.3, 0.4),
            V4f32::from_vec_f32(Vector([1.0, 2.0]), default)
        );
        assert_eq!(
            V4f32::new(1.0, 2.0, 3.0, 0.4),
            V4f32::from_vec_f32(Vector([1.0, 2.0, 3.0]), default)
        );
        assert_eq!(
            V4f32::new(1.0, 2.0, 3.0, 4.0),
            V4f32::from_vec_f32(Vector([1.0, 2.0, 3.0, 4.0]), default)
        );
    }

    #[wasm_bindgen_test]
    fn test_getter() {
        let v = V4u8::new(1, 2, 3, 4);
        assert_eq!(1, v.x());
        assert_eq!(2, v.y());
        assert_eq!(3, v.z());
        assert_eq!(4, v.w());
    }

    #[wasm_bindgen_test]
    fn test_setter() {
        let mut v = V4u8::new(1, 2, 3, 4);
        v.set_x(10);
        assert_eq!(V4u8::new(10, 2, 3, 4), v);
        v.set_y(20);
        assert_eq!(V4u8::new(10, 20, 3, 4), v);
        v.set_z(30);
        assert_eq!(V4u8::new(10, 20, 30, 4), v);
        v.set_w(40);
        assert_eq!(V4u8::new(10, 20, 30, 40), v);
        v.set(11, 12, 13, 14);
        assert_eq!(V4u8::new(11, 12, 13, 14), v);
    }

    #[wasm_bindgen_test]
    fn test_iter() {
        let v = V4u8::new(1, 2, 3, 4);
        let mut iter = v.iter();
        assert_eq!(Some(&1), iter.next());
        assert_eq!(Some(&2), iter.next());
        assert_eq!(Some(&3), iter.next());
        assert_eq!(Some(&4), iter.next());
    }

    #[wasm_bindgen_test]
    #[should_panic]
    fn test_norm_l2_with_u8_panic() {
        V4u8::new(1, 2, 3, 4).norm_l2();
    }

    #[wasm_bindgen_test]
    fn test_normalize() {
        let (x, y, z) = (1.0, 2.0, 3.0);
        let mut v = V3f32::new(x, y, z);
        let norm = v.norm_l2();
        let unit_v = v.into_unit();
        v.normalize();
        assert!((v.norm_l2() - 1.0).abs() < EPS);
        assert_eq!(V3f32::new(x / norm, y / norm, z / norm), v);
        assert_eq!(v, unit_v);
    }

    #[wasm_bindgen_test]
    fn test_ops() {
        let a = V4f32::new(1.0, 2.0, 3.0, 4.0);
        let b = V4f32::new(1.2, 3.4, 5.6, 7.8);
        assert_eq!(
            V4f32::new(a.x() + b.x(), a.y() + b.y(), a.z() + b.z(), a.w() + b.w()),
            a + b
        );
        assert_eq!(
            V4f32::new(a.x() - b.x(), a.y() - b.y(), a.z() - b.z(), a.w() - b.w()),
            a - b
        );
        assert_eq!(
            V4f32::new(a.x() * b.x(), a.y() * b.y(), a.z() * b.z(), a.w() * b.w()),
            a * b
        );
        assert_eq!(
            V4f32::new(a.x() / b.x(), a.y() / b.y(), a.z() / b.z(), a.w() / b.w()),
            a / b
        );
        let (mut x, y) = (a.clone(), b.clone());
        x += y;
        assert_eq!(a + b, x);
        let (mut x, y) = (a.clone(), b.clone());
        x -= y;
        assert_eq!(a - b, x);
        let (mut x, y) = (a.clone(), b.clone());
        x *= y;
        assert_eq!(a * b, x);
        let (mut x, y) = (a.clone(), b.clone());
        x /= y;
        assert_eq!(a / b, x);
    }

    #[wasm_bindgen_test]
    fn test_type_and_dimension_getter() {
        assert_eq!("u8", V1u8::get_type().unwrap());
        assert_eq!("f32", V1f32::get_type().unwrap());
        assert_eq!(1, V1f32::dim());
        assert_eq!(2, V2f32::dim());
        assert_eq!(3, V3f32::dim());
        assert_eq!(4, V4f32::dim());
    }
}
