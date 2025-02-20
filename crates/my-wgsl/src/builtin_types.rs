use super::builtin_functions::WgslDot;
use std::{
    mem,
    ops::{Add, Deref, DerefMut, Div, Mul, Neg, Rem, Sub},
};

#[rustfmt::skip]
macro_rules! impl_deref_mut {
    ($outer:ty, $inner:ty) => {
        impl Deref for $outer {
            type Target = $inner;
            fn deref(&self) -> &Self::Target { &self.0 }
        }
        impl DerefMut for $outer {
            fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
        }
    };
}

// +, -, *, /, % between two vectors.
macro_rules! impl_vec_component_wise_arithmetic {
    ($id:ty, $($i:expr),*) => {
        impl Add for $id {
            type Output = Self;
            fn add(self, rhs: Self) -> Self::Output {
                Self::new( $( self[$i] + rhs[$i] ),* )
            }
        }
        impl Sub for $id {
            type Output = Self;
            fn sub(self, rhs: Self) -> Self::Output {
                Self::new( $( self[$i] - rhs[$i] ),* )
            }
        }
        impl Mul for $id {
            type Output = Self;
            fn mul(self, rhs: Self) -> Self::Output {
                Self::new( $( self[$i] * rhs[$i] ),* )
            }
        }
        impl Div for $id {
            type Output = Self;
            fn div(self, rhs: Self) -> Self::Output {
                Self::new( $( self[$i] / rhs[$i] ),* )
            }
        }
        impl Rem for $id {
            type Output = Self;
            fn rem(self, rhs: Self) -> Self::Output {
                Self::new( $( self[$i] % rhs[$i] ),* )
            }
        }
    };
}

// -vector
macro_rules! impl_vec_component_wise_negation {
    ($id:ty, $($i:expr),*) => {
        impl Neg for $id {
            type Output = Self;
            fn neg(self) -> Self::Output {
                Self::new( $( -self[$i] ),* )
            }
        }
    };
}

// +, -, *, /, % between a vector and a scalar.
#[rustfmt::skip]
macro_rules! impl_vec_scalar_arithmetic {
    ($v:ident, $s:ty) => {
        impl Add<$s> for $v {
            type Output = Self;
            fn add(self, s: $s) -> Self::Output { self + Self::splat(s) }
        }
        impl Sub<$s> for $v {
            type Output = Self;
            fn sub(self, s: $s) -> Self::Output { self - Self::splat(s) }
        }
        impl Mul<$s> for $v {
            type Output = Self;
            fn mul(self, s: $s) -> Self::Output { self * Self::splat(s) }
        }
        impl Div<$s> for $v {
            type Output = Self;
            fn div(self, s: $s) -> Self::Output { self / Self::splat(s) }
        }
        impl Rem<$s> for $v {
            type Output = Self;
            fn rem(self, s: $s) -> Self::Output { self % Self::splat(s) }
        }
        impl Add<$v> for $s {
            type Output = $v;
            fn add(self, rhs: $v) -> Self::Output { $v::splat(self) + rhs }
        }
        impl Sub<$v> for $s {
            type Output = $v;
            fn sub(self, rhs: $v) -> Self::Output { $v::splat(self) - rhs }
        }
        impl Mul<$v> for $s {
            type Output = $v;
            fn mul(self, rhs: $v) -> Self::Output { $v::splat(self) * rhs }
        }
        impl Div<$v> for $s {
            type Output = $v;
            fn div(self, rhs: $v) -> Self::Output { $v::splat(self) / rhs }
        }
        impl Rem<$v> for $s {
            type Output = $v;
            fn rem(self, rhs: $v) -> Self::Output { $v::splat(self) % rhs }
        }
    };
}

// +, - between two matrices.
macro_rules! impl_mat_component_wise_arithmetic {
    ($id:ty, $($i:expr),*) => {
        impl Add for $id {
            type Output = Self;
            fn add(self, rhs: Self) -> Self::Output {
                Self::new( $( self[$i] + rhs[$i] ),* )
            }
        }
        impl Sub for $id {
            type Output = Self;
            fn sub(self, rhs: Self) -> Self::Output {
                Self::new( $( self[$i] - rhs[$i] ),* )
            }
        }
    };
}

// * between two matrices.
macro_rules! impl_mat_mul_mat {
    (C = $lhs_c:expr, R = $lhs_r:expr, RhsCols = $head:expr, $($tail:expr),*) => {
        impl_mat_mul_mat!(C = $lhs_c, R = $lhs_r, RhsCols = $head);
        impl_mat_mul_mat!(C = $lhs_c, R = $lhs_r, RhsCols = $($tail),*);
    };
    (C = $lhs_c:expr, R = $lhs_r:expr, RhsCols = $rhs_c:expr) => {const _: () = {
        paste::paste! {
            type Lhs = [<Mat $lhs_c x $lhs_r f>];
            type Rhs = [<Mat $rhs_c x $lhs_c f>];
            type Out = [<Mat $rhs_c x $lhs_r f>];

            impl Mul<Rhs> for Lhs {
                type Output = Out;
                fn mul(self, rhs: Rhs) -> Self::Output {
                    const NC: usize = $rhs_c;
                    const NR: usize = $lhs_r;

                    let mut out = Out::ZERO;
                    for c in 0..NC {
                        for r in 0..NR {
                            out[c][r] = self.dot(r, *rhs[c]);
                        }
                    }
                    out
                }
            }
        }
    };};
}

// * between a matrix and a scalar.
#[rustfmt::skip]
macro_rules! impl_mat_mul_scalar {
    ($m:ident, $s:ty, $($i:expr),*) => {
        impl Mul<$s> for $m {
            type Output = Self;
            fn mul(self, rhs: $s) -> Self::Output { 
                Self::new( $( self[$i] * rhs ),* )
            }
        }
        impl Mul<$m> for $s {
            type Output = $m;
            fn mul(self, rhs: $m) -> Self::Output {
                $m::new( $( self * rhs[$i] ),* )
            }
        }
    };
}

macro_rules! impl_vec_mul_mat {
    ($vec:ty, $mat:ty, $out:ident, $($i:expr),*) => {
        impl Mul<$mat> for $vec {
            type Output = $out;
            fn mul(self, rhs: $mat) -> Self::Output {
                $out::new(
                    $( self.dot( *rhs[$i] ) ),*
                )
            }
        }
    };
}

macro_rules! impl_mat_mul_vec {
    (C = $nc:expr, R = $nr:expr, Rows = $($i:expr),*) => {const _: () = {
        paste::paste! {
            type Lhs = [<Mat $nc x $nr f>];
            type Rhs = [<Vec $nc f>];
            type Out = [<Vec $nr f>];

            impl Mul<Rhs> for Lhs {
                type Output = Out;
                fn mul(self, rhs: Rhs) -> Self::Output {
                    Out::new(
                        $( self.dot($i, rhs) ),*
                    )
                }
            }
        }
    };};
}

macro_rules! impl_from_arr_for_mat {
    (C = $c:expr, R = $r:expr, Cols = $($i:expr),*) => { const _: () = {
        paste::paste! {
            type M = [<Mat $c x $r f>];
            type V = [<Vec $r f>];
            const L: usize = $c * $r;

            impl From<[f32; L]> for M {
                fn from(value: [f32; L]) -> Self {
                    let s: [[f32; $r]; $c] = unsafe { mem::transmute(value) };
                    Self::new(
                        $( V::from(s[$i]) ),*
                    )
                }
            }
        }
    };};
}

// === Bool ===

// Declares a struct defined by the macro for consistent layout.
// pub struct Bool ( bool );
my_wgsl_macros::decl_bool!();
impl_deref_mut!(Bool, bool);

impl Bool {
    pub const fn new(value: bool) -> Self {
        Self(value)
    }
}

// === Vec2i ===

// Declares a struct defined by the macro for consistent layout.
// pub struct Vec2i ( [i32; 2] );
my_wgsl_macros::decl_vec2i!();
impl_deref_mut!(Vec2i, [i32; 2]);
impl_vec_component_wise_arithmetic!(Vec2i, 0, 1);
impl_vec_component_wise_negation!(Vec2i, 0, 1);
impl_vec_scalar_arithmetic!(Vec2i, i32);

impl Vec2i {
    pub const ZERO: Self = Self::new(0, 0);

    pub const fn new(x: i32, y: i32) -> Self {
        Self([x, y])
    }

    pub const fn splat(v: i32) -> Self {
        Self::new(v, v)
    }
}

impl From<[i32; 2]> for Vec2i {
    fn from(value: [i32; 2]) -> Self {
        Self(value)
    }
}

// === Vec3i ===

// Declares a struct defined by the macro for consistent layout.
// pub struct Vec3i ( [i32; 3] );
my_wgsl_macros::decl_vec3i!();
impl_deref_mut!(Vec3i, [i32; 3]);
impl_vec_component_wise_arithmetic!(Vec3i, 0, 1, 2);
impl_vec_component_wise_negation!(Vec3i, 0, 1, 2);
impl_vec_scalar_arithmetic!(Vec3i, i32);

impl Vec3i {
    pub const ZERO: Self = Self::new(0, 0, 0);

    pub const fn new(x: i32, y: i32, z: i32) -> Self {
        Self([x, y, z])
    }

    pub const fn splat(v: i32) -> Self {
        Self::new(v, v, v)
    }
}

impl From<[i32; 3]> for Vec3i {
    fn from(value: [i32; 3]) -> Self {
        Self(value)
    }
}

// === Vec4i ===

// Declares a struct defined by the macro for consistent layout.
// pub struct Vec4i ( [i32; 4] );
my_wgsl_macros::decl_vec4i!();
impl_deref_mut!(Vec4i, [i32; 4]);
impl_vec_component_wise_arithmetic!(Vec4i, 0, 1, 2, 3);
impl_vec_component_wise_negation!(Vec4i, 0, 1, 2, 3);
impl_vec_scalar_arithmetic!(Vec4i, i32);

impl Vec4i {
    pub const ZERO: Self = Self::new(0, 0, 0, 0);

    pub const fn new(x: i32, y: i32, z: i32, w: i32) -> Self {
        Self([x, y, z, w])
    }

    pub const fn splat(v: i32) -> Self {
        Self::new(v, v, v, v)
    }
}

impl From<[i32; 4]> for Vec4i {
    fn from(value: [i32; 4]) -> Self {
        Self(value)
    }
}

// === Vec2u ===

// Declares a struct defined by the macro for consistent layout.
// pub struct Vec2u ( [u32; 2] );
my_wgsl_macros::decl_vec2u!();
impl_deref_mut!(Vec2u, [u32; 2]);
impl_vec_component_wise_arithmetic!(Vec2u, 0, 1);
impl_vec_scalar_arithmetic!(Vec2u, u32);

impl Vec2u {
    pub const ZERO: Self = Self::new(0, 0);

    pub const fn new(x: u32, y: u32) -> Self {
        Self([x, y])
    }

    pub const fn splat(v: u32) -> Self {
        Self::new(v, v)
    }
}

impl From<[u32; 2]> for Vec2u {
    fn from(value: [u32; 2]) -> Self {
        Self(value)
    }
}

// === Vec3u ===

// Declares a struct defined by the macro for consistent layout.
// pub struct Vec3u ( [u32; 3] );
my_wgsl_macros::decl_vec3u!();
impl_deref_mut!(Vec3u, [u32; 3]);
impl_vec_component_wise_arithmetic!(Vec3u, 0, 1, 2);
impl_vec_scalar_arithmetic!(Vec3u, u32);

impl Vec3u {
    pub const ZERO: Self = Self::new(0, 0, 0);

    pub const fn new(x: u32, y: u32, z: u32) -> Self {
        Self([x, y, z])
    }

    pub const fn splat(v: u32) -> Self {
        Self::new(v, v, v)
    }
}

impl From<[u32; 3]> for Vec3u {
    fn from(value: [u32; 3]) -> Self {
        Self(value)
    }
}

// === Vec4u ===

// Declares a struct defined by the macro for consistent layout.
// pub struct Vec4u ( [u32; 4] );
my_wgsl_macros::decl_vec4u!();
impl_deref_mut!(Vec4u, [u32; 4]);
impl_vec_component_wise_arithmetic!(Vec4u, 0, 1, 2, 3);
impl_vec_scalar_arithmetic!(Vec4u, u32);

impl Vec4u {
    pub const ZERO: Self = Self::new(0, 0, 0, 0);

    pub const fn new(x: u32, y: u32, z: u32, w: u32) -> Self {
        Self([x, y, z, w])
    }

    pub const fn splat(v: u32) -> Self {
        Self::new(v, v, v, v)
    }
}

impl From<[u32; 4]> for Vec4u {
    fn from(value: [u32; 4]) -> Self {
        Self(value)
    }
}

// === Vec2f ===

// Declares a struct defined by the macro for consistent layout.
// pub struct Vec2f ( [f32; 2] );
my_wgsl_macros::decl_vec2f!();
impl_deref_mut!(Vec2f, [f32; 2]);
impl_vec_component_wise_arithmetic!(Vec2f, 0, 1);
impl_vec_component_wise_negation!(Vec2f, 0, 1);
impl_vec_scalar_arithmetic!(Vec2f, f32);
impl_vec_mul_mat!(Vec2f, Mat2x2f, Vec2f, 0, 1);
impl_vec_mul_mat!(Vec2f, Mat3x2f, Vec3f, 0, 1, 2);
impl_vec_mul_mat!(Vec2f, Mat4x2f, Vec4f, 0, 1, 2, 3);

impl Vec2f {
    pub const ZERO: Self = Self::new(0., 0.);

    pub const fn new(x: f32, y: f32) -> Self {
        Self([x, y])
    }

    pub const fn splat(v: f32) -> Self {
        Self::new(v, v)
    }
}

impl From<[f32; 2]> for Vec2f {
    fn from(value: [f32; 2]) -> Self {
        Self(value)
    }
}

// === Vec3f ===

// Declares a struct defined by the macro for consistent layout.
// pub struct Vec3f ( [f32; 3] );
my_wgsl_macros::decl_vec3f!();
impl_deref_mut!(Vec3f, [f32; 3]);
impl_vec_component_wise_arithmetic!(Vec3f, 0, 1, 2);
impl_vec_component_wise_negation!(Vec3f, 0, 1, 2);
impl_vec_scalar_arithmetic!(Vec3f, f32);
impl_vec_mul_mat!(Vec3f, Mat2x3f, Vec2f, 0, 1);
impl_vec_mul_mat!(Vec3f, Mat3x3f, Vec3f, 0, 1, 2);
impl_vec_mul_mat!(Vec3f, Mat4x3f, Vec4f, 0, 1, 2, 3);

impl Vec3f {
    pub const ZERO: Self = Self::new(0., 0., 0.);

    pub const fn new(x: f32, y: f32, z: f32) -> Self {
        Self([x, y, z])
    }

    pub const fn splat(v: f32) -> Self {
        Self::new(v, v, v)
    }
}

impl From<[f32; 3]> for Vec3f {
    fn from(value: [f32; 3]) -> Self {
        Self(value)
    }
}

// === Vec4f ===

// Declares a struct defined by the macro for consistent layout.
// pub struct Vec4f ( [f32; 4] );
my_wgsl_macros::decl_vec4f!();
impl_deref_mut!(Vec4f, [f32; 4]);
impl_vec_component_wise_arithmetic!(Vec4f, 0, 1, 2, 3);
impl_vec_component_wise_negation!(Vec4f, 0, 1, 2, 3);
impl_vec_scalar_arithmetic!(Vec4f, f32);
impl_vec_mul_mat!(Vec4f, Mat2x4f, Vec2f, 0, 1);
impl_vec_mul_mat!(Vec4f, Mat3x4f, Vec3f, 0, 1, 2);
impl_vec_mul_mat!(Vec4f, Mat4x4f, Vec4f, 0, 1, 2, 3);

impl Vec4f {
    pub const ZERO: Self = Self::new(0., 0., 0., 0.);

    pub const fn new(x: f32, y: f32, z: f32, w: f32) -> Self {
        Self([x, y, z, w])
    }

    pub const fn splat(v: f32) -> Self {
        Self::new(v, v, v, v)
    }
}

impl From<[f32; 4]> for Vec4f {
    fn from(value: [f32; 4]) -> Self {
        Self(value)
    }
}

// === WideVec ===

#[rustfmt::skip]
macro_rules! impl_wide_vec_component_wise_arithmetic {
    ($id:ty, $out:ty) => {
        impl Add for $id {
            type Output = $out;
            fn add(self, rhs: Self) -> Self::Output { self.0 + rhs.0 }
        }
        impl Sub for $id {
            type Output = $out;
            fn sub(self, rhs: Self) -> Self::Output { self.0 - rhs.0 }
        }
        impl Mul for $id {
            type Output = $out;
            fn mul(self, rhs: Self) -> Self::Output { self.0 * rhs.0 }
        }
        impl Div for $id {
            type Output = $out;
            fn div(self, rhs: Self) -> Self::Output { self.0 / rhs.0 }
        }
        impl Rem for $id {
            type Output = $out;
            fn rem(self, rhs: Self) -> Self::Output { self.0 % rhs.0 }
        }
    };
}

#[rustfmt::skip]
macro_rules! impl_wide_vec_component_wise_negation {
    ($id:ty, $out:ty) => {
        impl Neg for $id {
            type Output = $out;
            fn neg(self) -> Self::Output { -self.0 }
        }
    };
}

#[rustfmt::skip]
macro_rules! impl_wide_vec_scalar_arithmetic {
    ($id:ty, $v:ty, $s:ty) => {
        impl Add<$s> for $id {
            type Output = $v;
            fn add(self, rhs: $s) -> Self::Output { self.0 + rhs }
        }
        impl Sub<$s> for $id {
            type Output = $v;
            fn sub(self, rhs: $s) -> Self::Output { self.0 - rhs }
        }
        impl Mul<$s> for $id {
            type Output = $v;
            fn mul(self, rhs: $s) -> Self::Output { self.0 * rhs }
        }
        impl Div<$s> for $id {
            type Output = $v;
            fn div(self, rhs: $s) -> Self::Output { self.0 / rhs }
        }
        impl Rem<$s> for $id {
            type Output = $v;
            fn rem(self, rhs: $s) -> Self::Output { self.0 % rhs }
        }
        impl Add<$id> for $s {
            type Output = $v;
            fn add(self, rhs: $id) -> Self::Output { self + rhs.0 }
        }
        impl Sub<$id> for $s {
            type Output = $v;
            fn sub(self, rhs: $id) -> Self::Output { self - rhs.0 }
        }
        impl Mul<$id> for $s {
            type Output = $v;
            fn mul(self, rhs: $id) -> Self::Output { self * rhs.0 }
        }
        impl Div<$id> for $s {
            type Output = $v;
            fn div(self, rhs: $id) -> Self::Output { self / rhs.0 }
        }
        impl Rem<$id> for $s {
            type Output = $v;
            fn rem(self, rhs: $id) -> Self::Output { self % rhs.0 }
        }
    };
}

// === WideVec2i ===

// Declares structs defined by the macro for consistent layout.
// #[repr(align(8))]
// pub struct WideVec2i ( Vec2i );
my_wgsl_macros::decl_wide_vec2i!();
impl_deref_mut!(WideVec2i, Vec2i);
impl_wide_vec_component_wise_arithmetic!(WideVec2i, Vec2i);
impl_wide_vec_component_wise_negation!(WideVec2i, Vec2i);
impl_wide_vec_scalar_arithmetic!(WideVec2i, Vec2i, i32);

impl WideVec2i {
    pub const ZERO: Self = Self::new(0, 0);

    pub const fn new(x: i32, y: i32) -> Self {
        Self(Vec2i::new(x, y))
    }

    pub const fn splat(v: i32) -> Self {
        Self::new(v, v)
    }
}

impl From<[i32; 2]> for WideVec2i {
    fn from(value: [i32; 2]) -> Self {
        Self(Vec2i::from(value))
    }
}
impl From<Vec2i> for WideVec2i {
    fn from(value: Vec2i) -> Self {
        Self(value)
    }
}
impl From<WideVec2i> for Vec2i {
    fn from(value: WideVec2i) -> Self {
        value.0
    }
}

// === WideVec3i ===

// Declares structs defined by the macro for consistent layout.
// #[repr(align(16))]
// pub struct WideVec3i ( Vec3i );
my_wgsl_macros::decl_wide_vec3i!();
impl_deref_mut!(WideVec3i, Vec3i);
impl_wide_vec_component_wise_arithmetic!(WideVec3i, Vec3i);
impl_wide_vec_component_wise_negation!(WideVec3i, Vec3i);
impl_wide_vec_scalar_arithmetic!(WideVec3i, Vec3i, i32);

impl WideVec3i {
    pub const ZERO: Self = Self::new(0, 0, 0);

    pub const fn new(x: i32, y: i32, z: i32) -> Self {
        Self(Vec3i::new(x, y, z))
    }

    pub const fn splat(v: i32) -> Self {
        Self::new(v, v, v)
    }
}

impl From<[i32; 3]> for WideVec3i {
    fn from(value: [i32; 3]) -> Self {
        Self(Vec3i::from(value))
    }
}
impl From<Vec3i> for WideVec3i {
    fn from(value: Vec3i) -> Self {
        Self(value)
    }
}
impl From<WideVec3i> for Vec3i {
    fn from(value: WideVec3i) -> Self {
        value.0
    }
}

// === WideVec4i ===

// Declares structs defined by the macro for consistent layout.
// #[repr(align(16))]
// pub struct WideVec4i ( Vec4i );
my_wgsl_macros::decl_wide_vec4i!();
impl_deref_mut!(WideVec4i, Vec4i);
impl_wide_vec_component_wise_arithmetic!(WideVec4i, Vec4i);
impl_wide_vec_component_wise_negation!(WideVec4i, Vec4i);
impl_wide_vec_scalar_arithmetic!(WideVec4i, Vec4i, i32);

impl WideVec4i {
    pub const ZERO: Self = Self::new(0, 0, 0, 0);

    pub const fn new(x: i32, y: i32, z: i32, w: i32) -> Self {
        Self(Vec4i::new(x, y, z, w))
    }

    pub const fn splat(v: i32) -> Self {
        Self::new(v, v, v, v)
    }
}

impl From<[i32; 4]> for WideVec4i {
    fn from(value: [i32; 4]) -> Self {
        Self(Vec4i::from(value))
    }
}
impl From<Vec4i> for WideVec4i {
    fn from(value: Vec4i) -> Self {
        Self(value)
    }
}
impl From<WideVec4i> for Vec4i {
    fn from(value: WideVec4i) -> Self {
        value.0
    }
}

// === WideVec2u ===

// Declares structs defined by the macro for consistent layout.
// #[repr(align(8))]
// pub struct WideVec2u ( Vec2u );
my_wgsl_macros::decl_wide_vec2u!();
impl_deref_mut!(WideVec2u, Vec2u);
impl_wide_vec_component_wise_arithmetic!(WideVec2u, Vec2u);
impl_wide_vec_scalar_arithmetic!(WideVec2u, Vec2u, u32);

impl WideVec2u {
    pub const ZERO: Self = Self::new(0, 0);

    pub const fn new(x: u32, y: u32) -> Self {
        Self(Vec2u::new(x, y))
    }

    pub const fn splat(v: u32) -> Self {
        Self::new(v, v)
    }
}

impl From<[u32; 2]> for WideVec2u {
    fn from(value: [u32; 2]) -> Self {
        Self(Vec2u::from(value))
    }
}
impl From<Vec2u> for WideVec2u {
    fn from(value: Vec2u) -> Self {
        Self(value)
    }
}
impl From<WideVec2u> for Vec2u {
    fn from(value: WideVec2u) -> Self {
        value.0
    }
}

// === WideVec3u ===

// Declares structs defined by the macro for consistent layout.
// #[repr(align(16))]
// pub struct WideVec3u ( Vec3u );
my_wgsl_macros::decl_wide_vec3u!();
impl_deref_mut!(WideVec3u, Vec3u);
impl_wide_vec_component_wise_arithmetic!(WideVec3u, Vec3u);
impl_wide_vec_scalar_arithmetic!(WideVec3u, Vec3u, u32);

impl WideVec3u {
    pub const ZERO: Self = Self::new(0, 0, 0);

    pub const fn new(x: u32, y: u32, z: u32) -> Self {
        Self(Vec3u::new(x, y, z))
    }

    pub const fn splat(v: u32) -> Self {
        Self::new(v, v, v)
    }
}

impl From<[u32; 3]> for WideVec3u {
    fn from(value: [u32; 3]) -> Self {
        Self(Vec3u::from(value))
    }
}
impl From<Vec3u> for WideVec3u {
    fn from(value: Vec3u) -> Self {
        Self(value)
    }
}
impl From<WideVec3u> for Vec3u {
    fn from(value: WideVec3u) -> Self {
        value.0
    }
}

// === WideVec4u ===

// Declares structs defined by the macro for consistent layout.
// #[repr(align(16))]
// pub struct WideVec4u ( Vec4u );
my_wgsl_macros::decl_wide_vec4u!();
impl_deref_mut!(WideVec4u, Vec4u);
impl_wide_vec_component_wise_arithmetic!(WideVec4u, Vec4u);
impl_wide_vec_scalar_arithmetic!(WideVec4u, Vec4u, u32);

impl WideVec4u {
    pub const ZERO: Self = Self::new(0, 0, 0, 0);

    pub const fn new(x: u32, y: u32, z: u32, w: u32) -> Self {
        Self(Vec4u::new(x, y, z, w))
    }

    pub const fn splat(v: u32) -> Self {
        Self::new(v, v, v, v)
    }
}

impl From<[u32; 4]> for WideVec4u {
    fn from(value: [u32; 4]) -> Self {
        Self(Vec4u::from(value))
    }
}
impl From<Vec4u> for WideVec4u {
    fn from(value: Vec4u) -> Self {
        Self(value)
    }
}
impl From<WideVec4u> for Vec4u {
    fn from(value: WideVec4u) -> Self {
        value.0
    }
}

// === WideVec2f ===

// Declares structs defined by the macro for consistent layout.
// #[repr(align(8))]
// pub struct WideVec2f ( Vec2f );
my_wgsl_macros::decl_wide_vec2f!();
impl_deref_mut!(WideVec2f, Vec2f);
impl_wide_vec_component_wise_arithmetic!(WideVec2f, Vec2f);
impl_wide_vec_component_wise_negation!(WideVec2f, Vec2f);
impl_wide_vec_scalar_arithmetic!(WideVec2f, Vec2f, f32);

impl WideVec2f {
    pub const ZERO: Self = Self::new(0., 0.);

    pub const fn new(x: f32, y: f32) -> Self {
        Self(Vec2f::new(x, y))
    }

    pub const fn splat(v: f32) -> Self {
        Self::new(v, v)
    }
}

impl From<[f32; 2]> for WideVec2f {
    fn from(value: [f32; 2]) -> Self {
        Self(Vec2f::from(value))
    }
}
impl From<Vec2f> for WideVec2f {
    fn from(value: Vec2f) -> Self {
        Self(value)
    }
}
impl From<WideVec2f> for Vec2f {
    fn from(value: WideVec2f) -> Self {
        value.0
    }
}

// === WideVec3f ===

// Declares structs defined by the macro for consistent layout.
// #[repr(align(16))]
// pub struct WideVec3f ( Vec3f );
my_wgsl_macros::decl_wide_vec3f!();
impl_deref_mut!(WideVec3f, Vec3f);
impl_wide_vec_component_wise_arithmetic!(WideVec3f, Vec3f);
impl_wide_vec_component_wise_negation!(WideVec3f, Vec3f);
impl_wide_vec_scalar_arithmetic!(WideVec3f, Vec3f, f32);

impl WideVec3f {
    pub const ZERO: Self = Self::new(0., 0., 0.);

    pub const fn new(x: f32, y: f32, z: f32) -> Self {
        Self(Vec3f::new(x, y, z))
    }

    pub const fn splat(v: f32) -> Self {
        Self::new(v, v, v)
    }
}

impl From<[f32; 3]> for WideVec3f {
    fn from(value: [f32; 3]) -> Self {
        Self(Vec3f::from(value))
    }
}
impl From<Vec3f> for WideVec3f {
    fn from(value: Vec3f) -> Self {
        Self(value)
    }
}
impl From<WideVec3f> for Vec3f {
    fn from(value: WideVec3f) -> Self {
        value.0
    }
}

// === WideVec4f ===

// Declares structs defined by the macro for consistent layout.
// #[repr(align(16))]
// pub struct WideVec4f ( Vec4f );
my_wgsl_macros::decl_wide_vec4f!();
impl_deref_mut!(WideVec4f, Vec4f);
impl_wide_vec_component_wise_arithmetic!(WideVec4f, Vec4f);
impl_wide_vec_component_wise_negation!(WideVec4f, Vec4f);
impl_wide_vec_scalar_arithmetic!(WideVec4f, Vec4f, f32);

impl WideVec4f {
    pub const ZERO: Self = Self::new(0., 0., 0., 0.);

    pub const fn new(x: f32, y: f32, z: f32, w: f32) -> Self {
        Self(Vec4f::new(x, y, z, w))
    }

    pub const fn splat(v: f32) -> Self {
        Self::new(v, v, v, v)
    }
}

impl From<[f32; 4]> for WideVec4f {
    fn from(value: [f32; 4]) -> Self {
        Self(Vec4f::from(value))
    }
}
impl From<Vec4f> for WideVec4f {
    fn from(value: Vec4f) -> Self {
        Self(value)
    }
}
impl From<WideVec4f> for Vec4f {
    fn from(value: WideVec4f) -> Self {
        value.0
    }
}

// === Mat2x2f ===

// Declares structs defined by the macro for consistent layout.
// pub struct Mat2x2f ( [WideVec2f; 2] )
my_wgsl_macros::decl_mat2x2f!();
impl_deref_mut!(Mat2x2f, [WideVec2f; 2]);
impl_from_arr_for_mat!(C = 2, R = 2, Cols = 0, 1);
impl_mat_component_wise_arithmetic!(Mat2x2f, 0, 1);
impl_mat_mul_scalar!(Mat2x2f, f32, 0, 1);
impl_mat_mul_vec!(C = 2, R = 2, Rows = 0, 1);
impl_mat_mul_mat!(C = 2, R = 2, RhsCols = 2, 3, 4);

impl Mat2x2f {
    pub const ZERO: Self = Self::new(Vec2f::ZERO, Vec2f::ZERO);
    pub const IDENTITY: Self = Self::new(Vec2f::new(1., 0.), Vec2f::new(0., 1.));

    pub const fn new(v1: Vec2f, v2: Vec2f) -> Self {
        Self([WideVec2f(v1), WideVec2f(v2)])
    }

    fn dot(&self, row: usize, rhs: Vec2f) -> f32 {
        self[0][row] * rhs[0] + self[1][row] * rhs[1]
    }
}

// === Mat2x3f ===

// Declares structs defined by the macro for consistent layout.
// pub struct Mat2x3f ( [WideVec3f; 2] )
my_wgsl_macros::decl_mat2x3f!();
impl_deref_mut!(Mat2x3f, [WideVec3f; 2]);
impl_from_arr_for_mat!(C = 2, R = 3, Cols = 0, 1);
impl_mat_component_wise_arithmetic!(Mat2x3f, 0, 1);
impl_mat_mul_scalar!(Mat2x3f, f32, 0, 1);
impl_mat_mul_vec!(C = 2, R = 3, Rows = 0, 1, 2);
impl_mat_mul_mat!(C = 2, R = 3, RhsCols = 2, 3, 4);

impl Mat2x3f {
    pub const ZERO: Self = Self::new(Vec3f::ZERO, Vec3f::ZERO);

    pub const fn new(v1: Vec3f, v2: Vec3f) -> Self {
        Self([WideVec3f(v1), WideVec3f(v2)])
    }

    fn dot(&self, row: usize, rhs: Vec2f) -> f32 {
        self[0][row] * rhs[0] + self[1][row] * rhs[1]
    }
}

// === Mat2x4f ===

// Declares structs defined by the macro for consistent layout.
// pub struct Mat2x4f ( [WideVec4f; 2] )
my_wgsl_macros::decl_mat2x4f!();
impl_deref_mut!(Mat2x4f, [WideVec4f; 2]);
impl_from_arr_for_mat!(C = 2, R = 4, Cols = 0, 1);
impl_mat_component_wise_arithmetic!(Mat2x4f, 0, 1);
impl_mat_mul_scalar!(Mat2x4f, f32, 0, 1);
impl_mat_mul_vec!(C = 2, R = 4, Rows = 0, 1, 2, 3);
impl_mat_mul_mat!(C = 2, R = 4, RhsCols = 2, 3, 4);

impl Mat2x4f {
    pub const ZERO: Self = Self::new(Vec4f::ZERO, Vec4f::ZERO);

    pub const fn new(v1: Vec4f, v2: Vec4f) -> Self {
        Self([WideVec4f(v1), WideVec4f(v2)])
    }

    fn dot(&self, row: usize, rhs: Vec2f) -> f32 {
        self[0][row] * rhs[0] + self[1][row] * rhs[1]
    }
}

// === Mat3x2f ===

// Declares structs defined by the macro for consistent layout.
// pub struct Mat3x2f ( [WideVec2f; 3] )
my_wgsl_macros::decl_mat3x2f!();
impl_deref_mut!(Mat3x2f, [WideVec2f; 3]);
impl_from_arr_for_mat!(C = 3, R = 2, Cols = 0, 1, 2);
impl_mat_component_wise_arithmetic!(Mat3x2f, 0, 1, 2);
impl_mat_mul_scalar!(Mat3x2f, f32, 0, 1, 2);
impl_mat_mul_vec!(C = 3, R = 2, Rows = 0, 1);
impl_mat_mul_mat!(C = 3, R = 2, RhsCols = 2, 3, 4);

impl Mat3x2f {
    pub const ZERO: Self = Self::new(Vec2f::ZERO, Vec2f::ZERO, Vec2f::ZERO);

    pub const fn new(v1: Vec2f, v2: Vec2f, v3: Vec2f) -> Self {
        Self([WideVec2f(v1), WideVec2f(v2), WideVec2f(v3)])
    }

    fn dot(&self, row: usize, rhs: Vec3f) -> f32 {
        self[0][row] * rhs[0] + self[1][row] * rhs[1] + self[2][row] * rhs[2]
    }
}

// === Mat3x3f ===

// Declares structs defined by the macro for consistent layout.
// pub struct Mat3x3f ( [WideVec3f; 3] )
my_wgsl_macros::decl_mat3x3f!();
impl_deref_mut!(Mat3x3f, [WideVec3f; 3]);
impl_from_arr_for_mat!(C = 3, R = 3, Cols = 0, 1, 2);
impl_mat_component_wise_arithmetic!(Mat3x3f, 0, 1, 2);
impl_mat_mul_scalar!(Mat3x3f, f32, 0, 1, 2);
impl_mat_mul_vec!(C = 3, R = 3, Rows = 0, 1, 2);
impl_mat_mul_mat!(C = 3, R = 3, RhsCols = 2, 3, 4);

impl Mat3x3f {
    pub const ZERO: Self = Self::new(Vec3f::ZERO, Vec3f::ZERO, Vec3f::ZERO);
    pub const IDENTITY: Self = Self::new(
        Vec3f::new(1., 0., 0.),
        Vec3f::new(0., 1., 0.),
        Vec3f::new(0., 0., 1.),
    );

    pub const fn new(v1: Vec3f, v2: Vec3f, v3: Vec3f) -> Self {
        Self([WideVec3f(v1), WideVec3f(v2), WideVec3f(v3)])
    }

    fn dot(&self, row: usize, rhs: Vec3f) -> f32 {
        self[0][row] * rhs[0] + self[1][row] * rhs[1] + self[2][row] * rhs[2]
    }
}

// === Mat3x4f ===

// Declares structs defined by the macro for consistent layout.
// pub struct Mat3x4f ( [WideVec4f; 3] )
my_wgsl_macros::decl_mat3x4f!();
impl_deref_mut!(Mat3x4f, [WideVec4f; 3]);
impl_from_arr_for_mat!(C = 3, R = 4, Cols = 0, 1, 2);
impl_mat_component_wise_arithmetic!(Mat3x4f, 0, 1, 2);
impl_mat_mul_scalar!(Mat3x4f, f32, 0, 1, 2);
impl_mat_mul_vec!(C = 3, R = 4, Rows = 0, 1, 2, 3);
impl_mat_mul_mat!(C = 3, R = 4, RhsCols = 2, 3, 4);

impl Mat3x4f {
    pub const ZERO: Self = Self::new(Vec4f::ZERO, Vec4f::ZERO, Vec4f::ZERO);

    pub const fn new(v1: Vec4f, v2: Vec4f, v3: Vec4f) -> Self {
        Self([WideVec4f(v1), WideVec4f(v2), WideVec4f(v3)])
    }

    fn dot(&self, row: usize, rhs: Vec3f) -> f32 {
        self[0][row] * rhs[0] + self[1][row] * rhs[1] + self[2][row] * rhs[2]
    }
}

// === Mat4x2f ===

// Declares structs defined by the macro for consistent layout.
// pub struct Mat4x2f ( [WideVec2f; 4] )
my_wgsl_macros::decl_mat4x2f!();
impl_deref_mut!(Mat4x2f, [WideVec2f; 4]);
impl_from_arr_for_mat!(C = 4, R = 2, Cols = 0, 1, 2, 3);
impl_mat_component_wise_arithmetic!(Mat4x2f, 0, 1, 2, 3);
impl_mat_mul_scalar!(Mat4x2f, f32, 0, 1, 2, 3);
impl_mat_mul_vec!(C = 4, R = 2, Rows = 0, 1);
impl_mat_mul_mat!(C = 4, R = 2, RhsCols = 2, 3, 4);

impl Mat4x2f {
    pub const ZERO: Self = Self::new(Vec2f::ZERO, Vec2f::ZERO, Vec2f::ZERO, Vec2f::ZERO);

    pub const fn new(v1: Vec2f, v2: Vec2f, v3: Vec2f, v4: Vec2f) -> Self {
        Self([WideVec2f(v1), WideVec2f(v2), WideVec2f(v3), WideVec2f(v4)])
    }

    fn dot(&self, row: usize, rhs: Vec4f) -> f32 {
        self[0][row] * rhs[0]
            + self[1][row] * rhs[1]
            + self[2][row] * rhs[2]
            + self[3][row] * rhs[3]
    }
}

// === Mat4x3f ===

// Declares structs defined by the macro for consistent layout.
// pub struct Mat4x3f ( [WideVec3f; 4] )
my_wgsl_macros::decl_mat4x3f!();
impl_deref_mut!(Mat4x3f, [WideVec3f; 4]);
impl_from_arr_for_mat!(C = 4, R = 3, Cols = 0, 1, 2, 3);
impl_mat_component_wise_arithmetic!(Mat4x3f, 0, 1, 2, 3);
impl_mat_mul_scalar!(Mat4x3f, f32, 0, 1, 2, 3);
impl_mat_mul_vec!(C = 4, R = 3, Rows = 0, 1, 2);
impl_mat_mul_mat!(C = 4, R = 3, RhsCols = 2, 3, 4);

impl Mat4x3f {
    pub const ZERO: Self = Self::new(Vec3f::ZERO, Vec3f::ZERO, Vec3f::ZERO, Vec3f::ZERO);

    pub const fn new(v1: Vec3f, v2: Vec3f, v3: Vec3f, v4: Vec3f) -> Self {
        Self([WideVec3f(v1), WideVec3f(v2), WideVec3f(v3), WideVec3f(v4)])
    }

    fn dot(&self, row: usize, rhs: Vec4f) -> f32 {
        self[0][row] * rhs[0]
            + self[1][row] * rhs[1]
            + self[2][row] * rhs[2]
            + self[3][row] * rhs[3]
    }
}

// === Mat4x4f ===

// Declares structs defined by the macro for consistent layout.
// pub struct Mat4x4f ( [WideVec4f; 4] )
my_wgsl_macros::decl_mat4x4f!();
impl_deref_mut!(Mat4x4f, [WideVec4f; 4]);
impl_from_arr_for_mat!(C = 4, R = 4, Cols = 0, 1, 2, 3);
impl_mat_component_wise_arithmetic!(Mat4x4f, 0, 1, 2, 3);
impl_mat_mul_scalar!(Mat4x4f, f32, 0, 1, 2, 3);
impl_mat_mul_vec!(C = 4, R = 4, Rows = 0, 1, 2, 3);
impl_mat_mul_mat!(C = 4, R = 4, RhsCols = 2, 3, 4);

impl Mat4x4f {
    pub const ZERO: Self = Self::new(Vec4f::ZERO, Vec4f::ZERO, Vec4f::ZERO, Vec4f::ZERO);
    pub const IDENTITY: Self = Self::new(
        Vec4f::new(1., 0., 0., 0.),
        Vec4f::new(0., 1., 0., 0.),
        Vec4f::new(0., 0., 1., 0.),
        Vec4f::new(0., 0., 0., 1.),
    );

    pub const fn new(v1: Vec4f, v2: Vec4f, v3: Vec4f, v4: Vec4f) -> Self {
        Self([WideVec4f(v1), WideVec4f(v2), WideVec4f(v3), WideVec4f(v4)])
    }

    fn dot(&self, row: usize, rhs: Vec4f) -> f32 {
        self[0][row] * rhs[0]
            + self[1][row] * rhs[1]
            + self[2][row] * rhs[2]
            + self[3][row] * rhs[3]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Test +, -, *, /, % between vectors.
    #[test]
    fn test_vec_arithmetic() {
        // vec op vec
        let a = Vec3i::new(1, 2, 3);
        let b = Vec3i::new(4, 5, 6);
        let c = Vec3i::new(5, 7, 9);
        assert_eq!(a + b, c);
        assert_eq!(c - b, a);
        assert_eq!(a * b, Vec3i::new(4, 10, 18));
        assert_eq!(c / a, Vec3i::new(5, 3, 3));
        assert_eq!(c % a, Vec3i::new(0, 1, 0));

        // vec op scalar & scalar op vec
        let v = Vec3i::new(1, 2, 3);
        let s = 2;
        assert_eq!(s + v, Vec3i::new(3, 4, 5));
        assert_eq!(v + s, Vec3i::new(3, 4, 5));
        assert_eq!(s - v, Vec3i::new(1, 0, -1));
        assert_eq!(v - s, Vec3i::new(-1, 0, 1));
        assert_eq!(s * v, Vec3i::new(2, 4, 6));
        assert_eq!(v * s, Vec3i::new(2, 4, 6));
        assert_eq!(s / v, Vec3i::new(2, 1, 0));
        assert_eq!(v / s, Vec3i::new(0, 1, 1));
        assert_eq!(s % v, Vec3i::new(0, 0, 2));
        assert_eq!(v % s, Vec3i::new(1, 0, 1));

        // vec * mat
        let v = Vec3f::new(1., 2., 3.);
        let m = Mat2x3f::from([1., 1., 1., 2., 2., 2.]);
        assert_eq!(v * m, Vec2f::new(6., 12.));
    }

    #[test]
    fn test_mat_arithmetic() {
        // mat +, - mat
        let a = Mat2x2f::from([1., 2., 3., 4.]);
        let b = Mat2x2f::from([5., 7., 9., 11.]);
        assert_eq!(a + b, Mat2x2f::from([6., 9., 12., 15.]));
        assert_eq!(b - a, Mat2x2f::from([4., 5., 6., 7.]));

        // mat * mat
        let a = Mat2x3f::from([1., 2., 3., 4., 5., 6.]);
        let b = Mat4x2f::from([7., 8., 9., 10., 11., 12., 13., 14.]);
        let c = Mat4x3f::from([39., 54., 69., 49., 68., 87., 59., 82., 105., 69., 96., 123.]);
        assert_eq!(a * b, c);

        // mat op scalar & scalar op mat
        let m = Mat2x2f::from([1., 2., 3., 4.]);
        let s = 2.;
        assert_eq!(s * m, Mat2x2f::from([2., 4., 6., 8.]));
        assert_eq!(m * s, Mat2x2f::from([2., 4., 6., 8.]));

        // mat * vec
        let m = Mat2x3f::from([1., 2., 3., 4., 5., 6.]);
        let v = Vec2f::new(1., 2.);
        assert_eq!(m * v, Vec3f::new(9., 12., 15.));
    }
}
