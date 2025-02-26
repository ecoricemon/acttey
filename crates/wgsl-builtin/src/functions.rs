// Use SIMD? Not convinced yet. Consider when portable SIMD becomes stable.

use super::types::{
    Mat2x2f, Mat2x3f, Mat2x4f, Mat3x2f, Mat3x3f, Mat3x4f, Mat4x2f, Mat4x3f, Mat4x4f, Vec2f, Vec2i,
    Vec2u, Vec3f, Vec3i, Vec3u, Vec4f, Vec4i, Vec4u,
};

// === abs ===

pub trait WgslAbs {
    #[must_use]
    fn abs(self) -> Self;
}

macro_rules! impl_abs_for_vec {
    ($id:ty, $($i:expr),*) => {
        impl WgslAbs for $id {
            fn abs(self) -> Self {
                Self::new( $( self[$i].abs() ),* )
            }
        }
    };
    ($id:ty) => {
        impl WgslAbs for $id { fn abs(self) -> Self { self } }
    };
}

#[rustfmt::skip] impl WgslAbs for i32 { fn abs(self) -> Self { self.abs() } }
#[rustfmt::skip] impl WgslAbs for u32 { fn abs(self) -> Self { self } }
#[rustfmt::skip] impl WgslAbs for f32 { fn abs(self) -> Self { self.abs() } }
impl_abs_for_vec!(Vec2i, 0, 1);
impl_abs_for_vec!(Vec3i, 0, 1, 2);
impl_abs_for_vec!(Vec4i, 0, 1, 2, 3);
impl_abs_for_vec!(Vec2u);
impl_abs_for_vec!(Vec3u);
impl_abs_for_vec!(Vec4u);
impl_abs_for_vec!(Vec2f, 0, 1);
impl_abs_for_vec!(Vec3f, 0, 1, 2);
impl_abs_for_vec!(Vec4f, 0, 1, 2, 3);

// === cross ===

pub trait WgslCross {
    #[must_use]
    fn cross(self, rhs: Self) -> Self;
}

impl WgslCross for Vec3f {
    fn cross(self, rhs: Self) -> Self {
        Self::new(
            self[1] * rhs[2] - self[2] * rhs[1],
            self[2] * rhs[0] - self[0] * rhs[2],
            self[0] * rhs[1] - self[1] * rhs[0],
        )
    }
}

// === determinant ===

pub trait WgslDeterminant {
    type Output;

    #[must_use]
    fn determinant(self) -> Self::Output;
}

impl WgslDeterminant for Mat2x2f {
    type Output = f32;

    fn determinant(self) -> Self::Output {
        self[0][0] * self[1][1] - self[0][1] * self[1][0]
    }
}

impl WgslDeterminant for Mat3x3f {
    type Output = f32;

    #[rustfmt::skip]
    fn determinant(self) -> Self::Output {
        let s = self;
          s[0][0] * (s[1][1] * s[2][2] - s[1][2] * s[2][1])
        - s[1][0] * (s[0][1] * s[2][2] - s[0][2] * s[2][1])
        + s[2][0] * (s[0][1] * s[1][2] - s[0][2] * s[1][1])
    }
}

impl WgslDeterminant for Mat4x4f {
    type Output = f32;

    #[rustfmt::skip]
    fn determinant(self) -> Self::Output {
        let s = self;

        let a = s[2][2] * s[3][3] - s[2][3] * s[3][2];
        let b = s[1][2] * s[3][3] - s[1][3] * s[3][2];
        let c = s[1][2] * s[2][3] - s[1][3] * s[2][2];
        let d = s[0][2] * s[3][3] - s[0][3] * s[3][2];
        let e = s[0][2] * s[2][3] - s[0][3] * s[2][2];
        let f = s[0][2] * s[1][3] - s[0][3] * s[1][2];

          s[0][0] * (s[1][1] * a - s[2][1] * b + s[3][1] * c)
        - s[1][0] * (s[0][1] * a - s[2][1] * d + s[3][1] * e)
        + s[2][0] * (s[0][1] * b - s[1][1] * d + s[3][1] * f)
        - s[3][0] * (s[0][1] * c - s[1][1] * e + s[2][1] * f)
    }
}

// === dot ===

pub trait WgslDot {
    type Output;

    #[must_use]
    fn dot(self, rhs: Self) -> Self::Output;
}

macro_rules! impl_dot_for_vec {
    ($id:ty, $out:ty, 0, $($tail:expr),*) => {
        impl WgslDot for $id {
            type Output = $out;
            fn dot(self, rhs: Self) -> Self::Output {
                self[0] * rhs[0]
                $( + self[$tail] * rhs[$tail] )*
            }
        }
    };
}

impl_dot_for_vec!(Vec2i, i32, 0, 1);
impl_dot_for_vec!(Vec3i, i32, 0, 1, 2);
impl_dot_for_vec!(Vec4i, i32, 0, 1, 2, 3);
impl_dot_for_vec!(Vec2u, u32, 0, 1);
impl_dot_for_vec!(Vec3u, u32, 0, 1, 2);
impl_dot_for_vec!(Vec4u, u32, 0, 1, 2, 3);
impl_dot_for_vec!(Vec2f, f32, 0, 1);
impl_dot_for_vec!(Vec3f, f32, 0, 1, 2);
impl_dot_for_vec!(Vec4f, f32, 0, 1, 2, 3);

// === length ===

pub trait WgslLength {
    type Output;

    #[must_use]
    fn length(self) -> Self::Output;
}

macro_rules! impl_length_for_vec {
    ($id:ty, $out:ty) => {
        impl WgslLength for $id {
            type Output = $out;

            fn length(self) -> Self::Output {
                self.dot(self).sqrt()
            }
        }
    };
}

#[rustfmt::skip]
impl WgslLength for f32 {
    type Output = Self;
    fn length(self) -> Self::Output { WgslAbs::abs(self) }
}
impl_length_for_vec!(Vec2f, f32);
impl_length_for_vec!(Vec3f, f32);
impl_length_for_vec!(Vec4f, f32);

// === sqrt ===

pub trait Sqrt {
    #[must_use]
    fn sqrt(self) -> Self;
}

macro_rules! impl_sqrt_fot_vec {
    ($id:ty, $($i:expr),*) => {
        impl Sqrt for $id {
            fn sqrt(self) -> Self {
                Self::new( $( self[$i].sqrt() ),* )
            }
        }
    };
}

#[rustfmt::skip] impl Sqrt for f32 { fn sqrt(self) -> Self { self.sqrt() } }
impl_sqrt_fot_vec!(Vec2f, 0, 1);
impl_sqrt_fot_vec!(Vec3f, 0, 1, 2);
impl_sqrt_fot_vec!(Vec4f, 0, 1, 2, 3);

// === transpose ===

pub trait WgslTranspose {
    type Output;
    #[must_use]
    fn transpose(self) -> Self::Output;
}

#[rustfmt::skip]
macro_rules! impl_transpose {
    (C = $nc:expr, R = $nr:expr, Cols = $($c:expr),* ; Rows = $($r:expr),*) => {
        const _: () = { paste::paste! {
            type T = [<Mat $nc x $nr f>];
            type Out = [<Mat $nr x $nc f>];
            type V = [<Vec $nc f>];

            impl WgslTranspose for T {
                type Output = Out;
                fn transpose(self) -> Self::Output {
                    impl_transpose!(@helper_mat Out ; V ; self ; ($($c),*) ; $($r),*)
                }
            }
        }};
    };
    // Hides repetition of $c for nested macro invocation.
    (@helper_mat $mat:ident ; $vec:ident ; $self:tt ; $c:tt ; $($r:expr),*) => {
        $mat::new(
            $( impl_transpose!(@helper_col $vec ; $self ; $c ; $r) ),*
        )
    };
    (@helper_col $vec:ident ; $self:tt ; ( $($c:expr),* ) ; $r:expr) => {
        $vec::new( 
            $( $self[$c][$r] ),* 
        )
    };
}

impl_transpose!(C = 2, R = 2, Cols = 0, 1; Rows = 0, 1);
impl_transpose!(C = 2, R = 3, Cols = 0, 1; Rows = 0, 1, 2);
impl_transpose!(C = 2, R = 4, Cols = 0, 1; Rows = 0, 1, 2, 3);
impl_transpose!(C = 3, R = 2, Cols = 0, 1, 2; Rows = 0, 1);
impl_transpose!(C = 3, R = 3, Cols = 0, 1, 2; Rows = 0, 1, 2);
impl_transpose!(C = 3, R = 4, Cols = 0, 1, 2; Rows = 0, 1, 2, 3);
impl_transpose!(C = 4, R = 2, Cols = 0, 1, 2, 3; Rows = 0, 1);
impl_transpose!(C = 4, R = 3, Cols = 0, 1, 2, 3; Rows = 0, 1, 2);
impl_transpose!(C = 4, R = 4, Cols = 0, 1, 2, 3; Rows = 0, 1, 2, 3);

#[cfg(test)]
mod tests {
    use super::*;

    #[rustfmt::skip]
    #[test]
    fn test_determinant() {
        assert_eq!(Mat2x2f::from([1., 2., 3., 4.]).determinant(), -2.);
        assert_eq!(
            Mat3x3f::from([
                2., 5., 3.,
                1., 2., 5.,
                3., 2., 1.
            ]).determinant(), 42.
        );
        assert_eq!(
            Mat4x4f::from([
                1., 4., 2., 3.,
                2., 3., 4., 5., 
                6., 3., 4., 5., 
                6., 7., 5., 4.,
            ]).determinant(), -116.
        );
    }

    #[rustfmt::skip]
    #[test]
    fn test_transpose() {
        assert_eq!(
            Mat2x2f::from([1., 2., 3., 4.]).transpose(),
            Mat2x2f::from([1., 3., 2., 4.])
        );
        assert_eq!(
            Mat3x3f::from([1., 2., 3., 4., 5., 6., 7., 8., 9.]).transpose(),
            Mat3x3f::from([1., 4., 7., 2., 5., 8., 3., 6., 9.])
        );
        assert_eq!(
            Mat4x4f::from([
                1., 2., 3., 4., 5., 6., 7., 8., 
                9., 10., 11., 12., 13., 14., 15., 16.
            ]).transpose(),
            Mat4x4f::from([
                1., 5., 9., 13., 2., 6., 10., 14., 
                3., 7., 11., 15., 4., 8., 12., 16.
            ])
        );
    }
}
