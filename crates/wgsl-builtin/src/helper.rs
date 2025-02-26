use super::types::*;

// === IdentHelper ===

pub trait IdentHelper {
    fn ident() -> &'static str;
    fn wgsl_ident() -> &'static str;
}

#[rustfmt::skip]
macro_rules! impl_ident_helper {
    ($ty:ty, $wgsl_ty:ident) => {
        impl IdentHelper for $ty {
            fn ident() -> &'static str { stringify!($ty) }
            fn wgsl_ident() -> &'static str { stringify!($wgsl_ty) }
        }
    };
}

impl_ident_helper!(Bool, bool);
impl_ident_helper!(Vec2i, vec2i);
impl_ident_helper!(Vec3i, vec3i);
impl_ident_helper!(Vec4i, vec4i);
impl_ident_helper!(Vec2u, vec2u);
impl_ident_helper!(Vec3u, vec3u);
impl_ident_helper!(Vec4u, vec4u);
impl_ident_helper!(Vec2f, vec2f);
impl_ident_helper!(Vec3f, vec3f);
impl_ident_helper!(Vec4f, vec4f);
impl_ident_helper!(WideVec2i, vec2i);
impl_ident_helper!(WideVec3i, vec3i);
impl_ident_helper!(WideVec4i, vec4i);
impl_ident_helper!(WideVec2u, vec2u);
impl_ident_helper!(WideVec3u, vec3u);
impl_ident_helper!(WideVec4u, vec4u);
impl_ident_helper!(WideVec2f, vec2f);
impl_ident_helper!(WideVec3f, vec3f);
impl_ident_helper!(WideVec4f, vec4f);
impl_ident_helper!(Mat2x2f, mat2x2f);
impl_ident_helper!(Mat2x3f, mat2x3f);
impl_ident_helper!(Mat2x4f, mat2x4f);
impl_ident_helper!(Mat3x2f, mat3x2f);
impl_ident_helper!(Mat3x3f, mat3x3f);
impl_ident_helper!(Mat3x4f, mat3x4f);
impl_ident_helper!(Mat4x2f, mat4x2f);
impl_ident_helper!(Mat4x3f, mat4x3f);
impl_ident_helper!(Mat4x4f, mat4x4f);

// === LayoutHelper ===

pub trait LayoutHelper {
    fn wgsl_size() -> usize;
    fn wgsl_align() -> usize;
}

#[rustfmt::skip]
macro_rules! impl_layout_helper {
    ($ty:ty, $size:expr, $align:expr) => {
        impl LayoutHelper for $ty {
            fn wgsl_size() -> usize { $size }
            fn wgsl_align() -> usize { $align }
        }
    };
}

impl_layout_helper!(Bool, 4, 4);
impl_layout_helper!(i32, 4, 4);
impl_layout_helper!(u32, 4, 4);
impl_layout_helper!(f32, 4, 4);
impl_layout_helper!(Vec2i, 8, 8);
impl_layout_helper!(Vec3i, 12, 16);
impl_layout_helper!(Vec4i, 16, 16);
impl_layout_helper!(Vec2u, 8, 8);
impl_layout_helper!(Vec3u, 12, 16);
impl_layout_helper!(Vec4u, 16, 16);
impl_layout_helper!(Vec2f, 8, 8);
impl_layout_helper!(Vec3f, 12, 16);
impl_layout_helper!(Vec4f, 16, 16);
impl_layout_helper!(WideVec2i, 8, 8);
impl_layout_helper!(WideVec3i, 16, 16);
impl_layout_helper!(WideVec4i, 16, 16);
impl_layout_helper!(WideVec2u, 8, 8);
impl_layout_helper!(WideVec3u, 16, 16);
impl_layout_helper!(WideVec4u, 16, 16);
impl_layout_helper!(WideVec2f, 8, 8);
impl_layout_helper!(WideVec3f, 16, 16);
impl_layout_helper!(WideVec4f, 16, 16);
impl_layout_helper!(Mat2x2f, 16, 8);
impl_layout_helper!(Mat2x3f, 32, 16);
impl_layout_helper!(Mat2x4f, 32, 16);
impl_layout_helper!(Mat3x2f, 24, 8);
impl_layout_helper!(Mat3x3f, 48, 16);
impl_layout_helper!(Mat3x4f, 48, 16);
impl_layout_helper!(Mat4x2f, 32, 8);
impl_layout_helper!(Mat4x3f, 64, 16);
impl_layout_helper!(Mat4x4f, 64, 16);

// === ConstructorHelper ===

pub trait ConstructorHelper {
    /// Returns pairs of Rust constructor & WGSL constructor.
    ///
    /// e.g. Vec2i::splat & vec2i
    fn constructors() -> impl Iterator<Item = (&'static str, &'static str)>;
}

macro_rules! impl_constructor_helper_for_vec {
    ($ty:ty, $wgsl_ty:ident) => {
        impl ConstructorHelper for $ty {
            fn constructors() -> impl Iterator<Item = (&'static str, &'static str)> {
                debug_assert_eq!(<$ty>::wgsl_ident(), stringify!($wgsl_ty));

                [
                    (concat!(stringify!($ty), "::new"), stringify!($wgsl_ty)),
                    (concat!(stringify!($ty), "::splat"), stringify!($wgsl_ty)),
                    (
                        concat!(stringify!($ty), "::ZERO"),
                        concat!(stringify!($wgsl_ty), "()"),
                    ),
                ]
                .into_iter()
            }
        }
    };
}

macro_rules! impl_constructor_helper_for_mat {
    ($ty:ty, $wgsl_ty:ident $(, $extra:expr)* ) => {
        impl ConstructorHelper for $ty {
            fn constructors() -> impl Iterator<Item = (&'static str, &'static str)> {
                debug_assert_eq!(<$ty>::wgsl_ident(), stringify!($wgsl_ty));

                [
                    (concat!(stringify!($ty), "::new"), stringify!($wgsl_ty)),
                    $($extra),*
                ].into_iter()
            }
        }
    };
}

impl ConstructorHelper for Bool {
    fn constructors() -> impl Iterator<Item = (&'static str, &'static str)> {
        [].into_iter()
    }
}

impl_constructor_helper_for_vec!(Vec2i, vec2i);
impl_constructor_helper_for_vec!(Vec3i, vec3i);
impl_constructor_helper_for_vec!(Vec4i, vec4i);
impl_constructor_helper_for_vec!(Vec2u, vec2u);
impl_constructor_helper_for_vec!(Vec3u, vec3u);
impl_constructor_helper_for_vec!(Vec4u, vec4u);
impl_constructor_helper_for_vec!(Vec2f, vec2f);
impl_constructor_helper_for_vec!(Vec3f, vec3f);
impl_constructor_helper_for_vec!(Vec4f, vec4f);
impl_constructor_helper_for_vec!(WideVec2i, vec2i);
impl_constructor_helper_for_vec!(WideVec3i, vec3i);
impl_constructor_helper_for_vec!(WideVec4i, vec4i);
impl_constructor_helper_for_vec!(WideVec2u, vec2u);
impl_constructor_helper_for_vec!(WideVec3u, vec3u);
impl_constructor_helper_for_vec!(WideVec4u, vec4u);
impl_constructor_helper_for_vec!(WideVec2f, vec2f);
impl_constructor_helper_for_vec!(WideVec3f, vec3f);
impl_constructor_helper_for_vec!(WideVec4f, vec4f);
impl_constructor_helper_for_mat!(
    Mat2x2f,
    mat2x2f,
    ("Mat2x2f::ZERO", "mat2x2f(0,0,0,0)"),
    ("Mat2x2f::IDENTITY", "mat2x2f(1,0,0,1)")
);
impl_constructor_helper_for_mat!(Mat2x3f, mat2x3f, ("Mat2x3f::ZERO", "mat2x3f(0,0,0,0,0,0)"));
impl_constructor_helper_for_mat!(
    Mat2x4f,
    mat2x4f,
    ("Mat2x4f::ZERO", "mat2x4f(0,0,0,0,0,0,0,0)")
);
impl_constructor_helper_for_mat!(Mat3x2f, mat3x2f, ("Mat3x2f::ZERO", "mat3x2f(0,0,0,0,0,0)"));
impl_constructor_helper_for_mat!(
    Mat3x3f,
    mat3x3f,
    ("Mat3x3f::ZERO", "mat3x3f(0,0,0,0,0,0,0,0,0)"),
    ("Mat3x3f::IDENTITY", "mat3x3f(1,0,0,0,1,0,0,0,1)")
);
impl_constructor_helper_for_mat!(
    Mat3x4f,
    mat3x4f,
    ("Mat3x4f::ZERO", "mat3x4f(0,0,0,0,0,0,0,0,0,0,0,0)")
);
impl_constructor_helper_for_mat!(
    Mat4x2f,
    mat4x2f,
    ("Mat4x2f::ZERO", "mat4x2f(0,0,0,0,0,0,0,0)")
);
impl_constructor_helper_for_mat!(
    Mat4x3f,
    mat4x3f,
    ("Mat4x3f::ZERO", "mat4x3f(0,0,0,0,0,0,0,0,0,0,0,0)")
);
impl_constructor_helper_for_mat!(
    Mat4x4f,
    mat4x4f,
    ("Mat4x4f::ZERO", "mat4x4f(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)"),
    (
        "Mat4x4f::IDENTITY",
        "mat4x4f(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1)"
    )
);
