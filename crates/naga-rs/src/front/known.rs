use super::tree::NamedPath;
use naga::{Bytes, Scalar, ScalarKind, Type, TypeInner, VectorSize};
use std::collections::HashMap;
use wgsl_builtin::{helper::*, prelude::*};

pub fn known(path: &NamedPath) -> Option<Type> {
    thread_local! {
        static KNOWN: HashMap<&'static str, Type> = {
            let mut map = HashMap::new();
            map.extend(i8::entry());
            map.extend(u8::entry());
            map.extend(i16::entry());
            map.extend(u16::entry());
            map.extend(i32::entry());
            map.extend(u32::entry());
            map.extend(f32::entry());
            map.extend(i64::entry());
            map.extend(u64::entry());
            map.extend(f64::entry());
            map.extend(isize::entry());
            map.extend(usize::entry());
            map.extend(Vec2i::entry());
            map.extend(Vec3i::entry());
            map.extend(Vec4i::entry());
            map.extend(Vec2u::entry());
            map.extend(Vec3u::entry());
            map.extend(Vec4u::entry());
            map.extend(Vec2f::entry());
            map.extend(Vec3f::entry());
            map.extend(Vec4f::entry());
            map.extend(Mat2x2f::entry());
            map.extend(Mat2x3f::entry());
            map.extend(Mat2x4f::entry());
            map.extend(Mat3x2f::entry());
            map.extend(Mat3x3f::entry());
            map.extend(Mat3x4f::entry());
            map.extend(Mat4x2f::entry());
            map.extend(Mat4x3f::entry());
            map.extend(Mat4x4f::entry());
            map
        };
    }

    if path == "i128" || path == "u128" {
        panic!("128-bits is not allowed");
    }

    KNOWN.with(|known| known.get(path.as_str()).cloned())
}

trait NagaType {
    type Output: IntoIterator<Item = (&'static str, Type)>;

    fn entry() -> Self::Output;
}

impl NagaType for i8 {
    type Output = [(&'static str, Type); 1];

    /// Rust i8 -> Naga i32
    fn entry() -> Self::Output {
        let key = "i8";
        let ty = Type {
            name: Some(key.to_owned()),
            inner: TypeInner::Scalar(Scalar {
                kind: ScalarKind::Sint,
                width: 4,
            }),
        };
        [(key, ty)]
    }
}

impl NagaType for u8 {
    type Output = [(&'static str, Type); 1];

    /// Rust u8 -> Naga u32
    fn entry() -> Self::Output {
        let key = "u8";
        let ty = Type {
            name: Some(key.to_owned()),
            inner: TypeInner::Scalar(Scalar {
                kind: ScalarKind::Uint,
                width: 4,
            }),
        };
        [(key, ty)]
    }
}

impl NagaType for i16 {
    type Output = [(&'static str, Type); 1];

    /// Rust i16 -> Naga i32
    fn entry() -> Self::Output {
        let key = "i16";
        let ty = Type {
            name: Some(key.to_owned()),
            inner: TypeInner::Scalar(Scalar {
                kind: ScalarKind::Sint,
                width: 4,
            }),
        };
        [(key, ty)]
    }
}

impl NagaType for u16 {
    type Output = [(&'static str, Type); 1];

    /// Rust u16 -> Naga u32
    fn entry() -> Self::Output {
        let key = "u16";
        let ty = Type {
            name: Some(key.to_owned()),
            inner: TypeInner::Scalar(Scalar {
                kind: ScalarKind::Uint,
                width: 4,
            }),
        };
        [(key, ty)]
    }
}

impl NagaType for i32 {
    type Output = [(&'static str, Type); 1];

    fn entry() -> Self::Output {
        let key = "i32";
        let ty = Type {
            name: Some(key.to_owned()),
            inner: TypeInner::Scalar(Scalar {
                kind: ScalarKind::Sint,
                width: 4,
            }),
        };
        [(key, ty)]
    }
}

impl NagaType for u32 {
    type Output = [(&'static str, Type); 1];

    fn entry() -> Self::Output {
        let key = "u32";
        let ty = Type {
            name: Some(key.to_owned()),
            inner: TypeInner::Scalar(Scalar {
                kind: ScalarKind::Uint,
                width: 4,
            }),
        };
        [(key, ty)]
    }
}

impl NagaType for f32 {
    type Output = [(&'static str, Type); 1];

    fn entry() -> Self::Output {
        let key = "f32";
        let ty = Type {
            name: Some(key.to_owned()),
            inner: TypeInner::Scalar(Scalar {
                kind: ScalarKind::Float,
                width: 4,
            }),
        };
        [(key, ty)]
    }
}

impl NagaType for i64 {
    type Output = [(&'static str, Type); 1];

    fn entry() -> Self::Output {
        let key = "i64";
        let ty = Type {
            name: Some(key.to_owned()),
            inner: TypeInner::Scalar(Scalar {
                kind: ScalarKind::Sint,
                width: 8,
            }),
        };
        [(key, ty)]
    }
}

impl NagaType for u64 {
    type Output = [(&'static str, Type); 1];

    fn entry() -> Self::Output {
        let key = "u64";
        let ty = Type {
            name: Some(key.to_owned()),
            inner: TypeInner::Scalar(Scalar {
                kind: ScalarKind::Uint,
                width: 8,
            }),
        };
        [(key, ty)]
    }
}

impl NagaType for f64 {
    type Output = [(&'static str, Type); 1];

    fn entry() -> Self::Output {
        let key = "f64";
        let ty = Type {
            name: Some(key.to_owned()),
            inner: TypeInner::Scalar(Scalar {
                kind: ScalarKind::Float,
                width: 8,
            }),
        };
        [(key, ty)]
    }
}

impl NagaType for isize {
    type Output = [(&'static str, Type); 1];

    /// Rust isize -> Naga abstract-int
    fn entry() -> Self::Output {
        let key = "isize";
        let ty = Type {
            name: Some(key.to_owned()),
            inner: TypeInner::Scalar(Scalar {
                kind: ScalarKind::AbstractInt,
                width: 8,
            }),
        };
        [(key, ty)]
    }
}

impl NagaType for usize {
    type Output = [(&'static str, Type); 1];

    /// Rust usize => Naga abstract-int
    fn entry() -> Self::Output {
        let key = "usize";
        let ty = Type {
            name: Some(key.to_owned()),
            inner: TypeInner::Scalar(Scalar {
                kind: ScalarKind::AbstractInt,
                width: 8,
            }),
        };
        [(key, ty)]
    }
}

impl NagaType for wgsl_builtin::prelude::Vec2i {
    type Output = [(&'static str, Type); 3];

    fn entry() -> Self::Output {
        let key_a = "wgsl_builtin::prelude::Vec2i";
        let key_b = "my_wgsl::Vec2i";
        let key_c = "Vec2i";
        let ty = Type {
            name: Some(Self::wgsl_ident().to_owned()),
            inner: TypeInner::Vector {
                size: VectorSize::Bi,
                scalar: Scalar {
                    kind: ScalarKind::Sint,
                    width: 4,
                },
            },
        };
        [(key_a, ty.clone()), (key_b, ty.clone()), (key_c, ty)]
    }
}

impl NagaType for wgsl_builtin::prelude::Vec3i {
    type Output = [(&'static str, Type); 3];

    fn entry() -> Self::Output {
        let key_a = "wgsl_builtin::prelude::Vec3i";
        let key_b = "my_wgsl::Vec3i";
        let key_c = "Vec3i";
        let ty = Type {
            name: Some(Self::wgsl_ident().to_owned()),
            inner: TypeInner::Vector {
                size: VectorSize::Tri,
                scalar: Scalar {
                    kind: ScalarKind::Sint,
                    width: 4,
                },
            },
        };
        [(key_a, ty.clone()), (key_b, ty.clone()), (key_c, ty)]
    }
}

impl NagaType for wgsl_builtin::prelude::Vec4i {
    type Output = [(&'static str, Type); 3];

    fn entry() -> Self::Output {
        let key_a = "wgsl_builtin::prelude::Vec4i";
        let key_b = "my_wgsl::Vec4i";
        let key_c = "Vec4i";
        let ty = Type {
            name: Some(Self::wgsl_ident().to_owned()),
            inner: TypeInner::Vector {
                size: VectorSize::Quad,
                scalar: Scalar {
                    kind: ScalarKind::Sint,
                    width: 4,
                },
            },
        };
        [(key_a, ty.clone()), (key_b, ty.clone()), (key_c, ty)]
    }
}

impl NagaType for wgsl_builtin::prelude::Vec2u {
    type Output = [(&'static str, Type); 3];

    fn entry() -> Self::Output {
        let key_a = "wgsl_builtin::prelude::Vec2u";
        let key_b = "my_wgsl::Vec2u";
        let key_c = "Vec2u";
        let ty = Type {
            name: Some(Self::wgsl_ident().to_owned()),
            inner: TypeInner::Vector {
                size: VectorSize::Bi,
                scalar: Scalar {
                    kind: ScalarKind::Uint,
                    width: 4,
                },
            },
        };
        [(key_a, ty.clone()), (key_b, ty.clone()), (key_c, ty)]
    }
}

impl NagaType for wgsl_builtin::prelude::Vec3u {
    type Output = [(&'static str, Type); 3];

    fn entry() -> Self::Output {
        let key_a = "wgsl_builtin::prelude::Vec3u";
        let key_b = "my_wgsl::Vec3u";
        let key_c = "Vec3u";
        let ty = Type {
            name: Some(Self::wgsl_ident().to_owned()),
            inner: TypeInner::Vector {
                size: VectorSize::Tri,
                scalar: Scalar {
                    kind: ScalarKind::Uint,
                    width: 4,
                },
            },
        };
        [(key_a, ty.clone()), (key_b, ty.clone()), (key_c, ty)]
    }
}

impl NagaType for wgsl_builtin::prelude::Vec4u {
    type Output = [(&'static str, Type); 3];

    fn entry() -> Self::Output {
        let key_a = "wgsl_builtin::prelude::Vec4u";
        let key_b = "my_wgsl::Vec4u";
        let key_c = "Vec4u";
        let ty = Type {
            name: Some(Self::wgsl_ident().to_owned()),
            inner: TypeInner::Vector {
                size: VectorSize::Quad,
                scalar: Scalar {
                    kind: ScalarKind::Uint,
                    width: 4,
                },
            },
        };
        [(key_a, ty.clone()), (key_b, ty.clone()), (key_c, ty)]
    }
}

impl NagaType for wgsl_builtin::prelude::Vec2f {
    type Output = [(&'static str, Type); 3];

    fn entry() -> Self::Output {
        let key_a = "wgsl_builtin::prelude::Vec2f";
        let key_b = "my_wgsl::Vec2f";
        let key_c = "Vec2f";
        let ty = Type {
            name: Some(Self::wgsl_ident().to_owned()),
            inner: TypeInner::Vector {
                size: VectorSize::Bi,
                scalar: Scalar {
                    kind: ScalarKind::Float,
                    width: 4,
                },
            },
        };
        [(key_a, ty.clone()), (key_b, ty.clone()), (key_c, ty)]
    }
}

impl NagaType for wgsl_builtin::prelude::Vec3f {
    type Output = [(&'static str, Type); 3];

    fn entry() -> Self::Output {
        let key_a = "wgsl_builtin::prelude::Vec3f";
        let key_b = "my_wgsl::Vec3f";
        let key_c = "Vec3f";
        let ty = Type {
            name: Some(Self::wgsl_ident().to_owned()),
            inner: TypeInner::Vector {
                size: VectorSize::Tri,
                scalar: Scalar {
                    kind: ScalarKind::Float,
                    width: 4,
                },
            },
        };
        [(key_a, ty.clone()), (key_b, ty.clone()), (key_c, ty)]
    }
}

impl NagaType for wgsl_builtin::prelude::Vec4f {
    type Output = [(&'static str, Type); 3];

    fn entry() -> Self::Output {
        let key_a = "wgsl_builtin::prelude::Vec4f";
        let key_b = "my_wgsl::Vec4f";
        let key_c = "Vec4f";
        let ty = Type {
            name: Some(Self::wgsl_ident().to_owned()),
            inner: TypeInner::Vector {
                size: VectorSize::Quad,
                scalar: Scalar {
                    kind: ScalarKind::Float,
                    width: 4,
                },
            },
        };
        [(key_a, ty.clone()), (key_b, ty.clone()), (key_c, ty)]
    }
}

impl NagaType for wgsl_builtin::prelude::Mat2x2f {
    type Output = [(&'static str, Type); 3];

    fn entry() -> Self::Output {
        let key_a = "wgsl_builtin::prelude::Mat2x2f";
        let key_b = "my_wgsl::Mat2x2f";
        let key_c = "Mat2x2f";
        let ty = Type {
            name: Some(Self::wgsl_ident().to_owned()),
            inner: TypeInner::Matrix {
                columns: VectorSize::Bi,
                rows: VectorSize::Bi,
                scalar: Scalar {
                    kind: ScalarKind::Float,
                    width: 4,
                },
            },
        };
        [(key_a, ty.clone()), (key_b, ty.clone()), (key_c, ty)]
    }
}

impl NagaType for wgsl_builtin::prelude::Mat2x3f {
    type Output = [(&'static str, Type); 3];

    fn entry() -> Self::Output {
        let key_a = "wgsl_builtin::prelude::Mat2x3f";
        let key_b = "my_wgsl::Mat2x3f";
        let key_c = "Mat2x3f";
        let ty = Type {
            name: Some(Self::wgsl_ident().to_owned()),
            inner: TypeInner::Matrix {
                columns: VectorSize::Bi,
                rows: VectorSize::Tri,
                scalar: Scalar {
                    kind: ScalarKind::Float,
                    width: 4,
                },
            },
        };
        [(key_a, ty.clone()), (key_b, ty.clone()), (key_c, ty)]
    }
}

impl NagaType for wgsl_builtin::prelude::Mat2x4f {
    type Output = [(&'static str, Type); 3];

    fn entry() -> Self::Output {
        let key_a = "wgsl_builtin::prelude::Mat2x4f";
        let key_b = "my_wgsl::Mat2x4f";
        let key_c = "Mat2x4f";
        let ty = Type {
            name: Some(Self::wgsl_ident().to_owned()),
            inner: TypeInner::Matrix {
                columns: VectorSize::Bi,
                rows: VectorSize::Quad,
                scalar: Scalar {
                    kind: ScalarKind::Float,
                    width: 4,
                },
            },
        };
        [
            (key_a, ty.clone()),
            (key_b, ty.clone()),
            (key_c, ty.clone()),
        ]
    }
}

impl NagaType for wgsl_builtin::prelude::Mat3x2f {
    type Output = [(&'static str, Type); 3];

    fn entry() -> Self::Output {
        let key_a = "wgsl_builtin::prelude::Mat3x2f";
        let key_b = "my_wgsl::Mat3x2f";
        let key_c = "Mat3x2f";
        let ty = Type {
            name: Some(Self::wgsl_ident().to_owned()),
            inner: TypeInner::Matrix {
                columns: VectorSize::Tri,
                rows: VectorSize::Bi,
                scalar: Scalar {
                    kind: ScalarKind::Float,
                    width: 4,
                },
            },
        };
        [(key_a, ty.clone()), (key_b, ty.clone()), (key_c, ty)]
    }
}

impl NagaType for wgsl_builtin::prelude::Mat3x3f {
    type Output = [(&'static str, Type); 3];

    fn entry() -> Self::Output {
        let key_a = "wgsl_builtin::prelude::Mat3x3f";
        let key_b = "my_wgsl::Mat3x3f";
        let key_c = "Mat3x3f";
        let ty = Type {
            name: Some(Self::wgsl_ident().to_owned()),
            inner: TypeInner::Matrix {
                columns: VectorSize::Tri,
                rows: VectorSize::Tri,
                scalar: Scalar {
                    kind: ScalarKind::Float,
                    width: 4,
                },
            },
        };
        [(key_a, ty.clone()), (key_b, ty.clone()), (key_c, ty)]
    }
}

impl NagaType for wgsl_builtin::prelude::Mat3x4f {
    type Output = [(&'static str, Type); 3];

    fn entry() -> Self::Output {
        let key_a = "wgsl_builtin::prelude::Mat3x4f";
        let key_b = "my_wgsl::Mat3x4f";
        let key_c = "Mat3x4f";
        let ty = Type {
            name: Some(Self::wgsl_ident().to_owned()),
            inner: TypeInner::Matrix {
                columns: VectorSize::Tri,
                rows: VectorSize::Quad,
                scalar: Scalar {
                    kind: ScalarKind::Float,
                    width: 4,
                },
            },
        };
        [(key_a, ty.clone()), (key_b, ty.clone()), (key_c, ty)]
    }
}

impl NagaType for wgsl_builtin::prelude::Mat4x2f {
    type Output = [(&'static str, Type); 3];

    fn entry() -> Self::Output {
        let key_a = "wgsl_builtin::prelude::Mat4x2f";
        let key_b = "my_wgsl::Mat4x2f";
        let key_c = "Mat4x2f";
        let ty = Type {
            name: Some(Self::wgsl_ident().to_owned()),
            inner: TypeInner::Matrix {
                columns: VectorSize::Quad,
                rows: VectorSize::Bi,
                scalar: Scalar {
                    kind: ScalarKind::Float,
                    width: 4,
                },
            },
        };
        [(key_a, ty.clone()), (key_b, ty.clone()), (key_c, ty)]
    }
}

impl NagaType for wgsl_builtin::prelude::Mat4x3f {
    type Output = [(&'static str, Type); 3];

    fn entry() -> Self::Output {
        let key_a = "wgsl_builtin::prelude::Mat4x3f";
        let key_b = "my_wgsl::Mat4x3f";
        let key_c = "Mat4x3f";
        let ty = Type {
            name: Some(Self::wgsl_ident().to_owned()),
            inner: TypeInner::Matrix {
                columns: VectorSize::Quad,
                rows: VectorSize::Tri,
                scalar: Scalar {
                    kind: ScalarKind::Float,
                    width: 4,
                },
            },
        };
        [(key_a, ty.clone()), (key_b, ty.clone()), (key_c, ty)]
    }
}

impl NagaType for wgsl_builtin::prelude::Mat4x4f {
    type Output = [(&'static str, Type); 3];

    fn entry() -> Self::Output {
        let key_a = "wgsl_builtin::prelude::Mat4x4f";
        let key_b = "my_wgsl::Mat4x4f";
        let key_c = "Mat4x4f";
        let ty = Type {
            name: Some(Self::wgsl_ident().to_owned()),
            inner: TypeInner::Matrix {
                columns: VectorSize::Quad,
                rows: VectorSize::Quad,
                scalar: Scalar {
                    kind: ScalarKind::Float,
                    width: 4,
                },
            },
        };
        [(key_a, ty.clone()), (key_b, ty.clone()), (key_c, ty)]
    }
}
