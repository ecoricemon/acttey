use my_wgsl::*;

#[test]
fn test_struct_size_align() {
    #[wgsl_mod]
    #[rustfmt::skip]
    mod m {
        use super::*;

        pub(super) struct S0 { a: Bool }
        pub(super) const S0_SIZE_ALIGN: (usize, usize) = (4, 4);

        pub(super) struct S1 { a: i32 }
        pub(super) const S1_SIZE_ALIGN: (usize, usize) = (4, 4);

        pub(super) struct S2 { a: u32 }
        pub(super) const S2_SIZE_ALIGN: (usize, usize) = (4, 4);

        pub(super) struct S3 { a: f32 }
        pub(super) const S3_SIZE_ALIGN: (usize, usize) = (4, 4);

        pub(super) struct S4 { a: Bool, b: u32 }
        pub(super) const S4_SIZE_ALIGN: (usize, usize) = (8, 4);

        pub(super) struct S5 { a: f32, b: u32 }
        pub(super) const S5_SIZE_ALIGN: (usize, usize) = (8, 4);

        pub(super) struct S6 { a: Vec2i }
        pub(super) const S6_SIZE_ALIGN: (usize, usize) = (8, 8);

        pub(super) struct S7 { a: Vec3i }
        pub(super) const S7_SIZE_ALIGN: (usize, usize) = (16, 16);

        pub(super) struct S8 { a: Vec4i }
        pub(super) const S8_SIZE_ALIGN: (usize, usize) = (16, 16);

        pub(super) struct S9 { a: Vec2u }
        pub(super) const S9_SIZE_ALIGN: (usize, usize) = (8, 8);

        pub(super) struct S10 { a: Vec3u }
        pub(super) const S10_SIZE_ALIGN: (usize, usize) = (16, 16);

        pub(super) struct S11 { a: Vec4u }
        pub(super) const S11_SIZE_ALIGN: (usize, usize) = (16, 16);

        pub(super) struct S12 { a: Vec2f }
        pub(super) const S12_SIZE_ALIGN: (usize, usize) = (8, 8);

        pub(super) struct S13 { a: Vec3f }
        pub(super) const S13_SIZE_ALIGN: (usize, usize) = (16, 16);

        pub(super) struct S14 { a: Vec4f }
        pub(super) const S14_SIZE_ALIGN: (usize, usize) = (16, 16);

        pub(super) struct S15 { a: Vec3i, b: u32 }
        pub(super) const S15_SIZE_ALIGN: (usize, usize) = (16, 16);

        pub(super) struct S16 { a: Vec3u, b: u32 }
        pub(super) const S16_SIZE_ALIGN: (usize, usize) = (16, 16);

        pub(super) struct S17 { a: Vec3f, b: u32 }
        pub(super) const S17_SIZE_ALIGN: (usize, usize) = (16, 16);

        pub(super) struct S18 { a: S12, b: S13 }
        pub(super) const S18_SIZE_ALIGN: (usize, usize) = (32, 16);

        pub(super) struct S19 { a: Mat2x2f }
        pub(super) const S19_SIZE_ALIGN: (usize, usize) = (16, 8);

        pub(super) struct S20 { a: Mat2x3f }
        pub(super) const S20_SIZE_ALIGN: (usize, usize) = (32, 16);

        pub(super) struct S21 { a: Mat2x4f }
        pub(super) const S21_SIZE_ALIGN: (usize, usize) = (32, 16);

        pub(super) struct S22 { a: Mat3x2f }
        pub(super) const S22_SIZE_ALIGN: (usize, usize) = (24, 8);

        pub(super) struct S23 { a: Mat3x3f }
        pub(super) const S23_SIZE_ALIGN: (usize, usize) = (48, 16);

        pub(super) struct S24 { a: Mat3x4f }
        pub(super) const S24_SIZE_ALIGN: (usize, usize) = (48, 16);

        pub(super) struct S25 { a: Mat4x2f }
        pub(super) const S25_SIZE_ALIGN: (usize, usize) = (32, 8);

        pub(super) struct S26 { a: Mat4x3f }
        pub(super) const S26_SIZE_ALIGN: (usize, usize) = (64, 16);

        pub(super) struct S27 { a: Mat4x4f }
        pub(super) const S27_SIZE_ALIGN: (usize, usize) = (64, 16);

        pub(super) struct S28 { a: [Bool; 3] }
        pub(super) const S28_SIZE_ALIGN: (usize, usize) = (12, 4);

        pub(super) struct S29 { a: [f32; 3] }
        pub(super) const S29_SIZE_ALIGN: (usize, usize) = (12, 4);

        pub(super) struct S30 { a: [[f32; 3]; 3] }
        pub(super) const S30_SIZE_ALIGN: (usize, usize) = (36, 4);

        pub(super) struct S31 { a: [S30; 3] }
        pub(super) const S31_SIZE_ALIGN: (usize, usize) = 
            (S30_SIZE_ALIGN.0 * 3, S30_SIZE_ALIGN.1);

        #[uniform] pub(super) struct S32 { a: f32 }
        pub(super) const S32_SIZE_ALIGN: (usize, usize) = (16, 16);

        pub(super) struct S100 { a: crate::Mat4x4f }
    }
    use m::*;

    macro_rules! assert_layout {
        ($ty:ident, $size_align:expr) => {
            assert_eq!(size_of::<$ty>(), $size_align.0);
            assert_eq!(align_of::<$ty>(), $size_align.1);
        };
    }

    macro_rules! assert_layout_many {
        ($head:expr, $($tail:expr),*) => {
            assert_layout_many!($head);
            assert_layout_many!($($tail),*);
        };
        ($i:expr) => {
            paste::paste! { assert_layout!([<S $i>], [<S $i _SIZE_ALIGN>]) }
        };
    }

    assert_layout_many!(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
    assert_layout_many!(10, 11, 12, 13, 14, 15, 16, 17, 18, 19);
    assert_layout_many!(20, 21, 22, 23, 24, 25, 26, 27, 28, 29);
    assert_layout_many!(30, 31, 32);
}

#[test]
fn test_struct_offset() {
    #[wgsl_mod]
    #[rustfmt::skip]
    mod m {
        use super::*;
        use std::mem::offset_of;

        struct S0 { a: f32, b: f32 }
        pub(super) fn s0() { assert_eq!(offset_of!(S0, b), 4) }

        #[uniform] struct S1 { a: f32, b: f32 }
        pub(super) fn s1() { assert_eq!(offset_of!(S1, b), 16) }

        struct S2 { a: f32, b: Vec3f }
        pub(super) fn s2() { assert_eq!(offset_of!(S2, b), 16) }

        #[uniform] struct S3 { a: f32, b: Vec3f }
        pub(super) fn s3() { assert_eq!(offset_of!(S3, b), 16) }

        struct S4 { a: f32, b: [f32; 3] }
        pub(super) fn s4() { assert_eq!(offset_of!(S4, b), 4) }

        struct S5 { a: Vec2i, b: Vec3i, c: [WideVec3i; 2] }
        pub(super) fn s5() { assert_eq!(offset_of!(S5, b), 16) }
        pub(super) fn s6() { assert_eq!(offset_of!(S5, c), 32) }
    }
    use m::*;

    macro_rules! assert_offset_many {
        ($head:expr, $($tail:expr),*) => {
            assert_offset_many!($head);
            assert_offset_many!($($tail),*);
        };
        ($i:expr) => {
            paste::paste! { [<s $i>](); }
        };
    }

    assert_offset_many!(0, 1, 2, 3, 4, 5, 6);
}

#[test]
fn test_runtime_sized_array_in_struct() {
    #[wgsl_mod]
    mod m {
        use super::*;

        pub(super) struct S {
            a: Vec2i,
            b: Vec3i,
            c: [WideVec3i],
        }
    }
    use m::*;

    fn test_get_set() {
        let a = Vec2i::new(0, 1);
        let b = Vec3i::new(2, 3, 4);
        let mut s = S::new(a, b);

        assert_eq!(s.get_a(), &Vec2i::new(0, 1));
        assert_eq!(s.get_b(), &Vec3i::new(2, 3, 4));

        let old_a = s.set_a(Vec2i::new(10, 11));
        let old_b = s.set_b(Vec3i::new(12, 13, 14));
        assert_eq!(old_a, Vec2i::new(0, 1));
        assert_eq!(old_b, Vec3i::new(2, 3, 4));

        assert_eq!(s.get_a(), &Vec2i::new(10, 11));
        assert_eq!(s.get_b(), &Vec3i::new(12, 13, 14));
    }
    test_get_set();

    fn test_resize() {
        let a = Vec2i::new(0, 0);
        let b = Vec3i::new(0, 0, 0);
        let mut s = S::new(a, b);

        assert!(s.get_c().is_empty());

        s.extend_with(3, |_| WideVec3i::new(1, 2, 3));
        assert_eq!(s.get_c().len(), 3);
        assert!(s.get_c().iter().all(|&c| c == WideVec3i::new(1, 2, 3)));

        s.truncate(1);
        assert_eq!(s.get_c().len(), 1);
    }
    test_resize();

    fn test_mutability() {
        let a = Vec2i::new(0, 0);
        let b = Vec3i::new(0, 0, 0);
        let mut s = S::new(a, b);

        s.extend_with(3, |_| WideVec3i::new(0, 0, 0));

        let c_slice = s.get_mut_c();
        c_slice[0] = WideVec3i::new(1, 2, 3);
        c_slice[1] = WideVec3i::new(4, 5, 6);
        c_slice[2] = WideVec3i::new(7, 8, 9);

        assert_eq!(s.get_c()[0], WideVec3i::new(1, 2, 3));
        assert_eq!(s.get_c()[1], WideVec3i::new(4, 5, 6));
        assert_eq!(s.get_c()[2], WideVec3i::new(7, 8, 9));
    }
    test_mutability();

    fn test_as_bytes() {
        #[repr(C)]
        struct Raw {
            a: Vec2i,
            _pad1: [u8; 8],
            b: Vec3i,
            _pad2: [u8; 4],
            c: [(Vec3i, /* pad */ [u8; 4]); 3],
        }

        let a = Vec2i::new(10, 20);
        let b = Vec3i::new(30, 40, 50);
        let mut s = S::new(a, b);

        s.extend_with(3, |i| WideVec3i::new(i as i32, i as i32, i as i32));

        let raw = Raw {
            a: Vec2i::new(10, 20),
            _pad1: [0; 8],
            b: Vec3i::new(30, 40, 50),
            _pad2: [0; 4],
            c: [
                (Vec3i::new(0, 0, 0), [0; 4]),
                (Vec3i::new(1, 1, 1), [0; 4]),
                (Vec3i::new(2, 2, 2), [0; 4]),
            ],
        };

        let s_bytes: &[u8] = s.as_bytes();
        let raw_ptr = &raw as *const Raw as *const u8;
        let raw_len = size_of::<Raw>();
        let raw_bytes: &[u8] = unsafe { std::slice::from_raw_parts(raw_ptr, raw_len) };

        let a_start = std::mem::offset_of!(Raw, a);
        let a_end = a_start + size_of::<Vec2i>();

        let b_start = std::mem::offset_of!(Raw, b);
        let b_end = b_start + size_of::<Vec3i>();

        let c0_start = std::mem::offset_of!(Raw, c);
        let c0_end = c0_start + size_of::<Vec3i>();
        let c1_start = c0_end + 4; // due to the pad.
        let c1_end = c1_start + size_of::<Vec3i>();
        let c2_start = c1_end + 4; // due to the pad.
        let c2_end = c2_start + size_of::<Vec3i>();

        assert_eq!(s_bytes.len(), raw_bytes.len());
        assert_eq!(s_bytes[a_start..a_end], raw_bytes[a_start..a_end]);
        assert_eq!(s_bytes[b_start..b_end], raw_bytes[b_start..b_end]);
        assert_eq!(s_bytes[c0_start..c0_end], raw_bytes[c0_start..c0_end]);
        assert_eq!(s_bytes[c1_start..c1_end], raw_bytes[c1_start..c1_end]);
        assert_eq!(s_bytes[c2_start..c2_end], raw_bytes[c2_start..c2_end]);
    }
    test_as_bytes();
}
