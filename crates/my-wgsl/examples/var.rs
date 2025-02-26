use my_wgsl::*;

fn main() {
    const_declaration();
    extern_const();
}

/// This example shows how to declare constants.
fn const_declaration() {
    #[wgsl_mod]
    mod m {
        use my_wgsl::*;

        // Rust usize -> WGSL AbstractInt
        const A: usize = 1;
        const B: usize = A + 1;

        // Rust i32 -> WGSL i32
        const C: i32 = 1;

        // Rust u32 -> WGSL u32
        const D: u32 = 1;

        // Rust f32 -> WGSL f32
        const E: f32 = 1.;

        // Rust f64 -> WGSL AbstractFloat
        const F: f64 = 1.;

        // Rust Vec -> WGSL vec
        const G: Vec2i = Vec2i::new(1, 2);
        const H: Vec2i = Vec2i::splat(3);
        const I: Vec2i = Vec2i::ZERO;

        // Rust Mat -> WGSL mat
        const J: Mat2x2f = Mat2x2f::new(Vec2f::new(0., 1.), Vec2f::new(2., 3.));
        const K: Mat2x2f = Mat2x2f::ZERO;
        const L: Mat2x2f = Mat2x2f::IDENTITY;

        // usize for array length
        struct Foo {
            a: [f32; B],
        }
    }

    println!("=== const declaration ===");
    println!("{}", m::Module::WGSL);
}

/// This example shows how to declare extern constants.
fn extern_const() {
    const A: &str = a::Module::WGSL;
    const B: &str = b::Module::WGSL;
    const MERGED: &str = const_format::concatcp!(A, B);

    println!("=== extern constants ===");
    println!("{MERGED}"); // struct A {..} struct B {..}
}

#[wgsl_mod]
mod a {
    pub const A: usize = 1;
}

#[wgsl_mod]
mod b {
    use super::a::A;
    use my_wgsl::*;

    // Extern constant `A` is 1. This macro shows error when `A` is not 1.
    extern_const!(A, 1);

    const B: usize = A + 1;
}
