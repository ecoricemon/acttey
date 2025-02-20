# my-wgsl

my-wgsl is a WGSL code generation library.

## Features

- Provides macros making your structs WGSL compatible at compile time.

## WGSL compatible structs

WGSL has different layout rules than Rust. This crate inserts padding fields
into structs in order to make them WGSL compatible.

```rust
use my_wgsl::*;

#[wgsl_mod]
mod m {
    use my_wgsl::*;

    // Can be used in storage buffers.
    struct A { a: f32, b: Vec2f }

    // Can be used in storage or uniform buffers.
    #[uniform]
    struct B { a: f32, b: Vec2f }
}
```

## Bytes representation

Structs are required to be represented as `&[u8]` in order to be written in GPU
buffers. This crate provides a method for each struct for the bytes
representation. The function can return `&[u8]` without copy because the structs
have the same layout as WGSL due to padding insertion.

```rust
use my_wgsl::*;

#[wgsl_mod]
mod m {
    use my_wgsl::*;

    pub struct A { a: i32, b: Vec2i }
}

let a = m::A::new(0x01010101, Vec2i::splat(0x02020202));
assert_eq!(&a.as_bytes()[0..4], &[1, 1, 1, 1]);
assert_eq!(&a.as_bytes()[8..16], &[2, 2, 2, 2, 2, 2, 2, 2]);
```

## WGSL arrays

There are array types `array<T, N>` and `array<T>` in WGSL. This crate converts
Rust types `[T; N]` and `[T]` into the WGSL array types respectively. But there
is one constraint. Take a look at an example below.

```rust
use my_wgsl::*;

#[wgsl_mod]
mod m {
    use my_wgsl::*;

    struct A { a: [WideVec2i; 2] } // Vec cannot be used in arrays directly.
    struct B { a: [C; 2] } // Wrapped Vec is Ok.
    struct C { a: Vec2i }
}
```

For the runtime sized arrays, this crate converts them into `Vec<u8>` for zero
copy translation. Instead, this crates provides setter and getter methods for
structs.

```rust
use my_wgsl::*;

#[wgsl_mod]
mod m {
    use my_wgsl::*;

    // b is a runtime sized array.
    pub struct S { a: Vec2i, b: [WideVec2i] }
}

use m::*;
let mut s = S::new(Vec2i::ZERO);

// Setter and getter for S::a
s.set_a(Vec2i::splat(1));
assert_eq!(s.get_a(), &Vec2i::splat(1));

// Setter and getter for S::b
s.extend_with(1, |index| WideVec2i::ZERO);
s.get_mut_b()[0] = WideVec2i::splat(1);
assert_eq!(s.get_b(), &[WideVec2i::splat(1)]);
```

## Import

Structs can be declared across multiple modules and imported from other modules.
But because type layout information cannot cross module boundary, you need to
let the crate know the information like this.

```rust ignore
use my_wgsl::*;

#[wgsl_mod]
mod a {
    use my_wgsl::*;

    pub struct A { a: f32 }
}

#[wgsl_mod]
mod b {
    use my_wgsl::*;
    use super::a::A;

    // A's size and alignment are 4 bytes each. There is compile time validation
    // as well, so that you will notice whenever you make changes to the type.
    layout!(A, 4, 4);

    struct B { a: A }
}
```

## WGSL code at compile time

This crate makes WGSL code as is, which is of type `&'static str`, at compile
time.

```rust ignore
#[wgsl_mod]
mod a {
    use my_wgsl::*;
    pub struct A { a: f32 }
}

#[wgsl_mod]
mod b {
    use my_wgsl::*;
    use super::a::A;

    layout!(A, 4, 4);
    struct B { a: A }
}

// `a::Module` and `b::Module` are generated types by this crate.
const A: &str = a::Module::WGSL; 
const B: &str = b::Module::WGSL;
const MERGED: &str = const_format::concatcp!(A, B);
println!("{MERGED}"); // struct A {..} struct B {..}
```

## WGSL code builder

This crate provides WGSL code builder as well. The builder allows you to inspect
and modify the code at runtime.

```rust ignore
use my_wgsl::*;

#[wgsl_mod]
mod a {
    use my_wgsl::*;
    pub struct A { a: f32 }
}

#[wgsl_mod]
mod b {
    use my_wgsl::*;
    use super::a::A;

    layout!(A, 4, 4);
    struct B { a: A }
}

let builder: WgslModule = (a::Module, b::Module).into();
let code: String = builder.build();
println!("{code}"); // struct A {..} struct B {..}
```
