# my-wgsl

my-wgsl is a WGSL code generation library.

## Features

- Provides macros making your structs WGSL compatible at compile time.

## Examples

Defines structs for storage buffer.

```rust
use my_wgsl::{*, builtin::*};

#[wgsl_mod]
mod m {
    use my_wgsl::{*, builtin::*};

    pub struct Object {
        pos: Position,
        tf: Mat2x2f,
        color: Vec3f,
    }

    pub struct Position {
        v: Vec2f,
    }
}

let obj = m::Object::new(
    m::Position::new(Vec2f::ZERO),
    Mat2x2f::IDENTITY,
    Vec3f::splat(1.),
);

// Write to GPU buffer.
let bytes: &[u8] = obj.as_bytes();

// To WGSL code.
let code: String = WgslModule::of::<m::Module>().build();
println!("{code}"); // struct Object { ... } struct Position { ... }
```

You can also define structs for uniform buffer like this.

```rust
use my_wgsl::{*, builtin::*};

#[wgsl_mod]
mod m {
    use my_wgsl::{*, builtin::*};

    #[uniform]
    pub struct Object {
        pos: Position,
        tf: Mat2x2f,
        color: Vec3f,
    }

    // Becomes uniform compatible automatically.
    pub struct Position {
        v: Vec2f,
    }
}
```

WGLS `array<T, N>` and `array<T>` are represented by `[T; N]` and `[T]`
respectively.

```rust
use my_wgsl::{builtin::*, *};

#[wgsl_mod]
mod m {
    use my_wgsl::{builtin::*, *};

    // Array.
    pub struct Sa {
        data: [WideVec3f; 2],
    }

    // Runtime-sized array.
    pub struct Rsa {
        v: Vec2f,
        last: [Element],
    }

    #[derive(PartialEq, Debug)]
    pub struct Element {
        x: Vec3f,
    }
}

let mut rsa = m::Rsa::new(Vec2f::ZERO);

// Set & Get fields except the last one.
rsa.set_v(Vec2f::splat(1.));
assert_eq!(rsa.get_v(), &Vec2f::splat(1.));

// Extends the last runtime-sized array.
rsa.extend_with(2, |index| m::Element::new(Default::default()));

// Set & Get the last runtime-sized array.
let slice: &mut [m::Element] = rsa.get_mut_last();
slice[0] = m::Element::new(Vec3f::splat(1.));
slice[1] = m::Element::new(Vec3f::splat(2.));
assert_eq!(
    rsa.get_last(),
    &[ m::Element::new(Vec3f::splat(1.)),
       m::Element::new(Vec3f::splat(2.)) ]
);
```
