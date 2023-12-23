# erased-generic-trait
A Rust crate for generating trait objects from traits with generic methods.

## When to use

- When you want to make a trait obejct from a trait having generic methods.
- When generic methods have only one kind of bounds.
- When generic methods require 'static lifetime.
- When you can know all types that are passed to generic methods before converting into a trait object.

## Motivation

In Rust, only object safe traits can become trait objects, generic methods make them not object safe.
To overcome this limitation, we can use `dyn Any` as parameters to take generic arguments
from non-generic methods, and call the generic methods in them.
We can inspect `TypeId`s from the 'dyn Ayn's, but we can't know concrete types from the 'TypeId's.
So macros are going to inject functions calling generic methods with the concrete types,
and invoke those functions according to the `TypeId`s.
But you should know this approach is not efficient.

---

[Please read rust documentation of this crate for more details.](https://docs.rs/erased-generic-trait/)
