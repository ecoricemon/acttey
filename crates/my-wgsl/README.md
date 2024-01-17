# my-wgsl

A WGSL code generator from Rust.   

This is a simple string generator for WGSL.  
You can construct your WGSL code like below,  

```rust
let mut builder = Builder::new();

// Appends a struct.
#[my_wgsl_decl_struct]
struct VertexInput {...}
wgsl_structs!(builder, VertexInput);

// Appends a binding.
wgsl_bind!(builder, group(0) binding(0) var<storage> ...);

// Appends a function.
wgsl_fn!(builder, fn foo(...) {...});
```

Almost every field is public and deriving Debug for now.
You can manipulate almost everything with a kind of complicate access for now.
