# my-wgsl

A WGSL code generator from Rust.   

This is a simple string generator for WGSL.  
You can construct your WGSL code like below,  

```rust
let mut builder = Builder::new();

// Appends a struct.
#[wgsl_struct]
struct VertexInput {...}
builder.push_struct_of::<VertexInput>();

// Appends a binding.
builder.push_global_variable(
    wgsl_global_var!(group(0) binding(0) var<storage> ...)
);

// Appends a function.
builder.push_function(
    wgsl_fn!(fn foo(...) {...})
);
```

Almost every field is public and deriving Debug for now.
You can manipulate almost everything with a kind of complicate access for now.
