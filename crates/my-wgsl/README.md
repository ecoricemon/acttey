# my-wgsl

my-wgsl is a WGSL code generation library.

## When to use

- When you want to compose multiple WGSL pieces into one WGSL text. You can
  generate shader module from it later.
- When you want to modify WGSL piece such as insertion or deletion dynamically.

## Example

```rust
// ref: https://www.w3.org/TR/WGSL/
use my_wgsl::*;

#[wgsl_struct]
struct PointLight {
    position: vec3f,
    color: vec3f,
}

#[wgsl_struct]
struct LightStorage {
    pointCount: u32,
    point: array<PointLight>,
}

let mut builder = WgslBuilder::new();

builder.push_struct_of::<PointLight>();
builder.push_struct_of::<LightStorage>();

builder.push_global_variable(
    wgsl_global_var!(group(0) binding(0) var<storage> lights : LightStorage)
);
builder.push_global_variable(
    wgsl_global_var!(group(1) binding(0) var baseColorSampler : sampler)
);
builder.push_global_variable(
    wgsl_global_var!(group(1) binding(1) var baseColorTexture : texture_2d<f32>)
);

let f = wgsl_fn!(
    #[fragment]
    fn fragmentMain(#[location(0)] worldPos : vec3f,
                    #[location(1)] normal : vec3f,
                    #[location(2)] uv : vec2f) -> #[location(0)] vec4f {
        // Sample the base color of the surface from a texture.
        let baseColor = textureSample(baseColorTexture, baseColorSampler, uv);

        let N = normalize(normal);
        var surfaceColor = vec3f(0);

        // Loop over the scene point lights.
        for (var i = 0u; i < lights.pointCount; ++i) {
            let worldToLight = lights.point[i].position - worldPos;
            let dist = length(worldToLight);
            let dir = normalize(worldToLight);

            // Determine the contribution of this light to the surface color.
            let radiance = lights.point[i].color * (1 / pow(dist, 2));
            let nDotL = max(dot(N, dir), 0);

            // Accumulate light contribution to the surface color.
            surfaceColor += baseColor.rgb * radiance * nDotL;
        }

        // Return the accumulated surface color.
        return vec4(surfaceColor, baseColor.a);
        
    }
);
builder.push_function(f);

let wgsl: String = builder.build();
```
