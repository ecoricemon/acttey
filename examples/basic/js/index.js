if (navigator.gpu) {
  // Loads wasm built on WebGPU.
  const { MyApp } = await import("../pkg_gpu/wasm-index.js");
  const app = new MyApp();
  await run(app);
} else {
  // TODO: Uncomment after make a system for multi canvas in WebGL.
  // Loads wasm built on WebGL.
  // const { MyApp } = await import("../pkg_gl/wasm-index.js");
  // const app = new MyApp();
  // await run(app);
}

async function run(app) {
  await app.run();
}

export const A = 0; // I'm a module..., I wanna use await at the top level.
