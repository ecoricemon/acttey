if (navigator.gpu) {
  // Loads wasm built on WebGPU.
  const { default: wbg_init, MyApp } = await import("../pkg_mt/wasm-index.js");
  await wbg_init();
  const myApp = new MyApp();
} else {
  // TODO: Uncomment after make a system for multi canvas in WebGL.
  // Loads wasm built on WebGL.
  // const { MyApp } = await import("../pkg_gl/wasm-index.js");
}
