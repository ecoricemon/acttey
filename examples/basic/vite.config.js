import { defineConfig } from 'vite';
import wasm from "vite-plugin-wasm";
import topLevelAwait from "vite-plugin-top-level-await";

export default defineConfig({
  build: {
    rollupOptions: {
      input: {
        app: 'static/index.html',
      },
      output: {
        // Worker will import wasm glue JS file to initialize wasm.
        // But the worker can't access any other resources such as document.
        // So we need to split the wasm glue from any other chunks.
        manualChunks: {
          'wasm-index': ['pkg_mt/wasm-index']
        }
      }
    },
    // Relative to 'root'.
    outDir: '../dist',
  },
  // For getting out of index.html from dist/static directory.
  root: 'static',
  plugins: [
    // Makes us be able to use top level await for wasm.
    // Otherwise, we can restrict build.target to 'es2022', which allows top level await.
    wasm(),
    topLevelAwait(),
  ],
  server: {
    port: 8080,
  },
  preview: {
    port: 8080,
    // In multi-threaded environment, we need to share wasm memory.
    // These headers are required to share the memory.
    headers: {
      'Cross-Origin-Opener-Policy': 'same-origin',
      'Cross-Origin-Embedder-Policy': 'require-corp'
    }
  },
});
