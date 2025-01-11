import { defineConfig } from 'vite';
import wasm from "vite-plugin-wasm";
import topLevelAwait from "vite-plugin-top-level-await";
import basicSsl from '@vitejs/plugin-basic-ssl';

export default defineConfig({
  build: {
    rollupOptions: {
      // We need to split wasm glue module into a sperate chunk
      // 1. Not to include window context data.
      //    * wasm glue module will be imported in worker context.
      //    * In worker context, we can't access such as document.
      // 2. To preserve indirectly used exports.
      //    * Rollup doesn't know that we're going to access some exported
      //      objects in wasm code.
      //    * So we need to make Rollup not to drop those objects.
      // 
      // First of all, we need to make a new entry point for wasm.
      input: {
        app: 'index.html',
        wasm: 'pkg/wasm-index.js',
      },

      // Without this, Rollup still will discard indirectly used exports.
      // * https://rollupjs.org/configuration-options/#preserveentrysignatures
      //   Although Rollup says default is already 'exports-only', 
      //   but I guess Vite 5.4.2 changes it to `false`.
      preserveEntrySignatures: 'exports-only',
    },
    // Path is relative to 'root'.
    outDir: 'dist',
  },
  plugins: [
    // Makes us be able to use top level await for wasm. Otherwise, we can
    // restrict build.target to 'es2022', which allows top level await.
    wasm(),
    topLevelAwait(),
    basicSsl({
      name: 'test',
      domains: ['*'],
      certDir: './cert'
    }),
  ],
  preview: {
    host: '0.0.0.0',
    port: 8443,
    // In multi-threaded environment, we need to share wasm memory. These
    // headers are required to share the memory.
    headers: {
      'Cross-Origin-Opener-Policy': 'same-origin',
      'Cross-Origin-Embedder-Policy': 'require-corp'
    }
  },
});
