// TODO: Minify and Remove check code at compile-time.
const eventBuf = [];

onmessage = async ev => {
  if (typeof ev.data === 'object' && Reflect.has(ev.data, 'module')) {
    // Imports wasm glue module.
    const { module, memory, url, wasmInit, init, listen } = ev.data;
    const wasmGlue = await import(new URL(url));

    // Initializes wasm with the same module and memory.
    // We use shared memory here.
    // To do that, we inserted '--target web' in our build command.
    if (wasmGlue[wasmInit] === undefined) {
      throw new Error('failed to find "' + wasmInit + '" from ' + url);
    }
    // `thread_stack_size` will be applied as default.
    // If we need `thread_stack_size` in the future, it must be divided by
    // '65536'. See generated glue JS file.
    await wasmGlue[wasmInit]({module_or_path: module, memory});

    // Initialize.
    if (wasmGlue[init] === undefined) {
      throw new Error('failed to find "' + init + '" from ' + url);
    }
    wasmGlue[init]();

    // Notifies ready.
    postMessage(undefined);

    // Consumes stacked events.
    const handle = wasmGlue[listen];
    if (handle === undefined) {
      throw new Error('failed to find "' + listen + '" from ' + url);
    }
    while (eventBuf.length > 0) {
      const ev = eventBuf.shift();
      await handle(ev.data);
    }

    // Run
    onmessage = async ev => await handle(ev.data);
  } else {
    // Holds events before we initialize wasm.
    eventBuf.push(ev);
  }
}
