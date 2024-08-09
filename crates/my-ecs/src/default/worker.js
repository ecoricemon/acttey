const eventBuf = [];

onmessage = async ev => {
  if (typeof ev.data === 'object' && Reflect.has(ev.data, 'module')) {
    // Imports wasm glue module.
    const { module, memory, import_url, init_fn, listen_fn } = ev.data;
    const wasm_glue = await import(new URL(import_url));

    // Initializes wasm with the same module and memory.
    // We use shared memory here.
    // To do that, we inserted '--target web' in our build command.
    const init = wasm_glue[init_fn];
    if (init === undefined) {
      throw new Error('unknown "' + init_method + '" from ' + import_url);
    }
    await init(module, memory);

    // Notifies ready.
    postMessage(undefined);

    // Consumes stacked events.
    const listen = wasm_glue[listen_fn];
    while (eventBuf.length > 0) {
      const ev = eventBuf.shift();
      await listen(ev.data);
    }

    // Run
    onmessage = async ev => {
      await listen(ev.data);
    }
  } else {
    // Holds events before we initialize wasm.
    eventBuf.push(ev);
  }
}
