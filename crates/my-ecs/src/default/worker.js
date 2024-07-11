const eventBuf = [];

onmessage = async ev => {
  if (typeof ev.data === 'object' && Reflect.has(ev.data, 'module')) {
    // Imports wasm glue module.
    const { module, memory, import_url, init_method } = ev.data;
    const wasm_glue = await import(new URL(import_url));

    // Initializes wasm with the same module and memory.
    // We use shared memory here.
    // To do that, we inserted '--target web' in our build command.
    const init = wasm_glue[init_method];
    if (init === undefined) {
      throw new Error('not found "' + init_method + '" from ' + import_url);
    }
    const wasm = await init(module, memory);
    postMessage(undefined); // Notifies ready.

    // Consumes stacked events.
    while (eventBuf.length > 0) {
      let ev = eventBuf.shift();
      wasm.workerOnMessage(ev.data);
    }

    // Run
    onmessage = ev => wasm.workerOnMessage(ev.data);
  } else {
    // Holds events before we initialize wasm.
    eventBuf.push(ev);
  }
}
