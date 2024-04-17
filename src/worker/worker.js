// Imports wasm statically.
// Reasonable because worker can't do anything without wasm.
// And worker's loading can still be determined from external dynamically.
import * as wasm from '../../../..';

export function attachWorker() {}

const eventBuf = [];
const stages = [initWasm, initMain, operateMain, operateSub];
let cur = 0;
let id = 0;

onmessage = async (event) => {
  eventBuf.push(event);
  if (stages[cur] !== null) {
    while (eventBuf.length > 0) {
      let f = stages[cur];
      stages[cur] = null;
      await f();
      if (id === 0) { // Only main-worker has id zero.
        cur += 1;
      } else { // Sub-workers have ids greater than zero.
        cur = 3;
      }
    }
  }
}

async function initWasm() {
  const { default: wbgInit } = wasm;
  const event = eventBuf.shift();
  id = event.data[0];
  if (event.data.length == 2) {
    await wbgInit(event.data[1]); // Non-shared memory
  } else {
    await wbgInit(event.data[1], event.data[2]); // Shared memory
  }

  postMessage(undefined); // Notifies ready.
}

async function initMain() {
  await wasm.mainOnMessageInit(eventBuf.shift());
}

async function operateMain() {
  while (eventBuf.length > 0) {
    wasm.mainOnMessage(eventBuf.shift(), id);
  }
  onmessage = (event) => {
    wasm.mainOnMessage(event, id);
  }
}

async function operateSub() {
  while (eventBuf.length > 0) {
    let event = eventBuf.shift();
    wasm.subOnMessage(event.data, id);
  }
  onmessage = (event) => {
    wasm.subOnMessage(event.data, id);
  }
}
