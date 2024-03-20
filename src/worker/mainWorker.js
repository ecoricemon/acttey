// Imports wasm statically.
// Reasonable because worker can't do anything without wasm.
// And worker's loading can still be determined from external dynamically.
import * as wasm from '../../../..';

export function attachMain() {}

const eventBuf = [];
const stages = [initWasm, initMain, operate];
let cur = 0;
let id;

onmessage = async (event) => {
  eventBuf.push(event);
  if (stages[cur] !== null) {
    while (eventBuf.length > 0) {
      let f = stages[cur];
      stages[cur] = null;
      await f();
      cur += 1;
    }
  }
}

async function initWasm() {
  const { default: wbg_init } = wasm;
  const event = eventBuf.shift();
  id = event.data[1];
  await wbg_init(event.data[0]);
}

async function initMain() {
  await wasm.main_onmessage_init(eventBuf.shift());
}

async function operate() {
  while (eventBuf.length > 0) {
    wasm.main_onmessage(eventBuf.shift(), id);
  }
  onmessage = (event) => {
    wasm.main_onmessage(event, id);
  }
}
