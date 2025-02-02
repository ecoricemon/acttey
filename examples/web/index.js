// Initialize wasm.
import * as wasm from './pkg/wasm-index.js';
await wasm.default();

const btn = document.getElementById('btnCpu');
const text = btn.innerHTML.replace('N', wasm.numCpus());
btn.innerHTML = text;

// === Buttons ===

const drawBtns = ['btnCpu', 'btnGpu', 'btnCpuGpu'];

drawBtns.forEach((id) => document.getElementById(id).addEventListener('click', () => draw(id)));
document.getElementById('btnStop').addEventListener('click', stop);

function enableDrawButtons() {
  drawBtns.forEach((id) => {
    const btn = document.getElementById(id);
    btn.disabled = false;
    btn.style.color = 'white';
  });
  document.getElementById('btnStop').disabled = true;
}

function disableDrawButtons(id) {
  drawBtns.forEach((id) => document.getElementById(id).disabled = true);
  document.getElementById(id).style.color = '#2069FA';
  document.getElementById('btnStop').disabled = false;
}

enableDrawButtons();

// === Canvas ===

// We're going to draw fractal images onto this fixed size canvas not to
// bring difference caused by real canvas size.
const baseCanvas = document.createElement('canvas');
baseCanvas.width = wasm.canvasWidth();
baseCanvas.height = wasm.canvasHeight();
const baseCx = baseCanvas.getContext('2d');

// This is real canvas we will see on the screen.
const canvas = document.getElementById('canvas');
const cx = canvas.getContext('2d');

// Wasm will fill this buffer with a fractal image.
const buf = new Uint8ClampedArray(baseCanvas.width * baseCanvas.height * 4);
const imageData = new ImageData(buf, baseCanvas.width, baseCanvas.height);

// === Target area ===

// Fractal images will be drawn in complex plane. (width, height) and (x, y)
// are an area and its center in complex plane respectively.
let width = 0;
let height = 0;
let x = 0;
let y = 0;

function resetTargetArea() {
  width = 10.0;
  height = 10.0;
  x = -0.741;
  y = 0.204;
}

function zoomInTargetArea() {
  const zoomRatio = 0.99;
  width *= zoomRatio;
  height *= zoomRatio;
}

// === Measurement ===

let start = undefined;
let frames = 0;
let discarded = 0;
let timer = undefined;

function resetMeasure() {
  start = performance.now();
  frames = 0;
  discarded = 0;
  timer = setInterval(() => {
    const elapsed = performance.now() - start;
    const x = (frames / elapsed) * 1000;
    const fps = Math.round(x * 10) / 10;
    let content = `${fps} fps`;
    if (discarded != 0) {
      content = `${fps} fps (${discarded} discarded)`;
    }
    document.getElementById('fps').innerHTML = content;
  }, 1000);
}

function stopMeasure() {
  if (timer !== undefined) {
    clearInterval(timer);
  }
}

// === Execution ===

let app = undefined;
let run = false;
let age = 0;

function draw(id) {
  if (!run) {
    if ((id == 'btnGpu' || id == 'btnCpuGpu') && !navigator.gpu) {
      window.alert('GPU is not available');
      return;
    }
    resetTargetArea();
    disableDrawButtons(id);
    resetMeasure();
    age = wasm.startAge();
    run = true;
    createApp(id);
    switch (id) {
      case 'btnCpu':
        requestCalculation(true);
        break;
      case 'btnGpu':
        requestCalculation(false);
        break;
      case 'btnCpuGpu':
        requestCalculation(true);
        zoomInTargetArea();
        requestCalculation(false);
        break;
    }
  }
}

function stop() {
  if (run) {
    stopMeasure();
    enableDrawButtons();
    app.destroy();
    run = false;
  }
}

function createApp(ty) {
  app = new wasm.App(ty);

  app.setOnMessage((isCpu) => {
    drawImage();
    frames += 1;

    if (run && width > 0.0001) {
      zoomInTargetArea();
      requestCalculation(isCpu);
    } else {
      stop();
    }
  });
}

// Function to request fractal image calculation to wasm.
function requestCalculation(isCpu) {
  const f = isCpu ? 'calcImageOnCpu' : 'calcImageOnGpu';
  app[f](
    age,
    x - width / 2,
    x + width / 2,
    y - height / 2,
    y + height / 2
  );
  age += 1;
}

// Function to draw image using data gotten from wasm with scaling.
function drawImage() {
  while (true) {
    switch (app.getResult(buf)) {
      case 'ready':
        draw();
        break;
      case 'discarded':
        discarded += 1;
        break;
      case 'none':
        return;
    }
  }

  function draw() {
    baseCx.putImageData(imageData, 0, 0);
    canvas.width = canvas.clientWidth; // this clears canvas
    canvas.height = canvas.clientHeight;
    cx.drawImage(
      baseCanvas,
      0,
      0,
      baseCanvas.width,
      baseCanvas.height,
      0,
      0,
      canvas.width,
      canvas.height
    );
  }
}
