// Initialize wasm.
import * as wasm from "./pkg/wasm-index.js";
await wasm.default();

// === Buttons ===

document.getElementById("btnSingle").addEventListener("click", () => draw(1));
document.getElementById("btnMulti").addEventListener("click", () => draw(0));
document.getElementById("btnGpu").addEventListener("click", () => draw(-1));
document.getElementById("btnStop").addEventListener("click", stop);

function enableDrawButtons() {
  document.getElementById("btnSingle").disabled = false;
  document.getElementById("btnMulti").disabled = false;
  document.getElementById("btnGpu").disabled = false;
  document.getElementById("btnStop").disabled = true;
}

function disableDrawButtons() {
  document.getElementById("btnSingle").disabled = true;
  document.getElementById("btnMulti").disabled = true;
  document.getElementById("btnGpu").disabled = true;
  document.getElementById("btnStop").disabled = false;
}

enableDrawButtons();

// === Canvas ===

// We're going to draw fractal images onto this fixed size canvas not to
// bring difference caused by real canvas size.
const baseCanvas = document.createElement("canvas");
baseCanvas.width = 500;
baseCanvas.height = 500;
const baseCtx = baseCanvas.getContext("2d");

// This is real canvas we will see on the screen.
const canvas = document.getElementById("canvas");
const ctx = canvas.getContext("2d");

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

let start = performance.now();
let frames = 0;
let timer = undefined;

function resetMeasure() {
  start = performance.now();
  frames = 0;
  timer = setInterval(() => {
    const elapsed = performance.now() - start;
    const x = (frames / elapsed) * 1000;
    const fps = Math.round(x * 10) / 10;
    document.getElementById("fps").innerHTML = `${fps} frames/sec`;
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

function draw(numWorkers) {
  if (!run) {
    resetTargetArea();
    disableDrawButtons();
    resetMeasure();
    run = true;
    createApp(numWorkers);
    requestCalculation();
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

function createApp(numWorkers) {
  app = new wasm.App(numWorkers);

  app.setOnMessage(() => {
    drawImage();
    frames += 1;

    if (run && width > 0.0001) {
      zoomInTargetArea();
      requestCalculation();
    } else {
      stop();
    }
  });
}

// Function to request fractal image calculation to wasm.
function requestCalculation() {
  app.calcImage(
    baseCanvas.width,
    baseCanvas.height,
    x - width / 2,
    x + width / 2,
    y - height / 2,
    y + height / 2
  );
}

// Function to draw image using data gotten from wasm with scaling.
function drawImage() {
  canvas.width = canvas.clientWidth;
  canvas.height = canvas.clientHeight;

  app.getResult(buf);
  baseCtx.putImageData(imageData, 0, 0);
  ctx.drawImage(
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
