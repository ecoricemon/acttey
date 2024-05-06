export function createWorker(name) {
  return new Worker(new URL('./worker.js', import.meta.url), {
    type: 'module',
    /* @vite-ignore */ name, // vite doesn't allow non static value here.
  });
}

// NOTE: This code is not related to worker.
//  But keeping this in here, somewhere in JS, helps us not to write complicate Rust code.
//  For now, I don't want to make another JS file for this one.
// Detects change of 'devicePixelRatio'.
// Code below is almost copied from 
// https://developer.mozilla.org/en-US/docs/Web/API/Window/devicePixelRatio.
let remove = null;
export function detectScaleChange() {
  if (remove != null) {
    remove();
  }
  const mqString = `(resolution: ${window.devicePixelRatio}dppx)`;
  const media = matchMedia(mqString);
  media.addEventListener('change', detectScaleChange);
  remove = () => {
    media.removeEventListener('change', detectScaleChange);
  };

  const event = new CustomEvent('scale', { detail: window.devicePixelRatio });
  window.dispatchEvent(event);
};
