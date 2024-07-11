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
