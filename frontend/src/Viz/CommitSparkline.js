// Canvas 2D FFI for CE2.Viz.CommitSparkline
// Minimal primitives for sparkline rendering.

export const getContext2D = (canvas) => () => {
  return canvas.getContext('2d');
};

export const setFillStyle = (ctx) => (style) => () => {
  ctx.fillStyle = style;
};

export const fillRect = (ctx) => (x) => (y) => (w) => (h) => () => {
  ctx.fillRect(x, y, w, h);
};

// Measure element's rendered width (for responsive canvas sizing)
export const getElementWidth = (element) => () => {
  return element.clientWidth || element.offsetWidth || 0;
};

// Set canvas pixel dimensions (must match CSS size to avoid blur)
export const setCanvasDimensions = (canvas) => (w) => (h) => () => {
  canvas.width = w;
  canvas.height = h;
};
