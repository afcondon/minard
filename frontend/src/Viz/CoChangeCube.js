// Canvas 2D FFI for CE2.Viz.CoChangeCube
// Minimal primitives — all projection and math stays in PureScript.

export const getContext2D = (canvas) => () => {
  return canvas.getContext('2d');
};

export const clearRect = (ctx) => (x) => (y) => (w) => (h) => () => {
  ctx.clearRect(x, y, w, h);
};

export const setFillStyle = (ctx) => (style) => () => {
  ctx.fillStyle = style;
};

export const fillRect = (ctx) => (x) => (y) => (w) => (h) => () => {
  ctx.fillRect(x, y, w, h);
};

export const beginPath = (ctx) => () => {
  ctx.beginPath();
};

export const moveTo = (ctx) => (x) => (y) => () => {
  ctx.moveTo(x, y);
};

export const lineTo = (ctx) => (x) => (y) => () => {
  ctx.lineTo(x, y);
};

export const stroke = (ctx) => () => {
  ctx.stroke();
};

export const setStrokeStyle = (ctx) => (style) => () => {
  ctx.strokeStyle = style;
};

export const setLineWidth = (ctx) => (width) => () => {
  ctx.lineWidth = width;
};

export const setGlobalAlpha = (ctx) => (alpha) => () => {
  ctx.globalAlpha = alpha;
};

export const fillCircle = (ctx) => (x) => (y) => (r) => () => {
  ctx.beginPath();
  ctx.arc(x, y, r, 0, Math.PI * 2);
  ctx.fill();
};

export const strokeRect = (ctx) => (x) => (y) => (w) => (h) => () => {
  ctx.strokeRect(x, y, w, h);
};

export const fillText = (ctx) => (text) => (x) => (y) => () => {
  ctx.fillText(text, x, y);
};

export const setFont = (ctx) => (font) => () => {
  ctx.font = font;
};

export const setTextAlign = (ctx) => (align) => () => {
  ctx.textAlign = align;
};

export const setTextBaseline = (ctx) => (baseline) => () => {
  ctx.textBaseline = baseline;
};
