// Canvas 2D FFI for CE2.Viz.CoChangeCube
// Uncurried EffectFn style — all projection and math stays in PureScript.

export const getContext2DImpl = (canvas) => {
  return canvas.getContext('2d');
};

export const clearRectImpl = (ctx, x, y, w, h) => {
  ctx.clearRect(x, y, w, h);
};

export const setFillStyleImpl = (ctx, style) => {
  ctx.fillStyle = style;
};

export const fillRectImpl = (ctx, x, y, w, h) => {
  ctx.fillRect(x, y, w, h);
};

export const beginPathImpl = (ctx) => {
  ctx.beginPath();
};

export const moveToImpl = (ctx, x, y) => {
  ctx.moveTo(x, y);
};

export const lineToImpl = (ctx, x, y) => {
  ctx.lineTo(x, y);
};

export const strokeImpl = (ctx) => {
  ctx.stroke();
};

export const setStrokeStyleImpl = (ctx, style) => {
  ctx.strokeStyle = style;
};

export const setLineWidthImpl = (ctx, width) => {
  ctx.lineWidth = width;
};

export const setGlobalAlphaImpl = (ctx, alpha) => {
  ctx.globalAlpha = alpha;
};

export const fillCircleImpl = (ctx, x, y, r) => {
  ctx.beginPath();
  ctx.arc(x, y, r, 0, Math.PI * 2);
  ctx.fill();
};

export const strokeRectImpl = (ctx, x, y, w, h) => {
  ctx.strokeRect(x, y, w, h);
};

export const fillTextImpl = (ctx, text, x, y) => {
  ctx.fillText(text, x, y);
};

export const setFontImpl = (ctx, font) => {
  ctx.font = font;
};

export const setTextAlignImpl = (ctx, align) => {
  ctx.textAlign = align;
};

export const setTextBaselineImpl = (ctx, baseline) => {
  ctx.textBaseline = baseline;
};
