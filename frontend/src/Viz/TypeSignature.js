// FFI for CE2.Viz.TypeSignature

export const renderIntoFFI = (containerId) => (declName) => (sig) => (parseResult) => (typeParams) => () => {
  const container = document.getElementById(containerId);
  if (!container) return;
  container.innerHTML = '';

  if (parseResult.ok) {
    const options = typeParams.length > 0 ? { typeParams } : {};
    const svg = TypeSigRenderer.renderSignature(declName, sig, parseResult.ast, options);
    container.appendChild(svg);
  } else {
    // Fallback: plain text
    const pre = document.createElement('pre');
    pre.textContent = declName + ' :: ' + sig;
    pre.style.cssText = 'margin:0; font-size:13px; color:#0E4C8A; font-family: var(--font-mono);';
    container.appendChild(pre);
  }
};

// --- Element-returning wrappers for ModuleSignatureMap ---

export const renderSignatureElement = (name) => (sig) => (parseResult) => () => {
  if (!parseResult.ok) return null;
  try {
    return TypeSigRenderer.renderSignature(name, sig, parseResult.ast, {});
  } catch (e) { return null; }
};

export const renderSignatureWithParamsElement = (name) => (sig) => (parseResult) => (typeParams) => () => {
  if (!parseResult.ok) return null;
  try {
    return TypeSigRenderer.renderSignature(name, sig, parseResult.ast, { typeParams });
  } catch (e) { return null; }
};

export const renderADTElement = (name) => (typeParams) => (constructors) => (parseFn) => () => {
  try {
    return TypeSigRenderer.renderADT(name, typeParams, constructors, parseFn);
  } catch (e) { return null; }
};

export const renderClassDefElement = (name) => (typeParams) => (superclasses) => (methods) => (parseFn) => () => {
  try {
    return TypeSigRenderer.renderClassDef(name, typeParams, superclasses, methods, parseFn);
  } catch (e) { return null; }
};

export const measureSVGElement = (el) => () => {
  // Try attribute-based measurement first
  let w = parseFloat(el.getAttribute('width')) || 0;
  let h = parseFloat(el.getAttribute('height')) || 0;

  // If attributes are missing or suspiciously small, use getBBox via temp DOM mount
  if (w < 10 || h < 10) {
    const offscreen = document.createElement('div');
    offscreen.style.cssText = 'position:absolute;left:-9999px;top:-9999px;visibility:hidden;';
    document.body.appendChild(offscreen);
    offscreen.appendChild(el);
    try {
      const bbox = el.getBBox();
      w = Math.max(w, bbox.width);
      h = Math.max(h, bbox.height);
    } catch (_) {}
    offscreen.removeChild(el);
    document.body.removeChild(offscreen);
  }

  return { width: w, height: h };
};

export const insertSVGIntoCell = (cellId) => (svgEl) => (_cellWidth) => (_cellPad) => () => {
  const div = document.getElementById(cellId);
  if (!div) return;
  // Normal flow — SVG is a block child inside the padded, scrollable cell
  svgEl.style.display = 'block';
  svgEl.style.flexShrink = '0';
  div.appendChild(svgEl);
};

export const injectSparklines = (containerSelector) => (cells) => () => {
  const svg = document.querySelector(containerSelector + ' svg');
  if (!svg) return;

  // Remove previous sparklines
  const old = svg.querySelector('.sparkline-group');
  if (old) old.remove();

  const group = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  group.setAttribute('class', 'sparkline-group');
  group.setAttribute('pointer-events', 'none');

  const MIN_W = 60, MIN_H = 40;
  const PAD = 4, STRIP_H = 3, LABEL_H = 14;

  for (const cell of cells) {
    if (cell.width < MIN_W || cell.height < MIN_H) continue;
    if (!cell.ast.ok) continue;

    const availW = cell.width - PAD * 2;
    const availH = cell.height - STRIP_H - LABEL_H - PAD;

    const result = TypeSigRenderer.renderSparkline(cell.ast.ast, availW, availH);
    if (!result) continue;

    // Center within available area
    const offsetX = cell.x + PAD + (availW - result.scaledWidth) / 2;
    const offsetY = cell.y + STRIP_H + PAD + (availH - result.scaledHeight) / 2;

    result.element.setAttribute('transform', `translate(${offsetX}, ${offsetY})`);
    group.appendChild(result.element);
  }

  svg.appendChild(group);
};
