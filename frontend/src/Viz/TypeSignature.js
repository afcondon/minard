// FFI for CE2.Viz.TypeSignature

// Insert element into container by ID, clearing existing content
export const replaceContainerContent = (containerId) => (element) => () => {
  const container = document.getElementById(containerId);
  if (!container) return;
  container.innerHTML = '';
  container.appendChild(element);
};

// Show fallback text in a container
export const showFallbackText = (containerId) => (text) => () => {
  const container = document.getElementById(containerId);
  if (!container) return;
  container.innerHTML = '';
  const pre = document.createElement('pre');
  pre.textContent = text;
  pre.style.cssText = 'margin:0; font-size:13px; color:#0E4C8A; font-family: var(--font-mono);';
  container.appendChild(pre);
};

// Insert SVG into cell div
export const insertSVGIntoCell = (cellId) => (svgEl) => (_cellWidth) => (_cellPad) => () => {
  const div = document.getElementById(cellId);
  if (!div) return;
  svgEl.style.display = 'block';
  svgEl.style.flexShrink = '0';
  div.appendChild(svgEl);
};

// --- Sparkline DOM helpers ---

export const querySvgInContainer = (selector) => () => {
  return document.querySelector(selector + ' svg');
};

export const removeOldSparklines = (svg) => () => {
  const old = svg.querySelector('.sparkline-group');
  if (old) old.remove();
};

export const createSparklineGroup = () => {
  const g = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  g.setAttribute('class', 'sparkline-group');
  g.setAttribute('pointer-events', 'none');
  return g;
};

export const setSvgAttr = (el) => (key) => (value) => () => {
  el.setAttribute(key, value);
};

export const appendSvgChild = (parent) => (child) => () => {
  parent.appendChild(child);
};
