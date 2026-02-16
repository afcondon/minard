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

// --- Element measurement ---

export const measureElementHeight = (selector) => () => {
  const el = document.querySelector(selector);
  if (!el) return 0;
  return el.scrollHeight;
};

export const clearElement = (selector) => () => {
  const el = document.querySelector(selector);
  if (el) el.innerHTML = '';
};

// --- Siglet tooltip ---

export const showSigletTooltip = (tooltipId) => (sourceId) => (cellId) => () => {
  const tooltip = document.getElementById(tooltipId);
  const source = document.getElementById(sourceId);
  const cell = document.getElementById(cellId);
  if (!tooltip || !source || !cell) return;

  // Clone SVG content into tooltip
  tooltip.innerHTML = '';
  Array.from(source.children).forEach(child => {
    tooltip.appendChild(child.cloneNode(true));
  });

  // Position above the cell
  const rect = cell.getBoundingClientRect();
  tooltip.style.display = 'block';
  tooltip.style.left = rect.left + 'px';
  tooltip.style.bottom = (window.innerHeight - rect.top + 8) + 'px';
  tooltip.style.top = 'auto';

  // Adjust if off-screen
  requestAnimationFrame(() => {
    const tr = tooltip.getBoundingClientRect();
    if (tr.right > window.innerWidth - 8) {
      tooltip.style.left = Math.max(8, window.innerWidth - tr.width - 8) + 'px';
    }
    if (tr.top < 8) {
      tooltip.style.bottom = 'auto';
      tooltip.style.top = (rect.bottom + 8) + 'px';
    }
  });
};

export const hideSigletTooltip = (tooltipId) => () => {
  const tooltip = document.getElementById(tooltipId);
  if (tooltip) {
    tooltip.style.display = 'none';
    tooltip.innerHTML = '';
  }
};
