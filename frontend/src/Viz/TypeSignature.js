// FFI for CE2.Viz.TypeSignature

export const renderIntoFFI = (containerId) => (declName) => (sig) => (parseResult) => () => {
  const container = document.getElementById(containerId);
  if (!container) return;
  container.innerHTML = '';

  if (parseResult.ok) {
    const svg = TypeSigRenderer.renderSignature(declName, sig, parseResult.ast, {});
    container.appendChild(svg);
  } else {
    // Fallback: plain text
    const pre = document.createElement('pre');
    pre.textContent = declName + ' :: ' + sig;
    pre.style.cssText = 'margin:0; font-size:13px; color:#0E4C8A; font-family: var(--font-mono);';
    container.appendChild(pre);
  }
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
