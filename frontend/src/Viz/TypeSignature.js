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
