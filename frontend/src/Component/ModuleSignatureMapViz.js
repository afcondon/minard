// FFI for CE2.Component.ModuleSignatureMapViz

export const openUri = (uri) => () => {
  // Use a temporary link click for protocol handlers (vscode://)
  // window.open is blocked by popup blockers for non-http URIs
  const a = document.createElement('a');
  a.href = uri;
  a.click();
};
