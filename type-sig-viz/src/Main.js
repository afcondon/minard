// FFI for TypeSigViz.Main

export const exportParser = (parseFn) => () => {
  window.TypeSigViz = window.TypeSigViz || {};
  window.TypeSigViz.parse = parseFn;
};

export const triggerInit = () => {
  if (typeof window.__typeSigVizInit === 'function') {
    window.__typeSigVizInit();
  }
};
