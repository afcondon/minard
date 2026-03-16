// FFI for CE2.Component.ModuleStructureViz

export const openUri = (uri) => () => {
  // Use a temporary link click for protocol handlers (vscode://)
  // window.open is blocked by popup blockers for non-http URIs
  const a = document.createElement('a');
  a.href = uri;
  a.click();
};

export const formatRelativeTime = (unixTimestamp) => {
  const now = Math.floor(Date.now() / 1000);
  const diff = now - unixTimestamp;
  if (diff < 60) return 'just now';
  if (diff < 3600) return Math.floor(diff / 60) + ' minutes ago';
  if (diff < 86400) return Math.floor(diff / 3600) + ' hours ago';
  if (diff < 604800) return Math.floor(diff / 86400) + ' days ago';
  if (diff < 2592000) return Math.floor(diff / 604800) + ' weeks ago';
  if (diff < 31536000) return Math.floor(diff / 2592000) + ' months ago';
  return Math.floor(diff / 31536000) + ' years ago';
};
