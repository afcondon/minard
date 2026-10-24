// Open a target (project tracker page or GitHub repo) in a new browser tab on
// modifier-click, so a gap spotted in the coverage map can be acted on at the source.
export const openInNewTabImpl = (url) => () => {
  window.open(url, "_blank", "noopener,noreferrer");
};

// Link policy is set by the EMBEDDING PAGE, not the data — the same bundle.js serves
// both the public showcase (links to GitHub) and the in-house newspaper (links to the
// project tracker). The host sets `window.MM_CONFIG` before loading the bundle:
//   { links: "github" | "tracker" | "none", trackerBase: "<origin>" }
// Defaults below keep the standalone dev tool pointing at the tailnet tracker.
export const readLinkConfig = () => {
  const c = (typeof window !== "undefined" && window.MM_CONFIG) || {};
  return {
    mode: c.links || "tracker",
    trackerBase: (c.trackerBase != null ? c.trackerBase : "http://andrews-mac-mini:3101"),
    // Where the tree data comes from: a baked static file (website) or a live endpoint
    // (the in-house newspaper fetches /api/tree). Same bundle, two data sources.
    dataUrl: c.dataUrl || "tree.json",
  };
};
