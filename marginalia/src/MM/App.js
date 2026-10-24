// Where the timeline data comes from, set by the embedding page like the map's own
// dataUrl (see Component.js): window.MM_CONFIG = { timelineUrl: "timeline-public.json" }.
export const readTimelineUrl = () => {
  const c = (typeof window !== "undefined" && window.MM_CONFIG) || {};
  return c.timelineUrl || "timeline-public.json";
};

// The Git view's data: window.MM_CONFIG = { gitUrl: "git-activity.json" }.
export const readGitUrl = () => {
  const c = (typeof window !== "undefined" && window.MM_CONFIG) || {};
  return c.gitUrl || "git-activity.json";
};
