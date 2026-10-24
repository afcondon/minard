// Where the timeline data comes from, set by the embedding page like the map's own
// dataUrl (see Component.js): window.MM_CONFIG = { timelineUrl: "timeline-public.json" }.
export const readTimelineUrl = () => {
  const c = (typeof window !== "undefined" && window.MM_CONFIG) || {};
  return c.timelineUrl || "timeline-public.json";
};
