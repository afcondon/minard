// When served behind the edge router at /code/, route API through it.
// Otherwise (local dev), hit the backend directly.
// The API port can be configured via the URL hash param ?api=PORT
// or defaults to 3000.
export const apiBaseUrl = (() => {
  if (window.location.pathname.startsWith("/code")) return "/code";
  // Check for ?api=PORT in the URL (e.g. http://localhost:3001/?api=4000)
  const params = new URLSearchParams(window.location.search);
  const apiPort = params.get("api") || "3000";
  return "http://localhost:" + apiPort;
})();
