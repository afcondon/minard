// When served behind the edge router at /code/, route API through it.
// Otherwise (local dev), hit the backend directly.
export const apiBaseUrl = window.location.pathname.startsWith("/code")
  ? "/code"
  : "http://localhost:3000";
