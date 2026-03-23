// Server.Main FFI — environment variable helpers

// Read PORT env var, defaulting to 3000
export const getPortFromEnv = () => {
  const port = parseInt(process.env.PORT || process.env.MINARD_API_PORT || "3000", 10);
  return isNaN(port) ? 3000 : port;
};
