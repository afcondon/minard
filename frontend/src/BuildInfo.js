// Build-time constants for Minard frontend.
// Both values are overwritten by Makefile targets (_stamp-build, _stamp-build-static).
// isStaticDeploy MUST be false in source control. Use `make bundle-static` for
// GH Pages / minard.app builds — it temporarily sets true, bundles, then resets.
export const buildStamp = "2026-04-03 09:40";
export const isStaticDeploy = false;
