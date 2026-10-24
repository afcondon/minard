#!/usr/bin/env bash
# Stage a complete local preview of andrewcondon.com/map (map + timeline) in
# marginalia/preview/, laid out as the site is (/map/...). Reads the deployed map's
# tree.json and covers from cloudflare-sites without writing there. Never deploys.
#
#   marginalia/stage-preview.sh            # build public data, bundle, stage
#   python3 -m http.server 8765 -d marginalia/preview
#   open http://localhost:8765/map/        # or /map/#timeline, /map/#line
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
SITE="${SITE:-$HERE/../../../cloudflare-sites/andrewcondon/map}"
OUT="$HERE/preview/map"

node "$HERE/build-public-timeline.mjs" "$@"
# The Git view's totals, from the per-machine exports (git-activity/collect.sh), if present.
if ls "$HERE"/git-activity/private/*.tsv >/dev/null 2>&1; then node "$HERE/build-git-activity.mjs"; fi
(cd "$HERE/.." && spago bundle -p minard-marginalia)

rm -rf "$HERE/preview"
mkdir -p "$OUT"
cp "$HERE/site/index.html" "$HERE/public/styles.css" "$HERE/public/bundle.js" "$HERE/public/timeline-public.json" "$OUT/"
[ -f "$HERE/public/git-activity.json" ] && cp "$HERE/public/git-activity.json" "$OUT/"
cp "$SITE/tree.json" "$OUT/tree.json"
cp -R "$SITE/covers" "$OUT/covers"
# Andrew's own images for Line-view notes (overrides.json "image"), if any.
[ -d "$HERE/images" ] && cp -R "$HERE/images" "$OUT/images"
cat > "$HERE/preview/index.html" <<'HTML'
<!DOCTYPE html><meta charset="utf-8"><meta http-equiv="refresh" content="0; url=map/"><a href="map/">map/</a>
HTML
echo "Staged $HERE/preview (serve with: python3 -m http.server 8765 -d $HERE/preview)"
