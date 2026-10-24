#!/usr/bin/env bash
# Collect Andrew's commits with per-file line counts from every git repo under
# ~/work, for the Git view. Run on each machine; the output is PRIVATE (repo paths,
# subjects, file paths) and is read only by build-git-activity.mjs, which publishes
# daily totals and nothing else.
#
#   git-activity/collect.sh > git-activity/private/<machine>.tsv
#
# Output, one block per commit:
#   @@ <repo> TAB <hash> TAB <author date, ISO> TAB <co-authors> TAB <subject>
#   <added> TAB <deleted> TAB <path>          (git log --numstat; "-" for binary)
set -euo pipefail
SINCE="${SINCE:-2025-10-01}"
AUTHOR="${AUTHOR:-Andrew Condon}"
ROOT="${ROOT:-$HOME/work}"

find "$ROOT" -maxdepth 6 -name .git \
  -not -path '*/node_modules/*' -not -path '*/.spago/*' -not -path '*/GitHub/local-copies/*' \
  -not -path '*/output/*' 2>/dev/null |
while read -r g; do
  repo="$(dirname "$g")"
  rel="${repo#"$ROOT"/}"
  git -C "$repo" -c diff.renameLimit=20000 log --all -M --no-merges --since="$SINCE" --author="$AUTHOR" \
      --numstat --format="@@${rel}%x09%H%x09%aI%x09%(trailers:key=Co-Authored-By,valueonly,separator=;)%x09%s" \
      2>/dev/null || true
done
