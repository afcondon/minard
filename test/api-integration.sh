#!/usr/bin/env bash
# =============================================================================
# Minard API Integration Tests
#
# Exercises the REST API against a running server on localhost:3000.
# Requires: curl, python3 (for JSON parsing)
#
# Usage:
#   ./test/api-integration.sh          # Run all tests
#   ./test/api-integration.sh --clean  # Run all tests + delete test data
# =============================================================================

set -euo pipefail

BASE="http://localhost:3000"
PASS=0
FAIL=0
CREATED_IDS=()
CLEAN=false

[[ "${1:-}" == "--clean" ]] && CLEAN=true

# — Helpers —

pass() { PASS=$((PASS + 1)); printf "  \033[32m✓\033[0m %s\n" "$1"; }
fail() { FAIL=$((FAIL + 1)); printf "  \033[31m✗\033[0m %s\n" "$1"; }

assert_eq() {
  local actual="$1" expected="$2" label="$3"
  if [[ "$actual" == "$expected" ]]; then
    pass "$label"
  else
    fail "$label (expected '$expected', got '$actual')"
  fi
}

assert_contains() {
  local haystack="$1" needle="$2" label="$3"
  if echo "$haystack" | grep -qF "$needle"; then
    pass "$label"
  else
    fail "$label (expected to contain '$needle')"
  fi
}

assert_not_empty() {
  local val="$1" label="$2"
  if [[ -n "$val" ]]; then
    pass "$label"
  else
    fail "$label (was empty)"
  fi
}

json_field() {
  python3 -c "import sys,json; d=json.load(sys.stdin); print(d$1)"
}

# — Preflight —

printf "\n\033[1mMinard API Integration Tests\033[0m\n"
printf "Target: %s\n\n" "$BASE"

HEALTH=$(curl -sf "$BASE/health" 2>/dev/null || echo "FAIL")
if [[ "$HEALTH" != "OK" ]]; then
  printf "\033[31mServer not running at %s\033[0m\n" "$BASE"
  exit 1
fi
pass "Server is healthy"

# =============================================================================
# 1. Core endpoints
# =============================================================================

printf "\n\033[1m— Core Endpoints —\033[0m\n"

STATS=$(curl -sf "$BASE/api/v2/stats")
MODULES=$(echo "$STATS" | json_field "['modules']")
assert_not_empty "$MODULES" "GET /api/v2/stats returns module count"

PKG_COUNT=$(curl -sf "$BASE/api/v2/packages" | json_field "['count']")
assert_not_empty "$PKG_COUNT" "GET /api/v2/packages returns count"

MOD_COUNT=$(curl -sf "$BASE/api/v2/modules" | json_field "['count']")
assert_not_empty "$MOD_COUNT" "GET /api/v2/modules returns count"

# =============================================================================
# 2. Annotations CRUD
# =============================================================================

printf "\n\033[1m— Annotations CRUD —\033[0m\n"

# Create a root annotation
ROOT=$(curl -sf -X POST "$BASE/api/v2/annotations" \
  -H 'Content-Type: application/json' \
  -d '{
    "target_type": "module",
    "target_id": "__test_module__",
    "kind": "summary",
    "value": "Test root annotation for integration tests.",
    "source": "ai",
    "confidence": 0.85
  }')
ROOT_ID=$(echo "$ROOT" | json_field "['id']")
ROOT_SUPERSEDES=$(echo "$ROOT" | json_field "['supersedes']")
CREATED_IDS+=("$ROOT_ID")

assert_not_empty "$ROOT_ID" "POST /annotations creates annotation (id=$ROOT_ID)"
assert_eq "$ROOT_SUPERSEDES" "None" "Root annotation has supersedes=null"

ROOT_STATUS=$(echo "$ROOT" | json_field "['status']")
assert_eq "$ROOT_STATUS" "proposed" "New annotation defaults to proposed"

ROOT_SOURCE=$(echo "$ROOT" | json_field "['source']")
assert_eq "$ROOT_SOURCE" "ai" "Source field preserved"

# GET the annotation back
GOT=$(curl -sf "$BASE/api/v2/annotations/$ROOT_ID")
GOT_ID=$(echo "$GOT" | json_field "['id']")
assert_eq "$GOT_ID" "$ROOT_ID" "GET /annotations/:id returns correct annotation"

GOT_VALUE=$(echo "$GOT" | json_field "['value']")
assert_contains "$GOT_VALUE" "Test root annotation" "GET returns correct value"

# List with filter
LIST=$(curl -sf "$BASE/api/v2/annotations?target_type=module&target_id=__test_module__")
LIST_COUNT=$(echo "$LIST" | json_field "['count']")
[[ "$LIST_COUNT" -ge 1 ]] && pass "GET /annotations?target_id filter works (count=$LIST_COUNT)" \
                            || fail "GET /annotations?target_id filter works (count=$LIST_COUNT)"

# PATCH status
PATCHED=$(curl -sf -X PATCH "$BASE/api/v2/annotations/$ROOT_ID" \
  -H 'Content-Type: application/json' \
  -d '{"status": "confirmed"}')
PATCHED_STATUS=$(echo "$PATCHED" | json_field "['status']")
assert_eq "$PATCHED_STATUS" "confirmed" "PATCH /annotations/:id updates status"

# PATCH value
PATCHED2=$(curl -sf -X PATCH "$BASE/api/v2/annotations/$ROOT_ID" \
  -H 'Content-Type: application/json' \
  -d '{"value": "Updated test value."}')
PATCHED2_VALUE=$(echo "$PATCHED2" | json_field "['value']")
assert_contains "$PATCHED2_VALUE" "Updated test value" "PATCH /annotations/:id updates value"

# =============================================================================
# 3. Supersedes / Thread Chains
# =============================================================================

printf "\n\033[1m— Supersedes / Thread Chains —\033[0m\n"

# Create a reply (supersedes the root)
REPLY1=$(curl -sf -X POST "$BASE/api/v2/annotations" \
  -H 'Content-Type: application/json' \
  -d "{
    \"target_type\": \"module\",
    \"target_id\": \"__test_module__\",
    \"kind\": \"summary\",
    \"value\": \"Human reply: I disagree with the test annotation.\",
    \"source\": \"human\",
    \"supersedes\": $ROOT_ID
  }")
REPLY1_ID=$(echo "$REPLY1" | json_field "['id']")
REPLY1_SUPER=$(echo "$REPLY1" | json_field "['supersedes']")
CREATED_IDS+=("$REPLY1_ID")

assert_not_empty "$REPLY1_ID" "POST reply with supersedes creates annotation (id=$REPLY1_ID)"
assert_eq "$REPLY1_SUPER" "$ROOT_ID" "Reply supersedes points to root ($ROOT_ID)"

# Create a counter-reply (supersedes the reply)
REPLY2=$(curl -sf -X POST "$BASE/api/v2/annotations" \
  -H 'Content-Type: application/json' \
  -d "{
    \"target_type\": \"module\",
    \"target_id\": \"__test_module__\",
    \"kind\": \"summary\",
    \"value\": \"AI counter-reply: revised summary incorporating feedback.\",
    \"source\": \"ai\",
    \"confidence\": 0.9,
    \"supersedes\": $REPLY1_ID
  }")
REPLY2_ID=$(echo "$REPLY2" | json_field "['id']")
REPLY2_SUPER=$(echo "$REPLY2" | json_field "['supersedes']")
CREATED_IDS+=("$REPLY2_ID")

assert_not_empty "$REPLY2_ID" "POST counter-reply creates annotation (id=$REPLY2_ID)"
assert_eq "$REPLY2_SUPER" "$REPLY1_ID" "Counter-reply supersedes the reply ($REPLY1_ID)"

# Verify the full chain via list endpoint
CHAIN=$(curl -sf "$BASE/api/v2/annotations?target_type=module&target_id=__test_module__")
CHAIN_COUNT=$(echo "$CHAIN" | json_field "['count']")
[[ "$CHAIN_COUNT" -ge 3 ]] && pass "Thread has 3+ annotations (count=$CHAIN_COUNT)" \
                             || fail "Thread has 3+ annotations (count=$CHAIN_COUNT)"

# Verify chain integrity: each annotation's supersedes is correct
CHAIN_OK=$(echo "$CHAIN" | python3 -c "
import sys, json
data = json.load(sys.stdin)
by_id = {a['id']: a for a in data['annotations']}
root = by_id.get($ROOT_ID)
r1 = by_id.get($REPLY1_ID)
r2 = by_id.get($REPLY2_ID)
ok = (root and root['supersedes'] is None
      and r1 and r1['supersedes'] == $ROOT_ID
      and r2 and r2['supersedes'] == $REPLY1_ID)
print('OK' if ok else 'FAIL')
")
assert_eq "$CHAIN_OK" "OK" "Thread chain integrity: root -> reply -> counter-reply"

# =============================================================================
# 4. Report Generation
# =============================================================================

printf "\n\033[1m— Report Generation —\033[0m\n"

REPORT=$(curl -sf "$BASE/api/v2/report")
assert_not_empty "$REPORT" "GET /api/v2/report returns content"
assert_contains "$REPORT" "# Codebase Report" "Report has markdown header"
assert_contains "$REPORT" "**Total annotations:**" "Report has summary stats"

# Check threaded display in report (our test thread should appear)
assert_contains "$REPORT" "Updated test value" "Report contains root annotation"
assert_contains "$REPORT" "> Human reply" "Report shows reply as blockquote"
assert_contains "$REPORT" "(human)" "Report includes source tag on reply"
assert_contains "$REPORT" "> AI counter-reply" "Report shows counter-reply as blockquote"
assert_contains "$REPORT" "(ai)" "Report includes source tag on counter-reply"

# =============================================================================
# 5. Validation / Error Handling
# =============================================================================

printf "\n\033[1m— Validation / Error Handling —\033[0m\n"

# Missing required fields
BAD_CREATE=$(curl -s -o /dev/null -w "%{http_code}" -X POST "$BASE/api/v2/annotations" \
  -H 'Content-Type: application/json' \
  -d '{"target_type": "module"}')
assert_eq "$BAD_CREATE" "400" "POST with missing fields returns 400"

# Invalid JSON body
BAD_JSON=$(curl -s -o /dev/null -w "%{http_code}" -X POST "$BASE/api/v2/annotations" \
  -H 'Content-Type: application/json' \
  -d 'not json at all')
assert_eq "$BAD_JSON" "400" "POST with invalid JSON returns 400"

# PATCH with nothing to update
BAD_PATCH=$(curl -s -o /dev/null -w "%{http_code}" -X PATCH "$BASE/api/v2/annotations/$ROOT_ID" \
  -H 'Content-Type: application/json' \
  -d '{}')
assert_eq "$BAD_PATCH" "400" "PATCH with no updateable fields returns 400"

# GET nonexistent annotation
NOT_FOUND=$(curl -s -o /dev/null -w "%{http_code}" "$BASE/api/v2/annotations/999999")
assert_eq "$NOT_FOUND" "404" "GET nonexistent annotation returns 404"

# =============================================================================
# 6. Module & Declaration Endpoints
# =============================================================================

printf "\n\033[1m— Module & Declaration Endpoints —\033[0m\n"

# Fetch a known module
MODULES_LIST=$(curl -sf "$BASE/api/v2/modules")
FIRST_MOD_ID=$(echo "$MODULES_LIST" | python3 -c "
import sys,json; d=json.load(sys.stdin)
mods = [m for m in d['modules'] if m['name'].startswith('CE2.')]
print(mods[0]['id'] if mods else '')
")
if [[ -n "$FIRST_MOD_ID" ]]; then
  DECLS=$(curl -sf "$BASE/api/v2/module-declarations/$FIRST_MOD_ID")
  DECL_COUNT=$(echo "$DECLS" | json_field "['count']")
  assert_not_empty "$DECL_COUNT" "GET /module-declarations/:id returns declarations (count=$DECL_COUNT)"
else
  fail "No CE2.* module found to test declarations endpoint"
fi

# Search
SEARCH=$(curl -sf "$BASE/api/v2/search/SceneCoordinator")
SEARCH_COUNT=$(echo "$SEARCH" | json_field "['count']")
[[ "$SEARCH_COUNT" -ge 1 ]] && pass "GET /search/:query returns results (count=$SEARCH_COUNT)" \
                              || fail "GET /search/:query returns results (count=$SEARCH_COUNT)"

# All imports (bulk)
IMPORTS=$(curl -sf "$BASE/api/v2/all-imports")
IMPORT_COUNT=$(echo "$IMPORTS" | json_field "['count']")
assert_not_empty "$IMPORT_COUNT" "GET /all-imports returns bulk data (count=$IMPORT_COUNT)"

# =============================================================================
# Cleanup
# =============================================================================

if $CLEAN; then
  printf "\n\033[1m— Cleanup —\033[0m\n"
  for id in "${CREATED_IDS[@]}"; do
    curl -sf -X PATCH "$BASE/api/v2/annotations/$id" \
      -H 'Content-Type: application/json' \
      -d '{"status": "rejected"}' > /dev/null 2>&1
  done
  pass "Marked ${#CREATED_IDS[@]} test annotations as rejected"
fi

# =============================================================================
# Summary
# =============================================================================

TOTAL=$((PASS + FAIL))
printf "\n\033[1m— Results —\033[0m\n"
printf "  %d/%d passed" "$PASS" "$TOTAL"
if [[ $FAIL -gt 0 ]]; then
  printf " (\033[31m%d failed\033[0m)" "$FAIL"
fi
printf "\n\n"

[[ $FAIL -eq 0 ]] && exit 0 || exit 1
