# Minard — Code Cartography for PureScript
#
# Targets:
#   bootstrap   — check prereqs, build everything, self-scan, print instructions
#   start       — start server + frontend
#   stop        — kill services (on configured ports, default 3000/3001)
#   clean-test  — clone → bootstrap → start in /tmp (verifies repo is self-contained)

MINARD := $(shell pwd)
DB     := database/ce-unified.duckdb

# Configurable ports (override with: make start API_PORT=4000 FRONTEND_PORT=4001)
API_PORT      ?= 3000
FRONTEND_PORT ?= 3001

# Platform detection for pre-built loader binary
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

ifeq ($(UNAME_S),Darwin)
  PLATFORM := darwin
  OPEN_CMD := open
else
  PLATFORM := linux
  OPEN_CMD := xdg-open 2>/dev/null || echo "Open"
endif

PREBUILT_LOADER := minard-loader/bin/minard-loader-$(PLATFORM)-$(UNAME_M)
CARGO_LOADER    := minard-loader/target/release/minard-loader

# Resolve loader binary: pre-built > cargo build > error
ifeq ($(wildcard $(PREBUILT_LOADER)),$(PREBUILT_LOADER))
  LOADER := $(PREBUILT_LOADER)
else ifeq ($(wildcard $(CARGO_LOADER)),$(CARGO_LOADER))
  LOADER := $(CARGO_LOADER)
else
  LOADER := __NEEDS_BUILD__
endif

.PHONY: bootstrap start stop help clean-test bundle-static

help:
	@echo "Minard — Code Cartography for PureScript"
	@echo ""
	@echo "  make bootstrap      Check prereqs, build everything, self-scan"
	@echo "  make start          Start server + frontend (ports 3000/3001)"
	@echo "  make stop           Stop services"
	@echo "  make bundle-static  Build frontend for GH Pages / minard.app (clone banner on)"
	@echo "  make clean-test     Fresh clone → bootstrap → start in /tmp"
	@echo ""

# =============================================================================
# bootstrap
# =============================================================================

bootstrap: _check-prereqs _build-loader _build-server _build-frontend _self-scan
	@echo ""
	@echo "============================================"
	@echo "  Bootstrap complete."
	@echo "  Run 'make start' to launch Minard."
	@echo "============================================"

_check-prereqs:
	@echo "Checking prerequisites..."
	@command -v node  >/dev/null 2>&1 || { echo "ERROR: node not found. Install Node.js."; exit 1; }
	@command -v spago >/dev/null 2>&1 || { echo "ERROR: spago not found. Install spago via npm."; exit 1; }
	@command -v purs  >/dev/null 2>&1 || { echo "ERROR: purs not found. Install the PureScript compiler."; exit 1; }
	@command -v cargo >/dev/null 2>&1 || echo "NOTE: cargo not found — Rust builds unavailable (pre-built binary will be used if present)."
	@echo "  node:  $$(node --version)"
	@echo "  spago: $$(spago --version)"
	@echo "  purs:  $$(purs --version)"
	@echo "Prerequisites OK."

_build-loader:
ifeq ($(LOADER),__NEEDS_BUILD__)
	@if command -v cargo >/dev/null 2>&1; then \
		echo "Building loader from source..."; \
		cd minard-loader && cargo build --release; \
	else \
		echo "ERROR: No pre-built loader binary for $(PLATFORM)-$(UNAME_M) and cargo is not installed."; \
		echo "Either install Rust (https://rustup.rs) or obtain a pre-built binary."; \
		exit 1; \
	fi
else
	@echo "Loader binary: $(LOADER)"
endif

_build-server:
	@echo "Building server..."
	@spago build -p minard-server

_install-deps:
	@echo "Installing npm dependencies..."
	@npm install

_stamp-build:
	@echo 'export const buildStamp = "'"$$(date '+%Y-%m-%d %H:%M')"'";' > frontend/src/BuildInfo.js
	@echo 'export const isStaticDeploy = false;' >> frontend/src/BuildInfo.js

_stamp-build-static:
	@echo 'export const buildStamp = "'"$$(date '+%Y-%m-%d %H:%M')"'";' > frontend/src/BuildInfo.js
	@echo 'export const isStaticDeploy = true;' >> frontend/src/BuildInfo.js

_build-frontend: _install-deps _stamp-build
	@echo "Building frontend..."
	@spago build -p minard-frontend
	@spago bundle -p minard-frontend

bundle-static: _install-deps _stamp-build-static
	@echo "Building static deploy bundle (clone banner enabled)..."
	@spago build -p minard-frontend
	@spago bundle -p minard-frontend
	@echo "Resetting BuildInfo to local mode..."
	@echo 'export const buildStamp = "'"$$(date '+%Y-%m-%d %H:%M')"'";' > frontend/src/BuildInfo.js
	@echo 'export const isStaticDeploy = false;' >> frontend/src/BuildInfo.js
	@echo "Static bundle ready at frontend/public/bundle.js"

_self-scan:
	@echo "Self-scanning minard codebase..."
	@# Resolve loader path again after potential cargo build
	@if [ -f "$(PREBUILT_LOADER)" ]; then \
		$(PREBUILT_LOADER) load --database $(DB) --scan .; \
	elif [ -f "$(CARGO_LOADER)" ]; then \
		$(CARGO_LOADER) load --database $(DB) --scan .; \
	else \
		echo "WARNING: Loader binary not found, skipping self-scan."; \
	fi

# =============================================================================
# start / stop
# =============================================================================

start:
	@echo "Starting Minard..."
	@cd $(MINARD) && PORT=$(API_PORT) MINARD_DB=$(DB) node server/run.js &
	@cd $(MINARD)/frontend && npx serve public -p $(FRONTEND_PORT) &
	@sleep 1
	@echo ""
	@echo "  API:      http://localhost:$(API_PORT)"
	@echo "  Frontend: http://localhost:$(FRONTEND_PORT)"
	@if [ "$(API_PORT)" != "3000" ]; then \
		echo "  (API on non-default port — open frontend with ?api=$(API_PORT))"; \
	fi
	@echo ""
	@$(OPEN_CMD) http://localhost:$(FRONTEND_PORT)

stop:
	@echo "Stopping Minard..."
	@lsof -ti :$(API_PORT) :$(FRONTEND_PORT) 2>/dev/null | xargs kill 2>/dev/null || true
	@sleep 1
	@echo "Stopped."

# =============================================================================
# clean-test — full clone-to-running verification in /tmp
# =============================================================================

TEST_DIR   := /tmp/minard-test
REMOTE_URL := $(shell git remote get-url origin 2>/dev/null || echo "https://github.com/afcondon/minard.git")

clean-test: stop
	@echo ""
	@echo "=== Clean test from clone ==="
	@echo ""
	@echo "Removing $(TEST_DIR)..."
	@rm -rf $(TEST_DIR)
	@echo "Cloning $(REMOTE_URL)..."
	@git clone $(REMOTE_URL) $(TEST_DIR)
	@echo ""
	@echo "--- Bootstrap ---"
	@cd $(TEST_DIR) && $(MAKE) bootstrap
	@echo ""
	@echo "--- Starting ---"
	@cd $(TEST_DIR) && $(MAKE) start
	@echo ""
	@echo "=== Clean test complete ==="
	@echo "  Running from: $(TEST_DIR)"
	@echo "  API:          http://localhost:3000"
	@echo "  Frontend:     http://localhost:3001"
	@echo ""
	@echo "To stop:  make stop"
	@echo "To clean: rm -rf $(TEST_DIR)"
