# Minard — Code Cartography for PureScript
#
# Targets:
#   bootstrap  — check prereqs, build everything, self-scan, print instructions
#   start      — start server + frontend
#   stop       — kill services on ports 3000/3001

MINARD := $(shell pwd)
DB     := database/ce-unified.duckdb

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

.PHONY: bootstrap start stop help

help:
	@echo "Minard — Code Cartography for PureScript"
	@echo ""
	@echo "  make bootstrap   Check prereqs, build everything, self-scan"
	@echo "  make start       Start server + frontend (ports 3000/3001)"
	@echo "  make stop        Stop services"
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

_build-frontend:
	@echo "Building frontend..."
	@spago build -p minard-frontend
	@spago bundle -p minard-frontend

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
	@cd $(MINARD) && node server/run.js &
	@cd $(MINARD)/frontend && npx serve public -p 3001 &
	@sleep 1
	@echo ""
	@echo "  API:      http://localhost:3000"
	@echo "  Frontend: http://localhost:3001"
	@echo ""
	@$(OPEN_CMD) http://localhost:3001

stop:
	@echo "Stopping Minard..."
	@lsof -ti :3000 :3001 2>/dev/null | xargs kill 2>/dev/null || true
	@echo "Stopped."
