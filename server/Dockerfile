# Minard Backend (Code Cartography)
# Node.js server with DuckDB for code analysis
#
# NOTE: Build from parent directory (apps/minard/)
#   docker build -f server/Dockerfile -t psd3-minard-backend .
#
# Run:   docker run -p 3000:3000 psd3-minard-backend

FROM node:20-slim

WORKDIR /app

# Install build dependencies for DuckDB native module and wget for healthcheck
RUN apt-get update && apt-get install -y python3 make g++ wget --no-install-recommends && rm -rf /var/lib/apt/lists/*

# Copy package files and install dependencies
COPY server/package*.json ./server/
RUN cd server && npm install --omit=dev

# Symlink node_modules to app root so output/ can find packages
RUN ln -s /app/server/node_modules /app/node_modules

# Copy compiled PureScript output
COPY output/ ./output/

# Copy server runner
COPY server/run.js ./server/

# Copy database (using unified schema for v2 API)
COPY database/ce-unified.duckdb ./database/

WORKDIR /app/server

# Symlink database so relative path ./database works from server
RUN ln -s /app/database ./database

EXPOSE 3000

HEALTHCHECK --interval=30s --timeout=10s --start-period=5s \
  CMD wget -q --spider http://localhost:3000/ || exit 1

CMD ["node", "run.js"]
