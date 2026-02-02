# Site Explorer (Halogen Route Discovery)
# Static PureScript app served via nginx
#
# Build:   docker build -t psd3-site-explorer .
# Run:     docker run -p 8093:80 psd3-site-explorer

FROM nginx:alpine

# Copy public files to /spider/ path (matches URL prefix)
COPY public/ /usr/share/nginx/html/spider/

# Fix permissions
RUN chmod -R 755 /usr/share/nginx/html/ && \
    find /usr/share/nginx/html/ -type f -exec chmod 644 {} \;

# SPA routing config under /spider/
RUN echo 'server { \
    listen 80; \
    root /usr/share/nginx/html; \
    index index.html; \
    location /spider/ { \
        try_files $uri $uri/ /spider/index.html; \
    } \
}' > /etc/nginx/conf.d/default.conf

EXPOSE 80

HEALTHCHECK --interval=30s --timeout=3s \
  CMD wget -q --spider http://localhost/spider/ || exit 1
