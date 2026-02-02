# Corrode Expel (Code Explorer) Frontend
# Static PureScript app served via nginx
#
# Build:   docker build -t psd3-ce-frontend .
# Run:     docker run -p 8082:80 psd3-ce-frontend

FROM nginx:alpine

# Copy static files to /code/ path (matches URL prefix)
COPY public/ /usr/share/nginx/html/code/

# Fix permissions
RUN chmod -R 755 /usr/share/nginx/html/ && \
    find /usr/share/nginx/html/ -type f -exec chmod 644 {} \;

# SPA routing config under /code/
RUN echo 'server { \
    listen 80; \
    root /usr/share/nginx/html; \
    index index.html; \
    location /code/ { \
        try_files $uri $uri/ /code/index.html; \
    } \
}' > /etc/nginx/conf.d/default.conf

EXPOSE 80

HEALTHCHECK --interval=30s --timeout=3s \
  CMD wget -q --spider http://localhost/ || exit 1
