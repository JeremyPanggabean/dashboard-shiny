version: '3.8'

services:
  shiny-dashboard:
    build: .
    ports:
      - "3838:3838"
    environment:
      - DB_NAME=${DB_NAME:-railway}
      - DB_HOST=${DB_HOST:-nozomi.proxy.rlwy.net}
      - DB_PORT=${DB_PORT:-49571}
      - DB_USER=${DB_USER:-postgres}
      - DB_PASSWORD=${DB_PASSWORD:-ZJvdaWwtZBzHSDnUglLzwsUwWjdahEip}
    volumes:
      - ./logs:/var/log/shiny-server
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:3838"]
      interval: 30s
      timeout: 10s
      retries: 3