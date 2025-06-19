# Gunakan image R dengan shiny server
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libpq-dev \
    libgdal-dev \
    libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c( \
    'shiny', \
    'shinydashboard', \
    'DBI', \
    'RPostgres', \
    'tidyverse', \
    'plotly', \
    'DT', \
    'viridis', \
    'scales', \
    'lubridate' \
    ), repos='https://cloud.r-project.org/', dependencies=TRUE)"

# Hapus aplikasi default dan salin aplikasi kita
RUN rm -rf /srv/shiny-server/*
COPY paste.txt /srv/shiny-server/app.R

# Konfigurasi shiny-server untuk Railway
RUN echo "run_as shiny; \
server { \
  listen 3838 0.0.0.0; \
  location / { \
    site_dir /srv/shiny-server; \
    log_dir /var/log/shiny-server; \
    directory_index off; \
  } \
}" > /etc/shiny-server/shiny-server.conf

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server && \
    chown -R shiny:shiny /var/log/shiny-server

# Expose port untuk Railway
EXPOSE 3838

# Start shiny-server
CMD ["/usr/bin/shiny-server"]
