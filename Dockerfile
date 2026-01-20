FROM rocker/shiny:4.5.1

# Install R packages at build time (not lazily at runtime)
# Add your packages here. Example:
# RUN R -e "install.packages(c('dplyr', 'ggplot2'), repos='https://cloud.r-project.org')"

# Remove default shiny apps
RUN rm -rf /srv/shiny-server/*

# Copy our app
# * Here we place app.R directly into /srv/shiny-server/. This makes it the "root app" served at http://server:3838/
# * Another option is you could place subdirectories there, which would be served at http://server:3838/subdir/
COPY ./app /srv/shiny-server/

# Remap shiny user to specific UID/GID who owns the files on the host filesystem.
# This allows the container to read files under mounted volumes such as /data_mount
ARG SHINY_UID=1000
ARG SHINY_GID=1000
RUN usermod -u ${SHINY_UID} shiny && groupmod -g ${SHINY_GID} shiny
RUN chown -R shiny:shiny /var/lib/shiny-server /var/log/shiny-server /srv/shiny-server

# Create directory for data mount point, and an environment variable for the shiny app workers
RUN mkdir -p /data_mount
RUN echo "APP_DATA_PATH=/data_mount" > /srv/shiny-server/.Renviron

# Configure app to run as Unix user 'shiny'
USER shiny
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Expose HTTP port
EXPOSE 3838

# Run shiny-server
CMD ["/usr/bin/shiny-server"]
