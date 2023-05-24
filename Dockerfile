FROM ghcr.io/river-epfl/metalp-data-portal:v1.0.0
USER root

# Delete the example application, copy dependency lock file and install R dependencies
RUN rm -rf /srv/shiny-server/*
COPY packages_installation.R renv.lock /srv/shiny-server/
ARG MAKE="make -j2"
WORKDIR /srv/shiny-server/
RUN R -f packages_installation.R


# Copy the rest of the application. Doing it in this order allows changes to the app folder
# without invoking a package rebuild
COPY ./app/ ./
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
RUN chown -R shiny:shiny /srv/shiny-server/ \
  && mkdir -p /var/lib/shiny-server/bookmarks/shiny \
  && chown -R shiny:shiny /var/lib/shiny-server \
  && R -f assets_compilation.R

  # Run as user shiny instead of root and expose the port
USER shiny
EXPOSE 3838

# Build app_config.R file and start application
CMD ["./start.sh"]
