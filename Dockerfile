FROM ghcr.io/river-epfl/metalp-data-portal:v1.0.2
USER root

ARG MAKE="make -j2"
WORKDIR /srv/shiny-server/
RUN R -e "install.packages('RMariaDB')"
RUN apt-get install -y libmariadbclient-dev


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
