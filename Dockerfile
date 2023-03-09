FROM rocker/shiny:4.2.0

# Install necessary dependencies for the R repository
RUN apt-get update && apt-get install -y \
    curl lsb-release wget

# Defining Zurich as local timezone
ARG DEBIAN_FRONTEND=noninteractive
ENV TZ="Europe/Zurich"
RUN ln -fs /usr/share/zoneinfo/Europe/Zurich /etc/localtime
# RUN apt-get -y install tzdata
RUN dpkg-reconfigure -f noninteractive tzdata

# Refresh repositories
RUN apt-get update

# Install required dependencies for building NodeJS and then install NodeJS
RUN apt-get install -y python3 g++ make python3-pip
RUN curl https://nodejs.org/download/release/v14.9.0/node-v14.9.0-linux-x64.tar.gz | tar -zx -C /usr/local --strip-components=1

# Install Terser globally
RUN npm install terser@5.3.0 -g

# Install cairo and dependencies
RUN apt-get install -y libcairo2-dev libxt-dev libgtk2.0-dev xvfb xauth xfonts-base

# Install sodium dependencies
RUN apt-get install -y libsodium-dev

# Install MySQL dependencies
RUN apt-get install -y mysql-client libmysqlclient-dev

# Install XML dependencies
RUN apt-get install -y libxml2-dev openssl libcurl4-openssl-dev libssl-dev libxml2-dev gdebi-core

# MariaDB dependency to build RMySQL 0.10.20
RUN apt-get install -y libmariadb-client-lgpl-dev

# Delete the example application, copy dependency file and install R dependencies
RUN rm -rf /srv/shiny-server/*
COPY packages_installation.R /srv/shiny-server/
ARG MAKE="make -j2"
WORKDIR /srv/shiny-server/
RUN R -f packages_installation.R

# Set runtime environment vars (that build the app_config.R in the start.sh file at runtime)
ENV NOMIS_ENV=$NOMIS_ENV
ENV NOMIS_DB_NAME=$NOMIS_DB_NAME
ENV NOMIS_DB_HOSTNAME=$NOMIS_DB_HOSTNAME
ENV NOMIS_DB_PORT=$NOMIS_DB_PORT
ENV NOMIS_DB_USERNAME=$NOMIS_DB_USERNAME
ENV NOMIS_DB_PASSWORD=$NOMIS_DB_PASSWORD

# Install fonts
RUN apt-get install -y fonts-roboto
RUN R -e 'hrbrthemes::import_roboto_condensed()'

# Copy the rest of the application. Doing it in this order allows changes to the app folder
# without invoking a package rebuild
COPY ./app/ ./
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
RUN chown -R shiny:shiny /srv/shiny-server/

# Build program assets
RUN R -f assets_compilation.R

# Run as user shiny instead of root
USER shiny
EXPOSE 3838

# Start application
CMD ["./start.sh"]
