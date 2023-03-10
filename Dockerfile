FROM ubuntu:18.04

# Install necessary dependencies for the R repository
RUN apt-get update && apt-get install -y \
    curl lsb-release wget build-essential

# Defining Zurich as local timezone
ARG DEBIAN_FRONTEND=noninteractive
ENV TZ="Europe/Zurich"
RUN ln -fs /usr/share/zoneinfo/Europe/Zurich /etc/localtime
RUN apt-get -y install tzdata
RUN dpkg-reconfigure -f noninteractive tzdata

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

# MariaDB dependency to build RMySQL 0.10.20
RUN apt-get install -y libmariadb-client-lgpl-dev

# Install XML dependencies
RUN apt-get install -y libxml2-dev openssl libcurl4-openssl-dev libssl-dev gdebi-core

# Install java deps for rJava
RUN apt-get install -y openjdk-8-jdk openjdk-8-jre

# Install fonts
RUN apt-get install -y fonts-roboto

# Add a group and user to run the RShiny interface
RUN groupadd shiny
RUN useradd -g shiny shiny

# Install RShiny
WORKDIR /tmp
RUN wget \
  --no-verbose --show-progress \
  --progress=dot:mega \
  https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.14.948-amd64.deb
RUN dpkg -i shiny-server-1.5.14.948-amd64.deb
RUN rm shiny-server-1.5.14.948-amd64.deb

# Install R
RUN apt-get install -y --no-install-recommends software-properties-common dirmngr
RUN wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
RUN add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
RUN apt-get install -y r-base r-recommended

# Setup java configuration in R
RUN R CMD javareconf

# Delete the example application, copy dependency lock file and install R dependencies
RUN rm -rf /srv/shiny-server/*
COPY packages_installation.R renv.lock /srv/shiny-server/
ARG MAKE="make -j2"
WORKDIR /srv/shiny-server/
RUN R -f packages_installation.R

# Set runtime environment vars (that build the app_config.R in the start.sh file at runtime)
ENV ENV=$METALP_ENV
ENV DB_NAME=$METALP_DB_NAME
ENV DB_HOSTNAME=$METALP_DB_HOSTNAME
ENV DB_PORT=$METALP_DB_PORT
ENV DB_USERNAME=$METALP_DB_USERNAME
ENV DB_PASSWORD=$METALP_DB_PASSWORD
ENV NOREPLY_ADDRESS=$METALP_NOREPLY_ADDRESS
ENV TO_ADDRESS=$METALP_TO_ADDRESS

# Copy the rest of the application. Doing it in this order allows changes to the app folder
# without invoking a package rebuild
COPY ./app/ ./
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
RUN chown -R shiny:shiny /srv/shiny-server/

# Build program assets
RUN R -f assets_compilation.R

# Create bookmark folders and associate permissions
RUN mkdir -p /var/lib/shiny-server/bookmarks/shiny
RUN chown -R shiny:shiny /var/lib/shiny-server

# Run as user shiny instead of root and expose the port
USER shiny
EXPOSE 3838

# Build app_config.R file and start application
CMD ["./start.sh"]
