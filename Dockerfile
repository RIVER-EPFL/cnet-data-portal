FROM ubuntu:18.04

# Defining Zurich as local timezone
ARG DEBIAN_FRONTEND=noninteractive
ENV TZ="Europe/Zurich"

# Set runtime environment vars (that build the app_config.R in the start.sh file at runtime)
ENV ENV=$METALP_ENV
ENV DB_NAME=$METALP_DB_NAME
ENV DB_HOSTNAME=$METALP_DB_HOSTNAME
ENV DB_PORT=$METALP_DB_PORT
ENV DB_USERNAME=$METALP_DB_USERNAME
ENV DB_PASSWORD=$METALP_DB_PASSWORD
ENV NOREPLY_ADDRESS=$METALP_NOREPLY_ADDRESS
ENV TO_ADDRESS=$METALP_TO_ADDRESS

# Update timezone
RUN ln -fs /usr/share/zoneinfo/Europe/Zurich /etc/localtime

# Install system and R dependencies
RUN apt-get update && apt-get install -y \
    software-properties-common dirmngr curl lsb-release wget build-essential \
    tzdata python3 g++ make python3-pip \
    libcairo2-dev libxt-dev libgtk2.0-dev xvfb xauth xfonts-base \
    libsodium-dev mysql-client libmysqlclient-dev libmariadb-client-lgpl-dev \
    libxml2-dev openssl libcurl4-openssl-dev libssl-dev gdebi-core \
    openjdk-8-jdk openjdk-8-jre fonts-roboto

# Install R
RUN wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
RUN add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
RUN apt-get update && apt-get install -y r-base r-recommended \
  && dpkg-reconfigure -f noninteractive tzdata \
  && R CMD javareconf

# Install required dependencies for building NodeJS and then install NodeJS and Terser and add shiny user
RUN curl https://nodejs.org/download/release/v14.9.0/node-v14.9.0-linux-x64.tar.gz | tar -zx -C /usr/local --strip-components=1 \
  && npm install terser@5.3.0 -g \
  && groupadd shiny && useradd -g shiny shiny

# Install RShiny
WORKDIR /tmp
RUN wget \
  --no-verbose --show-progress \
  --progress=dot:mega \
  https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.14.948-amd64.deb \
  && dpkg -i shiny-server-1.5.14.948-amd64.deb \
  && rm shiny-server-1.5.14.948-amd64.deb

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
