#!/bin/bash
# RShiny deployments do not pass system environment variables
# to the application. This script grabs the env vars set in docker
# and creates a config file which is read by the application on
# initialisation, then starts the application

# Add environment vars from env into R config. This is done
# due to RShiny not passing system env vars to application
cat <<EOF > secrets.R
ENV <- '$CNET_ENV'
MY_DB_NAME <- '$CNET_DB_NAME'
MY_DB_HOST <- '$CNET_DB_HOSTNAME'
MY_DB_PORT <- $CNET_DB_PORT
MY_DB_USER <- '$CNET_DB_USERNAME'
MY_DB_PWD <- '$CNET_DB_PASSWORD'
NOREPLY_ADDRESS <- '$CNET_NOREPLY_ADDRESS'
TO_ADDRESS <- '$CNET_TO_ADDRESS'
EOF

# Write database dump config file
cat <<EOF > ~/.my.cnf
[client]
user=$CNET_DB_USERNAME
password=$CNET_DB_PASSWORD
host=$CNET_DB_HOSTNAME
port=$CNET_DB_PORT
EOF

# Run RShiny
/usr/bin/shiny-server
