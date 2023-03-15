#!/bin/bash
# RShiny deployments do not pass system environment variables
# to the application. This script grabs the env vars set in docker
# and creates a config file which is read by the application on
# initialisation, then starts the application

# Delete any existing app config file and recreate from ENV vars
rm -f secrets.R

# Add environment vars from env into R config. This is done
# due to RShiny not passing system env vars to application
cat <<EOF > secrets.R
ENV <- '$METALP_ENV'
MY_DB_NAME <- '$METALP_DB_NAME'
MY_DB_HOST <- '$METALP_DB_HOSTNAME'
MY_DB_PORT <- $METALP_DB_PORT
MY_DB_USER <- '$METALP_DB_USERNAME'
MY_DB_PWD <- '$METALP_DB_PASSWORD'
NOREPLY_ADDRESS <- '$METALP_NOREPLY_ADDRESS'
TO_ADDRESS <- '$METALP_TO_ADDRESS'
EOF

# Delete any database dump config file and recreate from ENV vars
rm -f .my.cnf

cat <<EOF > .my.cnf
[client]\n" >> .my.cnf
user='$METALP_DB_USERNAME'
password='$METALP_DB_PASSWORD'
host='$METALP_DB_HOSTNAME'
port='$METALP_DB_PORT'
EOF

# Run RShiny
/usr/bin/shiny-server
