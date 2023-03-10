#!/bin/bash
# RShiny deployments do not pass system environment variables
# to the application. This script grabs the env vars set in docker
# and creates a config file which is read by the application on
# initialisation, then starts the application

# Delete any existing app config file and recreate from ENV vars
rm -f secrets.R

# Add environment vars from env into R config. This is done
# due to RShiny not passing system env vars to application
printf "ENV <- '%s'\n" "$METALP_ENV" >> secrets.R
printf "MY_DB_NAME <- '%s'\n" "$METALP_DB_NAME" >> secrets.R
printf "MY_DB_HOST <- '%s'\n" "$METALP_DB_HOSTNAME" >> secrets.R
printf "MY_DB_PORT <- %s\n" "$METALP_DB_PORT" >> secrets.R
printf "MY_DB_USER <- '%s'\n" "$METALP_DB_USERNAME" >> secrets.R
printf "MY_DB_PWD <- '%s'\n" "$METALP_DB_PASSWORD" >> secrets.R
printf "NOREPLY_ADDRESS <- '%s'\n" "$METALP_NOREPLY_ADDRESS" >> secrets.R
printf "TO_ADDRESS <- '%s'\n" "$METALP_TO_ADDRESS" >> secrets.R

# Delete any database dump config file and recreate from ENV vars
rm -f .my.cnf

printf "[client]\n" >> .my.cnf
printf "user='%s'\n" "$METALP_DB_USERNAME" >> .my.cnf
printf "password='%s'\n" "$METALP_DB_PASSWORD" >> .my.cnf
printf "host='%s'\n" "$METALP_DB_HOSTNAME" >> .my.cnf
printf "port='%s'\n" "$METALP_DB_PORT" >> .my.cnf

# Run RShiny
/usr/bin/shiny-server
