APPLICATION_NAME ?= metalp-data-portal
 
build:
	docker build --tag ${APPLICATION_NAME} .
run:
	docker compose up 

