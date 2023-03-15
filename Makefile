run:
	docker compose build --pull
	docker compose up -d --remove-orphans
