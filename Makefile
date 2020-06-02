NAME   := alexelcu/github-webhook-listener
TAG    := $$(./scripts/new-version)
IMG    := ${NAME}:${TAG}
LATEST := ${NAME}:latest

build:
	docker build -t "${IMG}" .
	docker tag "${IMG}" "${LATEST}"

push:
	docker push ${NAME}
