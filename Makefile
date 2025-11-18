NAME          := ghcr.io/alexandru/github-webhook-listener
TAG           := $$(./scripts/new-version.sh)
IMG           := ${NAME}:${TAG}
LATEST        := ${NAME}:latest

test:
	cargo test

build:
	cargo build --release

init-docker:
	docker buildx inspect mybuilder || docker buildx create --name mybuilder
	docker buildx use mybuilder

build-docker: init-docker
	docker buildx build --platform linux/amd64,linux/arm64 -f ./Dockerfile -t "${IMG}" -t "${LATEST}" ${DOCKER_EXTRA_ARGS} .

push-docker:
	DOCKER_EXTRA_ARGS="--push" $(MAKE) build-docker

build-docker-local:
	docker build -f ./Dockerfile -t "${IMG}" -t "${LATEST}" .

run-docker: build-docker-local
	docker run -p 8080:8080 -ti ${LATEST}

clean:
	cargo clean
	rm -rf target/
