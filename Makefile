NAME          := ghcr.io/alexandru/github-webhook-listener
TAG           := $$(./scripts/new-version.sh)
IMG           := ${NAME}:${TAG}
LATEST_V3	  := ${NAME}:v3
LATEST        := ${NAME}:latest
PLATFORM      ?= linux/amd64,linux/arm64

test:
	cargo test

build:
	cargo build --release

init-docker:
	docker buildx inspect mybuilder || docker buildx create --name mybuilder
	docker buildx use mybuilder

build-docker: init-docker
	docker buildx build --platform linux/amd64,linux/arm64 -f ./Dockerfile -t "${IMG}" -t "${LATEST_V3}" -t "${LATEST}" ${DOCKER_EXTRA_ARGS} .

push-docker:
	DOCKER_EXTRA_ARGS="--push" $(MAKE) build-docker

# Build and push for a single platform (used in matrix builds)
build-docker-platform: init-docker
	$(eval PLATFORM_TAG := $(shell echo ${PLATFORM} | tr '/' '-'))
	docker buildx build --platform ${PLATFORM} -f ./Dockerfile -t "${IMG}-${PLATFORM_TAG}" ${DOCKER_EXTRA_ARGS} .

push-docker-platform:
	DOCKER_EXTRA_ARGS="--push" $(MAKE) build-docker-platform

# Create and push multi-platform manifest combining platform-specific images
push-manifest:
	docker buildx imagetools create -t "${IMG}" -t "${LATEST_V3}" -t "${LATEST}" \
		"${IMG}-linux-amd64" \
		"${IMG}-linux-arm64"

build-docker-local:
	docker build -f ./Dockerfile -t "${IMG}" -t "${LATEST_V3}" -t "${LATEST}" .

run-docker: build-docker-local
	docker run -p 8080:8080 -ti "${LATEST_V3}"

clean:
	cargo clean
	rm -rf target/
