NAME          := ghcr.io/alexandru/github-webhook-listener
TAG           := $$(./scripts/new-version.sh)
IMG_JVM       := ${NAME}:jvm-${TAG}
IMG_NATIVE    := ${NAME}:native-${TAG}
LATEST_JVM    := ${NAME}:jvm-latest
LATEST_NATIVE := ${NAME}:native-latest
LATEST        := ${NAME}:latest

init-docker:
	docker buildx inspect mybuilder || docker buildx create --name mybuilder
	docker buildx use mybuilder

build-jvm: init-docker
	docker buildx build --platform linux/amd64,linux/arm64 -f ./src/main/docker/Dockerfile.jvm -t "${IMG_JVM}" -t "${LATEST_JVM}" ${DOCKER_EXTRA_ARGS} .

push-jvm:
	DOCKER_EXTRA_ARGS="--push" $(MAKE) build-jvm

run-jvm:
	docker run -p 8080:8080 -ti ${LATEST_JVM}

build-native: init-docker
	docker buildx build --platform linux/amd64,linux/arm64 -f ./src/main/docker/Dockerfile.native -t "${IMG_NATIVE}" -t "${LATEST_NATIVE}" -t "${LATEST}" ${DOCKER_EXTRA_ARGS} .

push-native:
	DOCKER_EXTRA_ARGS="--push" $(MAKE) build-native

run-native:
	docker run -p 8080:8080 -ti ${LATEST_NATIVE}

