NAME          := ghcr.io/alexandru/github-webhook-listener
TAG           := $$(./scripts/new-version.sh)
IMG_JVM       := ${NAME}:jvm-${TAG}
IMG_NATIVE    := ${NAME}:native-${TAG}
LATEST_JVM    := ${NAME}:jvm-v2
LATEST_NATIVE := ${NAME}:native-v2
LATEST        := ${NAME}:v2
PLATFORM      ?= linux/amd64,linux/arm64

dependency-updates:
	./gradlew dependencyUpdates \
		-Drevision=release \
		-DoutputFormatter=html \
		--refresh-dependencies && \
		open build/dependencyUpdates/report.html

init-docker:
	docker buildx inspect mybuilder || docker buildx create --name mybuilder
	docker buildx use mybuilder

build-jvm: init-docker
	docker buildx build --platform linux/amd64,linux/arm64 -f ./src/main/docker/Dockerfile.jvm -t "${IMG_JVM}" -t "${LATEST_JVM}" ${DOCKER_EXTRA_ARGS} .

push-jvm:
	DOCKER_EXTRA_ARGS="--push" $(MAKE) build-jvm

# Build and push for a single platform (used in matrix builds)
build-jvm-platform: init-docker
	$(eval PLATFORM_TAG := $(shell echo ${PLATFORM} | tr '/' '-'))
	docker buildx build --platform ${PLATFORM} -f ./src/main/docker/Dockerfile.jvm -t "${IMG_JVM}-${PLATFORM_TAG}" -t "${LATEST_JVM}-${PLATFORM_TAG}" ${DOCKER_EXTRA_ARGS} .

push-jvm-platform:
	DOCKER_EXTRA_ARGS="--push" $(MAKE) build-jvm-platform

# Create and push multi-platform manifest combining platform-specific images
push-jvm-manifest:
	docker buildx imagetools create -t "${IMG_JVM}" -t "${LATEST_JVM}" \
		"${IMG_JVM}-linux-amd64" \
		"${IMG_JVM}-linux-arm64"

build-jvm-local:
	docker build -f ./src/main/docker/Dockerfile.jvm -t "${IMG_JVM}" -t "${LATEST_JVM}" .

run-jvm: build-jvm-local
	docker run -p 8080:8080 -ti ${LATEST_JVM}

build-native: init-docker
	docker buildx build --platform linux/amd64,linux/arm64 -f ./src/main/docker/Dockerfile.native -t "${IMG_NATIVE}" -t "${LATEST_NATIVE}" -t "${LATEST}" ${DOCKER_EXTRA_ARGS} .

push-native:
	DOCKER_EXTRA_ARGS="--push" $(MAKE) build-native

# Build and push for a single platform (used in matrix builds)
build-native-platform: init-docker
	$(eval PLATFORM_TAG := $(shell echo ${PLATFORM} | tr '/' '-'))
	docker buildx build --platform ${PLATFORM} -f ./src/main/docker/Dockerfile.native -t "${IMG_NATIVE}-${PLATFORM_TAG}" -t "${LATEST_NATIVE}-${PLATFORM_TAG}" -t "${LATEST}-${PLATFORM_TAG}" ${DOCKER_EXTRA_ARGS} .

push-native-platform:
	DOCKER_EXTRA_ARGS="--push" $(MAKE) build-native-platform

# Create and push multi-platform manifest combining platform-specific images
push-native-manifest:
	docker buildx imagetools create -t "${IMG_NATIVE}" -t "${LATEST_NATIVE}" -t "${LATEST}" \
		"${IMG_NATIVE}-linux-amd64" \
		"${IMG_NATIVE}-linux-arm64"

build-native-local:
	docker build -f ./src/main/docker/Dockerfile.native -t "${IMG_NATIVE}" -t "${LATEST_NATIVE}" -t "${LATEST}" .

run-native: build-native-local
	docker run -p 8080:8080 -ti ${LATEST_NATIVE}
