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
	docker buildx build --platform linux/amd64,linux/arm64 -f ./src/main/docker/Dockerfile.jvm -t "${IMG_JVM}" .
	docker tag "${IMG_JVM}" "${LATEST_JVM}"

push-jvm:
	docker push ${IMG_JVM}
	docker push ${LATEST_JVM}

run-jvm:
	docker run -p 8080:8080 -ti ${LATEST_JVM}

build-native: init-docker
	docker buildx build --platform linux/amd64,linux/arm64 -f ./src/main/docker/Dockerfile.native -t "${IMG_NATIVE}" .
	docker tag "${IMG_NATIVE}" "${LATEST_NATIVE}"
	docker tag "${IMG_NATIVE}" "${LATEST}"

push-native:
	docker push ${IMG_NATIVE}
	docker push ${LATEST_NATIVE}
	docker push ${LATEST}

run-native:
	docker run -p 8080:8080 -ti ${LATEST_NATIVE}

