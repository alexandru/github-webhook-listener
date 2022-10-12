# GitHub Webhook Listener (ver. 2)

[![Build](https://github.com/alexandru/github-webhook-listener/workflows/build/badge.svg?branch=main)](https://github.com/alexandru/github-webhook-listener/actions?query=branch%3Amain+workflow%3Abuild) [![Deploy](https://github.com/alexandru/github-webhook-listener/workflows/deploy/badge.svg)](https://github.com/alexandru/github-webhook-listener/actions?query=workflow%3Adeploy)

A simple web app that can be registered as a
[GitHub Webhook](https://developer.github.com/webhooks/)
and trigger shell commands in response to events.

Main use-case is to trigger refreshes of websites hosted on your own
server via CI jobs (e.g., GitHub Actions), but in a secure way, without 
exposing server credentials or SSH keys.

The server process is also light in resource usage, not using more
than 20 MB of RAM, so it can be installed on under-powered servers.

## Development

To run the project in development mode:

```sh
./gradlew run -Pdevelopment --args="./config/application-dummy.conf"
```

After adding new dependencies:

```sh
./gradlew refreshVersionsMigrate  --mode=VersionCatalogOnly
```

To update project dependencies:

```sh
./gradlew refreshVersions
```

## Docker

To build the JVM image from scratch:

```sh 
docker build -f ./src/main/docker/Dockerfile.jvm -t github-webhook-listener-jvm .
```

To run the JVM image:

```sh
docker run -p 8080:8080 github-webhook-listener-jvm
```

## License

Copyright © 2018-2022 Alexandru Nedelcu, some rights reserved.

Licensed under the AGPL-3.0 license. See [LICENSE](./LICENSE).
