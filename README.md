# GitHub Webhook Listener (ver. 2)

[![Build](https://github.com/alexandru/github-webhook-listener/workflows/build/badge.svg?branch=main)](https://github.com/alexandru/github-webhook-listener/actions?query=branch%3Amain+workflow%3Abuild) [![Deploy](https://github.com/alexandru/github-webhook-listener/workflows/deploy/badge.svg)](https://github.com/alexandru/github-webhook-listener/actions?query=workflow%3Adeploy)

A simple web app that can be registered as a
[GitHub Webhook](https://developer.github.com/webhooks/)
and trigger shell commands in response to events.

Main use-case is to trigger refreshes of websites hosted on your own
server via CI jobs (e.g., GitHub Actions), but in a secure way, without 
exposing server credentials or SSH keys.

The server process is also light in resource usage, not using more
than 10 MB of RAM, so it can be installed on under-powered servers.

> **NOTE**
> 
> This used to be a Haskell project, that I switched to Kotlin. The code is still available on the [v1-haskell](https://github.com/alexandru/github-webhook-listener/tree/v1-haskell) branch. 
> There's also an experimental Rust branch, see [v3-rust](https://github.com/alexandru/github-webhook-listener/tree/v3-rust).

## Setup

Docker images are published via [GitHub's Packages](https://github.com/alexandru/github-webhook-listener/pkgs/container/github-webhook-listener). You can quickly run a process like this:

```sh
docker run \
  -p 8080:8080 \
  -ti ghcr.io/alexandru/github-webhook-listener:native-latest
```

There are 2 versions of this project being published. The default is a binary compiled to a native executable via [GraalVM's Native Image](https://www.graalvm.org/22.1/reference-manual/native-image/). The other image is a JAR that runs with OpenJDK. You can choose between them via the tag used. To use the OpenJDK version, look for tags prefixed with `jvm-`:

```sh
docker run \
  -p 8080:8080 \
  -ti ghcr.io/alexandru/github-webhook-listener:jvm-latest
```

### Which version to choose?

The native version (e.g., the `native-latest` tag) uses under 10 MB of RAM, and it's good for underpowered servers. The JVM version (e.g., `jvm-latest`) has at least a 50 MB penalty, so use it only if you bump into problems with the native version. The JVM's execution is optimized with the Shenandoah GC, though, releasing memory back to the OS, it's as optimal as a Java process can be, and if you have the RAM, you might prefer it.

### Server Configuration

On its own this just starts the server, but doesn't know how to do anything. We'll need to specify a configuration file: Create your `./config.yaml`:

```yaml
http:
  path: "/"
  port: 8080

projects:
  myproject:
    ref: "refs/heads/gh-pages"
    directory: "/var/www/myproject"
    command: "git pull"
    secret: "xxxxxxxxxxxxxxxxxxxxxxxxxx"
```

Notes:

1. `myproject` in `project.myproject` is just a name of a project, it could be anything;
2. `ref` says to only react on pushes to the `gh-pages` branch;
3. `directory` is where the `command` should be executed;
4. `command` is to be executed — note that `git` is not installed, see below;

You can then run the server:

```sh
docker run \
  -p 8080:8080 \
  -v "$(pwd)/config.yaml:/opt/app/config/config.yaml" \
  -v "/var/www:/var/www" \
  -u "$(id -u www-data):$(id -g www-data)" \
  -ti ghcr.io/alexandru/github-webhook-listener:latest
```

Note that we are forcing the use of `www-data` as the user. This is because we need permissions for `/var/www` that's on the host operating system. Adjust accordingly. 

You can also use a `docker-compose.yaml`:

```yaml
version: '3.3'

services:
  github-webhook-listener:
    container_name: github-webhook-listener
    image: ghcr.io/alexandru/github-webhook-listener:latest
    restart: unless-stopped
    ports:
      - "8080:8080"
    networks:
      - main
    volumes:
      - /var/www:/var/www
      - /etc/github-webhook-listener/config.yaml:/opt/app/config/config.yaml
    user: "${WWW_UID}:${WWW_GID}"

networks:
  main:
    external:
      name: main
```

Then to expose this server via [Nginx](https://www.nginx.com/), it's just a matter of configuring a `proxy_pass`:

```conf
location / {
  proxy_pass http://127.0.0.1:8080;
  proxy_set_header    Host            $host;
  proxy_set_header    X-Real-IP       $remote_addr;
  proxy_set_header    X-Forwarded-for $remote_addr;
  proxy_connect_timeout 300;
}
```

### Configuring Your GitHub Project

Go to the settings page of your project, the "Webhooks" section, link
should be like: `https://github.com/<user>/<project>/settings/hooks`

Setup screen for adding a new Webhook should look like this:

![Webhook setup screen](https://github.com/alexandru/github-webhook-listener/wiki/setup.png)

NOTEs on those fields:

1. the Payload URL contains a `some-id`, in the described path, that should be configured in your `config.yaml` file to identify your project
2. the Secret is the passphrase you also configured in `config.yaml` — this is optional, but if the `config.yaml` mentions a passphrase which you're not mentioning in this setup, then requests will fail

## Development

The project uses [Kotlin](https://kotlinlang.org/) as the programming language, with [Ktor](https://ktor.io/). And the setup is optimized for [GraalVM's Native Image](https://www.graalvm.org/22.2/reference-manual/native-image/).

To run the project in development mode:

```sh
./gradlew run -Pdevelopment --args="./config/application-dummy.conf"
```

To run after adding new dependencies:

```sh
./gradlew refreshVersionsMigrate  --mode=VersionCatalogOnly
```

To update project dependencies:

```sh
./gradlew refreshVersions
```

To build the Docker image for the JVM version from scratch:

```sh 
make build-jvm
```

Or the native version:

```sh
make build-native
```

### Issues with native-image

- [Kotlinx Serialization with GraalVM Native Images](https://github.com/Kotlin/kotlinx.serialization/issues/1125)

### Binary Size Optimization

The native executable is compressed using [UPX (Ultimate Packer for eXecutables)](https://upx.github.io/) to reduce its size. UPX is integrated into the Docker build process (`Dockerfile.native`) and automatically compresses the GraalVM native image binary after compilation.

The compression uses the `--best --lzma` flags for maximum compression, typically reducing the executable size by 60-70% while maintaining full functionality. The compressed binary is self-extracting and has minimal runtime overhead.

## License

Copyright © 2018-2022 Alexandru Nedelcu, some rights reserved.

Licensed under the AGPL-3.0 license. See [LICENSE](./LICENSE).
