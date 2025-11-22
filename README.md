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
> This version has been migrated to **Kotlin/Native** from the previous Kotlin/JVM + GraalVM Native Image implementation. The native binary is now compiled directly with Kotlin/Native for better memory efficiency and smaller binary size.
> 
> Previous versions: The original [v1-haskell](https://github.com/alexandru/github-webhook-listener/tree/v1-haskell) branch uses Haskell. There's also an experimental Rust branch, see [v3-rust](https://github.com/alexandru/github-webhook-listener/tree/v3-rust).

## Setup

Docker images are published via [GitHub's Packages](https://github.com/alexandru/github-webhook-listener/pkgs/container/github-webhook-listener). You can quickly run a process like this:

```sh
docker run \
  -p 8080:8080 \
  -ti ghcr.io/alexandru/github-webhook-listener:native-latest
```

The image contains a native executable compiled with Kotlin/Native, optimized for minimal memory usage (typically under 10 MB of RAM) and fast startup times.

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

The project uses [Kotlin/Native](https://kotlinlang.org/docs/native-overview.html) as the programming language, with [Ktor](https://ktor.io/) for the HTTP server. The setup is optimized for minimal memory usage and small binary size.

To run the project in development mode (requires Kotlin/Native toolchain):

```sh
./gradlew nativeCompile
./build/bin/native/releaseExecutable/github-webhook-listener.kexe ./config/application-dummy.yaml
```

To update project dependencies:

```sh
./gradlew dependencyUpdates
```

To build the Docker image:

```sh 
docker build -f ./src/nativeMain/docker/Dockerfile.native -t github-webhook-listener .
```

### Migration from JVM/GraalVM

This project was migrated from Kotlin/JVM with GraalVM Native Image to Kotlin/Native for:
- Better memory efficiency (native memory management)
- Smaller binary size (no JVM overhead)
- Faster startup times
- Direct native compilation without JVM intermediary

Key changes in the migration:
- Replaced JVM-specific libraries (Arrow, Logback, Commons) with native equivalents
- Replaced Java File I/O with POSIX-based native APIs
- Removed GraalVM Native Image configuration
- Simplified dependency management with Kotlin Multiplatform

### Issues with Kotlin/Native

- HMAC implementation uses a simplified approach - production deployments should use a proper crypto library
- YAML parsing is simplified - complex YAML files may not be fully supported

## License

Copyright © 2018-2022 Alexandru Nedelcu, some rights reserved.

Licensed under the AGPL-3.0 license. See [LICENSE](./LICENSE).
