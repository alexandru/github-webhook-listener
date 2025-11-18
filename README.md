# GitHub Webhook Listener (ver. 3)

[![Build](https://github.com/alexandru/github-webhook-listener/actions/workflows/build.yml/badge.svg?branch=v3-rust)](https://github.com/alexandru/github-webhook-listener/actions/workflows/build.yml?query=branch%3Av3-rust) [![Deploy](https://github.com/alexandru/github-webhook-listener/actions/workflows/deploy.yml/badge.svg)](https://github.com/alexandru/github-webhook-listener/actions/workflows/deploy.yml)

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
> This project has been rewritten in Rust (ver. 3.0). Previous versions were written in Kotlin (see [v2-kotlin](https://github.com/alexandru/github-webhook-listener/tree/v2-kotlin) branch) and Haskell (see [v1-haskell](https://github.com/alexandru/github-webhook-listener/tree/v1-haskell) branch). 

## Setup

Docker images are published via [GitHub's Packages](https://github.com/alexandru/github-webhook-listener/pkgs/container/github-webhook-listener). You can quickly run a process like this:

```sh
docker run \
  -p 8080:8080 \
  -ti ghcr.io/alexandru/github-webhook-listener:latest
```

The Docker image contains a statically-linked Rust binary that is highly optimized for size and performance, using less than 10 MB of RAM in typical usage.

### Server Configuration

The server supports both YAML and HOCON configuration formats. The format is automatically detected based on the file extension (`.yaml`/`.yml` for YAML, `.conf`/`.hocon` for HOCON).

#### YAML Configuration

Create your `./config.yaml`:

```yaml
http:
  path: "/"
  port: 8080

projects:
  myproject:
    ref: "refs/heads/gh-pages"
    directory: "/var/www/myproject"
    command: "git pull"
    timeout: "30s"
    secret: "xxxxxxxxxxxxxxxxxxxxxxxxxx"
```

#### HOCON Configuration

Alternatively, create your `./config.conf`:

```hocon
http {
  path: "/"
  port: 8080
}

projects {
  myproject {
    ref: "refs/heads/gh-pages"
    directory: "/var/www/myproject"
    command: "git pull"
    timeout: "PT30S"
    secret: "xxxxxxxxxxxxxxxxxxxxxxxxxx"
  }
}
```

Notes:

1. `myproject` in `project.myproject` is just a name of a project, it could be anything;
2. `ref` says to only react on pushes to the `gh-pages` branch;
3. `directory` is where the `command` should be executed;
4. `command` is to be executed — note that `git` is not installed, see below;
5. `timeout` can be specified in humantime format (e.g., "5s", "30s") for YAML or ISO 8601 format (e.g., "PT5S", "PT30S") for HOCON;

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

The project is written in [Rust](https://www.rust-lang.org/). To install the toolchain, which must include [Cargo](https://doc.rust-lang.org/cargo/), see [rustup](https://rustup.rs/).

The procedure for macOS:

```bash
brew install rustup-init

rustup-init -y
rustup install stable
rustup default stable
rustup update
rustup component add \
  clippy \
  rust-analyzer \
  rust-docs \
  rustfmt
```

### Useful commands

Running in development mode:

```sh
cargo run -- ./config/application-dummy.yaml
```

Running tests:

```sh
cargo test
```

Building for production:

```sh
cargo build --release
```

The optimized binary will be located at `target/release/github-webhook-listener`.

Building the Docker image:

```sh
make build-docker-local
```

Running the Docker image:

```sh
make run-docker
```

## License

Copyright © 2018-2025 Alexandru Nedelcu, some rights reserved.

Licensed under the AGPL-3.0 license. See [LICENSE](./LICENSE).
