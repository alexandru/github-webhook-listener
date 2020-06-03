# github-webhook-listener

[![Build](https://github.com/alexandru/github-webhook-listener/workflows/build/badge.svg?branch=master)](https://github.com/alexandru/github-webhook-listener/actions?query=branch%3Amaster+workflow%3Abuild)

A simple web app that can be registered as a
[GitHub Webhook](https://developer.github.com/webhooks/)
and trigger shell commands in response to events.

Main use-case is to trigger refreshes of websites hosted on your own
server via [Travis-CI](https://travis-ci.org/) jobs, but in a secure
way, without exposing server credentials or SSH keys.

The server process is also light in resource usage, not using more
than 20 MB of RAM on a 64-bit Ubuntu machine, so it can be installed
on under-powered servers.

## Setup

Images are being pushed on [Docker Hub](https://hub.docker.com/repository/docker/alexelcu/github-webhook-listener) and you can quickly run the process like this:

```sh
docker run \
  -p 8080:8080 \
  -ti alexelcu/github-webhook-listener
```

### Server Configuration

On its own this just starts the server, but doesn't know how to do anything. We'll need to specify a configuration file: Create your `./config.yaml`:

```yaml
http:
  path: "/"
  port: 8080

runtime:
  workers: 2
  output: stdout

projects:
  myproject:
    ref: "refs/heads/gh-pages"
    directory: "/var/www/myproject"
    command: "git pull"
    secret: "xxxxxxxxxxxxxxxxxxxxxxxxxx"
```

Notes:

1. `myproject` in `project.myproject` is just a name of a project, it could be anything
2. `ref` says to only react on pushes to the `gh-pages` branch
3. `directory` is where the `command` should be executed
4. `command` is to be executed — note that `git` is already installed in the Docker image, doing `git pull` on a directory being the primary use case

It would be better if the `git pull` command would update files using a specified host user and group. And we'll also need an SSH key to install our "deployment key". So on your Linux box:

```
sudo adduser synchronize

sudo adduser synchronize www-data

sudo chown -R synchornize:www-data /var/www/myproject
```

And afterwards:

```sh
docker run \
  -p 8080:8080 \
  -v "$(pwd)/config.yaml:/opt/app/config/config.yaml" \
  -u "$(id -u synchronize):$(id -g synchronize)" \
  -ti alexelcu/github-webhook-listener
```

You could also use [docker-compose](https://docs.docker.com/compose/), here's what I have on my own server:

```yaml
version: '3.3'

services:
  github-webhook-listener:
    container_name: github-webhook-listener
    image: 'alexelcu/github-webhook-listener:latest'
    restart: unless-stopped
    ports:
      - "8080:8080"
    tty: true
    networks:
      - main
    volumes:
      - /var/www:/var/www
      - /etc/github-webhook-listener/config.yaml:/opt/app/config/config.yaml
    user: "${SYNC_UID}:${SYNC_GID}"

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

### Manual Setup (without Docker)

[Wiki instructions for Setup](https://github.com/alexandru/github-webhook-listener/wiki/Setup)

## License

Copyright © 2018-2020 Alexandru Nedelcu, some rights reserved.

Licensed under the 3-Clause BSD License. See [LICENSE](./LICENSE).
