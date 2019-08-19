# github-webhook-listener

[![CircleCI](https://circleci.com/gh/alexandru/github-webhook-listener.svg?style=svg)](https://circleci.com/gh/alexandru/github-webhook-listener)

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

[Wiki instructions for Setup](https://github.com/alexandru/github-webhook-listener/wiki/Setup)

## License

Copyright ©2019 Alexandru Nedelcu

See [LICENSE](./LICENSE).
