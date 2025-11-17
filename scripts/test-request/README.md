# Test Request Script

This is a test client for the GitHub Webhook Listener server. It sends various test requests to verify the server is working correctly.

## Usage

From the repository root:

```bash
./scripts/test-request.sh
```

Or directly with cargo:

```bash
cd scripts/test-request
cargo run
```

## What it tests

1. **GET /** - Verifies the root endpoint returns the list of configured projects
2. **POST /myproject** with SHA256 signature - Tests webhook with HMAC-SHA256 authentication
3. **POST /myproject** with SHA1 signature - Tests webhook with HMAC-SHA1 authentication  
4. **POST /notAvailable** - Tests error handling for non-existent projects

## Requirements

- A running instance of github-webhook-listener on localhost:8080
- The server must be configured with a project named "myproject" with secret "xxxxxxxxxxxxxxxxxxxxxxxxxx"
- The default configuration in `config/application-dummy.yaml` works out of the box
