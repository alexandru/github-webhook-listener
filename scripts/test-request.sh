#!/bin/bash
# Test script for GitHub webhook listener
# This script sends test requests to a running server

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR/test-request"

# Build and run the test client
cargo run --quiet
