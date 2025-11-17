# Build stage
FROM rust:1.82-slim as builder

WORKDIR /build

# Install build dependencies
RUN apt-get update && apt-get install -y \
    pkg-config \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy manifests
COPY Cargo.toml Cargo.lock ./

# Copy source code
COPY src ./src

# Build the application
RUN cargo build --release

# Runtime stage
FROM debian:bookworm-slim

WORKDIR /opt/app

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Copy the binary from builder
COPY --from=builder /build/target/release/github-webhook-listener /opt/app/github-webhook-listener

# Create config directory
RUN mkdir -p /opt/app/config

# Set the entrypoint
ENTRYPOINT ["/opt/app/github-webhook-listener"]
CMD ["/opt/app/config/config.yaml"]
