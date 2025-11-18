# Build stage
FROM rust:1-slim-trixie AS builder

WORKDIR /build

# Install build dependencies
RUN apt-get update && apt-get install -y \
    pkg-config \
    libssl-dev \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Copy only dependency files first
COPY Cargo.toml Cargo.lock ./

# Create a dummy src/main.rs to build dependencies
RUN mkdir src && \
    echo "fn main() {}" > src/main.rs && \
    cargo build --release && \
    rm -rf src

# Copy all source files
COPY . .

# Build the application (dependencies are already cached)
RUN cargo build --release

# Runtime stage
FROM debian:trixie-slim

WORKDIR /opt/app

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Copy the binary from builder
COPY --from=builder /build/target/release/github-webhook-listener /opt/app/github-webhook-listener

# Create config directory
RUN mkdir -p /opt/app/config
COPY --from=builder /build/config/application-dummy.yaml /opt/app/config/config.yaml

# Set the entrypoint
ENTRYPOINT ["/opt/app/github-webhook-listener"]
CMD ["/opt/app/config/config.yaml"]
