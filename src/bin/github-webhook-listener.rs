use anyhow::Result;
use clap::Parser;
use github_webhook_listener::AppConfig;
use github_webhook_listener::server::start_server;
use std::path::PathBuf;
use tracing_subscriber::{EnvFilter, layer::SubscriberExt, util::SubscriberInitExt};

#[tokio::main]
async fn main() -> Result<()> {
    // `tracing_subscriber` turns `tracing::info!`/`debug!`/etc. calls made
    // throughout the app into actual log output. Respect RUST_LOG if set,
    // otherwise log this crate at info and HTTP request handling at debug,
    // so webhook traffic can be inspected without recompiling.
    tracing_subscriber::registry()
        .with(
            EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "github_webhook_listener=info,tower_http=debug".into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();

    let args = Args::parse();

    // Load configuration (auto-detects YAML or HOCON format)
    let config = AppConfig::from_file(&args.config_path)?;

    start_server(config).await?;
    Ok(())
}

#[derive(Parser, Debug)]
#[command(
    name = "github-webhook-listener",
    about = "A simple web app that can be registered as a GitHub Webhook and trigger shell commands"
)]
struct Args {
    /// Path to the application configuration file
    config_path: PathBuf,
}
