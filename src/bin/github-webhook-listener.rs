use clap::Parser;
use github_webhook_listener::{server::start_server, AppConfig};
use std::path::PathBuf;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

#[derive(Parser, Debug)]
#[command(
    name = "github-webhook-listener",
    about = "A simple web app that can be registered as a GitHub Webhook and trigger shell commands"
)]
struct Args {
    /// Path to the application configuration file
    config_path: PathBuf,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Initialize tracing
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "github_webhook_listener=info,tower_http=debug".into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();

    // Parse command line arguments
    let args = Args::parse();

    // Load configuration
    let config = AppConfig::from_yaml_file(&args.config_path)?;

    // Start the server
    start_server(config).await?;

    Ok(())
}
