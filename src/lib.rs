//! Webhook listener that runs configured shell commands when GitHub pushes
//! match a project's ref and action.

pub mod command;
pub mod config;
pub mod error;
pub mod event;
pub mod server;

pub use config::AppConfig;
pub use error::{AppError, Result};
