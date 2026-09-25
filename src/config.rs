//! Configuration types and loaders for YAML and HOCON files.

use crate::error::{AppError, Result as AppResult};
use hocon_rs::Config as HoconConfig;
use serde::{Deserialize, Deserializer, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::time::Duration;

/// Configuration for the listener, loaded from a YAML or HOCON file.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct AppConfig {
    /// HTTP listener settings.
    pub http: HttpConfig,
    /// Commands to run, keyed by the project name used in the webhook URL.
    pub projects: HashMap<String, ProjectConfig>,
}

/// HTTP listener settings.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct HttpConfig {
    /// TCP port to listen on.
    pub port: u16,
    /// Interface to bind, defaulting to `0.0.0.0`.
    #[serde(default, deserialize_with = "option_string")]
    pub host: Option<String>,
    /// Base path prefixed to the webhook routes, defaulting to the root.
    #[serde(default, deserialize_with = "option_string")]
    pub path: Option<String>,
}

impl HttpConfig {
    /// Returns the configured base path without a trailing slash.
    pub fn base_path(&self) -> String {
        let path = self.path.as_deref().unwrap_or("");
        if path.ends_with('/') {
            path.trim_end_matches('/').to_string()
        } else {
            path.to_string()
        }
    }

    /// Returns the `host:port` address the server binds to.
    pub fn bind_address(&self) -> String {
        format!(
            "{}:{}",
            self.host.as_deref().unwrap_or("0.0.0.0"),
            self.port
        )
    }
}

/// Command to run for one project, plus the conditions that trigger it.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ProjectConfig {
    /// Git ref that triggers the command, such as `refs/heads/main`.
    #[serde(rename = "ref")]
    pub git_ref: String,
    /// Working directory for the command.
    pub directory: String,
    /// Shell command to run through `/bin/sh -c`.
    pub command: String,
    /// Secret used to verify the GitHub HMAC signature.
    pub secret: String,
    /// Action that triggers the command, defaulting to `push`.
    #[serde(default, deserialize_with = "option_string")]
    pub action: Option<String>,
    /// Command timeout, defaulting to 30 seconds.
    #[serde(default, deserialize_with = "duration_serde::deserialize")]
    pub timeout: Option<Duration>,
}

impl ProjectConfig {
    /// Returns the action that triggers this project, defaulting to `push`.
    pub fn action_filter(&self) -> &str {
        self.action.as_deref().unwrap_or("push")
    }

    /// Returns the command timeout, defaulting to 30 seconds.
    pub fn timeout_duration(&self) -> Duration {
        self.timeout.unwrap_or(Duration::from_secs(30))
    }
}

impl AppConfig {
    /// Loads configuration from a file, detecting YAML or HOCON from the file
    /// extension.
    ///
    /// # Errors
    ///
    /// Returns an error when the file cannot be read or parsed.
    pub fn from_file<P: AsRef<Path>>(path: P) -> AppResult<Self> {
        let path = path.as_ref();
        let contents = fs::read_to_string(path)?;

        if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
            match ext {
                "yaml" | "yml" => Self::from_yaml_str(&contents),
                "conf" | "hocon" => Self::from_hocon_str(&contents),
                _ => Self::from_yaml_str(&contents).or_else(|_| Self::from_hocon_str(&contents)),
            }
        } else {
            Self::from_yaml_str(&contents).or_else(|_| Self::from_hocon_str(&contents))
        }
    }

    /// Parses YAML configuration directly. Prefer `from_file`, which detects
    /// the format from the file extension.
    ///
    /// # Errors
    ///
    /// Returns an error when the file cannot be read or the YAML is invalid.
    pub fn from_yaml_file<P: AsRef<Path>>(path: P) -> AppResult<Self> {
        let contents = fs::read_to_string(path)?;
        Self::from_yaml_str(&contents)
    }

    /// Parses YAML text into a configuration.
    ///
    /// # Errors
    ///
    /// Returns an error when the YAML is invalid.
    pub fn from_yaml_str(yaml: &str) -> AppResult<Self> {
        let config: AppConfig = serde_yaml::from_str(yaml)?;
        Ok(config)
    }

    /// Parses a HOCON configuration file.
    ///
    /// # Errors
    ///
    /// Returns an error when the file cannot be read or the HOCON is invalid.
    pub fn from_hocon_file<P: AsRef<Path>>(path: P) -> AppResult<Self> {
        let contents = fs::read_to_string(path)?;
        Self::from_hocon_str(&contents)
    }

    /// Parses HOCON text into a configuration.
    ///
    /// # Errors
    ///
    /// Returns an error when the HOCON is invalid.
    pub fn from_hocon_str(hocon: &str) -> AppResult<Self> {
        let config: AppConfig = HoconConfig::parse_str(hocon, None)
            .map_err(|e| AppError::Internal(format!("hocon parse error: {}", e)))?;
        Ok(config)
    }
}

// Helper to deserialize values that can be either T or Option<T> (for hocon-rs
// compatibility)
#[derive(Deserialize)]
#[serde(untagged)]
enum ValueOrOption<T> {
    Value(T),
    Opt(Option<T>),
}

impl<T> From<ValueOrOption<T>> for Option<T> {
    fn from(value: ValueOrOption<T>) -> Self {
        match value {
            ValueOrOption::Value(v) => Some(v),
            ValueOrOption::Opt(o) => o,
        }
    }
}

// Custom deserializer for Option<String> that works with hocon-rs
fn option_string<'de, D>(deserializer: D) -> Result<Option<String>, D::Error>
where
    D: Deserializer<'de>,
{
    Ok(ValueOrOption::<String>::deserialize(deserializer)?.into())
}

// Custom duration deserializer that supports both humantime and ISO 8601
// formats
mod duration_serde {
    use super::ValueOrOption;
    use iso8601_duration::Duration as IsoDuration;
    use serde::de::Error as DeError;
    use serde::{Deserialize, Deserializer};
    use std::time::Duration;

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Option<Duration>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s: Option<String> = ValueOrOption::<String>::deserialize(deserializer)?.into();

        match s {
            None => Ok(None),
            Some(s) => {
                // Try humantime format first (e.g., "5s", "30s")
                if let Ok(duration) = humantime::parse_duration(&s) {
                    return Ok(Some(duration));
                }
                // Try ISO 8601 format (e.g., "PT5S", "PT0.5S")
                if let Ok(duration) = IsoDuration::parse(&s) {
                    return match duration
                        .num_seconds()
                        .and_then(|seconds| Duration::try_from_secs_f64(seconds.into()).ok())
                    {
                        Some(timeout) => Ok(Some(timeout)),
                        None => Err(DeError::custom(format!(
                            "cannot convert ISO 8601 duration to a timeout: {}",
                            s
                        ))),
                    };
                }
                Err(DeError::custom(format!(
                    "invalid duration format: {}; expected humantime (e.g., '5s') or ISO 8601 (e.g., 'PT5S')",
                    s
                )))
            }
        }
    }
}

#[cfg(test)]
#[path = "../unit-tests/config.rs"]
mod tests;
