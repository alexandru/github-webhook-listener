use serde::{Deserialize, Deserializer, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::time::Duration;

// Custom deserializer for Option<String> that works with hocon-rs
fn option_string<'de, D>(deserializer: D) -> Result<Option<String>, D::Error>
where
    D: Deserializer<'de>,
{
    // First try to deserialize as Option<String>
    // If that fails, try to deserialize as String and wrap it
    #[derive(Deserialize)]
    #[serde(untagged)]
    enum StringOrOption {
        Opt(Option<String>),
        Str(String),
    }

    match StringOrOption::deserialize(deserializer)? {
        StringOrOption::Opt(o) => Ok(o),
        StringOrOption::Str(s) => Ok(Some(s)),
    }
}

// Custom duration deserializer that supports both humantime and ISO 8601 formats
mod duration_serde {
    use serde::{Deserialize, Deserializer};
    use std::time::Duration;

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Option<Duration>, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum StringOrOption {
            Opt(Option<String>),
            Str(String),
        }

        let s: Option<String> = match StringOrOption::deserialize(deserializer)? {
            StringOrOption::Opt(o) => o,
            StringOrOption::Str(s) => Some(s),
        };

        match s {
            None => Ok(None),
            Some(s) => {
                // Try humantime format first (e.g., "5s", "30s")
                if let Ok(duration) = humantime::parse_duration(&s) {
                    return Ok(Some(duration));
                }
                // Try ISO 8601 format (e.g., "PT5S", "PT30S")
                if let Ok(duration) = iso8601_duration::Duration::parse(&s) {
                    let seconds = duration.num_seconds().unwrap_or(0.0).max(0.0) as u64;
                    let std_duration = Duration::from_secs(seconds);
                    return Ok(Some(std_duration));
                }
                Err(serde::de::Error::custom(format!(
                    "Invalid duration format: {}. Expected humantime (e.g., '5s') or ISO 8601 (e.g., 'PT5S')",
                    s
                )))
            }
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct AppConfig {
    pub http: HttpConfig,
    pub projects: HashMap<String, ProjectConfig>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct HttpConfig {
    pub port: u16,
    #[serde(default, deserialize_with = "option_string")]
    pub host: Option<String>,
    #[serde(default, deserialize_with = "option_string")]
    pub path: Option<String>,
}

impl HttpConfig {
    pub fn base_path(&self) -> String {
        let path = self.path.as_deref().unwrap_or("");
        if path.ends_with('/') {
            path.trim_end_matches('/').to_string()
        } else {
            path.to_string()
        }
    }

    pub fn bind_address(&self) -> String {
        format!(
            "{}:{}",
            self.host.as_deref().unwrap_or("0.0.0.0"),
            self.port
        )
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ProjectConfig {
    #[serde(rename = "ref")]
    pub git_ref: String,
    pub directory: String,
    pub command: String,
    pub secret: String,
    #[serde(default, deserialize_with = "option_string")]
    pub action: Option<String>,
    #[serde(default, deserialize_with = "duration_serde::deserialize")]
    pub timeout: Option<Duration>,
}

impl ProjectConfig {
    pub fn ref_name(&self) -> &str {
        &self.git_ref
    }

    pub fn action_filter(&self) -> &str {
        self.action.as_deref().unwrap_or("push")
    }

    pub fn timeout_duration(&self) -> Duration {
        self.timeout.unwrap_or(Duration::from_secs(30))
    }
}

impl AppConfig {
    /// Load configuration from a file, auto-detecting the format based on extension
    pub fn from_file<P: AsRef<Path>>(path: P) -> crate::Result<Self> {
        let path = path.as_ref();
        let contents = fs::read_to_string(path)?;

        // Auto-detect format based on file extension
        if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
            match ext {
                "yaml" | "yml" => Self::from_yaml_str(&contents),
                "conf" | "hocon" => Self::from_hocon_str(&contents),
                _ => {
                    // Try YAML first, then HOCON
                    Self::from_yaml_str(&contents).or_else(|_| Self::from_hocon_str(&contents))
                }
            }
        } else {
            // No extension, try both formats
            Self::from_yaml_str(&contents).or_else(|_| Self::from_hocon_str(&contents))
        }
    }

    /// Parse YAML configuration (deprecated, use from_file)
    pub fn from_yaml_file<P: AsRef<Path>>(path: P) -> crate::Result<Self> {
        let contents = fs::read_to_string(path)?;
        Self::from_yaml_str(&contents)
    }

    pub fn from_yaml_str(yaml: &str) -> crate::Result<Self> {
        let config: AppConfig = serde_yaml::from_str(yaml)?;
        Ok(config)
    }

    /// Parse HOCON configuration
    pub fn from_hocon_file<P: AsRef<Path>>(path: P) -> crate::Result<Self> {
        let contents = fs::read_to_string(path)?;
        Self::from_hocon_str(&contents)
    }

    pub fn from_hocon_str(hocon: &str) -> crate::Result<Self> {
        // Parse HOCON and deserialize directly with serde
        let config: AppConfig = hocon_rs::Config::parse_str(hocon, None)
            .map_err(|e| crate::AppError::Internal(format!("HOCON parse error: {}", e)))?;
        Ok(config)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_yaml_config() {
        let yaml = r#"
http:
  host: "0.0.0.0"
  port: 8080
  path: "/"

projects:
  myproject:
    ref: "refs/heads/gh-pages"
    directory: "/tmp"
    command: "touch ./i-was-here.txt"
    timeout: "5s"
    secret: "xxxxxxxxxxxxxxxxxxxxxxxxxx"
"#;
        let config = AppConfig::from_yaml_str(yaml).unwrap();
        assert_eq!(config.http.port, 8080);
        assert_eq!(config.http.host, Some("0.0.0.0".to_string()));
        assert_eq!(config.projects.len(), 1);

        let project = config.projects.get("myproject").unwrap();
        assert_eq!(project.ref_name(), "refs/heads/gh-pages");
        assert_eq!(project.directory, "/tmp");
        assert_eq!(project.command, "touch ./i-was-here.txt");
        assert_eq!(project.secret, "xxxxxxxxxxxxxxxxxxxxxxxxxx");
        assert_eq!(project.timeout_duration(), Duration::from_secs(5));
    }

    #[test]
    fn test_base_path() {
        let config = HttpConfig {
            port: 8080,
            host: None,
            path: Some("/webhook/".to_string()),
        };
        assert_eq!(config.base_path(), "/webhook");

        let config2 = HttpConfig {
            port: 8080,
            host: None,
            path: Some("/webhook".to_string()),
        };
        assert_eq!(config2.base_path(), "/webhook");

        let config3 = HttpConfig {
            port: 8080,
            host: None,
            path: None,
        };
        assert_eq!(config3.base_path(), "");
    }

    #[test]
    fn test_parse_hocon_config() {
        let hocon = r#"
http {
  host: "0.0.0.0"
  port: 8080
  path: "/"
}

projects {
  myproject {
    action: "push"
    ref: "refs/heads/gh-pages"
    directory: "/tmp"
    command: "touch ./i-was-here.txt"
    timeout: "PT5S"
    secret: "xxxxxxxxxxxxxxxxxxxxxxxxxx"
  }
}
"#;
        let config = AppConfig::from_hocon_str(hocon).unwrap();
        assert_eq!(config.http.port, 8080);
        assert_eq!(config.http.host, Some("0.0.0.0".to_string()));
        assert_eq!(config.projects.len(), 1);

        let project = config.projects.get("myproject").unwrap();
        assert_eq!(project.ref_name(), "refs/heads/gh-pages");
        assert_eq!(project.directory, "/tmp");
        assert_eq!(project.command, "touch ./i-was-here.txt");
        assert_eq!(project.secret, "xxxxxxxxxxxxxxxxxxxxxxxxxx");
        assert_eq!(project.timeout_duration(), Duration::from_secs(5));
    }

    #[test]
    fn test_parse_yaml_with_humantime() {
        let yaml = r#"
http:
  host: "0.0.0.0"
  port: 8080
  path: "/"

projects:
  myproject:
    ref: "refs/heads/gh-pages"
    directory: "/tmp"
    command: "touch ./i-was-here.txt"
    timeout: "5s"
    secret: "xxxxxxxxxxxxxxxxxxxxxxxxxx"
"#;
        let config = AppConfig::from_yaml_str(yaml).unwrap();
        let project = config.projects.get("myproject").unwrap();
        assert_eq!(project.timeout_duration(), Duration::from_secs(5));
    }
}
