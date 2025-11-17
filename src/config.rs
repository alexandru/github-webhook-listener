use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::time::Duration;

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct AppConfig {
    pub http: HttpConfig,
    pub projects: HashMap<String, ProjectConfig>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct HttpConfig {
    pub port: u16,
    #[serde(default)]
    pub host: Option<String>,
    #[serde(default)]
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
    #[serde(default)]
    pub action: Option<String>,
    #[serde(default, with = "humantime_serde")]
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
    pub fn from_yaml_file<P: AsRef<Path>>(path: P) -> crate::Result<Self> {
        let contents = fs::read_to_string(path)?;
        Self::from_yaml_str(&contents)
    }

    pub fn from_yaml_str(yaml: &str) -> crate::Result<Self> {
        let config: AppConfig = serde_yaml::from_str(yaml)?;
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
}
