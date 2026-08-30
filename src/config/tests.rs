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
    assert_eq!(project.git_ref, "refs/heads/gh-pages");
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
    assert_eq!(project.git_ref, "refs/heads/gh-pages");
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
