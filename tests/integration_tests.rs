use github_webhook_listener::{AppConfig, server::start_server};
use hmac::{Hmac, Mac};
use reqwest::{Client, StatusCode};
use sha1::Sha1;
use sha2::Sha256;
use std::time::Duration;
use tempfile::TempDir;
use tokio::time::sleep;

async fn start_test_server(config: AppConfig) -> String {
    let addr = config.http.bind_address();
    let url = format!("http://{}", addr);
    
    tokio::spawn(async move {
        start_server(config).await.ok();
    });
    
    // Give the server time to start
    sleep(Duration::from_millis(100)).await;
    
    url
}

fn generate_hmac_sha256(body: &str, secret: &str) -> String {
    type HmacSha256 = Hmac<Sha256>;
    let mut mac = HmacSha256::new_from_slice(secret.as_bytes()).unwrap();
    mac.update(body.as_bytes());
    let result = mac.finalize();
    format!("sha256={}", hex::encode(result.into_bytes()))
}

fn generate_hmac_sha1(body: &str, secret: &str) -> String {
    type HmacSha1 = Hmac<Sha1>;
    let mut mac = HmacSha1::new_from_slice(secret.as_bytes()).unwrap();
    mac.update(body.as_bytes());
    let result = mac.finalize();
    format!("sha1={}", hex::encode(result.into_bytes()))
}

#[tokio::test]
async fn test_root_endpoint() {
    let _temp_dir = TempDir::new().unwrap();
    
    let config = AppConfig {
        http: github_webhook_listener::config::HttpConfig {
            port: 9374,
            host: Some("127.0.0.1".to_string()),
            path: None,
        },
        projects: std::collections::HashMap::new(),
    };
    
    let base_url = start_test_server(config).await;
    let client = Client::new();
    
    let response = client
        .get(&format!("{}/", base_url))
        .send()
        .await
        .unwrap();
    
    assert_eq!(response.status(), StatusCode::OK);
    let body = response.text().await.unwrap();
    assert!(body.contains("GitHub Webhook Listener"));
}

#[tokio::test]
async fn test_webhook_with_sha256_authentication() {
    let temp_dir = TempDir::new().unwrap();
    let dir_path = temp_dir.path().to_str().unwrap().to_string();
    
    let mut projects = std::collections::HashMap::new();
    projects.insert(
        "test-project".to_string(),
        github_webhook_listener::config::ProjectConfig {
            git_ref: "refs/heads/gh-pages".to_string(),
            directory: dir_path.clone(),
            command: "touch i-was-here.txt".to_string(),
            secret: "test-secret-123".to_string(),
            action: None,
            timeout: Some(Duration::from_secs(5)),
        },
    );
    
    let config = AppConfig {
        http: github_webhook_listener::config::HttpConfig {
            port: 9375,
            host: Some("127.0.0.1".to_string()),
            path: None,
        },
        projects,
    };
    
    let base_url = start_test_server(config).await;
    let client = Client::new();
    
    let payload = r#"{"action":"push","ref":"refs/heads/gh-pages"}"#;
    let signature = generate_hmac_sha256(payload, "test-secret-123");
    
    let response = client
        .post(&format!("{}/test-project", base_url))
        .header("Content-Type", "application/json")
        .header("X-Hub-Signature-256", signature)
        .body(payload)
        .send()
        .await
        .unwrap();
    
    assert_eq!(response.status(), StatusCode::OK);
    
    // Verify the file was created
    sleep(Duration::from_millis(100)).await;
    let test_file = temp_dir.path().join("i-was-here.txt");
    assert!(test_file.exists());
}

#[tokio::test]
async fn test_webhook_with_sha1_authentication() {
    let temp_dir = TempDir::new().unwrap();
    let dir_path = temp_dir.path().to_str().unwrap().to_string();
    
    let mut projects = std::collections::HashMap::new();
    projects.insert(
        "test-sha1".to_string(),
        github_webhook_listener::config::ProjectConfig {
            git_ref: "refs/heads/main".to_string(),
            directory: dir_path.clone(),
            command: "touch sha1-test.txt".to_string(),
            secret: "sha1-secret".to_string(),
            action: None,
            timeout: Some(Duration::from_secs(5)),
        },
    );
    
    let config = AppConfig {
        http: github_webhook_listener::config::HttpConfig {
            port: 9376,
            host: Some("127.0.0.1".to_string()),
            path: None,
        },
        projects,
    };
    
    let base_url = start_test_server(config).await;
    let client = Client::new();
    
    let payload = r#"{"action":"push","ref":"refs/heads/main"}"#;
    let signature = generate_hmac_sha1(payload, "sha1-secret");
    
    let response = client
        .post(&format!("{}/test-sha1", base_url))
        .header("Content-Type", "application/json")
        .header("X-Hub-Signature", signature)
        .body(payload)
        .send()
        .await
        .unwrap();
    
    assert_eq!(response.status(), StatusCode::OK);
    
    // Verify the file was created
    sleep(Duration::from_millis(100)).await;
    let test_file = temp_dir.path().join("sha1-test.txt");
    assert!(test_file.exists());
}

#[tokio::test]
async fn test_reject_unauthenticated_request() {
    let temp_dir = TempDir::new().unwrap();
    
    let mut projects = std::collections::HashMap::new();
    projects.insert(
        "secure-project".to_string(),
        github_webhook_listener::config::ProjectConfig {
            git_ref: "refs/heads/main".to_string(),
            directory: temp_dir.path().to_str().unwrap().to_string(),
            command: "echo test".to_string(),
            secret: "secret-key".to_string(),
            action: None,
            timeout: Some(Duration::from_secs(5)),
        },
    );
    
    let config = AppConfig {
        http: github_webhook_listener::config::HttpConfig {
            port: 9377,
            host: Some("127.0.0.1".to_string()),
            path: None,
        },
        projects,
    };
    
    let base_url = start_test_server(config).await;
    let client = Client::new();
    
    let payload = r#"{"action":"push","ref":"refs/heads/main"}"#;
    
    let response = client
        .post(&format!("{}/secure-project", base_url))
        .header("Content-Type", "application/json")
        .body(payload)
        .send()
        .await
        .unwrap();
    
    assert_eq!(response.status(), StatusCode::FORBIDDEN);
}

#[tokio::test]
async fn test_nonexistent_project() {
    let config = AppConfig {
        http: github_webhook_listener::config::HttpConfig {
            port: 9378,
            host: Some("127.0.0.1".to_string()),
            path: None,
        },
        projects: std::collections::HashMap::new(),
    };
    
    let base_url = start_test_server(config).await;
    let client = Client::new();
    
    let payload = r#"{"action":"push","ref":"refs/heads/main"}"#;
    let signature = generate_hmac_sha256(payload, "secret");
    
    let response = client
        .post(&format!("{}/nonexistent", base_url))
        .header("Content-Type", "application/json")
        .header("X-Hub-Signature-256", signature)
        .body(payload)
        .send()
        .await
        .unwrap();
    
    assert_eq!(response.status(), StatusCode::NOT_FOUND);
}
