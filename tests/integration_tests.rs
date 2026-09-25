use anyhow::{Result as TestResult, anyhow};
use github_webhook_listener::{
    AppConfig,
    config::{HttpConfig, ProjectConfig},
};
use hex::encode;
use hmac::{Hmac, KeyInit, Mac};
use reqwest::{Client, StatusCode};
use sha1::Sha1;
use sha2::Sha256;
use std::collections::HashMap;
use std::path::Path;
use std::time::Duration;
use tempfile::TempDir;
use tokio::{
    net::TcpListener,
    time::{sleep, timeout},
};

async fn start_test_server(mut config: AppConfig) -> TestResult<String> {
    let reserved = TcpListener::bind("127.0.0.1:0").await?;
    config.http.port = reserved.local_addr()?.port();
    drop(reserved);
    let addr = config.http.bind_address();
    let url = format!("http://{}", addr);

    let server = tokio::spawn(github_webhook_listener::server::start_server(config));
    let client = Client::new();
    timeout(Duration::from_secs(2), async {
        loop {
            if server.is_finished() {
                return Err(anyhow!(
                    "server exited before readiness: {:?}",
                    server.await
                ));
            }
            if client.get(format!("{}/", url)).send().await.is_ok() {
                return Ok(());
            }
            sleep(Duration::from_millis(10)).await;
        }
    })
    .await??;

    Ok(url)
}

async fn wait_for_file(path: &Path) -> TestResult<()> {
    timeout(Duration::from_secs(2), async {
        while !path.exists() {
            sleep(Duration::from_millis(10)).await;
        }
    })
    .await?;
    Ok(())
}

fn generate_hmac_sha256(body: &str, secret: &str) -> TestResult<String> {
    type HmacSha256 = Hmac<Sha256>;
    let mut mac = HmacSha256::new_from_slice(secret.as_bytes())
        .map_err(|error| anyhow!("invalid HMAC key: {error}"))?;
    mac.update(body.as_bytes());
    let result = mac.finalize();
    Ok(format!("sha256={}", encode(result.into_bytes())))
}

fn generate_hmac_sha1(body: &str, secret: &str) -> TestResult<String> {
    type HmacSha1 = Hmac<Sha1>;
    let mut mac = HmacSha1::new_from_slice(secret.as_bytes())
        .map_err(|error| anyhow!("invalid HMAC key: {error}"))?;
    mac.update(body.as_bytes());
    let result = mac.finalize();
    Ok(format!("sha1={}", encode(result.into_bytes())))
}

#[tokio::test]
async fn test_root_endpoint() -> TestResult<()> {
    let _temp_dir = TempDir::new()?;

    let config = AppConfig {
        http: HttpConfig {
            port: 0,
            host: Some("127.0.0.1".to_string()),
            path: None,
        },
        projects: HashMap::new(),
    };

    let base_url = start_test_server(config).await?;
    let client = Client::new();

    let response = client.get(format!("{}/", base_url)).send().await?;

    assert_eq!(response.status(), StatusCode::OK);
    let body = response.text().await?;
    assert!(body.contains("GitHub Webhook Listener"));
    Ok(())
}

#[tokio::test]
async fn test_webhook_with_sha256_authentication() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let dir_path = temp_dir
        .path()
        .to_str()
        .ok_or_else(|| anyhow!("temporary directory path is not valid UTF-8"))?
        .to_string();

    let mut projects = HashMap::new();
    projects.insert(
        "test-project".to_string(),
        ProjectConfig {
            git_ref: "refs/heads/gh-pages".to_string(),
            directory: dir_path.clone(),
            command: "touch i-was-here.txt".to_string(),
            secret: "test-secret-123".to_string(),
            action: None,
            timeout: Some(Duration::from_secs(5)),
        },
    );

    let config = AppConfig {
        http: HttpConfig {
            port: 0,
            host: Some("127.0.0.1".to_string()),
            path: None,
        },
        projects,
    };

    let base_url = start_test_server(config).await?;
    let client = Client::new();

    let payload = r#"{"action":"push","ref":"refs/heads/gh-pages"}"#;
    let signature = generate_hmac_sha256(payload, "test-secret-123")?;

    let response = client
        .post(format!("{}/test-project", base_url))
        .header("Content-Type", "application/json")
        .header("X-Hub-Signature-256", signature)
        .body(payload)
        .send()
        .await?;

    assert_eq!(response.status(), StatusCode::ACCEPTED);

    // Verify the file was created
    let test_file = temp_dir.path().join("i-was-here.txt");
    wait_for_file(&test_file).await?;
    Ok(())
}

#[tokio::test]
async fn test_webhook_with_sha1_authentication() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let dir_path = temp_dir
        .path()
        .to_str()
        .ok_or_else(|| anyhow!("temporary directory path is not valid UTF-8"))?
        .to_string();

    let mut projects = HashMap::new();
    projects.insert(
        "test-sha1".to_string(),
        ProjectConfig {
            git_ref: "refs/heads/main".to_string(),
            directory: dir_path.clone(),
            command: "touch sha1-test.txt".to_string(),
            secret: "sha1-secret".to_string(),
            action: None,
            timeout: Some(Duration::from_secs(5)),
        },
    );

    let config = AppConfig {
        http: HttpConfig {
            port: 0,
            host: Some("127.0.0.1".to_string()),
            path: None,
        },
        projects,
    };

    let base_url = start_test_server(config).await?;
    let client = Client::new();

    let payload = r#"{"action":"push","ref":"refs/heads/main"}"#;
    let signature = generate_hmac_sha1(payload, "sha1-secret")?;

    let response = client
        .post(format!("{}/test-sha1", base_url))
        .header("Content-Type", "application/json")
        .header("X-Hub-Signature", signature)
        .body(payload)
        .send()
        .await?;

    assert_eq!(response.status(), StatusCode::ACCEPTED);

    // Verify the file was created
    let test_file = temp_dir.path().join("sha1-test.txt");
    wait_for_file(&test_file).await?;
    Ok(())
}

#[tokio::test]
async fn test_reject_unauthenticated_request() -> TestResult<()> {
    let temp_dir = TempDir::new()?;

    let mut projects = HashMap::new();
    projects.insert(
        "secure-project".to_string(),
        ProjectConfig {
            git_ref: "refs/heads/main".to_string(),
            directory: temp_dir
                .path()
                .to_str()
                .ok_or_else(|| anyhow!("temporary directory path is not valid UTF-8"))?
                .to_string(),
            command: "echo test".to_string(),
            secret: "secret-key".to_string(),
            action: None,
            timeout: Some(Duration::from_secs(5)),
        },
    );

    let config = AppConfig {
        http: HttpConfig {
            port: 0,
            host: Some("127.0.0.1".to_string()),
            path: None,
        },
        projects,
    };

    let base_url = start_test_server(config).await?;
    let client = Client::new();

    let payload = r#"{"action":"push","ref":"refs/heads/main"}"#;

    let response = client
        .post(format!("{}/secure-project", base_url))
        .header("Content-Type", "application/json")
        .body(payload)
        .send()
        .await?;

    assert_eq!(response.status(), StatusCode::FORBIDDEN);
    Ok(())
}

#[tokio::test]
async fn test_nonexistent_project() -> TestResult<()> {
    let config = AppConfig {
        http: HttpConfig {
            port: 0,
            host: Some("127.0.0.1".to_string()),
            path: None,
        },
        projects: HashMap::new(),
    };

    let base_url = start_test_server(config).await?;
    let client = Client::new();

    let payload = r#"{"action":"push","ref":"refs/heads/main"}"#;
    let signature = generate_hmac_sha256(payload, "secret")?;

    let response = client
        .post(format!("{}/nonexistent", base_url))
        .header("Content-Type", "application/json")
        .header("X-Hub-Signature-256", signature)
        .body(payload)
        .send()
        .await?;

    assert_eq!(response.status(), StatusCode::NOT_FOUND);
    Ok(())
}

#[tokio::test]
async fn form_encoded_github_payload_runs_matching_command() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let project = ProjectConfig {
        git_ref: "refs/heads/main".to_string(),
        directory: temp_dir
            .path()
            .to_str()
            .ok_or_else(|| anyhow!("temporary directory path is not valid UTF-8"))?
            .to_string(),
        command: "touch form-delivered.txt".to_string(),
        secret: "form-secret".to_string(),
        action: None,
        timeout: Some(Duration::from_secs(5)),
    };
    let config = AppConfig {
        http: HttpConfig {
            port: 0,
            host: Some("127.0.0.1".to_string()),
            path: None,
        },
        projects: [("form-project".to_string(), project)].into(),
    };
    let url = start_test_server(config).await?;
    let payload = r#"{"ref":"refs/heads/main"}"#;
    let body = serde_urlencoded::to_string([("payload", payload)])?;
    let response = Client::new()
        .post(format!("{}/form-project", url))
        .header("Content-Type", "application/x-www-form-urlencoded")
        .header(
            "X-Hub-Signature-256",
            generate_hmac_sha256(&body, "form-secret")?,
        )
        .body(body)
        .send()
        .await?;

    assert!(response.status().is_success());
    wait_for_file(&temp_dir.path().join("form-delivered.txt")).await?;
    Ok(())
}

#[tokio::test]
async fn webhook_acknowledges_before_command_finishes() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let project = ProjectConfig {
        git_ref: "refs/heads/main".to_string(),
        directory: temp_dir
            .path()
            .to_str()
            .ok_or_else(|| anyhow!("temporary directory path is not valid UTF-8"))?
            .to_string(),
        command: "sleep 0.5; touch async-delivered.txt".to_string(),
        secret: "async-secret".to_string(),
        action: None,
        timeout: Some(Duration::from_secs(2)),
    };
    let url = start_test_server(AppConfig {
        http: HttpConfig {
            port: 0,
            host: Some("127.0.0.1".to_string()),
            path: None,
        },
        projects: [("async-project".to_string(), project)].into(),
    })
    .await?;
    let payload = r#"{"ref":"refs/heads/main"}"#;
    let response = timeout(
        Duration::from_millis(300),
        Client::new()
            .post(format!("{}/async-project", url))
            .header("Content-Type", "application/json")
            .header(
                "X-Hub-Signature-256",
                generate_hmac_sha256(payload, "async-secret")?,
            )
            .body(payload)
            .send(),
    )
    .await??;

    assert_eq!(response.status(), StatusCode::ACCEPTED);
    wait_for_file(&temp_dir.path().join("async-delivered.txt")).await?;
    Ok(())
}

#[tokio::test]
async fn queued_commands_run_one_at_a_time_across_projects() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let dir_path = temp_dir
        .path()
        .to_str()
        .ok_or_else(|| anyhow!("temporary directory path is not valid UTF-8"))?
        .to_string();
    let make_project = |command: &str| ProjectConfig {
        git_ref: "refs/heads/main".to_string(),
        directory: dir_path.clone(),
        command: command.to_string(),
        secret: "serial-secret".to_string(),
        action: None,
        timeout: Some(Duration::from_secs(2)),
    };
    let url = start_test_server(AppConfig {
        http: HttpConfig {
            port: 0,
            host: Some("127.0.0.1".to_string()),
            path: None,
        },
        projects: [
            (
                "first".to_string(),
                make_project("touch first-started; sleep 0.5; touch first-finished"),
            ),
            ("second".to_string(), make_project("touch second-started")),
        ]
        .into(),
    })
    .await?;
    let payload = r#"{"ref":"refs/heads/main"}"#;
    let client = Client::new();
    for key in ["first", "second"] {
        let response = client
            .post(format!("{}/{}", url, key))
            .header(
                "X-Hub-Signature-256",
                generate_hmac_sha256(payload, "serial-secret")?,
            )
            .body(payload)
            .send()
            .await?;
        assert_eq!(response.status(), StatusCode::ACCEPTED);
        if key == "first" {
            wait_for_file(&temp_dir.path().join("first-started")).await?;
        }
    }

    sleep(Duration::from_millis(100)).await;
    assert!(!temp_dir.path().join("second-started").exists());
    wait_for_file(&temp_dir.path().join("second-started")).await?;
    assert!(temp_dir.path().join("first-finished").exists());
    Ok(())
}

#[tokio::test]
async fn full_queue_rejects_new_delivery_without_waiting() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let project = ProjectConfig {
        git_ref: "refs/heads/main".to_string(),
        directory: temp_dir
            .path()
            .to_str()
            .ok_or_else(|| anyhow!("temporary directory path is not valid UTF-8"))?
            .to_string(),
        command: "touch worker-started; sleep 3".to_string(),
        secret: "queue-secret".to_string(),
        action: None,
        timeout: Some(Duration::from_secs(5)),
    };
    let url = start_test_server(AppConfig {
        http: HttpConfig {
            port: 0,
            host: Some("127.0.0.1".to_string()),
            path: None,
        },
        projects: [("busy".to_string(), project)].into(),
    })
    .await?;
    let payload = r#"{"ref":"refs/heads/main"}"#;
    let client = Client::new();
    let signature = generate_hmac_sha256(payload, "queue-secret")?;
    let send = || {
        client
            .post(format!("{}/busy", url))
            .header("X-Hub-Signature-256", signature.clone())
            .body(payload)
            .send()
    };

    assert_eq!(send().await?.status(), StatusCode::ACCEPTED);
    wait_for_file(&temp_dir.path().join("worker-started")).await?;
    for _ in 0..64 {
        assert_eq!(send().await?.status(), StatusCode::ACCEPTED);
    }
    let rejected = timeout(Duration::from_millis(500), send()).await??;
    assert_eq!(rejected.status(), StatusCode::SERVICE_UNAVAILABLE);
    Ok(())
}

#[tokio::test]
async fn failed_command_does_not_stop_worker_or_expose_output() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let dir_path = temp_dir
        .path()
        .to_str()
        .ok_or_else(|| anyhow!("temporary directory path is not valid UTF-8"))?
        .to_string();
    let make_project = |command: &str| ProjectConfig {
        git_ref: "refs/heads/main".to_string(),
        directory: dir_path.clone(),
        command: command.to_string(),
        secret: "failure-secret".to_string(),
        action: None,
        timeout: Some(Duration::from_secs(2)),
    };
    let url = start_test_server(AppConfig {
        http: HttpConfig {
            port: 0,
            host: Some("127.0.0.1".to_string()),
            path: None,
        },
        projects: [
            (
                "fails".to_string(),
                make_project("echo private-output >&2; touch failed-started; exit 1"),
            ),
            ("succeeds".to_string(), make_project("touch recovered")),
        ]
        .into(),
    })
    .await?;
    let payload = r#"{"ref":"refs/heads/main"}"#;
    let client = Client::new();
    for key in ["fails", "succeeds"] {
        let response = client
            .post(format!("{}/{}", url, key))
            .header(
                "X-Hub-Signature-256",
                generate_hmac_sha256(payload, "failure-secret")?,
            )
            .body(payload)
            .send()
            .await?;
        assert_eq!(response.status(), StatusCode::ACCEPTED);
        assert!(!response.text().await?.contains("private-output"));
    }

    wait_for_file(&temp_dir.path().join("recovered")).await?;
    assert!(temp_dir.path().join("failed-started").exists());
    Ok(())
}
