use super::*;
use anyhow::{Result as TestResult, anyhow};
use std::time::Duration;
use tempfile::TempDir;
use tokio::time::sleep;

#[tokio::test]
async fn test_execute_shell_command() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let dir_path = temp_dir
        .path()
        .to_str()
        .ok_or_else(|| anyhow!("temporary directory path is not valid UTF-8"))?;

    let result = execute_shell_command("echo 'Hello, World!'", dir_path).await?;
    assert!(result.is_successful());
    assert!(result.stdout.contains("Hello, World!"));
    Ok(())
}

#[tokio::test]
async fn test_execute_command_creates_file() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let dir_path = temp_dir
        .path()
        .to_str()
        .ok_or_else(|| anyhow!("temporary directory path is not valid UTF-8"))?;
    let test_file = temp_dir.path().join("test-file.txt");

    let result = execute_shell_command("touch test-file.txt", dir_path).await?;
    assert!(result.is_successful());
    assert!(test_file.exists());
    Ok(())
}

#[tokio::test]
async fn test_command_trigger() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let dir_path = temp_dir
        .path()
        .to_str()
        .ok_or_else(|| anyhow!("temporary directory path is not valid UTF-8"))?
        .to_string();
    let test_file = temp_dir.path().join("trigger-test.txt");

    let mut projects = HashMap::new();
    projects.insert(
        "test-project".to_string(),
        ProjectConfig {
            git_ref: "refs/heads/main".to_string(),
            directory: dir_path,
            command: "touch trigger-test.txt".to_string(),
            secret: "secret".to_string(),
            action: None,
            timeout: Some(Duration::from_secs(5)),
        },
    );

    let trigger = CommandTrigger::new(projects);
    trigger.trigger_command("test-project").await?;
    assert!(test_file.exists());
    Ok(())
}

#[tokio::test]
async fn timed_out_command_does_not_keep_running() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let mut projects = HashMap::new();
    projects.insert(
        "slow".to_string(),
        ProjectConfig {
            git_ref: "refs/heads/main".to_string(),
            directory: temp_dir
                .path()
                .to_str()
                .ok_or_else(|| anyhow!("temporary directory path is not valid UTF-8"))?
                .to_string(),
            command: "sleep 0.3; touch after-timeout.txt".to_string(),
            secret: "secret".to_string(),
            action: None,
            timeout: Some(Duration::from_millis(50)),
        },
    );

    let result = CommandTrigger::new(projects).trigger_command("slow").await;
    assert!(matches!(result, Err(AppError::Timeout(_))));
    sleep(Duration::from_millis(450)).await;
    assert!(!temp_dir.path().join("after-timeout.txt").exists());
    Ok(())
}

#[tokio::test]
async fn timed_out_shell_does_not_leave_subprocess_running() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let mut projects = HashMap::new();
    projects.insert(
        "slow-child".to_string(),
        ProjectConfig {
            git_ref: "refs/heads/main".to_string(),
            directory: temp_dir
                .path()
                .to_str()
                .ok_or_else(|| anyhow!("temporary directory path is not valid UTF-8"))?
                .to_string(),
            command: "(sleep 0.3; touch descendant-after-timeout.txt) & wait".to_string(),
            secret: "secret".to_string(),
            action: None,
            timeout: Some(Duration::from_millis(50)),
        },
    );

    let result = CommandTrigger::new(projects)
        .trigger_command("slow-child")
        .await;
    assert!(matches!(result, Err(AppError::Timeout(_))));
    sleep(Duration::from_millis(450)).await;
    assert!(
        !temp_dir
            .path()
            .join("descendant-after-timeout.txt")
            .exists()
    );
    Ok(())
}

#[tokio::test]
async fn command_with_full_stderr_pipe_can_complete() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let mut projects = HashMap::new();
    projects.insert(
        "noisy".to_string(),
        ProjectConfig {
            git_ref: "refs/heads/main".to_string(),
            directory: temp_dir
                .path()
                .to_str()
                .ok_or_else(|| anyhow!("temporary directory path is not valid UTF-8"))?
                .to_string(),
            command: "head -c 131072 /dev/zero >&2; touch completed.txt".to_string(),
            secret: "secret".to_string(),
            action: None,
            timeout: Some(Duration::from_secs(1)),
        },
    );

    CommandTrigger::new(projects)
        .trigger_command("noisy")
        .await?;
    assert!(temp_dir.path().join("completed.txt").exists());
    Ok(())
}

#[tokio::test]
async fn command_failure_limits_captured_output() -> TestResult<()> {
    let temp_dir = TempDir::new()?;
    let mut projects = HashMap::new();
    projects.insert(
        "verbose".to_string(),
        ProjectConfig {
            git_ref: "refs/heads/main".to_string(),
            directory: temp_dir
                .path()
                .to_str()
                .ok_or_else(|| anyhow!("temporary directory path is not valid UTF-8"))?
                .to_string(),
            command: "head -c 131072 /dev/zero >&2; exit 1".to_string(),
            secret: "secret".to_string(),
            action: None,
            timeout: Some(Duration::from_secs(2)),
        },
    );

    let error = CommandTrigger::new(projects)
        .trigger_command("verbose")
        .await
        .err()
        .ok_or_else(|| anyhow!("expected command to fail"))?
        .to_string();
    assert!(error.len() < 70 * 1024);
    assert!(error.contains("[output truncated]"));
    Ok(())
}
