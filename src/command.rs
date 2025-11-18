use crate::config::ProjectConfig;
use crate::error::{AppError, Result};
use std::collections::HashMap;
use std::path::Path;
use std::process::Stdio;
use std::sync::Arc;
use tokio::io::AsyncReadExt;
use tokio::process::Command;
use tokio::sync::Mutex;
use tokio::time::timeout;
use tracing::{debug, error, info};

/// Result type for command execution containing exit code and output
#[derive(Debug)]
pub struct CommandResult {
    pub exit_code: i32,
    pub stdout: String,
    pub stderr: String,
}

impl CommandResult {
    pub fn is_successful(&self) -> bool {
        self.exit_code == 0
    }
}

type SharedLock<T> = Arc<Mutex<T>>;

/// Manages command execution with per-project locking to prevent concurrent runs
pub struct CommandTrigger {
    projects: HashMap<String, ProjectConfig>,
    locks: SharedLock<HashMap<String, SharedLock<()>>>,
}

impl CommandTrigger {
    pub fn new(projects: HashMap<String, ProjectConfig>) -> Self {
        Self {
            projects,
            locks: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    async fn get_lock(&self, key: &str) -> SharedLock<()> {
        let mut locks = self.locks.lock().await;
        locks
            .entry(key.to_string())
            .or_insert_with(|| Arc::new(Mutex::new(())))
            .clone()
    }

    pub async fn trigger_command(&self, key: &str) -> Result<()> {
        let project = self
            .projects
            .get(key)
            .ok_or_else(|| AppError::NotFound(format!("Project `{}` does not exist", key)))?;

        let timeout_duration = project.timeout_duration();
        let lock = self.get_lock(key).await;

        info!("Executing command for project `{}`", key);

        let result = timeout(
            timeout_duration,
            execute_shell_command_locked(lock, &project.command, &project.directory),
        )
        .await
        .map_err(|_| {
            error!(
                "Command timed out for project `{}` after {:?}",
                key, timeout_duration
            );
            AppError::Timeout(format!(
                "Command execution timed-out after {:?}",
                timeout_duration
            ))
        })??;

        if result.is_successful() {
            info!("Command executed successfully for project `{}`", key);
            debug!("stdout: {}", result.stdout);
            Ok(())
        } else {
            error!(
                "Command failed for project `{}` with exit code {}: stderr={}",
                key, result.exit_code, result.stderr
            );
            Err(AppError::Internal(format!(
                "Command execution failed with exit code {}\nstdout: {}\nstderr: {}",
                result.exit_code, result.stdout, result.stderr
            )))
        }
    }
}

async fn execute_shell_command_locked(
    lock: SharedLock<()>,
    command: &str,
    directory: &str,
) -> Result<CommandResult> {
    let _guard = lock.lock().await;
    execute_shell_command(command, directory).await
}

async fn execute_shell_command(command: &str, directory: &str) -> Result<CommandResult> {
    let dir_path = Path::new(directory);
    if !dir_path.exists() {
        return Err(AppError::Internal(format!(
            "Directory does not exist: {}",
            directory
        )));
    }

    let mut child = Command::new("/bin/sh")
        .arg("-c")
        .arg(command)
        .current_dir(dir_path)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| AppError::Internal(format!("Failed to spawn command: {}", e)))?;

    let mut stdout = String::new();
    let mut stderr = String::new();

    if let Some(mut stdout_stream) = child.stdout.take() {
        stdout_stream
            .read_to_string(&mut stdout)
            .await
            .map_err(|e| AppError::Internal(format!("Failed to read stdout: {}", e)))?;
    }

    if let Some(mut stderr_stream) = child.stderr.take() {
        stderr_stream
            .read_to_string(&mut stderr)
            .await
            .map_err(|e| AppError::Internal(format!("Failed to read stderr: {}", e)))?;
    }

    let status = child
        .wait()
        .await
        .map_err(|e| AppError::Internal(format!("Failed to wait for command: {}", e)))?;

    Ok(CommandResult {
        exit_code: status.code().unwrap_or(-1),
        stdout,
        stderr,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[tokio::test]
    async fn test_execute_shell_command() {
        let temp_dir = TempDir::new().unwrap();
        let dir_path = temp_dir.path().to_str().unwrap();

        let result = execute_shell_command("echo 'Hello, World!'", dir_path)
            .await
            .unwrap();
        assert!(result.is_successful());
        assert!(result.stdout.contains("Hello, World!"));
    }

    #[tokio::test]
    async fn test_execute_command_creates_file() {
        let temp_dir = TempDir::new().unwrap();
        let dir_path = temp_dir.path().to_str().unwrap();
        let test_file = temp_dir.path().join("test-file.txt");

        let result = execute_shell_command("touch test-file.txt", dir_path)
            .await
            .unwrap();
        assert!(result.is_successful());
        assert!(test_file.exists());
    }

    #[tokio::test]
    async fn test_command_trigger() {
        let temp_dir = TempDir::new().unwrap();
        let dir_path = temp_dir.path().to_str().unwrap().to_string();
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
                timeout: Some(std::time::Duration::from_secs(5)),
            },
        );

        let trigger = CommandTrigger::new(projects);
        trigger.trigger_command("test-project").await.unwrap();
        assert!(test_file.exists());
    }
}
