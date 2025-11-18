use crate::config::ProjectConfig;
use crate::error::{AppError, Result};
use std::collections::HashMap;
use std::path::Path;
use std::process::Stdio;
use std::sync::Arc;
use std::time::Duration;
use tokio::io::AsyncReadExt;
use tokio::process::Command;
use tokio::sync::Mutex;
use tokio::time::timeout;
use tracing::{debug, error, info, warn};

/// Maximum time to wait for acquiring a lock (prevents deadlocks)
const LOCK_ACQUISITION_TIMEOUT: Duration = Duration::from_secs(30);

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

/// Manages command execution with per-project locking to prevent concurrent runs
pub struct CommandTrigger {
    projects: HashMap<String, ProjectConfig>,
    locks: Arc<Mutex<HashMap<String, Arc<Mutex<()>>>>>,
}

impl CommandTrigger {
    pub fn new(projects: HashMap<String, ProjectConfig>) -> Self {
        Self {
            projects,
            locks: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    async fn get_lock(&self, key: &str) -> Arc<Mutex<()>> {
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
        
        // Acquire lock with timeout to prevent indefinite blocking
        let _guard = timeout(LOCK_ACQUISITION_TIMEOUT, lock.lock())
            .await
            .map_err(|_| {
                warn!(
                    "Failed to acquire lock for project `{}` within {:?}",
                    key, LOCK_ACQUISITION_TIMEOUT
                );
                AppError::Timeout(format!(
                    "Failed to acquire lock within {:?}. Another command may be running.",
                    LOCK_ACQUISITION_TIMEOUT
                ))
            })?;

        info!("Executing command for project `{}`", key);

        let result = timeout(
            timeout_duration,
            execute_shell_command(&project.command, &project.directory),
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

    // Add timeout to wait operation to prevent hanging indefinitely
    let status = timeout(Duration::from_secs(60), child.wait())
        .await
        .map_err(|_| {
            error!("Child process wait timed out after 60 seconds");
            AppError::Timeout("Process wait timed out".to_string())
        })?
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

    #[tokio::test]
    async fn test_command_timeout() {
        let temp_dir = TempDir::new().unwrap();
        let dir_path = temp_dir.path().to_str().unwrap().to_string();

        let mut projects = HashMap::new();
        projects.insert(
            "slow-project".to_string(),
            ProjectConfig {
                git_ref: "refs/heads/main".to_string(),
                directory: dir_path,
                command: "sleep 10".to_string(),
                secret: "secret".to_string(),
                action: None,
                timeout: Some(Duration::from_secs(1)), // Short timeout
            },
        );

        let trigger = CommandTrigger::new(projects);
        let result = trigger.trigger_command("slow-project").await;

        assert!(result.is_err());
        if let Err(AppError::Timeout(msg)) = result {
            assert!(msg.contains("timed-out"));
        } else {
            panic!("Expected timeout error");
        }
    }

    #[tokio::test]
    async fn test_concurrent_command_execution() {
        let temp_dir = TempDir::new().unwrap();
        let dir_path = temp_dir.path().to_str().unwrap().to_string();

        let mut projects = HashMap::new();
        projects.insert(
            "concurrent-project".to_string(),
            ProjectConfig {
                git_ref: "refs/heads/main".to_string(),
                directory: dir_path,
                command: "sleep 2 && echo 'done'".to_string(),
                secret: "secret".to_string(),
                action: None,
                timeout: Some(Duration::from_secs(5)),
            },
        );

        let trigger = Arc::new(CommandTrigger::new(projects));

        // Spawn two concurrent tasks for the same project
        let trigger1 = trigger.clone();
        let task1 = tokio::spawn(async move {
            trigger1.trigger_command("concurrent-project").await
        });

        let trigger2 = trigger.clone();
        let task2 = tokio::spawn(async move {
            // Small delay to ensure task1 acquires the lock first
            tokio::time::sleep(Duration::from_millis(100)).await;
            trigger2.trigger_command("concurrent-project").await
        });

        let result1 = task1.await.unwrap();
        let result2 = task2.await.unwrap();

        // Both should complete successfully (second one waits for first)
        assert!(result1.is_ok());
        assert!(result2.is_ok());
    }

    #[tokio::test]
    async fn test_lock_acquisition_timeout() {
        let temp_dir = TempDir::new().unwrap();
        let dir_path = temp_dir.path().to_str().unwrap().to_string();

        let mut projects = HashMap::new();
        projects.insert(
            "locked-project".to_string(),
            ProjectConfig {
                git_ref: "refs/heads/main".to_string(),
                directory: dir_path,
                command: "sleep 35".to_string(), // Longer than lock timeout
                secret: "secret".to_string(),
                action: None,
                timeout: Some(Duration::from_secs(60)), // Long command timeout
            },
        );

        let trigger = Arc::new(CommandTrigger::new(projects));

        // Start a long-running command
        let trigger1 = trigger.clone();
        let task1 = tokio::spawn(async move {
            trigger1.trigger_command("locked-project").await
        });

        // Try to run another command immediately
        let trigger2 = trigger.clone();
        let task2 = tokio::spawn(async move {
            tokio::time::sleep(Duration::from_millis(100)).await;
            trigger2.trigger_command("locked-project").await
        });

        let _result1 = task1.await.unwrap(); // This should complete
        let result2 = task2.await.unwrap(); // This should timeout waiting for lock

        // Second task should timeout on lock acquisition
        assert!(result2.is_err());
        if let Err(AppError::Timeout(msg)) = result2 {
            assert!(msg.contains("acquire lock"));
        } else {
            panic!("Expected lock acquisition timeout error, got: {:?}", result2);
        }
    }
}
