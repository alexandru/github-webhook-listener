use crate::config::ProjectConfig;
use crate::error::{AppError, Result};
use nix::sys::signal::{Signal, killpg};
use nix::unistd::Pid;
use std::collections::HashMap;
use std::io::Result as IoResult;
use std::os::unix::process::CommandExt;
use std::path::Path;
use std::process::Stdio;
use std::sync::Arc;
use tokio::io::{AsyncRead, AsyncReadExt};
use tokio::process::Command;
use tokio::sync::Mutex;
use tokio::time::timeout;
use tokio::try_join;
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

impl CommandTrigger {
    async fn get_lock(&self, key: &str) -> SharedLock<()> {
        let mut locks = self.locks.lock().await;
        locks
            .entry(key.to_string())
            .or_insert_with(|| Arc::new(Mutex::new(())))
            .clone()
    }
}

type SharedLock<T> = Arc<Mutex<T>>;

const MAX_CAPTURED_OUTPUT_BYTES: usize = 64 * 1024;

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

    let mut shell = Command::new("/bin/sh");
    shell
        .arg("-c")
        .arg(command)
        .current_dir(dir_path)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .kill_on_drop(true);
    shell.as_std_mut().process_group(0);
    let mut child = shell
        .spawn()
        .map_err(|e| AppError::Internal(format!("Failed to spawn command: {}", e)))?;
    let mut group = ProcessGroupGuard(child.id().map(|id| Pid::from_raw(id as i32)));

    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| AppError::Internal("Failed to capture command stdout".to_string()))?;
    let stderr = child
        .stderr
        .take()
        .ok_or_else(|| AppError::Internal("Failed to capture command stderr".to_string()))?;
    let (stdout, stderr, status) =
        try_join!(capture_output(stdout), capture_output(stderr), child.wait(),)
            .map_err(|e| AppError::Internal(format!("Failed to wait for command: {}", e)))?;
    group.0 = None;

    Ok(CommandResult {
        exit_code: status.code().unwrap_or(-1),
        stdout,
        stderr,
    })
}

async fn capture_output<R: AsyncRead + Unpin>(mut stream: R) -> IoResult<String> {
    let mut output = Vec::new();
    let mut buffer = [0; 8192];
    let mut truncated = false;
    loop {
        let count = stream.read(&mut buffer).await?;
        if count == 0 {
            break;
        }
        let remaining = MAX_CAPTURED_OUTPUT_BYTES.saturating_sub(output.len());
        output.extend_from_slice(&buffer[..count.min(remaining)]);
        truncated |= count > remaining;
    }
    let mut result = String::from_utf8_lossy(&output).into_owned();
    if truncated {
        result.push_str("\n[output truncated]");
    }
    Ok(result)
}

struct ProcessGroupGuard(Option<Pid>);

impl Drop for ProcessGroupGuard {
    fn drop(&mut self) {
        if let Some(group) = self.0
            && let Err(err) = killpg(group, Signal::SIGKILL)
        {
            debug!("Could not stop command process group {}: {}", group, err);
        }
    }
}

#[cfg(test)]
#[path = "../unit-tests/command.rs"]
mod tests;
