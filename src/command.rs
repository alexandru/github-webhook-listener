//! Runs configured shell commands with per-project serialization, timeouts,
//! and bounded output capture.

use crate::config::ProjectConfig;
use crate::error::{AppError, Result};
use nix::sys::signal::Signal;
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
use tracing::{debug, info};

/// Outcome of a shell command: its exit code and captured output.
#[derive(Debug)]
pub struct CommandResult {
    /// Exit status reported by the shell, or `-1` when a signal killed the
    /// process.
    pub exit_code: i32,
    /// Captured standard output, capped at 64 KiB.
    pub stdout: String,
    /// Captured standard error, capped at 64 KiB.
    pub stderr: String,
}

impl CommandResult {
    /// Returns `true` when the command exited with status zero.
    pub fn is_successful(&self) -> bool {
        self.exit_code == 0
    }
}

/// Manages command execution with per-project locking to prevent concurrent
/// runs.
pub struct CommandTrigger {
    projects: HashMap<String, ProjectConfig>,
    locks: SharedLock<HashMap<String, SharedLock<()>>>,
}

impl CommandTrigger {
    /// Creates a trigger over the given projects.
    pub fn new(projects: HashMap<String, ProjectConfig>) -> Self {
        Self {
            projects,
            locks: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Runs the command for `key` with the configured timeout. Calls for the
    /// same project are serialized; a timeout kills the command's process
    /// group.
    ///
    /// # Errors
    ///
    /// Returns [`AppError::NotFound`] for an unknown project,
    /// [`AppError::Timeout`] when the command exceeds its timeout, and
    /// [`AppError::Internal`] when the command fails or cannot be started.
    pub async fn trigger_command(&self, key: &str) -> Result<()> {
        let project = self
            .projects
            .get(key)
            .ok_or_else(|| AppError::NotFound(format!("project `{}` does not exist", key)))?;

        let timeout_duration = project.timeout_duration();
        let lock = self.get_lock(key).await;

        info!("Executing command for project `{}`", key);

        let result = tokio::time::timeout(
            timeout_duration,
            execute_shell_command_locked(lock, &project.command, &project.directory),
        )
        .await
        .map_err(|_| {
            AppError::Timeout(format!(
                "command execution timed-out after {:?}",
                timeout_duration
            ))
        })??;

        if result.is_successful() {
            Ok(())
        } else {
            Err(AppError::Internal(format!(
                "command execution failed with exit code {}",
                result.exit_code
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
    let is_directory = tokio::fs::metadata(dir_path)
        .await
        .is_ok_and(|metadata| metadata.is_dir());
    if !is_directory {
        return Err(AppError::Internal(format!(
            "directory does not exist: {}",
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
    // Run the shell in its own process group so a timeout can kill the shell
    // and its children.
    shell.as_std_mut().process_group(0);
    let mut child = shell
        .spawn()
        .map_err(|e| AppError::Internal(format!("failed to spawn command: {}", e)))?;
    let mut group = ProcessGroupGuard(child.id().map(|id| Pid::from_raw(id as i32)));

    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| AppError::Internal("failed to capture command stdout".to_string()))?;
    let stderr = child
        .stderr
        .take()
        .ok_or_else(|| AppError::Internal("failed to capture command stderr".to_string()))?;
    let (stdout, stderr, status) =
        tokio::try_join!(capture_output(stdout), capture_output(stderr), child.wait(),)
            .map_err(|e| AppError::Internal(format!("failed to wait for command: {}", e)))?;
    group.0 = None;

    Ok(CommandResult {
        exit_code: status.code().unwrap_or(-1),
        stdout,
        stderr,
    })
}

/// Reads a stream to the end, keeping at most `MAX_CAPTURED_OUTPUT_BYTES` and
/// appending a truncation marker when more data arrives.
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

/// Kills the command's process group when dropped, including when the timeout
/// drops the execution future.
struct ProcessGroupGuard(Option<Pid>);

impl Drop for ProcessGroupGuard {
    fn drop(&mut self) {
        if let Some(group) = self.0
            && let Err(err) = nix::sys::signal::killpg(group, Signal::SIGKILL)
        {
            debug!("Could not stop command process group {}: {}", group, err);
        }
    }
}

#[cfg(test)]
#[path = "../unit-tests/command.rs"]
mod tests;
