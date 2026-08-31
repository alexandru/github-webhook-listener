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
