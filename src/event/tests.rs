use super::*;
use crate::config::ProjectConfig;
use std::time::Duration;

fn test_project() -> ProjectConfig {
    ProjectConfig {
        git_ref: "refs/heads/gh-pages".to_string(),
        directory: "/tmp".to_string(),
        command: "echo test".to_string(),
        secret: "test-secret".to_string(),
        action: None,
        timeout: Some(Duration::from_secs(5)),
    }
}

#[test]
fn test_parse_json() {
    let json = r#"{"action":"push","ref":"refs/heads/gh-pages"}"#;
    let payload = EventPayload::from_json(json).unwrap();
    assert_eq!(payload.action, Some("push".to_string()));
    assert_eq!(payload.git_ref, Some("refs/heads/gh-pages".to_string()));
}

#[test]
fn test_should_process() {
    let project = test_project();
    let payload = EventPayload {
        action: Some("push".to_string()),
        git_ref: Some("refs/heads/gh-pages".to_string()),
    };
    assert!(payload.should_process(&project));

    let wrong_ref = EventPayload {
        action: Some("push".to_string()),
        git_ref: Some("refs/heads/main".to_string()),
    };
    assert!(!wrong_ref.should_process(&project));
}

#[test]
fn test_verify_signature_sha256() {
    let body = "test body";
    let secret = "test-secret";

    // Generate the actual signature using hmac
    type HmacSha256 = Hmac<Sha256>;
    let mut mac = HmacSha256::new_from_slice(secret.as_bytes()).unwrap();
    mac.update(body.as_bytes());
    let result = mac.finalize();
    let actual_sig = format!("sha256={}", hex::encode(result.into_bytes()));

    // Test with correct signature
    EventPayload::verify_signature(body, secret, Some(&actual_sig)).unwrap();

    // Test with wrong signature
    let wrong_sig = "sha256=0000000000000000000000000000000000000000000000000000000000000000";
    assert!(EventPayload::verify_signature(body, secret, Some(wrong_sig)).is_err());
}
