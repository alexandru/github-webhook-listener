use crate::config::ProjectConfig;
use crate::error::{AppError, Result};
use hmac::{Hmac, Mac};
use serde::{Deserialize, Serialize};
use sha1::Sha1;
use sha2::Sha256;

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct EventPayload {
    pub action: Option<String>,
    #[serde(rename = "ref")]
    pub git_ref: Option<String>,
}

impl EventPayload {
    /// Parse JSON payload
    pub fn from_json(json: &str) -> Result<Self> {
        Ok(serde_json::from_str(json)?)
    }

    /// Parse form-urlencoded payload
    pub fn from_form(form_data: &str) -> Result<Self> {
        Ok(serde_urlencoded::from_str(form_data)?)
    }

    /// Check if this payload should trigger the project
    pub fn should_process(&self, project: &ProjectConfig) -> bool {
        let action_matches = self.action.as_deref().unwrap_or("push") == project.action_filter();
        let ref_matches = self.git_ref.as_deref() == Some(&project.git_ref);
        action_matches && ref_matches
    }

    /// Verify HMAC signature
    pub fn verify_signature(
        body: &str,
        secret: &str,
        signature_header: Option<&str>,
    ) -> Result<()> {
        let signature = signature_header
            .ok_or_else(|| AppError::Forbidden("No signature header was provided".to_string()))?;

        if let Some(sig) = signature.strip_prefix("sha256=") {
            verify_hmac_sha256(body, secret, sig)
        } else if let Some(sig) = signature.strip_prefix("sha1=") {
            verify_hmac_sha1(body, secret, sig)
        } else {
            Err(AppError::Forbidden(
                "Unsupported signature algorithm".to_string(),
            ))
        }
    }
}

fn verify_hmac_sha256(body: &str, secret: &str, expected_hex: &str) -> Result<()> {
    type HmacSha256 = Hmac<Sha256>;
    verify_hmac_with_algorithm::<HmacSha256>(body, secret, expected_hex, "sha256")
}

fn verify_hmac_sha1(body: &str, secret: &str, expected_hex: &str) -> Result<()> {
    type HmacSha1 = Hmac<Sha1>;
    verify_hmac_with_algorithm::<HmacSha1>(body, secret, expected_hex, "sha1")
}

fn verify_hmac_with_algorithm<M>(
    body: &str,
    secret: &str,
    expected_hex: &str,
    algorithm: &str,
) -> Result<()>
where
    M: Mac + hmac::digest::KeyInit,
{
    let mut mac = <M as Mac>::new_from_slice(secret.as_bytes())
        .map_err(|e| AppError::Internal(format!("Invalid secret key: {}", e)))?;

    mac.update(body.as_bytes());

    let expected = hex::decode(expected_hex)
        .map_err(|_| AppError::Forbidden("Invalid signature format".to_string()))?;

    mac.verify_slice(&expected)
        .map_err(|_| AppError::Forbidden(format!("Invalid checksum ({})", algorithm)))?;

    Ok(())
}

#[cfg(test)]
mod tests {
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
}
