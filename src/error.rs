use axum::{
    http::StatusCode,
    response::{IntoResponse, Response},
};
use serde_json::Error as JsonError;
use serde_urlencoded::de::Error as UrlEncodedError;
use serde_yaml::Error as YamlError;
use std::io::Error as IoError;
use std::result::Result as StdResult;
use thiserror::Error;

pub type Result<T> = StdResult<T, AppError>;

#[derive(Debug, Error)]
pub enum AppError {
    #[error("IO error: {0}")]
    Io(#[from] IoError),

    #[error("YAML parse error: {0}")]
    Yaml(#[from] YamlError),

    #[error("JSON parse error: {0}")]
    Json(#[from] JsonError),

    #[error("URL-encoded parse error: {0}")]
    UrlEncoded(#[from] UrlEncodedError),

    #[error("Bad request: {0}")]
    BadRequest(String),

    #[error("Forbidden: {0}")]
    Forbidden(String),

    #[error("Not found: {0}")]
    NotFound(String),

    #[error("Internal error: {0}")]
    Internal(String),

    #[error("Timeout: {0}")]
    Timeout(String),

    #[error("Unsupported media type: {0}")]
    UnsupportedMediaType(String),
}

/// Maps each error to the HTTP status and message returned to the webhook
/// sender.
impl IntoResponse for AppError {
    fn into_response(self) -> Response {
        let (status, message) = match &self {
            AppError::BadRequest(msg) => (StatusCode::BAD_REQUEST, format!("Bad request: {}", msg)),
            AppError::Json(e) => (StatusCode::BAD_REQUEST, format!("Invalid JSON: {}", e)),
            AppError::UrlEncoded(e) => {
                (StatusCode::BAD_REQUEST, format!("Invalid form data: {}", e))
            }
            AppError::Forbidden(msg) => (StatusCode::FORBIDDEN, msg.clone()),
            AppError::NotFound(msg) => (StatusCode::NOT_FOUND, msg.clone()),
            AppError::Timeout(msg) => (StatusCode::REQUEST_TIMEOUT, msg.clone()),
            AppError::UnsupportedMediaType(msg) => {
                (StatusCode::UNSUPPORTED_MEDIA_TYPE, msg.clone())
            }
            AppError::Internal(msg) => (StatusCode::INTERNAL_SERVER_ERROR, msg.clone()),
            _ => (StatusCode::INTERNAL_SERVER_ERROR, self.to_string()),
        };

        (status, message).into_response()
    }
}
