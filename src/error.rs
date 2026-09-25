//! Error type shared across the crate, with its HTTP status mapping.

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

/// Result type used across the crate, with [`AppError`] as the error.
pub type Result<T> = StdResult<T, AppError>;

/// Errors produced by the listener. Each variant maps to an HTTP status.
#[derive(Debug, Error)]
pub enum AppError {
    #[error("io error: {0}")]
    Io(#[from] IoError),

    #[error("yaml parse error: {0}")]
    Yaml(#[from] YamlError),

    #[error("json parse error: {0}")]
    Json(#[from] JsonError),

    #[error("url-encoded parse error: {0}")]
    UrlEncoded(#[from] UrlEncodedError),

    #[error("bad request: {0}")]
    BadRequest(String),

    #[error("forbidden: {0}")]
    Forbidden(String),

    #[error("not found: {0}")]
    NotFound(String),

    #[error("internal error: {0}")]
    Internal(String),

    #[error("timeout: {0}")]
    Timeout(String),

    #[error("unsupported media type: {0}")]
    UnsupportedMediaType(String),
}

/// Maps each error to the HTTP status and message returned to the webhook
/// sender.
impl IntoResponse for AppError {
    fn into_response(self) -> Response {
        let (status, message) = match self {
            AppError::BadRequest(msg) => (StatusCode::BAD_REQUEST, format!("bad request: {msg}")),
            AppError::Json(error) => (StatusCode::BAD_REQUEST, format!("invalid JSON: {error}")),
            AppError::UrlEncoded(error) => (
                StatusCode::BAD_REQUEST,
                format!("invalid form data: {error}"),
            ),
            AppError::Forbidden(msg) => (StatusCode::FORBIDDEN, msg),
            AppError::NotFound(msg) => (StatusCode::NOT_FOUND, msg),
            AppError::Timeout(msg) => (StatusCode::REQUEST_TIMEOUT, msg),
            AppError::UnsupportedMediaType(msg) => (StatusCode::UNSUPPORTED_MEDIA_TYPE, msg),
            AppError::Internal(msg) => (StatusCode::INTERNAL_SERVER_ERROR, msg),
            AppError::Io(error) => (
                StatusCode::INTERNAL_SERVER_ERROR,
                format!("io error: {error}"),
            ),
            AppError::Yaml(error) => (
                StatusCode::INTERNAL_SERVER_ERROR,
                format!("yaml parse error: {error}"),
            ),
        };

        (status, message).into_response()
    }
}
