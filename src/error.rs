use axum::{
    http::StatusCode,
    response::{IntoResponse, Response},
};
use std::fmt;

pub type Result<T> = std::result::Result<T, AppError>;

#[derive(Debug)]
pub enum AppError {
    Io(std::io::Error),
    Yaml(serde_yaml::Error),
    Json(serde_json::Error),
    UrlEncoded(serde_urlencoded::de::Error),
    BadRequest(String),
    Forbidden(String),
    NotFound(String),
    Internal(String),
    Timeout(String),
    UnsupportedMediaType(String),
}

impl fmt::Display for AppError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AppError::Io(e) => write!(f, "IO error: {}", e),
            AppError::Yaml(e) => write!(f, "YAML parse error: {}", e),
            AppError::Json(e) => write!(f, "JSON parse error: {}", e),
            AppError::UrlEncoded(e) => write!(f, "URL-encoded parse error: {}", e),
            AppError::BadRequest(msg) => write!(f, "Bad request: {}", msg),
            AppError::Forbidden(msg) => write!(f, "Forbidden: {}", msg),
            AppError::NotFound(msg) => write!(f, "Not found: {}", msg),
            AppError::Internal(msg) => write!(f, "Internal error: {}", msg),
            AppError::Timeout(msg) => write!(f, "Timeout: {}", msg),
            AppError::UnsupportedMediaType(msg) => write!(f, "Unsupported media type: {}", msg),
        }
    }
}

impl std::error::Error for AppError {}

impl From<std::io::Error> for AppError {
    fn from(err: std::io::Error) -> Self {
        AppError::Io(err)
    }
}

impl From<serde_yaml::Error> for AppError {
    fn from(err: serde_yaml::Error) -> Self {
        AppError::Yaml(err)
    }
}

impl From<serde_json::Error> for AppError {
    fn from(err: serde_json::Error) -> Self {
        AppError::Json(err)
    }
}

impl From<serde_urlencoded::de::Error> for AppError {
    fn from(err: serde_urlencoded::de::Error) -> Self {
        AppError::UrlEncoded(err)
    }
}

impl IntoResponse for AppError {
    fn into_response(self) -> Response {
        let (status, message) = match &self {
            AppError::BadRequest(msg) => {
                (StatusCode::BAD_REQUEST, format!("Bad request: {}", msg))
            }
            AppError::Json(e) => {
                (StatusCode::BAD_REQUEST, format!("Invalid JSON: {}", e))
            }
            AppError::UrlEncoded(e) => {
                (StatusCode::BAD_REQUEST, format!("Invalid form data: {}", e))
            }
            AppError::Forbidden(msg) => (StatusCode::FORBIDDEN, msg.clone()),
            AppError::NotFound(msg) => (StatusCode::NOT_FOUND, msg.clone()),
            AppError::Timeout(msg) => (StatusCode::REQUEST_TIMEOUT, msg.clone()),
            AppError::UnsupportedMediaType(msg) => (StatusCode::UNSUPPORTED_MEDIA_TYPE, msg.clone()),
            AppError::Internal(msg) => (StatusCode::INTERNAL_SERVER_ERROR, msg.clone()),
            _ => (StatusCode::INTERNAL_SERVER_ERROR, self.to_string()),
        };

        (status, message).into_response()
    }
}
