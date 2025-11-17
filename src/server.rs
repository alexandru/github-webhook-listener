use crate::command::CommandTrigger;
use crate::config::AppConfig;
use crate::error::{AppError, Result};
use crate::event::EventPayload;
use axum::{
    body::Bytes,
    extract::{Path, State},
    http::{HeaderMap, StatusCode},
    response::{Html, IntoResponse, Response},
    routing::{get, post},
    Router,
};
use std::sync::Arc;
use tracing::{info, warn};

#[derive(Clone)]
pub struct AppState {
    pub config: AppConfig,
    pub command_trigger: Arc<CommandTrigger>,
}

pub async fn start_server(config: AppConfig) -> Result<()> {
    let command_trigger = Arc::new(CommandTrigger::new(config.projects.clone()));
    let state = AppState {
        config: config.clone(),
        command_trigger,
    };

    let base_path = config.http.base_path();
    
    let mut app = Router::new();
    
    // Add root GET handler
    if !base_path.is_empty() {
        app = app.route(&base_path, get(redirect_to_slash));
    }
    app = app.route(&format!("{}/", base_path), get(list_projects));
    app = app.route(&format!("{}/:project", base_path), post(handle_webhook));
    
    let app = app.with_state(state);

    let addr = config.http.bind_address();
    info!("Starting server on {}", addr);

    let listener = tokio::net::TcpListener::bind(&addr)
        .await
        .map_err(|e| AppError::Internal(format!("Failed to bind to {}: {}", addr, e)))?;

    axum::serve(listener, app)
        .await
        .map_err(|e| AppError::Internal(format!("Server error: {}", e)))?;

    Ok(())
}

async fn redirect_to_slash() -> Response {
    (StatusCode::MOVED_PERMANENTLY, [("Location", "/")]).into_response()
}

async fn list_projects(State(state): State<AppState>) -> Html<String> {
    let project_names: Vec<String> = state.config.projects.keys().cloned().collect();
    
    let html = format!(
        r#"<!DOCTYPE html>
<html>
<head>
    <title>GitHub Webhook Listener</title>
</head>
<body>
    <p>Configured hooks:</p>
    <ul>
{}
    </ul>
</body>
</html>"#,
        project_names
            .iter()
            .map(|name| format!("        <li>{}</li>", html_escape(name)))
            .collect::<Vec<_>>()
            .join("\n")
    );
    
    Html(html)
}

async fn handle_webhook(
    State(state): State<AppState>,
    Path(project_key): Path<String>,
    headers: HeaderMap,
    body: Bytes,
) -> Result<Response> {
    // Get the project config
    let project = state
        .config
        .projects
        .get(&project_key)
        .ok_or_else(|| AppError::NotFound(format!("Project `{}` does not exist", project_key)))?;

    // Get body as string
    let body_str = std::str::from_utf8(&body)
        .map_err(|_| AppError::BadRequest("Invalid UTF-8 in request body".to_string()))?;

    // Verify signature
    let signature = headers
        .get("x-hub-signature-256")
        .or_else(|| headers.get("x-hub-signature"))
        .and_then(|v| v.to_str().ok());
    
    EventPayload::verify_signature(body_str, &project.secret, signature)?;

    // Parse the payload based on content type
    let content_type = headers
        .get("content-type")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("application/json");

    let payload = if content_type.contains("application/json") {
        EventPayload::from_json(body_str)?
    } else if content_type.contains("application/x-www-form-urlencoded") {
        EventPayload::from_form(body_str)?
    } else {
        return Err(AppError::UnsupportedMediaType(format!(
            "Cannot process `{}` media type",
            content_type
        )));
    };

    // Check if we should process this payload
    if !payload.should_process(project) {
        info!("POST /{} — Skipped (ref or action mismatch)", project_key);
        return Ok((StatusCode::OK, "Nothing to do").into_response());
    }

    // Execute the command
    match state.command_trigger.trigger_command(&project_key).await {
        Ok(_) => {
            info!("POST /{} — OK", project_key);
            Ok((StatusCode::OK, "OK").into_response())
        }
        Err(e) => {
            warn!("POST /{} — Error: {}", project_key, e);
            Err(e)
        }
    }
}

fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&#x27;")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_html_escape() {
        assert_eq!(html_escape("test"), "test");
        assert_eq!(html_escape("<script>"), "&lt;script&gt;");
        assert_eq!(html_escape("a&b"), "a&amp;b");
    }
}
