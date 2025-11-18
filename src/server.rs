use crate::command::CommandTrigger;
use crate::config::AppConfig;
use crate::error::{AppError, Result};
use crate::event::EventPayload;
use askama::Template;
use axum::{
    Router,
    body::Bytes,
    extract::{Path, State},
    http::{HeaderMap, StatusCode},
    response::{Html, IntoResponse, Response},
    routing::{get, post},
};
use std::sync::Arc;
use tracing::{info, warn};

#[derive(Template)]
#[template(path = "projects.html")]
struct ProjectsTemplate {
    projects: Vec<String>,
}

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
    app = app.route(&format!("{}/{{project}}", base_path), post(handle_webhook));

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
    let projects: Vec<String> = state.config.projects.keys().cloned().collect();
    
    let template = ProjectsTemplate { projects };
    Html(template.render().unwrap_or_else(|_| "Error rendering template".to_string()))
}

async fn handle_webhook(
    State(state): State<AppState>,
    Path(project_key): Path<String>,
    headers: HeaderMap,
    body: Bytes,
) -> Result<Response> {
    // Get the project config
    let project =
        state.config.projects.get(&project_key).ok_or_else(|| {
            AppError::NotFound(format!("Project `{}` does not exist", project_key))
        })?;

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
    state
        .command_trigger
        .trigger_command(&project_key)
        .await
        .inspect(|_| info!("POST /{} — OK", project_key))
        .inspect_err(|e| warn!("POST /{} — Error: {}", project_key, e))?;
    
    Ok((StatusCode::OK, "OK").into_response())
}

#[cfg(test)]
mod tests {
    // Note: html_escape test removed as escaping is now handled by Askama template engine
}
