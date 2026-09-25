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
    routing::get,
};
use std::sync::Arc;
use tokio::{
    net::TcpListener,
    spawn,
    sync::{
        Mutex,
        mpsc::{self, Sender, error::TrySendError},
    },
};
use tracing::{error, info, warn};

/// State shared by the HTTP handlers: the loaded configuration and the command
/// queue.
#[derive(Clone)]
pub struct AppState {
    pub config: AppConfig,
    command_queue: Sender<String>,
}

pub async fn start_server(config: AppConfig) -> Result<()> {
    let command_trigger = Arc::new(CommandTrigger::new(config.projects.clone()));
    let command_queue = start_command_worker(command_trigger);
    let state = AppState {
        config: config.clone(),
        command_queue,
    };

    let base_path = config.http.base_path();
    let app = routes(&base_path).with_state(state);

    let addr = config.http.bind_address();
    info!("Starting server on {}", addr);

    let listener = TcpListener::bind(&addr)
        .await
        .map_err(|e| AppError::Internal(format!("Failed to bind to {}: {}", addr, e)))?;

    axum::serve(listener, app)
        .await
        .map_err(|e| AppError::Internal(format!("Server error: {}", e)))?;

    Ok(())
}

/// Maximum commands waiting in the queue. New deliveries get 503 when it is
/// full.
const QUEUE_CAPACITY: usize = 64;

#[derive(Template)]
#[template(path = "projects.html")]
struct ProjectsTemplate {
    projects: Vec<String>,
}

/// Spawns the single worker that runs queued commands one at a time. The
/// supervisor restarts the worker if it panics.
fn start_command_worker(trigger: Arc<CommandTrigger>) -> Sender<String> {
    let (sender, receiver) = mpsc::channel::<String>(QUEUE_CAPACITY);
    let receiver = Arc::new(Mutex::new(receiver));

    spawn(async move {
        loop {
            let receiver = receiver.clone();
            let trigger = trigger.clone();
            let worker = spawn(async move {
                loop {
                    let Some(project_key) = receiver.lock().await.recv().await else {
                        break;
                    };
                    let trigger = trigger.clone();
                    let task = spawn(async move {
                        let result = trigger.trigger_command(&project_key).await;
                        (project_key, result)
                    });
                    match task.await {
                        Ok((key, Ok(()))) => info!("Command completed for project `{}`", key),
                        Ok((key, Err(err))) => {
                            error!("Command failed for project `{}`: {}", key, err)
                        }
                        Err(err) => error!("Command task failed: {}", err),
                    }
                }
            });

            match worker.await {
                Ok(()) => break,
                Err(err) => error!("Command worker stopped unexpectedly; restarting: {}", err),
            }
        }
    });

    sender
}

fn routes(base_path: &str) -> Router<AppState> {
    let mut router = Router::new();

    // Redirect base path without trailing slash to base path with trailing
    // slash
    if !base_path.is_empty() {
        let redirect_location = format!("{}/", base_path.trim_end_matches('/'));
        router = router.route(
            base_path,
            get(move || async move {
                (
                    StatusCode::MOVED_PERMANENTLY,
                    [("Location", redirect_location.clone())],
                )
                    .into_response()
            }),
        );
    }

    router = router.route(&format!("{}/", base_path), get(list_projects));
    router = router.route(
        &format!("{}/{{project}}", base_path),
        axum::routing::post(handle_webhook),
    );

    router
}

async fn list_projects(State(state): State<AppState>) -> Html<String> {
    let projects: Vec<String> = state.config.projects.keys().cloned().collect();

    let template = ProjectsTemplate { projects };
    Html(
        template
            .render()
            .unwrap_or_else(|_| "Error rendering template".to_string()),
    )
}

async fn handle_webhook(
    State(state): State<AppState>,
    Path(project_key): Path<String>,
    headers: HeaderMap,
    body: Bytes,
) -> Result<Response> {
    let project =
        state.config.projects.get(&project_key).ok_or_else(|| {
            AppError::NotFound(format!("Project `{}` does not exist", project_key))
        })?;

    let body_str = std::str::from_utf8(&body)
        .map_err(|_| AppError::BadRequest("Invalid UTF-8 in request body".to_string()))?;

    let signature = headers
        .get("x-hub-signature-256")
        .or_else(|| headers.get("x-hub-signature"))
        .and_then(|v| v.to_str().ok());

    EventPayload::verify_signature(body_str, &project.secret, signature)?;

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

    if !payload.should_process(project) {
        info!("POST /{} — Skipped (ref or action mismatch)", project_key);
        return Ok((StatusCode::OK, "Nothing to do").into_response());
    }

    match state.command_queue.try_send(project_key.clone()) {
        Ok(()) => {
            info!("POST /{} — Accepted", project_key);
            Ok((StatusCode::ACCEPTED, "Accepted").into_response())
        }
        Err(TrySendError::Full(_)) => {
            warn!("POST /{} — Queue full", project_key);
            Ok((StatusCode::SERVICE_UNAVAILABLE, "Queue full").into_response())
        }
        Err(TrySendError::Closed(_)) => {
            error!("POST /{} — Command worker unavailable", project_key);
            Ok((StatusCode::SERVICE_UNAVAILABLE, "Worker unavailable").into_response())
        }
    }
}
