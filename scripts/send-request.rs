#!/usr/bin/env -S cargo +nightly -Zscript --quiet
---
[package]
edition = "2024"

[dependencies]
anyhow = "1.0"
clap = { version = "4.6", features = ["derive"] }
reqwest = { version = "0.12", default-features = false, features = ["blocking", "json", "rustls-tls"] }
serde_json = "1.0"
hmac = "0.12"
sha2 = "0.10"
hex = "0.4"
---

use anyhow::{anyhow, Result};
use clap::Parser;
use hmac::{Hmac, Mac};
use reqwest::blocking::Client;
use reqwest::header::ACCEPT;
use reqwest::Url;
use sha2::Sha256;

#[derive(Parser)]
#[command(about = "List projects or send a signed push webhook")]
struct Args {
    #[arg(long, help = "Server URL, optionally including its configured path")]
    base: String,
    #[arg(long, help = "Project to send a webhook to")]
    project: Option<String>,
    #[arg(long, requires = "project", help = "Project webhook secret")]
    secret: Option<String>,
    #[arg(
        long = "ref",
        requires = "project",
        help = "Git ref for the push payload"
    )]
    git_ref: Option<String>,
}

fn main() -> Result<()> {
    let output = send_request(Args::parse())?;
    print!("{output}");
    Ok(())
}

fn send_request(args: Args) -> Result<String> {
    let mut url = Url::parse(&args.base)?;
    url.path_segments_mut()
        .map_err(|_| anyhow!("base URL must contain a path"))?
        .pop_if_empty()
        .push("");

    if let Some(project) = args.project {
        let secret = args
            .secret
            .ok_or_else(|| anyhow!("--secret is required with --project"))?;
        let git_ref = args
            .git_ref
            .ok_or_else(|| anyhow!("--ref is required with --project"))?;
        url.path_segments_mut()
            .map_err(|_| anyhow!("base URL must contain a path"))?
            .pop_if_empty()
            .push(&project);
        let body = serde_json::json!({ "ref": git_ref }).to_string();
        let mut mac = Hmac::<Sha256>::new_from_slice(secret.as_bytes())
            .map_err(|error| anyhow!("invalid HMAC key: {error}"))?;
        mac.update(body.as_bytes());
        let signature = format!("sha256={}", hex::encode(mac.finalize().into_bytes()));
        let response = Client::new()
            .post(url)
            .header("Content-Type", "application/json")
            .header("X-Hub-Signature-256", signature)
            .body(body)
            .send()?
            .error_for_status()?;
        return Ok(format!("{}\n", response.text()?));
    }

    let projects: Vec<String> = Client::new()
        .get(url)
        .header(ACCEPT, "application/json")
        .send()?
        .error_for_status()?
        .json()?;
    let mut output = String::new();
    for project in projects {
        output.push_str(&project);
        output.push('\n');
    }
    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::bail;
    use std::io::{Read, Write};
    use std::net::TcpListener;

    #[test]
    fn base_url_lists_projects_one_per_line() -> Result<()> {
        let listener = TcpListener::bind("127.0.0.1:0")?;
        let base = format!("http://{}/hooks", listener.local_addr()?);
        let server = std::thread::spawn(move || -> Result<String> {
            let (mut stream, _) = listener.accept()?;
            let mut request = Vec::new();
            let mut buffer = [0u8; 1024];
            while !request.windows(4).any(|window| window == b"\r\n\r\n") {
                let read = stream.read(&mut buffer)?;
                if read == 0 {
                    bail!("request ended before its headers");
                }
                request.extend_from_slice(&buffer[..read]);
            }
            let response = "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: 25\r\nConnection: close\r\n\r\n[\"alexn\",\"bangs\",\"monix\"]";
            stream.write_all(response.as_bytes())?;
            Ok(String::from_utf8(request)?)
        });

        let args = Args::try_parse_from(["send-request", "--base", &base])?;
        let output = send_request(args)?;
        assert_eq!(output, "alexn\nbangs\nmonix\n");
        let request = server
            .join()
            .map_err(|_| anyhow::anyhow!("server panicked"))??;
        assert!(request.starts_with("GET /hooks/ HTTP/1.1\r\n"));
        assert!(request
            .to_ascii_lowercase()
            .contains("accept: application/json"));
        Ok(())
    }

    #[test]
    fn project_option_posts_signed_push_delivery() -> Result<()> {
        let listener = TcpListener::bind("127.0.0.1:0")?;
        let base = format!("http://{}/hooks", listener.local_addr()?);
        let server = std::thread::spawn(move || -> Result<String> {
            let (mut stream, _) = listener.accept()?;
            let mut request = Vec::new();
            let mut buffer = [0u8; 1024];
            let header_end = loop {
                if let Some(index) = request.windows(4).position(|window| window == b"\r\n\r\n") {
                    break index + 4;
                }
                let read = stream.read(&mut buffer)?;
                if read == 0 {
                    bail!("request ended before its headers");
                }
                request.extend_from_slice(&buffer[..read]);
            };
            let headers = String::from_utf8_lossy(&request[..header_end]).to_ascii_lowercase();
            let length: usize = headers
                .lines()
                .find_map(|line| line.strip_prefix("content-length: "))
                .ok_or_else(|| anyhow!("request has no content length"))?
                .trim()
                .parse()?;
            while request.len() < header_end + length {
                let read = stream.read(&mut buffer)?;
                if read == 0 {
                    bail!("request ended before its body");
                }
                request.extend_from_slice(&buffer[..read]);
            }
            stream.write_all(
                b"HTTP/1.1 202 Accepted\r\nContent-Length: 8\r\nConnection: close\r\n\r\nAccepted",
            )?;
            Ok(String::from_utf8(request)?)
        });

        let args = Args::try_parse_from([
            "send-request",
            "--base",
            &base,
            "--project",
            "monix",
            "--secret",
            "fixture-secret",
            "--ref",
            "refs/heads/main",
        ])?;
        assert_eq!(send_request(args)?, "Accepted\n");
        let request = server.join().map_err(|_| anyhow!("server panicked"))??;
        assert!(request.starts_with("POST /hooks/monix HTTP/1.1\r\n"));
        assert!(request
            .to_ascii_lowercase()
            .contains("content-type: application/json"));
        assert!(request.to_ascii_lowercase().contains(concat!(
            "x-hub-signature-256: sha256=",
            "4f5b0491edee14337ae1159a76d0b527d8f52f7662f43ba6ac0893e42e7cdd36"
        )));
        assert!(request.ends_with("{\"ref\":\"refs/heads/main\"}"));
        Ok(())
    }

    #[test]
    fn credentials_without_project_are_rejected() {
        assert!(Args::try_parse_from([
            "send-request",
            "--base",
            "https://hook.example.org",
            "--secret",
            "fixture-secret",
        ])
        .is_err());
        assert!(Args::try_parse_from([
            "send-request",
            "--base",
            "https://hook.example.org",
            "--ref",
            "refs/heads/main",
        ])
        .is_err());
    }
}
