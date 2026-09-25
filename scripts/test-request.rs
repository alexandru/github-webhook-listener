#!/usr/bin/env -S cargo +nightly -Zscript --quiet
---
[package]
edition = "2024"

[dependencies]
reqwest = { version = "0.12", default-features = false, features = ["blocking"] }
hmac = "0.12"
sha1 = "0.10"
sha2 = "0.10"
hex = "0.4"
---

use hex::encode;
use hmac::{Hmac, Mac};
use reqwest::blocking::Client;
use sha1::Sha1;
use sha2::Sha256;
use std::error::Error;
use std::io::Error as IoError;

type HmacSha256 = Hmac<Sha256>;
type HmacSha1 = Hmac<Sha1>;

fn generate_hmac_sha256(body: &str, secret: &str) -> Result<String, Box<dyn Error>> {
    let mut mac = HmacSha256::new_from_slice(secret.as_bytes())
        .map_err(|error| IoError::other(format!("invalid HMAC key: {error}")))?;
    mac.update(body.as_bytes());
    let result = mac.finalize();
    Ok(format!("sha256={}", encode(result.into_bytes())))
}

fn generate_hmac_sha1(body: &str, secret: &str) -> Result<String, Box<dyn Error>> {
    let mut mac = HmacSha1::new_from_slice(secret.as_bytes())
        .map_err(|error| IoError::other(format!("invalid HMAC key: {error}")))?;
    mac.update(body.as_bytes());
    let result = mac.finalize();
    Ok(format!("sha1={}", encode(result.into_bytes())))
}

fn main() -> Result<(), Box<dyn Error>> {
    let sign_key = "xxxxxxxxxxxxxxxxxxxxxxxxxx";
    let body_text = r#"{
    "action": "push",
    "ref": "refs/heads/gh-pages"
}"#;

    let client = Client::new();

    println!("Test 1: GET /");
    let response = client.get("http://localhost:8080/").send()?;
    println!("HTTP {}: {}", response.status(), response.text()?);
    println!();

    println!("Test 2: POST /myproject with SHA256");
    let signature_sha256 = generate_hmac_sha256(body_text, sign_key)?;
    let response = client
        .post("http://localhost:8080/myproject")
        .header("Content-Type", "application/json")
        .header("X-Hub-Signature-256", signature_sha256)
        .body(body_text)
        .send()?;
    println!("HTTP {}: {}", response.status(), response.text()?);
    println!();

    println!("Test 3: POST /myproject with SHA1");
    let signature_sha1 = generate_hmac_sha1(body_text, sign_key)?;
    let response = client
        .post("http://localhost:8080/myproject")
        .header("Content-Type", "application/json")
        .header("X-Hub-Signature", signature_sha1)
        .body(body_text)
        .send()?;
    println!("HTTP {}: {}", response.status(), response.text()?);
    println!();

    println!("Test 4: POST /notAvailable");
    let signature = generate_hmac_sha1(body_text, sign_key)?;
    let response = client
        .post("http://localhost:8080/notAvailable")
        .header("Content-Type", "application/json")
        .header("X-Hub-Signature", signature)
        .body(body_text)
        .send()?;
    println!("HTTP {}: {}", response.status(), response.text()?);

    Ok(())
}
