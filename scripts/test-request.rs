#!/usr/bin/env -S cargo +nightly -Zscript --quiet
---
[package]
edition = "2024"

[dependencies]
reqwest = { version = "0.12", features = ["blocking"] }
hmac = "0.12"
sha1 = "0.10"
sha2 = "0.10"
hex = "0.4"
---

use hmac::{Hmac, Mac};
use sha1::Sha1;
use sha2::Sha256;

type HmacSha256 = Hmac<Sha256>;
type HmacSha1 = Hmac<Sha1>;

fn generate_hmac_sha256(body: &str, secret: &str) -> String {
    let mut mac = HmacSha256::new_from_slice(secret.as_bytes()).unwrap();
    mac.update(body.as_bytes());
    let result = mac.finalize();
    format!("sha256={}", hex::encode(result.into_bytes()))
}

fn generate_hmac_sha1(body: &str, secret: &str) -> String {
    let mut mac = HmacSha1::new_from_slice(secret.as_bytes()).unwrap();
    mac.update(body.as_bytes());
    let result = mac.finalize();
    format!("sha1={}", hex::encode(result.into_bytes()))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let sign_key = "xxxxxxxxxxxxxxxxxxxxxxxxxx";
    let body_text = r#"{
    "action": "push",
    "ref": "refs/heads/gh-pages"
}"#;

    let client = reqwest::blocking::Client::new();

    // Test 1: GET request to root
    println!("Test 1: GET /");
    let response = client.get("http://localhost:8080/").send()?;
    println!("HTTP {}: {}", response.status(), response.text()?);
    println!();

    // Test 2: POST with SHA256 signature
    println!("Test 2: POST /myproject with SHA256");
    let signature_sha256 = generate_hmac_sha256(body_text, sign_key);
    let response = client
        .post("http://localhost:8080/myproject")
        .header("Content-Type", "application/json")
        .header("X-Hub-Signature-256", signature_sha256)
        .body(body_text)
        .send()?;
    println!("HTTP {}: {}", response.status(), response.text()?);
    println!();

    // Test 3: POST with SHA1 signature
    println!("Test 3: POST /myproject with SHA1");
    let signature_sha1 = generate_hmac_sha1(body_text, sign_key);
    let response = client
        .post("http://localhost:8080/myproject")
        .header("Content-Type", "application/json")
        .header("X-Hub-Signature", signature_sha1)
        .body(body_text)
        .send()?;
    println!("HTTP {}: {}", response.status(), response.text()?);
    println!();

    // Test 4: POST to non-existent project
    println!("Test 4: POST /notAvailable");
    let signature = generate_hmac_sha1(body_text, sign_key);
    let response = client
        .post("http://localhost:8080/notAvailable")
        .header("Content-Type", "application/json")
        .header("X-Hub-Signature", signature)
        .body(body_text)
        .send()?;
    println!("HTTP {}: {}", response.status(), response.text()?);

    Ok(())
}
