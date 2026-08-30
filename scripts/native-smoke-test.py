#!/usr/bin/env python3

from __future__ import annotations

import argparse
import hashlib
import hmac
import json
import os
import signal
import socket
import subprocess
import sys
import tempfile
import time
import urllib.error
import urllib.request
from dataclasses import dataclass
from pathlib import Path


HOST = "127.0.0.1"
PROJECT = "native-smoke"
REF = "refs/heads/native-smoke"
SECRET = "native-smoke-secret"
PAYLOAD = json.dumps(
    {"action": "push", "ref": REF},
    separators=(",", ":"),
).encode()
PORT_ATTEMPTS = 3
REQUEST_TIMEOUT_SECONDS = 5.0
SHUTDOWN_TIMEOUT_SECONDS = 5.0


class SmokeTestFailure(RuntimeError):
    pass


class PortCollision(SmokeTestFailure):
    pass


@dataclass(frozen=True)
class Response:
    status: int
    body: str


def available_port() -> int:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as server:
        server.bind((HOST, 0))
        return server.getsockname()[1]


def write_config(work_dir: Path, port: int) -> Path:
    config_path = work_dir / "application-smoke.yaml"
    command = "printf 'invoked\\n' >> smoke-marker.txt"
    config_path.write_text(
        "\n".join(
            [
                "http:",
                f"  host: {json.dumps(HOST)}",
                f"  port: {port}",
                '  path: "/"',
                "projects:",
                f"  {PROJECT}:",
                '    action: "push"',
                f"    ref: {json.dumps(REF)}",
                f"    directory: {json.dumps(str(work_dir))}",
                f"    command: {json.dumps(command)}",
                '    timeout: "PT5S"',
                f"    secret: {json.dumps(SECRET)}",
                "",
            ],
        ),
        encoding="utf-8",
    )
    return config_path


def send_request(
    url: str,
    *,
    method: str = "GET",
    body: bytes | None = None,
    headers: dict[str, str] | None = None,
) -> Response:
    request = urllib.request.Request(
        url,
        data=body,
        headers=headers or {},
        method=method,
    )
    try:
        with urllib.request.urlopen(request, timeout=REQUEST_TIMEOUT_SECONDS) as response:
            return Response(response.status, response.read().decode("utf-8"))
    except urllib.error.HTTPError as error:
        with error:
            return Response(error.code, error.read().decode("utf-8"))


def wait_until_ready(process: subprocess.Popen[bytes], base_url: str, timeout: float) -> None:
    deadline = time.monotonic() + timeout
    last_error = "server did not accept a connection"
    while time.monotonic() < deadline:
        exit_code = process.poll()
        if exit_code is not None:
            raise SmokeTestFailure(f"native process exited before readiness with code {exit_code}")
        try:
            response = send_request(f"{base_url}/")
            if response.status == 200 and PROJECT in response.body:
                return
            last_error = f"GET / returned HTTP {response.status}: {response.body!r}"
        except (OSError, urllib.error.URLError) as error:
            last_error = str(error)
        time.sleep(0.1)
    raise SmokeTestFailure(f"server was not ready after {timeout:.1f}s: {last_error}")


def expect_response(response: Response, status: int, body: str, request_name: str) -> None:
    if response.status != status or response.body != body:
        raise SmokeTestFailure(
            f"{request_name} returned HTTP {response.status} with {response.body!r}; "
            f"expected HTTP {status} with {body!r}",
        )


def wait_for_marker(marker: Path, timeout: float = 2.0) -> None:
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if marker.exists() and marker.read_text(encoding="utf-8") == "invoked\n":
            return
        time.sleep(0.05)
    actual = marker.read_text(encoding="utf-8") if marker.exists() else "<missing>"
    raise SmokeTestFailure(f"command marker was {actual!r}; expected 'invoked\\n'")


def stop_process(process: subprocess.Popen[bytes]) -> None:
    if process.poll() is not None:
        return
    try:
        os.killpg(process.pid, signal.SIGTERM)
        process.wait(timeout=SHUTDOWN_TIMEOUT_SECONDS)
    except ProcessLookupError:
        return
    except subprocess.TimeoutExpired:
        try:
            os.killpg(process.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        process.wait(timeout=SHUTDOWN_TIMEOUT_SECONDS)


def process_output(log_path: Path) -> str:
    if not log_path.exists():
        return "<no process output>"
    output = log_path.read_text(encoding="utf-8", errors="replace").strip()
    return output or "<no process output>"


def is_port_collision(output: str) -> bool:
    normalized = output.casefold()
    return "address already in use" in normalized or "bindexception" in normalized


def run_attempt(executable: Path, work_dir: Path, port: int, startup_timeout: float, attempt: int) -> None:
    config_path = write_config(work_dir, port)
    log_path = work_dir / f"native-process-{attempt}.log"
    marker = work_dir / "smoke-marker.txt"
    process: subprocess.Popen[bytes] | None = None
    try:
        with log_path.open("wb", buffering=0) as process_log:
            process = subprocess.Popen(
                [str(executable), str(config_path)],
                cwd=work_dir,
                stdin=subprocess.DEVNULL,
                stdout=process_log,
                stderr=subprocess.STDOUT,
                start_new_session=True,
            )
            base_url = f"http://{HOST}:{port}"
            wait_until_ready(process, base_url, startup_timeout)

            signature = hmac.new(SECRET.encode(), PAYLOAD, hashlib.sha256).hexdigest()
            request_headers = {
                "Content-Type": "application/json",
                "X-Hub-Signature-256": f"sha256={signature}",
            }
            success = send_request(
                f"{base_url}/{PROJECT}",
                method="POST",
                body=PAYLOAD,
                headers=request_headers,
            )
            expect_response(success, 200, "OK", "authenticated webhook")
            wait_for_marker(marker)

            invalid_signature = send_request(
                f"{base_url}/{PROJECT}",
                method="POST",
                body=PAYLOAD,
                headers={
                    **request_headers,
                    "X-Hub-Signature-256": f"sha256={'0' * 64}",
                },
            )
            expect_response(invalid_signature, 403, "Invalid checksum (sha256)", "invalid signature")

            unknown_project = send_request(
                f"{base_url}/missing-project",
                method="POST",
                body=PAYLOAD,
                headers=request_headers,
            )
            expect_response(
                unknown_project,
                404,
                "Project `missing-project` does not exist",
                "unknown project",
            )

            marker_contents = marker.read_text(encoding="utf-8")
            if marker_contents != "invoked\n":
                raise SmokeTestFailure(
                    f"rejected requests executed the command; marker was {marker_contents!r}",
                )
    except Exception as error:
        if process is not None:
            stop_process(process)
        output = process_output(log_path)
        if is_port_collision(output):
            raise PortCollision(output) from error
        message = str(error) if isinstance(error, SmokeTestFailure) else f"{type(error).__name__}: {error}"
        raise SmokeTestFailure(f"{message}\n\nnative process output:\n{output}") from error
    finally:
        if process is not None:
            stop_process(process)


def positive_float(value: str) -> float:
    parsed = float(value)
    if parsed <= 0:
        raise argparse.ArgumentTypeError("must be greater than zero")
    return parsed


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Smoke-test the production native executable")
    parser.add_argument("executable", type=Path)
    parser.add_argument("--startup-timeout", type=positive_float, default=30.0)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    executable = args.executable.resolve()
    if not executable.is_file():
        raise SmokeTestFailure(f"native executable does not exist: {executable}")
    if not os.access(executable, os.X_OK):
        raise SmokeTestFailure(f"native executable is not executable: {executable}")

    last_collision: PortCollision | None = None
    with tempfile.TemporaryDirectory(prefix="github-webhook-listener-native-smoke-") as temp_dir:
        work_dir = Path(temp_dir)
        for attempt in range(1, PORT_ATTEMPTS + 1):
            try:
                run_attempt(executable, work_dir, available_port(), args.startup_timeout, attempt)
                print("Native executable smoke test passed")
                return 0
            except PortCollision as error:
                last_collision = error
        raise SmokeTestFailure(
            f"could not start the server after {PORT_ATTEMPTS} port attempts:\n{last_collision}",
        )


if __name__ == "__main__":
    try:
        sys.exit(main())
    except SmokeTestFailure as error:
        print(f"Native executable smoke test failed: {error}", file=sys.stderr)
        sys.exit(1)
