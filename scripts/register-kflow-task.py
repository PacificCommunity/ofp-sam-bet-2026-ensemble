#!/usr/bin/env python3
"""Register the BET ensemble task and optionally submit all models concurrently."""

from __future__ import annotations

import argparse
import csv
import json
import os
import subprocess
import urllib.error
import urllib.parse
import urllib.request
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path
from typing import Any

import yaml


ROOT = Path(__file__).resolve().parents[1]


def api_json(method: str, url: str, token: str, payload: dict[str, Any] | None = None) -> dict[str, Any]:
    headers = {"Authorization": f"Bearer {token}"}
    body = None
    if payload is not None:
        body = json.dumps(payload).encode("utf-8")
        headers["Content-Type"] = "application/json"
    request = urllib.request.Request(url, data=body, headers=headers, method=method)
    try:
        with urllib.request.urlopen(request, timeout=60) as response:
            raw = response.read()
    except urllib.error.HTTPError as exc:
        detail = exc.read().decode("utf-8", errors="replace")
        raise RuntimeError(f"{method} {url} failed: HTTP {exc.code}: {detail}") from exc
    return json.loads(raw.decode("utf-8")) if raw else {}


def git(*args: str) -> str:
    return subprocess.run(
        ["git", "-C", str(ROOT), *args], check=True, text=True,
        stdout=subprocess.PIPE,
    ).stdout.strip()


def repo_full_name() -> str:
    remote = git("remote", "get-url", "origin").removesuffix(".git")
    if remote.startswith("git@github.com:"):
        return remote.split(":", 1)[1]
    return remote.split("github.com/", 1)[1].strip("/")


def read_inputs() -> tuple[dict[str, Any], list[dict[str, str]]]:
    with (ROOT / "kflow.yaml").open(encoding="utf-8") as handle:
        config = yaml.safe_load(handle)
    with (ROOT / "design" / "model-draws.csv").open(encoding="utf-8", newline="") as handle:
        rows = list(csv.DictReader(handle))
    if len(rows) != 100 or len({row["ensemble_id"] for row in rows}) != 100:
        raise ValueError("The Kflow campaign requires exactly 100 unique ensemble rows.")
    image = str(config["docker_image"])
    if "@sha256:" not in image or len(image.rsplit("@sha256:", 1)[1]) != 64:
        raise ValueError("docker_image must be pinned by sha256 digest.")
    if config.get("remote_host") != "suva" or "suvofp" not in str(config.get("slot_requirements", "")):
        raise ValueError("The ensemble campaign must be explicitly pinned to Suva nodes.")
    return config, rows


def task_payload(config: dict[str, Any], rows: list[dict[str, str]]) -> dict[str, Any]:
    resources = config["resources"]
    metadata = {
        **config.get("metadata", {}),
        "repository_commit": git("rev-parse", "HEAD"),
        "model_rows": [
            {
                "ensemble_id": row["ensemble_id"],
                "model_label": row["model_label"],
                "steepness": row["steepness"],
                "tag_tau": row["tag_tau"],
                "tau_fish_pars4": row["tau_fish_pars4"],
                "tag_mixing_k_cutoff": row["tag_mixing_k_cutoff"],
                "tag_reporting": row["tag_reporting"],
                "tag_reporting_zero_mixing_exclusions": row["tag_reporting_zero_mixing_exclusions"],
                "m0_quarterly": row["m_age40_quarterly"],
                "effort_creep_primary": row["effort_creep_primary"],
                "effort_creep_secondary": row["effort_creep_secondary"],
            }
            for row in rows
        ],
    }
    return {
        "name": config["name"],
        "description": config["description"],
        "repo_full_name": repo_full_name(),
        "branch": git("branch", "--show-current") or "main",
        "command": config["command"],
        "docker_image": config["docker_image"],
        "remote_host": config["remote_host"],
        "remote_user": config["remote_user"],
        "remote_base_dir": config["remote_base_dir"],
        "slot_requirements": config["slot_requirements"],
        "exclude_machines": config.get("exclude_machines", []),
        "exclude_slots": config.get("exclude_slots", []),
        "cpus": resources["cpus"],
        "memory": resources["memory"],
        "disk": resources["disk"],
        "env": config["env"],
        "tags": config["tags"],
        "metadata": metadata,
        "output_patterns": config["output_patterns"],
        "input_jobs": [],
        "triggers": {},
    }


def job_payload(config: dict[str, Any], row: dict[str, str], index: int) -> dict[str, Any]:
    resources = config["resources"]
    title = f"BET Diagnostic | {row['model_label']}"
    description = (
        f"Independent BET 2026 Diagnostic ensemble fit {index:03d}/100. "
        f"Changed basis: Job 21641 Diagnostic with tau fixed at {row['tag_tau']}, "
        f"ordinary makepar/no fitted seed, Diagnostic selectivity "
        f"(F10 and F33 weak non-decreasing). "
        f"Preflight-verified draw: {row['model_label']}."
    )
    env = {
        **config["env"],
        "ENSEMBLE_SELECT": row["ensemble_id"],
        "JOB_KEY": row["ensemble_id"],
        "JOB_TITLE": title,
        "MODEL_LABEL": row["model_label"],
        "JOB_DESCRIPTION": description,
    }
    return {
        "docker_image": config["docker_image"],
        "remote_host": config["remote_host"],
        "remote_user": config["remote_user"],
        "remote_base_dir": config["remote_base_dir"],
        "slot_requirements": config["slot_requirements"],
        "exclude_machines": config.get("exclude_machines", []),
        "exclude_slots": config.get("exclude_slots", []),
        "cpus": resources["cpus"],
        "memory": resources["memory"],
        "disk": resources["disk"],
        "batch_name": "bet-2026-ensemble-tau-axis | Job 21641 Diagnostic | 100 independent Suva fits",
        "output_patterns": config["output_patterns"],
        "input_jobs": [],
        "env": env,
        "tags": {
            **config["tags"],
            "ensemble_id": row["ensemble_id"],
            "model_label": row["model_label"],
            "tag_tau": row["tag_tau"],
            "independent_fit": "true",
            "inputs_frozen": "true",
        },
        "metadata": {
            "campaign_row": index,
            "campaign_row_count": 100,
            "job_title": title,
            "job_description": description,
            "ensemble_id": row["ensemble_id"],
            "model_label": row["model_label"],
            "steepness": row["steepness"],
            "tag_tau": row["tag_tau"],
            "tau_fish_pars4": row["tau_fish_pars4"],
            "tag_mixing_k_cutoff": row["tag_mixing_k_cutoff"],
            "tag_mixing_source_file": row["tag_mixing_source_file"],
            "tag_mixing_source_branch": "SC22-IP10-regionMean",
            "tag_mixing_source_commit": "efe3107c72774ee73b5e6dc45e44cf51f0fc20e8",
            "tag_reporting_flag2": row["tag_reporting_flag2"],
            "tag_reporting": row["tag_reporting"],
            "tag_reporting_zero_mixing_exclusions": row["tag_reporting_zero_mixing_exclusions"],
            "m0_quarterly": row["m_age40_quarterly"],
            "lorenzen_log_intercept": row["lorenzen_log_intercept"],
            "effort_creep_primary": row["effort_creep_primary"],
            "effort_creep_secondary": row["effort_creep_secondary"],
            "effort_source_file": row["effort_source_file"],
            "initialization": row["initialization"],
            "diagnostic_source_job": 21641,
            "diagnostic_source_commit": "3abf0c64fb9b0c2d70b9c672dc7d9a655d3060d6",
            "diagnostic_model": "Diagnostic",
            "fixed_tau": row["tag_tau"],
            "selectivity_model": "Diagnostic",
            "inputs_frozen": True,
            "input_preflight": "strict-before-MFCL",
        },
        "triggers": {},
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--kflow-url", default=os.environ.get("KFLOW_URL", "http://127.0.0.1:8089"))
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument("--submit", action="store_true", help="Submit all 100 model jobs after registration.")
    args = parser.parse_args()
    config, rows = read_inputs()
    task = task_payload(config, rows)
    jobs = [job_payload(config, row, i) for i, row in enumerate(rows, 1)]
    if args.dry_run:
        print(json.dumps({"task": task, "jobs": jobs}, indent=2, sort_keys=True))
        return 0
    token = os.environ.get("KFLOW_API_TOKEN", "")
    if not token:
        raise SystemExit("Set KFLOW_API_TOKEN before registering the task.")
    base_url = args.kflow_url.rstrip("/")
    task_path = urllib.parse.quote(config["name"], safe="")
    response = api_json("POST", f"{base_url}/api/report/{task_path}", token, task)
    report = response.get("report", response)
    print(f"registered {report.get('code', config['name'])}: {task['repo_full_name']}@{task['branch']}")
    if not args.submit:
        return 0

    results: list[dict[str, Any] | None] = [None] * len(jobs)
    failures: list[str] = []
    with ThreadPoolExecutor(max_workers=20) as executor:
        futures = {
            executor.submit(api_json, "POST", f"{base_url}/api/job/{task_path}", token, payload): index
            for index, payload in enumerate(jobs)
        }
        for future in as_completed(futures):
            index = futures[future]
            try:
                results[index] = future.result()
            except Exception as exc:
                failures.append(f"{rows[index]['ensemble_id']}: {exc}")
    for row, result in zip(rows, results):
        job = (result or {}).get("job", result or {})
        print(f"submitted {row['ensemble_id']}: {job.get('job_number', job.get('id', 'submitted'))}")
    if failures:
        raise RuntimeError("Submission failures: " + " | ".join(failures))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
