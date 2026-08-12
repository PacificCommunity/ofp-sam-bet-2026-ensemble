#!/usr/bin/env python3
"""Validate, register, submit, and audit the BET more_tau Kflow campaign.

The campaign is intentionally resumable but never retries an ambiguous POST.
An existing task is read and checked, not overwritten. Existing JOB_KEY values
are collected over every API page before any missing row is submitted.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import math
import os
import random
import re
import subprocess
import time
import urllib.error
import urllib.parse
import urllib.request
from collections import Counter
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path
from typing import Any, Iterable

import yaml


ROOT = Path(__file__).resolve().parents[1]
CONFIG_PATH = ROOT / "kflow.yaml"
MODEL_DRAWS_PATH = ROOT / "design" / "model-draws.csv"
SITE_MAP_PATH = ROOT / "design" / "submission-sites.csv"

TASK_NAME = "bet-2026-ensemble-more-tau"
EXPECTED_REPOSITORY = "PacificCommunity/ofp-sam-bet-2026-ensemble"
EXPECTED_BRANCH = "more_tau"
EXPECTED_IMAGE = (
    "ghcr.io/pacificcommunity/tuna-flow:v2.5@sha256:"
    "c87f1f6d9d4f62dc447844b58afe35f96af175bf933cb6cffbbbe39a59172360"
)
EXPECTED_PROGRAM_PATH = "/home/mfcl/mfclo64"
EXPECTED_PROGRAM_SHA256 = (
    "f5bc1e232a86e51f920bce7271d8e0930d0b160e4d18dc46de44078f0fa24cd0"
)
EXPECTED_SITE_MAP_SHA256 = (
    "28ea8e11866cfa591f7ed1f7ad3a66b2832c4350eb230be35a4dceb028574454"
)
SITE_SEED_LABEL = "more_tau-site-split-v1"
SITE_ATTEMPT = 2242
SITE_SEED_HEX = (
    "d528c13cc9a6fda081ae2eb5e221a52df96528cf031d96e93c5fdc0861f29f01"
)
SITE_SEED_DECIMAL = (
    "96414644306705073331900794533701662312955457501360897334324090601130129792769"
)
SITE_CONTRACT = {
    "noumea": {
        "remote_host": "nouofpsubmit.corp.spc.int",
        "slot_prefix": "nouofp",
        "slot_requirements": (
            'regexp("^nouofp", Machine) && OpSys == "LINUX" '
            "&& HasDocker =?= True"
        ),
    },
    "suva": {
        "remote_host": "suvofpsubmit.corp.spc.int",
        "slot_prefix": "suvofp",
        "slot_requirements": (
            'regexp("^suvofp", Machine) && OpSys == "LINUX" '
            "&& HasDocker =?= True"
        ),
    },
}
AXIS_COLUMNS = (
    "steepness",
    "tag_tau",
    "tag_mixing_k_cutoff",
    "tag_reporting_flag2",
    "m_age40_quarterly",
    "effort_creep_primary",
)


class ApiHttpError(RuntimeError):
    """An HTTP response with a non-success status."""

    def __init__(self, method: str, url: str, status: int, detail: str):
        super().__init__(f"{method} {url} failed: HTTP {status}: {detail}")
        self.status = status


def api_json(
    method: str,
    url: str,
    token: str,
    payload: dict[str, Any] | None = None,
    *,
    timeout: int = 180,
) -> dict[str, Any]:
    headers = {"Authorization": f"Bearer {token}"}
    body = None
    if payload is not None:
        body = json.dumps(payload, separators=(",", ":")).encode("utf-8")
        headers["Content-Type"] = "application/json"
    request = urllib.request.Request(url, data=body, headers=headers, method=method)
    try:
        with urllib.request.urlopen(request, timeout=timeout) as response:
            raw = response.read()
    except urllib.error.HTTPError as exc:
        detail = exc.read().decode("utf-8", errors="replace")
        raise ApiHttpError(method, url, exc.code, detail) from exc
    return json.loads(raw.decode("utf-8")) if raw else {}


def git(*args: str, stderr: bool = False) -> str:
    result = subprocess.run(
        ["git", "-C", str(ROOT), *args],
        check=True,
        text=True,
        stdout=subprocess.PIPE,
        stderr=None if stderr else subprocess.DEVNULL,
    )
    return result.stdout.strip()


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def repo_full_name() -> str:
    remote = git("remote", "get-url", "origin").removesuffix(".git")
    if remote.startswith("git@github.com:"):
        return remote.split(":", 1)[1].strip("/")
    marker = "github.com/"
    if marker not in remote:
        raise ValueError(f"origin is not a GitHub repository: {remote}")
    return remote.split(marker, 1)[1].strip("/")


def read_yaml(path: Path) -> dict[str, Any]:
    with path.open(encoding="utf-8") as handle:
        value = yaml.safe_load(handle)
    if not isinstance(value, dict):
        raise ValueError(f"{path.relative_to(ROOT)} must contain one YAML mapping.")
    return value


def read_csv(path: Path) -> list[dict[str, str]]:
    with path.open(encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle))


def average_ranks(values: Iterable[float]) -> list[float]:
    values = list(values)
    order = sorted(range(len(values)), key=values.__getitem__)
    ranks = [0.0] * len(values)
    start = 0
    while start < len(order):
        stop = start + 1
        while stop < len(order) and values[order[stop]] == values[order[start]]:
            stop += 1
        average = ((start + 1) + stop) / 2.0
        for position in range(start, stop):
            ranks[order[position]] = average
        start = stop
    return ranks


def spearman(left: Iterable[float], right: Iterable[float]) -> float:
    x = average_ranks(left)
    y = average_ranks(right)
    x_mean = sum(x) / len(x)
    y_mean = sum(y) / len(y)
    numerator = sum((a - x_mean) * (b - y_mean) for a, b in zip(x, y))
    denominator = math.sqrt(
        sum((a - x_mean) ** 2 for a in x) * sum((b - y_mean) ** 2 for b in y)
    )
    if denominator == 0:
        raise ValueError("Cannot calculate a rank correlation with a constant vector.")
    return numerator / denominator


def generated_site_vector() -> list[str]:
    seed_text = f"{SITE_SEED_LABEL}:{SITE_ATTEMPT}"
    digest = hashlib.sha256(seed_text.encode("utf-8")).hexdigest()
    if digest != SITE_SEED_HEX or str(int(digest, 16)) != SITE_SEED_DECIMAL:
        raise AssertionError("Frozen site-assignment seed provenance is inconsistent.")
    indices = list(range(100))
    random.Random(int(digest, 16)).shuffle(indices)
    noumea_indices = set(indices[:50])
    return ["noumea" if index in noumea_indices else "suva" for index in range(100)]


def validate_site_map(
    rows: list[dict[str, str]], sites: list[dict[str, str]]
) -> dict[str, Any]:
    if sha256_file(SITE_MAP_PATH) != EXPECTED_SITE_MAP_SHA256:
        raise ValueError("design/submission-sites.csv has changed from its frozen hash.")
    expected_ids = [f"ensemble-{index:03d}" for index in range(1, 101)]
    if len(rows) != 100 or [row.get("ensemble_id") for row in rows] != expected_ids:
        raise ValueError("model-draws.csv must contain ensemble-001 through ensemble-100 in order.")
    if len(sites) != 100 or [row.get("ensemble_id") for row in sites] != expected_ids:
        raise ValueError("submission-sites.csv must match all 100 ordered ensemble IDs.")
    frozen_vector = [row.get("submission_site", "") for row in sites]
    if frozen_vector != generated_site_vector():
        raise ValueError("Frozen site mapping does not reproduce the recorded CPython shuffle.")
    if Counter(frozen_vector) != Counter({"noumea": 50, "suva": 50}):
        raise ValueError("Submission mapping must assign exactly 50 jobs to each site.")
    ranks = Counter()
    for row in sites:
        site = row["submission_site"]
        ranks[site] += 1
        if row.get("site_rank") != str(ranks[site]):
            raise ValueError(f"Invalid {site} site_rank for {row['ensemble_id']}.")

    rr_counts: dict[str, Counter[int]] = {site: Counter() for site in SITE_CONTRACT}
    tau_counts: dict[str, Counter[float]] = {site: Counter() for site in SITE_CONTRACT}
    numeric_site = [1.0 if site == "noumea" else 0.0 for site in frozen_vector]
    correlations: dict[str, float] = {}
    for model, site in zip(rows, frozen_vector):
        rr_counts[site][int(model["tag_reporting_flag2"])] += 1
        tau_counts[site][float(model["tag_tau"])] += 1
    expected_rr = Counter({0: 25, 1: 25})
    if any(counts != expected_rr for counts in rr_counts.values()):
        raise ValueError(f"Site assignment does not balance tag reporting 25/25: {rr_counts}")
    if tau_counts["noumea"] != Counter({4.96: 17, 5.14: 17, 5.20: 16}):
        raise ValueError(f"Unexpected Noumea tau allocation: {tau_counts['noumea']}")
    if tau_counts["suva"] != Counter({4.96: 16, 5.14: 17, 5.20: 17}):
        raise ValueError(f"Unexpected Suva tau allocation: {tau_counts['suva']}")
    for column in AXIS_COLUMNS:
        correlations[column] = spearman(
            numeric_site, [float(model[column]) for model in rows]
        )
    maximum = max(abs(value) for value in correlations.values())
    if abs(maximum - 0.02461829819586655) > 1e-12:
        raise ValueError(f"Frozen site-assignment correlation changed: {maximum:.17g}")
    return {
        "counts": dict(Counter(frozen_vector)),
        "rr_counts": {site: dict(counts) for site, counts in rr_counts.items()},
        "tau_counts": {
            site: {str(key): value for key, value in sorted(counts.items())}
            for site, counts in tau_counts.items()
        },
        "axis_spearman": correlations,
        "max_abs_axis_spearman": maximum,
    }


def read_inputs() -> tuple[dict[str, Any], list[dict[str, str]], list[dict[str, str]], dict[str, Any]]:
    config = read_yaml(CONFIG_PATH)
    rows = read_csv(MODEL_DRAWS_PATH)
    sites = read_csv(SITE_MAP_PATH)
    if len({row.get("ensemble_id") for row in rows}) != 100:
        raise ValueError("The campaign requires 100 unique ensemble rows.")
    if config.get("name") != TASK_NAME:
        raise ValueError(f"kflow.yaml name must be exactly {TASK_NAME}.")
    if config.get("docker_image") != EXPECTED_IMAGE:
        raise ValueError("Kflow image has changed from the approved sha256 digest.")
    if config.get("remote_host") != SITE_CONTRACT["noumea"]["remote_host"]:
        raise ValueError("Task default remote_host must be the full Noumea submitter hostname.")
    if config.get("slot_requirements") != SITE_CONTRACT["noumea"]["slot_requirements"]:
        raise ValueError("Task default must explicitly require Noumea Linux Docker slots.")
    resources = config.get("resources") or {}
    if resources != {"cpus": 2, "memory": "8GB", "disk": "20GB"}:
        raise ValueError("Every fit must retain 2 CPUs, 8GB memory and 20GB disk.")
    env = config.get("env") or {}
    required_env = {
        "PROGRAM_PATH": EXPECTED_PROGRAM_PATH,
        "EXPECTED_PROGRAM_SHA256": EXPECTED_PROGRAM_SHA256,
        "BET_PHASE10_11_CONVERGENCE": "-4",
        "TUNA_FLOW_RUNTIME_UPDATE": "never",
    }
    mismatch = {
        key: (env.get(key), expected)
        for key, expected in required_env.items()
        if str(env.get(key)) != expected
    }
    if mismatch:
        raise ValueError(f"Kflow environment contract mismatch: {mismatch}")
    for field in ("input_jobs", "input_files", "artifacts", "attachments"):
        if config.get(field):
            raise ValueError(f"Independent fits must not define {field}.")
    if config.get("triggers"):
        raise ValueError("Independent fits must not define triggers.")
    if config.get("output_patterns") != ["outputs/**"]:
        raise ValueError("Kflow must archive only outputs/**.")
    metadata = config.get("metadata") or {}
    required_metadata = {
        "tau_distribution": "three-point-empirical-anchor",
        "tau_levels": [4.96, 5.14, 5.20],
        "tau_counts": [33, 34, 33],
        "model_count": 100,
        "execution": "independent-concurrent-dual-site",
        "submission_sites": {"noumea": 50, "suva": 50},
        "submission_site_file": "design/submission-sites.csv",
        "submission_site_seed_label": SITE_SEED_LABEL,
        "submission_site_attempt": SITE_ATTEMPT,
        "submission_site_seed_sha256": SITE_SEED_HEX,
        "program_path": EXPECTED_PROGRAM_PATH,
        "program_sha256": EXPECTED_PROGRAM_SHA256,
    }
    for key, expected in required_metadata.items():
        if metadata.get(key) != expected:
            raise ValueError(
                f"kflow.yaml metadata.{key} must be {expected!r}; found {metadata.get(key)!r}."
            )
    site_summary = validate_site_map(rows, sites)
    return config, rows, sites, site_summary


def model_metadata(row: dict[str, str]) -> dict[str, str]:
    keys = (
        "ensemble_id",
        "model_label",
        "steepness",
        "tag_tau",
        "tau_fish_pars4",
        "tag_mixing_k_cutoff",
        "tag_mixing_source_file",
        "tag_reporting_flag2",
        "tag_reporting",
        "tag_reporting_zero_mixing_exclusions",
        "m_age40_quarterly",
        "lorenzen_log_intercept",
        "effort_creep_primary",
        "effort_creep_secondary",
        "effort_source_file",
        "initialization",
    )
    return {key: row[key] for key in keys}


def task_payload(
    config: dict[str, Any],
    rows: list[dict[str, str]],
    sites: list[dict[str, str]],
    commit: str,
) -> dict[str, Any]:
    resources = config["resources"]
    metadata = {
        **dict(config.get("metadata") or {}),
        "repository_commit": commit,
        "source_commit": commit,
        "submission_site_map_sha256": EXPECTED_SITE_MAP_SHA256,
        "program_path": EXPECTED_PROGRAM_PATH,
        "program_sha256": EXPECTED_PROGRAM_SHA256,
        "model_rows": [
            {
                **model_metadata(row),
                "submission_site": site["submission_site"],
                "site_rank": site["site_rank"],
            }
            for row, site in zip(rows, sites)
        ],
    }
    env = {
        **dict(config["env"]),
        "EXPECTED_REPOSITORY_COMMIT": commit,
    }
    return {
        "name": TASK_NAME,
        "description": config["description"],
        "repo_full_name": repo_full_name(),
        "branch": EXPECTED_BRANCH,
        "command": config["command"],
        "checkout": {"mode": "full", "paths": []},
        "docker_image": config["docker_image"],
        "remote_host": config["remote_host"],
        "remote_user": config["remote_user"],
        "remote_base_dir": config["remote_base_dir"],
        "slot_requirements": config["slot_requirements"],
        "exclude_machines": [],
        "exclude_slots": [],
        "cpus": resources["cpus"],
        "memory": resources["memory"],
        "disk": resources["disk"],
        "env": env,
        "tags": config["tags"],
        "metadata": metadata,
        "output_patterns": config["output_patterns"],
        "artifacts": [],
        "input_jobs": [],
        "triggers": {},
    }


def job_payload(
    config: dict[str, Any],
    row: dict[str, str],
    site_row: dict[str, str],
    index: int,
    commit: str,
) -> dict[str, Any]:
    site = site_row["submission_site"]
    site_contract = SITE_CONTRACT[site]
    resources = config["resources"]
    title = f"BET more_tau | {row['model_label']}"
    description = (
        f"Independent BET 2026 more_tau fit {index:03d}/100 on {site.title()}; "
        f"tau fixed at {row['tag_tau']}; all non-tau model inputs and the frozen "
        "all-axis pairing retained from main."
    )
    env = {
        **dict(config["env"]),
        "EXPECTED_REPOSITORY_COMMIT": commit,
        "ENSEMBLE_SELECT": row["ensemble_id"],
        "JOB_KEY": row["ensemble_id"],
        "JOB_TITLE": title,
        "MODEL_LABEL": row["model_label"],
        "JOB_DESCRIPTION": description,
    }
    return {
        "repo": repo_full_name(),
        "branch": EXPECTED_BRANCH,
        "command": config["command"],
        "checkout": {"mode": "full", "paths": []},
        "docker_image": config["docker_image"],
        "remote_host": site_contract["remote_host"],
        "remote_user": config["remote_user"],
        "remote_base_dir": config["remote_base_dir"],
        "slot_requirements": site_contract["slot_requirements"],
        "exclude_machines": [],
        "exclude_slots": [],
        "cpus": resources["cpus"],
        "memory": resources["memory"],
        "disk": resources["disk"],
        "batch_name": "BET 2026 more_tau | 100 independent dual-site fits",
        "output_patterns": config["output_patterns"],
        "input_jobs": [],
        "env": env,
        "tags": {
            **dict(config["tags"]),
            "ensemble_id": row["ensemble_id"],
            "model_label": row["model_label"],
            "tag_tau": row["tag_tau"],
            "submission_site": site,
            "independent_fit": "true",
            "inputs_frozen": "true",
        },
        "metadata": {
            **model_metadata(row),
            "campaign_row": index,
            "campaign_row_count": 100,
            "job_title": title,
            "job_description": description,
            "submission_site": site,
            "submission_site_rank": int(site_row["site_rank"]),
            "submission_site_map_sha256": EXPECTED_SITE_MAP_SHA256,
            "repository_commit": commit,
            "source_commit": commit,
            "program_path": EXPECTED_PROGRAM_PATH,
            "program_sha256": EXPECTED_PROGRAM_SHA256,
            "diagnostic_source_job": 21641,
            "diagnostic_source_commit": "3abf0c64fb9b0c2d70b9c672dc7d9a655d3060d6",
            "diagnostic_model": "Diagnostic",
            "fixed_tau": row["tag_tau"],
            "selectivity_model": "Diagnostic",
            "inputs_frozen": True,
            "independent_fit": True,
            "input_preflight": "strict-before-MFCL",
        },
        "triggers": {},
    }


def expect_equal(actual: Any, expected: Any, context: str) -> None:
    if actual != expected:
        raise ValueError(f"{context}: expected {expected!r}, found {actual!r}")


def expect_mapping_values(actual: Any, expected: dict[str, Any], context: str) -> None:
    if not isinstance(actual, dict):
        raise ValueError(f"{context}: expected a mapping, found {type(actual).__name__}")
    mismatches = {
        key: (actual.get(key), value)
        for key, value in expected.items()
        if str(actual.get(key)) != str(value)
    }
    if mismatches:
        raise ValueError(f"{context} mismatch: {mismatches}")


def get_report(base_url: str, token: str) -> dict[str, Any] | None:
    task_path = urllib.parse.quote(TASK_NAME, safe="")
    try:
        response = api_json("GET", f"{base_url}/api/report/{task_path}", token)
    except ApiHttpError as exc:
        if exc.status == 404:
            return None
        raise
    report = response.get("report", response)
    if not isinstance(report, dict):
        raise ValueError("Kflow task response does not contain a report mapping.")
    return report


def verify_task_contract(report: dict[str, Any], expected: dict[str, Any]) -> None:
    scalar_fields = (
        "name",
        "repo_full_name",
        "branch",
        "command",
        "docker_image",
        "remote_host",
        "remote_user",
        "remote_base_dir",
        "slot_requirements",
        "cpus",
        "memory",
        "disk",
    )
    expect_equal(report.get("code"), TASK_NAME, "task code")
    for field in scalar_fields:
        expect_equal(report.get(field), expected[field], f"task {field}")
    expect_equal(report.get("description"), expected["description"], "task description")
    for field in ("output_patterns", "artifacts", "input_jobs", "triggers"):
        expect_equal(report.get(field) or ([] if field != "triggers" else {}), expected[field], f"task {field}")
    expect_equal(report.get("exclude_machines") or [], [], "task exclude_machines")
    expect_equal(report.get("exclude_slots") or [], [], "task exclude_slots")
    expect_equal(report.get("checkout"), expected["checkout"], "task checkout")
    expect_mapping_values(report.get("env"), expected["env"], "task env")
    expect_mapping_values(report.get("tags"), expected["tags"], "task tags")
    metadata_required = {
        "repository_commit": expected["metadata"]["repository_commit"],
        "source_commit": expected["metadata"]["source_commit"],
        "submission_site_map_sha256": EXPECTED_SITE_MAP_SHA256,
        "program_path": EXPECTED_PROGRAM_PATH,
        "program_sha256": EXPECTED_PROGRAM_SHA256,
        "submission_site_attempt": SITE_ATTEMPT,
    }
    expect_mapping_values(report.get("metadata"), metadata_required, "task metadata")
    metadata = report.get("metadata") or {}
    rows = metadata.get("model_rows")
    if not isinstance(rows, list) or rows != expected["metadata"]["model_rows"]:
        raise ValueError("task metadata.model_rows does not match all 100 frozen campaign rows.")


def registered_source_commit(report: dict[str, Any]) -> str:
    """Return the immutable campaign SHA recorded by the registered task.

    Audits intentionally use the registered snapshot, not the checkout running
    the audit.  Every persisted copy of the SHA must be present, valid, and
    identical before it is trusted to reconstruct the expected job payloads.
    """

    metadata = report.get("metadata") if isinstance(report.get("metadata"), dict) else {}
    env = report.get("env") if isinstance(report.get("env"), dict) else {}
    recorded = {
        "task metadata.repository_commit": metadata.get("repository_commit"),
        "task metadata.source_commit": metadata.get("source_commit"),
        "task env.EXPECTED_REPOSITORY_COMMIT": env.get("EXPECTED_REPOSITORY_COMMIT"),
    }
    missing = [label for label, value in recorded.items() if value in (None, "")]
    if missing:
        raise ValueError(
            "Registered task does not contain complete source-commit provenance: "
            + ", ".join(missing)
        )
    invalid = {
        label: value
        for label, value in recorded.items()
        if re.fullmatch(r"[0-9a-fA-F]{40}", str(value)) is None
    }
    if invalid:
        raise ValueError(f"Registered task contains invalid source-commit provenance: {invalid}")
    normalized = {str(value).lower() for value in recorded.values()}
    if len(normalized) != 1:
        raise ValueError(f"Registered task source-commit provenance disagrees: {recorded}")
    return normalized.pop()


def register_or_verify_task(
    base_url: str, token: str, expected: dict[str, Any]
) -> tuple[dict[str, Any], bool]:
    existing = get_report(base_url, token)
    if existing is not None:
        verify_task_contract(existing, expected)
        return existing, False

    task_path = urllib.parse.quote(TASK_NAME, safe="")
    post_error: Exception | None = None
    try:
        api_json("POST", f"{base_url}/api/report/{task_path}", token, expected)
    except Exception as exc:  # POST may have committed before a transport timeout.
        post_error = exc
    reconciled = None
    for attempt in range(5):
        reconciled = get_report(base_url, token)
        if reconciled is not None:
            break
        if attempt < 4:
            time.sleep(1)
    if reconciled is None:
        if post_error is not None:
            raise RuntimeError(
                "Task registration failed and no task appeared during reconciliation."
            ) from post_error
        raise RuntimeError("Task registration returned without creating the task.")
    verify_task_contract(reconciled, expected)
    return reconciled, True


def job_key(job: dict[str, Any]) -> str:
    env = job.get("env") if isinstance(job.get("env"), dict) else {}
    metadata = job.get("metadata") if isinstance(job.get("metadata"), dict) else {}
    return str(job.get("job_key") or metadata.get("job_key") or env.get("JOB_KEY") or "")


def list_all_jobs(base_url: str, token: str) -> list[dict[str, Any]]:
    task_path = urllib.parse.quote(TASK_NAME, safe="")
    output: list[dict[str, Any]] = []
    for page in range(1, 10001):
        response = api_json(
            "GET", f"{base_url}/api/jobs/{task_path}/?page={page}", token, timeout=180
        )
        jobs = response.get("jobs")
        if not isinstance(jobs, list):
            raise ValueError(f"Kflow jobs page {page} is not a list.")
        if not jobs:
            return output
        output.extend(jobs)
    raise RuntimeError("Kflow pagination exceeded 10,000 non-empty pages.")


def jobs_by_key(jobs: list[dict[str, Any]], expected_keys: set[str]) -> dict[str, dict[str, Any]]:
    grouped: dict[str, list[dict[str, Any]]] = {}
    missing_key: list[Any] = []
    for job in jobs:
        key = job_key(job)
        if not key:
            missing_key.append(job.get("job_number", job.get("id")))
            continue
        grouped.setdefault(key, []).append(job)
    if missing_key:
        raise ValueError(f"Task contains jobs without JOB_KEY: {missing_key}")
    duplicates = {key: len(items) for key, items in grouped.items() if len(items) != 1}
    if duplicates:
        raise ValueError(f"Duplicate JOB_KEY values detected; refusing to submit: {duplicates}")
    unexpected = sorted(set(grouped) - expected_keys)
    if unexpected:
        raise ValueError(f"Task contains unexpected JOB_KEY values: {unexpected}")
    return {key: items[0] for key, items in grouped.items()}


def get_job_detail(base_url: str, token: str, summary: dict[str, Any]) -> dict[str, Any]:
    reference = summary.get("job_number") or summary.get("id")
    if reference in (None, ""):
        raise ValueError("Kflow job summary has no job_number or id.")
    response = api_json("GET", f"{base_url}/api/job/{reference}/", token, timeout=180)
    job = response.get("job", response)
    if not isinstance(job, dict):
        raise ValueError(f"Kflow job {reference} detail is not a mapping.")
    return job


def verify_job_remote_target(
    job: dict[str, Any], expected: dict[str, Any], key: str
) -> None:
    """Verify the requested submit host and any persisted execution-slot hint.

    Kflow stores jobs sent through its own Noumea submitter as ``local``.  That
    single normalization is accepted; Suva and every other alias remain exact.
    When Kflow exposes the selected remote_host_slot, its site prefix must also
    agree with the requested site.
    """

    expected_host = expected["remote_host"]
    site_matches = [
        (site, contract)
        for site, contract in SITE_CONTRACT.items()
        if contract["remote_host"] == expected_host
    ]
    if len(site_matches) != 1:
        raise ValueError(f"job {key} has an unknown expected remote host {expected_host!r}")
    site, contract = site_matches[0]
    actual_host = job.get("remote_host")
    normalized_noumea = (
        site == "noumea"
        and expected_host == SITE_CONTRACT["noumea"]["remote_host"]
        and actual_host == "local"
    )
    if actual_host != expected_host and not normalized_noumea:
        raise ValueError(
            f"job {key} remote_host: expected {expected_host!r}, found {actual_host!r}"
        )

    remote_host_slot = job.get("remote_host_slot")
    if remote_host_slot not in (None, ""):
        slot = str(remote_host_slot).rsplit("@", 1)[-1].casefold()
        prefix = str(contract["slot_prefix"]).casefold()
        if not slot.startswith(prefix):
            raise ValueError(
                f"job {key} remote_host_slot must start with {prefix!r} for {site}; "
                f"found {remote_host_slot!r}"
            )


def verify_job_contract(job: dict[str, Any], expected: dict[str, Any]) -> None:
    key = expected["env"]["JOB_KEY"]
    scalar_pairs = {
        "report_code": TASK_NAME,
        "repo_full_name": expected["repo"],
        "branch": expected["branch"],
        "command": expected["command"],
        "docker_image": expected["docker_image"],
        "remote_user": expected["remote_user"],
        "cpus": expected["cpus"],
        "memory": expected["memory"],
        "disk": expected["disk"],
        "batch_name": expected["batch_name"],
    }
    for field, value in scalar_pairs.items():
        expect_equal(job.get(field), value, f"job {key} {field}")
    verify_job_remote_target(job, expected, key)
    remote_dir = str(job.get("remote_dir") or "")
    expected_prefix = f"{expected['remote_base_dir'].rstrip('/')}/"
    if not remote_dir.startswith(expected_prefix):
        raise ValueError(
            f"job {key} remote_dir must be under {expected_prefix!r}; found {remote_dir!r}"
        )
    expect_mapping_values(job.get("env"), expected["env"], f"job {key} env")
    expect_mapping_values(job.get("tags"), expected["tags"], f"job {key} tags")
    metadata_required = {
        field: expected["metadata"][field]
        for field in (
            "ensemble_id",
            "model_label",
            "tag_tau",
            "tau_fish_pars4",
            "submission_site",
            "submission_site_rank",
            "submission_site_map_sha256",
            "repository_commit",
            "source_commit",
            "program_path",
            "program_sha256",
            "fixed_tau",
            "inputs_frozen",
            "independent_fit",
        )
    }
    expect_mapping_values(job.get("metadata"), metadata_required, f"job {key} metadata")
    details = job.get("details") if isinstance(job.get("details"), dict) else {}
    expect_equal(
        details.get("git_commit_sha"),
        expected["metadata"]["repository_commit"],
        f"job {key} resolved git_commit_sha",
    )
    report_spec = details.get("report_spec") if isinstance(details.get("report_spec"), dict) else {}
    expect_equal(
        report_spec.get("slot_requirements"),
        expected["slot_requirements"],
        f"job {key} slot_requirements",
    )
    expect_equal(report_spec.get("input_jobs") or [], [], f"job {key} input_jobs")
    expect_equal(
        report_spec.get("resolved_input_jobs") or [], [], f"job {key} resolved_input_jobs"
    )
    expect_equal(report_spec.get("input_files") or [], [], f"job {key} input_files/attachments")
    expect_equal(report_spec.get("triggers") or {}, {}, f"job {key} triggers")
    expect_equal(report_spec.get("artifacts") or [], [], f"job {key} attachments/artifacts")
    expect_equal(
        report_spec.get("output_patterns"), expected["output_patterns"], f"job {key} output_patterns"
    )
    expect_equal(report_spec.get("checkout"), expected["checkout"], f"job {key} checkout")
    expect_equal(report_spec.get("exclude_machines") or [], [], f"job {key} exclude_machines")
    expect_equal(report_spec.get("exclude_slots") or [], [], f"job {key} exclude_slots")


def verify_jobs(
    base_url: str,
    token: str,
    summaries: dict[str, dict[str, Any]],
    expected_payloads: dict[str, dict[str, Any]],
    max_workers: int,
) -> None:
    failures: list[str] = []
    with ThreadPoolExecutor(max_workers=max(1, min(max_workers, len(summaries) or 1))) as pool:
        futures = {
            pool.submit(get_job_detail, base_url, token, summary): key
            for key, summary in summaries.items()
        }
        for future in as_completed(futures):
            key = futures[future]
            try:
                verify_job_contract(future.result(), expected_payloads[key])
            except Exception as exc:
                failures.append(f"{key}: {exc}")
    if failures:
        raise ValueError("Kflow job contract audit failed: " + " | ".join(sorted(failures)))


def wait_for_resolved_checkouts(
    base_url: str,
    token: str,
    summaries: dict[str, dict[str, Any]],
    keys: list[str],
    max_workers: int,
    settle_attempts: int,
) -> None:
    """Wait for ambiguous POSTs to finish without issuing another POST."""

    pending = set(keys)
    last_state: dict[str, str] = {}
    terminal_without_checkout = {
        "cancelled",
        "failed",
        "held",
        "removed",
        "submit-failed",
    }
    for attempt in range(max(1, settle_attempts)):
        failures: list[str] = []
        with ThreadPoolExecutor(max_workers=max(1, min(max_workers, len(pending) or 1))) as pool:
            futures = {
                pool.submit(get_job_detail, base_url, token, summaries[key]): key
                for key in pending
            }
            for future in as_completed(futures):
                key = futures[future]
                try:
                    job = future.result()
                except Exception as exc:
                    last_state[key] = f"GET failed: {exc}"
                    continue
                details = job.get("details") if isinstance(job.get("details"), dict) else {}
                checkout = str(details.get("git_commit_sha") or "").strip()
                status = str(job.get("status") or "unknown").strip().lower()
                last_state[key] = f"status={status}, git_commit_sha={checkout or 'absent'}"
                if checkout:
                    pending.discard(key)
                elif status in terminal_without_checkout:
                    failures.append(f"{key}: {last_state[key]}")
        if failures:
            raise RuntimeError(
                "Kflow jobs reached a terminal state without a resolved checkout: "
                + " | ".join(sorted(failures))
            )
        if not pending:
            return
        if attempt + 1 < settle_attempts:
            time.sleep(2)
    raise RuntimeError(
        "Kflow jobs were created but their checkout provenance did not settle; no POST was retried: "
        + json.dumps({key: last_state.get(key, "not read") for key in sorted(pending)})
    )


def submit_missing_jobs(
    base_url: str,
    token: str,
    payloads: dict[str, dict[str, Any]],
    existing: dict[str, dict[str, Any]],
    max_workers: int,
    reconcile_attempts: int,
    settle_attempts: int,
) -> tuple[dict[str, dict[str, Any]], list[str]]:
    pending = [key for key in payloads if key not in existing]
    if not pending:
        return existing, []
    task_path = urllib.parse.quote(TASK_NAME, safe="")
    post_errors: dict[str, str] = {}
    with ThreadPoolExecutor(max_workers=max(1, min(max_workers, len(pending)))) as pool:
        futures = {
            pool.submit(
                api_json,
                "POST",
                f"{base_url}/api/job/{task_path}",
                token,
                payloads[key],
                timeout=180,
            ): key
            for key in pending
        }
        for future in as_completed(futures):
            key = futures[future]
            try:
                future.result()
            except Exception as exc:
                # Do not retry. A timed-out POST may already have inserted a job.
                post_errors[key] = str(exc)

    expected_keys = set(payloads)
    if post_errors:
        # A timed-out POST may still be executing server-side. Wait before the
        # first read-only reconciliation so the committed row can become visible.
        time.sleep(2)
    reconciled: dict[str, dict[str, Any]] = {}
    for attempt in range(max(1, reconcile_attempts)):
        reconciled = jobs_by_key(list_all_jobs(base_url, token), expected_keys)
        if all(key in reconciled for key in pending):
            break
        if attempt + 1 < reconcile_attempts:
            time.sleep(2)
    missing = sorted(expected_keys - set(reconciled))
    if missing:
        errors = {key: post_errors.get(key, "POST returned but JOB_KEY is absent") for key in missing}
        raise RuntimeError(
            "Submission reconciliation is incomplete. Missing JOB_KEY values were not retried: "
            + json.dumps(errors, sort_keys=True)
        )
    wait_for_resolved_checkouts(
        base_url,
        token,
        reconciled,
        pending,
        max_workers,
        settle_attempts,
    )
    return reconciled, pending


def ensure_publish_contract() -> str:
    branch = git("branch", "--show-current")
    if branch != EXPECTED_BRANCH:
        raise ValueError(f"Campaign mutation is allowed only from branch {EXPECTED_BRANCH}, not {branch}.")
    dirty = git("status", "--porcelain")
    if dirty:
        raise ValueError("Commit all campaign files before registering or submitting Kflow jobs.")
    head = git("rev-parse", "HEAD")
    remote = git("ls-remote", "--heads", "origin", f"refs/heads/{EXPECTED_BRANCH}", stderr=True)
    fields = remote.split()
    if len(fields) != 2 or fields[0] != head:
        raise ValueError(
            f"origin/{EXPECTED_BRANCH} must exist and equal local HEAD {head} before Kflow mutation."
        )
    return head


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--kflow-url", default=os.environ.get("KFLOW_URL", "http://127.0.0.1:8089")
    )
    parser.add_argument("--max-workers", type=int, default=10)
    parser.add_argument("--reconcile-attempts", type=int, default=5)
    parser.add_argument(
        "--settle-attempts",
        type=int,
        default=90,
        help="Two-second read-only polls allowed while submitted checkout provenance settles.",
    )
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--dry-run", action="store_true", help="Validate and print all payloads; no API calls.")
    mode.add_argument("--register", action="store_true", help="Create the absent task, or verify it without overwrite.")
    mode.add_argument("--submit", action="store_true", help="Register/verify, then submit only absent JOB_KEY rows.")
    mode.add_argument("--audit", action="store_true", help="Read-only audit of an existing task and all present jobs.")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    config, rows, sites, site_summary = read_inputs()
    repository = repo_full_name()
    if repository.casefold() != EXPECTED_REPOSITORY.casefold():
        raise ValueError(f"Expected repository {EXPECTED_REPOSITORY}, found {repository}.")
    commit = git("rev-parse", "HEAD")
    task = task_payload(config, rows, sites, commit)
    jobs = [
        job_payload(config, row, site, index, commit)
        for index, (row, site) in enumerate(zip(rows, sites), 1)
    ]
    payloads = {payload["env"]["JOB_KEY"]: payload for payload in jobs}
    if len(payloads) != 100:
        raise ValueError("Generated job payloads do not contain 100 unique JOB_KEY values.")

    if args.dry_run:
        print(
            json.dumps(
                {"site_validation": site_summary, "task": task, "jobs": jobs},
                indent=2,
                sort_keys=True,
            )
        )
        return 0

    token = os.environ.get("KFLOW_API_TOKEN", "")
    if not token:
        raise SystemExit("Set KFLOW_API_TOKEN before accessing Kflow.")
    base_url = args.kflow_url.rstrip("/")
    if args.audit:
        report = get_report(base_url, token)
        if report is None:
            raise SystemExit(f"Kflow task {TASK_NAME} does not exist.")
        commit = registered_source_commit(report)
        task = task_payload(config, rows, sites, commit)
        jobs = [
            job_payload(config, row, site, index, commit)
            for index, (row, site) in enumerate(zip(rows, sites), 1)
        ]
        payloads = {payload["env"]["JOB_KEY"]: payload for payload in jobs}
        verify_task_contract(report, task)
        existing = jobs_by_key(list_all_jobs(base_url, token), set(payloads))
        verify_jobs(base_url, token, existing, payloads, args.max_workers)
        print(json.dumps({"task": TASK_NAME, "jobs_audited": len(existing), "site_validation": site_summary}, indent=2))
        return 0

    commit = ensure_publish_contract()
    # Rebuild payloads after the remote-HEAD check so their provenance is exact.
    task = task_payload(config, rows, sites, commit)
    jobs = [
        job_payload(config, row, site, index, commit)
        for index, (row, site) in enumerate(zip(rows, sites), 1)
    ]
    payloads = {payload["env"]["JOB_KEY"]: payload for payload in jobs}
    _, created = register_or_verify_task(base_url, token, task)
    existing = jobs_by_key(list_all_jobs(base_url, token), set(payloads))
    verify_jobs(base_url, token, existing, payloads, args.max_workers)

    submitted: list[str] = []
    if args.submit:
        existing, submitted = submit_missing_jobs(
            base_url,
            token,
            payloads,
            existing,
            args.max_workers,
            args.reconcile_attempts,
            args.settle_attempts,
        )
        if len(existing) != 100:
            raise RuntimeError(f"Expected 100 reconciled jobs, found {len(existing)}.")
        verify_jobs(base_url, token, existing, payloads, args.max_workers)
        site_counts = Counter(
            payloads[key]["metadata"]["submission_site"] for key in existing
        )
        if site_counts != Counter({"noumea": 50, "suva": 50}):
            raise RuntimeError(f"Post-submit site count mismatch: {site_counts}")

    print(
        json.dumps(
            {
                "task": TASK_NAME,
                "task_created": created,
                "existing_before_submit": len(existing) - len(submitted),
                "submitted_now": len(submitted),
                "jobs_verified": len(existing),
                "site_validation": site_summary,
                "repository_commit": commit,
            },
            indent=2,
            sort_keys=True,
        )
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
