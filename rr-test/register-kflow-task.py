#!/usr/bin/env python3
"""Register and submit the exact RR paired-fit campaign to Kflow."""

from __future__ import annotations

import argparse
import csv
import json
import os
import subprocess
import sys
import urllib.error
import urllib.parse
import urllib.request
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path
from typing import Any

import yaml


ROOT = Path(__file__).resolve().parents[1]
RR_ROOT = ROOT / "rr-test"
CHECKS_ROOT = ROOT.parent / "ofp-sam-bet-2026-checks"
CHECKS_SUBMITTER = CHECKS_ROOT / "scripts" / "submit_kflow_checks.py"
EXPECTED_IMAGE = (
    "ghcr.io/pacificcommunity/tuna-flow:v2.5@"
    "sha256:c87f1f6d9d4f62dc447844b58afe35f96af175bf933cb6cffbbbe39a59172360"
)
FIT_MFCL_PATH = "/home/mfcl/mfclo64"
HESSIAN_MFCL_PATH = "/home/mfcl/./mfclo64"
FIT_MFCL_SHA256 = "f5bc1e232a86e51f920bce7271d8e0930d0b160e4d18dc46de44078f0fa24cd0"


def api_json(
    method: str,
    url: str,
    token: str,
    payload: dict[str, Any] | None = None,
) -> dict[str, Any]:
    headers = {"Authorization": f"Bearer {token}"}
    body = None
    if payload is not None:
        body = json.dumps(payload).encode("utf-8")
        headers["Content-Type"] = "application/json"
    request = urllib.request.Request(url, data=body, headers=headers, method=method)
    try:
        with urllib.request.urlopen(request, timeout=120) as response:
            raw = response.read()
    except urllib.error.HTTPError as exc:
        detail = exc.read().decode("utf-8", errors="replace")
        raise RuntimeError(f"{method} {url} failed: HTTP {exc.code}: {detail}") from exc
    return json.loads(raw.decode("utf-8")) if raw else {}


def git(repo: Path, *args: str) -> str:
    return subprocess.run(
        ["git", "-C", str(repo), *args],
        check=True,
        text=True,
        stdout=subprocess.PIPE,
    ).stdout.strip()


def repo_full_name(repo: Path) -> str:
    remote = git(repo, "remote", "get-url", "origin").removesuffix(".git")
    if remote.startswith("git@github.com:"):
        return remote.split(":", 1)[1].strip("/")
    if "github.com/" not in remote:
        raise ValueError(f"Unsupported GitHub remote: {remote}")
    return remote.split("github.com/", 1)[1].strip("/")


def branch_name(repo: Path) -> str:
    branch = git(repo, "branch", "--show-current")
    if not branch:
        raise ValueError(f"Detached HEAD is not supported for Kflow submission: {repo}")
    return branch


def read_inputs() -> tuple[dict[str, Any], list[dict[str, str]]]:
    with (RR_ROOT / "kflow.yaml").open(encoding="utf-8") as handle:
        config = yaml.safe_load(handle)
    with (RR_ROOT / "model-draws.csv").open(encoding="utf-8", newline="") as handle:
        rows = list(csv.DictReader(handle))

    if len(rows) != 34 or len({row["ensemble_id"] for row in rows}) != 34:
        raise ValueError("The paired campaign requires exactly 34 unique RR1 rows.")
    if len({row["anchor_ensemble_id"] for row in rows}) != 34:
        raise ValueError("The paired campaign requires exactly 34 unique RR0 anchors.")
    if any(row["tag_reporting_flag2"] != "1" or row["tag_reporting"] != "exclusion" for row in rows):
        raise ValueError("Every submitted paired row must request RR1 exclusion.")
    if any(row["ensemble_id"] != row["anchor_ensemble_id"].replace("ensemble-", "rrtest-", 1) + "-rr1" for row in rows):
        raise ValueError("Paired model IDs do not map deterministically from their anchors.")
    rows.sort(key=lambda row: row["anchor_ensemble_id"])

    if str(config.get("docker_image")) != EXPECTED_IMAGE:
        raise ValueError("The paired fits must use the exact pinned ensemble v2.5 image.")
    if config.get("remote_host") != "suva" or "suvofp" not in str(config.get("slot_requirements", "")):
        raise ValueError("The paired campaign must be explicitly pinned to Suva nodes.")
    if config.get("command") != "bash rr-test/run.sh":
        raise ValueError("Unexpected paired-fit command.")
    if config.get("env", {}).get("PROGRAM_PATH") != FIT_MFCL_PATH:
        raise ValueError("Paired fits must use the original ensemble image MFCL path.")
    if int(config.get("metadata", {}).get("submitted_model_count", 0)) != 34:
        raise ValueError("Kflow metadata must declare all 34 paired reruns.")
    return config, rows


def model_metadata(row: dict[str, str]) -> dict[str, Any]:
    return {
        "ensemble_id": row["ensemble_id"],
        "anchor_ensemble_id": row["anchor_ensemble_id"],
        "model_selector": row["ensemble_id"],
        "model_selectors": [row["ensemble_id"]],
        "model_key": row["ensemble_id"],
        "model_label": row["model_label"],
        "steepness": row["steepness"],
        "steepness_prior_quantile": row["steepness_prior_quantile"],
        "tag_tau": row["tag_tau"],
        "tau_fish_pars4": row["tau_fish_pars4"],
        "tag_mixing_k_cutoff": row["tag_mixing_k_cutoff"],
        "tag_mixing_source_file": row["tag_mixing_source_file"],
        "tag_reporting_flag2": row["tag_reporting_flag2"],
        "tag_reporting": row["tag_reporting"],
        "tag_reporting_zero_mixing_exclusions": row["tag_reporting_zero_mixing_exclusions"],
        "zero_mixing_events": row["zero_mixing_events"],
        "m0_quarterly": row["m_age40_quarterly"],
        "lorenzen_log_intercept": row["lorenzen_log_intercept"],
        "m_prior_quantile": row["m_prior_quantile"],
        "effort_creep_primary": row["effort_creep_primary"],
        "effort_creep_secondary": row["effort_creep_secondary"],
        "effort_source_file": row["effort_source_file"],
        "initialization": row["initialization"],
        "pairing_version": row["pairing_version"],
    }


def task_payload(config: dict[str, Any], rows: list[dict[str, str]]) -> dict[str, Any]:
    resources = config["resources"]
    metadata = {
        **config.get("metadata", {}),
        "repository_commit": git(ROOT, "rev-parse", "HEAD"),
        "mfcl_path": FIT_MFCL_PATH,
        "mfcl_sha256": FIT_MFCL_SHA256,
        "model_selectors": [row["ensemble_id"] for row in rows],
        "model_rows": [model_metadata(row) for row in rows],
    }
    return {
        "name": config["name"],
        "description": config["description"],
        "repo_full_name": repo_full_name(ROOT),
        "branch": branch_name(ROOT),
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


def fit_job_payload(
    config: dict[str, Any],
    row: dict[str, str],
    index: int,
) -> dict[str, Any]:
    resources = config["resources"]
    selector = row["ensemble_id"]
    anchor = row["anchor_ensemble_id"]
    title = f"BET exact RR pair | {anchor} -> {selector}"
    description = (
        f"RR1 counterpart {index:02d}/34 for retained RR0 anchor {anchor}. "
        "All model-defining inputs are held exactly fixed except requested tag flag column 2."
    )
    env = {
        **config["env"],
        "RR_TEST_SELECT": selector,
        "ENSEMBLE_SELECT": selector,
        "JOB_KEY": selector,
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
        "batch_name": "bet-2026 exact RR paired reruns | 34 independent Suva fits",
        "output_patterns": config["output_patterns"],
        "input_jobs": [],
        "env": env,
        "tags": {
            **config["tags"],
            "ensemble_id": selector,
            "anchor_ensemble_id": anchor,
            "model": selector,
            "exact_pair": "true",
            "independent_fit": "true",
            "inputs_frozen": "true",
        },
        "metadata": {
            **model_metadata(row),
            "campaign_row": index,
            "campaign_row_count": 34,
            "job_title": title,
            "job_description": description,
            "diagnostic_source_job": 21641,
            "diagnostic_source_commit": "3abf0c64fb9b0c2d70b9c672dc7d9a655d3060d6",
            "diagnostic_model": "Diagnostic",
            "fixed_tau": row["tag_tau"],
            "selectivity_model": "Diagnostic",
            "inputs_frozen": True,
            "input_preflight": "strict-exact-pair-before-MFCL",
            "paired_difference": "tag_reporting_flag2-only",
            "docker_image": config["docker_image"],
            "mfcl_program": FIT_MFCL_PATH,
            "mfcl_sha256": FIT_MFCL_SHA256,
        },
        "triggers": {},
    }


def job_reference(response: dict[str, Any]) -> str:
    job = response.get("job", response)
    for key in ("job_number", "number", "code", "id"):
        value = str(job.get(key) or "").strip().lstrip("#")
        if value:
            return value
    raise RuntimeError(f"Kflow did not return a job reference: {response!r}")


def checks_branch_and_commit() -> tuple[str, str]:
    if not CHECKS_SUBMITTER.is_file():
        raise FileNotFoundError(
            "Authoritative Hessian submitter is missing: " + str(CHECKS_SUBMITTER)
        )
    try:
        commit = git(CHECKS_ROOT, "rev-parse", "--verify", "refs/remotes/origin/main^{commit}")
    except subprocess.CalledProcessError as exc:
        raise RuntimeError("The authoritative checks repository has no origin/main ref.") from exc
    if len(commit) != 40:
        raise RuntimeError(f"Invalid origin/main commit for the checks repository: {commit!r}")
    return "main", commit


def hessian_command(
    *,
    config: dict[str, Any],
    row: dict[str, str],
    fit_job: str,
    kflow_url: str,
    partitions: int,
    checks_branch: str,
    submit_workers: int,
    dry_run: bool = False,
) -> list[str]:
    command = [
        sys.executable,
        str(CHECKS_SUBMITTER),
        "--kflow-url",
        kflow_url,
        "--task-prefix",
        "ofp-sam-bet-2026-rr-paired-check",
        "--checks",
        "hessian",
        "--models",
        row["ensemble_id"],
        "--input-jobs",
        fit_job,
        "--flow-group",
        "bet-2026-rr-paired-test",
        "--repo-full-name",
        repo_full_name(CHECKS_ROOT),
        "--branch",
        checks_branch,
        "--docker-image",
        config["docker_image"],
        "--cpus",
        "2",
        "--memory",
        "8GB",
        "--disk",
        "10GB",
        "--model-source-repo",
        repo_full_name(ROOT),
        "--model-source-ref",
        "RR_test",
        "--program-path",
        HESSIAN_MFCL_PATH,
        "--remote-host",
        config["remote_host"],
        "--remote-user",
        config["remote_user"],
        "--remote-base-dir",
        config["remote_base_dir"],
        "--slot-requirements",
        config["slot_requirements"],
        "--parallel-units",
        "true",
        "--auto-merge",
        "true",
        "--auto-attach",
        "true",
        "--submit-workers",
        str(submit_workers),
    ]
    if dry_run:
        command.append("--dry-run")
    if partitions < 1:
        raise ValueError("Hessian partition count must be positive.")
    return command


def hessian_environment(partitions: int) -> dict[str, str]:
    env = dict(os.environ)
    env.update(
        {
            "HESSIAN_NSPLIT": str(partitions),
            "HESSIAN_PARTS": " ".join(str(part) for part in range(1, partitions + 1)),
            "HESSIAN_KEEP_MATRIX": "true",
            "ATTACH_OUTPUT_MODE": "delta",
            "KFLOW_AUTO_MERGE": "true",
            "KFLOW_AUTO_ATTACH": "true",
            "KFLOW_PARALLEL_UNITS": "true",
            "FLOW_SPECIES": "BET",
            "FLOW_SPECIES_LABEL": "Bigeye tuna",
            "FLOW_ASSESSMENT_YEAR": "2026",
        }
    )
    return env


def submit_hessian(
    command: list[str],
    partitions: int,
    selector: str,
) -> tuple[str, str]:
    result = subprocess.run(
        command,
        cwd=CHECKS_ROOT,
        env=hessian_environment(partitions),
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    combined = "\n".join(part for part in (result.stderr.strip(), result.stdout.strip()) if part)
    if result.returncode != 0:
        raise RuntimeError(f"Hessian submission failed for {selector}:\n{combined}")
    return selector, combined


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--kflow-url", default=os.environ.get("KFLOW_URL", "http://127.0.0.1:8089"))
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument("--submit", action="store_true", help="Submit all 34 exact paired fits.")
    parser.add_argument(
        "--with-hessian",
        action="store_true",
        help="After fit submission, fan out three Hessian parts per fit and auto merge/attach them.",
    )
    parser.add_argument("--hessian-partitions", type=int, default=3)
    parser.add_argument("--fit-submit-workers", type=int, default=12)
    parser.add_argument("--hessian-submit-workers", type=int, default=4)
    args = parser.parse_args()

    if args.dry_run and args.submit:
        raise SystemExit("Choose either --dry-run or --submit, not both.")
    if args.with_hessian and not (args.dry_run or args.submit):
        raise SystemExit("--with-hessian requires --dry-run or --submit.")
    if args.hessian_partitions < 1:
        raise SystemExit("--hessian-partitions must be positive.")
    if not 1 <= args.fit_submit_workers <= 32 or not 1 <= args.hessian_submit_workers <= 32:
        raise SystemExit("Submission worker counts must be between 1 and 32.")

    config, rows = read_inputs()
    task = task_payload(config, rows)
    jobs = [fit_job_payload(config, row, index) for index, row in enumerate(rows, 1)]

    checks_branch = ""
    checks_commit = ""
    hessian_plan: list[dict[str, Any]] = []
    if args.with_hessian:
        checks_branch, checks_commit = checks_branch_and_commit()
        for row in rows:
            placeholder = f"FIT-JOB-{row['ensemble_id']}"
            command = hessian_command(
                config=config,
                row=row,
                fit_job=placeholder,
                kflow_url=args.kflow_url,
                partitions=args.hessian_partitions,
                checks_branch=checks_branch,
                submit_workers=args.hessian_partitions,
                dry_run=True,
            )
            hessian_plan.append(
                {
                    "model_selector": row["ensemble_id"],
                    "anchor_ensemble_id": row["anchor_ensemble_id"],
                    "depends_on_fit": placeholder,
                    "partitions": list(range(1, args.hessian_partitions + 1)),
                    "environment": {
                        "HESSIAN_NSPLIT": str(args.hessian_partitions),
                        "HESSIAN_PARTS": " ".join(
                            str(part) for part in range(1, args.hessian_partitions + 1)
                        ),
                        "HESSIAN_KEEP_MATRIX": "true",
                        "ATTACH_OUTPUT_MODE": "delta",
                    },
                    "auto_merge": True,
                    "auto_attach": True,
                    "checks_repo_branch": checks_branch,
                    "checks_repo_commit": checks_commit,
                    "command": command,
                }
            )

    if args.dry_run:
        print(
            json.dumps(
                {"task": task, "fit_jobs": jobs, "hessian_plan": hessian_plan},
                indent=2,
                sort_keys=True,
            )
        )
        return 0

    token = os.environ.get("KFLOW_API_TOKEN", "")
    if not token:
        raise SystemExit("Set KFLOW_API_TOKEN before registering or submitting the task.")
    base_url = args.kflow_url.rstrip("/")
    task_path = urllib.parse.quote(config["name"], safe="")
    response = api_json("POST", f"{base_url}/api/report/{task_path}", token, task)
    report = response.get("report", response)
    print(f"registered {report.get('code', config['name'])}: {task['repo_full_name']}@{task['branch']}")
    if not args.submit:
        return 0

    fit_responses: list[dict[str, Any] | None] = [None] * len(jobs)
    fit_failures: list[str] = []
    with ThreadPoolExecutor(max_workers=min(args.fit_submit_workers, len(jobs))) as executor:
        futures = {
            executor.submit(api_json, "POST", f"{base_url}/api/job/{task_path}", token, payload): index
            for index, payload in enumerate(jobs)
        }
        for future in as_completed(futures):
            index = futures[future]
            try:
                fit_responses[index] = future.result()
            except Exception as exc:  # keep a complete, ordered failure ledger
                fit_failures.append(f"{rows[index]['ensemble_id']}: {exc}")
    if fit_failures:
        raise RuntimeError("Fit submission failures: " + " | ".join(sorted(fit_failures)))

    fit_refs: list[str] = []
    for row, response in zip(rows, fit_responses):
        if response is None:
            raise RuntimeError(f"Missing Kflow response for {row['ensemble_id']}.")
        reference = job_reference(response)
        fit_refs.append(reference)
        print(f"submitted fit {row['ensemble_id']} (anchor {row['anchor_ensemble_id']}): job {reference}")

    if not args.with_hessian:
        return 0

    hessian_commands = [
        hessian_command(
            config=config,
            row=row,
            fit_job=fit_ref,
            kflow_url=args.kflow_url,
            partitions=args.hessian_partitions,
            checks_branch=checks_branch,
            submit_workers=args.hessian_partitions,
        )
        for row, fit_ref in zip(rows, fit_refs)
    ]
    hessian_results: list[str | None] = [None] * len(rows)
    hessian_failures: list[str] = []
    workers = min(args.hessian_submit_workers, len(rows))
    with ThreadPoolExecutor(max_workers=workers) as executor:
        futures = {
            executor.submit(submit_hessian, command, args.hessian_partitions, row["ensemble_id"]): index
            for index, (row, command) in enumerate(zip(rows, hessian_commands))
        }
        for future in as_completed(futures):
            index = futures[future]
            try:
                _, output = future.result()
                hessian_results[index] = output
            except Exception as exc:
                hessian_failures.append(f"{rows[index]['ensemble_id']}: {exc}")
    for row, output in zip(rows, hessian_results):
        if output:
            print(f"Hessian fan-out for {row['ensemble_id']}:")
            print(output)
    if hessian_failures:
        raise RuntimeError("Hessian submission failures: " + " | ".join(sorted(hessian_failures)))
    print(
        f"Submitted {len(rows)} fits plus {args.hessian_partitions} Hessian partitions per fit; "
        "each partition depends on its paired fit and each set will auto merge/attach."
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
