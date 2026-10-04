#!/usr/bin/env python3
"""Verify the archived paired fits and their recorded provenance (standard library only)."""
import csv
import hashlib
import json
import math
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


def read_csv(path):
    with (ROOT / path).open(newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle))


def require(condition, message):
    if not condition:
        raise ValueError(message)


def scalar(path, header):
    lines = path.read_text().splitlines()
    positions = [i for i, line in enumerate(lines) if line.strip() == header]
    require(len(positions) == 1, f"Missing or repeated {header}: {path}")
    value = float(lines[positions[0] + 1])
    require(math.isfinite(value), f"Nonfinite {header}: {path}")
    return value


def verify_ini(anchor, counterpart):
    left = anchor.read_text().splitlines()
    right = counterpart.read_text().splitlines()
    require(len(left) == len(right), f"INI length differs: {counterpart}")
    start = left.index("# tag flags") + 1
    tag_rows = set(range(start, start + 98))
    changed = 0
    for i, (a, b) in enumerate(zip(left, right)):
        if i not in tag_rows:
            require(a == b, f"Non-tag INI line differs at {i + 1}: {counterpart}")
            continue
        x, y = a.split(), b.split()
        require(len(x) == len(y) == 10, f"Unexpected tag row: {counterpart}:{i + 1}")
        require(x[:1] + x[2:] == y[:1] + y[2:], f"Other tag field differs: {counterpart}:{i + 1}")
        if int(x[0]) == 0:
            require(x[1] == y[1] == "1" and a == b, f"Zero-mixing sentinel changed: {counterpart}")
        else:
            require(int(x[0]) > 0 and x[1] == "0" and y[1] == "1", f"Unexpected RR flags: {counterpart}")
            changed += 1
    return changed


def main():
    runs = read_csv("rr-test/results/run-manifest.csv")
    design = read_csv("rr-test/model-draws.csv")
    files = read_csv("rr-test/results/file-manifest.csv")
    require(len(runs) == len(design) == 34, "Expected all 34 planned pairs.")
    require(len({row["anchor_id"] for row in runs}) == 34, "Repeated anchors.")
    require({row["rr1_id"] for row in runs} == {row["ensemble_id"] for row in design}, "Run/design membership differs.")
    require({row["anchor_id"] for row in runs} == {row["anchor_ensemble_id"] for row in design}, "Anchor membership differs.")
    require(len({row["rr1_kflow_job"] for row in runs}) == 34, "Repeated source job numbers.")
    recorded_paths = set()
    for row in files:
        path = ROOT / row["path"]
        require(path.resolve().is_relative_to(ROOT), f"Non-repository path: {path}")
        require(row["path"] not in recorded_paths, f"Repeated file: {path}")
        recorded_paths.add(row["path"])
        require(path.is_file(), f"Missing file: {path}")
        require(path.stat().st_size == int(row["bytes"]), f"Size mismatch: {path}")
        require(hashlib.sha256(path.read_bytes()).hexdigest() == row["sha256"], f"SHA-256 mismatch: {path}")
    source = {row["ensemble_id"]: row for row in read_csv("data/ensemble/retained-final-par-rr-split-manifest.csv")}
    provenance = {row["ensemble_id"]: row for row in read_csv("data/ensemble/retained-final-par-manifest.csv")}
    passed, failed = [], []
    for row in runs:
        anchor = row["anchor_id"]
        model = row["rr1_id"]
        require(model == anchor.replace("ensemble-", "rrtest-", 1) + "-rr1", f"Pair mapping changed: {model}")
        require(row["rr0_kflow_job"] == provenance[anchor]["kflow_job"], f"Anchor job changed: {anchor}")
        for kind in ("final_par", "bet_ini", "plot_rep"):
            path = source[anchor][kind + "_split_path"]
            entries = [entry for entry in files if entry["path"] == path]
            require(len(entries) == 1 and entries[0]["sha256"] == source[anchor][kind + "_sha256"], f"Anchor source hash changed: {path}")
        mgc0 = scalar(ROOT / source[anchor]["final_par_split_path"], "# Maximum magnitude gradient value")
        require(mgc0 <= 1e-4 and math.isclose(mgc0, float(row["rr0_mgc"]), abs_tol=1e-15), f"RR0 MGC mismatch: {anchor}")
        folder = ROOT / "rr-test/fits" / model
        if row["rr1_status"] == "completed":
            require(row["included_in_pair_summary"] == "true", f"Completed pair missing: {model}")
            mgc1 = scalar(folder / "final.par", "# Maximum magnitude gradient value")
            require(mgc1 <= 1e-4 and math.isclose(mgc1, float(row["rr1_mgc"]), abs_tol=1e-15), f"RR1 MGC mismatch: {model}")
            verify_ini(ROOT / source[anchor]["bet_ini_split_path"], folder / "bet.ini")
            for path in folder.iterdir():
                require(path.is_file() and str(path.relative_to(ROOT)) in recorded_paths, f"Unrecorded fit file: {path}")
            passed.append(model)
        else:
            require(row["rr1_status"] == "failed" and row["included_in_pair_summary"] == "false" and not folder.exists(), f"Unexpected failed-fit state: {model}")
            text = (ROOT / "rr-test/failures" / model / "status.txt").read_text()
            require("Exit code: 134" in text and "Job id: " + row["rr1_source_id"] in text, f"Failure identity differs: {model}")
            failed.append(model)
    require(len(passed) == 30 and set(failed) == {"rrtest-001-rr1", "rrtest-022-rr1", "rrtest-025-rr1", "rrtest-080-rr1"}, "Historical 30/4 membership differs.")
    references = json.loads((ROOT / "rr-test/reference/reference-manifest.json").read_text())
    for row in references:
        path = ROOT / "rr-test/reference" / row["original_name"]
        require(path.stat().st_size == row["bytes"] and hashlib.sha256(path.read_bytes()).hexdigest() == row["sha256"], f"Original analysis changed: {path}")
    print(f"Verified {len(runs)} source jobs, {len(files)} original files, {len(references)} original analysis files; 30 exact MGC-passing pairs and 4 failed fits.")


if __name__ == "__main__":
    main()
