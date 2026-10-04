#!/usr/bin/env python3
"""Verify the archived paired fits and their recorded provenance (standard library only)."""
import argparse
import csv
import hashlib
import json
import math
from pathlib import Path
import re

from saved import Source


ROOT = Path(__file__).resolve().parents[1]

# These tolerances cover platform roundoff in derived CSV values only. Original
# archived files still require byte counts and exact SHA-256 below. At current
# scales objectives near 1e5 permit ~1e-7: math.isclose uses max(atol, rtol*scale),
# not their sum. Ratios and small deltas use the 1e-10 bound. Identities, periods, years,
# job numbers and counts are exact and never participate in numeric tolerance.
GENERATED_ATOL = 1e-10
GENERATED_RTOL = 1e-12
GENERATED_SCHEMAS = {
    "paired-quantities.csv": {
        "text": {"anchor_ensemble_id", "rr1_ensemble_id", "sb_recent_period", "sb0_recent_period", "f_recent_period"},
        "integer": {"rr0_kflow_job", "rr1_kflow_job", "zero_mixing_events"},
        "real": set("""
            steepness tag_tau m_age40_quarterly tag_mixing_k_cutoff
            effort_creep_primary effort_creep_secondary mgc_rr0 mgc_rr1
            objective_function_rr0 objective_function_rr1
            sb_recent_kt_rr0 sb_recent_kt_rr1 delta_sb_recent_kt_rr1_minus_rr0
            sb0_recent_kt_rr0 sb0_recent_kt_rr1 delta_sb0_recent_kt_rr1_minus_rr0
            sb_recent_sb0_rr0 sb_recent_sb0_rr1 delta_sb_recent_sb0_rr1_minus_rr0
            sb_recent_sbmsy_rr0 sb_recent_sbmsy_rr1 delta_sb_recent_sbmsy_rr1_minus_rr0
            f_recent_fmsy_rr0 f_recent_fmsy_rr1 delta_f_recent_fmsy_rr1_minus_rr0
            recent_mean_depletion_rr0 recent_mean_depletion_rr1 delta_recent_mean_depletion_rr1_minus_rr0
            historical_target_depletion_rr0 historical_target_depletion_rr1 delta_historical_target_depletion_rr1_minus_rr0
            recent_historical_target_ratio_rr0 recent_historical_target_ratio_rr1 delta_recent_historical_target_ratio_rr1_minus_rr0
        """.split()),
    },
    "paired-timeseries.csv": {
        "text": {"anchor_ensemble_id", "rr1_ensemble_id"},
        "integer": {"year"},
        "real": set("""
            m_age40_quarterly tag_mixing_k_cutoff sb_annual_kt_rr0 sb_annual_kt_rr1
            sbf0_annual_kt_rr0 sbf0_annual_kt_rr1 depletion_rr0 depletion_rr1 delta_depletion_rr1_minus_rr0
        """.split()),
    },
    "summary.csv": {
        "text": {"quantity"},
        "integer": {"n_pairs", "n_positive", "n_negative", "n_zero"},
        "real": set("""
            median_rr0 median_rr1 median_delta_rr1_minus_rr0 mean_delta_rr1_minus_rr0
            minimum_delta_rr1_minus_rr0 maximum_delta_rr1_minus_rr0
        """.split()),
    },
}


def read_csv(path):
    with (ROOT / path).open(newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle))


def require(condition, message):
    if not condition:
        raise ValueError(message)


def read_generated_csv(path, schema):
    with path.open(newline="", encoding="utf-8") as handle:
        records = list(csv.reader(handle, strict=True))
    require(bool(records), f"Empty generated table: {path}")
    header, rows = records[0], records[1:]
    fields = schema["text"] | schema["integer"] | schema["real"]
    require(len(header) == len(set(header)) and set(header) == fields,
            f"Unexpected generated CSV schema: {path}")
    require(all(len(row) == len(header) for row in rows), f"Malformed generated CSV row: {path}")
    return header, rows


def compare_generated_results(rebuilt_root):
    for name, schema in GENERATED_SCHEMAS.items():
        original_path = ROOT / "rr-test/results" / name
        rebuilt_path = rebuilt_root / "results" / name
        header, original = read_generated_csv(original_path, schema)
        rebuilt_header, rebuilt = read_generated_csv(rebuilt_path, schema)
        require(header == rebuilt_header, f"Generated CSV column order differs: {name}")
        require(len(original) == len(rebuilt), f"Generated CSV row count differs: {name}")
        largest_error = 0.0
        for row_number, (left, right) in enumerate(zip(original, rebuilt), start=2):
            for column, a, b in zip(header, left, right):
                location = f"{name}:{row_number}:{column}"
                if column in schema["text"]:
                    require(a == b, f"Generated identity/text or row order differs: {location}")
                elif column in schema["integer"]:
                    require(re.fullmatch(r"-?[0-9]+", a) and re.fullmatch(r"-?[0-9]+", b),
                            f"Invalid generated integer: {location}")
                    require(a == b, f"Generated integer or row order differs: {location}")
                else:
                    try:
                        x, y = float(a), float(b)
                    except ValueError as error:
                        raise ValueError(f"Invalid generated real number: {location}") from error
                    require(math.isfinite(x) and math.isfinite(y), f"Nonfinite generated real number: {location}")
                    require(math.isclose(x, y, abs_tol=GENERATED_ATOL, rel_tol=GENERATED_RTOL),
                            f"Generated value differs beyond atol={GENERATED_ATOL:g}, rtol={GENERATED_RTOL:g}: "
                            f"{location}: {a} versus {b}")
                    largest_error = max(largest_error, abs(x - y))
        print(f"Verified rebuilt {name}: exact schema/order/text/integers, {len(original)} rows; "
              f"real atol={GENERATED_ATOL:g}, rtol={GENERATED_RTOL:g}, max absolute error={largest_error:g}.")


def scalar(path, header):
    lines = (path.decode() if isinstance(path, bytes) else path.read_text()).splitlines()
    positions = [i for i, line in enumerate(lines) if line.strip() == header]
    require(len(positions) == 1, f"Missing or repeated {header}: {path}")
    value = float(lines[positions[0] + 1])
    require(math.isfinite(value), f"Nonfinite {header}: {path}")
    return value


def verify_ini(anchor, counterpart):
    left = (anchor.decode() if isinstance(anchor, bytes) else anchor.read_text()).splitlines()
    right = (counterpart.decode() if isinstance(counterpart, bytes) else counterpart.read_text()).splitlines()
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
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rebuilt-root", type=Path,
                        help="also compare results/ CSVs beneath this summarize.R output root; "
                             "exact schema/order/text/integers, finite reals at atol=1e-10 and rtol=1e-12")
    args = parser.parse_args()
    runs = read_csv("rr-test/results/run-manifest.csv")
    design = read_csv("rr-test/model-draws.csv")
    files = read_csv("rr-test/results/file-manifest.csv")
    require(len(runs) == len(design) == 34, "Expected all 34 planned pairs.")
    require(len({row["anchor_id"] for row in runs}) == 34, "Repeated anchors.")
    require({row["rr1_id"] for row in runs} == {row["ensemble_id"] for row in design}, "Run/design membership differs.")
    require({row["anchor_id"] for row in runs} == {row["anchor_ensemble_id"] for row in design}, "Anchor membership differs.")
    require(len({row["rr1_kflow_job"] for row in runs}) == 34, "Repeated source job numbers.")
    saved = Source(ROOT)
    recorded_paths = set()
    expected_records = {}
    for model in saved.models.values():
        for record in list(model["files"].values())+[model["final_par"],model["reference_rep"]]:
            key = (record["storage"],record.get("path","reproduce/native.tar.gz"),record.get("member",""))
            require(key not in expected_records,"Repeated indexed native file")
            expected_records[key] = record
    for row in files:
        key = (row["storage"],row["path"],row["member"])
        require(key not in recorded_paths,f"Repeated delivered file: {key}")
        recorded_paths.add(key)
        if row["role"] == "original_failure_evidence":
            require(row["storage"]=="repository" and row["path"].startswith("rr-test/failures/"),"Invalid failure evidence path")
        else:
            require(key in expected_records,f"Unindexed delivered file: {key}")
            expected = expected_records[key]
            require(row["sha256"]==expected["sha256"] and int(row["bytes"])==expected["bytes"],f"Delivered file binding changed: {key}")
        saved.read(row)
    require(set(expected_records) <= recorded_paths,"Missing delivered native files")
    require(len(files)==998 and len(expected_records)==990,"Expected exact 990 saved files plus 8 failure files")
    source = {row["ensemble_id"]: row for row in read_csv("rr-test/reference/retained-final-par-rr-split-manifest.csv")}
    provenance = {row["ensemble_id"]: row for row in read_csv("data/ensemble/retained-final-par-manifest.csv")}
    passed, failed = [], []
    for row in runs:
        anchor = row["anchor_id"]
        model = row["rr1_id"]
        require(model == anchor.replace("ensemble-", "rrtest-", 1) + "-rr1", f"Pair mapping changed: {model}")
        require(row["rr0_kflow_job"] == provenance[anchor]["kflow_job"], f"Anchor job changed: {anchor}")
        original = saved.model(anchor)
        for kind, entry in (("final_par",original["final_par"]),("bet_ini",original["files"]["bet.ini"]),("plot_rep",original["reference_rep"])):
            require(entry["sha256"]==source[anchor][kind+"_sha256"],f"Anchor source hash changed: {anchor}:{kind}")
        saved.input_hashes(anchor)
        mgc0 = scalar(saved.read(original["final_par"]), "# Maximum magnitude gradient value")
        require(mgc0 <= 1e-4 and math.isclose(mgc0, float(row["rr0_mgc"]), abs_tol=1e-15), f"RR0 MGC mismatch: {anchor}")
        folder = ROOT / "rr-test/fits" / model
        if row["rr1_status"] == "completed":
            require(row["included_in_pair_summary"] == "true", f"Completed pair missing: {model}")
            counterpart = saved.model(model)
            mgc1 = scalar(saved.read(counterpart["final_par"]), "# Maximum magnitude gradient value")
            require(mgc1 <= 1e-4 and math.isclose(mgc1, float(row["rr1_mgc"]), abs_tol=1e-15), f"RR1 MGC mismatch: {model}")
            saved.input_hashes(model)
            verify_ini(saved.read(original["files"]["bet.ini"]), saved.read(counterpart["files"]["bet.ini"]))
            require(not folder.exists(),f"Unexpected duplicate raw fit folder: {folder}")
            passed.append(model)
        else:
            require(row["rr1_status"] == "failed" and row["included_in_pair_summary"] == "false" and not folder.exists() and model not in saved.models, f"Unexpected failed-fit state: {model}")
            text = (ROOT / "rr-test/failures" / model / "status.txt").read_text()
            require("Exit code: 134" in text and "Job id: " + row["rr1_source_id"] in text, f"Failure identity differs: {model}")
            failed.append(model)
    require(len(passed) == 30 and set(failed) == {"rrtest-001-rr1", "rrtest-022-rr1", "rrtest-025-rr1", "rrtest-080-rr1"}, "Historical 30/4 membership differs.")
    references = json.loads((ROOT / "rr-test/reference/reference-manifest.json").read_text())
    for row in references:
        path = ROOT / "rr-test/reference" / row["original_name"]
        require(path.stat().st_size == row["bytes"] and hashlib.sha256(path.read_bytes()).hexdigest() == row["sha256"], f"Original analysis changed: {path}")
    print(f"Verified {len(runs)} source jobs, {len(files)} delivered files, {len(references)} original analysis files; 30 exact MGC-passing pairs and 4 failed fits.")
    if args.rebuilt_root is not None:
        compare_generated_results(args.rebuilt_root)


if __name__ == "__main__":
    main()
