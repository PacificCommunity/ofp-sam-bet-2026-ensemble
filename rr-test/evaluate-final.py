#!/usr/bin/env python3
"""Evaluate an original final PAR in a freshly prepared model directory."""

import hashlib
import csv
import json
import math
from pathlib import Path
import re
import subprocess
import sys


CONTROLS = "1 1 1\n1 50 -4\n1 121 0\n1 186 0\n1 187 0\n1 188 0\n1 189 0\n1 190 1\n1 246 1\n"
MFCL_SHA256 = "f5bc1e232a86e51f920bce7271d8e0930d0b160e4d18dc46de44078f0fa24cd0"
REPO = Path(__file__).resolve().parent.parent
INPUTS = ("bet.frq", "bet.ini", "bet.tag", "bet.age_length", "bet.reg_scaling", "mfcl.cfg")
REP_FIELDS = {"Number of time periods", "Year 1", "Number of regions", "Number of species",
              "Number of age classes", "Number of recruitments per year", "Adult biomass",
              "Adult biomass in absence of fishing", "Adult biomass at MSY", "F multiplier at MSY"}


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def scalar(path, marker):
    lines = path.read_text().splitlines()
    matches = [i for i, line in enumerate(lines) if line.strip() == marker]
    if len(matches) != 1:
        raise ValueError(f"missing or duplicate {marker}: {path}")
    value = float(lines[matches[0] + 1].strip())
    if not math.isfinite(value):
        raise ValueError(f"non-finite {marker}: {path}")
    return value


def rep_sections(path):
    result, label = {}, None
    for line in path.read_text().splitlines():
        if line.lstrip().startswith("#"):
            label = line.lstrip()[1:].strip()
            if label in REP_FIELDS and label in result:
                raise ValueError(f"duplicate REP section: {label}")
            result[label] = []
        elif line.strip() and label is not None:
            result[label].append(line.split())
    return result


def compare_rep(actual, original):
    a, b = rep_sections(actual), rep_sections(original)
    dimensions = {"Number of time periods": 292, "Year 1": 1952,
                  "Number of regions": 5, "Number of species": 1,
                  "Number of age classes": 40, "Number of recruitments per year": 4}
    sections = list(dimensions) + ["Adult biomass", "Adult biomass in absence of fishing",
                                 "Adult biomass at MSY", "F multiplier at MSY"]
    max_diff = 0.0
    for name in sections:
        shape = (292, 5) if name in ("Adult biomass", "Adult biomass in absence of fishing") else (1, 1)
        values = []
        for source in (a, b):
            rows = source.get(name, [])
            if len(rows) != shape[0] or any(len(row) != shape[1] for row in rows):
                raise ValueError(f"invalid REP dimensions: {name}; file={actual if source is a else original}; expected={shape}; rows={len(rows)}; widths={sorted(set(map(len, rows)))}")
            numbers = [float(token) for row in rows for token in row]
            if any(not math.isfinite(x) or x <= 0 for x in numbers):
                raise ValueError(f"invalid REP values: {name}")
            if name in dimensions and numbers != [dimensions[name]]:
                raise ValueError(f"unexpected model dimensions: {name}")
            values.append(numbers)
        for x, y in zip(*values):
            max_diff = max(max_diff, abs(x - y))
            if abs(x - y) > 1e-10 * max(1, abs(y)):
                raise ValueError(f"central REP differs from the original: {name}")
    return max_diff


def verify_inputs(source, run):
    model = source.parent.name
    manifest = (source.parent / "INPUTS.sha256" if model.startswith("rrtest-")
                else REPO / "rr-test" / "reference" / "input-hashes" / (model + ".sha256"))
    expected = dict((name, sha) for sha, name in
                    (line.split(None, 1) for line in manifest.read_text().splitlines()))
    hashes = {name: digest(run / name) for name in INPUTS}
    if any(hashes[name] != expected.get(name) for name in INPUTS):
        raise ValueError("prepared native inputs differ from the original input manifest")
    return hashes


def evaluate(program, source, expected_sha, run):
    if digest(program) != MFCL_SHA256:
        raise ValueError("MFCL executable checksum mismatch")
    if digest(source) != expected_sha:
        raise ValueError("saved final PAR checksum mismatch")
    for name in ("input.par", "evaluated.par", "plot-evaluated.par.rep", "mfcl-evaluation.log", "evaluation-check.json"):
        if (run / name).exists() or (run / name).is_symlink():
            raise ValueError(f"existing evaluation output refused: {name}")
    input_hashes = verify_inputs(source, run)
    original_rep = source.parent / "plot-11.par.rep"
    with (REPO / "rr-test" / "results" / "file-manifest.csv").open(newline="") as stream:
        entries = [row for row in csv.DictReader(stream) if row["path"] == original_rep.relative_to(REPO).as_posix()]
    if len(entries) != 1 or digest(original_rep) != entries[0]["sha256"]:
        raise ValueError("original reference REP checksum mismatch")
    staged = run / "input.par"
    with staged.open("xb") as stream:
        stream.write(source.read_bytes())
    if digest(staged) != expected_sha:
        raise ValueError("staged final PAR checksum mismatch")
    expected_objective = scalar(staged, "# Objective function value")
    expected_parameters = scalar(staged, "# The number of parameters")
    command = [str(program), "bet.frq", "input.par", "evaluated.par", "-file", "-"]
    with (run / "mfcl-evaluation.log").open("x") as log:
        process = subprocess.run(command, input=CONTROLS, text=True, cwd=run,
                                 stdout=log, stderr=subprocess.STDOUT)
    if digest(staged) != expected_sha or digest(source) != expected_sha:
        raise ValueError("input final PAR changed during evaluation")
    evaluated = run / "evaluated.par"
    rep = run / "plot-evaluated.par.rep"
    if process.returncode not in (0, 3) or not evaluated.is_file() or not rep.is_file() or rep.stat().st_size == 0:
        raise ValueError(f"native evaluation failed (exit {process.returncode}); inspect mfcl-evaluation.log")
    observed_objective = scalar(evaluated, "# Objective function value")
    parameters = scalar(evaluated, "# The number of parameters")
    log = (run / "mfcl-evaluation.log").read_text()
    objectives = re.findall(r"^\s*Total func\s+([^\s]+)\s*$", log, re.MULTILINE)
    logged_objective = float(objectives[0]) if objectives else math.nan
    if parameters != expected_parameters or expected_parameters != 1997:
        raise ValueError("evaluated parameter count differs from the saved model")
    if not math.isfinite(logged_objective) or max(abs(observed_objective - expected_objective), abs(logged_objective - expected_objective)) > 1e-6:
        raise ValueError(f"native objective differs from the saved model by more than 1e-6: saved={expected_objective}, PAR={observed_objective}, first_log={logged_objective}, all_logged={objectives}")
    rep_difference = compare_rep(rep, original_rep)
    if verify_inputs(source, run) != input_hashes:
        raise ValueError("native inputs changed during evaluation")
    receipt = {
        "mode": "outputs-only", "function_evaluation_limit": 1,
        "mfcl_sha256": MFCL_SHA256,
        "controls": CONTROLS.splitlines(), "native_exit_code": process.returncode,
        "input_par_sha256": expected_sha, "input_unchanged": True,
        "native_input_sha256": input_hashes,
        "reference_rep_sha256": entries[0]["sha256"],
        "central_rep_max_abs_diff": rep_difference,
        "saved_objective": expected_objective, "evaluated_objective": observed_objective,
        "logged_objective": logged_objective, "parameters": parameters,
        "files": {p.name: {"sha256": digest(p), "bytes": p.stat().st_size}
                  for p in (evaluated, rep)},
    }
    with (run / "evaluation-check.json").open("x") as stream:
        json.dump(receipt, stream, indent=2)
        stream.write("\n")
    print(f"Central outputs verified; objective difference {abs(observed_objective - expected_objective):.3g}.")


if __name__ == "__main__":
    try:
        if len(sys.argv) != 5:
            raise ValueError("Usage: evaluate-final.py MFCL FINAL_PAR SHA256 NEW_MODEL_DIR")
        evaluate(Path(sys.argv[1]), Path(sys.argv[2]), sys.argv[3], Path(sys.argv[4]))
    except (ValueError, OSError, IndexError) as error:
        sys.exit(str(error))
