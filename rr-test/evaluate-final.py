#!/usr/bin/env python3
"""Evaluate an original final PAR in a freshly prepared model directory."""

import hashlib
import contextlib
import csv
import json
import math
from pathlib import Path
import re
import subprocess
import sys

from saved import Source


CONTROLS = "1 1 1\n1 50 -4\n1 121 0\n1 186 0\n1 187 0\n1 188 0\n1 189 0\n1 190 1\n1 246 1\n"
MFCL_SHA256 = "f5bc1e232a86e51f920bce7271d8e0930d0b160e4d18dc46de44078f0fa24cd0"
REPO = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO / "reproduce"))
from native_directory import NativeDirectory, read_regular_bytes
INPUTS = ("bet.frq", "bet.ini", "bet.tag", "bet.age_length", "bet.reg_scaling", "mfcl.cfg")
REP_FIELDS = {"Number of time periods", "Year 1", "Number of regions", "Number of species",
              "Number of age classes", "Number of recruitments per year", "Adult biomass",
              "Adult biomass in absence of fishing", "Adult biomass at MSY", "F multiplier at MSY"}


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def scalar(path, marker):
    lines = (path.decode() if isinstance(path, bytes) else path.read_text()).splitlines()
    matches = [i for i, line in enumerate(lines) if line.strip() == marker]
    if len(matches) != 1:
        raise ValueError(f"missing or duplicate {marker}: {path}")
    value = float(lines[matches[0] + 1].strip())
    if not math.isfinite(value):
        raise ValueError(f"non-finite {marker}: {path}")
    return value


def rep_sections(path):
    result, label = {}, None
    for line in (path.decode() if isinstance(path, bytes) else path.read_text()).splitlines():
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


def verify_inputs(model, run, saved):
    expected = saved.input_hashes(model)
    hashes = {name: hashlib.sha256(run.read_bytes(name)).hexdigest() for name in INPUTS}
    if hashes != expected:
        raise ValueError("prepared native inputs differ from the original input manifest")
    return hashes


def evaluate(program, source, expected_sha, run, model=None, saved=None, directory=None):
    run = Path(run).absolute()
    with (contextlib.nullcontext(directory) if directory is not None else NativeDirectory(run)) as output:
        model = model or source.parent.name
        saved = saved or Source(REPO)
        reference = saved.model(model)
        if expected_sha != reference["final_par"]["sha256"]:
            raise ValueError("final PAR hash differs from the original model binding")
        if Path(source).absolute().parent == run:
            source_name = Path(source).name
            before = output.read_bytes(source_name)
        else:
            source_name = None
            before = read_regular_bytes(source)
        if hashlib.sha256(before).hexdigest() != expected_sha:
            raise ValueError("saved final PAR checksum mismatch")
        if Path(program).absolute().parent == run:
            program_bytes = output.read_bytes(Path(program).name)
        else:
            program_bytes = read_regular_bytes(program)
        if hashlib.sha256(program_bytes).hexdigest() != MFCL_SHA256:
            raise ValueError("MFCL executable checksum mismatch")
        if output.exists("mfclo64"):
            if output.read_bytes("mfclo64") != program_bytes:
                raise ValueError("prepared native executable differs")
        else:
            output.write_new("mfclo64", program_bytes, 0o755)
        for name in ("input.par", "evaluated.par", "plot-evaluated.par.rep", "mfcl-evaluation.log", "evaluation-check.json"):
            if output.exists(name):
                raise ValueError(f"existing evaluation output refused: {name}")
        input_hashes = verify_inputs(model, output, saved)
        original_rep = saved.read(reference["reference_rep"])
        output.write_new("input.par", before)
        staged = output.read_bytes("input.par")
        if hashlib.sha256(staged).hexdigest() != expected_sha:
            raise ValueError("staged final PAR checksum mismatch")
        expected_objective = scalar(staged, "# Objective function value")
        expected_parameters = scalar(staged, "# The number of parameters")
        with output.open_input("mfclo64") as (engine_fd, engine_bytes):
            if hashlib.sha256(engine_bytes).hexdigest() != MFCL_SHA256:
                raise ValueError("MFCL executable checksum mismatch")
            command = [output.child_file(engine_fd), "bet.frq", "input.par", "evaluated.par", "-file", "-"]
            with output.open_new("mfcl-evaluation.log", text=True) as log:
                process = subprocess.run(command, input=CONTROLS, text=True,
                                         stdout=log, stderr=subprocess.STDOUT,
                                         **output.child_kwargs(engine_fd))
        if source_name:
            source_now = output.read_bytes(source_name)
        else:
            source_now = read_regular_bytes(source)
        if (hashlib.sha256(output.read_bytes("input.par")).hexdigest() != expected_sha
                or hashlib.sha256(source_now).hexdigest() != expected_sha):
            raise ValueError("input final PAR changed during evaluation")
        if process.returncode not in (0, 3):
            raise ValueError(f"native evaluation failed (exit {process.returncode}); inspect mfcl-evaluation.log")
        evaluated = output.read_bytes("evaluated.par")
        rep = output.read_bytes("plot-evaluated.par.rep")
        if not evaluated or not rep:
            raise ValueError(f"native evaluation failed (exit {process.returncode}); inspect mfcl-evaluation.log")
        observed_objective = scalar(evaluated, "# Objective function value")
        parameters = scalar(evaluated, "# The number of parameters")
        log = output.read_bytes("mfcl-evaluation.log").decode()
        objectives = re.findall(r"^\s*Total func\s+([^\s]+)\s*$", log, re.MULTILINE)
        logged_objective = float(objectives[0]) if objectives else math.nan
        if parameters != expected_parameters or expected_parameters != 1997:
            raise ValueError("evaluated parameter count differs from the saved model")
        if not math.isfinite(logged_objective) or max(abs(observed_objective - expected_objective), abs(logged_objective - expected_objective)) > 1e-6:
            raise ValueError(f"native objective differs from the saved model by more than 1e-6: saved={expected_objective}, PAR={observed_objective}, first_log={logged_objective}, all_logged={objectives}")
        rep_difference = compare_rep(rep, original_rep)
        if verify_inputs(model, output, saved) != input_hashes:
            raise ValueError("native inputs changed during evaluation")
        receipt = {
            "mode": "outputs-only", "function_evaluation_limit": 1,
            "mfcl_sha256": MFCL_SHA256,
            "controls": CONTROLS.splitlines(), "native_exit_code": process.returncode,
            "input_par_sha256": expected_sha, "input_unchanged": True,
            "native_input_sha256": input_hashes,
            "reference_rep_sha256": reference["reference_rep"]["sha256"],
            "reference_rep_role": reference["reference_rep"].get("role", "original_whole_rep"),
            "source_whole_rep_sha256": reference["reference_rep"].get("source_whole_rep_sha256", reference["reference_rep"]["sha256"]),
            "central_rep_max_abs_diff": rep_difference,
            "saved_objective": expected_objective, "evaluated_objective": observed_objective,
            "logged_objective": logged_objective, "parameters": parameters,
            "files": {name: {"sha256": hashlib.sha256(data).hexdigest(), "bytes": len(data)}
                      for name, data in (("evaluated.par", evaluated), ("plot-evaluated.par.rep", rep))},
        }
        output.write_new("evaluation-check.json", (json.dumps(receipt, indent=2) + "\n").encode())
        print(f"Central outputs verified; objective difference {abs(observed_objective - expected_objective):.3g}.")


if __name__ == "__main__":
    try:
        if len(sys.argv) != 4:
            raise ValueError("Usage: evaluate-final.py MFCL MODEL NEW_MODEL_DIR")
        saved = Source()
        model, run = sys.argv[2], Path(sys.argv[3])
        evaluate(Path(sys.argv[1]), run/"final.par", saved.model(model)["final_par"]["sha256"], run, model, saved)
    except (ValueError, OSError, IndexError) as error:
        sys.exit(str(error))
