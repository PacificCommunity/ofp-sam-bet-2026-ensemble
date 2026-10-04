"""Run the saved RR1/RR0 central-output checks in fresh CI scratch folders."""
import concurrent.futures
import csv
import os
from pathlib import Path
import subprocess
import sys


ROOT = Path(__file__).resolve().parents[1]


def main():
    if len(sys.argv) != 3:
        sys.exit("Usage: regenerate-rr.py MFCL NEW_OUTPUT_ROOT")
    program, output = Path(sys.argv[1]).resolve(), Path(sys.argv[2]).resolve()
    output.mkdir(exist_ok=False)
    with (ROOT / "rr-test/results/run-manifest.csv").open(newline="") as stream:
        pairs = [row for row in csv.DictReader(stream) if row["included_in_pair_summary"] == "true"]
    with (ROOT / "rr-test/results/file-manifest.csv").open(newline="") as stream:
        pars = {row["model_id"]: row for row in csv.DictReader(stream) if row["path"].endswith("/final.par")}
    jobs = [(row[key], design) for row in pairs for key, design in
            (("rr1_id", "rr-test/model-draws.csv"), ("anchor_id", "design/model-draws.csv"))]
    if len(jobs) != 60 or len(set(model for model, _ in jobs)) != 60:
        sys.exit("Expected 30 completed pairs, each with two distinct saved final PARs")

    def check(job):
        model, design = job
        run = output / model
        environment = dict(os.environ, ENSEMBLE_DESIGN_FILE=design)
        subprocess.run(["Rscript", str(ROOT / "rr-test/prepare.R"), model, str(run)],
                       cwd=ROOT, env=environment, check=True)
        entry = pars[model]
        subprocess.run([sys.executable, str(ROOT / "rr-test/evaluate-final.py"), str(program),
                        str(ROOT / entry["path"]), entry["sha256"], str(run)], check=True)
        return model

    with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
        for model in pool.map(check, jobs):
            print("Verified saved central output:", model, flush=True)


if __name__ == "__main__":
    main()
