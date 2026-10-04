"""Check central outputs for all 80 original models and 30 completed RR1 fits."""
import concurrent.futures
import hashlib
import importlib.util
import json
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "rr-test"))
sys.path.insert(0, str(ROOT / "reproduce"))
from saved import Source
from native_directory import NativeDirectory


def main():
    if len(sys.argv) != 3:
        sys.exit("Usage: regenerate-rr.py MFCL NEW_OUTPUT_ROOT")
    program, output = Path(sys.argv[1]).resolve(), Path(sys.argv[2])
    saved = Source(ROOT)
    jobs = sorted(saved.models)
    if (len(jobs) != 110 or sum(name.startswith("ensemble-") for name in jobs) != 80
            or sum(name.startswith("rrtest-") for name in jobs) != 30):
        sys.exit("Expected all 80 original retained models and 30 completed RR1 fits")
    forbidden = {f"rrtest-{index:03d}-rr1" for index in (1, 22, 25, 80)}
    if forbidden.intersection(jobs):
        sys.exit("Failed RR1 fits cannot regenerate outputs")
    with saved.tool.frozen_bytes(program) as data:
        engine = saved.tool.checked(data, saved.recipe["engine"])
    output = saved.new_output(output)
    saved.tool.save_files(output, {}, "all-saved-models")
    spec = importlib.util.spec_from_file_location("saved_evaluation", ROOT / "rr-test/evaluate-final.py")
    evaluation = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(evaluation)
    with NativeDirectory(output) as collection:
        def check(model):
            files = saved.native_files(model)
            files["mfclo64"] = (engine, 0o755)
            with collection.create_child(model) as case:
                case.stage_files(files, model)
                evaluation.evaluate(case.path / "mfclo64", case.path / "final.par",
                                    saved.model(model)["final_par"]["sha256"], case.path, model, saved, case)
                case.check()
            return model
        with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
            for model in pool.map(check, jobs):
                print("Verified saved central output:", model, flush=True)
        collection.write_new("native-coverage.json", (json.dumps(
            {"cases": jobs, "case_count": len(jobs), "original_models": 80,
             "completed_rr1": 30, "failed_rr1_excluded": sorted(forbidden)}, indent=2) + "\n").encode())


if __name__ == "__main__":
    main()
