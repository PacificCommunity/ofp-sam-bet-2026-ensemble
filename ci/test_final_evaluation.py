import csv
import hashlib
import importlib.util
import json
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch


spec = importlib.util.spec_from_file_location("final_evaluation", Path(__file__).resolve().parents[1] / "rr-test/evaluate-final.py")
evaluation = importlib.util.module_from_spec(spec)
spec.loader.exec_module(evaluation)


def rep_text():
    sections = [("Number of time periods", "292"), ("Year 1", "1952"),
                ("Number of regions", "5"), ("Number of species", "1"),
                ("Number of age classes", "40"), ("Number of recruitments per year", "4"),
                ("Adult biomass", "\n".join(["1 2 3 4 5"] * 292)),
                ("Adult biomass in absence of fishing", "\n".join(["2 4 6 8 10"] * 292)),
                ("Adult biomass at MSY", "2"), ("F multiplier at MSY", "1.5")]
    return "\n".join("# " + name + "\n" + value for name, value in sections) + "\n"


class EvaluationGuards(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.source = self.root / "rr-test/fits/rrtest-005-rr1/final.par"
        self.source.parent.mkdir(parents=True)
        self.source.write_text("# Objective function value\n100\n# The number of parameters\n1997\n")
        self.rep = self.source.parent / "plot-11.par.rep"
        self.rep.write_text(rep_text())
        manifest = self.root / "rr-test/results/file-manifest.csv"
        manifest.parent.mkdir()
        with manifest.open("w", newline="") as stream:
            writer = csv.DictWriter(stream, fieldnames=["path", "sha256"])
            writer.writeheader()
            writer.writerow({"path": self.rep.relative_to(self.root).as_posix(), "sha256": evaluation.digest(self.rep)})
        self.run = self.root / "new-run"
        self.run.mkdir()
        rows = []
        for name in evaluation.INPUTS:
            file = self.run / name
            file.write_text(name + " original input\n")
            rows.append(evaluation.digest(file) + " " + name)
        (self.source.parent / "INPUTS.sha256").write_text("\n".join(rows) + "\n")
        self.program = self.root / "fake-program-not-executed"
        self.program.write_text("mock only\n")
        self.sha = evaluation.digest(self.source)
        self.addCleanup(patch.stopall)
        patch.object(evaluation, "REPO", self.root).start()
        patch.object(evaluation, "MFCL_SHA256", evaluation.digest(self.program)).start()

    def native_mock(self, command, input, text, cwd, stdout, stderr):
        self.assertEqual(command[1:], ["bet.frq", "input.par", "evaluated.par", "-file", "-"])
        self.assertEqual(input, "1 1 1\n1 50 -4\n1 121 0\n1 186 0\n1 187 0\n1 188 0\n1 189 0\n1 190 1\n1 246 1\n")
        (cwd / "evaluated.par").write_bytes(self.source.read_bytes())
        (cwd / "plot-evaluated.par.rep").write_bytes(self.rep.read_bytes())
        stdout.write("Total func      100\n")
        return subprocess.CompletedProcess(command, 3)

    def invoke(self, side_effect=None):
        with patch.object(evaluation.subprocess, "run", side_effect=side_effect or self.native_mock) as native:
            evaluation.evaluate(self.program, self.source, self.sha, self.run)
            return native

    def test_successful_mock_requires_original_inputs_and_rep(self):
        self.invoke()
        receipt = json.loads((self.run / "evaluation-check.json").read_text())
        self.assertEqual(receipt["central_rep_max_abs_diff"], 0)
        self.assertEqual(receipt["native_exit_code"], 3)
        self.assertEqual(evaluation.digest(self.source), self.sha)

    def test_stale_outputs_refused_before_native_call(self):
        for name in ("input.par", "evaluated.par", "plot-evaluated.par.rep", "mfcl-evaluation.log", "evaluation-check.json"):
            with self.subTest(name=name):
                target = self.run / name
                target.write_text("stale")
                with patch.object(evaluation.subprocess, "run") as native:
                    with self.assertRaisesRegex(ValueError, "existing evaluation output"):
                        evaluation.evaluate(self.program, self.source, self.sha, self.run)
                    native.assert_not_called()
                target.unlink()

    def test_changed_native_input_refused_before_native_call(self):
        (self.run / "bet.frq").write_text("changed input")
        with patch.object(evaluation.subprocess, "run") as native:
            with self.assertRaisesRegex(ValueError, "prepared native inputs"):
                evaluation.evaluate(self.program, self.source, self.sha, self.run)
            native.assert_not_called()

    def test_wrong_engine_refused(self):
        self.program.write_text("different engine")
        with self.assertRaisesRegex(ValueError, "executable checksum"):
            self.invoke()

    def test_source_par_corruption_refused(self):
        self.source.write_text("changed PAR")
        with self.assertRaisesRegex(ValueError, "final PAR checksum"):
            self.invoke()

    def test_nonempty_but_malformed_rep_refused(self):
        def bad(*args, **kwargs):
            result = self.native_mock(*args, **kwargs)
            (self.run / "plot-evaluated.par.rep").write_text("nonempty garbage")
            return result
        with self.assertRaisesRegex(ValueError, "REP dimensions"):
            self.invoke(bad)

    def test_changed_biomass_refused(self):
        def bad(*args, **kwargs):
            result = self.native_mock(*args, **kwargs)
            p = self.run / "plot-evaluated.par.rep"
            p.write_text(p.read_text().replace("1 2 3 4 5", "1 2 3 4 6", 1))
            return result
        with self.assertRaisesRegex(ValueError, "central REP differs"):
            self.invoke(bad)

    def test_input_changed_during_native_call_refused(self):
        def bad(*args, **kwargs):
            result = self.native_mock(*args, **kwargs)
            (self.run / "input.par").write_text("mutated PAR")
            return result
        with self.assertRaisesRegex(ValueError, "input final PAR changed"):
            self.invoke(bad)

    def test_matching_files_with_failed_native_exit_refused(self):
        def bad(*args, **kwargs):
            result = self.native_mock(*args, **kwargs)
            result.returncode = 134
            return result
        with self.assertRaisesRegex(ValueError, "native evaluation failed"):
            self.invoke(bad)


if __name__ == "__main__":
    unittest.main()
