.DEFAULT_GOAL := help
CASE ?= rrtest-005-rr1
OUT ?=
ANCHOR ?= 0
export CASE OUT ANCHOR
.PHONY: help verify rerun restore refit plan-refit _check-output

help:
	@printf '%s\n' 'make verify                            Check preserved files and RR results' 'make hessian CASE=ensemble-001 OUT=/absolute/bet-hessian' 'make rerun CASE=rrtest-005-rr1 OUT=/tmp/bet-rr005' 'make rerun CASE=ensemble-005 OUT=/tmp/bet-005' 'make restore CASE=ensemble-005 OUT=/tmp/bet-inputs' 'make refit CASE=rrtest-005-rr1 OUT=/tmp/bet-refit' 'make plan-refit CASE=rrtest-005-rr1 OUT=/tmp/bet-refit' 'Use ANCHOR=1 for the RR0 arm of refit/plan-refit.' 'Reruns require Linux x86-64; full refits use Docker and the pinned historical image.'

verify:
	python3 reproduce/hessian.py --verify
	python3 ci/verify-preserved-files.py
	python3 reproduce/restore.py --verify
	python3 rr-test/saved.py verify
	python3 rr-test/verify-results.py

rerun: _check-output
	@python3 -c 'import os,sys,platform,importlib.util; from pathlib import Path; assert platform.system()=="Linux" and platform.machine() in ("x86_64","amd64"), "MFCL requires Linux x86-64"; sys.path.insert(0,"rr-test"); from saved import Source; source=Source(); case=os.environ["CASE"]; output=source.new_output(os.environ["OUT"]); files=source.native_files(case); files["mfclo64"]=(source.tool.git_bytes(source.recipe["engine"]),0o755); source.tool.save_files(output,files,case); spec=importlib.util.spec_from_file_location("saved_evaluation","rr-test/evaluate-final.py"); runner=importlib.util.module_from_spec(spec); spec.loader.exec_module(runner); runner.evaluate(output/"mfclo64",output/"final.par",source.model(case)["final_par"]["sha256"],output,case,source)'

restore: _check-output
	python3 reproduce/restore.py "$$CASE" "$$OUT"

refit: _check-output
	@case "$$ANCHOR" in 0) ./rr-test/rerun "$$CASE" --output-dir "$$OUT";; 1) ./rr-test/rerun "$$CASE" --anchor --output-dir "$$OUT";; *) echo 'ANCHOR must be 0 or 1.' >&2; exit 2;; esac

plan-refit: _check-output
	@case "$$ANCHOR" in 0) ./rr-test/rerun "$$CASE" --output-dir "$$OUT" --dry-run;; 1) ./rr-test/rerun "$$CASE" --anchor --output-dir "$$OUT" --dry-run;; *) echo 'ANCHOR must be 0 or 1.' >&2; exit 2;; esac

_check-output:
	@python3 -c 'import os; from pathlib import Path; raw=os.environ.get("OUT", ""); p=Path(raw); root=Path.cwd().resolve(); assert raw and p.is_absolute(), "Set OUT to an absolute, new directory"; assert not os.path.lexists(p), "OUT already exists; choose a new directory"; q=p.resolve(); assert q != root and (root not in q.parents or root/"outputs" in q.parents), "OUT inside the checkout must be beneath outputs/"'

# Install the helper, manifest and offline tests in reproduce/.
export CASE OUT ARCHIVE
.PHONY: hessian hessian-verify

hessian:
	@if [ -n "$$ARCHIVE" ]; then \
		python3 reproduce/hessian.py --case "$$CASE" --out "$$OUT" --archive "$$ARCHIVE"; \
	else \
		python3 reproduce/hessian.py --case "$$CASE" --out "$$OUT"; \
	fi

hessian-verify:
	@if [ -n "$$ARCHIVE" ]; then \
		python3 reproduce/hessian.py --verify --case "$$CASE" --archive "$$ARCHIVE"; \
	else \
		python3 reproduce/hessian.py --verify; \
	fi
