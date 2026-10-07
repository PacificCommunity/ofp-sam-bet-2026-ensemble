.DEFAULT_GOAL := help
CASE ?= rrtest-005-rr1
OUT ?=
ANCHOR ?= 0
export CASE OUT ANCHOR ARCHIVE
.PHONY: help verify list rerun restore prepare refit plan-refit _check-output hessian hessian-verify

help:
	@printf '%s\n' 'make verify' 'make list' 'make rerun CASE=rrtest-005-rr1 OUT=/tmp/bet-rr005' 'make rerun CASE=ensemble-005 OUT=/tmp/bet-005' 'make prepare CASE=ensemble-005 OUT=/tmp/bet-inputs' 'make refit CASE=rrtest-005-rr1 OUT=/tmp/bet-refit' 'make plan-refit CASE=rrtest-005-rr1 OUT=/tmp/bet-refit' 'make hessian CASE=ensemble-001 OUT=/tmp/bet-hessian' 'Use ANCHOR=1 to select the RR0 arm.' 'R and Make are required; native MFCL execution requires Linux x86-64.'

verify:
	@sha256sum --quiet -c ci/PRESERVED.sha256
	@Rscript reproduce/hessian.R --verify
	@Rscript rr-test/run-final.R verify
	@Rscript reproduce/run-final.R verify

list:
	@Rscript reproduce/run-final.R list
	@Rscript rr-test/run-final.R list

rerun restore prepare refit: _check-output
	@selected="$$CASE"; \
	case "$$ANCHOR" in 0) ;; 1) case "$$selected" in rrtest-*-rr1) selected=$$(printf '%s\n' "$$selected" | sed 's/^rrtest-/ensemble-/;s/-rr1$$//');; ensemble-*) ;; *) echo 'No RR0 anchor for this case.' >&2; exit 2;; esac;; *) echo 'ANCHOR must be 0 or 1.' >&2; exit 2;; esac; \
	case "$$selected" in rrtest-*-rr1) reader=rr-test/run-final.R;; ensemble-*) reader=reproduce/run-final.R;; *) echo 'Choose a retained ensemble-* or completed rrtest-*-rr1 case.' >&2; exit 2;; esac; \
	action='$@'; case "$$action" in restore|prepare) action=prepare;; esac; \
	Rscript "$$reader" "$$action" "$$selected" "$$OUT"

plan-refit: _check-output
	@selected="$$CASE"; \
	case "$$ANCHOR" in 0) ;; 1) case "$$selected" in rrtest-*-rr1) selected=$$(printf '%s\n' "$$selected" | sed 's/^rrtest-/ensemble-/;s/-rr1$$//');; ensemble-*) ;; *) exit 2;; esac;; *) echo 'ANCHOR must be 0 or 1.' >&2; exit 2;; esac; \
	case "$$selected" in rrtest-*-rr1) reader=rr-test/run-final.R;; ensemble-*) reader=reproduce/run-final.R;; *) echo 'Unknown saved case.' >&2; exit 2;; esac; \
	printf 'Rscript %s refit %s "%s"\n' "$$reader" "$$selected" "$$OUT"

_check-output:
	@case "$$OUT" in /*) ;; *) echo 'Set OUT to an absolute, new directory.' >&2; exit 2;; esac; \
	[ ! -e "$$OUT" ] && [ ! -L "$$OUT" ] || { echo 'OUT already exists.' >&2; exit 2; }; \
	parent=$$(cd "$$(dirname "$$OUT")" && pwd -P) || exit 2; \
	root=$$(pwd -P); destination="$$parent/$$(basename "$$OUT")"; \
	case "$$destination" in "$$root/outputs/"*) ;; "$$root"|"$$root/"*) echo 'OUT inside the checkout must be beneath outputs/.' >&2; exit 2;; esac

hessian:
	@if [ -n "$$ARCHIVE" ]; then Rscript reproduce/hessian.R --case "$$CASE" --out "$$OUT" --archive "$$ARCHIVE"; else Rscript reproduce/hessian.R --case "$$CASE" --out "$$OUT"; fi

hessian-verify:
	@if [ -n "$$ARCHIVE" ]; then Rscript reproduce/hessian.R --verify --case "$$CASE" --archive "$$ARCHIVE"; else Rscript reproduce/hessian.R --verify; fi
