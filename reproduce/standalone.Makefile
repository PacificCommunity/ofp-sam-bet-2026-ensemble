.DEFAULT_GOAL := help
CASE ?= ensemble-001
OUT ?=
RSCRIPT ?= Rscript
RUN_FINAL ?= run-final.R
export CASE OUT
.PHONY: help list unpack verify prepare rerun refit

help:
	@printf '%s\n' 'make list                         List 80 original retained ensemble cases' 'make unpack                       Unpack exact native model files' 'make verify                       Check bytes, modes and scientific bindings' 'make prepare CASE=ensemble-001 OUT=/absolute/new-folder' 'make rerun CASE=ensemble-001 OUT=/absolute/new-folder' 'make refit CASE=ensemble-001 OUT=/absolute/new-folder' 'prepare creates a working copy; rerun evaluates the saved final PAR.' 'rerun/refit require Linux x86-64; refit starts the original full fit.'

list unpack verify:
	@"$(RSCRIPT)" "$(RUN_FINAL)" "$@"

prepare rerun refit:
	@"$(RSCRIPT)" "$(RUN_FINAL)" "$@" "$$CASE" "$$OUT"
