#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: Rscript scripts/verify-ensemble-inputs.R MODEL_ID RUN_DIR", call. = FALSE)
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
repo <- normalizePath(file.path(dirname(sub("^--file=", "", script_arg)), ".."), mustWork = TRUE)
source(file.path(repo, "scripts", "ensemble-inputs.R"), local = TRUE)
audit <- ensemble_verify_inputs(repo, args[[1L]], normalizePath(args[[2L]], mustWork = TRUE))
write.csv(audit, file.path(args[[2L]], "input-change-audit.csv"), row.names = FALSE, quote = TRUE)
cat("Verified exact ensemble inputs for ", args[[1L]], ".\n", sep = "")
