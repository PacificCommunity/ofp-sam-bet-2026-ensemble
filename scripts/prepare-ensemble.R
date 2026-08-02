#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: Rscript scripts/prepare-ensemble.R MODEL_ID OUTPUT_DIR", call. = FALSE)
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
repo <- normalizePath(file.path(dirname(sub("^--file=", "", script_arg)), ".."), mustWork = TRUE)
source(file.path(repo, "scripts", "ensemble-inputs.R"), local = TRUE)
ensemble_prepare_inputs(repo, args[[1L]], normalizePath(args[[2L]], mustWork = FALSE))
