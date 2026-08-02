#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
repo <- normalizePath(file.path(dirname(sub("^--file=", "", script_arg)), ".."), mustWork = TRUE)
source(file.path(repo, "scripts", "ensemble-inputs.R"), local = TRUE)
design <- read.csv(file.path(repo, "design", "model-draws.csv"), stringsAsFactors = FALSE)
test_root <- tempfile("bet-ensemble-input-validation-")
dir.create(test_root)
on.exit(unlink(test_root, recursive = TRUE), add = TRUE)
audits <- vector("list", nrow(design))
for (i in seq_len(nrow(design))) {
  run_dir <- file.path(test_root, design$ensemble_id[[i]])
  audits[[i]] <- ensemble_prepare_inputs(repo, design$ensemble_id[[i]], run_dir)
  unlink(run_dir, recursive = TRUE)
}
audit <- do.call(rbind, audits)
write.csv(audit, file.path(repo, "design", "input-validation-summary.csv"), row.names = FALSE, quote = TRUE)
cat("Validated exact frozen inputs for all ", nrow(audit), " ensemble models.\n", sep = "")
