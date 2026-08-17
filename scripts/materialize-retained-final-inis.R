#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop(
    "Usage: Rscript scripts/materialize-retained-final-inis.R OUTPUT_DIR MODEL_IDS.txt",
    call. = FALSE
  )
}

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_arg) != 1L) stop("Could not locate this script.", call. = FALSE)
script_file <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)
source(file.path(repo, "scripts", "ensemble-inputs.R"))

output <- args[[1L]]
id_file <- normalizePath(args[[2L]], mustWork = TRUE)
if (!dir.exists(output) || nzchar(Sys.readlink(output))) {
  stop("OUTPUT_DIR must be an existing real directory.", call. = FALSE)
}
if (length(list.files(output, all.files = TRUE, no.. = TRUE))) {
  stop("OUTPUT_DIR must be empty.", call. = FALSE)
}
model_ids <- readLines(id_file, warn = FALSE)
if (
  length(model_ids) != 80L || anyDuplicated(model_ids) ||
    any(!grepl("^ensemble-[0-9]{3}$", model_ids))
) {
  stop("MODEL_IDS.txt must contain exactly 80 unique ensemble-NNN IDs.", call. = FALSE)
}

design <- read.csv(
  file.path(repo, "design", "model-draws.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
design <- design[match(model_ids, design$ensemble_id), , drop = FALSE]
if (anyNA(design$ensemble_id)) stop("One or more model IDs are absent from the design.", call. = FALSE)

ensemble_verify_manifest(
  file.path(repo, "model"),
  file.path(repo, "model", "MANIFEST.sha256")
)
ensemble_source_hashes(repo)

base_ini <- file.path(repo, "model", "bet.ini")
base_lines <- readLines(base_ini, warn = FALSE)
h_row <- ensemble_rows_after(base_lines, "^# sv[(]29[)][[:space:]]*$", 1L)[[1L]]
m_row <- ensemble_rows_after(base_lines, "^# age_pars[[:space:]]*$", 5L)[[5L]]
tag_rows <- ensemble_rows_after(base_lines, "^# tag flags[[:space:]]*$", 98L)
allowed <- setNames(lapply(tag_rows, function(x) c(1L, 2L)), as.character(tag_rows))
allowed[[as.character(h_row)]] <- 1L
allowed[[as.character(m_row)]] <- 1L

for (index in seq_len(nrow(design))) {
  row <- design[index, , drop = FALSE]
  model_dir <- file.path(output, row$ensemble_id)
  if (!dir.create(model_dir, mode = "0755")) {
    stop("Could not create model INI directory: ", model_dir, call. = FALSE)
  }
  ini <- file.path(model_dir, "bet.ini")
  if (!file.copy(base_ini, ini, overwrite = FALSE)) {
    stop("Could not copy frozen base INI for ", row$ensemble_id, call. = FALSE)
  }

  h_value <- format(row$steepness, digits = 17, scientific = TRUE)
  m_value <- format(row$lorenzen_log_intercept, digits = 17, scientific = TRUE)
  mixing_source <- file.path(repo, "sources", "mixing", row$tag_mixing_source_file)
  ensemble_replace_field(ini, "^# sv[(]29[)][[:space:]]*$", 1L, 1L, h_value)
  ensemble_replace_field(ini, "^# age_pars[[:space:]]*$", 5L, 1L, m_value)
  ensemble_replace_tag_column(ini, 1L, source_path = mixing_source)
  ensemble_replace_tag_reporting(ini, row$tag_reporting_flag2)

  observed_h <- as.numeric(ensemble_value_after(ini, "^# sv[(]29[)][[:space:]]*$", 1L, 1L))
  observed_m <- as.numeric(ensemble_value_after(ini, "^# age_pars[[:space:]]*$", 5L, 1L))
  source_tags <- ensemble_tag_matrix(mixing_source)
  actual_tags <- ensemble_tag_matrix(ini)
  base_tags <- ensemble_tag_matrix(base_ini)
  expected_flag2 <- ensemble_expected_tag_reporting(
    as.integer(source_tags[, 1L]), row$tag_reporting_flag2
  )
  if (
    abs(observed_h - row$steepness) > 1e-12 ||
      abs(observed_m - row$lorenzen_log_intercept) > 1e-12 ||
      !identical(actual_tags[, 1L], source_tags[, 1L]) ||
      any(as.integer(actual_tags[, 2L]) != expected_flag2) ||
      !identical(actual_tags[, -(1:2), drop = FALSE], base_tags[, -(1:2), drop = FALSE])
  ) {
    stop("Materialized INI does not match design controls for ", row$ensemble_id, call. = FALSE)
  }
  ensemble_assert_lines(base_ini, ini, allowed)
}

message("Materialized and scientifically audited 80 model-specific bet.ini files.")
