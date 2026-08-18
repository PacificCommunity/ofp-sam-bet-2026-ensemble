#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_arg) != 1L) stop("Cannot locate repository root.", call. = FALSE)
repo <- normalizePath(file.path(dirname(sub("^--file=", "", script_arg)), ".."), mustWork = TRUE)

source(file.path(repo, "scripts", "ensemble-inputs.R"))

canonical_path <- file.path(repo, "design", "model-draws.csv")
paired_path <- file.path(repo, "rr-test", "model-draws.csv")
retained_path <- file.path(repo, "data", "ensemble", "retained-final-par-manifest.csv")

canonical <- read.csv(canonical_path, stringsAsFactors = FALSE, check.names = FALSE)
paired <- read.csv(paired_path, stringsAsFactors = FALSE, check.names = FALSE)
retained <- read.csv(retained_path, stringsAsFactors = FALSE, check.names = FALSE)

fail <- function(...) stop(..., call. = FALSE)
assert <- function(value, ...) if (!isTRUE(value)) fail(...)

assert(nrow(canonical) == 100L, "Canonical design must contain exactly 100 rows.")
assert(nrow(retained) == 80L, "Retained manifest must contain exactly 80 rows.")
assert(nrow(paired) == 34L, "Paired design must contain exactly 34 RR1 rows.")
assert(length(unique(paired$ensemble_id)) == 34L, "Paired model IDs are not unique.")
assert(length(unique(paired$anchor_ensemble_id)) == 34L, "Anchor model IDs are not unique.")
assert(all(paired$anchor_ensemble_id %in% retained$ensemble_id),
       "Every paired row must point to a retained model.")

retained_rr0 <- canonical[
  canonical$ensemble_id %in% retained$ensemble_id & canonical$tag_reporting_flag2 == 0L,
  , drop = FALSE
]
retained_rr0 <- retained_rr0[order(retained_rr0$ensemble_id), , drop = FALSE]
paired <- paired[order(paired$anchor_ensemble_id), , drop = FALSE]
anchors <- canonical[match(paired$anchor_ensemble_id, canonical$ensemble_id), , drop = FALSE]

assert(nrow(retained_rr0) == 34L, "Expected exactly 34 retained RR0 anchors.")
assert(identical(paired$anchor_ensemble_id, retained_rr0$ensemble_id),
       "Paired design does not cover the complete retained RR0 set exactly once.")
assert(identical(anchors$ensemble_id, paired$anchor_ensemble_id),
       "Anchor lookup did not preserve paired-design order.")
assert(all(anchors$tag_reporting_flag2 == 0L & anchors$tag_reporting == "inclusion"),
       "Every anchor must be an RR0 inclusion row.")
assert(all(paired$tag_reporting_flag2 == 1L & paired$tag_reporting == "exclusion"),
       "Every rerun must be an RR1 exclusion row.")
assert(all(paired$tag_reporting_zero_mixing_exclusions == 0L),
       "RR1 rows must not count any zero-mixing sentinel rows as RR0 exclusions.")
assert(all(paired$pairing_version == "retained-rr0-paired-rerun-v1"),
       "Unexpected paired-design version.")

expected_test_id <- paste0(sub("^ensemble-", "rrtest-", paired$anchor_ensemble_id), "-rr1")
assert(identical(paired$ensemble_id, expected_test_id), "Paired model ID mapping changed.")

pair_invariant_columns <- c(
  "steepness",
  "steepness_prior_quantile",
  "tag_mixing_k_cutoff",
  "tag_mixing_source_file",
  "tag_tau",
  "tau_fish_pars4",
  "m_age40_quarterly",
  "lorenzen_log_intercept",
  "m_prior_quantile",
  "effort_creep_primary",
  "effort_creep_secondary",
  "effort_source_file",
  "initialization",
  "zero_mixing_events"
)
missing_invariants <- setdiff(pair_invariant_columns, intersect(names(anchors), names(paired)))
assert(!length(missing_invariants),
       "Missing pair invariant columns: ", paste(missing_invariants, collapse = ", "))
for (column in pair_invariant_columns) {
  assert(identical(anchors[[column]], paired[[column]]),
         "Paired design differs from its anchor in ", column, ".")
}

read_manifest <- function(path) {
  read.table(path, col.names = c("sha256", "file"), stringsAsFactors = FALSE)
}

with_design <- function(path, expression) {
  old <- Sys.getenv("ENSEMBLE_DESIGN_FILE", unset = NA_character_)
  on.exit({
    if (is.na(old)) Sys.unsetenv("ENSEMBLE_DESIGN_FILE") else Sys.setenv(ENSEMBLE_DESIGN_FILE = old)
  }, add = TRUE)
  Sys.setenv(ENSEMBLE_DESIGN_FILE = path)
  force(expression)
}

materialize <- function(design, model_id, output) {
  with_design(design, ensemble_prepare_inputs(repo, model_id, output))
  invisible(output)
}

relative_files <- function(path) {
  files <- list.files(path, recursive = TRUE, all.files = TRUE, full.names = TRUE, no.. = TRUE)
  files <- files[!file.info(files)$isdir]
  sort(substring(files, nchar(path) + 2L))
}

assert_pair_files <- function(anchor_dir, test_dir, anchor_row, test_row) {
  anchor_files <- relative_files(anchor_dir)
  test_files <- relative_files(test_dir)
  assert(identical(anchor_files, test_files),
         "Prepared file inventories differ for ", anchor_row$ensemble_id, " and ", test_row$ensemble_id, ".")

  permitted_differences <- c(
    "bet.ini",
    "MANIFEST.sha256",
    "INPUTS.sha256",
    "ensemble-metadata.csv",
    "input-change-audit.csv"
  )
  invariant_files <- setdiff(anchor_files, permitted_differences)
  anchor_hashes <- unname(vapply(file.path(anchor_dir, invariant_files), ensemble_sha256, character(1)))
  test_hashes <- unname(vapply(file.path(test_dir, invariant_files), ensemble_sha256, character(1)))
  assert(identical(anchor_hashes, test_hashes),
         "A non-reporting input differs in pair ", anchor_row$ensemble_id, " -> ", test_row$ensemble_id,
         ": ", paste(invariant_files[anchor_hashes != test_hashes], collapse = ", "))

  anchor_modes <- file.info(file.path(anchor_dir, invariant_files))$mode
  test_modes <- file.info(file.path(test_dir, invariant_files))$mode
  assert(identical(anchor_modes, test_modes),
         "A non-reporting input mode differs in pair ", anchor_row$ensemble_id, " -> ", test_row$ensemble_id, ".")

  anchor_ini <- readLines(file.path(anchor_dir, "bet.ini"), warn = FALSE)
  test_ini <- readLines(file.path(test_dir, "bet.ini"), warn = FALSE)
  assert(length(anchor_ini) == length(test_ini), "bet.ini line count changed in paired materialization.")
  tag_rows <- ensemble_rows_after(anchor_ini, "^# tag flags[[:space:]]*$", 98L)
  non_tag_rows <- setdiff(seq_along(anchor_ini), tag_rows)
  assert(identical(anchor_ini[non_tag_rows], test_ini[non_tag_rows]),
         "A non-tag line in bet.ini differs within pair ", anchor_row$ensemble_id, ".")

  anchor_tags <- do.call(rbind, lapply(tag_rows, function(i) ensemble_fields(anchor_ini[[i]])))
  test_tags <- do.call(rbind, lapply(tag_rows, function(i) ensemble_fields(test_ini[[i]])))
  assert(ncol(anchor_tags) == 10L && ncol(test_tags) == 10L,
         "Tag blocks must have ten fields.")
  assert(identical(anchor_tags[, -2L, drop = FALSE], test_tags[, -2L, drop = FALSE]),
         "A tag field other than column 2 differs within pair ", anchor_row$ensemble_id, ".")
  mixing <- as.integer(anchor_tags[, 1L])
  anchor_flag2 <- as.integer(anchor_tags[, 2L])
  test_flag2 <- as.integer(test_tags[, 2L])
  zero <- mixing == 0L
  positive <- mixing > 0L
  assert(sum(zero) == anchor_row$zero_mixing_events,
         "Observed zero-mixing count differs from design for ", anchor_row$ensemble_id, ".")
  assert(all(anchor_flag2[zero] == 1L & test_flag2[zero] == 1L),
         "Zero-mixing rows must remain sentinel 1 in both arms for ", anchor_row$ensemble_id, ".")
  assert(all(anchor_flag2[positive] == 0L & test_flag2[positive] == 1L),
         "Positive-mixing rows must be RR0=0 and RR1=1 for ", anchor_row$ensemble_id, ".")
  changed_rows <- which(anchor_ini != test_ini)
  assert(identical(changed_rows, tag_rows[positive]),
         "bet.ini changes are not exactly the positive-mixing tag rows for ", anchor_row$ensemble_id, ".")

  anchor_model_manifest <- read_manifest(file.path(anchor_dir, "MANIFEST.sha256"))
  test_model_manifest <- read_manifest(file.path(test_dir, "MANIFEST.sha256"))
  assert(identical(anchor_model_manifest$file, test_model_manifest$file),
         "Model manifest inventories differ within pair.")
  model_manifest_changes <- anchor_model_manifest$file[
    anchor_model_manifest$sha256 != test_model_manifest$sha256
  ]
  assert(identical(model_manifest_changes, "bet.ini"),
         "Only bet.ini may differ in MANIFEST.sha256; observed: ",
         paste(model_manifest_changes, collapse = ", "))

  anchor_input_manifest <- read_manifest(file.path(anchor_dir, "INPUTS.sha256"))
  test_input_manifest <- read_manifest(file.path(test_dir, "INPUTS.sha256"))
  assert(identical(anchor_input_manifest$file, test_input_manifest$file),
         "Input manifest inventories differ within pair.")
  input_manifest_changes <- anchor_input_manifest$file[
    anchor_input_manifest$sha256 != test_input_manifest$sha256
  ]
  expected_manifest_changes <- c("MANIFEST.sha256", "bet.ini", "ensemble-metadata.csv")
  assert(identical(sort(input_manifest_changes), sort(expected_manifest_changes)),
         "Unexpected INPUTS.sha256 changes: ", paste(input_manifest_changes, collapse = ", "))

  anchor_metadata <- read.csv(file.path(anchor_dir, "ensemble-metadata.csv"), check.names = FALSE)
  test_metadata <- read.csv(file.path(test_dir, "ensemble-metadata.csv"), check.names = FALSE)
  assert(nrow(anchor_metadata) == 1L && nrow(test_metadata) == 1L,
         "Prepared metadata must contain one row per model.")
  assert(anchor_metadata$ensemble_id == anchor_row$ensemble_id,
         "Anchor metadata ID mismatch.")
  assert(test_metadata$ensemble_id == test_row$ensemble_id &&
           test_metadata$anchor_ensemble_id == anchor_row$ensemble_id,
         "Paired metadata identity mismatch.")

  anchor_audit <- read.csv(file.path(anchor_dir, "input-change-audit.csv"), check.names = FALSE)
  test_audit <- read.csv(file.path(test_dir, "input-change-audit.csv"), check.names = FALSE)
  assert(nrow(anchor_audit) == 1L && nrow(test_audit) == 1L &&
           anchor_audit$status == "passed" && test_audit$status == "passed",
         "Input verification audit did not pass for both pair members.")
}

scratch <- tempfile("rr-pair-validation-")
dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)

for (i in seq_len(nrow(paired))) {
  anchor_dir <- file.path(scratch, "anchor")
  test_dir <- file.path(scratch, "test")
  materialize("design/model-draws.csv", anchors$ensemble_id[[i]], anchor_dir)
  materialize("rr-test/model-draws.csv", paired$ensemble_id[[i]], test_dir)
  assert_pair_files(anchor_dir, test_dir, anchors[i, , drop = FALSE], paired[i, , drop = FALSE])
  unlink(anchor_dir, recursive = TRUE, force = TRUE)
  unlink(test_dir, recursive = TRUE, force = TRUE)
  cat(sprintf("[%02d/34] exact pair verified: %s -> %s\n",
              i, anchors$ensemble_id[[i]], paired$ensemble_id[[i]]))
}

cat("Validated 34 retained RR0 anchors and 34 exact RR1 counterparts.\n")
cat("All non-reporting prepared inputs are byte-identical within every pair.\n")
cat("Zero-mixing tag rows are sentinel 1 in both arms; positive-mixing rows are RR0=0 and RR1=1.\n")
