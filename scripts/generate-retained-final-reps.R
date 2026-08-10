#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L || length(args) > 5L) {
  stop(
    paste(
      "Usage: Rscript scripts/generate-retained-final-reps.R",
      "FINAL_PAR_DIR OUTPUT_ROOT OUTPUT_MANIFEST.csv [PARALLEL] [MODEL_ID|all]"
    ),
    call. = FALSE
  )
}

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_arg) != 1L) stop("Could not locate this script.", call. = FALSE)
script_file <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)
source(file.path(repo, "scripts", "ensemble-inputs.R"), local = TRUE)

par_root <- normalizePath(args[[1L]], mustWork = TRUE)
output_parent <- normalizePath(dirname(args[[2L]]), mustWork = TRUE)
output_root <- file.path(output_parent, basename(args[[2L]]))
manifest_parent <- normalizePath(dirname(args[[3L]]), mustWork = TRUE)
output_manifest <- file.path(manifest_parent, basename(args[[3L]]))
parallel_jobs <- if (length(args) >= 4L) suppressWarnings(as.integer(args[[4L]])) else 1L
requested_model <- if (length(args) >= 5L) args[[5L]] else "all"

if (is.na(parallel_jobs) || parallel_jobs < 1L || parallel_jobs > 8L) {
  stop("PARALLEL must be an integer from 1 through 8.", call. = FALSE)
}
if (!nzchar(basename(output_root)) || basename(output_root) %in% c(".", "..")) {
  stop("OUTPUT_ROOT must name a new directory.", call. = FALSE)
}
if (!nzchar(basename(output_manifest)) || basename(output_manifest) %in% c(".", "..")) {
  stop("OUTPUT_MANIFEST must name a new CSV file.", call. = FALSE)
}

link_exists <- function(path) {
  link <- Sys.readlink(path)
  !is.na(link) && nzchar(link)
}
path_exists <- function(path) file.exists(path) || link_exists(path)
is_within <- function(path, root) {
  identical(path, root) || startsWith(path, paste0(root, .Platform$file.sep))
}
if (path_exists(output_root) || path_exists(output_manifest)) {
  stop("Refusing to overwrite OUTPUT_ROOT or OUTPUT_MANIFEST.", call. = FALSE)
}
if (is_within(output_root, par_root) || is_within(par_root, output_root)) {
  stop("OUTPUT_ROOT must be separate from FINAL_PAR_DIR.", call. = FALSE)
}
if (is_within(output_manifest, output_root)) {
  stop("OUTPUT_MANIFEST must be a sidecar outside OUTPUT_ROOT.", call. = FALSE)
}

sha256_file <- function(path) {
  output <- system2("sha256sum", shQuote(path), stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) stop("sha256sum failed for ", path, call. = FALSE)
  sub("[[:space:]].*$", "", output[[1L]])
}

numeric_after <- function(lines, marker, source) {
  hit <- which(trimws(lines) == marker)
  if (length(hit) != 1L || hit[[1L]] >= length(lines)) {
    stop(source, " is missing a unique ", marker, " field.", call. = FALSE)
  }
  value <- suppressWarnings(as.numeric(trimws(lines[[hit[[1L]] + 1L]])))
  if (length(value) != 1L || !is.finite(value)) {
    stop(source, " has an invalid ", marker, " field.", call. = FALSE)
  }
  value
}

required_rep_sections <- c(
  "# Number of time periods",
  "# Year 1",
  "# Number of regions",
  "# Number of age classes",
  "# Fishery species pointer",
  "# Number of length bins",
  "# Number of recruitments per year",
  "# Number of fisheries",
  "# Time of each realization by fishery (down)",
  "# Mean lengths at age",
  "# SD of length at age",
  "# Mean weights at age",
  "# Natural mortality at age",
  "# Selectivity by age class (across) and fishery (down)",
  "# length-based selectivity by fishery",
  "# Implicit catchability by realization (across) by fishery (down)",
  "# Fishing mortality by age class (across) and year (down)",
  "# Fishing mortality by age class (across), year (down) and region (block)",
  "# Population Number by age (across), year (down) and region",
  "# Exploitable population biomass by fishery (down) and by year-season  (across)",
  "# Absolute biomass by region (across) and year (down)",
  "# Recruitment",
  "# Total biomass",
  "# Adult biomass",
  "# Relative biomass by region (across) and year (down)",
  "# Observed catch by fishery (down) and time (across)",
  "# Predicted catch by fishery (down) and time (across)",
  "# Observed CPUE by fishery (down) and time (across)",
  "# Predicted CPUE by fishery (down) and time (across)",
  "# Beverton-Holt stock-recruitment relationship report",
  "# Beverton-Holt yield analysis report",
  "# Yield per recruit report",
  "# Tag reporting rates",
  "# Observed tag returns by time period (across) by fishery groupings (down)",
  "# Predicted tag returns by time period (across) by fishery groupings (down)",
  "# Movement analysis",
  "# Total biomass in absence of fishing",
  "# Adult biomass in absence of fishing",
  "# Exploitable populations in absence of fishing",
  "# Predicted catch for interaction analysis by fishery (down) and time (across)"
)

validate_rep <- function(path, source) {
  lines <- readLines(path, warn = FALSE)
  expected_header <- c(
    "# MULTIFAN-CL Viewer",
    "# 4.0",
    "# Frq file = bet.frq",
    "# Input par file = 10.par",
    "# Output par file = 11.par",
    "# MULTIFAN-CL version number: 2.2.7.9"
  )
  if (length(lines) != 6349L || length(lines) < length(expected_header) ||
      !identical(lines[seq_along(expected_header)], expected_header)) {
    stop(source, " is not the complete 6,349-line MFCL 2.2.7.9 Viewer report.", call. = FALSE)
  }
  trimmed <- trimws(lines)
  positions <- vapply(required_rep_sections, function(marker) {
    hit <- which(trimmed == marker)
    if (length(hit) != 1L) return(NA_integer_)
    hit[[1L]]
  }, integer(1L))
  if (anyNA(positions) || is.unsorted(positions, strictly = TRUE)) {
    stop(source, " is missing, duplicating, or reordering a required Viewer section.", call. = FALSE)
  }
  expected_dimensions <- c(
    "# Number of time periods" = 292,
    "# Year 1" = 1952,
    "# Number of regions" = 5,
    "# Number of age classes" = 40,
    "# Number of length bins" = 95,
    "# Number of recruitments per year" = 4,
    "# Number of fisheries" = 33
  )
  observed <- vapply(names(expected_dimensions), function(marker) {
    numeric_after(lines, marker, source)
  }, numeric(1L))
  if (!identical(unname(observed), unname(expected_dimensions))) {
    stop(source, " has unexpected model dimensions.", call. = FALSE)
  }
  invisible(length(lines))
}

par_manifest_path <- file.path(repo, "data", "ensemble", "retained-final-par-manifest.csv")
par_manifest <- read.csv(par_manifest_path, check.names = FALSE)
par_manifest <- par_manifest[order(par_manifest$ensemble_id), , drop = FALSE]
if (nrow(par_manifest) != 80L || anyDuplicated(par_manifest$ensemble_id)) {
  stop("The retained final-PAR manifest is not the exact 80-model set.", call. = FALSE)
}
if (requested_model == "all") {
  selected <- par_manifest
} else {
  if (!grepl("^ensemble-[0-9]{3}$", requested_model) ||
      !requested_model %in% par_manifest$ensemble_id) {
    stop("MODEL_ID must be all or one retained ensemble-NNN identifier.", call. = FALSE)
  }
  selected <- par_manifest[par_manifest$ensemble_id == requested_model, , drop = FALSE]
}

expected_mfcl_sha <- "8995f72019869863c1d1c0b4f44fc6a6268d1f79031f5bc79dc354ee10f0a63e"
mfcl <- file.path(repo, "mfclo64")
if (!file.exists(mfcl) || link_exists(mfcl) || file.access(mfcl, mode = 1L) != 0L ||
    !identical(sha256_file(mfcl), expected_mfcl_sha)) {
  stop("The bundled native MFCL 2.2.7.9 executable is missing or changed.", call. = FALSE)
}

fast_check <- system2(
  file.path(repo, "scripts", "verify-retained-final-pars"),
  c(shQuote(par_root), "-", "1", "fast"),
  stdout = TRUE, stderr = TRUE
)
fast_status <- attr(fast_check, "status")
if (!is.null(fast_status) && fast_status != 0L) {
  stop("Source retained-PAR verification failed:\n", paste(fast_check, collapse = "\n"), call. = FALSE)
}

stage_root <- tempfile(pattern = paste0(".", basename(output_root), ".stage-"), tmpdir = output_parent)
scratch_root <- tempfile(pattern = "bet-ensemble-reps-")
manifest_temp <- tempfile(
  pattern = paste0(".", basename(output_manifest), ".stage-"),
  tmpdir = manifest_parent, fileext = ".csv"
)
if (!dir.create(stage_root, mode = "0700") || !dir.create(scratch_root, mode = "0700")) {
  stop("Could not create generation staging directories.", call. = FALSE)
}
stage_root <- normalizePath(stage_root, mustWork = TRUE)
scratch_root <- normalizePath(scratch_root, mustWork = TRUE)

safe_remove_dir <- function(path, parent, prefix) {
  if (!dir.exists(path)) return(invisible(NULL))
  resolved <- normalizePath(path, mustWork = TRUE)
  if (!identical(dirname(resolved), normalizePath(parent, mustWork = TRUE)) ||
      !startsWith(basename(resolved), prefix) || link_exists(resolved) ||
      !isTRUE(file.info(resolved)$isdir)) {
    stop("Refusing unexpected temporary-directory cleanup: ", resolved, call. = FALSE)
  }
  unlink(resolved, recursive = TRUE, force = FALSE)
  if (file.exists(resolved)) stop("Could not remove temporary directory: ", resolved, call. = FALSE)
}

published <- FALSE
cleanup <- function() {
  safe_remove_dir(scratch_root, tempdir(), "bet-ensemble-reps-")
  if (!published) safe_remove_dir(stage_root, output_parent, paste0(".", basename(output_root), ".stage-"))
  if (file.exists(manifest_temp) && !link_exists(manifest_temp)) unlink(manifest_temp, force = FALSE)
}
on.exit(cleanup(), add = TRUE)

controls <- c(
  "1 1 1", "1 50 -4", "1 121 0",
  "1 186 0", "1 187 0", "1 188 0", "1 189 0",
  "1 190 1", "1 246 1"
)
worker <- function(index) {
  entry <- selected[index, , drop = FALSE]
  model_id <- entry$ensemble_id
  source_par <- file.path(par_root, model_id, "final.par")
  source_sha_before <- sha256_file(source_par)
  if (!identical(source_sha_before, entry$final_par_sha256) ||
      !identical(as.numeric(file.info(source_par)$size), as.numeric(entry$final_par_bytes))) {
    stop(model_id, " source final.par differs from its manifest.", call. = FALSE)
  }

  run_dir <- file.path(scratch_root, model_id)
  prepare_output <- system2(
    "Rscript",
    c(shQuote(file.path(repo, "scripts", "prepare-ensemble.R")), shQuote(model_id), shQuote(run_dir)),
    stdout = TRUE, stderr = TRUE
  )
  prepare_status <- attr(prepare_output, "status")
  if (!is.null(prepare_status) && prepare_status != 0L) {
    stop(model_id, " input preparation failed:\n", paste(prepare_output, collapse = "\n"), call. = FALSE)
  }
  run_dir <- normalizePath(run_dir, mustWork = TRUE)
  staged_par <- file.path(run_dir, "10.par")
  if (!file.copy(source_par, staged_par, copy.mode = TRUE, copy.date = TRUE)) {
    stop("Could not copy ", model_id, " final.par to 10.par.", call. = FALSE)
  }
  if (!identical(sha256_file(staged_par), source_sha_before)) {
    stop(model_id, " staged 10.par is not byte-identical to final.par.", call. = FALSE)
  }

  old_wd <- setwd(run_dir)
  on.exit(setwd(old_wd), add = TRUE)
  stdout_path <- file.path(run_dir, "phase11.stdout.log")
  stderr_path <- file.path(run_dir, "phase11.stderr.log")
  native_status <- suppressWarnings(system2(
    "./mfclo64",
    c("bet.frq", "10.par", "11.par", "-file", "-"),
    stdout = stdout_path, stderr = stderr_path, input = controls
  ))
  native_status <- as.integer(native_status)
  output_par <- file.path(run_dir, "11.par")
  output_rep <- file.path(run_dir, "plot-11.par.rep")
  if (native_status != 0L || !file.exists(output_par) ||
      file.info(output_par)$size <= 0 || !file.exists(output_rep) ||
      file.info(output_rep)$size <= 0) {
    log_tail <- c(tail(readLines(stdout_path, warn = FALSE), 30L),
                  tail(readLines(stderr_path, warn = FALSE), 30L))
    stop(model_id, " Phase-11 report generation failed (status ", native_status,
         "):\n", paste(log_tail, collapse = "\n"), call. = FALSE)
  }
  if (!identical(sha256_file(source_par), source_sha_before) ||
      !identical(sha256_file(staged_par), source_sha_before)) {
    stop(model_id, " input PAR changed during native evaluation.", call. = FALSE)
  }

  stdout_lines <- readLines(stdout_path, warn = FALSE)
  total_lines <- grep("^[[:space:]]*Total func[[:space:]]+", stdout_lines, value = TRUE)
  log_objective <- suppressWarnings(as.numeric(sub(
    ".*Total func[[:space:]]+", "", total_lines[[1L]]
  )))
  output_lines <- readLines(output_par, warn = FALSE)
  output_objective <- numeric_after(output_lines, "# Objective function value", output_par)
  output_parameters <- numeric_after(output_lines, "# The number of parameters", output_par)
  expected_objective <- as.numeric(entry$objective_function)
  objective_difference <- abs(output_objective - expected_objective)
  if (length(total_lines) < 1L || length(log_objective) != 1L ||
      !is.finite(log_objective) || output_parameters != 1997 ||
      abs(log_objective - output_objective) > 1e-6 || objective_difference > 1e-6) {
    stop(model_id, " Phase-11 objective or active-parameter parity failed.", call. = FALSE)
  }
  rep_lines <- validate_rep(output_rep, model_id)

  destination <- file.path(stage_root, model_id)
  if (!dir.create(destination, mode = "0700")) {
    stop("Could not create staged output for ", model_id, ".", call. = FALSE)
  }
  copied <- c(
    file.copy(source_par, file.path(destination, "final.par"), copy.mode = TRUE, copy.date = TRUE),
    file.copy(output_rep, file.path(destination, "plot-11.par.rep"), copy.mode = TRUE, copy.date = TRUE)
  )
  if (!all(copied) || !identical(sha256_file(file.path(destination, "final.par")), source_sha_before)) {
    stop("Could not stage exact public files for ", model_id, ".", call. = FALSE)
  }
  public_rep <- file.path(destination, "plot-11.par.rep")
  validate_rep(public_rep, model_id)

  result <- data.frame(
    ensemble_id = model_id,
    source_archive = entry$source_archive,
    archive_sha256 = entry$archive_sha256,
    final_par_sha256 = source_sha_before,
    plot_rep_file = file.path(model_id, "plot-11.par.rep"),
    source_rep_member = paste0("./outputs/models/", model_id, "/plot-11.par.rep"),
    plot_rep_sha256 = sha256_file(public_rep),
    plot_rep_bytes = as.numeric(file.info(public_rep)$size),
    plot_rep_lines = rep_lines,
    source_commit = entry$source_commit,
    mfcl_version = "2.2.7.9",
    mfcl_sha256 = expected_mfcl_sha,
    stringsAsFactors = FALSE
  )
  setwd(old_wd)
  resolved_run <- normalizePath(run_dir, mustWork = TRUE)
  if (!identical(dirname(resolved_run), scratch_root) ||
      !identical(basename(resolved_run), model_id) || link_exists(resolved_run) ||
      !isTRUE(file.info(resolved_run)$isdir)) {
    stop("Refusing unexpected model scratch cleanup: ", resolved_run, call. = FALSE)
  }
  unlink(resolved_run, recursive = TRUE, force = FALSE)
  if (file.exists(resolved_run)) stop("Could not remove model scratch: ", resolved_run, call. = FALSE)
  message(sprintf(
    "Generated %s plot-11.par.rep (%d lines; objective |difference| %.3g).",
    model_id, rep_lines, objective_difference
  ))
  result
}

safe_worker <- function(index) {
  tryCatch(list(ok = TRUE, value = worker(index)),
           error = function(error) list(ok = FALSE, error = conditionMessage(error)))
}
if (parallel_jobs == 1L || nrow(selected) == 1L) {
  generated <- lapply(seq_len(nrow(selected)), safe_worker)
} else {
  generated <- parallel::mclapply(
    seq_len(nrow(selected)), safe_worker,
    mc.cores = parallel_jobs, mc.preschedule = FALSE
  )
}
failed <- which(!vapply(generated, `[[`, logical(1L), "ok"))
if (length(failed)) {
  errors <- vapply(generated[failed], `[[`, character(1L), "error")
  stop("Retained REP generation failed:\n", paste(errors, collapse = "\n"), call. = FALSE)
}

rep_manifest <- do.call(rbind, lapply(generated, `[[`, "value"))
row.names(rep_manifest) <- NULL
rep_manifest <- rep_manifest[order(rep_manifest$ensemble_id), , drop = FALSE]
if (!identical(rep_manifest$ensemble_id, selected$ensemble_id)) {
  stop("Generated manifest lost retained-manifest order.", call. = FALSE)
}
write.table(
  rep_manifest, manifest_temp, sep = ",", row.names = FALSE,
  col.names = TRUE, quote = TRUE, qmethod = "double", eol = "\n"
)

if (path_exists(output_root) || path_exists(output_manifest)) {
  stop("Output target appeared during generation; refusing to overwrite it.", call. = FALSE)
}
if (!file.rename(stage_root, output_root)) {
  stop("Could not publish the completed output tree atomically.", call. = FALSE)
}
published <- TRUE
if (!file.rename(manifest_temp, output_manifest)) {
  resolved_output <- normalizePath(output_root, mustWork = TRUE)
  if (!identical(dirname(resolved_output), output_parent) ||
      !identical(basename(resolved_output), basename(output_root)) ||
      link_exists(resolved_output) || !isTRUE(file.info(resolved_output)$isdir)) {
    stop("Manifest publication failed and OUTPUT_ROOT is unsafe to roll back.", call. = FALSE)
  }
  unlink(resolved_output, recursive = TRUE, force = FALSE)
  published <- TRUE
  if (file.exists(resolved_output)) stop("Manifest publication failed and rollback failed.", call. = FALSE)
  stop("Could not publish the completed manifest; output tree was rolled back.", call. = FALSE)
}

message("Wrote fresh REP tree: ", output_root)
message("Wrote deterministic manifest: ", output_manifest)
