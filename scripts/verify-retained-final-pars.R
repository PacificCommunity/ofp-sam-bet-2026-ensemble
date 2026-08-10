#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || length(args) > 4L) {
  stop(
    paste(
      "Usage: Rscript scripts/verify-retained-final-pars.R",
      "FINAL_PAR_DIR [OUTPUT.csv|-] [PARALLEL] [fast|native]"
    ),
    call. = FALSE
  )
}

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_arg) != 1L) stop("Could not locate this script.", call. = FALSE)
script_file <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)
par_root <- normalizePath(args[[1L]], mustWork = TRUE)
output_file <- if (length(args) >= 2L) args[[2L]] else "-"
parallel_jobs <- if (length(args) >= 3L) suppressWarnings(as.integer(args[[3L]])) else 1L
mode <- if (length(args) >= 4L) args[[4L]] else "native"
if (is.na(parallel_jobs) || parallel_jobs < 1L || parallel_jobs > 16L) {
  stop("PARALLEL must be an integer from 1 through 16.", call. = FALSE)
}
if (!mode %in% c("fast", "native")) stop("MODE must be fast or native.", call. = FALSE)

manifest_path <- file.path(repo, "data", "ensemble", "retained-final-par-manifest.csv")
ledger_path <- file.path(repo, "data", "ensemble", "fit-diagnostics.csv")
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
ledger <- read.csv(ledger_path, stringsAsFactors = FALSE, check.names = FALSE)

expected_columns <- c(
  "ensemble_id", "kflow_job", "kflow_task", "source_archive",
  "archive_sha256", "final_par_member", "final_par_sha256", "final_par_bytes",
  "objective_function", "maximum_gradient_component", "active_parameters",
  "retention_criterion", "source_commit", "docker_image", "mfcl_version",
  "mfcl_sha256"
)
if (!identical(names(manifest), expected_columns)) {
  stop("Unexpected retained-final-PAR manifest schema.", call. = FALSE)
}

retained_ledger <- ledger[
  is.finite(ledger$maximum_gradient) & ledger$maximum_gradient <= 1e-4,
  c("ensemble_id", "objective_function", "maximum_gradient"),
  drop = FALSE
]
manifest <- manifest[order(manifest$ensemble_id), , drop = FALSE]
retained_ledger <- retained_ledger[order(retained_ledger$ensemble_id), , drop = FALSE]
if (
  nrow(manifest) != 80L || anyDuplicated(manifest$ensemble_id) ||
    !identical(manifest$ensemble_id, retained_ledger$ensemble_id)
) {
  stop("The manifest is not the exact 80-model public MGC-retained set.", call. = FALSE)
}
if (
  any(abs(manifest$objective_function - retained_ledger$objective_function) > 1e-8) ||
    any(abs(manifest$maximum_gradient_component - retained_ledger$maximum_gradient) > 1e-15) ||
    any(manifest$maximum_gradient_component > 1e-4)
) {
  stop("Manifest objective/MGC values differ from the public fit ledger.", call. = FALSE)
}

expected_source_commit <- "24483e3c3a36cddb511fc85454b81a218e1c46e7"
expected_image <- paste0(
  "ghcr.io/pacificcommunity/tuna-flow:v2.5@sha256:",
  "c87f1f6d9d4f62dc447844b58afe35f96af175bf933cb6cffbbbe39a59172360"
)
expected_mfcl_sha <- "8995f72019869863c1d1c0b4f44fc6a6268d1f79031f5bc79dc354ee10f0a63e"
if (
  any(manifest$kflow_task != "bet-2026-ensemble-tau") ||
    any(manifest$source_commit != expected_source_commit) ||
    any(manifest$docker_image != expected_image) ||
    any(manifest$mfcl_version != "2.2.7.9") ||
    any(manifest$mfcl_sha256 != expected_mfcl_sha) ||
    any(manifest$active_parameters != 1997L) ||
    any(manifest$retention_criterion != "public fit ledger maximum_gradient <= 1e-4")
) {
  stop("Manifest execution provenance or retention metadata changed.", call. = FALSE)
}
expected_archive <- sprintf("job-%06d/output_archive.tar.gz", manifest$kflow_job)
expected_member <- sprintf(
  "./outputs/models/%s/final.par", manifest$ensemble_id
)
if (
  !identical(manifest$source_archive, expected_archive) ||
    !identical(manifest$final_par_member, expected_member) ||
    any(!grepl("^[0-9a-f]{64}$", manifest$archive_sha256)) ||
    any(!grepl("^[0-9a-f]{64}$", manifest$final_par_sha256))
) {
  stop("Manifest archive/member identity is invalid.", call. = FALSE)
}

sha256_file <- function(path) {
  output <- system2("sha256sum", shQuote(path), stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) stop("sha256sum failed for ", path, call. = FALSE)
  sub("[[:space:]].*$", "", output[[1L]])
}

mfcl <- file.path(repo, "mfclo64")
if (!file.exists(mfcl) || file.access(mfcl, mode = 1L) != 0L) {
  stop("The bundled native MFCL executable is missing or not executable.", call. = FALSE)
}
if (!identical(sha256_file(mfcl), expected_mfcl_sha)) {
  stop("The bundled native MFCL executable differs from the archived fits.", call. = FALSE)
}

version_dir <- tempfile(pattern = "bet-ensemble-mfcl-version-")
if (!dir.create(version_dir, mode = "0700")) stop("Could not create version scratch.", call. = FALSE)
old_wd <- setwd(version_dir)
version_output <- system2(mfcl, "--version", stdout = TRUE, stderr = TRUE)
version_status <- attr(version_output, "status")
setwd(old_wd)
version_match <- unique(unlist(regmatches(
  version_output,
  gregexpr("[0-9]+([.][0-9]+){3}", version_output)
)))
version_match <- version_match[nzchar(version_match)]
if (
  (!is.null(version_status) && version_status != 0L) ||
    !identical(version_match, "2.2.7.9")
) {
  stop("The bundled executable is not native MFCL 2.2.7.9.", call. = FALSE)
}
version_resolved <- normalizePath(version_dir, mustWork = TRUE)
if (
  !identical(dirname(version_resolved), normalizePath(tempdir(), mustWork = TRUE)) ||
    nzchar(Sys.readlink(version_resolved)) || !isTRUE(file.info(version_resolved)$isdir)
) {
  stop("Refusing an unexpected version scratch directory.", call. = FALSE)
}
unlink(version_resolved, recursive = TRUE, force = FALSE)
if (file.exists(version_resolved)) stop("Could not remove version scratch.", call. = FALSE)

expected_paths <- file.path(par_root, manifest$ensemble_id, "final.par")
actual_paths <- Sys.glob(file.path(par_root, "ensemble-*", "final.par"))
expected_rep_paths <- file.path(
  par_root, manifest$ensemble_id, "plot-11.par.rep"
)
rep_present <- file.exists(expected_rep_paths)
if (any(rep_present) && !all(rep_present)) {
  stop(
    "FINAL_PAR_DIR has an incomplete set of retained plot-11.par.rep files.",
    call. = FALSE
  )
}
expected_files <- if (all(rep_present)) {
  c(expected_paths, expected_rep_paths)
} else {
  expected_paths
}
actual_files <- list.files(par_root, recursive = TRUE, full.names = TRUE, all.files = TRUE)
actual_files <- actual_files[!file.info(actual_files)$isdir]
if (
  !setequal(normalizePath(actual_paths, mustWork = TRUE), normalizePath(expected_paths, mustWork = TRUE)) ||
    !setequal(
      normalizePath(actual_files, mustWork = TRUE),
      normalizePath(expected_files, mustWork = TRUE)
    ) ||
    any(nzchar(Sys.readlink(expected_files)))
) {
  stop(
    paste(
      "FINAL_PAR_DIR must contain exactly the 80 manifest PARs, optionally",
      "paired with all 80 plot-11.par.rep files, and no other files."
    ),
    call. = FALSE
  )
}

numeric_rows_after <- function(lines, marker, count, source) {
  hit <- which(trimws(lines) %in% marker)
  if (!length(hit)) {
    stop(source, " is missing marker: ", paste(marker, collapse = " or "), call. = FALSE)
  }
  rows <- list()
  index <- hit[[1L]] + 1L
  while (index <= length(lines) && length(rows) < count) {
    line <- trimws(lines[[index]])
    if (nzchar(line) && !startsWith(line, "#")) {
      values <- scan(text = line, quiet = TRUE)
      if (!length(values) || any(!is.finite(values))) {
        stop(source, " has a non-numeric row after ", marker[[1L]], call. = FALSE)
      }
      rows[[length(rows) + 1L]] <- values
    }
    index <- index + 1L
  }
  if (length(rows) != count) stop(source, " has an incomplete block.", call. = FALSE)
  rows
}

par_scalar <- function(lines, label, source) {
  numeric_rows_after(lines, label, 1L, source)[[1L]][[1L]]
}

selectivity <- read.csv(
  file.path(repo, "model", "selectivity-models", "F2.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
if (nrow(selectivity) != 33L || !identical(selectivity$fishery, 1:33)) {
  stop("The explicit Diagnostic selectivity source is invalid.", call. = FALSE)
}
design <- read.csv(
  file.path(repo, "design", "model-draws.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
design <- design[match(manifest$ensemble_id, design$ensemble_id), , drop = FALSE]
if (anyNA(design$ensemble_id)) stop("Manifest models are absent from the design.", call. = FALSE)

audit_science <- function(lines, row, source) {
  parest <- numeric_rows_after(lines, "# The parest_flags", 1L, source)[[1L]]
  age_flags <- numeric_rows_after(lines, "# age flags", 1L, source)[[1L]]
  fish_flags <- numeric_rows_after(lines, "# fish flags", 33L, source)
  growth <- numeric_rows_after(lines, "# Seasonal growth parameters", 1L, source)[[1L]]
  age_pars <- numeric_rows_after(
    lines,
    c("# age_pars", "# age-class related parameters (age_pars)"),
    5L, source
  )
  fish_pars <- numeric_rows_after(lines, "# extra fishery parameters", 22L, source)
  if (
    length(parest) < 306L || parest[[111L]] != 4 ||
      parest[[305L]] != 1 || parest[[306L]] != 0 ||
      length(age_flags) < 162L || age_flags[[162L]] != 0 ||
      length(growth) < 29L || abs(growth[[29L]] - row$steepness) > 5e-7
  ) {
    stop(source, " failed fixed tau/steepness switch audit.", call. = FALSE)
  }
  fish_matrix <- do.call(rbind, fish_flags)
  if (
    ncol(fish_matrix) < 61L || any(fish_matrix[, 43L] != 0) ||
      any(fish_matrix[, 44L] != 0) ||
      any(fish_matrix[, 16L] != selectivity$flag16) ||
      any(fish_matrix[, 24L] != selectivity$flag24) ||
      any(fish_matrix[, 56L] != selectivity$flag56) ||
      any(fish_matrix[, 57L] != selectivity$flag57) ||
      any(fish_matrix[, 61L] != selectivity$flag61)
  ) {
    stop(source, " failed the exact 33-fishery Diagnostic selectivity audit.", call. = FALSE)
  }
  expected_tau_par <- row$tau_fish_pars4
  if (
    length(fish_pars[[4L]]) != 33L ||
      any(abs(fish_pars[[4L]] - expected_tau_par) > 5e-7) ||
      length(fish_pars[[22L]]) != 33L || any(fish_pars[[22L]] != 7) ||
      length(age_pars[[5L]]) < 2L ||
      abs(age_pars[[5L]][[1L]] - row$lorenzen_log_intercept) > 5e-7 ||
      age_pars[[5L]][[2L]] != -1
  ) {
    stop(source, " failed fixed tau/M/DM value audit.", call. = FALSE)
  }
  invisible(TRUE)
}

fast_record <- function(index) {
  entry <- manifest[index, , drop = FALSE]
  row <- design[index, , drop = FALSE]
  path <- expected_paths[[index]]
  if (
    !identical(as.numeric(file.info(path)$size), as.numeric(entry$final_par_bytes)) ||
      !identical(sha256_file(path), entry$final_par_sha256)
  ) {
    stop(entry$ensemble_id, " final.par size or SHA-256 differs from the Suva archive.", call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE)
  objective <- par_scalar(lines, "# Objective function value", path)
  mgc <- par_scalar(lines, "# Maximum magnitude gradient value", path)
  active <- par_scalar(lines, "# The number of parameters", path)
  compilation <- par_scalar(lines, "# MULTIFAN-CL compilation version number", path)
  if (
    abs(objective - entry$objective_function) > 1e-8 ||
      abs(mgc - entry$maximum_gradient_component) > 1e-15 ||
      mgc > 1e-4 || active != 1997 || compilation != 2279
  ) {
    stop(entry$ensemble_id, " footer metadata differs from its archive/ledger.", call. = FALSE)
  }
  audit_science(lines, row, path)
  data.frame(
    ensemble_id = entry$ensemble_id,
    par_sha256 = entry$final_par_sha256,
    expected_objective = entry$objective_function,
    par_objective = objective,
    maximum_gradient_component = mgc,
    active_parameters = as.integer(active),
    science_audit = TRUE,
    native_status = NA_integer_,
    native_objective = NA_real_,
    objective_abs_diff = NA_real_,
    evaluated_par_created = NA,
    plot_rep_created = NA,
    mfcl_version = "2.2.7.9",
    mfcl_sha256 = expected_mfcl_sha,
    stringsAsFactors = FALSE
  )
}

records <- lapply(seq_len(nrow(manifest)), fast_record)
names(records) <- manifest$ensemble_id
message("Verified archive identity, ledger metadata and model science for all 80 retained PARs.")

scratch_root <- NULL
if (mode == "native") {
  scratch_root <- tempfile(pattern = "bet-ensemble-native-")
  if (!dir.create(scratch_root, mode = "0700")) stop("Could not create native scratch root.", call. = FALSE)
  scratch_root <- normalizePath(scratch_root, mustWork = TRUE)
  if (
    !identical(dirname(scratch_root), normalizePath(tempdir(), mustWork = TRUE)) ||
      nzchar(Sys.readlink(scratch_root)) || !isTRUE(file.info(scratch_root)$isdir)
  ) {
    stop("Refusing an unexpected native scratch root.", call. = FALSE)
  }

  controls <- c("1 1 0", "1 190 1", "1 246 1")
  native_worker <- function(index) {
    entry <- manifest[index, , drop = FALSE]
    model_id <- entry$ensemble_id
    run_dir <- file.path(scratch_root, model_id)
    cleanup <- function() {
      if (!dir.exists(run_dir)) return(invisible(NULL))
      resolved <- normalizePath(run_dir, mustWork = TRUE)
      if (
        !identical(dirname(resolved), scratch_root) ||
          !identical(basename(resolved), model_id) ||
          nzchar(Sys.readlink(resolved)) || !isTRUE(file.info(resolved)$isdir)
      ) stop("Refusing to remove unexpected native scratch: ", resolved, call. = FALSE)
      unlink(resolved, recursive = TRUE, force = FALSE)
      if (file.exists(resolved)) stop("Could not remove native scratch: ", resolved, call. = FALSE)
    }
    on.exit(cleanup(), add = TRUE)

    prepare_output <- system2(
      "Rscript",
      c(
        shQuote(file.path(repo, "scripts", "prepare-ensemble.R")),
        shQuote(model_id), shQuote(run_dir)
      ),
      stdout = TRUE, stderr = TRUE
    )
    prepare_status <- attr(prepare_output, "status")
    if (!is.null(prepare_status) && prepare_status != 0L) {
      stop(model_id, " input preparation failed: ", paste(prepare_output, collapse = "\n"), call. = FALSE)
    }
    staged_par <- file.path(run_dir, "input.par")
    if (!file.copy(expected_paths[[index]], staged_par, copy.mode = TRUE)) {
      stop("Could not stage ", model_id, " final.par.", call. = FALSE)
    }
    input_sha <- sha256_file(staged_par)
    old_wd <- setwd(run_dir)
    on.exit(setwd(old_wd), add = TRUE)
    audit_log <- file.path(run_dir, "model-audit.log")
    audit_status <- system2(
      "./doitall.sh",
      env = c(
        "MODEL_ID=S0.90-F2", "MODEL_AUDIT_PAR=input.par",
        "PROGRAM_PATH=./mfclo64"
      ),
      stdout = audit_log, stderr = audit_log
    )
    if (audit_status != 0L) stop(model_id, " official model audit failed.", call. = FALSE)

    native_log <- file.path(run_dir, "mfcl-native-load.log")
    native_status <- suppressWarnings(system2(
      "./mfclo64",
      c("bet.frq", "input.par", "evaluated.par", "-file", "-"),
      stdout = native_log, stderr = native_log, input = controls
    ))
    native_status <- as.integer(native_status)
    evaluated_par <- file.path(run_dir, "evaluated.par")
    plot_rep <- file.path(run_dir, "plot-evaluated.par.rep")
    if (
      !(native_status %in% c(0L, 3L)) || !file.exists(evaluated_par) ||
        file.info(evaluated_par)$size <= 0 || !file.exists(plot_rep) ||
        file.info(plot_rep)$size <= 0 || !identical(sha256_file(staged_par), input_sha)
    ) {
      stop(model_id, " native MFCL load/evaluation failed.", call. = FALSE)
    }
    log_lines <- readLines(native_log, warn = FALSE)
    total_lines <- grep("^[[:space:]]*Total func[[:space:]]+", log_lines, value = TRUE)
    native_objective <- suppressWarnings(as.numeric(sub(
      ".*Total func[[:space:]]+", "", tail(total_lines, 1L)
    )))
    evaluated_lines <- readLines(evaluated_par, warn = FALSE)
    evaluated_objective <- par_scalar(
      evaluated_lines, "# Objective function value", evaluated_par
    )
    evaluated_parameters <- par_scalar(
      evaluated_lines, "# The number of parameters", evaluated_par
    )
    audit_science(evaluated_lines, design[index, , drop = FALSE], evaluated_par)
    evaluated_audit_status <- system2(
      "./doitall.sh",
      env = c(
        "MODEL_ID=S0.90-F2", "MODEL_AUDIT_PAR=evaluated.par",
        "PROGRAM_PATH=./mfclo64"
      ),
      stdout = audit_log, stderr = audit_log
    )
    expected_objective <- entry$objective_function
    if (
      length(total_lines) < 1L || length(native_objective) != 1L ||
        !is.finite(native_objective) || evaluated_parameters != 1997 ||
        evaluated_audit_status != 0L ||
        abs(native_objective - expected_objective) > 1e-6 ||
        abs(evaluated_objective - expected_objective) > 1e-6
    ) {
      stop(model_id, " native objective/model parity failed.", call. = FALSE)
    }
    result <- records[[model_id]]
    result$native_status <- native_status
    result$native_objective <- native_objective
    result$objective_abs_diff <- abs(native_objective - expected_objective)
    result$evaluated_par_created <- TRUE
    result$plot_rep_created <- TRUE
    message(sprintf(
      "Native MFCL verified %s: objective %.12f (|diff| %.3g), status %d",
      model_id, native_objective, result$objective_abs_diff, native_status
    ))
    result
  }

  safe_worker <- function(index) {
    tryCatch(
      list(ok = TRUE, value = native_worker(index)),
      error = function(error) list(ok = FALSE, error = conditionMessage(error))
    )
  }
  if (parallel_jobs == 1L) {
    native_results <- lapply(seq_len(nrow(manifest)), safe_worker)
  } else {
    native_results <- parallel::mclapply(
      seq_len(nrow(manifest)), safe_worker,
      mc.cores = parallel_jobs, mc.preschedule = FALSE
    )
  }
  failed <- which(!vapply(native_results, `[[`, logical(1L), "ok"))
  if (length(failed)) {
    errors <- vapply(native_results[failed], `[[`, character(1L), "error")
    stop("Native retained-PAR verification failed:\n", paste(errors, collapse = "\n"), call. = FALSE)
  }
  records <- lapply(native_results, `[[`, "value")
  if (length(list.files(scratch_root, all.files = TRUE, no.. = TRUE))) {
    stop("Native verification scratch root is unexpectedly non-empty.", call. = FALSE)
  }
  unlink(scratch_root, recursive = TRUE, force = FALSE)
  if (file.exists(scratch_root)) stop("Could not remove native scratch root.", call. = FALSE)
}

validation <- do.call(rbind, records)
row.names(validation) <- NULL
if (!identical(validation$ensemble_id, manifest$ensemble_id)) {
  stop("Validation output lost manifest order.", call. = FALSE)
}
if (!identical(output_file, "-")) {
  output_parent <- normalizePath(dirname(output_file), mustWork = TRUE)
  output_path <- file.path(output_parent, basename(output_file))
  output_link <- Sys.readlink(output_path)
  if (file.exists(output_path) || (!is.na(output_link) && nzchar(output_link))) {
    stop("Refusing to overwrite validation output: ", output_path, call. = FALSE)
  }
  write.csv(validation, output_path, row.names = FALSE, quote = TRUE)
  message("Wrote ", output_path)
}

if (mode == "native") {
  message(
    "Verified all 80 retained final PARs with native MFCL 2.2.7.9; maximum objective |difference| = ",
    format(max(validation$objective_abs_diff), scientific = TRUE, digits = 6), "."
  )
} else {
  message("Fast verification passed for all 80 retained final PARs.")
}
