#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_arg) != 1L) stop("Cannot locate repository root.", call. = FALSE)
repo <- normalizePath(file.path(dirname(sub("^--file=", "", script_arg)), ".."), mustWork = TRUE)
args <- commandArgs(trailingOnly = TRUE)
baseline_commit <- if (length(args)) args[[1L]] else {
  "3a940f07aac5c123d019adadc73b3b0ab3897a88"
}
if (!grepl("^[0-9a-f]{40}$", baseline_commit)) {
  stop("BASELINE_COMMIT must be a full 40-character Git commit.", call. = FALSE)
}

old_wd <- setwd(repo)
on.exit(setwd(old_wd), add = TRUE)
baseline_file <- tempfile(pattern = "more-tau-main-draws-")
baseline_pairing <- tempfile(pattern = "more-tau-main-pairing-")
git_error <- tempfile(pattern = "more-tau-git-error-")
on.exit(unlink(c(baseline_file, baseline_pairing, git_error)), add = TRUE)

capture_git_file <- function(spec, output) {
  status <- system2("git", c("show", spec), stdout = output, stderr = git_error)
  if (!identical(status, 0L)) {
    detail <- if (file.exists(git_error)) paste(readLines(git_error, warn = FALSE), collapse = "\n") else ""
    stop("Could not read baseline Git object ", spec, ". ", detail, call. = FALSE)
  }
}
capture_git_file(paste0(baseline_commit, ":design/model-draws.csv"), baseline_file)
capture_git_file(paste0(baseline_commit, ":design/pairing-map.csv"), baseline_pairing)

baseline <- read.csv(baseline_file, check.names = FALSE)
current <- read.csv(file.path(repo, "design", "model-draws.csv"), check.names = FALSE)
tau_columns <- c("tag_tau", "tau_fish_pars4", "model_label")
if (!identical(names(current), names(baseline))) {
  stop("model-draws.csv schema differs from main.", call. = FALSE)
}
if (!all(tau_columns %in% names(current))) {
  stop("model-draws.csv lacks a required tau-dependent field.", call. = FALSE)
}
non_tau_columns <- setdiff(names(current), tau_columns)
different_non_tau <- non_tau_columns[!vapply(
  non_tau_columns,
  function(column) identical(current[[column]], baseline[[column]]),
  logical(1L)
)]
if (length(different_non_tau)) {
  stop(
    "Non-tau fields differ from main: ",
    paste(different_non_tau, collapse = ", "),
    call. = FALSE
  )
}
unchanged_tau <- tau_columns[vapply(
  tau_columns,
  function(column) identical(current[[column]], baseline[[column]]),
  logical(1L)
)]
if (length(unchanged_tau)) {
  stop("Expected tau-dependent fields did not change: ", paste(unchanged_tau, collapse = ", "),
       call. = FALSE)
}

read_raw <- function(path) {
  size <- file.info(path)$size
  connection <- file(path, open = "rb")
  on.exit(close(connection))
  readBin(connection, what = "raw", n = size)
}
if (!identical(read_raw(baseline_pairing), read_raw(file.path(repo, "design", "pairing-map.csv")))) {
  stop("The frozen pairing map differs byte-for-byte from main.", call. = FALSE)
}

protected_roots <- c("data", "final-par", "report", "results")
legacy_changes <- system2(
  "git",
  c("diff", "--name-only", baseline_commit, "--", protected_roots),
  stdout = TRUE,
  stderr = TRUE
)
legacy_status <- attr(legacy_changes, "status")
if ((!is.null(legacy_status) && legacy_status != 0L) || length(legacy_changes)) {
  stop(
    "Legacy main result payload changed: ",
    paste(legacy_changes, collapse = ", "),
    call. = FALSE
  )
}
legacy_status_lines <- system2(
  "git",
  c("status", "--porcelain=v1", "--untracked-files=all", "--", protected_roots),
  stdout = TRUE,
  stderr = TRUE
)
legacy_status_code <- attr(legacy_status_lines, "status")
legacy_untracked <- legacy_status_lines[grepl("^[?][?][[:space:]]", legacy_status_lines)]
if ((!is.null(legacy_status_code) && legacy_status_code != 0L) || length(legacy_untracked)) {
  stop(
    "Untracked files were added under protected legacy result roots: ",
    paste(legacy_untracked, collapse = ", "),
    call. = FALSE
  )
}

cat(
  "Parity passed against main ", baseline_commit, ": all 100 rows and ",
  length(non_tau_columns), " non-tau fields are identical; only ",
  paste(tau_columns, collapse = ", "), " changed; pairing map and legacy outputs are byte-identical.\n",
  sep = ""
)
