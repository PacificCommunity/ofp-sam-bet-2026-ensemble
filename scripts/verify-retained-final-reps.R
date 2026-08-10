#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || length(args) > 3L) {
  stop(
    paste(
      "Usage: Rscript scripts/verify-retained-final-reps.R",
      "PUBLIC_ROOT [REP_MANIFEST.csv] [fast|parse]"
    ),
    call. = FALSE
  )
}

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_arg) != 1L) stop("Could not locate this script.", call. = FALSE)
script_file <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)
public_root <- normalizePath(args[[1L]], mustWork = TRUE)
rep_manifest_path <- if (length(args) >= 2L) {
  normalizePath(args[[2L]], mustWork = TRUE)
} else {
  normalizePath(
    file.path(repo, "data", "ensemble", "retained-final-rep-manifest.csv"),
    mustWork = TRUE
  )
}
mode <- if (length(args) >= 3L) args[[3L]] else "fast"
if (!mode %in% c("fast", "parse")) stop("MODE must be fast or parse.", call. = FALSE)

link_exists <- function(path) {
  link <- Sys.readlink(path)
  !is.na(link) && nzchar(link)
}
if (link_exists(public_root) || !isTRUE(file.info(public_root)$isdir)) {
  stop("PUBLIC_ROOT must be a real directory, not a symbolic link.", call. = FALSE)
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

par_manifest <- read.csv(
  file.path(repo, "data", "ensemble", "retained-final-par-manifest.csv"),
  check.names = FALSE
)
rep_manifest <- read.csv(rep_manifest_path, check.names = FALSE)
par_manifest <- par_manifest[order(par_manifest$ensemble_id), , drop = FALSE]
rep_manifest <- rep_manifest[order(rep_manifest$ensemble_id), , drop = FALSE]

expected_rep_columns <- c(
  "ensemble_id", "source_archive", "archive_sha256", "final_par_sha256",
  "plot_rep_file", "source_rep_member", "plot_rep_sha256", "plot_rep_bytes",
  "plot_rep_lines", "source_commit", "mfcl_version", "mfcl_sha256"
)
if (!identical(names(rep_manifest), expected_rep_columns)) {
  stop("Unexpected retained-final-REP manifest schema.", call. = FALSE)
}
expected_mfcl_sha <- "8995f72019869863c1d1c0b4f44fc6a6268d1f79031f5bc79dc354ee10f0a63e"
if (nrow(par_manifest) != 80L || nrow(rep_manifest) != 80L ||
    anyDuplicated(par_manifest$ensemble_id) || anyDuplicated(rep_manifest$ensemble_id) ||
    !identical(par_manifest$ensemble_id, rep_manifest$ensemble_id)) {
  stop("The PAR and REP manifests are not the same exact 80-model set.", call. = FALSE)
}
expected_rep_files <- file.path(rep_manifest$ensemble_id, "plot-11.par.rep")
expected_source_rep_members <- paste0(
  "./outputs/models/", rep_manifest$ensemble_id, "/plot-11.par.rep"
)
if (!identical(rep_manifest$plot_rep_file, expected_rep_files) ||
    !identical(rep_manifest$final_par_sha256, par_manifest$final_par_sha256) ||
    !identical(rep_manifest$source_archive, par_manifest$source_archive) ||
    !identical(rep_manifest$archive_sha256, par_manifest$archive_sha256) ||
    !identical(rep_manifest$source_rep_member, expected_source_rep_members) ||
    !identical(rep_manifest$source_commit, par_manifest$source_commit) ||
    any(rep_manifest$plot_rep_lines != 6349L) ||
    any(rep_manifest$mfcl_version != "2.2.7.9") ||
    any(rep_manifest$mfcl_sha256 != expected_mfcl_sha) ||
    any(!grepl("^[0-9a-f]{64}$", rep_manifest$plot_rep_sha256)) ||
    anyDuplicated(rep_manifest$plot_rep_sha256)) {
  stop("Retained-final-REP provenance or objective parity is invalid.", call. = FALSE)
}

top_entries <- list.files(
  public_root, all.files = TRUE, full.names = TRUE, no.. = TRUE
)
expected_dirs <- file.path(public_root, rep_manifest$ensemble_id)
if (length(top_entries) != 80L ||
    !setequal(normalizePath(top_entries, mustWork = TRUE),
              normalizePath(expected_dirs, mustWork = TRUE)) ||
    any(nzchar(Sys.readlink(top_entries))) || any(!file.info(top_entries)$isdir)) {
  stop("PUBLIC_ROOT must contain exactly the 80 real ensemble directories.", call. = FALSE)
}

for (index in seq_len(nrow(rep_manifest))) {
  entry <- rep_manifest[index, , drop = FALSE]
  model_id <- entry$ensemble_id
  model_dir <- file.path(public_root, model_id)
  paths <- c(file.path(model_dir, "final.par"), file.path(public_root, entry$plot_rep_file))
  actual <- list.files(
    model_dir, all.files = TRUE, full.names = TRUE, no.. = TRUE
  )
  if (length(actual) != 2L || !setequal(actual, paths) ||
      any(nzchar(Sys.readlink(paths))) || any(file.info(paths)$isdir)) {
    stop(model_id, " must contain exactly final.par and plot-11.par.rep without links.", call. = FALSE)
  }
  observed_bytes <- as.numeric(file.info(paths)$size)
  expected_bytes <- c(
    as.numeric(par_manifest$final_par_bytes[[index]]),
    as.numeric(entry$plot_rep_bytes)
  )
  observed_sha <- vapply(paths, sha256_file, character(1L))
  expected_sha <- c(entry$final_par_sha256, entry$plot_rep_sha256)
  if (!identical(observed_bytes, expected_bytes) ||
      !identical(unname(observed_sha), unname(expected_sha))) {
    stop(model_id, " public PAR or REP differs from its manifest.", call. = FALSE)
  }
  validate_rep(paths[[2L]], model_id)
  if (mode == "parse") {
    if (!requireNamespace("FLR4MFCL", quietly = TRUE)) {
      stop("parse mode requires the optional FLR4MFCL package.", call. = FALSE)
    }
    parsed <- tryCatch(
      FLR4MFCL::read.MFCLRep(paths[[2L]]),
      error = function(error) stop(model_id, " FLR4MFCL parse failed: ", conditionMessage(error), call. = FALSE)
    )
    if (!methods::is(parsed, "MFCLRep")) {
      stop(model_id, " did not parse to an MFCLRep object.", call. = FALSE)
    }
    methods::validObject(parsed)
    nofish <- c(
      as.numeric(FLR4MFCL::totalBiomass_nofish(parsed)),
      as.numeric(FLR4MFCL::adultBiomass_nofish(parsed))
    )
    if (!length(nofish) || any(!is.finite(nofish))) {
      stop(model_id, " has incomplete no-fishing biomass sections.", call. = FALSE)
    }
  }
}

message(
  "Verified all 80 retained final PAR/REP pairs (exact hashes, 6,349-line Viewer reports",
  if (mode == "parse") ", and FLR4MFCL parses" else "",
  ")."
)
