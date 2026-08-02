ensemble_sha256 <- function(path) {
  output <- system2("sha256sum", path, stdout = TRUE)
  if (length(output) != 1L) stop("sha256sum failed for ", path, call. = FALSE)
  strsplit(output, "[[:space:]]+")[[1L]][1L]
}

ensemble_fields <- function(line) strsplit(trimws(line), "[[:space:]]+")[[1L]]

ensemble_rows_after <- function(lines, marker, n) {
  hit <- grep(marker, lines)
  if (length(hit) != 1L) stop("Expected one marker matching ", marker, call. = FALSE)
  rows <- integer()
  i <- hit + 1L
  while (i <= length(lines) && length(rows) < n) {
    if (nzchar(trimws(lines[[i]])) && !grepl("^[[:space:]]*#", lines[[i]])) {
      rows <- c(rows, i)
    }
    i <- i + 1L
  }
  if (length(rows) != n) stop("Incomplete block after ", marker, call. = FALSE)
  rows
}

ensemble_value_after <- function(path, marker, row, column) {
  lines <- readLines(path, warn = FALSE)
  index <- ensemble_rows_after(lines, marker, row)[[row]]
  ensemble_fields(lines[[index]])[[column]]
}

ensemble_replace_field <- function(path, marker, row, column, value) {
  lines <- readLines(path, warn = FALSE)
  index <- ensemble_rows_after(lines, marker, row)[[row]]
  values <- ensemble_fields(lines[[index]])
  if (length(values) < column) stop("Missing field in ", path, call. = FALSE)
  values[[column]] <- as.character(value)
  lines[[index]] <- paste(values, collapse = " ")
  writeLines(lines, path, useBytes = TRUE)
}

ensemble_tag_matrix <- function(path) {
  lines <- readLines(path, warn = FALSE)
  rows <- ensemble_rows_after(lines, "^# tag flags[[:space:]]*$", 98L)
  do.call(rbind, lapply(rows, function(i) ensemble_fields(lines[[i]])))
}

ensemble_validate_mixing_source <- function(path) {
  values <- ensemble_tag_matrix(path)[, 1L]
  numeric_values <- suppressWarnings(as.numeric(values))
  if (length(values) != 98L || anyNA(numeric_values) || any(!is.finite(numeric_values)) ||
      any(numeric_values != as.integer(numeric_values)) || any(!numeric_values %in% 0:4)) {
    stop(
      "Invalid tag-mixing source first column (expected 98 finite integers in 0:4): ",
      path, call. = FALSE
    )
  }
  invisible(as.integer(numeric_values))
}

ensemble_replace_tag_column <- function(path, column, source_path = NULL, constant = NULL) {
  lines <- readLines(path, warn = FALSE)
  rows <- ensemble_rows_after(lines, "^# tag flags[[:space:]]*$", 98L)
  source_values <- NULL
  if (!is.null(source_path)) source_values <- ensemble_tag_matrix(source_path)[, column]
  for (j in seq_along(rows)) {
    values <- ensemble_fields(lines[[rows[[j]]]])
    if (length(values) != 10L) stop("Tag flag row does not have 10 fields.", call. = FALSE)
    values[[column]] <- if (is.null(source_values)) as.character(constant) else source_values[[j]]
    lines[[rows[[j]]]] <- paste(values, collapse = " ")
  }
  writeLines(lines, path, useBytes = TRUE)
}

ensemble_effort_records <- function(lines) {
  answer <- list()
  for (i in seq_along(lines)) {
    values <- ensemble_fields(lines[[i]])
    if (length(values) < 7L || any(!grepl("^[0-9]+$", values[1:4]))) next
    fishery <- suppressWarnings(as.integer(values[[4L]]))
    if (!is.na(fishery) && fishery >= 29L && fishery <= 33L) {
      key <- paste(values[1:4], collapse = ":")
      if (!is.null(answer[[key]])) stop("Duplicate FRQ key: ", key, call. = FALSE)
      answer[[key]] <- list(line = i, fields = values)
    }
  }
  answer
}

ensemble_replace_sixth_token <- function(line, value) {
  locations <- gregexpr("[^[:space:]]+", line)[[1L]]
  lengths <- attr(locations, "match.length")
  if (length(locations) < 6L) stop("FRQ row has fewer than six fields.", call. = FALSE)
  start <- locations[[6L]]
  finish <- start + lengths[[6L]] - 1L
  paste0(substr(line, 1L, start - 1L), value, substr(line, finish + 1L, nchar(line)))
}

ensemble_replace_effort <- function(base_path, source_path) {
  base <- readLines(base_path, warn = FALSE)
  source <- readLines(source_path, warn = FALSE)
  base_records <- ensemble_effort_records(base)
  source_records <- ensemble_effort_records(source)
  if (length(base_records) != 1458L || !setequal(names(base_records), names(source_records))) {
    stop("Diagnostic and source F29-F33 records do not match (expected 1458).", call. = FALSE)
  }
  for (key in names(base_records)) {
    index <- base_records[[key]]$line
    base[[index]] <- ensemble_replace_sixth_token(
      base[[index]], source_records[[key]]$fields[[6L]]
    )
  }
  writeLines(base, base_path, useBytes = TRUE)
}

ensemble_verify_manifest <- function(root, manifest_path) {
  manifest <- read.table(manifest_path, col.names = c("sha256", "file"), stringsAsFactors = FALSE)
  observed <- vapply(file.path(root, manifest$file), ensemble_sha256, character(1))
  if (any(observed != manifest$sha256)) {
    stop("Hash mismatch: ", paste(manifest$file[observed != manifest$sha256], collapse = ", "), call. = FALSE)
  }
  invisible(manifest)
}

ensemble_refresh_manifest <- function(run_dir) {
  path <- file.path(run_dir, "MANIFEST.sha256")
  manifest <- read.table(path, col.names = c("sha256", "file"), stringsAsFactors = FALSE)
  manifest$sha256 <- vapply(file.path(run_dir, manifest$file), ensemble_sha256, character(1))
  write.table(manifest, path, row.names = FALSE, col.names = FALSE, quote = FALSE)
}

ensemble_refresh_checkpoint_md5 <- function(doitall, checkpoints) {
  lines <- readLines(doitall, warn = FALSE)
  hits <- grep("^[[:space:]]*expected_output=[0-9a-f]{32}[[:space:]]*$", lines)
  if (length(hits) != 3L) stop("Expected three checkpoint output hashes.", call. = FALSE)
  lines[hits] <- paste0("      expected_output=", unname(tools::md5sum(checkpoints)))
  writeLines(lines, doitall, useBytes = TRUE)
}

ensemble_source_hashes <- function(repo) {
  mixing <- read.csv(file.path(repo, "design", "mixing-sources.csv"), stringsAsFactors = FALSE)
  effort <- read.csv(file.path(repo, "design", "effort-creep-sources.csv"), stringsAsFactors = FALSE)
  expected <- c(
    setNames(mixing$source_sha256, file.path("sources", "mixing", mixing$tag_mixing_source_file)),
    setNames(effort$source_sha256, file.path("sources", "effort-creep", effort$effort_source_file))
  )
  observed <- vapply(file.path(repo, names(expected)), ensemble_sha256, character(1))
  if (any(observed != unname(expected))) {
    stop("Authoritative source hash mismatch: ", paste(names(expected)[observed != unname(expected)], collapse = ", "), call. = FALSE)
  }
  invisible(lapply(
    file.path(repo, "sources", "mixing", mixing$tag_mixing_source_file),
    ensemble_validate_mixing_source
  ))
  invisible(expected)
}

ensemble_load_row <- function(repo, model_id) {
  design <- read.csv(file.path(repo, "design", "model-draws.csv"), stringsAsFactors = FALSE, check.names = FALSE)
  row <- design[design$ensemble_id == model_id, , drop = FALSE]
  if (nrow(row) != 1L) stop("Unknown ensemble model: ", model_id, call. = FALSE)
  row
}

ensemble_assert_lines <- function(base_path, actual_path, allowed_fields) {
  base <- readLines(base_path, warn = FALSE)
  actual <- readLines(actual_path, warn = FALSE)
  if (length(base) != length(actual)) stop("Line count changed in ", actual_path, call. = FALSE)
  changed <- which(base != actual)
  allowed_rows <- as.integer(names(allowed_fields))
  if (length(setdiff(changed, allowed_rows))) {
    stop("Unexpected line changed in ", actual_path, ": ", paste(setdiff(changed, allowed_rows), collapse = ", "), call. = FALSE)
  }
  for (index in changed) {
    before <- ensemble_fields(base[[index]])
    after <- ensemble_fields(actual[[index]])
    allowed <- allowed_fields[[as.character(index)]]
    if (length(before) != length(after) || !identical(before[-allowed], after[-allowed])) {
      stop("Unexpected field changed at line ", index, " in ", actual_path, call. = FALSE)
    }
  }
}

ensemble_verify_inputs <- function(repo, model_id, run_dir) {
  row <- ensemble_load_row(repo, model_id)
  ensemble_verify_manifest(file.path(repo, "model"), file.path(repo, "model", "MANIFEST.sha256"))
  ensemble_source_hashes(repo)
  ensemble_verify_manifest(run_dir, file.path(run_dir, "INPUTS.sha256"))

  base <- file.path(repo, "model")
  checkpoints <- file.path("checkpoints", paste0(c("phase01", "phase02", "phase05"), "-seed23.par"))
  target_paths <- c("bet.ini", checkpoints)
  markers_h <- c("^# sv[(]29[)][[:space:]]*$", rep("^# Seasonal growth parameters[[:space:]]*$", 3L))
  fields_h <- c(1L, rep(29L, 3L))
  markers_m <- c("^# age_pars[[:space:]]*$", rep("^# age-class related parameters [(]age_pars[)][[:space:]]*$", 3L))
  source_tags <- ensemble_tag_matrix(file.path(repo, "sources", "mixing", row$tag_mixing_source_file))

  for (i in seq_along(target_paths)) {
    path <- file.path(run_dir, target_paths[[i]])
    base_path <- file.path(base, target_paths[[i]])
    observed_h <- as.numeric(ensemble_value_after(path, markers_h[[i]], 1L, fields_h[[i]]))
    observed_m <- as.numeric(ensemble_value_after(path, markers_m[[i]], 5L, 1L))
    observed_slope <- as.numeric(ensemble_value_after(path, markers_m[[i]], 5L, 2L))
    if (abs(observed_h - row$steepness) > 1e-12) stop("Steepness mismatch in ", target_paths[[i]], call. = FALSE)
    if (abs(observed_m - row$lorenzen_log_intercept) > 1e-12 || observed_slope != -1) {
      stop("Lorenzen M0 mismatch in ", target_paths[[i]], call. = FALSE)
    }
    actual_tags <- ensemble_tag_matrix(path)
    base_tags <- ensemble_tag_matrix(base_path)
    if (!identical(actual_tags[, 1L], source_tags[, 1L]) ||
        any(as.integer(actual_tags[, 2L]) != row$tag_reporting_flag2) ||
        !identical(actual_tags[, -(1:2), drop = FALSE], base_tags[, -(1:2), drop = FALSE])) {
      stop("Tag flags do not match the selected mixing/reporting inputs in ", target_paths[[i]], call. = FALSE)
    }

    base_lines <- readLines(base_path, warn = FALSE)
    actual_lines <- readLines(path, warn = FALSE)
    h_row <- ensemble_rows_after(base_lines, markers_h[[i]], 1L)[[1L]]
    m_row <- ensemble_rows_after(base_lines, markers_m[[i]], 5L)[[5L]]
    tag_rows <- ensemble_rows_after(base_lines, "^# tag flags[[:space:]]*$", 98L)
    allowed <- setNames(lapply(tag_rows, function(x) c(1L, 2L)), as.character(tag_rows))
    allowed[[as.character(h_row)]] <- fields_h[[i]]
    allowed[[as.character(m_row)]] <- 1L
    ensemble_assert_lines(base_path, path, allowed)
  }

  base_frq <- readLines(file.path(base, "bet.frq"), warn = FALSE)
  actual_frq <- readLines(file.path(run_dir, "bet.frq"), warn = FALSE)
  source_frq <- readLines(file.path(repo, "sources", "effort-creep", row$effort_source_file), warn = FALSE)
  base_records <- ensemble_effort_records(base_frq)
  actual_records <- ensemble_effort_records(actual_frq)
  source_records <- ensemble_effort_records(source_frq)
  if (length(base_records) != 1458L || !setequal(names(base_records), names(actual_records)) ||
      !setequal(names(base_records), names(source_records))) stop("Incomplete F29-F33 effort records.", call. = FALSE)
  allowed_frq <- list()
  for (key in names(base_records)) {
    before <- base_records[[key]]$fields
    actual <- actual_records[[key]]$fields
    source <- source_records[[key]]$fields
    if (!identical(actual[[6L]], source[[6L]]) || !identical(before[-6L], actual[-6L])) {
      stop("Effort-creep transfer mismatch at ", key, call. = FALSE)
    }
    allowed_frq[[as.character(base_records[[key]]$line)]] <- 6L
  }
  ensemble_assert_lines(file.path(base, "bet.frq"), file.path(run_dir, "bet.frq"), allowed_frq)

  base_doitall <- readLines(file.path(base, "doitall.sh"), warn = FALSE)
  actual_doitall <- readLines(file.path(run_dir, "doitall.sh"), warn = FALSE)
  allowed_doitall_rows <- c(
    grep("^[[:space:]]*expected_output=[0-9a-f]{32}[[:space:]]*$", base_doitall),
    grep("^[[:space:]]*expected = -2[.]54930339768360[[:space:]]*$", base_doitall)
  )
  allowed_doitall <- setNames(lapply(allowed_doitall_rows, function(x) seq_along(ensemble_fields(base_doitall[[x]]))),
                                as.character(allowed_doitall_rows))
  ensemble_assert_lines(file.path(base, "doitall.sh"), file.path(run_dir, "doitall.sh"), allowed_doitall)
  if (!any(grepl(paste0("expected = ", format(row$lorenzen_log_intercept, digits = 17, scientific = TRUE)), actual_doitall, fixed = TRUE))) {
    stop("Final M audit was not updated in doitall.sh.", call. = FALSE)
  }

  base_manifest <- read.table(file.path(base, "MANIFEST.sha256"), col.names = c("sha256", "file"), stringsAsFactors = FALSE)
  actual_manifest <- read.table(file.path(run_dir, "MANIFEST.sha256"), col.names = c("sha256", "file"), stringsAsFactors = FALSE)
  if (!identical(base_manifest$file, actual_manifest$file)) stop("Model manifest file list changed.", call. = FALSE)
  permitted <- c("bet.ini", "bet.frq", "doitall.sh", checkpoints)
  changed_manifest <- base_manifest$file[base_manifest$sha256 != actual_manifest$sha256]
  if (length(setdiff(changed_manifest, permitted))) stop("Unexpected model file changed: ", paste(setdiff(changed_manifest, permitted), collapse = ", "), call. = FALSE)
  for (relative in setdiff(base_manifest$file, permitted)) {
    if (ensemble_sha256(file.path(base, relative)) != ensemble_sha256(file.path(run_dir, relative))) {
      stop("Unexpected non-target input change: ", relative, call. = FALSE)
    }
  }

  metadata <- read.csv(file.path(run_dir, "ensemble-metadata.csv"), stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(metadata) != 1L || metadata$ensemble_id != model_id || metadata$model_label != row$model_label) {
    stop("Ensemble metadata mismatch.", call. = FALSE)
  }
  invisible(data.frame(
    ensemble_id = model_id,
    model_label = row$model_label,
    steepness = row$steepness,
    tag_mixing_k_cutoff = row$tag_mixing_k_cutoff,
    tag_reporting_flag2 = row$tag_reporting_flag2,
    m0_quarterly = row$m_age40_quarterly,
    effort_creep_primary = row$effort_creep_primary,
    effort_creep_secondary = row$effort_creep_secondary,
    status = "passed",
    stringsAsFactors = FALSE
  ))
}

ensemble_prepare_inputs <- function(repo, model_id, output) {
  row <- ensemble_load_row(repo, model_id)
  ensemble_verify_manifest(file.path(repo, "model"), file.path(repo, "model", "MANIFEST.sha256"))
  ensemble_source_hashes(repo)
  if (dir.exists(output) && length(list.files(output, all.files = TRUE, no.. = TRUE))) {
    stop("Output directory is not empty: ", output, call. = FALSE)
  }
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  status <- system2("cp", c("-a", paste0(file.path(repo, "model"), "/."), output))
  if (!identical(status, 0L)) stop("Failed to copy frozen Diagnostic inputs.", call. = FALSE)
  if (!file.copy(file.path(repo, "mfclo64"), file.path(output, "mfclo64"), overwrite = TRUE)) {
    stop("Failed to copy mfclo64.", call. = FALSE)
  }
  Sys.chmod(file.path(output, c("mfclo64", "doitall.sh")), mode = "0755")

  ini <- file.path(output, "bet.ini")
  doitall <- file.path(output, "doitall.sh")
  checkpoint_rel <- file.path("checkpoints", paste0(c("phase01", "phase02", "phase05"), "-seed23.par"))
  checkpoints <- file.path(output, checkpoint_rel)
  target_paths <- c(ini, checkpoints)
  h_markers <- c("^# sv[(]29[)][[:space:]]*$", rep("^# Seasonal growth parameters[[:space:]]*$", 3L))
  h_fields <- c(1L, rep(29L, 3L))
  m_markers <- c("^# age_pars[[:space:]]*$", rep("^# age-class related parameters [(]age_pars[)][[:space:]]*$", 3L))
  h_value <- format(row$steepness, digits = 17, scientific = TRUE)
  m_value <- format(row$lorenzen_log_intercept, digits = 17, scientific = TRUE)
  mixing_source <- file.path(repo, "sources", "mixing", row$tag_mixing_source_file)

  for (i in seq_along(target_paths)) {
    ensemble_replace_field(target_paths[[i]], h_markers[[i]], 1L, h_fields[[i]], h_value)
    ensemble_replace_field(target_paths[[i]], m_markers[[i]], 5L, 1L, m_value)
    ensemble_replace_tag_column(target_paths[[i]], 1L, source_path = mixing_source)
    ensemble_replace_tag_column(target_paths[[i]], 2L, constant = row$tag_reporting_flag2)
  }
  ensemble_replace_effort(
    file.path(output, "bet.frq"),
    file.path(repo, "sources", "effort-creep", row$effort_source_file)
  )

  lines <- readLines(doitall, warn = FALSE)
  hit <- grep("^[[:space:]]*expected = -2[.]54930339768360[[:space:]]*$", lines)
  if (length(hit) != 1L) stop("Could not locate the final M audit in doitall.sh.", call. = FALSE)
  lines[[hit]] <- paste0("  expected = ", m_value)
  writeLines(lines, doitall, useBytes = TRUE)
  ensemble_refresh_checkpoint_md5(doitall, checkpoints)
  ensemble_refresh_manifest(output)

  metadata <- row
  metadata$diagnostic_source_commit <- "be953e4271e7f8119f982d5efebb21a5e8e364b3"
  metadata$input_status <- "prepared-and-verified"
  write.csv(metadata, file.path(output, "ensemble-metadata.csv"), row.names = FALSE, quote = TRUE)

  files <- list.files(output, recursive = TRUE, full.names = TRUE)
  files <- files[!file.info(files)$isdir]
  relative <- substring(files, nchar(output) + 2L)
  keep <- relative != "INPUTS.sha256"
  input_manifest <- data.frame(
    sha256 = vapply(files[keep], ensemble_sha256, character(1)),
    file = relative[keep], stringsAsFactors = FALSE
  )
  input_manifest <- input_manifest[order(input_manifest$file), ]
  write.table(input_manifest, file.path(output, "INPUTS.sha256"), row.names = FALSE, col.names = FALSE, quote = FALSE)

  audit <- ensemble_verify_inputs(repo, model_id, output)
  write.csv(audit, file.path(output, "input-change-audit.csv"), row.names = FALSE, quote = TRUE)
  cat("Prepared and verified ", model_id, ": ", row$model_label, "\n", sep = "")
  invisible(audit)
}
