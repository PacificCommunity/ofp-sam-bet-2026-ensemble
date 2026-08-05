#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4L) {
  stop(
    paste(
      "Usage: process-native-hessian-uncertainty.R MODEL_DIR",
      "ENSEMBLE_ID POINT_TIMESERIES_CSV OUTPUT_RDS"
    ),
    call. = FALSE
  )
}

model_dir <- normalizePath(args[[1L]], mustWork = TRUE)
ensemble_id <- args[[2L]]
point_file <- normalizePath(args[[3L]], mustWork = TRUE)
output_file <- args[[4L]]
n_draws <- as.integer(Sys.getenv("HESSIAN_DRAWS_PER_MODEL", "100"))
if (!grepl("^ensemble-[0-9]{3}$", ensemble_id) || n_draws < 1L) {
  stop("Invalid ensemble ID or draw count.", call. = FALSE)
}

required_files <- c(
  "final.par", "bet.hes", "bet.dep", "bet.dp2",
  "deplabel.tmp", "deplabel_noeff.tmp"
)
missing_files <- required_files[!file.exists(file.path(model_dir, required_files))]
if (length(missing_files)) {
  stop("Missing native MFCL file: ", missing_files[[1L]], call. = FALSE)
}

sha256 <- function(path) {
  output <- system2("sha256sum", path, stdout = TRUE, stderr = TRUE)
  if (!length(output)) stop("Could not calculate SHA-256 for ", path)
  strsplit(output[[1L]], "[[:space:]]+")[[1L]][[1L]]
}

read_native_matrix <- function(path) {
  connection <- file(path, "rb")
  on.exit(close(connection), add = TRUE)
  n_parameter <- readBin(connection, integer(), n = 1L, size = 4L, endian = "little")
  n_derived <- readBin(connection, integer(), n = 1L, size = 4L, endian = "little")
  values <- readBin(
    connection, numeric(), n = n_parameter * n_derived,
    size = 8L, endian = "little"
  )
  if (length(values) != n_parameter * n_derived) {
    stop("Incomplete native MFCL gradient matrix: ", path, call. = FALSE)
  }
  matrix(values, nrow = n_derived, ncol = n_parameter, byrow = TRUE)
}

read_native_hessian <- function(path) {
  connection <- file(path, "rb")
  on.exit(close(connection), add = TRUE)
  n_parameter <- readBin(connection, integer(), n = 1L, size = 4L, endian = "little")
  # MFCL writes an ADMB dmatrix after n_parameter.  The uostream dmatrix
  # record begins with its row bounds (1, n_parameter), followed by the
  # contiguous row-major double values.  These two integers are not Hessian
  # entries and must be consumed explicitly outside ADMB.
  bounds <- readBin(connection, integer(), n = 2L, size = 4L, endian = "little")
  if (!identical(bounds, c(1L, n_parameter))) {
    stop("Unexpected native MFCL Hessian matrix bounds in ", path, call. = FALSE)
  }
  values <- readBin(
    connection, numeric(), n = n_parameter * n_parameter,
    size = 8L, endian = "little"
  )
  if (length(values) != n_parameter * n_parameter) {
    stop("Incomplete native MFCL Hessian: ", path, call. = FALSE)
  }
  value <- matrix(values, nrow = n_parameter, ncol = n_parameter, byrow = TRUE)
  (value + t(value)) / 2
}

read_labels <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (length(lines) %% 2L != 0L) {
    stop("Incomplete native MFCL label file: ", path, call. = FALSE)
  }
  data.frame(
    value = as.numeric(lines[seq.int(1L, length(lines), by = 2L)]),
    label = lines[seq.int(2L, length(lines), by = 2L)],
    stringsAsFactors = FALSE
  )
}

indexed_rows <- function(labels, prefix, count) {
  wanted <- paste0(prefix, "(", seq_len(count), ")")
  rows <- match(wanted, labels$label)
  if (anyNA(rows)) {
    stop("Missing native MFCL dependent variable: ", wanted[is.na(rows)][[1L]])
  }
  rows
}

annual_log_gradient <- function(log_value, gradient, operation = c("mean", "sum")) {
  operation <- match.arg(operation)
  if (length(log_value) %% 4L != 0L) stop("Expected quarterly native values.")
  n_year <- length(log_value) %/% 4L
  result <- matrix(NA_real_, nrow = n_year, ncol = ncol(gradient))
  log_result <- numeric(n_year)
  for (year_index in seq_len(n_year)) {
    rows <- 4L * (year_index - 1L) + seq_len(4L)
    shifted <- log_value[rows] - max(log_value[rows])
    weights <- exp(shifted) / sum(exp(shifted))
    result[year_index, ] <- colSums(gradient[rows, , drop = FALSE] * weights)
    scale <- if (operation == "mean") 4 else 1
    log_result[[year_index]] <- max(log_value[rows]) + log(sum(exp(shifted)) / scale)
  }
  list(value = log_result, gradient = result)
}

gradient <- read_native_matrix(file.path(model_dir, "bet.dep"))
gradient_noeff <- read_native_matrix(file.path(model_dir, "bet.dp2"))
hessian <- read_native_hessian(file.path(model_dir, "bet.hes"))
labels <- read_labels(file.path(model_dir, "deplabel.tmp"))
labels_noeff <- read_labels(file.path(model_dir, "deplabel_noeff.tmp"))

n_parameter <- nrow(hessian)
if (
  ncol(gradient) != n_parameter || ncol(gradient_noeff) != n_parameter ||
  nrow(labels) != nrow(gradient) || nrow(labels_noeff) != nrow(gradient_noeff)
) {
  stop("Native Hessian, gradient and label dimensions do not agree.", call. = FALSE)
}

point_series <- read.csv(point_file, check.names = FALSE)
point_series <- point_series[point_series$ensemble_id == ensemble_id, , drop = FALSE]
point_series <- point_series[order(point_series$year), , drop = FALSE]
years <- sort(unique(point_series$year))
n_year <- length(years)
n_quarter <- n_year * 4L
if (n_year < 1L || nrow(point_series) != n_year) {
  stop("Point-estimate time series is incomplete for ", ensemble_id)
}

adult_rows <- indexed_rows(labels, "adult_rbio", n_quarter)
recruitment_rows <- indexed_rows(labels, "ln_abs_recr", n_quarter)
adult_noeff_rows <- indexed_rows(labels_noeff, "adult_rbio_noeff", n_quarter)

adult <- annual_log_gradient(
  labels$value[adult_rows], gradient[adult_rows, , drop = FALSE], "mean"
)
adult_noeff <- annual_log_gradient(
  labels_noeff$value[adult_noeff_rows],
  gradient_noeff[adult_noeff_rows, , drop = FALSE], "mean"
)
recruitment <- annual_log_gradient(
  labels$value[recruitment_rows], gradient[recruitment_rows, , drop = FALSE], "sum"
)
depletion <- annual_log_gradient(
  labels$value[adult_rows] - labels_noeff$value[adult_noeff_rows],
  gradient[adult_rows, , drop = FALSE] -
    gradient_noeff[adult_noeff_rows, , drop = FALSE],
  "mean"
)

direct_log_row <- function(label) {
  row <- match(label, labels$label)
  if (is.na(row)) stop("Missing native MFCL dependent variable: ", label)
  value <- labels$value[[row]]
  if (!is.finite(value) || value <= 0) stop("Invalid positive native value: ", label)
  list(value = log(value), gradient = gradient[row, , drop = FALSE] / value)
}

recent_sbmsy <- direct_log_row("average_SB/SBmsy(recent)")
recent_ffmsy <- direct_log_row("average_F/Fmsy(recent)")

central_log <- c(
  depletion$value,
  adult$value,
  adult_noeff$value,
  recruitment$value,
  recent_sbmsy$value,
  recent_ffmsy$value
)
derived_gradient <- rbind(
  depletion$gradient,
  adult$gradient,
  adult_noeff$gradient,
  recruitment$gradient,
  recent_sbmsy$gradient,
  recent_ffmsy$gradient
)
quantity <- c(
  rep("depletion", n_year),
  rep("spawning_potential", n_year),
  rep("spawning_potential_noeff", n_year),
  rep("recruitment", n_year),
  "sb_recent_sbmsy_native", "f_recent_fmsy_native"
)
derived_year <- c(rep(years, 4L), NA_integer_, NA_integer_)

chol_hessian <- tryCatch(chol(hessian), error = function(error) NULL)
if (is.null(chol_hessian)) {
  stop("The native Hessian is not positive definite for ", ensemble_id)
}

set.seed(20260806L + as.integer(sub("ensemble-", "", ensemble_id)))
standard_normal <- matrix(
  stats::rnorm(n_parameter * n_draws), nrow = n_parameter, ncol = n_draws
)
parameter_delta <- backsolve(chol_hessian, standard_normal)
derived_delta <- derived_gradient %*% parameter_delta
draw_matrix <- exp(central_log + derived_delta)

quantity_index <- split(seq_along(quantity), quantity)
annual_draws <- data.frame(
  ensemble_id = ensemble_id,
  draw = rep(seq_len(n_draws), each = n_year),
  year = rep(years, times = n_draws),
  depletion = as.vector(draw_matrix[quantity_index$depletion, , drop = FALSE]),
  spawning_potential = as.vector(
    draw_matrix[quantity_index$spawning_potential, , drop = FALSE] / 1e6
  ),
  spawning_potential_noeff = as.vector(
    draw_matrix[quantity_index$spawning_potential_noeff, , drop = FALSE] / 1e6
  ),
  recruitment = as.vector(
    draw_matrix[quantity_index$recruitment, , drop = FALSE] / 1e6
  ),
  stringsAsFactors = FALSE
)

draw_by_year <- split(annual_draws, annual_draws$draw)
management_draws <- do.call(rbind, lapply(draw_by_year, function(value) {
  recent_sb <- mean(value$spawning_potential[value$year %in% (max(years) - 3L):max(years)])
  recent_sb0 <- mean(value$spawning_potential_noeff[
    value$year %in% (max(years) - 10L):(max(years) - 1L)
  ])
  historical <- mean(value$depletion[value$year %in% 2012:2015])
  data.frame(
    sb_recent_sb0 = recent_sb / recent_sb0,
    historical_target_depletion = historical,
    recent_historical_target_ratio = (recent_sb / recent_sb0) / historical
  )
}))
management_draws$ensemble_id <- ensemble_id
management_draws$draw <- seq_len(n_draws)
management_draws$sb_recent_sbmsy_native <- as.vector(
  draw_matrix[quantity_index$sb_recent_sbmsy_native, ]
)
management_draws$f_recent_fmsy_native <- as.vector(
  draw_matrix[quantity_index$f_recent_fmsy_native, ]
)
management_draws <- management_draws[c(
  "ensemble_id", "draw", "sb_recent_sb0", "sb_recent_sbmsy_native",
  "f_recent_fmsy_native", "historical_target_depletion",
  "recent_historical_target_ratio"
)]

label_central <- data.frame(
  year = years,
  depletion = exp(depletion$value),
  spawning_potential = exp(adult$value) / 1e6,
  recruitment = exp(recruitment$value) / 1e6
)
relative_error <- c(
  abs(label_central$depletion - point_series$depletion) /
    pmax(abs(point_series$depletion), .Machine$double.eps),
  abs(label_central$spawning_potential - point_series$spawning_potential) /
    pmax(abs(point_series$spawning_potential), .Machine$double.eps),
  abs(label_central$recruitment - point_series$recruitment) /
    pmax(abs(point_series$recruitment), .Machine$double.eps)
)
if (max(relative_error) > 5e-4) {
  stop(
    sprintf(
      "Native gradients do not reproduce %s point estimates (max relative error %.3g).",
      ensemble_id, max(relative_error)
    )
  )
}

metadata <- data.frame(
  ensemble_id = ensemble_id,
  n_parameter = n_parameter,
  n_dependent = nrow(gradient),
  n_dependent_noeff = nrow(gradient_noeff),
  draws = n_draws,
  maximum_point_relative_error = max(relative_error),
  final_par_sha256 = sha256(file.path(model_dir, "final.par")),
  hessian_sha256 = sha256(file.path(model_dir, "bet.hes")),
  gradient_sha256 = sha256(file.path(model_dir, "bet.dep")),
  noeff_gradient_sha256 = sha256(file.path(model_dir, "bet.dp2")),
  method = paste(
    "joint native-MFCL Hessian draws propagated through dependent-variable",
    "gradients; no covariance regularisation"
  ),
  stringsAsFactors = FALSE
)

payload <- list(
  metadata = metadata,
  central = label_central,
  annual_draws = annual_draws,
  management_draws = management_draws
)
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
saveRDS(payload, output_file, version = 3, compress = "xz")
message(
  "Wrote ", ensemble_id, ": ", n_draws,
  " correlated native-Hessian draws; maximum point error ",
  format(max(relative_error), scientific = TRUE, digits = 3)
)
