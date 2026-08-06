#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 6L) {
  stop(
    paste(
      "Usage: process-equilibrium-hessian-uncertainty.R MODEL_DIR",
      "ENSEMBLE_ID POINT_MANAGEMENT_CSV BASE_MODEL_RDS OUTPUT_RDS DRAW_COUNT"
    ),
    call. = FALSE
  )
}

model_dir <- normalizePath(args[[1L]], mustWork = TRUE)
ensemble_id <- args[[2L]]
point_file <- normalizePath(args[[3L]], mustWork = TRUE)
base_file <- normalizePath(args[[4L]], mustWork = TRUE)
output_file <- args[[5L]]
n_draws <- as.integer(args[[6L]])
if (!grepl("^ensemble-[0-9]{3}$", ensemble_id) ||
    !is.finite(n_draws) || n_draws < 1L) {
  stop("Invalid model identifier or draw count.", call. = FALSE)
}

sha256 <- function(path) {
  value <- system2("sha256sum", path, stdout = TRUE, stderr = TRUE)
  if (!length(value)) stop("Could not hash ", path, ".", call. = FALSE)
  strsplit(value[[1L]], "[[:space:]]+")[[1L]][[1L]]
}

read_native_matrix <- function(path) {
  connection <- file(path, "rb")
  on.exit(close(connection), add = TRUE)
  n_parameter <- readBin(connection, integer(), n = 1L, size = 4L, endian = "little")
  n_derived <- readBin(connection, integer(), n = 1L, size = 4L, endian = "little")
  split_bounds <- readBin(connection, integer(), n = 2L, size = 4L, endian = "little")
  if (length(split_bounds) != 2L || split_bounds[[1L]] <= 0L ||
      split_bounds[[2L]] < split_bounds[[1L]]) {
    stop("Missing dependent-variable split bounds.", call. = FALSE)
  }
  values <- readBin(
    connection, numeric(), n = n_parameter * n_derived,
    size = 8L, endian = "little"
  )
  complete_rows <- length(values) %/% n_parameter
  if (complete_rows < 1L) {
    stop("Incomplete dependent-variable gradient matrix.", call. = FALSE)
  }
  # The program opens the fished gradient stream before its optional no-fishing
  # pass.  If that later pass terminates, the final unused buffer row can be
  # incomplete even though all equilibrium rows were written.  Retain complete
  # rows only; the required curve rows are checked explicitly below.
  values <- values[seq_len(complete_rows * n_parameter)]
  matrix(values, nrow = complete_rows, ncol = n_parameter, byrow = TRUE)
}

read_native_hessian <- function(path) {
  connection <- file(path, "rb")
  on.exit(close(connection), add = TRUE)
  n_parameter <- readBin(connection, integer(), n = 1L, size = 4L, endian = "little")
  bounds <- readBin(connection, integer(), n = 2L, size = 4L, endian = "little")
  if (!identical(bounds, c(1L, n_parameter))) {
    stop("Unexpected Hessian matrix bounds.", call. = FALSE)
  }
  values <- readBin(
    connection, numeric(), n = n_parameter * n_parameter,
    size = 8L, endian = "little"
  )
  if (length(values) != n_parameter * n_parameter) {
    stop("Incomplete Hessian matrix.", call. = FALSE)
  }
  value <- matrix(values, nrow = n_parameter, ncol = n_parameter, byrow = TRUE)
  (value + t(value)) / 2
}

read_labels <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (length(lines) %% 2L != 0L) stop("Incomplete dependent-variable labels.")
  data.frame(
    value = as.numeric(lines[seq.int(1L, length(lines), by = 2L)]),
    label = lines[seq.int(2L, length(lines), by = 2L)],
    stringsAsFactors = FALSE
  )
}

gradient <- read_native_matrix(file.path(model_dir, "bet.dep"))
hessian <- read_native_hessian(file.path(model_dir, "bet.hes"))
labels <- read_labels(file.path(model_dir, "deplabel.tmp"))
if (nrow(labels) < nrow(gradient) || ncol(gradient) != nrow(hessian)) {
  stop("Hessian, gradient and label dimensions do not agree.", call. = FALSE)
}
labels <- labels[seq_len(nrow(gradient)), , drop = FALSE]

yield_rows <- grep("^pred_yield_for_F_mult", labels$label)
sb_rows <- grep("^pred_equilib_SB_for_F_mult", labels$label)
f_multiplier <- seq(0, 3.62, by = 0.01)
if (length(yield_rows) != length(f_multiplier) ||
    length(sb_rows) != length(f_multiplier) ||
    !identical(yield_rows, seq.int(min(yield_rows), max(yield_rows))) ||
    !identical(sb_rows, seq.int(min(sb_rows), max(sb_rows)))) {
  stop("The full equilibrium-yield or spawning-biomass curve is missing.")
}

central_yield <- labels$value[yield_rows]
central_sb <- labels$value[sb_rows]
if (central_yield[[1L]] != 0 || any(central_yield[-1L] <= 0) ||
    any(central_sb <= 0)) {
  stop("Invalid central equilibrium curves.", call. = FALSE)
}

chol_hessian <- tryCatch(chol(hessian), error = function(error) NULL)
if (is.null(chol_hessian)) stop("The model Hessian is not positive definite.")
set.seed(20260806L + as.integer(sub("ensemble-", "", ensemble_id)))
standard_normal <- matrix(
  stats::rnorm(nrow(hessian) * n_draws), nrow = nrow(hessian), ncol = n_draws
)
parameter_delta <- backsolve(chol_hessian, standard_normal)

quadratic_maximum <- function(y) {
  index <- which.max(y)
  if (index <= 1L || index >= length(y)) return(NA_real_)
  denominator <- y[index - 1L] - 2 * y[index] + y[index + 1L]
  if (!is.finite(denominator) || denominator >= 0) return(f_multiplier[[index]])
  offset <- 0.5 * (y[index - 1L] - y[index + 1L]) / denominator
  f_multiplier[[index]] + max(-1, min(1, offset)) * 0.01
}

central_fmult <- quadratic_maximum(central_yield)
central_index <- which.max(central_yield)
step <- f_multiplier[[2L]] - f_multiplier[[1L]]
yield_curvature <- (
  central_yield[central_index - 1L] - 2 * central_yield[central_index] +
    central_yield[central_index + 1L]
) / step^2
yield_parameter_slope <- (
  gradient[yield_rows[central_index + 1L], , drop = FALSE] -
    gradient[yield_rows[central_index - 1L], , drop = FALSE]
) / (2 * step)
fmult_gradient <- as.numeric(-yield_parameter_slope / yield_curvature)
if (!is.finite(central_fmult) || central_fmult <= 0 ||
    !is.finite(yield_curvature) || yield_curvature >= 0 ||
    any(!is.finite(fmult_gradient))) {
  stop("The equilibrium-yield optimum cannot be differentiated.")
}

# The optimum satisfies dY/dm=0.  Implicit differentiation gives
# dm/dtheta = -(d2Y/dm dtheta)/(d2Y/dm2).  This avoids independently drawing
# hundreds of curve points and then selecting spurious boundary maxima.
fmult_draw <- as.vector(exp(
  log(central_fmult) + (fmult_gradient / central_fmult) %*% parameter_delta
))
central_sbmsy <- stats::approx(
  f_multiplier, central_sb, xout = central_fmult
)$y
lower_index <- max(which(f_multiplier <= central_fmult))
upper_index <- min(which(f_multiplier >= central_fmult))
weight_upper <- if (lower_index == upper_index) 0 else
  (central_fmult - f_multiplier[[lower_index]]) /
    (f_multiplier[[upper_index]] - f_multiplier[[lower_index]])
sb_partial_gradient <-
  (1 - weight_upper) * gradient[sb_rows[lower_index], ] +
  weight_upper * gradient[sb_rows[upper_index], ]
sb_multiplier_slope <- (
  central_sb[central_index + 1L] - central_sb[central_index - 1L]
) / (2 * step)
sbmsy_gradient <- sb_partial_gradient + sb_multiplier_slope * fmult_gradient
sbmsy_draw <- as.vector(exp(
  log(central_sbmsy) + (sbmsy_gradient / central_sbmsy) %*% parameter_delta
))
if (any(!is.finite(sbmsy_draw)) || any(sbmsy_draw <= 0) || central_sbmsy <= 0) {
  stop("Invalid spawning biomass at MSY.", call. = FALSE)
}

base <- readRDS(base_file)
if (!identical(base$metadata$ensemble_id[[1L]], ensemble_id) ||
    base$metadata$draws[[1L]] != n_draws ||
    base$metadata$hessian_sha256[[1L]] != sha256(file.path(model_dir, "bet.hes"))) {
  stop("The base Hessian payload does not match this model.", call. = FALSE)
}
recent <- stats::aggregate(
  spawning_potential ~ draw,
  base$annual_draws[base$annual_draws$year %in% 2021:2024, ], mean
)
recent <- recent[match(seq_len(n_draws), recent$draw), ]
central_recent <- mean(
  base$central$spawning_potential[base$central$year %in% 2021:2024]
) * 1000

point <- read.csv(point_file, check.names = FALSE)
point <- point[point$ensemble_id == ensemble_id, , drop = FALSE]
if (nrow(point) != 1L) stop("Missing central management quantities.")
central_f_fmsy <- 1 / central_fmult
central_sb_sbmsy <- point$sb_recent_sbmsy[[1L]]
central_error <- c(
  f_recent_fmsy = abs(central_f_fmsy - point$f_recent_fmsy[[1L]]) /
    point$f_recent_fmsy[[1L]],
  sb_recent_sbmsy = abs(central_recent / central_sbmsy - central_sb_sbmsy) /
    central_sb_sbmsy
)
if (central_error[["f_recent_fmsy"]] > 5e-4 ||
    central_error[["sb_recent_sbmsy"]] > 5e-3) {
  stop(
    "Equilibrium curves do not reproduce the official central quantities: ",
    paste(names(central_error), signif(central_error, 4), collapse = "; ")
  )
}

# Retain the exact official central recent biomass scale while using the joint
# annual Hessian draws for its relative parameter variation.
official_recent_sb <- central_sb_sbmsy * central_sbmsy
recent_sb_draw <- recent$spawning_potential * 1000 *
  (official_recent_sb / central_recent)
draws <- data.frame(
  ensemble_id = ensemble_id,
  draw = seq_len(n_draws),
  sb_recent_sbmsy = recent_sb_draw / sbmsy_draw,
  f_recent_fmsy = 1 / fmult_draw,
  f_multiplier_at_msy = fmult_draw,
  sbmsy_mt = sbmsy_draw,
  stringsAsFactors = FALSE
)
if (any(!is.finite(unlist(draws[-(1:2)]))) ||
    any(unlist(draws[-(1:2)]) <= 0)) {
  stop("Invalid exact management-quantity draws.", call. = FALSE)
}

metadata <- data.frame(
  ensemble_id = ensemble_id,
  draws = n_draws,
  n_parameter = nrow(hessian),
  equilibrium_dependent_variables = nrow(gradient),
  central_f_multiplier_at_msy = central_fmult,
  central_sbmsy_mt = central_sbmsy,
  central_f_recent_fmsy_relative_error = central_error[["f_recent_fmsy"]],
  central_sb_recent_sbmsy_relative_error = central_error[["sb_recent_sbmsy"]],
  minimum_draw_f_multiplier_at_msy = min(fmult_draw),
  maximum_draw_f_multiplier_at_msy = max(fmult_draw),
  equilibrium_gradient_sha256 = sha256(file.path(model_dir, "bet.dep")),
  method = paste(
    "joint Hessian draws propagated through the equilibrium-yield optimum",
    "and spawning biomass at MSY by implicit differentiation"
  ),
  stringsAsFactors = FALSE
)
payload <- list(
  metadata = metadata,
  central = data.frame(
    ensemble_id = ensemble_id,
    sb_recent_sbmsy = point$sb_recent_sbmsy[[1L]],
    f_recent_fmsy = point$f_recent_fmsy[[1L]],
    f_multiplier_at_msy = central_fmult,
    sbmsy_mt = central_sbmsy
  ),
  draws = draws
)
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
saveRDS(payload, output_file, version = 3L, compress = "xz")
message(
  "Wrote exact MSY-based Hessian draws for ", ensemble_id,
  "; central relative errors ",
  paste(names(central_error), format(central_error, scientific = TRUE), collapse = ", ")
)
