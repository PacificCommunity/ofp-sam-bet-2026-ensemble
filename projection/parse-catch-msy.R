#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop(
    "Usage: parse-catch-msy.R WORK_DIR N_SIMULATIONS OUTPUT_CSV",
    call. = FALSE
  )
}

work_dir <- normalizePath(args[[1L]], mustWork = TRUE)
n_simulations <- as.integer(args[[2L]])
output_file <- args[[3L]]
capture_dir <- file.path(work_dir, "projection-diagnostics")
if (!is.finite(n_simulations) || n_simulations < 1L || !dir.exists(capture_dir)) {
  stop("Invalid projection-diagnostic capture.", call. = FALSE)
}

read_section <- function(path, label) {
  lines <- readLines(path, warn = FALSE)
  header <- match(label, trimws(lines))
  if (is.na(header)) {
    stop("Missing ", label, " in ", basename(path), ".", call. = FALSE)
  }
  following <- lines[seq.int(header + 1L, length(lines))]
  following <- following[nzchar(trimws(following))]
  following <- following[!grepl("^#", trimws(following))]
  if (!length(following)) {
    stop("Empty ", label, " in ", basename(path), ".", call. = FALSE)
  }
  scan(text = following[[1L]], what = numeric(), quiet = TRUE)
}

audit <- read.csv(file.path(work_dir, "projection-audit.csv"), check.names = FALSE)
audit_value <- function(field) {
  value <- audit$value[match(field, audit$field)]
  if (length(value) != 1L || is.na(value)) {
    stop("Projection audit does not contain ", field, ".", call. = FALSE)
  }
  as.integer(value)
}
last_projection_year <- audit_value("last_projection_year")
first_projection_year <- audit_value("first_projection_year")
terminal_year <- audit_value("terminal_year")

pieces <- lapply(seq_len(n_simulations), function(simulation) {
  suffix <- sprintf("%02d", simulation)
  catch_path <- file.path(capture_dir, paste0("catch-simulation-", suffix, ".rep"))
  report_path <- file.path(capture_dir, paste0("plot-simulation-", suffix, ".rep"))
  if (!file.exists(catch_path) || !file.exists(report_path)) {
    stop("Missing captured output for simulation ", simulation, ".", call. = FALSE)
  }

  quarterly_catch <- read_section(catch_path, "# Total catch by year")
  model_period_msy <- read_section(report_path, "# MSY")
  if (length(model_period_msy) != 1L || !is.finite(model_period_msy) ||
      model_period_msy <= 0 ||
      length(quarterly_catch) %% 4L != 0L ||
      any(!is.finite(quarterly_catch)) || any(quarterly_catch < 0)) {
    stop("Invalid catch or MSY output for simulation ", simulation, ".", call. = FALSE)
  }

  n_year <- length(quarterly_catch) %/% 4L
  first_year <- last_projection_year - n_year + 1L
  annual_catch <- rowSums(matrix(quarterly_catch, ncol = 4L, byrow = TRUE))
  annual_msy <- 4 * model_period_msy
  value <- data.frame(
    simulation = simulation,
    year = seq.int(first_year, last_projection_year),
    catch_biomass_mt = annual_catch,
    annual_msy_mt = rep(annual_msy, n_year),
    catch_msy = annual_catch / annual_msy,
    stringsAsFactors = FALSE
  )
  value[value$year >= first_projection_year, , drop = FALSE]
})

result <- do.call(rbind, pieces)
expected_rows <- n_simulations * (last_projection_year - first_projection_year + 1L)
if (nrow(result) != expected_rows || any(!is.finite(result$catch_msy))) {
  stop("Incomplete annual Catch/MSY projection output.", call. = FALSE)
}
write.csv(result, output_file, row.names = FALSE)

read_regional_adult_biomass <- function(path) {
  lines <- readLines(path, warn = FALSE)
  main_header <- match(
    "# Absolute biomass by region (across) and year (down)", trimws(lines)
  )
  if (is.na(main_header)) stop("Missing regional biomass report section.")
  adult_candidates <- which(trimws(lines) == "# Adult biomass")
  adult_header <- adult_candidates[adult_candidates > main_header][[1L]]
  following_headers <- which(
    seq_along(lines) > adult_header & grepl("^#", trimws(lines))
  )
  if (!length(following_headers)) stop("Unterminated regional adult-biomass section.")
  numeric_lines <- lines[seq.int(adult_header + 1L, following_headers[[1L]] - 1L)]
  numeric_lines <- numeric_lines[nzchar(trimws(numeric_lines))]
  values <- utils::read.table(
    text = numeric_lines, header = FALSE, check.names = FALSE
  )
  if (ncol(values) != 5L || nrow(values) %% 4L != 0L ||
      any(!is.finite(unlist(values)))) {
    stop("Invalid regional adult-biomass section.", call. = FALSE)
  }
  n_year <- nrow(values) %/% 4L
  first_year <- last_projection_year - n_year + 1L
  array_value <- array(
    unlist(values), dim = c(4L, n_year, 5L),
    dimnames = list(NULL, seq.int(first_year, last_projection_year), seq_len(5L))
  )
  annual <- apply(array_value, c(2L, 3L), mean)
  long <- as.data.frame(as.table(annual), stringsAsFactors = FALSE)
  names(long) <- c("year", "region", "spawning_biomass_mt")
  long$year <- as.integer(long$year)
  long$region <- as.integer(long$region)
  long[long$year <= terminal_year, , drop = FALSE]
}

historical_region <- read_regional_adult_biomass(
  file.path(capture_dir, "plot-simulation-01.rep")
)
write.csv(
  historical_region,
  file.path(work_dir, "historical-regional-spawning-biomass.csv"),
  row.names = FALSE
)
