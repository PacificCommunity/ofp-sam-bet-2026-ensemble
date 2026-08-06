#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4L) {
  stop(
    paste(
      "Usage: parse-native-projection.R WORK_DIR ENSEMBLE_ID",
      "CACHE_KEY OUTPUT_RDS"
    ),
    call. = FALSE
  )
}

work_dir <- normalizePath(args[[1L]], mustWork = TRUE)
ensemble_id <- args[[2L]]
cache_key <- args[[3L]]
output_file <- args[[4L]]
if (!grepl("^ensemble-[0-9]{3}$", ensemble_id)) {
  stop("Invalid ensemble ID.", call. = FALSE)
}
if (!grepl("^[0-9a-f]{64}$", cache_key)) {
  stop("Invalid projection cache key.", call. = FALSE)
}

required <- c(
  "projected_spawning_biomass",
  "projected_spawning_biomass_noeff",
  "Fmults.txt",
  "projection-catch-msy.csv",
  "historical-regional-spawning-biomass.csv",
  "projection-audit.csv",
  "projection-conditioning.csv",
  "native-flag-audit.csv",
  "recruitment-sampling-window.csv",
  "annual-projection-catch-audit.csv",
  "native-projection-SHA256SUMS",
  "projection-input-SHA256SUMS",
  "projection-scenario.txt"
)
missing <- required[!file.exists(file.path(work_dir, required))]
if (length(missing)) {
  stop("Missing native projection output: ", missing[[1L]], call. = FALSE)
}

sha256 <- function(path) {
  value <- system2("sha256sum", path, stdout = TRUE, stderr = TRUE)
  if (!length(value)) stop("Could not hash ", path, call. = FALSE)
  strsplit(value[[1L]], "[[:space:]]+")[[1L]][[1L]]
}

read_audit_value <- function(audit, field) {
  value <- audit$value[match(field, audit$field)]
  if (length(value) != 1L || is.na(value)) {
    stop("Projection audit does not contain ", field, ".", call. = FALSE)
  }
  value
}

read_spawning_biomass <- function(path, last_year, expected_simulations) {
  lines <- readLines(path, warn = FALSE)
  simulation_headers <- grep("^# Simulation number [0-9]+", lines)
  if (length(simulation_headers) != expected_simulations) {
    stop("Unexpected simulation count in ", basename(path), ".", call. = FALSE)
  }
  numeric_lines <- lines[!grepl("^#", lines) & nzchar(trimws(lines))]
  values <- utils::read.table(
    text = numeric_lines,
    header = FALSE,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (!ncol(values) || nrow(values) %% expected_simulations != 0L) {
    stop("Malformed native spawning-biomass output: ", path, call. = FALSE)
  }
  periods_per_simulation <- nrow(values) %/% expected_simulations
  if (periods_per_simulation %% 4L != 0L) {
    stop("Native spawning biomass is not quarterly.", call. = FALSE)
  }
  first_year <- last_year - periods_per_simulation %/% 4L + 1L
  simulation <- rep(seq_len(expected_simulations), each = periods_per_simulation)
  period <- rep(seq_len(periods_per_simulation), times = expected_simulations)
  year <- first_year + (period - 1L) %/% 4L
  quarter <- (period - 1L) %% 4L + 1L
  region_names <- paste0("region_", seq_len(ncol(values)))
  names(values) <- region_names
  wide <- data.frame(
    simulation = simulation,
    year = year,
    quarter = quarter,
    values,
    check.names = FALSE
  )
  long <- reshape(
    wide,
    varying = region_names,
    v.names = "spawning_biomass_mt",
    timevar = "region",
    times = seq_along(region_names),
    direction = "long"
  )
  rownames(long) <- NULL
  long <- long[order(long$simulation, long$year, long$quarter, long$region), ]
  long$id <- NULL
  long
}

audit <- read.csv(file.path(work_dir, "projection-audit.csv"), check.names = FALSE)
terminal_year <- as.integer(read_audit_value(audit, "terminal_year"))
first_projection_year <- as.integer(read_audit_value(audit, "first_projection_year"))
last_projection_year <- as.integer(read_audit_value(audit, "last_projection_year"))
nsims <- as.integer(read_audit_value(audit, "stochastic_replicates"))
seed <- as.integer(read_audit_value(audit, "recruitment_seed"))
if (
  any(!is.finite(c(terminal_year, first_projection_year, last_projection_year,
                   nsims, seed))) ||
    first_projection_year != terminal_year + 1L ||
    last_projection_year - first_projection_year + 1L != 30L ||
    nsims != 10L
) {
  stop("Projection horizon or simulation count differs from the locked scenario.")
}

fished <- read_spawning_biomass(
  file.path(work_dir, "projected_spawning_biomass"),
  last_projection_year,
  nsims
)
noeff <- read_spawning_biomass(
  file.path(work_dir, "projected_spawning_biomass_noeff"),
  last_projection_year,
  nsims
)
key <- c("simulation", "year", "quarter", "region")
if (!identical(fished[key], noeff[key])) {
  stop("Fished and no-fishing native projection calendars differ.", call. = FALSE)
}
names(noeff)[names(noeff) == "spawning_biomass_mt"] <-
  "spawning_biomass_noeff_mt"
quarterly_region <- merge(fished, noeff, by = key, sort = TRUE)
quarterly_region$depletion <- with(
  quarterly_region,
  spawning_biomass_mt / spawning_biomass_noeff_mt
)
if (any(!is.finite(quarterly_region$depletion)) ||
    any(quarterly_region$spawning_biomass_noeff_mt <= 0)) {
  stop("Invalid native projected spawning depletion.", call. = FALSE)
}

annual_region <- aggregate(
  cbind(spawning_biomass_mt, spawning_biomass_noeff_mt) ~
    simulation + year + region,
  quarterly_region,
  mean
)
annual_region$depletion <- with(
  annual_region,
  spawning_biomass_mt / spawning_biomass_noeff_mt
)

quarterly_stock <- aggregate(
  cbind(spawning_biomass_mt, spawning_biomass_noeff_mt) ~
    simulation + year + quarter,
  quarterly_region,
  sum
)
annual_stock <- aggregate(
  cbind(spawning_biomass_mt, spawning_biomass_noeff_mt) ~ simulation + year,
  quarterly_stock,
  mean
)
annual_stock$depletion <- with(
  annual_stock,
  spawning_biomass_mt / spawning_biomass_noeff_mt
)

projection_years <- first_projection_year:last_projection_year
annual_region <- annual_region[annual_region$year %in% projection_years, ]
annual_stock <- annual_stock[annual_stock$year %in% projection_years, ]
quarterly_region <- quarterly_region[quarterly_region$year %in% projection_years, ]
if (
  nrow(annual_stock) != nsims * length(projection_years) ||
    nrow(annual_region) != nsims * length(projection_years) * 5L ||
    nrow(quarterly_region) != nsims * length(projection_years) * 4L * 5L
) {
  stop("The compact native projection calendar is incomplete.", call. = FALSE)
}

fmult_lines <- readLines(file.path(work_dir, "Fmults.txt"), warn = FALSE)
fmult_headers <- grep("^#simulation [0-9]+", fmult_lines, value = TRUE)
fmult_values <- fmult_lines[!grepl("^#", fmult_lines) & nzchar(trimws(fmult_lines))]
if (length(fmult_headers) != nsims || length(fmult_values) != nsims) {
  stop("Malformed Fmults.txt.", call. = FALSE)
}
simulation <- as.integer(sub("^#simulation ([0-9]+).*$", "\\1", fmult_headers))
fmult_matrix <- utils::read.table(text = fmult_values, header = FALSE)
if (ncol(fmult_matrix) != 4L || !identical(simulation, seq_len(nsims))) {
  stop("Unexpected Fmults.txt fields.", call. = FALSE)
}
names(fmult_matrix) <- c(
  "f_multiplier_at_msy", "terminal_sb_sbmsy",
  "terminal_spawning_biomass_mt", "sbmsy_mt"
)
terminal_msy <- data.frame(simulation = simulation, fmult_matrix)
terminal_msy$terminal_f_fmsy <- 1 / terminal_msy$f_multiplier_at_msy
if (any(!is.finite(unlist(terminal_msy))) ||
    any(terminal_msy$f_multiplier_at_msy <= 0) ||
    any(terminal_msy$sbmsy_mt <= 0)) {
  stop("Invalid native terminal MSY output.", call. = FALSE)
}

catch_msy <- read.csv(
  file.path(work_dir, "projection-catch-msy.csv"), check.names = FALSE
)
if (
  !identical(names(catch_msy), c(
    "simulation", "year", "catch_biomass_mt", "annual_msy_mt", "catch_msy"
  )) ||
    nrow(catch_msy) != nsims * length(projection_years) ||
    !identical(sort(unique(catch_msy$simulation)), seq_len(nsims)) ||
    !identical(sort(unique(catch_msy$year)), projection_years) ||
    any(!is.finite(unlist(catch_msy))) || any(catch_msy$annual_msy_mt <= 0) ||
    any(catch_msy$catch_biomass_mt < 0) || any(catch_msy$catch_msy < 0)
) {
  stop("Invalid model-derived annual Catch/MSY output.", call. = FALSE)
}

historical_region <- read.csv(
  file.path(work_dir, "historical-regional-spawning-biomass.csv"),
  check.names = FALSE
)
if (
  !identical(names(historical_region), c(
    "year", "region", "spawning_biomass_mt"
  )) || nrow(historical_region) != (terminal_year - 1952L + 1L) * 5L ||
    !identical(sort(unique(historical_region$year)), 1952:terminal_year) ||
    !identical(sort(unique(historical_region$region)), 1:5) ||
    any(!is.finite(unlist(historical_region))) ||
    any(historical_region$spawning_biomass_mt <= 0)
) {
  stop("Invalid historical regional spawning-biomass output.", call. = FALSE)
}

conditioning <- read.csv(
  file.path(work_dir, "projection-conditioning.csv"), check.names = FALSE
)
if (nrow(conditioning) != 33L || any(conditioning$caeff != 1L) ||
    any(conditioning$conditioning != "catch")) {
  stop("The locked scenario is not catch-conditioned for all 33 fisheries.")
}
catch_audit <- read.csv(
  file.path(work_dir, "annual-projection-catch-audit.csv"), check.names = FALSE
)
if (nrow(catch_audit) != 30L ||
    length(unique(round(catch_audit$catch, 8L))) != 1L) {
  stop("The projection catch is not constant across all 30 years.")
}
flag_audit <- read.csv(
  file.path(work_dir, "native-flag-audit.csv"), check.names = FALSE
)
tau <- as.numeric(flag_audit$value[
  flag_audit$flag_type == "derived" & is.na(flag_audit$flag)
])
if (length(tau) != 1L || !any(abs(tau - c(1.2, 1.3, 1.4)) < 1e-8)) {
  stop("The fixed ensemble tau audit is invalid.")
}

raw_hashes <- data.frame(
  file = required,
  sha256 = vapply(file.path(work_dir, required), sha256, character(1L)),
  stringsAsFactors = FALSE
)
metadata <- data.frame(
  ensemble_id = ensemble_id,
  cache_key = cache_key,
  terminal_year = terminal_year,
  first_projection_year = first_projection_year,
  last_projection_year = last_projection_year,
  simulations = nsims,
  recruitment_seed = seed,
  recruitment_sampling_start = read.csv(
    file.path(work_dir, "recruitment-sampling-window.csv"),
    check.names = FALSE
  )$calendar_year_start[[1L]],
  recruitment_sampling_end = read.csv(
    file.path(work_dir, "recruitment-sampling-window.csv"),
    check.names = FALSE
  )$calendar_year_end[[1L]],
  recent_catch_years = read_audit_value(audit, "recent_catch_years"),
  catch_conditioned_fisheries = nrow(conditioning),
  constant_annual_catch = catch_audit$catch[[1L]],
  tau = tau,
  stringsAsFactors = FALSE
)

payload <- list(
  schema_version = "1.0.0",
  created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  method = paste(
    "MFCL stochastic projection; all fisheries held at their exact",
    "2022-2024 mean catch; recruitment sampled from fitted 1972-2023",
    "deviates; 10 simulations over 2025-2054. Annual biomass is the mean",
    "of four quarterly stock-wide or regional values; annual depletion is",
    "the ratio of annual mean fished to annual mean no-fishing spawning biomass."
  ),
  metadata = metadata,
  annual_stock = annual_stock,
  annual_region = annual_region,
  quarterly_region = quarterly_region,
  terminal_msy = terminal_msy,
  catch_msy = catch_msy,
  historical_region = historical_region,
  conditioning = conditioning,
  annual_catch = catch_audit,
  flag_audit = flag_audit,
  recruitment_sampling_window = read.csv(
    file.path(work_dir, "recruitment-sampling-window.csv"),
    check.names = FALSE
  ),
  input_hash_manifest = readLines(
    file.path(work_dir, "projection-input-SHA256SUMS"), warn = FALSE
  ),
  scenario = readLines(
    file.path(work_dir, "projection-scenario.txt"), warn = FALSE
  ),
  raw_output_hashes = raw_hashes
)

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
saveRDS(payload, output_file, version = 3L, compress = "xz")
cat(sprintf(
  "Stored compact native projection for %s: %d simulations x %d years.\n",
  ensemble_id, nsims, length(projection_years)
))
