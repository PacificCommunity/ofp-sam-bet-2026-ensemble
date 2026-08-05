#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(FLCore)
  library(FLR4MFCL)
  library(mfclshiny)
})

args <- commandArgs(trailingOnly = TRUE)
archive_dir <- if (length(args)) args[[1]] else Sys.getenv("ENSEMBLE_ARCHIVE_DIR")
if (!nzchar(archive_dir) || !dir.exists(archive_dir)) {
  stop("Provide the directory containing ensemble-###.tar.gz archives.", call. = FALSE)
}

design_file <- "design/model-draws.csv"
output_dir <- "data/ensemble"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
design <- utils::read.csv(design_file, check.names = FALSE)

decode_object <- function(payload, role) {
  object <- payload$object_cache$objects[[role]]
  if (is.null(object)) stop("Payload does not contain ", role, ".")
  bytes <- if (identical(object$compression, "none")) {
    object$bytes
  } else {
    memDecompress(object$bytes, type = object$compression)
  }
  unserialize(bytes)
}

annual_mean <- function(x) {
  data <- as.data.frame(x)
  names(data)[names(data) == "data"] <- "value"
  stats::aggregate(value ~ year, data = data, FUN = mean)
}

annual_stock_total <- function(x) {
  data <- as.data.frame(x)
  names(data)[names(data) == "data"] <- "value"
  year_season <- stats::aggregate(value ~ year + season, data = data, FUN = sum)
  annual <- stats::aggregate(value ~ year, data = year_season, FUN = mean)
  stats::setNames(annual$value, annual$year)
}

scalar <- function(x, default = NA_real_) {
  value <- suppressWarnings(as.numeric(x)[[1]])
  if (length(value) && is.finite(value)) value else default
}

safe_field <- function(x, name, default = NA) {
  value <- x[[name]]
  if (is.null(value) || !length(value)) default else value[[1]]
}

archive_files <- list.files(
  archive_dir,
  pattern = "^ensemble-[0-9]{3}\\.tar\\.gz$",
  full.names = TRUE
)
if (!length(archive_files)) stop("No ensemble archives were found.", call. = FALSE)

series_rows <- list()
endpoint_rows <- list()
diagnostic_rows <- list()
objective_rows <- list()
completed <- character()

extract_timeseries <- getFromNamespace(
  "mfclshiny_report_extract_rep_timeseries",
  "mfclshiny"
)

for (archive in sort(archive_files)) {
  ensemble_id <- sub("\\.tar\\.gz$", "", basename(archive))
  work <- tempfile(paste0(ensemble_id, "-"))
  dir.create(work)
  utils::untar(archive, exdir = work)
  payload_file <- list.files(
    work,
    pattern = "model_payload\\.rds$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(payload_file) != 1L) {
    warning(ensemble_id, ": expected one model_payload.rds; skipped.")
    next
  }

  payload <- readRDS(payload_file[[1]])
  rep <- decode_object(payload, "RepOut")
  par <- decode_object(payload, "ParOut")
  draw <- design[design$ensemble_id == ensemble_id, , drop = FALSE]
  if (nrow(draw) != 1L) stop("Design row missing for ", ensemble_id, ".")

  tau_actual <- 1 + exp(as.numeric(fish_params(par)[4, 1]))
  if (!isTRUE(all.equal(tau_actual, draw$tag_tau, tolerance = 5e-7))) {
    stop(ensemble_id, ": fitted tau does not match the design.")
  }
  if (as.integer(flagval(par, 1, 305)$value) != 1L) {
    stop(ensemble_id, ": parest flag 305 is not the native negative-binomial option.")
  }

  ts <- extract_timeseries(rep)
  sbmsy <- annual_mean(slot(rep, "ABBMSY_ts"))
  fmsy <- annual_mean(slot(rep, "FFMSY_ts"))
  ts <- merge(ts, sbmsy, by = "year", all.x = TRUE, sort = FALSE)
  names(ts)[names(ts) == "value"] <- "sb_sbmsy"
  ts <- merge(ts, fmsy, by = "year", all.x = TRUE, sort = FALSE)
  names(ts)[names(ts) == "value"] <- "f_fmsy"
  ts$ensemble_id <- ensemble_id
  ts <- ts[, c(
    "ensemble_id", "year", "depletion", "spawning_potential",
    "spawning_potential_nofish", "recruitment", "fishing_mortality",
    "sb_sbmsy", "f_fmsy"
  )]
  series_rows[[ensemble_id]] <- ts

  terminal_year <- max(ts$year, na.rm = TRUE)
  sb <- annual_stock_total(slot(rep, "adultBiomass"))
  sb0 <- annual_stock_total(slot(rep, "adultBiomass_nofish"))
  sb_recent_years <- seq.int(terminal_year - 3L, terminal_year)
  sb0_recent_years <- seq.int(terminal_year - 10L, terminal_year - 1L)
  f_recent_years <- seq.int(terminal_year - 4L, terminal_year - 1L)
  historical_target_years <- 2012:2015
  sb_recent <- mean(sb[as.character(sb_recent_years)], na.rm = TRUE)
  sb0_recent <- mean(sb0[as.character(sb0_recent_years)], na.rm = TRUE)
  bmsy <- scalar(slot(rep, "BMSY"))
  fmult <- scalar(slot(rep, "Fmult"))
  historical_target <- mean(
    ts$depletion[match(historical_target_years, ts$year)]
  )
  if (!is.finite(historical_target)) {
    stop(ensemble_id, ": incomplete 2012--2015 depletion history.")
  }
  recent_depletion <- sb_recent / sb0_recent
  endpoint_rows[[ensemble_id]] <- data.frame(
    ensemble_id = ensemble_id,
    terminal_year = terminal_year,
    sb_recent_period = paste(range(sb_recent_years), collapse = "–"),
    sb0_recent_period = paste(range(sb0_recent_years), collapse = "–"),
    f_recent_period = paste(range(f_recent_years), collapse = "–"),
    sb_recent_kt = sb_recent / 1000,
    sb0_recent_kt = sb0_recent / 1000,
    sb_recent_sb0 = recent_depletion,
    sb_recent_sbmsy = sb_recent / bmsy,
    f_recent_fmsy = 1 / fmult,
    historical_target_period = paste(range(historical_target_years), collapse = "–"),
    historical_target_depletion = historical_target,
    recent_historical_target_ratio = recent_depletion / historical_target,
    below_lrp_020 = recent_depletion < 0.20,
    below_sbmsy = sb_recent / bmsy < 1,
    above_fmsy = 1 / fmult > 1,
    stringsAsFactors = FALSE
  )

  hessian <- payload$data$info$hessian
  diagnostic_rows[[ensemble_id]] <- data.frame(
    ensemble_id = ensemble_id,
    objective_function = scalar(payload$obj_fun),
    maximum_gradient = scalar(payload$max_grad),
    hessian_status = as.character(safe_field(hessian, "hessian_status", "Not available")),
    positive_definite_hessian = isTRUE(safe_field(hessian, "is_pdh", FALSE)),
    hessian_reliability = as.character(safe_field(hessian, "reliability", NA_character_)),
    negative_eigenvalues = as.integer(safe_field(hessian, "n_negative_eigenvalues", NA_integer_)),
    nonpositive_eigenvalues = as.integer(safe_field(hessian, "n_nonpositive_eigenvalues", NA_integer_)),
    smallest_eigenvalue = as.numeric(safe_field(hessian, "smallest_eigenvalue", NA_real_)),
    dominant_uncertainty_block = as.character(safe_field(hessian, "dominant_parameter_block", NA_character_)),
    tau = tau_actual,
    stringsAsFactors = FALSE
  )

  components <- payload$data$Diagnostics$objective_components
  if (is.data.frame(components) && all(c("Component", "Value") %in% names(components))) {
    components$ensemble_id <- ensemble_id
    objective_rows[[ensemble_id]] <- components[, c("ensemble_id", "Component", "Value")]
  }
  completed <- c(completed, ensemble_id)
}

timeseries <- do.call(rbind, series_rows)
endpoints <- do.call(rbind, endpoint_rows)
diagnostics <- do.call(rbind, diagnostic_rows)
objectives <- do.call(rbind, objective_rows)
successful_design <- design[match(completed, design$ensemble_id), ]

row.names(timeseries) <- NULL
row.names(endpoints) <- NULL
row.names(diagnostics) <- NULL
row.names(objectives) <- NULL
row.names(successful_design) <- NULL

utils::write.csv(timeseries, file.path(output_dir, "ensemble-timeseries.csv"), row.names = FALSE)
saveRDS(timeseries, file.path(output_dir, "ensemble-timeseries.rds"), compress = "xz", version = 3)
utils::write.csv(endpoints, file.path(output_dir, "management-quantities.csv"), row.names = FALSE)
utils::write.csv(diagnostics, file.path(output_dir, "fit-diagnostics.csv"), row.names = FALSE)
utils::write.csv(objectives, file.path(output_dir, "objective-components.csv"), row.names = FALSE)
utils::write.csv(successful_design, file.path(output_dir, "successful-model-design.csv"), row.names = FALSE)
utils::write.csv(
  data.frame(
    requested_models = nrow(design),
    completed_models = length(completed),
    terminal_year = max(timeseries$year),
    generated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    stringsAsFactors = FALSE
  ),
  file.path(output_dir, "data-summary.csv"),
  row.names = FALSE
)

files <- list.files(output_dir, full.names = TRUE)
files <- files[basename(files) != "SHA256SUMS"]
hashes <- vapply(files, function(path) {
  output <- system2("sha256sum", path, stdout = TRUE)
  strsplit(output[[1]], "[[:space:]]+")[[1]][[1]]
}, character(1))
writeLines(
  paste(hashes, basename(files)),
  file.path(output_dir, "SHA256SUMS")
)

cat(sprintf(
  "Built public ensemble payload: %d completed models, %d annual rows, %d Hessian audits.\n",
  length(completed), nrow(timeseries), nrow(diagnostics)
))
