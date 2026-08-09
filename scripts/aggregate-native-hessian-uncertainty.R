#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4L) {
  stop(
    paste(
      "Usage: aggregate-native-hessian-uncertainty.R PER_MODEL_DIR",
      "FIT_DIAGNOSTICS_CSV OUTPUT_RDS OUTPUT_METADATA_CSV"
    ),
    call. = FALSE
  )
}

input_dir <- normalizePath(args[[1L]], mustWork = TRUE)
fit_file <- normalizePath(args[[2L]], mustWork = TRUE)
output_file <- args[[3L]]
metadata_file <- args[[4L]]

fit <- read.csv(fit_file, check.names = FALSE)
pdh_ids <- sort(fit$ensemble_id[as.logical(fit$positive_definite_hessian)])
near_pdh_ids <- sort(setdiff(fit$ensemble_id, pdh_ids))
if (length(pdh_ids) < 1L || length(pdh_ids) + length(near_pdh_ids) != nrow(fit)) {
  stop("The fit diagnostic Hessian classification is incomplete.", call. = FALSE)
}

paths <- file.path(input_dir, paste0(pdh_ids, ".rds"))
if (any(!file.exists(paths))) {
  stop("Missing per-model payload: ", basename(paths[!file.exists(paths)][[1L]]))
}

per_model <- lapply(paths, readRDS)
metadata_rows <- lapply(per_model, `[[`, "metadata")
metadata_names <- unique(unlist(lapply(metadata_rows, names)))
metadata_rows <- lapply(metadata_rows, function(value) {
  missing_names <- setdiff(metadata_names, names(value))
  for (name in missing_names) value[[name]] <- NA
  value[metadata_names]
})
metadata <- do.call(rbind, metadata_rows)
if ("method" %in% names(metadata)) {
  metadata$method <- gsub(
    "native[- ]MFCL", "MFCL", metadata$method, ignore.case = TRUE
  )
}
annual_draws <- do.call(rbind, lapply(per_model, `[[`, "annual_draws"))
management_draws <- do.call(rbind, lapply(per_model, `[[`, "management_draws"))

# Payloads created before schema 1.1 stored the mean of quarterly depletion
# ratios.  The official annual definition is the ratio of annual mean biomass
# quantities.  Both joint biomass draws are retained in every payload, so the
# correction is exact and requires no Hessian rerun.
annual_draws$depletion <- with(
  annual_draws, spawning_potential / spawning_potential_noeff
)
# Schema 1.2 additionally separates the adopted rolling LRP biomass ratio from
# the CMM comparison of recent and 2012--2015 mean annual depletion.
draw_groups <- split(annual_draws, interaction(
  annual_draws$ensemble_id, annual_draws$draw, drop = TRUE
))
period_mean_rolling_depletion <- function(value, target_years) {
  mean(vapply(target_years, function(year) {
    numerator <- value$spawning_potential[value$year == year]
    denominator <- mean(value$spawning_potential_noeff[
      value$year %in% seq.int(year - 10L, year - 1L)
    ])
    numerator / denominator
  }, numeric(1)))
}
management_correction <- do.call(rbind, lapply(draw_groups, function(value) {
  terminal_year <- max(value$year)
  recent_years <- (terminal_year - 3L):terminal_year
  recent_sb <- mean(value$spawning_potential[
    value$year %in% recent_years
  ])
  recent_sb0 <- mean(value$spawning_potential_noeff[
    value$year %in% (terminal_year - 10L):(terminal_year - 1L)
  ])
  recent_mean_depletion <- period_mean_rolling_depletion(value, recent_years)
  historical <- period_mean_rolling_depletion(value, 2012:2015)
  data.frame(
    ensemble_id = value$ensemble_id[[1L]],
    draw = value$draw[[1L]],
    sb_recent_sb0 = recent_sb / recent_sb0,
    recent_mean_depletion = recent_mean_depletion,
    historical_target_depletion = historical,
    recent_historical_target_ratio = recent_mean_depletion / historical
  )
}))
management_correction <- management_correction[order(
  management_correction$ensemble_id, management_correction$draw
), ]
management_draws <- management_draws[order(
  management_draws$ensemble_id, management_draws$draw
), ]
management_keys <- management_draws[c("ensemble_id", "draw")]
correction_keys <- management_correction[c("ensemble_id", "draw")]
if (anyDuplicated(management_keys) || anyDuplicated(correction_keys) ||
    !identical(as.character(management_keys$ensemble_id),
               as.character(correction_keys$ensemble_id)) ||
    !identical(as.integer(management_keys$draw),
               as.integer(correction_keys$draw))) {
  stop("Annual and management Hessian draws do not align.", call. = FALSE)
}
for (name in c(
  "sb_recent_sb0", "recent_mean_depletion", "historical_target_depletion",
  "recent_historical_target_ratio"
)) management_draws[[name]] <- management_correction[[name]]

validation_tolerance <- if ("point_validation_tolerance" %in% names(metadata)) {
  ifelse(is.na(metadata$point_validation_tolerance), 1e-3,
         metadata$point_validation_tolerance)
} else {
  rep(1e-3, nrow(metadata))
}
if (
  !identical(sort(metadata$ensemble_id), pdh_ids) ||
    anyDuplicated(metadata$ensemble_id) ||
    any(metadata$draws < 1L) ||
    any(!is.finite(validation_tolerance)) ||
    any(metadata$maximum_point_relative_error > validation_tolerance)
) {
  stop("Per-model Hessian metadata validation failed.", call. = FALSE)
}
draw_count <- unique(metadata$draws)
if (length(draw_count) != 1L) {
  stop("Per-model Hessian draw counts differ.", call. = FALSE)
}

expected_annual <- length(pdh_ids) * draw_count * length(unique(annual_draws$year))
expected_management <- length(pdh_ids) * draw_count
if (
  nrow(annual_draws) != expected_annual ||
    nrow(management_draws) != expected_management ||
    any(!is.finite(unlist(annual_draws[-(1:3)]))) ||
    any(!is.finite(unlist(management_draws[-(1:2)])))
) {
  stop("Aggregated Hessian draw dimensions or values are invalid.", call. = FALSE)
}

method <- paste(
  "For each positive-definite assessment-model Hessian, correlated parameter-space",
  "normal draws were propagated jointly through dependent-variable",
  "gradients on the log scale. No Hessian regularisation or eigenvalue",
  "replacement was applied. Near-PDH models retain their central estimates",
  "in the all-model hybrid summary and do not receive estimation draws."
)

payload <- list(
  schema_version = "1.2.0",
  created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  method = method,
  pdh_model_ids = pdh_ids,
  near_pdh_model_ids = near_pdh_ids,
  draws_per_pdh_model = draw_count,
  metadata = metadata,
  annual_draws = annual_draws,
  management_draws = management_draws
)

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(metadata_file), recursive = TRUE, showWarnings = FALSE)
saveRDS(payload, output_file, version = 3, compress = "xz")
write.csv(metadata, metadata_file, row.names = FALSE)

cat(sprintf(
  paste0(
    "Stored %d PDH models x %d joint draws; %d Near-PDH central fits are ",
    "recorded separately.\n"
  ),
  length(pdh_ids), draw_count, length(near_pdh_ids)
))
