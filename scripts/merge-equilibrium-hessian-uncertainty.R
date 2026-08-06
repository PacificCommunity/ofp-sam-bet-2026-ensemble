#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 5L) {
  stop(
    paste(
      "Usage: merge-equilibrium-hessian-uncertainty.R EQUILIBRIUM_DIR",
      "BASE_RDS FIT_DIAGNOSTICS_CSV OUTPUT_RDS OUTPUT_METADATA_CSV"
    ),
    call. = FALSE
  )
}

input_dir <- normalizePath(args[[1L]], mustWork = TRUE)
base_file <- normalizePath(args[[2L]], mustWork = TRUE)
fit_file <- normalizePath(args[[3L]], mustWork = TRUE)
output_file <- args[[4L]]
metadata_file <- args[[5L]]

base <- readRDS(base_file)
fit <- read.csv(fit_file, check.names = FALSE)
# Enforce the official annual-depletion definition even when the base cache
# predates that correction.  The two joint biomass draws are already retained.
base$annual_draws$depletion <- with(
  base$annual_draws, spawning_potential / spawning_potential_noeff
)
draw_groups <- split(base$annual_draws, interaction(
  base$annual_draws$ensemble_id, base$annual_draws$draw, drop = TRUE
))
management_correction <- do.call(rbind, lapply(draw_groups, function(value) {
  terminal_year <- max(value$year)
  recent_sb <- mean(value$spawning_potential[
    value$year %in% (terminal_year - 3L):terminal_year
  ])
  recent_sb0 <- mean(value$spawning_potential_noeff[
    value$year %in% (terminal_year - 10L):(terminal_year - 1L)
  ])
  historical <- mean(value$depletion[value$year %in% 2012:2015])
  data.frame(
    ensemble_id = value$ensemble_id[[1L]], draw = value$draw[[1L]],
    sb_recent_sb0 = recent_sb / recent_sb0,
    historical_target_depletion = historical,
    recent_historical_target_ratio = (recent_sb / recent_sb0) / historical
  )
}))
management_correction <- management_correction[order(
  management_correction$ensemble_id, management_correction$draw
), ]
base$management_draws <- base$management_draws[order(
  base$management_draws$ensemble_id, base$management_draws$draw
), ]
base_keys <- base$management_draws[c("ensemble_id", "draw")]
correction_keys <- management_correction[c("ensemble_id", "draw")]
if (anyDuplicated(base_keys) || anyDuplicated(correction_keys) ||
    !identical(as.character(base_keys$ensemble_id),
               as.character(correction_keys$ensemble_id)) ||
    !identical(as.integer(base_keys$draw),
               as.integer(correction_keys$draw))) {
  stop("Base Hessian draw keys do not align.", call. = FALSE)
}
for (name in c(
  "sb_recent_sb0", "historical_target_depletion",
  "recent_historical_target_ratio"
)) base$management_draws[[name]] <- management_correction[[name]]
pdh_ids <- sort(fit$ensemble_id[as.logical(fit$positive_definite_hessian)])
paths <- file.path(input_dir, paste0(pdh_ids, ".rds"))
if (any(!file.exists(paths))) {
  stop("Missing equilibrium payload: ", basename(paths[!file.exists(paths)][[1L]]))
}
values <- lapply(paths, readRDS)
metadata <- do.call(rbind, lapply(values, `[[`, "metadata"))
draws <- do.call(rbind, lapply(values, `[[`, "draws"))
if (!identical(sort(unique(draws$ensemble_id)), pdh_ids) ||
    anyDuplicated(draws[c("ensemble_id", "draw")]) ||
    nrow(draws) != length(pdh_ids) * base$draws_per_pdh_model ||
    any(!is.finite(unlist(draws[-(1:2)]))) ||
    max(metadata$central_f_recent_fmsy_relative_error) > 1e-3 ||
    max(metadata$central_sb_recent_sbmsy_relative_error) > 5e-3) {
  stop("Equilibrium Hessian payload validation failed.", call. = FALSE)
}

management <- base$management_draws
management$sb_recent_sbmsy_native <- NULL
management$f_recent_fmsy_native <- NULL
management <- merge(
  management,
  draws[c("ensemble_id", "draw", "sb_recent_sbmsy", "f_recent_fmsy")],
  by = c("ensemble_id", "draw"), sort = FALSE
)
management <- management[order(management$ensemble_id, management$draw), ]
if (nrow(management) != nrow(base$management_draws)) {
  stop("The exact management draws did not join one-to-one.", call. = FALSE)
}

base$schema_version <- "1.1.0"
base$created_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
base$method <- paste(
  "For each positive-definite Hessian, a single joint parameter draw is",
  "propagated through all derived quantities. MSY-based quantities use",
  "first-order implicit derivatives of each model's equilibrium yield and",
  "spawning-biomass curves; the same draw is used for recent biomass,",
  "preserving cross-quantity covariance.",
  "No Hessian regularisation or independent marginal resampling is used."
)
base$management_draws <- management
base$equilibrium_metadata <- metadata
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(metadata_file), recursive = TRUE, showWarnings = FALSE)
saveRDS(base, output_file, version = 3L, compress = "xz")
write.csv(metadata, metadata_file, row.names = FALSE)
cat(sprintf(
  "Merged exact MSY-based quantities for %d PDH models x %d joint draws.\n",
  length(pdh_ids), base$draws_per_pdh_model
))
