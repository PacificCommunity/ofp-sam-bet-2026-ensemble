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
    max(metadata$central_f_recent_fmsy_relative_error) > 5e-4 ||
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
  "draw-specific equilibrium yield and spawning-biomass curves; the same",
  "draw is used for recent biomass, preserving cross-quantity covariance.",
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
