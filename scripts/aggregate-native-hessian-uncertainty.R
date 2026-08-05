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
metadata <- do.call(rbind, lapply(per_model, `[[`, "metadata"))
annual_draws <- do.call(rbind, lapply(per_model, `[[`, "annual_draws"))
management_draws <- do.call(rbind, lapply(per_model, `[[`, "management_draws"))

if (
  !identical(sort(metadata$ensemble_id), pdh_ids) ||
    anyDuplicated(metadata$ensemble_id) ||
    any(metadata$draws < 1L) ||
    any(metadata$maximum_point_relative_error > 5e-4)
) {
  stop("Per-model native-Hessian metadata validation failed.", call. = FALSE)
}
draw_count <- unique(metadata$draws)
if (length(draw_count) != 1L) {
  stop("Per-model native-Hessian draw counts differ.", call. = FALSE)
}

expected_annual <- length(pdh_ids) * draw_count * length(unique(annual_draws$year))
expected_management <- length(pdh_ids) * draw_count
if (
  nrow(annual_draws) != expected_annual ||
    nrow(management_draws) != expected_management ||
    any(!is.finite(unlist(annual_draws[-(1:3)]))) ||
    any(!is.finite(unlist(management_draws[-(1:2)])))
) {
  stop("Aggregated native-Hessian draw dimensions or values are invalid.", call. = FALSE)
}

method <- paste(
  "For each positive-definite native MFCL Hessian, correlated parameter-space",
  "normal draws were propagated jointly through native dependent-variable",
  "gradients on the log scale. No Hessian regularisation or eigenvalue",
  "replacement was applied. Near-PDH models retain their central estimates",
  "in the all-model hybrid summary and do not receive estimation draws."
)

payload <- list(
  schema_version = "1.0.0",
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
