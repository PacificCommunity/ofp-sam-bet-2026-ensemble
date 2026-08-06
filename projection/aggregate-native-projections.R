#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4L) {
  stop(
    paste(
      "Usage: aggregate-native-projections.R PER_MODEL_DIR",
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
assessment_ids <- sort(unique(fit$ensemble_id))
if (length(assessment_ids) != 88L) stop("Expected the 88 completed ensemble models.")

# A constant-catch projection can terminate natively when the requested catch
# cannot be taken from one or more regions, or when the terminal equilibrium
# calculation fails.  Such models remain part of the fitted assessment
# ensemble, but incomplete stochastic projections must not be silently filled,
# regularised or treated as successful simulations.  Aggregate only complete,
# checksum-validated per-model caches and record the omitted model IDs.
available <- list.files(input_dir, pattern = "^ensemble-[0-9]{3}\\.rds$", full.names = TRUE)
ids <- sort(sub("\\.rds$", "", basename(available)))
if (!length(ids) || any(!ids %in% assessment_ids) || anyDuplicated(ids)) {
  stop("The available per-model projection cache set is invalid.")
}
failed_projection_ids <- setdiff(assessment_ids, ids)
paths <- file.path(input_dir, paste0(ids, ".rds"))
per_model <- lapply(paths, readRDS)
metadata <- do.call(rbind, lapply(per_model, `[[`, "metadata"))
conditioning <- per_model[[1L]]$conditioning
conditioning_matches <- vapply(per_model, function(value) {
  identical(value$conditioning, conditioning)
}, logical(1))
# MFCL input data flags in model/bet.frq define the original catch unit for
# every fishery (0 = number, 1 = weight).  Preserve that distinction in the
# public aggregate: a numeric sum across these unlike units is useful only as
# an internal cache audit and is not a stock-wide catch statistic.
catch_unit_flag <- c(rep(0L, 11L), rep(1L, 17L), rep(0L, 5L))
conditioning$catch_unit <- ifelse(catch_unit_flag == 1L, "weight", "number")
metadata$mixed_unit_audit_sum <- metadata$constant_annual_catch
metadata$constant_annual_catch <- NULL
metadata$catch_conditioning <-
  "Fishery-specific 2022-2024 mean in the original number or weight unit"
annual_stock <- do.call(rbind, lapply(seq_along(per_model), function(i) {
  transform(per_model[[i]]$annual_stock, ensemble_id = ids[[i]])
}))
annual_region <- do.call(rbind, lapply(seq_along(per_model), function(i) {
  transform(per_model[[i]]$annual_region, ensemble_id = ids[[i]])
}))
terminal_msy <- do.call(rbind, lapply(seq_along(per_model), function(i) {
  transform(per_model[[i]]$terminal_msy, ensemble_id = ids[[i]])
}))
catch_msy <- do.call(rbind, lapply(seq_along(per_model), function(i) {
  transform(per_model[[i]]$catch_msy, ensemble_id = ids[[i]])
}))
historical_region <- do.call(rbind, lapply(seq_along(per_model), function(i) {
  transform(per_model[[i]]$historical_region, ensemble_id = ids[[i]])
}))
annual_catch <- do.call(rbind, lapply(seq_along(per_model), function(i) {
  transform(per_model[[i]]$annual_catch, ensemble_id = ids[[i]])
}))

model_count <- length(ids)
if (
  !identical(metadata$ensemble_id, ids) || anyDuplicated(metadata$cache_key) ||
    !all(conditioning_matches) || nrow(conditioning) != 33L ||
    any(conditioning$caeff != 1L) || any(conditioning$conditioning != "catch") ||
    !identical(conditioning$catch_unit, ifelse(catch_unit_flag == 1L, "weight", "number")) ||
    nrow(annual_stock) != model_count * 10L * 30L ||
    nrow(annual_region) != model_count * 10L * 30L * 5L ||
    nrow(terminal_msy) != model_count * 10L ||
    nrow(catch_msy) != model_count * 10L * 30L ||
    nrow(historical_region) != model_count * 73L * 5L ||
    any(!is.finite(unlist(annual_stock[c(
      "spawning_biomass_mt", "spawning_biomass_noeff_mt", "depletion"
    )]))) ||
    any(!is.finite(unlist(terminal_msy[-c(1L, ncol(terminal_msy))]))) ||
    any(!is.finite(unlist(catch_msy[c(
      "catch_biomass_mt", "annual_msy_mt", "catch_msy"
    )]))) || any(catch_msy$annual_msy_mt <= 0) || any(catch_msy$catch_msy < 0) ||
    any(!is.finite(historical_region$spawning_biomass_mt)) ||
    any(historical_region$spawning_biomass_mt <= 0) ||
    !identical(sort(unique(historical_region$year)), 1952:2024) ||
    !identical(sort(unique(historical_region$region)), 1:5)
) {
  stop("Aggregated native-projection validation failed.", call. = FALSE)
}

payload <- list(
  schema_version = "1.0.0",
  created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  method = gsub(
    "native[- ]MFCL", "MFCL", per_model[[1L]]$method, ignore.case = TRUE
  ),
  assessment_ensemble_ids = assessment_ids,
  ensemble_ids = ids,
  failed_projection_ids = failed_projection_ids,
  projection_complete_models = model_count,
  projection_incomplete_models = length(failed_projection_ids),
  simulations_per_model = 10L,
  projection_years = 2025:2054,
  metadata = metadata,
  conditioning = conditioning,
  catch_units_note = paste(
    "MFCL fisheries retain their original number or weight units.",
    "The mixed-unit audit sum is not a reportable total catch."
  ),
  annual_stock = annual_stock,
  annual_region = annual_region,
  terminal_msy = terminal_msy,
  catch_msy = catch_msy,
  historical_region = historical_region,
  annual_catch = annual_catch
)
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(metadata_file), recursive = TRUE, showWarnings = FALSE)
saveRDS(payload, output_file, version = 3L, compress = "xz")
write.csv(metadata, metadata_file, row.names = FALSE)
write.csv(
  conditioning,
  file.path(dirname(metadata_file), "catch-conditioning.csv"),
  row.names = FALSE
)
cat(sprintf(
  paste0(
    "Stored %d complete models x 10 stochastic projections for ",
    "2025-2054; %d fitted ensemble models had incomplete native projections.\n"
  ),
  model_count, length(failed_projection_ids)
))
