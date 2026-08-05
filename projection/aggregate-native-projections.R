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
ids <- sort(unique(fit$ensemble_id))
if (length(ids) != 88L) stop("Expected the 88 completed ensemble models.")

paths <- file.path(input_dir, paste0(ids, ".rds"))
if (any(!file.exists(paths))) {
  stop(
    "Missing per-model projection cache: ",
    paste(basename(paths[!file.exists(paths)]), collapse = ", ")
  )
}
per_model <- lapply(paths, readRDS)
metadata <- do.call(rbind, lapply(per_model, `[[`, "metadata"))
annual_stock <- do.call(rbind, lapply(seq_along(per_model), function(i) {
  transform(per_model[[i]]$annual_stock, ensemble_id = ids[[i]])
}))
annual_region <- do.call(rbind, lapply(seq_along(per_model), function(i) {
  transform(per_model[[i]]$annual_region, ensemble_id = ids[[i]])
}))
terminal_msy <- do.call(rbind, lapply(seq_along(per_model), function(i) {
  transform(per_model[[i]]$terminal_msy, ensemble_id = ids[[i]])
}))
annual_catch <- do.call(rbind, lapply(seq_along(per_model), function(i) {
  transform(per_model[[i]]$annual_catch, ensemble_id = ids[[i]])
}))

if (
  !identical(metadata$ensemble_id, ids) || anyDuplicated(metadata$cache_key) ||
    nrow(annual_stock) != 88L * 10L * 30L ||
    nrow(annual_region) != 88L * 10L * 30L * 5L ||
    nrow(terminal_msy) != 88L * 10L ||
    any(!is.finite(unlist(annual_stock[c(
      "spawning_biomass_mt", "spawning_biomass_noeff_mt", "depletion"
    )]))) ||
    any(!is.finite(unlist(terminal_msy[-c(1L, ncol(terminal_msy))])))
) {
  stop("Aggregated native-projection validation failed.", call. = FALSE)
}

payload <- list(
  schema_version = "1.0.0",
  created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  method = per_model[[1L]]$method,
  ensemble_ids = ids,
  simulations_per_model = 10L,
  projection_years = 2025:2054,
  metadata = metadata,
  annual_stock = annual_stock,
  annual_region = annual_region,
  terminal_msy = terminal_msy,
  annual_catch = annual_catch
)
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(metadata_file), recursive = TRUE, showWarnings = FALSE)
saveRDS(payload, output_file, version = 3L, compress = "xz")
write.csv(metadata, metadata_file, row.names = FALSE)
cat("Stored 88 models x 10 native stochastic projections for 2025-2054.\n")
