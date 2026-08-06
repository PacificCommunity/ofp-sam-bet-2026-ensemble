#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(FLCore)
  library(FLR4MFCL)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4L) {
  stop(
    paste(
      "Usage: upgrade-historical-regional-depletion.R ARCHIVE_DIR",
      "INPUT_PROJECTION_RDS OUTPUT_PROJECTION_RDS OUTPUT_METADATA_CSV"
    ),
    call. = FALSE
  )
}
archive_dir <- normalizePath(args[[1L]], mustWork = TRUE)
input_file <- normalizePath(args[[2L]], mustWork = TRUE)
output_file <- args[[3L]]
metadata_file <- args[[4L]]

projection <- readRDS(input_file)
ids <- sort(projection$assessment_ensemble_ids)
if (length(ids) != 88L ||
    nrow(projection$historical_region) != 88L * 73L * 5L) {
  stop("Expected the complete 88-model projection payload.")
}

decode_rep <- function(payload) {
  object <- payload$object_cache$objects$RepOut
  if (is.null(object)) stop("A model payload does not contain RepOut.")
  bytes <- if (identical(object$compression, "none")) {
    object$bytes
  } else {
    memDecompress(object$bytes, type = object$compression)
  }
  unserialize(bytes)
}

annual_region <- function(value, value_name) {
  data <- as.data.frame(value)
  names(data)[names(data) == "data"] <- value_name
  result <- stats::aggregate(
    data[[value_name]],
    list(year = as.integer(as.character(data$year)),
         region = as.integer(as.character(data$area))),
    mean
  )
  names(result)[[3L]] <- value_name
  result[order(result$year, result$region), ]
}

regional_rows <- vector("list", length(ids))
metadata_rows <- vector("list", length(ids))
for (index in seq_along(ids)) {
  ensemble_id <- ids[[index]]
  archive <- file.path(archive_dir, paste0(ensemble_id, ".tar.gz"))
  if (!file.exists(archive)) stop("Missing source archive for ", ensemble_id, ".")
  work <- tempfile(paste0(ensemble_id, "-regional-"))
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)
  utils::untar(archive, exdir = work)
  payload_files <- list.files(
    work, pattern = "model_payload[.]rds$", recursive = TRUE, full.names = TRUE
  )
  if (length(payload_files) != 1L) {
    stop("Expected one model payload in ", basename(archive), ".")
  }
  payload <- readRDS(payload_files[[1L]])
  rep <- decode_rep(payload)
  fished <- annual_region(slot(rep, "adultBiomass"), "spawning_biomass_mt")
  nofish <- annual_region(
    slot(rep, "adultBiomass_nofish"), "spawning_biomass_noeff_mt"
  )
  model_region <- merge(fished, nofish, by = c("year", "region"), sort = TRUE)
  model_region$ensemble_id <- ensemble_id
  model_region$depletion <- with(
    model_region, spawning_biomass_mt / spawning_biomass_noeff_mt
  )
  if (nrow(model_region) != 73L * 5L ||
      !identical(sort(unique(model_region$year)), 1952:2024) ||
      !identical(sort(unique(model_region$region)), 1:5) ||
      any(!is.finite(unlist(model_region[c(
        "year", "region", "spawning_biomass_mt",
        "spawning_biomass_noeff_mt", "depletion"
      )]))) ||
      any(model_region$spawning_biomass_noeff_mt <= 0)) {
    stop("Invalid regional time series for ", ensemble_id, ".")
  }

  existing <- projection$historical_region[
    projection$historical_region$ensemble_id == ensemble_id,
    c("year", "region", "spawning_biomass_mt")
  ]
  comparison <- merge(
    existing, fished, by = c("year", "region"),
    suffixes = c("_projection", "_assessment")
  )
  maximum_difference <- max(abs(
    comparison$spawning_biomass_mt_projection -
      comparison$spawning_biomass_mt_assessment
  ))
  maximum_relative_difference <- max(
    abs(
      comparison$spawning_biomass_mt_projection -
        comparison$spawning_biomass_mt_assessment
    ) / comparison$spawning_biomass_mt_assessment
  )
  if (nrow(comparison) != 73L * 5L || maximum_relative_difference > 5e-3) {
    stop("Regional fished biomass differs from the source model for ", ensemble_id, ".")
  }
  model_region <- merge(
    existing, nofish, by = c("year", "region"), sort = TRUE
  )
  model_region$ensemble_id <- ensemble_id
  model_region$depletion <- with(
    model_region, spawning_biomass_mt / spawning_biomass_noeff_mt
  )

  hash_output <- system2("sha256sum", archive, stdout = TRUE)
  archive_hash <- strsplit(hash_output[[1L]], "[[:space:]]+")[[1L]][[1L]]
  regional_rows[[index]] <- model_region
  metadata_rows[[index]] <- data.frame(
    ensemble_id = ensemble_id,
    source_archive_sha256 = archive_hash,
    maximum_fished_biomass_difference_mt = maximum_difference,
    maximum_fished_biomass_relative_difference = maximum_relative_difference,
    stringsAsFactors = FALSE
  )
  unlink(work, recursive = TRUE, force = TRUE)
  on.exit(NULL, add = FALSE)
}

historical_region <- do.call(rbind, regional_rows)
historical_region <- historical_region[c(
  "year", "region", "spawning_biomass_mt",
  "spawning_biomass_noeff_mt", "depletion", "ensemble_id"
)]
rownames(historical_region) <- NULL
if (nrow(historical_region) != 88L * 73L * 5L ||
    any(abs(
      historical_region$depletion -
        historical_region$spawning_biomass_mt /
          historical_region$spawning_biomass_noeff_mt
    ) > 1e-12)) {
  stop("The combined historical regional-depletion data are invalid.")
}

projection$schema_version <- "1.2.0"
projection$historical_region <- historical_region
projection$historical_region_method <- paste(
  "Annual regional depletion is the ratio of the four-quarter mean fished",
  "spawning biomass to the matching no-fishing spawning biomass."
)
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(metadata_file), recursive = TRUE, showWarnings = FALSE)
saveRDS(projection, output_file, version = 3L, compress = "xz")
utils::write.csv(
  do.call(rbind, metadata_rows), metadata_file, row.names = FALSE
)

cat("Added exact historical regional depletion for all 88 completed models.\n")
