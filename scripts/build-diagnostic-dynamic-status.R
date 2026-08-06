#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(FLCore)
  library(FLR4MFCL)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop(
    "Usage: build-diagnostic-dynamic-status.R MODEL_PAYLOAD_RDS OUTPUT_DIR",
    call. = FALSE
  )
}
payload_file <- normalizePath(args[[1L]], mustWork = TRUE)
output_dir <- args[[2L]]
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

payload <- readRDS(payload_file)
decode_object <- function(role) {
  object <- payload$object_cache$objects[[role]]
  if (is.null(object)) stop("Diagnostic payload does not contain ", role, ".")
  bytes <- if (identical(object$compression, "none")) {
    object$bytes
  } else {
    memDecompress(object$bytes, type = object$compression)
  }
  unserialize(bytes)
}

annual_mean <- function(value) {
  data <- as.data.frame(value)
  names(data)[names(data) == "data"] <- "value"
  stats::aggregate(value ~ year, data, mean)
}

annual_stock_total <- function(value) {
  data <- as.data.frame(value)
  names(data)[names(data) == "data"] <- "value"
  by_period <- stats::aggregate(value ~ year + season, data, sum)
  stats::aggregate(value ~ year, by_period, mean)
}

rep <- decode_object("RepOut")
fished <- annual_stock_total(slot(rep, "adultBiomass"))
nofish <- annual_stock_total(slot(rep, "adultBiomass_nofish"))
sbmsy <- annual_mean(slot(rep, "ABBMSY_ts"))
fmsy <- annual_mean(slot(rep, "FFMSY_ts"))
names(fished)[names(fished) == "value"] <- "spawning_biomass_mt"
names(nofish)[names(nofish) == "value"] <- "spawning_biomass_noeff_mt"
names(sbmsy)[names(sbmsy) == "value"] <- "sb_sbmsy"
names(fmsy)[names(fmsy) == "value"] <- "f_fmsy"

status <- Reduce(
  function(x, y) merge(x, y, by = "year", sort = TRUE),
  list(fished, nofish, sbmsy, fmsy)
)
status$depletion <- with(
  status, spawning_biomass_mt / spawning_biomass_noeff_mt
)
status <- status[c("year", "depletion", "sb_sbmsy", "f_fmsy")]
status$year <- as.integer(status$year)
if (nrow(status) != 73L || !identical(status$year, 1952:2024) ||
    any(!is.finite(unlist(status))) ||
    any(status[c("depletion", "sb_sbmsy", "f_fmsy")] <= 0)) {
  stop("The diagnostic dynamic-status series is incomplete or invalid.")
}

hash_output <- system2("sha256sum", payload_file, stdout = TRUE)
payload_hash <- strsplit(hash_output[[1L]], "[[:space:]]+")[[1L]][[1L]]
metadata <- data.frame(
  source_repository = "PacificCommunity/ofp-sam-bet-2026-diagnostic",
  source_artifact = "results/reference/model_payload.rds",
  source_payload_sha256 = payload_hash,
  first_year = min(status$year),
  terminal_year = max(status$year),
  objective_function = as.numeric(payload$obj_fun),
  maximum_gradient = as.numeric(payload$max_grad),
  stringsAsFactors = FALSE
)

utils::write.csv(
  status, file.path(output_dir, "dynamic-status.csv"), row.names = FALSE
)
utils::write.csv(
  metadata, file.path(output_dir, "dynamic-status-metadata.csv"), row.names = FALSE
)

files <- c("dynamic-status.csv", "dynamic-status-metadata.csv")
hashes <- vapply(file.path(output_dir, files), function(path) {
  output <- system2("sha256sum", path, stdout = TRUE)
  strsplit(output[[1L]], "[[:space:]]+")[[1L]][[1L]]
}, character(1L))
writeLines(paste(hashes, files), file.path(output_dir, "SHA256SUMS"))

cat("Stored the verified 1952-2024 diagnostic dynamic-status series.\n")
