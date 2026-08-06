#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(FLCore)
  library(FLR4MFCL)
})

args <- commandArgs(trailingOnly = TRUE)
input_dir <- if (length(args) >= 1L) args[[1L]] else "model"
output_file <- if (length(args) >= 2L) {
  args[[2L]]
} else {
  "data/projection/fishery-quarter-conditioning.csv"
}
input_dir <- normalizePath(input_dir, mustWork = TRUE)

source(file.path(input_dir, "fishery_map.R"), local = TRUE)
frq <- read.MFCLFrq(file.path(input_dir, "bet.frq"))
average_years <- 2022:2024
months <- sort(unique(freq(frq)$month))
if (length(months) != 4L || n_fisheries(frq) != 33L) {
  stop("The conditioning audit requires 33 fisheries and four quarters.")
}

historical_catch <- freq(frq)[
  freq(frq)$year %in% average_years &
    is.na(freq(frq)$length) & is.na(freq(frq)$weight) &
    is.finite(freq(frq)$catch) & freq(frq)$catch >= 0,
  c("year", "month", "fishery", "catch"), drop = FALSE
]
historical_catch <- aggregate(
  catch ~ year + month + fishery, historical_catch, sum
)
catch_grid <- expand.grid(
  year = average_years, month = months,
  fishery = seq_len(n_fisheries(frq)),
  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
)
catch_grid <- merge(
  catch_grid, historical_catch,
  by = c("year", "month", "fishery"), all.x = TRUE, sort = FALSE
)
catch_grid$catch[is.na(catch_grid$catch)] <- 0
quarterly <- aggregate(catch ~ month + fishery, catch_grid, mean)
names(quarterly)[names(quarterly) == "catch"] <- "mean_quarterly_catch"
quarterly$quarter <- match(quarterly$month, months)
quarterly$fishery_name <- fishery_map$fishery_name[
  match(quarterly$fishery, fishery_map$fishery)
]
quarterly$region <- fishery_map$region[
  match(quarterly$fishery, fishery_map$fishery)
]
catch_unit_flag <- c(rep(0L, 11L), rep(1L, 17L), rep(0L, 5L))
quarterly$catch_unit <- ifelse(
  catch_unit_flag[quarterly$fishery] == 1L, "weight", "number"
)
quarterly$exact_zero <- quarterly$mean_quarterly_catch == 0
quarterly <- quarterly[order(quarterly$fishery, quarterly$quarter), c(
  "fishery", "fishery_name", "region", "quarter", "month", "catch_unit",
  "mean_quarterly_catch", "exact_zero"
)]

annual <- aggregate(mean_quarterly_catch ~ fishery, quarterly, sum)
if (nrow(quarterly) != 33L * 4L || sum(quarterly$exact_zero) != 25L ||
    !identical(annual$fishery[annual$mean_quarterly_catch == 0], 9L)) {
  stop("The fishery-quarter conditioning audit does not match the locked scenario.")
}

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
write.csv(quarterly, output_file, row.names = FALSE)
cat(sprintf(
  "Wrote %d fishery-quarter means; %d are exact zeros.\n",
  nrow(quarterly), sum(quarterly$exact_zero)
))
