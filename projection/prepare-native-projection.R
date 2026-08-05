#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(FLCore)
  library(FLR4MFCL)
})

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(name, default = NULL) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", name, "="), "", hit[[length(hit)]])
}

input_dir <- normalizePath(arg_value("--input-dir", "model"), mustWork = TRUE)
output_dir <- arg_value("--output-dir", "projection/work")
final_par <- normalizePath(
  arg_value("--final-par", "results/reference/final.par"),
  mustWork = TRUE
)
nyears <- as.integer(arg_value("--nyears", "30"))
nsims <- as.integer(arg_value("--nsims", "10"))
seed <- as.integer(arg_value("--seed", "20260806"))
average_years <- as.integer(strsplit(
  arg_value("--average-years", "2022,2023,2024"), ",", fixed = TRUE
)[[1]])

stopifnot(
  nyears > 0L,
  nsims > 0L,
  seed > 0L,
  length(average_years) > 0L,
  all(is.finite(average_years))
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, mustWork = TRUE)

source(file.path(input_dir, "fishery_map.R"), local = TRUE)
frq <- read.MFCLFrq(file.path(input_dir, "bet.frq"))
par <- read.MFCLPar(final_par, first.yr = range(frq)["minyear"])

terminal_year <- as.integer(range(frq)["maxyear"])
first_projection_year <- terminal_year + 1L
projection_years <- seq.int(first_projection_year, length.out = nyears)
if (!identical(sort(fishery_map$fishery), seq_len(n_fisheries(frq)))) {
  stop("fishery_map.R does not cover every MFCL fishery exactly once.")
}
if (!all(average_years %in% unique(freq(frq)$year))) {
  stop("At least one requested recent-catch averaging year is absent from bet.frq.")
}

# Generate MFCL's complete future incident pattern with every fishery
# conditioned on its 2022--2024 mean catch.  This is the assessment projection
# scenario, not a continuation of CPUE/index effort.  Mixing catch- and
# effort-conditioned fisheries creates future missing-catch q0 parameters and
# is not the requested constant-recent-catch projection.
template_caeff <- rep(1L, n_fisheries(frq))

controls <- data.frame(
  name = fishery_map$fishery_name,
  region = fishery_map$region,
  caeff = template_caeff,
  scaler = 1,
  ess_length = NA_real_,
  ess_weight = NA_real_,
  stringsAsFactors = FALSE
)

proj_control <- MFCLprojControl(
  nyears = nyears,
  nsims = nsims,
  avyrs = as.character(average_years),
  fprojyr = first_projection_year,
  controls = controls
)
proj_frq <- generate(frq, proj_control)

# FLR4MFCL::generate() averages only the catch rows that are present for a
# fishery-quarter.  That is not an annual 2022--2024 mean when a fishery is
# absent in one or more quarters: an isolated observed catch can otherwise be
# repeated into every future quarter.  It can also misalign a fishery with no
# recent rows.  Retain the native future incident calendar, but replace its
# catch values with an explicit fishery-by-quarter mean over all three calendar
# years, treating an absent catch incident as zero.  Consequently, the four
# quarterly projection catches sum exactly to each fishery's three-year mean
# annual catch.
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
  year = average_years,
  month = sort(unique(freq(frq)$month)),
  fishery = seq_len(n_fisheries(frq)),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
catch_grid <- merge(
  catch_grid, historical_catch,
  by = c("year", "month", "fishery"), all.x = TRUE, sort = FALSE
)
catch_grid$catch[is.na(catch_grid$catch)] <- 0
quarterly_mean_catch <- aggregate(
  catch ~ month + fishery, catch_grid, mean
)

proj_data <- freq(proj_frq)
future_rows <- which(
  proj_data$year %in% projection_years &
    is.na(proj_data$length) & is.na(proj_data$weight)
)
future_key <- paste(proj_data$month[future_rows], proj_data$fishery[future_rows])
catch_key <- paste(quarterly_mean_catch$month, quarterly_mean_catch$fishery)
catch_match <- match(future_key, catch_key)
if (anyNA(catch_match)) {
  stop("The native future incident calendar contains an unmatched fishery-quarter.")
}
proj_data$catch[future_rows] <- quarterly_mean_catch$catch[catch_match]
proj_data$effort[future_rows] <- -1
# MFCL option 7 requires every retained future incident to have a positive
# catch.  Use a 1e-6 numerical placeholder (less than 5 g per fishery-year)
# only where the exact three-year mean is zero; the audit below verifies that
# this does not change the annual total at reporting precision.
zero_future_rows <- future_rows[proj_data$catch[future_rows] == 0]
if (length(zero_future_rows)) proj_data$catch[zero_future_rows] <- 1e-6
freq(proj_frq) <- proj_data

# Preserve the projection calendar and conditioning produced by FLR4MFCL.
# Rebuilding these rows by hand changes MFCL's regional incident counters and
# can make native option 7 fail even when annual catches appear correct.
proj_data <- freq(proj_frq)
projection_grid <- proj_data[
  proj_data$year %in% projection_years &
    is.na(proj_data$length) & is.na(proj_data$weight),
  , drop = FALSE
]

conditioning <- data.frame(
  fishery = seq_len(n_fisheries(frq)),
  fishery_name = fishery_map$fishery_name,
  region = fishery_map$region,
  caeff = template_caeff,
  conditioning = ifelse(template_caeff == 1L, "catch", "effort"),
  stringsAsFactors = FALSE
)
annual_mean_by_fishery <- aggregate(catch ~ fishery, catch_grid, sum)
annual_mean_by_fishery$catch <- annual_mean_by_fishery$catch / length(average_years)
conditioning$mean_annual_catch <- annual_mean_by_fishery$catch[
  match(conditioning$fishery, annual_mean_by_fishery$fishery)
]
catch_rows <- projection_grid[
  is.finite(projection_grid$catch) & projection_grid$catch >= 0,
  , drop = FALSE
]
if (!nrow(catch_rows)) stop("The native projection contains no future catch rows.")
if (!setequal(
  unique(catch_rows$fishery), conditioning$fishery[conditioning$caeff == 1L]
)) {
  stop("Future catch-conditioned fisheries do not match MFCLprojControl.")
}
if (any(is.finite(projection_grid$catch) & projection_grid$catch < 0)) {
  stop("A future projection incident remains effort-conditioned.")
}

write(proj_frq, file.path(output_dir, "proj.frq"))
for (pair in list(c("bet.ini", "proj.ini"), c("bet.tag", "proj.tag"))) {
  ok <- file.copy(
    file.path(input_dir, pair[[1]]),
    file.path(output_dir, pair[[2]]),
    overwrite = TRUE
  )
  if (!ok) stop("Could not copy ", pair[[1]])
}

for (suffix in c("age_length", "reg_scaling")) {
  source_file <- file.path(input_dir, paste0("bet.", suffix))
  if (file.exists(source_file)) {
    ok <- file.copy(source_file, file.path(output_dir, paste0("proj.", suffix)),
                    overwrite = TRUE)
    if (!ok) stop("Could not copy ", basename(source_file))
  }
}
for (name in c("mfcl.cfg", "mfclo64")) {
  source_file <- file.path(input_dir, name)
  if (!file.exists(source_file)) {
    source_file <- file.path(dirname(input_dir), name)
  }
  if (file.exists(source_file)) {
    ok <- file.copy(source_file, file.path(output_dir, name), overwrite = TRUE,
                    copy.mode = TRUE)
    if (!ok) stop("Could not copy ", basename(source_file))
  }
}

projection_audit <- data.frame(
  field = c(
    "terminal_year", "first_projection_year", "last_projection_year",
    "projection_years", "stochastic_replicates", "recruitment_seed",
    "recent_catch_years", "catch_conditioned_fisheries",
    "effort_conditioned_fisheries",
    "conditioning", "mfcl_final_par_sha256"
  ),
  value = c(
    terminal_year, first_projection_year, max(projection_years), nyears, nsims,
    seed, paste(average_years, collapse = ","),
    paste(conditioning$fishery[conditioning$caeff == 1L], collapse = ","),
    paste(conditioning$fishery[conditioning$caeff == 2L], collapse = ","),
    paste(
      "native FLR4MFCL MFCLprojControl calendar retained; every fishery uses",
      "its exact 2022-2024 mean catch; native future incident structure retained"
    ),
    strsplit(
      system2("sha256sum", shQuote(final_par), stdout = TRUE)[[1]],
      "[[:space:]]+"
    )[[1]][[1]]
  ),
  stringsAsFactors = FALSE
)
names(projection_audit)[2] <- "value"
write.csv(projection_audit, file.path(output_dir, "projection-audit.csv"),
          row.names = FALSE)
write.csv(conditioning, file.path(output_dir, "projection-conditioning.csv"),
          row.names = FALSE)
saveRDS(proj_control, file.path(output_dir, "projection-control.rds"),
        version = 3)

annual_projection_catch <- aggregate(
  catch ~ year,
  catch_rows,
  sum
)
observed_annual_catch <- aggregate(catch ~ year, catch_grid, sum)
expected_annual_catch <- mean(observed_annual_catch$catch)
if (
  length(unique(round(annual_projection_catch$catch, 8))) != 1L ||
    !isTRUE(all.equal(
      annual_projection_catch$catch[[1L]], expected_annual_catch,
      tolerance = 1e-10
    ))
) {
  stop("The total projected extraction catch is not constant among years.")
}
write.csv(
  annual_projection_catch,
  file.path(output_dir, "annual-projection-catch-audit.csv"),
  row.names = FALSE
)

cat(sprintf(
  paste0(
    "Prepared native MFCL projection inputs: %d--%d, %d simulations, ",
    "%d catch-conditioned fisheries, recent years %s.\n"
  ),
  first_projection_year, max(projection_years), nsims,
  sum(conditioning$caeff == 1L),
  paste(average_years, collapse = "--")
))
