options(stringsAsFactors = FALSE)
source("report/management-quantities.R")

required <- c(
  "data/ensemble/ensemble-timeseries.rds",
  "data/ensemble/fit-diagnostics.csv",
  "data/ensemble/management-quantities.csv",
  "data/ensemble/successful-model-design.csv",
  "data/ensemble/objective-components.csv",
  "data/estimation/native-hessian-uncertainty.rds",
  "data/estimation/native-hessian-metadata.csv",
  "data/estimation/EQUILIBRIUM_PER_MODEL_SHA256SUMS",
  "data/projection/native-projections.rds",
  "data/projection/native-projection-metadata.csv",
  "data/projection/fishery-quarter-conditioning.csv"
)
missing <- required[!file.exists(required)]
if (length(missing)) {
  stop("Missing public ensemble data: ", paste(missing, collapse = ", "), call. = FALSE)
}

series <- readRDS(required[[1]])
fit <- read.csv(required[[2]], check.names = FALSE)
management <- read.csv(required[[3]], check.names = FALSE)
design <- read.csv(required[[4]], check.names = FALSE)

expected_series <- c(
  "ensemble_id", "year", "depletion", "spawning_potential",
  "spawning_potential_nofish", "recruitment", "fishing_mortality",
  "sb_sbmsy", "f_fmsy"
)
expected_fit <- c(
  "ensemble_id", "maximum_gradient", "hessian_status",
  "positive_definite_hessian", "tau"
)
expected_management <- c(
  "ensemble_id", "terminal_year", "sb_recent_sb0", "sb_recent_sbmsy",
  "f_recent_fmsy", "historical_target_depletion",
  "recent_historical_target_ratio", "below_lrp_020", "below_sbmsy",
  "above_fmsy"
)
expected_design <- c(
  "ensemble_id", "steepness", "tag_mixing_k_cutoff", "tag_reporting",
  "tag_tau", "m_age40_quarterly", "effort_creep_primary"
)
for (check in list(
  series = list(data = series, columns = expected_series),
  fit = list(data = fit, columns = expected_fit),
  management = list(data = management, columns = expected_management),
  design = list(data = design, columns = expected_design)
)) {
  absent <- setdiff(check$columns, names(check$data))
  if (length(absent)) stop("Missing columns: ", paste(absent, collapse = ", "), call. = FALSE)
}

ids <- sort(unique(series$ensemble_id))
if (!identical(ids, sort(fit$ensemble_id)) ||
    !identical(ids, sort(management$ensemble_id)) ||
    !identical(ids, sort(design$ensemble_id))) {
  stop("The public ensemble tables do not contain identical model identifiers.", call. = FALSE)
}
if (length(ids) != 88L) stop("Expected 88 completed ensemble models; found ", length(ids), call. = FALSE)
if (anyNA(series[expected_series]) || any(!is.finite(series$depletion))) {
  stop("The public time-series data contain missing or non-finite values.", call. = FALSE)
}
if (!setequal(as.integer(unique(series$year)), 1952:2024)) {
  stop("The ensemble time series must cover 1952--2024.", call. = FALSE)
}
coverage <- split(as.integer(series$year), series$ensemble_id)
if (any(!vapply(coverage, function(years) setequal(years, 1952:2024), logical(1)))) {
  stop("Every ensemble model must contain a complete 1952--2024 time series.", call. = FALSE)
}
if (any(management$terminal_year != 2024L) ||
    any(management$sb_recent_period != "2021–2024") ||
    any(management$sb0_recent_period != "2014–2023") ||
    any(management$f_recent_period != "2020–2023") ||
    any(management$historical_target_period != "2012–2015")) {
  stop("The official BET recent or historical-target periods are inconsistent.", call. = FALSE)
}
historical_check <- aggregate(
  depletion ~ ensemble_id,
  data = series[series$year %in% 2012:2015, ],
  FUN = mean
)
historical_check <- historical_check$depletion[
  match(management$ensemble_id, historical_check$ensemble_id)
]
if (any(abs(historical_check - management$historical_target_depletion) > 1e-12)) {
  stop("The 2012--2015 historical target was not computed from annual depletion.", call. = FALSE)
}
historical_management <- rolling_recent_depletion(
  series[c(
    "ensemble_id", "year", "spawning_potential", "spawning_potential_nofish"
  )],
  "ensemble_id", target_years = 2024L
)
historical_management <- historical_management[
  match(management$ensemble_id, historical_management$ensemble_id),
]
if (any(abs(historical_management$sb_recent - management$sb_recent_kt) > 1e-10) ||
    any(abs(historical_management$sb_f0_recent - management$sb0_recent_kt) > 1e-10) ||
    any(abs(historical_management$sb_recent_sb0 - management$sb_recent_sb0) > 1e-12) ||
    any(management$below_lrp_020 != (management$sb_recent_sb0 < 0.20)) ||
    any(management$below_sbmsy != (management$sb_recent_sbmsy < 1)) ||
    any(management$above_fmsy != (management$f_recent_fmsy > 1))) {
  stop("The WCPFC BET management quantities do not reproduce from the public series.", call. = FALSE)
}
if (any(abs(fit$tau - design$tag_tau[match(fit$ensemble_id, design$ensemble_id)]) > 1e-10)) {
  stop("Fitted and designed tau values differ.", call. = FALSE)
}
if (any(!fit$hessian_status %in% c("PDH", "Near-PDH"))) {
  stop("Unexpected Hessian status in public data.", call. = FALSE)
}
if (sum(fit$positive_definite_hessian) != 68L) {
  stop("Expected 68 positive-definite Hessians.", call. = FALSE)
}
if (sum(fit$maximum_gradient > 1e-4) != 8L) {
  stop("Expected eight models above the reporting MGC threshold.", call. = FALSE)
}
included_ids <- sort(fit$ensemble_id[fit$maximum_gradient <= 1e-4])
included_fit <- fit[fit$ensemble_id %in% included_ids, , drop = FALSE]
if (length(included_ids) != 80L ||
    sum(included_fit$positive_definite_hessian) != 62L ||
    sum(!included_fit$positive_definite_hessian) != 18L) {
  stop("The MGC-filtered reporting ensemble must contain 80 models: 62 PDH and 18 Near-PDH.", call. = FALSE)
}

hessian <- readRDS("data/estimation/native-hessian-uncertainty.rds")
if (length(hessian$pdh_model_ids) != 68L ||
    length(hessian$near_pdh_model_ids) != 20L ||
    hessian$draws_per_pdh_model != 100L ||
    nrow(hessian$annual_draws) != 68L * 100L * 73L ||
    nrow(hessian$management_draws) != 68L * 100L) {
  stop("The Hessian uncertainty cache has unexpected dimensions.", call. = FALSE)
}
if (!setequal(hessian$pdh_model_ids, fit$ensemble_id[fit$positive_definite_hessian]) ||
    !setequal(hessian$near_pdh_model_ids, fit$ensemble_id[!fit$positive_definite_hessian])) {
  stop("Hessian-draw model identifiers do not match the fit audit.", call. = FALSE)
}
if (any(!is.finite(hessian$annual_draws$depletion)) ||
    max(abs(
      hessian$annual_draws$depletion -
        hessian$annual_draws$spawning_potential /
          hessian$annual_draws$spawning_potential_noeff
    )) > 1e-12 ||
    any(!is.finite(unlist(hessian$management_draws[c(
      "sb_recent_sb0", "sb_recent_sbmsy", "f_recent_fmsy",
      "historical_target_depletion", "recent_historical_target_ratio"
    )]))) ||
    any(c("sb_recent_sbmsy_native", "f_recent_fmsy_native") %in%
        names(hessian$management_draws)) ||
    nrow(hessian$equilibrium_metadata) != 68L ||
    max(hessian$equilibrium_metadata$central_f_recent_fmsy_relative_error) > 1e-3 ||
    max(hessian$equilibrium_metadata$central_sb_recent_sbmsy_relative_error) > 5e-3) {
  stop("The exact joint Hessian uncertainty cache is invalid.", call. = FALSE)
}

projection <- readRDS("data/projection/native-projections.rds")
quarterly_conditioning <- read.csv(
  "data/projection/fishery-quarter-conditioning.csv", check.names = FALSE
)
if (projection$schema_version != "1.1.0" ||
    projection$projection_complete_models != 88L ||
    projection$projection_incomplete_models != 0L ||
    length(projection$failed_projection_ids) != 0L ||
    projection$simulations_per_model != 10L ||
    !setequal(projection$ensemble_ids, fit$ensemble_id) ||
    !setequal(projection$projection_years, 2025:2054)) {
  stop("The native projection cache is not the complete 88-model, 10-sequence ensemble.", call. = FALSE)
}
if (nrow(projection$annual_stock) != 88L * 10L * 30L ||
    nrow(projection$terminal_msy) != 88L * 10L ||
    nrow(projection$catch_msy) != 88L * 10L * 30L ||
    nrow(projection$historical_region) != 88L * 73L * 5L) {
  stop("The native projection cache has unexpected dimensions.", call. = FALSE)
}
if (any(!is.finite(unlist(projection$catch_msy[c(
      "catch_biomass_mt", "annual_msy_mt", "catch_msy"
    )]))) || any(projection$catch_msy$annual_msy_mt <= 0) ||
    any(projection$catch_msy$catch_msy < 0) ||
    any(!is.finite(projection$historical_region$spawning_biomass_mt)) ||
    any(projection$historical_region$spawning_biomass_mt <= 0)) {
  stop("Catch/MSY or historical regional projection diagnostics are invalid.")
}
catch_msy_balance_error <- max(abs(
  projection$catch_msy$catch_msy -
    projection$catch_msy$catch_biomass_mt /
      projection$catch_msy$annual_msy_mt
))
annual_msy_ranges <- aggregate(
  annual_msy_mt ~ ensemble_id + simulation,
  projection$catch_msy,
  function(value) diff(range(value))
)
if (catch_msy_balance_error > 1e-12 ||
    any(annual_msy_ranges$annual_msy_mt > 1e-8)) {
  stop("Catch/MSY does not reproduce from a path-specific constant annual MSY.")
}
if (nrow(projection$conditioning) != 33L ||
    !identical(projection$conditioning$fishery, 1:33) ||
    any(projection$conditioning$caeff != 1L) ||
    any(projection$conditioning$conditioning != "catch") ||
    !identical(
      projection$conditioning$catch_unit,
      c(rep("number", 11L), rep("weight", 17L), rep("number", 5L))
    ) ||
    any(!is.finite(projection$conditioning$mean_annual_catch)) ||
    any(projection$conditioning$mean_annual_catch < 0) ||
    !identical(
      projection$conditioning$fishery[
        projection$conditioning$mean_annual_catch == 0
      ],
      9L
    )) {
  stop("The fishery-specific projection conditioning is invalid.", call. = FALSE)
}
expected_quarterly_columns <- c(
  "fishery", "fishery_name", "region", "quarter", "month", "catch_unit",
  "mean_quarterly_catch", "exact_zero"
)
quarterly_coverage <- table(quarterly_conditioning$fishery)
quarterly_annual <- aggregate(
  mean_quarterly_catch ~ fishery, quarterly_conditioning, sum
)
quarterly_annual <- quarterly_annual$mean_quarterly_catch[
  match(projection$conditioning$fishery, quarterly_annual$fishery)
]
quarterly_units <- unique(quarterly_conditioning[c("fishery", "catch_unit")])
quarterly_units <- quarterly_units$catch_unit[
  match(projection$conditioning$fishery, quarterly_units$fishery)
]
if (!identical(names(quarterly_conditioning), expected_quarterly_columns) ||
    nrow(quarterly_conditioning) != 33L * 4L ||
    !identical(as.integer(names(quarterly_coverage)), 1:33) ||
    any(quarterly_coverage != 4L) ||
    !identical(sort(unique(quarterly_conditioning$quarter)), 1:4) ||
    any(!is.finite(quarterly_conditioning$mean_quarterly_catch)) ||
    any(quarterly_conditioning$mean_quarterly_catch < 0) ||
    any(quarterly_conditioning$exact_zero !=
      (quarterly_conditioning$mean_quarterly_catch == 0)) ||
    sum(quarterly_conditioning$exact_zero) != 25L ||
    !identical(
      sort(unique(quarterly_conditioning$fishery[
        quarterly_conditioning$exact_zero
      ])),
      c(2L, 7L, 8L, 9L, 12L, 15L, 18L, 19L, 22L, 24L)
    ) ||
    max(abs(
      quarterly_annual - projection$conditioning$mean_annual_catch
    )) > 1e-8 ||
    !identical(quarterly_units, projection$conditioning$catch_unit)) {
  stop("The public fishery-quarter conditioning audit is invalid.")
}
catch_by_model <- split(projection$annual_catch$catch, projection$annual_catch$ensemble_id)
if (length(catch_by_model) != 88L ||
    any(!vapply(catch_by_model, function(value) {
      length(value) == 30L && max(abs(value - value[[1L]])) < 1e-6
    }, logical(1))) ||
    any(abs(projection$metadata$mixed_unit_audit_sum -
      vapply(catch_by_model, `[[`, numeric(1), 1L)) > 1e-6)) {
  stop("The mixed-unit projection catch audit is not constant through time.", call. = FALSE)
}
terminal_recent_sb <- aggregate(
  spawning_biomass_mt ~ ensemble_id + simulation,
  projection$annual_stock[projection$annual_stock$year %in% 2051:2054, ],
  mean
)
terminal_check <- merge(
  projection$terminal_msy, terminal_recent_sb,
  by = c("ensemble_id", "simulation"), sort = FALSE
)
terminal_error <- with(
  terminal_check,
  abs(terminal_sb_sbmsy - spawning_biomass_mt / sbmsy_mt)
)
if (nrow(terminal_check) != 88L * 10L || max(terminal_error) > 1e-12 ||
    any(abs(
      projection$terminal_msy$terminal_f_fmsy -
        1 / projection$terminal_msy$f_multiplier_at_msy
    ) > 1e-12)) {
  stop("Terminal MSY-based management quantities do not reproduce exactly.")
}
projection_management <- build_projection_management(series, projection)
if (nrow(projection_management$projected) != 88L * 10L * 30L ||
    !setequal(projection_management$projected$year, 2025:2054) ||
    any(!is.finite(projection_management$projected$sb_recent_sb0)) ||
    any(projection_management$projected$below_lrp_020 !=
      (projection_management$projected$sb_recent_sb0 < 0.20)) ||
    any(projection_management$projected$below_historical_objective !=
      (projection_management$projected$recent_historical_target_ratio < 1))) {
  stop("The projected WCPFC management quantities are invalid.", call. = FALSE)
}

reporting_projection <- projection
for (component in names(reporting_projection)) {
  value <- reporting_projection[[component]]
  if (is.data.frame(value) && "ensemble_id" %in% names(value)) {
    reporting_projection[[component]] <- value[
      value$ensemble_id %in% included_ids, , drop = FALSE
    ]
  }
}
reporting_projection$ensemble_ids <- included_ids
reporting_projection$projection_complete_models <- length(included_ids)
if (nrow(reporting_projection$annual_stock) != 80L * 10L * 30L ||
    nrow(reporting_projection$terminal_msy) != 80L * 10L ||
    nrow(reporting_projection$catch_msy) != 80L * 10L * 30L ||
    nrow(reporting_projection$historical_region) != 80L * 73L * 5L ||
    !setequal(reporting_projection$annual_stock$ensemble_id, included_ids)) {
  stop("The MGC-filtered reporting projection has unexpected dimensions.", call. = FALSE)
}
included_hessian_ids <- intersect(hessian$pdh_model_ids, included_ids)
if (length(included_hessian_ids) != 62L ||
    nrow(hessian$annual_draws[
      hessian$annual_draws$ensemble_id %in% included_hessian_ids, , drop = FALSE
    ]) != 62L * 100L * 73L ||
    nrow(hessian$management_draws[
      hessian$management_draws$ensemble_id %in% included_hessian_ids, , drop = FALSE
    ]) != 62L * 100L) {
  stop("The MGC-filtered Hessian uncertainty subset has unexpected dimensions.", call. = FALSE)
}

regional_sum <- stats::aggregate(
  spawning_biomass_mt ~ ensemble_id + year,
  projection$historical_region, sum
)
regional_sum$spawning_potential <- regional_sum$spawning_biomass_mt / 1000
regional_check <- merge(
  regional_sum,
  series[c("ensemble_id", "year", "spawning_potential")],
  by = c("ensemble_id", "year"), suffixes = c("_regional", "_stock")
)
regional_error <- with(
  regional_check,
  abs(spawning_potential_regional - spawning_potential_stock) /
    pmax(abs(spawning_potential_stock), .Machine$double.eps)
)
if (max(regional_error) > 1e-3) {
  stop("Historical regional spawning biomass does not reproduce stock totals.")
}

cat(sprintf(
  paste0(
    "Validated the source caches and the %d-model reporting ensemble ",
    "(%d PDH; %d Near-PDH), 1952--2024; %d x 100 Hessian draws ",
    "and %d x 10 stochastic projections used in the report.\n"
  ),
  length(included_ids), sum(included_fit$positive_definite_hessian),
  sum(!included_fit$positive_definite_hessian), length(included_hessian_ids),
  length(included_ids)
))
