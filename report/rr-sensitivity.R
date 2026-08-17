#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)
set.seed(20260817)

required_packages <- c("ggplot2", "patchwork", "ragg", "scales", "jsonlite", "MASS")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Install RR-sensitivity report dependencies: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_arg) != 1L) {
  stop("Cannot locate the repository root.", call. = FALSE)
}
repo_dir <- normalizePath(
  file.path(dirname(sub("^--file=", "", script_arg)), ".."),
  mustWork = TRUE
)
old_wd <- setwd(repo_dir)
on.exit(setwd(old_wd), add = TRUE)

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})
source("report/management-quantities.R")

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
  invisible(TRUE)
}

required_inputs <- c(
  "design/model-draws.csv",
  "data/ensemble/successful-model-design.csv",
  "data/ensemble/fit-diagnostics.csv",
  "data/ensemble/management-quantities.csv",
  "data/ensemble/ensemble-timeseries.rds",
  "data/estimation/native-hessian-uncertainty.rds",
  "data/projection/native-projections.rds",
  "data/diagnostic/dynamic-status.csv"
)
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs)) {
  stop(
    "Missing RR-sensitivity input: ", paste(missing_inputs, collapse = ", "),
    call. = FALSE
  )
}

planned_design <- read.csv(required_inputs[[1L]], check.names = FALSE)
design_all <- read.csv(required_inputs[[2L]], check.names = FALSE)
fit_all <- read.csv(required_inputs[[3L]], check.names = FALSE)
management_all <- read.csv(required_inputs[[4L]], check.names = FALSE)
series_all <- readRDS(required_inputs[[5L]])
hessian <- readRDS(required_inputs[[6L]])
projection_all <- readRDS(required_inputs[[7L]])
diagnostic_dynamic <- read.csv(required_inputs[[8L]], check.names = FALSE)

required_design_columns <- c(
  "ensemble_id", "tag_reporting_flag2", "tag_reporting", "steepness",
  "tag_tau", "tag_mixing_k_cutoff", "m_age40_quarterly",
  "effort_creep_primary", "effort_creep_secondary"
)
required_fit_columns <- c(
  "ensemble_id", "maximum_gradient", "positive_definite_hessian"
)
required_management_columns <- c(
  "ensemble_id", "sb_recent_sb0", "sb_recent_sbmsy", "f_recent_fmsy",
  "below_lrp_020", "below_sbmsy", "above_fmsy"
)
required_series_columns <- c(
  "ensemble_id", "year", "depletion", "spawning_potential",
  "spawning_potential_nofish", "recruitment", "fishing_mortality"
)
for (check in list(
  planned_design = list(data = planned_design, columns = required_design_columns),
  design = list(data = design_all, columns = required_design_columns),
  fit = list(data = fit_all, columns = required_fit_columns),
  management = list(data = management_all, columns = required_management_columns),
  series = list(data = series_all, columns = required_series_columns)
)) {
  absent <- setdiff(check$columns, names(check$data))
  if (length(absent)) {
    stop("Missing RR-sensitivity columns: ", paste(absent, collapse = ", "), call. = FALSE)
  }
}
assert_true(
  identical(names(diagnostic_dynamic), c(
    "year", "depletion", "sb_sbmsy", "f_fmsy"
  )) && nrow(diagnostic_dynamic) == 73L &&
    identical(as.integer(diagnostic_dynamic$year), 1952:2024) &&
    all(is.finite(unlist(diagnostic_dynamic))),
  "The shared diagnostic dynamic-status reference is invalid."
)

planned_ids <- sprintf("ensemble-%03d", seq_len(100L))
assert_true(
  nrow(planned_design) == 100L &&
    !anyDuplicated(planned_design$ensemble_id) &&
    setequal(planned_design$ensemble_id, planned_ids),
  "The RR sensitivity requires the exact 100-model planned design."
)
assert_true(
  identical(sort(unique(as.integer(planned_design$tag_reporting_flag2))), 0:1) &&
    all(planned_design$tag_reporting == ifelse(
      planned_design$tag_reporting_flag2 == 0L, "inclusion", "exclusion"
    )) &&
    identical(
      as.integer(table(factor(planned_design$tag_reporting_flag2, levels = 0:1))),
      c(50L, 50L)
    ),
  "Tag-reporting encoding must be 0 = inclusion and 1 = exclusion, with 50 planned models each."
)

public_ids <- sort(fit_all$ensemble_id)
assert_true(
  length(public_ids) == 88L &&
    !anyDuplicated(public_ids) &&
    nrow(design_all) == 88L && !anyDuplicated(design_all$ensemble_id) &&
    nrow(management_all) == 88L && !anyDuplicated(management_all$ensemble_id) &&
    setequal(public_ids, design_all$ensemble_id) &&
    setequal(public_ids, management_all$ensemble_id) &&
    setequal(public_ids, unique(series_all$ensemble_id)),
  "The public structural payloads must contain the same 88 model identifiers."
)

# Authoritative source audit from docs/retained-final-pars.md. Two completed-PAR
# MGC failures (ensemble-020 and ensemble-096) are not in the 88-row public
# report payload, so the planned/completion/failure accounting cannot be inferred
# from fit_all alone.
documented_incomplete_ids <- c(
  "ensemble-013", "ensemble-026", "ensemble-028", "ensemble-058",
  "ensemble-060", "ensemble-061", "ensemble-064", "ensemble-067",
  "ensemble-077", "ensemble-097"
)
documented_mgc_excluded_ids <- c(
  "ensemble-003", "ensemble-016", "ensemble-017", "ensemble-020",
  "ensemble-037", "ensemble-048", "ensemble-059", "ensemble-083",
  "ensemble-096", "ensemble-100"
)
assert_true(
  length(documented_incomplete_ids) == 10L &&
    length(documented_mgc_excluded_ids) == 10L &&
    !length(intersect(documented_incomplete_ids, documented_mgc_excluded_ids)) &&
    all(c(documented_incomplete_ids, documented_mgc_excluded_ids) %in% planned_ids),
  "The authoritative non-retained source-ID audit is invalid."
)

retained_ids <- sort(fit_all$ensemble_id[fit_all$maximum_gradient <= 1e-4])
authoritative_retained_ids <- sort(setdiff(
  planned_ids, c(documented_incomplete_ids, documented_mgc_excluded_ids)
))
assert_true(
  length(retained_ids) == 80L && identical(retained_ids, authoritative_retained_ids),
  "The MGC <= 1e-4 public set does not match the authoritative 80 retained models."
)
observed_mgc_excluded <- intersect(documented_mgc_excluded_ids, public_ids)
assert_true(
  length(observed_mgc_excluded) == 8L &&
    all(fit_all$maximum_gradient[
      match(observed_mgc_excluded, fit_all$ensemble_id)
    ] > 1e-4) &&
    setequal(
      setdiff(documented_mgc_excluded_ids, public_ids),
      c("ensemble-020", "ensemble-096")
    ),
  "The public and authoritative MGC-exclusion audits do not reconcile."
)

planned_rr <- setNames(
  as.integer(planned_design$tag_reporting_flag2), planned_design$ensemble_id
)
assert_true(
  all(as.integer(design_all$tag_reporting_flag2) == planned_rr[design_all$ensemble_id]) &&
    all(design_all$tag_reporting == ifelse(
      design_all$tag_reporting_flag2 == 0L, "inclusion", "exclusion"
    )),
  "The public design must retain the planned tag-reporting treatment labels."
)
retained_rr <- planned_rr[retained_ids]
group_keys <- c("combined", "inclusion", "exclusion")
group_labels <- c(
  combined = "Combined (RR=0 + RR=1)",
  inclusion = "RR=0: inclusion",
  exclusion = "RR=1: exclusion"
)
group_short_labels <- c(
  combined = "Combined", inclusion = "RR=0", exclusion = "RR=1"
)
group_ids <- list(
  combined = retained_ids,
  inclusion = sort(names(retained_rr)[retained_rr == 0L]),
  exclusion = sort(names(retained_rr)[retained_rr == 1L])
)
assert_true(
  length(group_ids$combined) == 80L &&
    length(group_ids$inclusion) == 34L &&
    length(group_ids$exclusion) == 46L &&
    !length(intersect(group_ids$inclusion, group_ids$exclusion)) &&
    setequal(group_ids$combined, c(group_ids$inclusion, group_ids$exclusion)),
  "The retained RR subsets must be 34 inclusion and 46 exclusion models."
)

expected_group_counts <- data.frame(
  group = group_keys,
  planned_models = c(100L, 50L, 50L),
  public_payload_models = c(88L, 41L, 47L),
  completed_final_par_models = c(90L, 43L, 47L),
  retained_models = c(80L, 34L, 46L),
  documented_mgc_excluded = c(10L, 9L, 1L),
  documented_incomplete = c(10L, 7L, 3L),
  pdh_models = c(62L, 25L, 37L),
  near_pdh_models = c(18L, 9L, 9L),
  hessian_mixture_rows = c(8000L, 3400L, 4600L),
  projection_combinations = c(800L, 340L, 460L),
  stringsAsFactors = FALSE
)

group_selector <- function(ids, key) {
  if (key == "combined") return(rep(TRUE, length(ids)))
  flag <- if (key == "inclusion") 0L else 1L
  as.integer(planned_rr[ids]) == flag
}

draws_per_model <- as.integer(hessian$draws_per_pdh_model)
assert_true(draws_per_model == 100L, "Every PDH model must have exactly 100 Hessian draws.")
assert_true(
  projection_all$simulations_per_model == 10L &&
    projection_all$projection_complete_models == 88L &&
    setequal(projection_all$ensemble_ids, public_ids),
  "The stochastic-projection cache must contain ten paths for every public model."
)

fit_retained <- fit_all[match(retained_ids, fit_all$ensemble_id), , drop = FALSE]
design_retained <- design_all[match(retained_ids, design_all$ensemble_id), , drop = FALSE]
management_retained <- management_all[
  match(retained_ids, management_all$ensemble_id), , drop = FALSE
]
series_retained <- series_all[series_all$ensemble_id %in% retained_ids, , drop = FALSE]
structural_series_counts <- table(interaction(
  series_retained$ensemble_id, series_retained$year, drop = TRUE
))
assert_true(
  nrow(management_retained) == 80L && !anyDuplicated(management_retained$ensemble_id) &&
    nrow(series_retained) == 80L * 73L &&
    length(structural_series_counts) == 80L * 73L &&
    all(structural_series_counts == 1L),
  "The retained structural payload must give each model one management row and one row per model-year."
)

pdh_ids <- intersect(retained_ids, hessian$pdh_model_ids)
near_pdh_ids <- intersect(retained_ids, hessian$near_pdh_model_ids)
assert_true(
  length(pdh_ids) == 62L && length(near_pdh_ids) == 18L &&
    !length(intersect(pdh_ids, near_pdh_ids)) &&
    setequal(retained_ids, c(pdh_ids, near_pdh_ids)) &&
    setequal(pdh_ids, fit_retained$ensemble_id[fit_retained$positive_definite_hessian]) &&
    setequal(near_pdh_ids, fit_retained$ensemble_id[!fit_retained$positive_definite_hessian]),
  "The retained Hessian audit must contain 62 PDH and 18 Near-PDH models."
)

core_management_columns <- c(
  "sb_recent_sb0", "sb_recent_sbmsy", "f_recent_fmsy"
)
pdh_management <- hessian$management_draws[
  hessian$management_draws$ensemble_id %in% pdh_ids,
  c("ensemble_id", "draw", core_management_columns),
  drop = FALSE
]
near_management <- merge(
  management_retained[
    management_retained$ensemble_id %in% near_pdh_ids,
    c("ensemble_id", core_management_columns),
    drop = FALSE
  ],
  data.frame(draw = seq_len(draws_per_model)),
  by = NULL
)
near_management <- near_management[
  c("ensemble_id", "draw", core_management_columns)
]
hybrid_management <- rbind(pdh_management, near_management)
hybrid_management <- hybrid_management[
  order(hybrid_management$ensemble_id, hybrid_management$draw), , drop = FALSE
]
management_weight_counts <- table(hybrid_management$ensemble_id)
assert_true(
  length(management_weight_counts) == 80L &&
    all(management_weight_counts == draws_per_model),
  "The Hessian-inclusive mixture must give every retained model exactly 100 rows."
)

annual_columns <- c("depletion", "spawning_potential", "recruitment")
pdh_annual <- hessian$annual_draws[
  hessian$annual_draws$ensemble_id %in% pdh_ids,
  c("ensemble_id", "draw", "year", annual_columns),
  drop = FALSE
]
near_annual <- merge(
  series_retained[
    series_retained$ensemble_id %in% near_pdh_ids,
    c("ensemble_id", "year", annual_columns),
    drop = FALSE
  ],
  data.frame(draw = seq_len(draws_per_model)),
  by = NULL
)
near_annual <- near_annual[
  c("ensemble_id", "draw", "year", annual_columns)
]
hybrid_annual <- rbind(pdh_annual, near_annual)
annual_weight_counts <- table(interaction(
  hybrid_annual$ensemble_id, hybrid_annual$year, drop = TRUE
))
assert_true(
  length(annual_weight_counts) == 80L * 73L &&
    all(annual_weight_counts == draws_per_model),
  "The annual Hessian-inclusive mixture must give every model-year exactly 100 rows."
)

projection <- projection_all
for (name in names(projection)) {
  value <- projection[[name]]
  if (is.data.frame(value) && "ensemble_id" %in% names(value)) {
    projection[[name]] <- value[
      value$ensemble_id %in% retained_ids, , drop = FALSE
    ]
  }
}
projection$ensemble_ids <- retained_ids
projection$projection_complete_models <- length(retained_ids)
assert_true(
  nrow(projection$annual_stock) == 80L * 10L * 30L &&
    nrow(projection$annual_region) == 80L * 10L * 30L * 5L &&
    nrow(projection$terminal_msy) == 80L * 10L &&
    nrow(projection$catch_msy) == 80L * 10L * 30L &&
    all(table(projection$terminal_msy$ensemble_id) == 10L),
  "The retained projection subset does not provide ten complete paths per model."
)
projection_management <- build_projection_management(series_retained, projection)
assert_true(
  nrow(projection_management$projected) == 80L * 10L * 30L &&
    all(table(interaction(
      projection_management$projected$ensemble_id,
      projection_management$projected$year,
      drop = TRUE
    )) == 10L),
  "Projected management quantities must contain ten equally weighted paths per model-year."
)

count_rows <- lapply(group_keys, function(key) {
  ids <- group_ids[[key]]
  flag <- if (key == "combined") NA_integer_ else if (key == "inclusion") 0L else 1L
  planned <- if (is.na(flag)) planned_ids else names(planned_rr)[planned_rr == flag]
  documented_incomplete <- intersect(planned, documented_incomplete_ids)
  documented_mgc <- intersect(planned, documented_mgc_excluded_ids)
  public <- intersect(planned, public_ids)
  pdh <- intersect(ids, pdh_ids)
  near <- intersect(ids, near_pdh_ids)
  data.frame(
    Group = group_labels[[key]],
    RR_flag2 = if (is.na(flag)) "0 + 1" else as.character(flag),
    Planned_models = length(planned),
    Public_payload_models = length(public),
    Completed_final_PAR_models = length(setdiff(planned, documented_incomplete)),
    Retained_models = length(ids),
    Documented_MGC_excluded = length(documented_mgc),
    Documented_incomplete = length(documented_incomplete),
    PDH_models = length(pdh),
    Near_PDH_models = length(near),
    Hessian_draws_per_PDH = draws_per_model,
    Hessian_mixture_rows = length(ids) * draws_per_model,
    Projection_sequences_per_model = projection$simulations_per_model,
    Projection_combinations = length(ids) * projection$simulations_per_model,
    Within_group_model_weight = 1 / length(ids),
    Share_of_combined = length(ids) / length(retained_ids),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
})
group_qc <- do.call(rbind, count_rows)
rownames(group_qc) <- NULL
for (column in names(expected_group_counts)[-1L]) {
  observed_name <- c(
    planned_models = "Planned_models",
    public_payload_models = "Public_payload_models",
    completed_final_par_models = "Completed_final_PAR_models",
    retained_models = "Retained_models",
    documented_mgc_excluded = "Documented_MGC_excluded",
    documented_incomplete = "Documented_incomplete",
    pdh_models = "PDH_models",
    near_pdh_models = "Near_PDH_models",
    hessian_mixture_rows = "Hessian_mixture_rows",
    projection_combinations = "Projection_combinations"
  )[[column]]
  assert_true(
    identical(
      as.integer(group_qc[[observed_name]]),
      as.integer(expected_group_counts[[column]])
    ),
    paste0("Unexpected RR group count: ", column, ".")
  )
}
assert_true(
  abs(group_qc$Share_of_combined[group_qc$RR_flag2 == "0"] - 34 / 80) < 1e-15 &&
    abs(group_qc$Share_of_combined[group_qc$RR_flag2 == "1"] - 46 / 80) < 1e-15,
  "The combined result must preserve 34/80 RR=0 and 46/80 RR=1 equal-model weight."
)

output_dir <- Sys.getenv("REPORT_OUTPUT_DIR", "results")
rr_dir <- file.path(output_dir, "rr-sensitivity")
figure_dir <- file.path(rr_dir, "figures")
table_dir <- file.path(rr_dir, "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

quantile_values <- function(values) {
  assert_true(length(values) > 0L && all(is.finite(values)), "Cannot summarize non-finite values.")
  c(
    q025 = stats::quantile(values, 0.025, names = FALSE),
    q10 = stats::quantile(values, 0.10, names = FALSE),
    q25 = stats::quantile(values, 0.25, names = FALSE),
    median = stats::median(values),
    mean = mean(values),
    q75 = stats::quantile(values, 0.75, names = FALSE),
    q90 = stats::quantile(values, 0.90, names = FALSE),
    q975 = stats::quantile(values, 0.975, names = FALSE)
  )
}

summarise_by <- function(data, value, groups) {
  assert_true(all(c(value, groups) %in% names(data)), "Summary input is missing columns.")
  key <- interaction(data[groups], drop = TRUE, lex.order = TRUE)
  pieces <- split(seq_len(nrow(data)), key)
  out <- do.call(rbind, lapply(pieces, function(index) {
    row <- data[index[[1L]], groups, drop = FALSE]
    statistics <- quantile_values(data[[value]][index])
    for (name in names(statistics)) row[[name]] <- statistics[[name]]
    row
  }))
  rownames(out) <- NULL
  out[do.call(order, out[groups]), , drop = FALSE]
}

management_specs <- data.frame(
  column = core_management_columns,
  quantity = c("SBrecent / SBF=0", "SBrecent / SBMSY", "Frecent / FMSY"),
  period = c(
    "2021–2024 / 2014–2023",
    "2021–2024 / equilibrium SBMSY",
    "2020–2023 / equilibrium FMSY"
  ),
  stringsAsFactors = FALSE
)

structural_summary <- do.call(rbind, lapply(group_keys, function(key) {
  ids <- group_ids[[key]]
  values <- management_retained[management_retained$ensemble_id %in% ids, , drop = FALSE]
  do.call(rbind, lapply(seq_len(nrow(management_specs)), function(index) {
    statistics <- quantile_values(values[[management_specs$column[[index]]]])
    data.frame(
      Group = group_labels[[key]],
      Quantity = management_specs$quantity[[index]],
      Period = management_specs$period[[index]],
      Models = length(ids),
      `10%` = statistics[["q10"]],
      Median = statistics[["median"]],
      Mean = statistics[["mean"]],
      `90%` = statistics[["q90"]],
      check.names = FALSE
    )
  }))
}))
rownames(structural_summary) <- NULL

risk_specs <- data.frame(
  column = c("below_lrp_020", "below_sbmsy", "above_fmsy"),
  criterion = c(
    "SBrecent/SBF=0 < 0.20", "SBrecent/SBMSY < 1", "Frecent/FMSY > 1"
  ),
  stringsAsFactors = FALSE
)
structural_risk <- do.call(rbind, lapply(group_keys, function(key) {
  ids <- group_ids[[key]]
  values <- management_retained[management_retained$ensemble_id %in% ids, , drop = FALSE]
  do.call(rbind, lapply(seq_len(nrow(risk_specs)), function(index) {
    events <- as.logical(values[[risk_specs$column[[index]]]])
    data.frame(
      Group = group_labels[[key]],
      Criterion = risk_specs$criterion[[index]],
      Models = length(ids),
      Events = sum(events),
      Probability = mean(events),
      stringsAsFactors = FALSE
    )
  }))
}))
rownames(structural_risk) <- NULL

hessian_intervals <- do.call(rbind, lapply(group_keys, function(key) {
  ids <- group_ids[[key]]
  values <- hybrid_management[hybrid_management$ensemble_id %in% ids, , drop = FALSE]
  assert_true(
    nrow(values) == length(ids) * draws_per_model &&
      all(table(values$ensemble_id) == draws_per_model),
    paste0("Unequal Hessian-mixture model weight in ", key, ".")
  )
  do.call(rbind, lapply(seq_len(nrow(management_specs)), function(index) {
    statistics <- quantile_values(values[[management_specs$column[[index]]]])
    data.frame(
      Group = group_labels[[key]],
      Quantity = management_specs$quantity[[index]],
      Period = management_specs$period[[index]],
      Models = length(ids),
      PDH_models = length(intersect(ids, pdh_ids)),
      Near_PDH_models = length(intersect(ids, near_pdh_ids)),
      Mixture_rows = nrow(values),
      `2.5%` = statistics[["q025"]],
      `10%` = statistics[["q10"]],
      `25%` = statistics[["q25"]],
      Median = statistics[["median"]],
      `75%` = statistics[["q75"]],
      `90%` = statistics[["q90"]],
      `97.5%` = statistics[["q975"]],
      check.names = FALSE
    )
  }))
}))
rownames(hessian_intervals) <- NULL

hessian_risk <- do.call(rbind, lapply(group_keys, function(key) {
  ids <- group_ids[[key]]
  values <- hybrid_management[hybrid_management$ensemble_id %in% ids, , drop = FALSE]
  events <- list(
    values$sb_recent_sb0 < 0.20,
    values$sb_recent_sbmsy < 1,
    values$f_recent_fmsy > 1
  )
  do.call(rbind, lapply(seq_len(nrow(risk_specs)), function(index) {
    data.frame(
      Group = group_labels[[key]],
      Criterion = risk_specs$criterion[[index]],
      Models = length(ids),
      Mixture_rows = nrow(values),
      Probability = mean(events[[index]]),
      stringsAsFactors = FALSE
    )
  }))
}))
rownames(hessian_risk) <- NULL

terminal_depletion <- projection_management$projected[
  projection_management$projected$year == max(projection$projection_years),
  c("ensemble_id", "simulation", "sb_recent_sb0"),
  drop = FALSE
]
terminal <- merge(
  projection$terminal_msy,
  terminal_depletion,
  by = c("ensemble_id", "simulation"),
  sort = FALSE
)
assert_true(
  nrow(terminal) == 80L * 10L &&
    all(table(terminal$ensemble_id) == projection$simulations_per_model) &&
    all(is.finite(terminal$terminal_sb_sbmsy)) &&
    all(is.finite(terminal$terminal_f_fmsy)) &&
    all(is.finite(terminal$sb_recent_sb0)),
  "Terminal projection quantities are incomplete."
)

terminal_specs <- data.frame(
  column = c("terminal_sb_sbmsy", "sb_recent_sb0", "terminal_f_fmsy"),
  quantity = c(
    "SB2051–2054 / SBMSY", "SB2051–2054 / SBF=0", "F2050–2053 / FMSY"
  ),
  criterion = c(
    "SB2051–2054/SBMSY < 1", "SB2051–2054/SBF=0 < 0.20",
    "F2050–2053/FMSY > 1"
  ),
  direction = c("below_1", "below_020", "above_1"),
  stringsAsFactors = FALSE
)
projection_terminal_summary <- do.call(rbind, lapply(group_keys, function(key) {
  ids <- group_ids[[key]]
  values <- terminal[terminal$ensemble_id %in% ids, , drop = FALSE]
  assert_true(
    nrow(values) == length(ids) * projection$simulations_per_model &&
      all(table(values$ensemble_id) == projection$simulations_per_model),
    paste0("Unequal projection model weight in ", key, ".")
  )
  do.call(rbind, lapply(seq_len(nrow(terminal_specs)), function(index) {
    x <- values[[terminal_specs$column[[index]]]]
    statistics <- quantile_values(x)
    beyond <- switch(
      terminal_specs$direction[[index]],
      below_1 = x < 1,
      below_020 = x < 0.20,
      above_1 = x > 1
    )
    data.frame(
      Group = group_labels[[key]],
      Quantity = terminal_specs$quantity[[index]],
      Models = length(ids),
      Projection_combinations = nrow(values),
      `10%` = statistics[["q10"]],
      Median = statistics[["median"]],
      Mean = statistics[["mean"]],
      `90%` = statistics[["q90"]],
      Criterion = terminal_specs$criterion[[index]],
      Beyond_criterion_probability = mean(beyond),
      check.names = FALSE
    )
  }))
}))
rownames(projection_terminal_summary) <- NULL

projection_years_reported <- c(2030L, 2040L, 2054L)
projection_selected_years <- do.call(rbind, lapply(group_keys, function(key) {
  ids <- group_ids[[key]]
  depletion <- projection_management$projected[
    projection_management$projected$ensemble_id %in% ids &
      projection_management$projected$year %in% projection_years_reported,
    , drop = FALSE
  ]
  spawning <- projection$annual_stock[
    projection$annual_stock$ensemble_id %in% ids &
      projection$annual_stock$year %in% projection_years_reported,
    , drop = FALSE
  ]
  catch <- projection$catch_msy[
    projection$catch_msy$ensemble_id %in% ids &
      projection$catch_msy$year %in% projection_years_reported,
    , drop = FALSE
  ]
  do.call(rbind, lapply(projection_years_reported, function(year) {
    d <- depletion[depletion$year == year, ]
    s <- spawning[spawning$year == year, ]
    c <- catch[catch$year == year, ]
    assert_true(
      nrow(d) == length(ids) * 10L && nrow(s) == length(ids) * 10L &&
        nrow(c) == length(ids) * 10L,
      paste0("Projection-year coverage is incomplete for ", key, " / ", year, ".")
    )
    d_stats <- quantile_values(d$sb_recent_sb0)
    s_stats <- quantile_values(s$spawning_biomass_mt / 1000)
    c_stats <- quantile_values(c$catch_msy)
    data.frame(
      Group = group_labels[[key]], Year = year,
      Depletion_q10 = d_stats[["q10"]],
      Depletion_median = d_stats[["median"]],
      Depletion_q90 = d_stats[["q90"]],
      Probability_below_LRP = mean(d$below_lrp_020),
      Spawning_q10_kt = s_stats[["q10"]],
      Spawning_median_kt = s_stats[["median"]],
      Spawning_q90_kt = s_stats[["q90"]],
      Catch_MSY_q10 = c_stats[["q10"]],
      Catch_MSY_median = c_stats[["median"]],
      Catch_MSY_q90 = c_stats[["q90"]],
      stringsAsFactors = FALSE
    )
  }))
}))
rownames(projection_selected_years) <- NULL

make_difference_rows <- function(data, domain, keys, measurements) {
  combined <- data[data$Group == group_labels[["combined"]], , drop = FALSE]
  subsets <- data[data$Group != group_labels[["combined"]], , drop = FALSE]
  out <- list()
  counter <- 0L
  for (index in seq_len(nrow(subsets))) {
    matching <- rep(TRUE, nrow(combined))
    for (key in keys) matching <- matching & combined[[key]] == subsets[[key]][[index]]
    assert_true(sum(matching) == 1L, paste0("Missing combined baseline for ", domain, "."))
    baseline <- combined[matching, , drop = FALSE]
    for (measurement in measurements) {
      counter <- counter + 1L
      value <- as.numeric(subsets[[measurement]][[index]])
      base <- as.numeric(baseline[[measurement]][[1L]])
      key_text <- paste(
        vapply(keys, function(key) as.character(subsets[[key]][[index]]), character(1)),
        collapse = " | "
      )
      out[[counter]] <- data.frame(
        Group = subsets$Group[[index]], Domain = domain,
        Metric = key_text, Statistic = measurement,
        Value = value, Combined_value = base, Difference = value - base,
        Relative_difference = if (base == 0) NA_real_ else (value - base) / base,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}

differences_vs_combined <- rbind(
  make_difference_rows(
    structural_summary, "Current structural management", "Quantity", "Median"
  ),
  make_difference_rows(
    structural_risk, "Current structural risk", "Criterion", "Probability"
  ),
  make_difference_rows(
    hessian_intervals, "Current structure + available estimation", "Quantity", "Median"
  ),
  make_difference_rows(
    hessian_risk, "Current risk + available estimation", "Criterion", "Probability"
  ),
  make_difference_rows(
    projection_terminal_summary, "Terminal projection", "Quantity",
    c("Median", "Beyond_criterion_probability")
  ),
  make_difference_rows(
    projection_selected_years, "Projection selected years", "Year",
    c(
      "Depletion_median", "Probability_below_LRP", "Spawning_median_kt",
      "Catch_MSY_median"
    )
  )
)
rownames(differences_vs_combined) <- NULL

write.csv(group_qc, file.path(table_dir, "group-counts-qc.csv"), row.names = FALSE)
write.csv(
  structural_summary,
  file.path(table_dir, "structural-management-summary.csv"), row.names = FALSE
)
write.csv(
  structural_risk,
  file.path(table_dir, "structural-management-risk.csv"), row.names = FALSE
)
write.csv(
  hessian_intervals,
  file.path(table_dir, "hessian-management-intervals.csv"), row.names = FALSE
)
write.csv(
  hessian_risk,
  file.path(table_dir, "hessian-management-risk.csv"), row.names = FALSE
)
write.csv(
  projection_terminal_summary,
  file.path(table_dir, "projection-terminal-summary.csv"), row.names = FALSE
)
write.csv(
  projection_selected_years,
  file.path(table_dir, "projection-selected-years.csv"), row.names = FALSE
)
write.csv(
  differences_vs_combined,
  file.path(table_dir, "differences-vs-combined.csv"), row.names = FALSE
)

theme_rr <- function(base_size = 12.5) {
  ggplot2::theme_bw(base_size = max(base_size, 11.5), base_family = "serif") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#E1E8EB", linewidth = 0.30),
      panel.border = ggplot2::element_rect(colour = "#2E4857", fill = NA, linewidth = 0.50),
      axis.title = ggplot2::element_text(face = "bold", colour = "#173B4D"),
      axis.text = ggplot2::element_text(colour = "#34576A"),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "#EAF2F4", colour = "#718B97"),
      strip.text = ggplot2::element_text(face = "bold", colour = "#173B4D"),
      plot.title = ggplot2::element_text(face = "bold", colour = "#0A5266"),
      plot.subtitle = ggplot2::element_text(colour = "#4A6877"),
      plot.margin = ggplot2::margin(8, 10, 8, 9)
    )
}

# Match the canonical public-report figure styling for scope-specific copies.
theme_main_report <- function(base_size = 10.8) {
  ggplot2::theme_bw(base_size = base_size, base_family = "serif") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        colour = "#E2E8EB", linewidth = 0.28
      ),
      panel.border = ggplot2::element_rect(
        colour = "#263844", fill = NA, linewidth = 0.45
      ),
      axis.title = ggplot2::element_text(face = "bold", colour = "#183246"),
      axis.text = ggplot2::element_text(colour = "#36566A"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      legend.key.width = grid::unit(1.1, "cm"),
      plot.tag = ggplot2::element_text(
        face = "bold", colour = "#183246", size = 12
      ),
      plot.margin = ggplot2::margin(7, 9, 7, 8)
    )
}

group_colours <- c(
  "Combined" = "#173F5F", "RR=0" = "#D97706", "RR=1" = "#0F8B8D"
)

save_plot <- function(plot, stem, width, height) {
  png <- file.path(figure_dir, paste0(stem, ".png"))
  pdf <- file.path(figure_dir, paste0(stem, ".pdf"))
  ggplot2::ggsave(
    png, plot, width = width, height = height, units = "in", dpi = 300,
    device = ragg::agg_png, bg = "white"
  )
  ggplot2::ggsave(
    pdf, plot, width = width, height = height, units = "in",
    device = grDevices::cairo_pdf, bg = "white"
  )
  c(png = png, pdf = pdf)
}

history_specs <- data.frame(
  column = c(annual_columns, "fishing_mortality"),
  metric = c(
    "Depletion (SBt / SBF=0)", "Spawning potential (10^3 MT)",
    "Recruitment (millions)", "Fishing mortality (year^-1)"
  ),
  estimation = c(TRUE, TRUE, TRUE, FALSE),
  stringsAsFactors = FALSE
)
history_summary <- do.call(rbind, lapply(group_keys, function(key) {
  ids <- group_ids[[key]]
  do.call(rbind, lapply(seq_len(nrow(history_specs)), function(index) {
    spec <- history_specs[index, ]
    data <- if (spec$estimation) {
      hybrid_annual[hybrid_annual$ensemble_id %in% ids, , drop = FALSE]
    } else {
      series_retained[series_retained$ensemble_id %in% ids, , drop = FALSE]
    }
    summary <- summarise_by(data, spec$column, "year")
    summary$Group <- group_short_labels[[key]]
    summary$Metric <- spec$metric
    summary
  }))
}))
history_summary$Group <- factor(
  history_summary$Group, levels = unname(group_short_labels[group_keys])
)
history_summary$Metric <- factor(
  history_summary$Metric, levels = history_specs$metric
)
history_plot <- ggplot2::ggplot(
  history_summary,
  ggplot2::aes(
    x = .data$year, y = .data$median,
    colour = .data$Group, fill = .data$Group
  )
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = .data$q10, ymax = .data$q90),
    alpha = 0.10, colour = NA
  ) +
  ggplot2::geom_line(linewidth = 0.82) +
  ggplot2::geom_hline(
    data = data.frame(
      Metric = factor(history_specs$metric[[1L]], levels = history_specs$metric),
      value = 0.20
    ),
    ggplot2::aes(yintercept = .data$value),
    inherit.aes = FALSE, colour = "#B83232", linetype = "33", linewidth = 0.55
  ) +
  ggplot2::facet_wrap(~Metric, scales = "free_y", ncol = 2) +
  ggplot2::scale_colour_manual(values = group_colours) +
  ggplot2::scale_fill_manual(values = group_colours) +
  ggplot2::scale_x_continuous(breaks = c(1960, 1980, 2000, 2020)) +
  ggplot2::coord_cartesian(ylim = c(0, NA)) +
  ggplot2::labs(
    title = "Historical comparison by pre-mixing reporting treatment",
    subtitle = "Median and central 80% interval; Hessian uncertainty is included where available",
    x = "Year", y = NULL
  ) + theme_rr()
history_files <- save_plot(
  history_plot, "rr-sensitivity-history-comparison", width = 11.4, height = 7.6
)

management_long <- do.call(rbind, lapply(group_keys, function(key) {
  ids <- group_ids[[key]]
  values <- hybrid_management[hybrid_management$ensemble_id %in% ids, , drop = FALSE]
  do.call(rbind, lapply(seq_len(nrow(management_specs)), function(index) {
    data.frame(
      Group = group_short_labels[[key]],
      Quantity = management_specs$quantity[[index]],
      Value = values[[management_specs$column[[index]]]],
      stringsAsFactors = FALSE
    )
  }))
}))
management_long$Group <- factor(
  management_long$Group, levels = unname(group_short_labels[group_keys])
)
management_long$Quantity <- factor(
  management_long$Quantity, levels = management_specs$quantity
)
management_plot <- ggplot2::ggplot(
  management_long,
  ggplot2::aes(x = .data$Group, y = .data$Value, fill = .data$Group)
) +
  ggplot2::geom_violin(
    trim = TRUE, scale = "width", alpha = 0.62, colour = "#294C5C", linewidth = 0.42
  ) +
  ggplot2::geom_boxplot(
    width = 0.16, outlier.shape = NA, fill = "white", alpha = 0.78,
    colour = "#173B4D", linewidth = 0.44
  ) +
  ggplot2::facet_wrap(~Quantity, scales = "free_y", ncol = 3) +
  ggplot2::scale_fill_manual(values = group_colours, guide = "none") +
  ggplot2::labs(
    title = "Current management distributions",
    subtitle = "Equal model weight within each result; PDH draws and Near-PDH point masses have equal total model weight",
    x = NULL, y = NULL
  ) + theme_rr()
management_files <- save_plot(
  management_plot, "rr-sensitivity-management-distributions", width = 12.0, height = 5.1
)

hdr_surface <- function(data, x, y, probability = 0.80, n = 160L) {
  x_values <- data[[x]]
  y_values <- data[[y]]
  keep <- is.finite(x_values) & is.finite(y_values)
  x_values <- x_values[keep]
  y_values <- y_values[keep]
  assert_true(
    length(x_values) >= 20L && stats::sd(x_values) > 0 && stats::sd(y_values) > 0,
    "Insufficient status variation for an HDR."
  )
  hx <- MASS::bandwidth.nrd(x_values)
  hy <- MASS::bandwidth.nrd(y_values)
  assert_true(is.finite(hx) && hx > 0 && is.finite(hy) && hy > 0, "Invalid HDR bandwidth.")
  x_margin <- max(diff(range(x_values)) * 0.10, 4 * hx)
  y_margin <- max(diff(range(y_values)) * 0.10, 4 * hy)
  estimate <- MASS::kde2d(
    x_values, y_values, n = n, h = c(hx, hy),
    lims = c(
      min(x_values) - x_margin, max(x_values) + x_margin,
      min(y_values) - y_margin, max(y_values) + y_margin
    )
  )
  density <- as.vector(estimate$z)
  ordered <- order(density, decreasing = TRUE)
  mass <- cumsum(density[ordered]) / sum(density)
  threshold <- density[ordered][which(mass >= probability)[[1L]]]
  surface <- expand.grid(x = estimate$x, y = estimate$y)
  surface$density <- density
  attr(surface, "threshold") <- threshold
  surface
}

status_panel <- function(key, diagram = c("kobe", "majuro"), show_title = TRUE) {
  diagram <- match.arg(diagram)
  ids <- group_ids[[key]]
  draws <- hybrid_management[hybrid_management$ensemble_id %in% ids, , drop = FALSE]
  central <- Reduce(
    function(x, y) merge(x, y, by = "ensemble_id", sort = FALSE),
    list(
      management_retained[
        management_retained$ensemble_id %in% ids, , drop = FALSE
      ],
      fit_retained[
        fit_retained$ensemble_id %in% ids,
        c("ensemble_id", "maximum_gradient", "positive_definite_hessian"),
        drop = FALSE
      ],
      design_retained[
        design_retained$ensemble_id %in% ids,
        c("ensemble_id", "tag_tau"), drop = FALSE
      ]
    )
  )
  central$hessian_status <- ifelse(
    central$positive_definite_hessian, "PDH", "Near-PDH"
  )
  central$point_alpha <- ifelse(central$maximum_gradient <= 1e-4, 0.90, 0.46)
  x_column <- if (diagram == "kobe") "sb_recent_sbmsy" else "sb_recent_sb0"
  x_boundary <- if (diagram == "kobe") 1 else 0.20
  surface <- hdr_surface(draws, x_column, "f_recent_fmsy")
  x_upper <- max(
    stats::quantile(draws[[x_column]], 0.995, names = FALSE),
    central[[x_column]], x_boundary, na.rm = TRUE
  ) * 1.04
  y_upper <- max(
    stats::quantile(draws$f_recent_fmsy, 0.995, names = FALSE),
    central$f_recent_fmsy, 1, na.rm = TRUE
  ) * 1.04
  background <- if (diagram == "kobe") {
    list(
      ggplot2::annotate("rect", xmin = x_boundary, xmax = Inf, ymin = -Inf, ymax = 1, fill = "#2a9d8f", alpha = 0.50),
      ggplot2::annotate("rect", xmin = -Inf, xmax = x_boundary, ymin = -Inf, ymax = 1, fill = "#e9c46a", alpha = 0.50),
      ggplot2::annotate("rect", xmin = x_boundary, xmax = Inf, ymin = 1, ymax = Inf, fill = "#f4a261", alpha = 0.48),
      ggplot2::annotate("rect", xmin = -Inf, xmax = x_boundary, ymin = 1, ymax = Inf, fill = "#e76f51", alpha = 0.50),
      ggplot2::geom_hline(yintercept = 1, colour = "#1f2937", linewidth = 0.48),
      ggplot2::geom_vline(xintercept = x_boundary, colour = "#1f2937", linewidth = 0.48)
    )
  } else {
    list(
      ggplot2::annotate("rect", xmin = -Inf, xmax = x_boundary, ymin = -Inf, ymax = Inf, fill = "#e76f51", alpha = 0.50),
      ggplot2::annotate("rect", xmin = x_boundary, xmax = Inf, ymin = -Inf, ymax = 1, fill = "#2a9d8f", alpha = 0.50),
      ggplot2::annotate("rect", xmin = x_boundary, xmax = Inf, ymin = 1, ymax = Inf, fill = "#f4a261", alpha = 0.48),
      ggplot2::annotate("segment", x = x_boundary, xend = x_upper, y = 1, yend = 1, colour = "#1f2937", linewidth = 0.48),
      ggplot2::geom_vline(xintercept = x_boundary, colour = "#1f2937", linewidth = 0.48)
    )
  }
  ggplot2::ggplot() +
    background +
    ggplot2::geom_contour(
      data = surface,
      ggplot2::aes(x = .data$x, y = .data$y, z = .data$density),
      breaks = attr(surface, "threshold"), colour = "#073B4C", linewidth = 1.05
    ) +
    ggplot2::geom_point(
      data = central,
      ggplot2::aes(x = .data[[x_column]], y = .data$f_recent_fmsy),
      colour = group_colours[[group_short_labels[[key]]]],
      fill = "white", shape = 21, size = 2.0, stroke = 0.58, alpha = 0.78
    ) +
    ggplot2::annotate(
      "label", x = x_upper * 0.97, y = y_upper * 0.96,
      label = paste0("80% HDR\n", length(ids), " models"),
      hjust = 1, vjust = 1, size = 4.1, colour = "#173B4D",
      fill = "white", alpha = 0.82, linewidth = 0.18
    ) +
    ggplot2::coord_cartesian(
      xlim = c(0, x_upper), ylim = c(0, y_upper), expand = FALSE
    ) +
    ggplot2::labs(
      title = if (show_title) paste(group_short_labels[[key]], toupper(diagram)) else NULL,
      x = if (diagram == "kobe") "SBrecent / SBMSY" else "SBrecent / SBF=0",
      y = "Frecent / FMSY"
    ) + theme_rr(11.8) +
    ggplot2::theme(legend.position = "none")
}

status_plot <- patchwork::wrap_plots(
  unlist(lapply(group_keys, function(key) {
    list(status_panel(key, "kobe"), status_panel(key, "majuro"))
  }), recursive = FALSE),
  ncol = 2
) + patchwork::plot_annotation(
  title = "Current Kobe and Majuro status by reporting treatment",
  subtitle = "Contours are 80% HDRs from the equal-model-weight structural + available estimation mixture"
)
status_files <- save_plot(
  status_plot, "rr-sensitivity-current-status", width = 11.8, height = 11.2
)

projection_plot_summary <- do.call(rbind, lapply(group_keys, function(key) {
  ids <- group_ids[[key]]
  group_label <- group_short_labels[[key]]
  depletion <- projection_management$projected[
    projection_management$projected$ensemble_id %in% ids, , drop = FALSE
  ]
  spawning <- projection$annual_stock[
    projection$annual_stock$ensemble_id %in% ids, , drop = FALSE
  ]
  spawning$spawning_kt <- spawning$spawning_biomass_mt / 1000
  catch <- projection$catch_msy[projection$catch_msy$ensemble_id %in% ids, , drop = FALSE]
  d <- summarise_by(depletion, "sb_recent_sb0", "year")
  d$Metric <- "SBrecent / SBF=0"
  s <- summarise_by(spawning, "spawning_kt", "year")
  s$Metric <- "Spawning potential (10^3 MT)"
  c <- summarise_by(catch, "catch_msy", "year")
  c$Metric <- "Catch / MSY"
  risk <- stats::aggregate(below_lrp_020 ~ year, depletion, mean)
  risk$q025 <- risk$below_lrp_020
  risk$q10 <- risk$below_lrp_020
  risk$q25 <- risk$below_lrp_020
  risk$median <- risk$below_lrp_020
  risk$mean <- risk$below_lrp_020
  risk$q75 <- risk$below_lrp_020
  risk$q90 <- risk$below_lrp_020
  risk$q975 <- risk$below_lrp_020
  risk$below_lrp_020 <- NULL
  risk$Metric <- "Probability below LRP"
  out <- rbind(d, s, c, risk)
  out$Group <- group_label
  out
}))
projection_plot_summary$Group <- factor(
  projection_plot_summary$Group, levels = unname(group_short_labels[group_keys])
)

projection_panel <- function(metric, y_label, probability = FALSE, reference = NULL) {
  data <- projection_plot_summary[projection_plot_summary$Metric == metric, ]
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$year, y = .data$median,
      colour = .data$Group, fill = .data$Group
    )
  )
  if (!probability) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$q10, ymax = .data$q90),
      alpha = 0.10, colour = NA
    )
  }
  p <- p +
    ggplot2::geom_line(linewidth = 0.86) +
    ggplot2::scale_colour_manual(values = group_colours) +
    ggplot2::scale_fill_manual(values = group_colours) +
    ggplot2::scale_x_continuous(breaks = c(2030, 2040, 2050)) +
    ggplot2::labs(x = "Year", y = y_label) + theme_rr(11.8)
  if (probability) {
    p <- p + ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1), limits = c(0, 1)
    )
  } else {
    p <- p + ggplot2::coord_cartesian(ylim = c(0, NA))
  }
  if (!is.null(reference)) {
    p <- p + ggplot2::geom_hline(
      yintercept = reference, colour = "#B83232", linewidth = 0.52,
      linetype = "33"
    )
  }
  p
}

projection_plot <- (
  projection_panel("SBrecent / SBF=0", "SBrecent / SBF=0", reference = 0.20) |
    projection_panel("Spawning potential (10^3 MT)", "Spawning potential (10^3 MT)")
) / (
  projection_panel("Catch / MSY", "Catch / MSY", reference = 1) |
    projection_panel("Probability below LRP", "Simulation frequency", probability = TRUE)
) + patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(
    title = "Stochastic projections by reporting treatment",
    subtitle = "Median and central 80% interval across ten recruitment paths per model; no Hessian draws"
  ) & ggplot2::theme(legend.position = "bottom")
projection_files <- save_plot(
  projection_plot, "rr-sensitivity-projections", width = 11.8, height = 8.0
)

group_key_files <- list()
for (key in group_keys) {
  short <- group_short_labels[[key]]
  history_data <- history_summary[
    history_summary$Group == short &
      history_summary$Metric == history_specs$metric[[1L]],
  ]
  p_history <- ggplot2::ggplot(
    history_data, ggplot2::aes(x = .data$year, y = .data$median)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$q10, ymax = .data$q90),
      fill = group_colours[[short]], alpha = 0.20
    ) +
    ggplot2::geom_line(colour = group_colours[[short]], linewidth = 0.88) +
    ggplot2::geom_hline(
      yintercept = 0.20, colour = "#B83232", linetype = "33", linewidth = 0.52
    ) +
    ggplot2::labs(title = "Historical depletion", x = "Year", y = "SBt / SBF=0") +
    theme_rr(11.8)

  intervals <- hessian_intervals[hessian_intervals$Group == group_labels[[key]], ]
  intervals$Quantity <- factor(intervals$Quantity, levels = rev(management_specs$quantity))
  p_management <- ggplot2::ggplot(
    intervals, ggplot2::aes(x = .data$Median, y = .data$Quantity)
  ) +
    ggplot2::geom_linerange(
      ggplot2::aes(xmin = .data$`10%`, xmax = .data$`90%`),
      colour = "#82BDC7", linewidth = 5.0, lineend = "round"
    ) +
    ggplot2::geom_linerange(
      ggplot2::aes(xmin = .data$`25%`, xmax = .data$`75%`),
      colour = group_colours[[short]], linewidth = 7.0, lineend = "round"
    ) +
    ggplot2::geom_point(colour = "#102F40", size = 2.6) +
    ggplot2::labs(title = "Current management", x = "Ratio", y = NULL) +
    theme_rr(11.8)

  p_status <- status_panel(key, "kobe", show_title = FALSE) +
    ggplot2::labs(title = "Current Kobe status")

  projected <- projection_plot_summary[
    projection_plot_summary$Group == short &
      projection_plot_summary$Metric == "SBrecent / SBF=0",
  ]
  p_projected <- ggplot2::ggplot(
    projected, ggplot2::aes(x = .data$year, y = .data$median)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$q10, ymax = .data$q90),
      fill = group_colours[[short]], alpha = 0.20
    ) +
    ggplot2::geom_line(colour = group_colours[[short]], linewidth = 0.88) +
    ggplot2::geom_hline(
      yintercept = 0.20, colour = "#B83232", linetype = "33", linewidth = 0.52
    ) +
    ggplot2::labs(title = "Projected depletion", x = "Year", y = "SBrecent / SBF=0") +
    theme_rr(11.8)

  key_plot <- (p_history | p_management) / (p_status | p_projected) +
    patchwork::plot_annotation(
      title = paste0(group_labels[[key]], " — key quantities"),
      subtitle = paste0(
        length(group_ids[[key]]),
        " equal-weight models; reporting-rate differences are retained-subset sensitivity, not isolated causal effects"
      )
    )
  group_key_files[[key]] <- save_plot(
    key_plot, paste0("rr-sensitivity-key-", key), width = 11.8, height = 8.2
  )
}

html_escape <- function(value) {
  value <- gsub("&", "&amp;", as.character(value), fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  value <- gsub(">", "&gt;", value, fixed = TRUE)
  value <- gsub('"', "&quot;", value, fixed = TRUE)
  value
}

image_uri <- function(path) {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  bytes <- readBin(connection, what = "raw", n = file.info(path)$size)
  paste0("data:image/png;base64,", jsonlite::base64_enc(bytes))
}

html_table <- function(data) {
  headers <- paste0("<th>", html_escape(names(data)), "</th>", collapse = "")
  rows <- apply(data, 1, function(row) {
    paste0(
      "<tr>", paste0("<td>", html_escape(row), "</td>", collapse = ""), "</tr>"
    )
  })
  paste0(
    "<div class='rr-table-wrap'><table><thead><tr>", headers,
    "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>"
  )
}

qc_display <- group_qc[c(
  "Group", "Planned_models", "Public_payload_models",
  "Completed_final_PAR_models", "Retained_models", "Documented_MGC_excluded",
  "Documented_incomplete", "PDH_models", "Near_PDH_models",
  "Hessian_mixture_rows", "Projection_combinations",
  "Within_group_model_weight", "Share_of_combined"
)]
names(qc_display) <- c(
  "Group", "Planned", "Public payload", "Completed final PAR", "Retained",
  "MGC excluded", "Incomplete", "PDH", "Near-PDH", "Hessian mixture rows",
  "Projection combinations", "Within-group model weight", "Share of combined"
)
qc_display$`Within-group model weight` <- sprintf(
  "%.4f", qc_display$`Within-group model weight`
)
qc_display$`Share of combined` <- scales::percent(
  qc_display$`Share of combined`, accuracy = 0.1
)

difference_display <- differences_vs_combined[c(
  "Group", "Domain", "Metric", "Statistic", "Value", "Combined_value", "Difference"
)]
for (column in c("Value", "Combined_value", "Difference")) {
  difference_display[[column]] <- sprintf("%.4f", difference_display[[column]])
}
names(difference_display)[names(difference_display) == "Combined_value"] <- "Combined"

structural_display <- structural_summary[c(
  "Group", "Quantity", "Models", "10%", "Median", "90%"
)]
for (column in c("10%", "Median", "90%")) {
  structural_display[[column]] <- sprintf("%.3f", structural_display[[column]])
}
structural_risk_display <- structural_risk[c(
  "Group", "Criterion", "Models", "Events", "Probability"
)]
structural_risk_display$Probability <- scales::percent(
  structural_risk_display$Probability, accuracy = 0.1
)

hessian_display <- hessian_intervals[c(
  "Group", "Quantity", "Models", "PDH_models", "Near_PDH_models",
  "2.5%", "10%", "Median", "90%", "97.5%"
)]
names(hessian_display)[names(hessian_display) == "PDH_models"] <- "PDH"
names(hessian_display)[names(hessian_display) == "Near_PDH_models"] <- "Near-PDH"
for (column in c("2.5%", "10%", "Median", "90%", "97.5%")) {
  hessian_display[[column]] <- sprintf("%.3f", hessian_display[[column]])
}
hessian_risk_display <- hessian_risk[c(
  "Group", "Criterion", "Models", "Mixture_rows", "Probability"
)]
names(hessian_risk_display)[names(hessian_risk_display) == "Mixture_rows"] <-
  "Mixture rows"
hessian_risk_display$Probability <- scales::percent(
  hessian_risk_display$Probability, accuracy = 0.1
)

projection_terminal_display <- projection_terminal_summary[c(
  "Group", "Quantity", "Models", "Projection_combinations", "10%", "Median",
  "90%", "Criterion", "Beyond_criterion_probability"
)]
names(projection_terminal_display)[
  names(projection_terminal_display) == "Projection_combinations"
] <- "Projection combinations"
names(projection_terminal_display)[
  names(projection_terminal_display) == "Beyond_criterion_probability"
] <- "Beyond criterion"
for (column in c("10%", "Median", "90%")) {
  projection_terminal_display[[column]] <- sprintf(
    "%.3f", projection_terminal_display[[column]]
  )
}
projection_terminal_display$`Beyond criterion` <- scales::percent(
  projection_terminal_display$`Beyond criterion`, accuracy = 0.1
)

figure_card <- function(title, caption, files, wide = FALSE) {
  relative_pdf <- file.path(
    "rr-sensitivity", "figures", basename(files[["pdf"]])
  )
  paste0(
    "<article class='rr-figure", if (wide) " rr-wide" else "", "'><h3>",
    html_escape(title), "</h3>",
    "<img src='", image_uri(files[["png"]]), "' alt='", html_escape(title), "'>",
    "<p><strong>Figure.</strong> ", caption, "</p>",
    "<p class='rr-actions'><a href='", relative_pdf, "'>Open vector PDF</a></p></article>"
  )
}

table_links <- paste0(
  "<div class='rr-actions'>",
  paste(vapply(c(
    "group-counts-qc.csv", "structural-management-summary.csv",
    "structural-management-risk.csv", "hessian-management-intervals.csv",
    "hessian-management-risk.csv", "projection-terminal-summary.csv",
    "projection-selected-years.csv", "differences-vs-combined.csv"
  ), function(file) {
    paste0(
      "<a href='rr-sensitivity/tables/", file, "'>", html_escape(file), "</a>"
    )
  }, character(1)), collapse = ""),
  "</div>"
)

# Standalone scope reports -------------------------------------------------
#
# These products deliberately use one calculation path for the combined,
# RR=0 and RR=1 scopes.  Filtering the stable source ensemble identifiers is
# the only operation that differs by scope.  This prevents the subgroup
# reports from silently drifting away from the definitions used by Tables
# 8--13 in the assessment report.
scope_slugs <- c(
  combined = "combined",
  inclusion = "rr-inclusion-flag2-0",
  exclusion = "rr-exclusion-flag2-1"
)
standalone_scope_keys <- c("inclusion", "exclusion")
scope_prefixes <- c(
  inclusion = "rr0-inclusion",
  exclusion = "rr1-exclusion"
)

canonical_model_map <- read.csv(
  file.path(output_dir, "tables", "model-id-map.csv"), check.names = FALSE
)
canonical_fit_diagnostics <- read.csv(
  file.path(output_dir, "tables", "ensemble-fit-diagnostics.csv"),
  check.names = FALSE
)
canonical_fit_hessian <- read.csv(
  file.path(output_dir, "tables", "fit-hessian-summary.csv"),
  check.names = FALSE, colClasses = "character"
)
assert_true(
  nrow(canonical_model_map) == 80L &&
    identical(canonical_model_map$source_ensemble_id, retained_ids) &&
    !anyDuplicated(canonical_model_map$ensemble_id) &&
    !anyDuplicated(canonical_model_map$source_ensemble_id),
  "The canonical 80-model public/source identifier map is invalid."
)

format_interval <- function(lower, upper) sprintf("%.3f–%.3f", lower, upper)

scope_table_products <- function(key) {
  ids <- group_ids[[key]]
  central <- management_retained[
    management_retained$ensemble_id %in% ids, , drop = FALSE
  ]
  draws <- hybrid_management[
    hybrid_management$ensemble_id %in% ids, , drop = FALSE
  ]
  pdh <- pdh_management[pdh_management$ensemble_id %in% ids, , drop = FALSE]
  draws_50 <- draws[draws$draw <= 50L, , drop = FALSE]
  scope_fit <- fit_retained[fit_retained$ensemble_id %in% ids, , drop = FALSE]

  assert_true(
    nrow(central) == length(ids) && !anyDuplicated(central$ensemble_id) &&
      nrow(draws) == length(ids) * draws_per_model &&
      all(table(draws$ensemble_id) == draws_per_model) &&
      nrow(draws_50) == length(ids) * 50L &&
      all(table(draws_50$ensemble_id) == 50L) &&
      nrow(pdh) == length(intersect(ids, pdh_ids)) * draws_per_model,
    paste0("Invalid structural/Hessian mixture for standalone scope ", key, ".")
  )

  table08_intervals <- data.frame(
    Quantity = management_specs$quantity,
    Period = management_specs$period,
    `2.5%` = vapply(
      management_specs$column, function(column) {
        stats::quantile(draws[[column]], 0.025, names = FALSE)
      }, numeric(1)
    ),
    `10%` = vapply(
      management_specs$column, function(column) {
        stats::quantile(draws[[column]], 0.10, names = FALSE)
      }, numeric(1)
    ),
    `25%` = vapply(
      management_specs$column, function(column) {
        stats::quantile(draws[[column]], 0.25, names = FALSE)
      }, numeric(1)
    ),
    Median = vapply(
      management_specs$column, function(column) stats::median(draws[[column]]),
      numeric(1)
    ),
    `75%` = vapply(
      management_specs$column, function(column) {
        stats::quantile(draws[[column]], 0.75, names = FALSE)
      }, numeric(1)
    ),
    `90%` = vapply(
      management_specs$column, function(column) {
        stats::quantile(draws[[column]], 0.90, names = FALSE)
      }, numeric(1)
    ),
    `97.5%` = vapply(
      management_specs$column, function(column) {
        stats::quantile(draws[[column]], 0.975, names = FALSE)
      }, numeric(1)
    ),
    check.names = FALSE
  )
  table08_summary <- data.frame(
    Quantity = table08_intervals$Quantity,
    Period = table08_intervals$Period,
    Median = sprintf("%.3f", table08_intervals$Median),
    `50% interval` = format_interval(
      table08_intervals$`25%`, table08_intervals$`75%`
    ),
    `80% interval` = format_interval(
      table08_intervals$`10%`, table08_intervals$`90%`
    ),
    `95% interval` = format_interval(
      table08_intervals$`2.5%`, table08_intervals$`97.5%`
    ),
    check.names = FALSE
  )

  cmm_recent <- mean(central$recent_mean_depletion)
  cmm_historical <- mean(central$historical_target_depletion)
  table09 <- data.frame(
    Quantity = c(
      "Mean annual spawning depletion, 2021–2024",
      "Mean annual spawning depletion, 2012–2015",
      "Recent-to-2012–2015 spawning depletion ratio"
    ),
    Aggregation = c(
      paste0("Arithmetic mean across ", length(ids), " central models"),
      paste0("Arithmetic mean across ", length(ids), " central models"),
      "Ratio of the preceding two arithmetic means"
    ),
    Models = length(ids),
    Value = c(cmm_recent, cmm_historical, cmm_recent / cmm_historical),
    check.names = FALSE
  )

  table10 <- data.frame(
    Criterion = risk_specs$criterion,
    Probability = c(
      mean(draws$sb_recent_sb0 < 0.20),
      mean(draws$sb_recent_sbmsy < 1),
      mean(draws$f_recent_fmsy > 1)
    ),
    check.names = FALSE
  )

  management_summary <- data.frame(
    Quantity = management_specs$quantity,
    Period = management_specs$period,
    Models = length(ids),
    `10%` = vapply(
      management_specs$column, function(column) {
        stats::quantile(central[[column]], 0.10, names = FALSE)
      }, numeric(1)
    ),
    Median = vapply(
      management_specs$column, function(column) stats::median(central[[column]]),
      numeric(1)
    ),
    `90%` = vapply(
      management_specs$column, function(column) {
        stats::quantile(central[[column]], 0.90, names = FALSE)
      }, numeric(1)
    ),
    check.names = FALSE
  )
  management_risk <- data.frame(
    Indicator = c("Below the LRP", "Below SBMSY", "Above FMSY"),
    Criterion = risk_specs$criterion,
    Events = c(
      sum(central$below_lrp_020), sum(central$below_sbmsy),
      sum(central$above_fmsy)
    ),
    Models = rep(length(ids), 3L),
    check.names = FALSE
  )
  management_risk$Percent <-
    100 * management_risk$Events / management_risk$Models

  terminal_series <- series_retained[
    series_retained$ensemble_id %in% ids & series_retained$year == 2024L,
    c("ensemble_id", "spawning_potential", "sb_sbmsy"), drop = FALSE
  ]
  names(terminal_series)[-1L] <- c("sb_latest_kt", "sb_latest_sbmsy")
  reference <- merge(central, terminal_series, by = "ensemble_id", sort = FALSE)
  reference$sb_latest_sb0 <- reference$sb_latest_kt / reference$sb0_recent_kt
  reference$f_multiplier_at_msy <- 1 / reference$f_recent_fmsy
  reference$sbmsy_kt <- reference$sb_recent_kt / reference$sb_recent_sbmsy
  reference$sbmsy_sb0 <- reference$sbmsy_kt / reference$sb0_recent_kt
  reference_specs <- data.frame(
    Quantity = c(
      "F multiplier at MSY", "Frecent / FMSY", "SBF=0",
      "SBlatest / SBF=0", "SBlatest / SBMSY", "SBrecent / SBF=0",
      "SBrecent / SBMSY", "SBMSY", "SBMSY / SBF=0"
    ),
    Period = c(
      "2020–2023", "2020–2023", "2014–2023 mean",
      "2024 / 2014–2023", "2024 / equilibrium SBMSY",
      "2021–2024 / 2014–2023", "2021–2024 / equilibrium SBMSY",
      "Equilibrium", "Equilibrium / 2014–2023"
    ),
    Unit = c(
      "multiplier", "ratio", "thousand MT", rep("ratio", 4L),
      "thousand MT", "ratio"
    ),
    Column = c(
      "f_multiplier_at_msy", "f_recent_fmsy", "sb0_recent_kt",
      "sb_latest_sb0", "sb_latest_sbmsy", "sb_recent_sb0",
      "sb_recent_sbmsy", "sbmsy_kt", "sbmsy_sb0"
    ),
    stringsAsFactors = FALSE
  )
  reference_statistics <- t(vapply(reference_specs$Column, function(column) {
    value <- reference[[column]]
    c(
      Minimum = min(value),
      `10%` = stats::quantile(value, 0.10, names = FALSE),
      Median = stats::median(value), Mean = mean(value),
      `90%` = stats::quantile(value, 0.90, names = FALSE), Maximum = max(value)
    )
  }, numeric(6L)))
  table11 <- cbind(
    reference_specs[c("Quantity", "Period", "Unit")],
    as.data.frame(reference_statistics, check.names = FALSE)
  )

  audit_summary <- function(value) c(
    q10 = stats::quantile(value, 0.10, names = FALSE),
    median = stats::median(value),
    q90 = stats::quantile(value, 0.90, names = FALSE)
  )
  table12 <- do.call(rbind, lapply(seq_len(nrow(management_specs)), function(index) {
    column <- management_specs$column[[index]]
    all_100 <- audit_summary(draws[[column]])
    all_50 <- audit_summary(draws_50[[column]])
    pdh_only <- audit_summary(pdh[[column]])
    data.frame(
      Quantity = management_specs$quantity[[index]],
      All_q10 = all_100[["q10"]], All_median = all_100[["median"]],
      All_q90 = all_100[["q90"]],
      PDH_q10 = pdh_only[["q10"]], PDH_median = pdh_only[["median"]],
      PDH_q90 = pdh_only[["q90"]],
      Max_50_100_difference = max(abs(all_50 - all_100)),
      check.names = FALSE
    )
  }))

  projected <- projection_management$projected[
    projection_management$projected$ensemble_id %in% ids, , drop = FALSE
  ]
  projected_spawning <- projection$annual_stock[
    projection$annual_stock$ensemble_id %in% ids, , drop = FALSE
  ]
  projected_spawning$spawning_kt <- projected_spawning$spawning_biomass_mt / 1000
  projected_catch <- projection$catch_msy[
    projection$catch_msy$ensemble_id %in% ids, , drop = FALSE
  ]
  table13_rows <- lapply(projection_years_reported, function(year) {
    d <- projected[projected$year == year, , drop = FALSE]
    s <- projected_spawning[projected_spawning$year == year, , drop = FALSE]
    c <- projected_catch[projected_catch$year == year, , drop = FALSE]
    assert_true(
      nrow(d) == length(ids) * 10L && nrow(s) == length(ids) * 10L &&
        nrow(c) == length(ids) * 10L,
      paste0("Incomplete Table 13 projection inputs for ", key, " / ", year, ".")
    )
    data.frame(
      Year = year,
      `SBrecent/SBF=0 10%` = sprintf(
        "%.3f", stats::quantile(d$sb_recent_sb0, 0.10, names = FALSE)
      ),
      `SBrecent/SBF=0 median` = sprintf("%.3f", stats::median(d$sb_recent_sb0)),
      `SBrecent/SBF=0 90%` = sprintf(
        "%.3f", stats::quantile(d$sb_recent_sb0, 0.90, names = FALSE)
      ),
      `Simulation frequency below LRP` = scales::percent(
        mean(d$below_lrp_020), accuracy = 0.1
      ),
      `Spawning potential median (10^3 MT)` = sprintf(
        "%.1f", stats::median(s$spawning_kt)
      ),
      `Catch/MSY 10%` = sprintf(
        "%.3f", stats::quantile(c$catch_msy, 0.10, names = FALSE)
      ),
      `Catch/MSY median` = sprintf("%.3f", stats::median(c$catch_msy)),
      `Catch/MSY 90%` = sprintf(
        "%.3f", stats::quantile(c$catch_msy, 0.90, names = FALSE)
      ),
      check.names = FALSE
    )
  })
  table13 <- do.call(rbind, table13_rows)
  rownames(table13) <- NULL

  terminal_scope <- terminal[terminal$ensemble_id %in% ids, , drop = FALSE]
  assert_true(
    nrow(terminal_scope) == length(ids) * 10L &&
      all(table(terminal_scope$ensemble_id) == 10L),
    paste0("Incomplete terminal projection inputs for ", key, ".")
  )
  terminal_values <- list(
    terminal_scope$terminal_sb_sbmsy, terminal_scope$terminal_f_fmsy
  )
  terminal_table <- data.frame(
    Quantity = c("SB2051–2054 / SBMSY", "F2050–2053 / FMSY"),
    `10%` = sprintf("%.3f", vapply(
      terminal_values, stats::quantile, numeric(1), probs = 0.10, names = FALSE
    )),
    Median = sprintf("%.3f", vapply(terminal_values, stats::median, numeric(1))),
    `90%` = sprintf("%.3f", vapply(
      terminal_values, stats::quantile, numeric(1), probs = 0.90, names = FALSE
    )),
    Criterion = c("SB2051–2054/SBMSY < 1", "F2050–2053/FMSY > 1"),
    `Beyond criterion` = scales::percent(c(
      mean(terminal_scope$terminal_sb_sbmsy < 1),
      mean(terminal_scope$terminal_f_fmsy > 1)
    ), accuracy = 0.1),
    check.names = FALSE
  )

  public_ids_scope <- canonical_model_map$ensemble_id[
    canonical_model_map$source_ensemble_id %in% ids
  ]
  diagnostics <- canonical_fit_diagnostics[
    canonical_fit_diagnostics$source_ensemble_id %in% ids, , drop = FALSE
  ]
  fit_listing <- canonical_fit_hessian[
    canonical_fit_hessian$Model %in% public_ids_scope, , drop = FALSE
  ]
  assert_true(
    nrow(diagnostics) == length(ids) && nrow(fit_listing) == length(ids) &&
      identical(diagnostics$ensemble_id, public_ids_scope) &&
      identical(diagnostics$source_ensemble_id, sort(ids)) &&
      identical(fit_listing$Model, public_ids_scope) &&
      !anyDuplicated(diagnostics$ensemble_id) &&
      !anyDuplicated(diagnostics$source_ensemble_id),
    paste0("Canonical public/source identifiers were not preserved for ", key, ".")
  )

  scope_metadata <- data.frame(
    Field = c(
      "Scope", "Retained models", "PDH models", "Near-PDH models",
      "Hessian rows", "Projection paths per model", "Projection combinations",
      "Current-status uncertainty", "Dynamic-status uncertainty",
      "Projection uncertainty", "CMM objective uncertainty"
    ),
    Value = c(
      group_labels[[key]], length(ids), length(intersect(ids, pdh_ids)),
      length(intersect(ids, near_pdh_ids)), nrow(draws), 10L,
      length(ids) * 10L,
      "Structural + available Hessian estimation uncertainty",
      "Central structural models only; no Hessian draws",
      "Structural + stochastic recruitment; no Hessian draws",
      "Central structural models only; no Hessian draws"
    ),
    stringsAsFactors = FALSE
  )

  list(
    table08_intervals = table08_intervals,
    table08_summary = table08_summary,
    table09 = table09,
    table10 = table10,
    management_summary = management_summary,
    management_risk = management_risk,
    table11 = table11,
    table12 = table12,
    table13 = table13,
    terminal = terminal_table,
    diagnostics = diagnostics,
    fit_hessian = fit_listing,
    metadata = scope_metadata,
    terminal_rows = terminal_scope
  )
}

scope_products <- setNames(lapply(group_keys, scope_table_products), group_keys)

bind_scope_product <- function(product_name) {
  output <- do.call(rbind, lapply(group_keys, function(key) {
    data.frame(
      Group = group_labels[[key]],
      scope_products[[key]][[product_name]],
      check.names = FALSE
    )
  }))
  rownames(output) <- NULL
  output
}

# Copy-ready cross-scope versions of the report's Tables 8–13. These bind the
# exact per-scope products rather than recalculating a second set of summaries.
grouped_wp06_tables <- list(
  table08_intervals = bind_scope_product("table08_intervals"),
  table08_summary = bind_scope_product("table08_summary"),
  table09 = bind_scope_product("table09"),
  table10 = bind_scope_product("table10"),
  table11 = bind_scope_product("table11"),
  table12 = bind_scope_product("table12"),
  table13 = bind_scope_product("table13")
)
grouped_wp06_filenames <- c(
  table08_intervals = "estimation-management-intervals.csv",
  table08_summary = "estimation-management-summary.csv",
  table09 = "cmm-depletion-comparison.csv",
  table10 = "estimation-management-risk.csv",
  table11 = "structural-reference-points.csv",
  table12 = "estimation-uncertainty-audit.csv",
  table13 = "projection-summary.csv"
)
for (product_name in names(grouped_wp06_filenames)) {
  write.csv(
    grouped_wp06_tables[[product_name]],
    file.path(table_dir, grouped_wp06_filenames[[product_name]]),
    row.names = FALSE
  )
}

assert_numeric_frame_equal <- function(observed, expected_path, label, tolerance = 5e-12) {
  expected <- read.csv(expected_path, check.names = FALSE)
  assert_true(
    identical(names(observed), names(expected)) && nrow(observed) == nrow(expected),
    paste0("Combined parity schema mismatch: ", label, ".")
  )
  for (column in names(observed)) {
    if (is.numeric(expected[[column]]) || is.integer(expected[[column]])) {
      observed_value <- as.numeric(observed[[column]])
      expected_value <- as.numeric(expected[[column]])
      assert_true(
        identical(is.na(observed_value), is.na(expected_value)),
        paste0("Combined parity NA mismatch: ", label, " / ", column, ".")
      )
      finite <- !is.na(observed_value)
      difference <- if (any(finite)) {
        max(abs(observed_value[finite] - expected_value[finite]))
      } else {
        0
      }
      assert_true(
        is.finite(difference) && difference <= tolerance,
        paste0("Combined parity numeric mismatch: ", label, " / ", column, ".")
      )
    } else {
      assert_true(
        identical(as.character(observed[[column]]), as.character(expected[[column]])),
        paste0("Combined parity text mismatch: ", label, " / ", column, ".")
      )
    }
  }
  invisible(TRUE)
}

assert_character_frame_equal <- function(observed, expected_path, label) {
  expected <- read.csv(
    expected_path, check.names = FALSE, colClasses = "character"
  )
  converted <- as.data.frame(
    lapply(observed, as.character), stringsAsFactors = FALSE, check.names = FALSE
  )
  names(converted) <- names(observed)
  assert_true(
    identical(names(converted), names(expected)) &&
      identical(converted, expected),
    paste0("Combined formatted-table parity mismatch: ", label, ".")
  )
  invisible(TRUE)
}

canonical_table_dir <- file.path(output_dir, "tables")
assert_numeric_frame_equal(
  scope_products$combined$management_summary,
  file.path(canonical_table_dir, "management-summary.csv"),
  "central management summary"
)
assert_numeric_frame_equal(
  scope_products$combined$management_risk,
  file.path(canonical_table_dir, "management-risk.csv"),
  "central management risk"
)
assert_numeric_frame_equal(
  scope_products$combined$diagnostics,
  file.path(canonical_table_dir, "ensemble-fit-diagnostics.csv"),
  "ensemble fit diagnostics"
)
assert_numeric_frame_equal(
  scope_products$combined$table08_intervals,
  file.path(canonical_table_dir, "estimation-management-intervals.csv"),
  "Table 8 numeric intervals"
)
assert_character_frame_equal(
  scope_products$combined$table08_summary,
  file.path(canonical_table_dir, "estimation-management-summary.csv"),
  "Table 8 formatted intervals"
)
assert_numeric_frame_equal(
  scope_products$combined$table09,
  file.path(canonical_table_dir, "cmm-depletion-comparison.csv"),
  "Table 9 CMM objective"
)
assert_numeric_frame_equal(
  scope_products$combined$table10,
  file.path(canonical_table_dir, "estimation-management-risk.csv"),
  "Table 10 status risk"
)
assert_numeric_frame_equal(
  scope_products$combined$table11,
  file.path(canonical_table_dir, "structural-reference-points.csv"),
  "Table 11 structural reference points"
)
assert_numeric_frame_equal(
  scope_products$combined$table12,
  file.path(canonical_table_dir, "estimation-uncertainty-audit.csv"),
  "Table 12 estimation audit"
)
assert_character_frame_equal(
  scope_products$combined$table13,
  file.path(canonical_table_dir, "projection-summary.csv"),
  "Table 13 projections"
)
assert_character_frame_equal(
  scope_products$combined$terminal,
  file.path(canonical_table_dir, "projection-terminal-management.csv"),
  "terminal projection management"
)
assert_character_frame_equal(
  scope_products$combined$fit_hessian,
  file.path(canonical_table_dir, "fit-hessian-summary.csv"),
  "fit/Hessian display"
)

scope_table_filenames <- c(
  management_summary = "management-summary.csv",
  management_risk = "management-risk.csv",
  diagnostics = "ensemble-fit-diagnostics.csv",
  table09 = "cmm-depletion-comparison.csv",
  table08_intervals = "estimation-management-intervals.csv",
  table08_summary = "estimation-management-summary.csv",
  table10 = "estimation-management-risk.csv",
  table11 = "structural-reference-points.csv",
  table12 = "estimation-uncertainty-audit.csv",
  table13 = "projection-summary.csv",
  terminal = "projection-terminal-management.csv",
  fit_hessian = "fit-hessian-summary.csv"
)

for (key in standalone_scope_keys) {
  prefix <- scope_prefixes[[key]]
  for (product_name in names(scope_table_filenames)) {
    write.csv(
      scope_products[[key]][[product_name]],
      file.path(
        table_dir,
        paste0(prefix, "-", scope_table_filenames[[product_name]])
      ),
      row.names = FALSE
    )
  }
  assert_true(
    length(list.files(
      table_dir, pattern = paste0("^", prefix, "-.*[.]csv$")
    )) == 12L,
    paste0("Expected exactly 12 scoped CSVs for ", key, ".")
  )
}

scope_word_table <- function(caption, data) {
  paste(
    c(
      caption, paste(names(data), collapse = "\t"),
      apply(data, 1, paste, collapse = "\t")
    ),
    collapse = "\n"
  )
}

scope_latex_escape <- function(value) {
  value <- as.character(value)
  value <- gsub("\\", "\\textbackslash{}", value, fixed = TRUE)
  for (target in names(c(
    "&" = "\\&", "%" = "\\%", "$" = "\\$", "#" = "\\#",
    "_" = "\\_", "{" = "\\{", "}" = "\\}"
  ))) {
    replacement <- c(
      "&" = "\\&", "%" = "\\%", "$" = "\\$", "#" = "\\#",
      "_" = "\\_", "{" = "\\{", "}" = "\\}"
    )[[target]]
    value <- gsub(target, replacement, value, fixed = TRUE)
  }
  value <- gsub("–", "--", value, fixed = TRUE)
  value <- gsub("−", "$-$", value, fixed = TRUE)
  value
}

scope_latex_table <- function(caption, data) {
  rows <- apply(data, 1, function(row) {
    paste0(
      paste(vapply(row, scope_latex_escape, character(1)), collapse = " & "),
      " \\\\"
    )
  })
  paste0(
    "% Requires \\usepackage{booktabs,tabularx}\n",
    "\\begin{table}[htbp]\n\\centering\n\\caption{",
    scope_latex_escape(caption), "}\n\\scriptsize\n",
    "\\begin{tabularx}{\\textwidth}{@{}",
    paste(rep("X", ncol(data)), collapse = ""), "@{}}\n\\toprule\n",
    paste(vapply(names(data), scope_latex_escape, character(1)), collapse = " & "),
    " \\\\\n\\midrule\n", paste(rows, collapse = "\n"),
    "\n\\bottomrule\n\\end{tabularx}\n\\end{table}"
  )
}

scope_table_card <- function(id, title, caption, display, prefix, filename) {
  word_id <- paste0("word-", id)
  latex_id <- paste0("latex-", id)
  paste0(
    "<article class='table-card' id='", html_escape(id), "'>",
    "<h2>", html_escape(title), "</h2>",
    "<p class='caption'><strong>Table.</strong> ", html_escape(caption), "</p>",
    "<div class='actions'><button onclick=\"copyText('", word_id,
    "',this)\">Copy table for Word</button>",
    "<button onclick=\"copyText('", latex_id,
    "',this)\">Copy LaTeX</button>",
    "<a href='rr-sensitivity/tables/", prefix, "-",
    filename, "' download>Download CSV</a></div>",
    html_table(display),
    "<textarea id='", word_id, "' class='copy-source'>",
    html_escape(scope_word_table(caption, display)), "</textarea>",
    "<textarea id='", latex_id, "' class='copy-source'>",
    html_escape(scope_latex_table(caption, display)), "</textarea></article>"
  )
}

scope_figure_card <- function(id, title, caption, files) {
  caption_id <- paste0("caption-", id)
  latex_id <- paste0("figure-latex-", id)
  latex_figure <- paste0(
    "\\begin{figure}[htbp]\n\\centering\n",
    "\\includegraphics[width=\\textwidth]{rr-sensitivity/figures/",
    basename(files[["pdf"]]), "}\n",
    "\\caption{", scope_latex_escape(caption), "}\n\\end{figure}"
  )
  paste0(
    "<article class='figure-card' id='", html_escape(id), "'>",
    "<h2>", html_escape(title), "</h2>",
    "<img src='", image_uri(files[["png"]]), "' alt='", html_escape(title), "'>",
    "<p class='caption'><strong>Figure.</strong> ", html_escape(caption), "</p>",
    "<div class='actions'><button onclick=\"copyText('", caption_id,
    "',this)\">Copy caption</button>",
    "<a href='rr-sensitivity/figures/",
    basename(files[["pdf"]]), "'>Open vector PDF</a>",
    "<a href='rr-sensitivity/figures/", basename(files[["png"]]),
    "' download>Save PNG</a>",
    "<button onclick=\"copyText('", latex_id,
    "',this)\">Copy figure for LaTeX</button></div>",
    "<textarea id='", caption_id, "' class='copy-source'>",
    html_escape(caption), "</textarea>",
    "<textarea id='", latex_id, "' class='copy-source'>",
    html_escape(latex_figure), "</textarea></article>"
  )
}

scope_display_tables <- function(key) {
  products <- scope_products[[key]]
  table09 <- products$table09
  table09$Value <- sprintf("%.3f", table09$Value)

  table10 <- products$table10
  table10$Probability <- scales::percent(table10$Probability, accuracy = 0.1)

  table11 <- products$table11
  for (column in c("Minimum", "10%", "Median", "Mean", "90%", "Maximum")) {
    table11[[column]] <- vapply(seq_len(nrow(table11)), function(index) {
      digits <- if (table11$Unit[[index]] == "thousand MT") 1L else 3L
      sprintf(paste0("%.", digits, "f"), products$table11[[column]][[index]])
    }, character(1))
  }

  table12 <- data.frame(
    Quantity = products$table12$Quantity,
    `All models: median (80% interval)` = sprintf(
      "%.3f (%.3f–%.3f)", products$table12$All_median,
      products$table12$All_q10, products$table12$All_q90
    ),
    `PDH only: median (80% interval)` = sprintf(
      "%.3f (%.3f–%.3f)", products$table12$PDH_median,
      products$table12$PDH_q10, products$table12$PDH_q90
    ),
    `Max |50-draw − 100-draw quantile|` = sprintf(
      "%.4f", products$table12$Max_50_100_difference
    ),
    check.names = FALSE
  )

  list(
    table08 = products$table08_summary,
    table09 = table09,
    table10 = table10,
    table11 = table11,
    table12 = table12,
    table13 = products$table13,
    terminal = products$terminal,
    fit = products$fit_hessian
  )
}

scope_report_html <- function(key) {
  ids <- group_ids[[key]]
  prefix <- scope_prefixes[[key]]
  display <- scope_display_tables(key)
  figures <- scope_figure_files[[key]]
  pdh_count <- length(intersect(ids, pdh_ids))
  near_count <- length(intersect(ids, near_pdh_ids))
  flag <- if (key == "inclusion") 0L else 1L
  treatment <- if (key == "inclusion") {
    "pre-mixing reporting-rate inclusion"
  } else {
    "pre-mixing reporting-rate exclusion"
  }
  table08_caption <- paste0(
    "Management quantities for ", length(ids),
    " equal-weight retained models with available Hessian estimation uncertainty. ",
    "Each of ", pdh_count, " PDH models contributes 100 joint draws; each of ",
    near_count, " Near-PDH models contributes its central estimate with equal total model weight. ",
    "Median and central 50%, 80% and 95% equal-tailed intervals are shown."
  )
  table09_caption <- paste0(
    "Recent spawning depletion relative to 2012–2015 across ", length(ids),
    " equal-weight central models. Estimation uncertainty is excluded; period values are arithmetic means across models and the final row is their ratio."
  )
  table10_caption <- paste0(
    "Status probabilities from the same equal-model-weight mixture as Table 8. Available Hessian estimation uncertainty is included for ",
    pdh_count, " PDH models; ", near_count,
    " Near-PDH models enter as point estimates."
  )
  table11_caption <- paste0(
    "Reference-point quantities across ", length(ids),
    " retained central model estimates. Ratios are calculated within model before summarizing. These intervals describe structural uncertainty only; Hessian estimation uncertainty is excluded."
  )
  table12_caption <- paste0(
    "Estimation-uncertainty audit for the three core quantities. The all-model column uses ",
    pdh_count, " PDH draw sets plus ", near_count,
    " Near-PDH point masses; the PDH-only column excludes those point masses. The last column checks 50 versus 100 draws per model."
  )
  table13_caption <- paste0(
    "Projected quantities at selected years across ", length(ids) * 10L,
    " equally weighted model–recruitment combinations. Intervals include structural differences and stochastic recruitment; Hessian parameter uncertainty is excluded."
  )
  terminal_caption <- paste0(
    "Terminal stock status under the fixed recent-catch projection across ",
    length(ids) * 10L,
    " model–recruitment combinations. Recruitment variation is included; Hessian parameter uncertainty is excluded."
  )
  fit_caption <- paste0(
    "Fit and Hessian diagnostics for the ", length(ids),
    " retained models in this scope. Model labels retain the canonical 80-model numbering; use the shared model-ID map to recover stable source ensemble IDs."
  )

  table_cards <- list(
    table08 = scope_table_card(
      "table-08", "Table 8 · Management quantities with estimation uncertainty",
      table08_caption, display$table08, prefix,
      "estimation-management-summary.csv"
    ),
    table09 = scope_table_card(
      "table-09", "Table 9 · CMM depletion comparison",
      table09_caption, display$table09, prefix,
      "cmm-depletion-comparison.csv"
    ),
    table10 = scope_table_card(
      "table-10", "Table 10 · Status probabilities",
      table10_caption, display$table10, prefix,
      "estimation-management-risk.csv"
    ),
    table11 = scope_table_card(
      "table-11", "Table 11 · Structural reference points",
      table11_caption, display$table11, prefix,
      "structural-reference-points.csv"
    ),
    table12 = scope_table_card(
      "table-12", "Table 12 · Estimation-uncertainty audit",
      table12_caption, display$table12, prefix,
      "estimation-uncertainty-audit.csv"
    ),
    table13 = scope_table_card(
      "table-13", "Table 13 · Projection summary",
      table13_caption, display$table13, prefix,
      "projection-summary.csv"
    ),
    terminal = scope_table_card(
      "terminal-management", "Terminal management quantities",
      terminal_caption, display$terminal, prefix,
      "projection-terminal-management.csv"
    ),
    fit = scope_table_card(
      "fit-hessian", "Fit and Hessian diagnostics",
      fit_caption, display$fit, prefix, "fit-hessian-summary.csv"
    )
  )

  figure_cards <- list(
    history = scope_figure_card(
      "annual-uncertainty", "Annual structural and estimation uncertainty",
      paste0(
        "Annual depletion (a), spawning potential (b), recruitment (c) and fishing mortality (d) across the ",
        length(ids), " retained models. Depletion is SBt/SBF=0,t and the horizontal line is the LRP of 0.20. Grey lines are central model trajectories. For depletion, spawning potential and recruitment, medians and pointwise central 50%, 80% and 95% intervals combine structure with available joint Hessian draws (",
        pdh_count, " PDH models) while repeating each of the ", near_count,
        " Near-PDH central estimates to preserve equal model weight. Annual fishing mortality is structural-only because matching Hessian F draws are unavailable."
      ), figures$history
    ),
    current = scope_figure_card(
      "current-status", "Current Kobe and Majuro status",
      paste0(
        "Current status based on mean spawning biomass for 2021–2024 and mean fishing mortality for 2020–2023 across ",
        length(ids), " equally weighted models. Panel (a) is relative to SBMSY and FMSY; panel (b) divides mean 2021–2024 spawning biomass by mean 2014–2023 SBF=0 and uses the LRP of 0.20. Filled and open central points distinguish the ",
        pdh_count, " PDH and ", near_count,
        " Near-PDH fits. Nested 50%, 80% and 95% bivariate kernel highest-density regions combine structure with available joint Hessian uncertainty, with Near-PDH central estimates repeated to retain equal model weight. Backgrounds follow the Kobe four-category and Majuro three-category definitions; labels give probabilities from the same mixture."
      ), figures$current_status
    ),
    dynamic = scope_figure_card(
      "dynamic-status", "Time-dynamic Kobe and Majuro status",
      paste0(
        "Time-dynamic Majuro and Kobe trajectories for the 2026 diagnostic model and the ",
        length(ids), "-model RR scope. Panels (a) and (b) show Majuro trajectories for the diagnostic model and coordinate-wise annual scope median; panels (c) and (d) show the corresponding Kobe trajectories. Majuro axes are SBt/SBF=0,t and Ft/FMSY,t; Kobe axes are SBt/SBMSY,t and Ft/FMSY,t. Colour shows year and the outlined point marks 2024. The scope trajectory uses central models only, so estimation uncertainty is excluded. Axis limits are shared between RR scopes."
      ),
      figures$dynamic_status
    ),
    continuous = scope_figure_card(
      "continuous-axes", "Continuous ensemble axes",
      paste0(
        "Distributions of Frecent/FMSY (top; mean F over 2020–2023) and the LRP statistic SBrecent/SBF=0 (bottom; mean 2021–2024 spawning biomass divided by mean 2014–2023 unfished spawning biomass), grouped using the combined 80-model quartile breaks for steepness and natural mortality at the reference length. Violins show density and boxes show the median and interquartile range. Each of the ",
        length(ids), " models has equal total weight; ", pdh_count,
        " PDH models contribute joint Hessian draws and ", near_count,
        " Near-PDH models contribute repeated central estimates. All axes vary jointly, so contrasts are descriptive rather than one-factor causal effects. Combined breaks and plot scales are fixed across RR scopes."
      ),
      figures$continuous_axes
    ),
    tag = scope_figure_card(
      "tag-axes", "Tag-model axes",
      paste0(
        "Distributions of Frecent/FMSY (top; mean F over 2020–2023) and the LRP statistic SBrecent/SBF=0 (bottom; mean 2021–2024 spawning biomass divided by mean 2014–2023 unfished spawning biomass), grouped by fixed tag overdispersion τ and tag-mixing cutoff. Violins show density and boxes show the median and interquartile range. Each of the ",
        length(ids), " models has equal total weight; available joint Hessian draws are included for PDH fits and Near-PDH central estimates are repeated. The reporting treatment is fixed within this scope and is not re-plotted. Comparisons are marginal descriptions of a jointly varying ensemble; scales are fixed across RR scopes."
      ),
      figures$tag_axes
    ),
    terminal = scope_figure_card(
      "terminal-status", "Terminal projection status",
      paste0(
        "Terminal Kobe (a) and Majuro (b) status for the projection ending in 2054 across ",
        length(ids) * 10L,
        " equally weighted model–recruitment combinations. Biomass is averaged over 2051–2054; fishing mortality is averaged over 2050–2053 and divided by FMSY. Kobe biomass is relative to SBMSY; the Majuro statistic divides mean 2051–2054 spawning biomass by the preceding ten-year mean SBF=0 and uses the LRP of 0.20. Points are individual combinations, nested shading gives 50%, 80% and 95% bivariate kernel HDRs, and labels give category percentages. Projection uncertainty includes structure and stochastic recruitment; Hessian parameter uncertainty is excluded. Axes are fixed across RR scopes."
      ), figures$terminal_status
    ),
    projection_key = scope_figure_card(
      "projection-key", "Projection key quantities",
      paste0(
        "Projection simulation frequency below the LRP (a) and current versus terminal F/FMSY (b) across ",
        length(ids) * 10L,
        " model–recruitment combinations. The LRP statistic divides each four-year mean spawning biomass by the preceding ten-year mean unfished biomass. Panel (b) points are medians; dark, medium and light ranges are central 50%, 80% and 95% intervals. Below-LRP frequency and mean 2050–2053 F/FMSY include structure and stochastic recruitment without Hessian draws; current Frecent/FMSY includes available Hessian uncertainty. Axes are fixed across RR scopes."
      ),
      figures$projection_key
    ),
    projection_stock = scope_figure_card(
      "projection-stock", "Stock trajectories",
      paste0(
        "All-region LRP depletion (a) and spawning potential in thousand metric tonnes (b), joining estimates through 2024 to projections for 2025–2054 across ",
        length(ids), " models and ", length(ids) * 10L,
        " model–recruitment combinations. The LRP statistic is each four-year mean spawning biomass divided by the preceding ten-year mean no-fishing biomass. Thick lines are medians; ten thin lines show representative linked historical–projection paths; nested bands are central 50%, 80% and 95% intervals. Historical bands show structural uncertainty only; projections add stochastic recruitment without Hessian parameter draws. The 0.20 line is the stock-wide LRP; axes are fixed across RR scopes."
      ),
      figures$projection_stock
    )
  )

  download_links <- paste(vapply(names(scope_table_filenames), function(name) {
    filename <- paste0(prefix, "-", scope_table_filenames[[name]])
    paste0(
      "<a href='rr-sensitivity/tables/", filename, "' download>",
      html_escape(sub(paste0("^", prefix, "-"), "", filename)), "</a>"
    )
  }, character(1)), collapse = "")

  paste0(
    "<!doctype html><html lang='en'><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width,initial-scale=1'>",
    "<title>BET 2026 · ", html_escape(group_labels[[key]]), "</title>",
    "<style>",
    ":root{--navy:#082f49;--teal:#087f8f;--cyan:#dff3f5;--orange:#d97706;--ink:#203846;--muted:#58707d;--line:#c8dce2;--paper:#f4f8fa}",
    "*{box-sizing:border-box}body{margin:0;background:var(--paper);color:var(--ink);font:16px/1.5 Georgia,'Times New Roman',serif}",
    "header{background:linear-gradient(120deg,#062d4a,#087f8f);color:white;padding:34px clamp(20px,5vw,72px) 28px}header h1{font-size:clamp(2rem,4vw,3.5rem);line-height:1.05;margin:.25rem 0}.kicker{font:700 .86rem Arial,sans-serif;letter-spacing:.11em;text-transform:uppercase;color:#bcecf2}",
    "header p{max-width:1000px;font-size:1.08rem;margin:.6rem 0}.shell{width:min(1500px,96vw);margin:0 auto;padding:24px 0 60px}",
    ".tabs{position:sticky;top:0;z-index:10;display:flex;gap:8px;flex-wrap:wrap;padding:10px;background:rgba(244,248,250,.96);border-bottom:1px solid var(--line)}",
    ".tabs button,.actions a,.actions button{border:0;border-radius:5px;background:var(--teal);color:white;padding:9px 13px;text-decoration:none;font:700 .86rem Arial,sans-serif;cursor:pointer}.tabs button.active{background:var(--navy)}",
    ".panel{display:none}.panel.active{display:block}.hero-grid{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:14px;margin:16px 0}.stat{background:white;border:1px solid var(--line);border-top:4px solid var(--teal);border-radius:8px;padding:16px;box-shadow:0 4px 14px #173b4d14}.stat strong{display:block;font:800 1.55rem Arial,sans-serif;color:var(--navy)}",
    ".boundary{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:10px;background:#fff7e8;border-left:6px solid var(--orange);padding:15px;margin:16px 0}.boundary div{padding:7px}.boundary strong{display:block;color:#9a4d00;font-family:Arial,sans-serif}",
    ".context{background:#e8f5f6;border-left:6px solid var(--teal);padding:14px 18px;margin:16px 0}.context a{color:#07566b;font-weight:bold}",
    ".figure-card,.table-card{background:white;border:1px solid var(--line);border-radius:9px;padding:clamp(14px,2vw,24px);margin:18px 0;box-shadow:0 5px 18px #173b4d12}.figure-card img{width:100%;height:auto;display:block;margin:10px auto}.figure-card h2,.table-card h2{color:#07566b;margin:.1rem 0 .5rem;font-size:clamp(1.35rem,2.2vw,1.9rem)}",
    ".caption{color:#405e6d}.actions{display:flex;gap:8px;flex-wrap:wrap;margin:12px 0}.actions button.copied{background:#2a9d8f}.copy-source{position:absolute;left:-10000px;width:1px;height:1px}.downloads{background:white;border:1px solid var(--line);border-radius:8px;padding:16px}.downloads .actions a{margin:2px}",
    ".rr-table-wrap{overflow:auto;max-height:560px;border:1px solid #d7e3e8;margin:12px 0}.rr-table-wrap table{width:100%;min-width:760px;border-collapse:collapse}.rr-table-wrap th{position:sticky;top:0;background:#0a5266;color:white;font-family:Arial,sans-serif}.rr-table-wrap th,.rr-table-wrap td{padding:9px 11px;border-bottom:1px solid #d7e3e8;text-align:left;white-space:nowrap}.rr-table-wrap tr:nth-child(even){background:#f1f7f8}",
    "#table-08 th:nth-child(n+3),#table-08 td:nth-child(n+3),#table-09 th:nth-child(n+3),#table-09 td:nth-child(n+3),#table-10 th:nth-child(2),#table-10 td:nth-child(2),#table-11 th:nth-child(n+4),#table-11 td:nth-child(n+4),#table-12 th:nth-child(n+2),#table-12 td:nth-child(n+2),#table-13 th,#table-13 td{text-align:right;font-variant-numeric:tabular-nums}",
    "footer{color:var(--muted);font-size:.92rem;border-top:1px solid var(--line);padding-top:18px;margin-top:28px}",
    "@media(max-width:900px){.hero-grid,.boundary{grid-template-columns:1fr 1fr}}@media(max-width:560px){.hero-grid,.boundary{grid-template-columns:1fr}.shell{width:98vw}.tabs{position:static}.rr-table-wrap table{min-width:680px}}",
    "@media print{.tabs,.actions{display:none!important}.panel{display:block!important}.shell{width:100%;padding:0}.figure-card,.table-card{break-inside:avoid;box-shadow:none}}",
    "</style></head><body>",
    "<header><div class='kicker'>BET 2026 · reporting-rate sensitivity</div>",
    "<h1>", html_escape(group_labels[[key]]), "</h1>",
    "<p>MFCL tag flag column 2 = ", flag, " · ", html_escape(treatment),
    ". For mixing=0 rows, stored flag2=1 is an inactive compatibility sentinel; no tag event or recapture is removed.</p></header>",
    "<main class='shell'><nav class='tabs' aria-label='Result sections'>",
    "<button class='active' data-tab='overview'>Overview</button>",
    "<button data-tab='figures'>Figures</button>",
    "<button data-tab='tables'>Tables</button>",
    "<button onclick='window.print()'>Print / PDF</button></nav>",
    "<section class='panel active' data-panel='overview'>",
    "<div class='hero-grid'><div class='stat'><strong>", length(ids),
    "</strong>retained models</div><div class='stat'><strong>", pdh_count,
    " + ", near_count, "</strong>PDH + Near-PDH</div><div class='stat'><strong>",
    length(ids) * 100L, "</strong>equal-weight mixture rows</div><div class='stat'><strong>",
    length(ids) * 10L, "</strong>projection combinations</div></div>",
    "<div class='boundary'><div><strong>Annual SB / recruitment</strong>Available estimation included</div><div><strong>Annual F</strong>Structural only</div><div><strong>Dynamic status & CMM</strong>Central models only</div><div><strong>Projection</strong>Recruitment paths; no Hessian</div></div>",
    "<div class='context'><strong>Weighting.</strong> Every retained model has weight 1/",
    length(ids), " within this scope. This is a retained-subset sensitivity, not a matched causal reporting-rate effect. ",
    "Estimation uncertainty is unavailable for the Near-PDH fits; their central estimates are repeated only to preserve equal total model weight. ",
    "Projection uncertainty excludes Hessian parameter draws. ",
    "The <a href='bet-2026-ensemble-report.html'>canonical combined 80-model result</a> remains the primary combined analysis.</div>",
    "<div class='context'>Use the Figures tab for all eight scope figures and the Tables tab for the eight copy-ready reporting tables, diagnostics and CSV downloads. The shared ensemble-design and natural-mortality evidence remain in the <a href='bet-2026-ensemble-report.html'>combined analysis</a>; the fixed reporting-rate axis is not re-plotted within its own subset.</div>",
    "</section>",
    "<section class='panel' data-panel='figures'>",
    figure_cards$history, figure_cards$current, figure_cards$dynamic,
    figure_cards$continuous, figure_cards$tag,
    figure_cards$projection_key, figure_cards$projection_stock,
    figure_cards$terminal,
    "</section>",
    "<section class='panel' data-panel='tables'>",
    table_cards$table08, table_cards$table09, table_cards$table10,
    table_cards$table11, table_cards$table12, table_cards$table13,
    table_cards$terminal, table_cards$fit,
    "<div class='downloads'><h2>All machine-readable scope files</h2><div class='actions'>",
    download_links,
    "<a href='tables/model-id-map.csv'>Shared canonical model-ID map</a>",
    "</div><p>The scope-specific diagnostics preserve the canonical 80-model public IDs. The shared map links those labels to stable source ensemble IDs; models are not renumbered within RR subsets.</p></div>",
    "</section><footer>Generated from the locked retained ensemble, native Hessian cache and native projection cache. No model fitting, Hessian calculation or projection was rerun by this reporting step.</footer></main>",
    "<script>function copyText(id,button){const source=document.getElementById(id);const old=button.textContent;const done=()=>{button.textContent='Copied';button.classList.add('copied');setTimeout(()=>{button.textContent=old;button.classList.remove('copied')},1400)};const fallback=()=>{source.focus();source.select();document.execCommand('copy');done()};if(navigator.clipboard&&window.isSecureContext){navigator.clipboard.writeText(source.value).then(done).catch(fallback)}else{fallback()}}(()=>{const buttons=[...document.querySelectorAll('[data-tab]')],panels=[...document.querySelectorAll('[data-panel]')];buttons.forEach(b=>b.addEventListener('click',()=>{buttons.forEach(x=>x.classList.toggle('active',x===b));panels.forEach(p=>p.classList.toggle('active',p.dataset.panel===b.dataset.tab));history.replaceState(null,'','#'+b.dataset.tab)}));const target=location.hash.slice(1);const button=buttons.find(b=>b.dataset.tab===target);if(button)button.click()})();</script>",
    "</body></html>"
  )
}

normalize_pdf_creation_date <- function(path) {
  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  marker <- charToRaw("/CreationDate (D:")
  candidates <- which(bytes == marker[[1L]])
  starts <- candidates[vapply(candidates, function(index) {
    end <- index + length(marker) - 1L
    end <= length(bytes) && identical(bytes[index:end], marker)
  }, logical(1))]
  assert_true(length(starts) == 1L, paste0("Missing PDF CreationDate in ", path, "."))
  start <- starts[[1L]]
  close_offset <- which(
    bytes[start:length(bytes)] == charToRaw(")")[[1L]]
  )[[1L]] - 1L
  end <- start + close_offset
  replacement <- charToRaw("/CreationDate (D:20260817000000+11'00)")
  assert_true(
    length(replacement) == end - start + 1L,
    paste0("Unexpected PDF CreationDate width in ", path, ".")
  )
  bytes[start:end] <- replacement
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(bytes, connection)
  invisible(path)
}

save_scope_plot <- function(plot, key, stem, width, height) {
  assert_true(key %in% standalone_scope_keys, "Only RR=0 and RR=1 get standalone figures.")
  prefixed_stem <- paste0(scope_prefixes[[key]], "-", stem)
  png <- file.path(figure_dir, paste0(prefixed_stem, ".png"))
  pdf <- file.path(figure_dir, paste0(prefixed_stem, ".pdf"))
  ggplot2::ggsave(
    png, plot, width = width, height = height, units = "in", dpi = 300,
    device = ragg::agg_png, bg = "white"
  )
  ggplot2::ggsave(
    pdf, plot, width = width, height = height, units = "in",
    device = grDevices::cairo_pdf, bg = "white"
  )
  normalize_pdf_creation_date(pdf)
  c(png = png, pdf = pdf)
}

scope_hdr_surface <- function(
    data, x, y, probabilities = c(0.95, 0.80, 0.50), n = 220L) {
  x_values <- data[[x]]
  y_values <- data[[y]]
  keep <- is.finite(x_values) & is.finite(y_values)
  x_values <- x_values[keep]
  y_values <- y_values[keep]
  assert_true(
    length(x_values) >= 20L && stats::sd(x_values) > 0 &&
      stats::sd(y_values) > 0,
    "Insufficient scope variation for a two-dimensional HDR."
  )
  hx <- MASS::bandwidth.nrd(x_values)
  hy <- MASS::bandwidth.nrd(y_values)
  assert_true(
    is.finite(hx) && hx > 0 && is.finite(hy) && hy > 0,
    "Invalid scope HDR bandwidth."
  )
  x_margin <- max(diff(range(x_values)) * 0.12, 4 * hx)
  y_margin <- max(diff(range(y_values)) * 0.12, 4 * hy)
  estimate <- MASS::kde2d(
    x_values, y_values, n = n, h = c(hx, hy),
    lims = c(
      min(x_values) - x_margin, max(x_values) + x_margin,
      min(y_values) - y_margin, max(y_values) + y_margin
    )
  )
  density <- as.vector(estimate$z)
  ordered <- order(density, decreasing = TRUE)
  cumulative <- cumsum(density[ordered]) / sum(density)
  thresholds <- vapply(probabilities, function(probability) {
    density[ordered][which(cumulative >= probability)[[1L]]]
  }, numeric(1))
  thresholds <- sort(unique(thresholds))
  assert_true(
    length(thresholds) == length(probabilities),
    "Scope HDR thresholds are not distinct."
  )
  surface <- expand.grid(x = estimate$x, y = estimate$y)
  surface$density <- density
  attr(surface, "breaks") <- c(
    thresholds, max(density) + .Machine$double.eps * max(density)
  )
  surface
}

scope_hdr_bounds <- function(surface) {
  x <- sort(unique(surface$x))
  y <- sort(unique(surface$y))
  density <- matrix(surface$density, nrow = length(x), ncol = length(y))
  contours <- do.call(c, lapply(
    head(attr(surface, "breaks"), -1L),
    function(level) grDevices::contourLines(x, y, density, levels = level)
  ))
  assert_true(length(contours) > 0L, "Scope HDR contours are empty.")
  contour_x <- unlist(lapply(contours, `[[`, "x"), use.names = FALSE)
  contour_y <- unlist(lapply(contours, `[[`, "y"), use.names = FALSE)
  assert_true(
    length(contour_x) > 0L && length(contour_y) > 0L &&
      all(is.finite(contour_x)) && all(is.finite(contour_y)),
    "Scope HDR contour coordinates are invalid."
  )
  c(
    x_min = min(contour_x), x_max = max(contour_x),
    y_min = min(contour_y), y_max = max(contour_y)
  )
}

scope_kobe_category <- function(sb, fishing) {
  ifelse(
    sb >= 1 & fishing <= 1, "Green",
    ifelse(
      sb < 1 & fishing <= 1, "Yellow",
      ifelse(sb >= 1 & fishing > 1, "Orange", "Red")
    )
  )
}

scope_majuro_category <- function(depletion, fishing) {
  ifelse(
    depletion < 0.20, "Red",
    ifelse(fishing <= 1, "Green", "Orange")
  )
}

scope_status_background <- function(
    x_boundary, diagram = c("kobe", "majuro"), x_upper = NULL) {
  diagram <- match.arg(diagram)
  if (diagram == "kobe") {
    list(
      ggplot2::annotate("rect", xmin = x_boundary, xmax = Inf, ymin = -Inf, ymax = 1, fill = "#2a9d8f", alpha = 0.72),
      ggplot2::annotate("rect", xmin = -Inf, xmax = x_boundary, ymin = -Inf, ymax = 1, fill = "#e9c46a", alpha = 0.72),
      ggplot2::annotate("rect", xmin = x_boundary, xmax = Inf, ymin = 1, ymax = Inf, fill = "#f4a261", alpha = 0.70),
      ggplot2::annotate("rect", xmin = -Inf, xmax = x_boundary, ymin = 1, ymax = Inf, fill = "#e76f51", alpha = 0.72),
      ggplot2::geom_hline(yintercept = 1, colour = "#1f2937", linewidth = 0.48),
      ggplot2::geom_vline(xintercept = x_boundary, colour = "#1f2937", linewidth = 0.48)
    )
  } else {
    if (is.null(x_upper)) x_upper <- Inf
    list(
      ggplot2::annotate("rect", xmin = -Inf, xmax = x_boundary, ymin = -Inf, ymax = Inf, fill = "#e76f51", alpha = 0.72),
      ggplot2::annotate("rect", xmin = x_boundary, xmax = Inf, ymin = -Inf, ymax = 1, fill = "#2a9d8f", alpha = 0.72),
      ggplot2::annotate("rect", xmin = x_boundary, xmax = Inf, ymin = 1, ymax = Inf, fill = "#f4a261", alpha = 0.70),
      ggplot2::annotate(
        "segment", x = x_boundary, xend = x_upper, y = 1, yend = 1,
        colour = "#1f2937", linewidth = 0.48
      ),
      ggplot2::geom_vline(xintercept = x_boundary, colour = "#1f2937", linewidth = 0.48)
    )
  }
}

scope_status_panel <- function(
    draws, central, x_column, y_column, x_boundary,
    diagram = c("kobe", "majuro"), x_label, y_label,
    fixed_x_upper = NULL, fixed_y_upper = NULL) {
  diagram <- match.arg(diagram)
  surface <- scope_hdr_surface(draws, x_column, y_column)
  x_upper <- if (is.null(fixed_x_upper)) {
    max(
      stats::quantile(draws[[x_column]], 0.995, names = FALSE),
      central[[x_column]], x_boundary, na.rm = TRUE
    ) * 1.04
  } else {
    fixed_x_upper
  }
  y_upper <- if (is.null(fixed_y_upper)) {
    max(
      stats::quantile(draws[[y_column]], 0.995, names = FALSE),
      central[[y_column]], 1, na.rm = TRUE
    ) * 1.04
  } else {
    fixed_y_upper
  }
  category <- if (diagram == "kobe") {
    scope_kobe_category(draws[[x_column]], draws[[y_column]])
  } else {
    scope_majuro_category(draws[[x_column]], draws[[y_column]])
  }
  category_levels <- if (diagram == "kobe") {
    c("Green", "Yellow", "Orange", "Red")
  } else {
    c("Green", "Orange", "Red")
  }
  probability <- as.numeric(table(factor(category, levels = category_levels))) /
    length(category)
  probability_labels <- if (diagram == "kobe") {
    data.frame(
      x = c(
        mean(c(x_boundary, x_upper)), x_boundary * 0.50,
        mean(c(x_boundary, x_upper)), x_boundary * 0.50
      ),
      y = c(
        min(0.48, y_upper * 0.24), min(0.48, y_upper * 0.24),
        mean(c(1, y_upper)), mean(c(1, y_upper))
      ),
      label = scales::percent(probability, accuracy = 0.1)
    )
  } else {
    data.frame(
      x = c(
        mean(c(x_boundary, x_upper)),
        mean(c(x_boundary, x_upper)), x_boundary * 0.50
      ),
      y = c(
        min(0.48, y_upper * 0.24), mean(c(1, y_upper)), y_upper * 0.50
      ),
      label = scales::percent(probability, accuracy = 0.1)
    )
  }
  encoded_current_points <- all(c(
    "hessian_status", "point_alpha"
  ) %in% names(central))
  contour_alpha <- if (encoded_current_points) 0.82 else 0.78
  contour_colour <- if (encoded_current_points) "#285F6B" else "#426D76"
  contour_linewidth <- if (encoded_current_points) 0.42 else 0.34
  plot <- ggplot2::ggplot() +
    scope_status_background(x_boundary, diagram, x_upper) +
    ggplot2::geom_contour_filled(
      data = surface,
      ggplot2::aes(x = .data$x, y = .data$y, z = .data$density),
      breaks = attr(surface, "breaks"), alpha = contour_alpha,
      colour = contour_colour, linewidth = contour_linewidth
    ) +
    ggplot2::scale_fill_manual(
      values = c("#D4EDF1", "#8BC9D3", "#2F93A5"),
      labels = c("95% HDR", "80% HDR", "50% HDR"), name = NULL
    )
  if (encoded_current_points) {
    plot <- plot +
      ggplot2::geom_point(
        data = central,
        ggplot2::aes(
          x = .data[[x_column]], y = .data[[y_column]],
          shape = .data$hessian_status, alpha = .data$point_alpha
        ), colour = "#0B6477", size = 2.25, stroke = 0.75
      ) +
      ggplot2::scale_shape_manual(
        values = c("PDH" = 16, "Near-PDH" = 1),
        breaks = c("PDH", "Near-PDH"),
        labels = c("PDH Hessian", "Near-PDH Hessian"), name = NULL
      ) +
      ggplot2::scale_alpha_identity() +
      ggplot2::guides(
        fill = ggplot2::guide_legend(order = 1, nrow = 1, byrow = TRUE),
        shape = ggplot2::guide_legend(order = 2, nrow = 1, byrow = TRUE)
      )
  } else {
    plot <- plot +
      ggplot2::geom_point(
        data = central,
        ggplot2::aes(x = .data[[x_column]], y = .data[[y_column]]),
        colour = "#0B6477", size = 1.24, alpha = 0.28
      )
  }
  label_alpha <- if (encoded_current_points) 0.82 else 0.84
  label_size <- if (encoded_current_points) 3.05 else 3.0
  panel_theme <- if (encoded_current_points) {
    theme_main_report(11.6) +
      ggplot2::theme(
        legend.position = "bottom", legend.box = "horizontal",
        legend.text = ggplot2::element_text(size = 8.8),
        legend.key.width = grid::unit(0.66, "cm"),
        legend.spacing.x = grid::unit(0.14, "cm"),
        plot.margin = ggplot2::margin(3, 4, 3, 4)
      )
  } else {
    theme_main_report(11.2) +
      ggplot2::theme(
        legend.position = "bottom", legend.box = "horizontal",
        legend.text = ggplot2::element_text(size = 8.8),
        legend.key.width = grid::unit(0.70, "cm"),
        plot.margin = ggplot2::margin(3, 5, 3, 5)
      )
  }
  plot +
    ggplot2::geom_label(
      data = probability_labels,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      inherit.aes = FALSE, colour = "#173042", fill = "white",
      alpha = label_alpha, linewidth = 0.15, size = label_size,
      fontface = "bold", label.padding = grid::unit(0.12, "lines")
    ) +
    ggplot2::coord_cartesian(
      xlim = c(0, x_upper), ylim = c(0, y_upper), expand = FALSE
    ) +
    ggplot2::labs(x = x_label, y = y_label) +
    panel_theme
}

scope_history_upper <- setNames(vapply(history_specs$metric, function(metric) {
  max(history_summary$q975[history_summary$Metric == metric], na.rm = TRUE) * 1.04
}, numeric(1)), history_specs$metric)

scope_history_figure <- function(key) {
  ids <- group_ids[[key]]
  panels <- lapply(seq_len(nrow(history_specs)), function(index) {
    spec <- history_specs[index, ]
    structural <- series_retained[
      series_retained$ensemble_id %in% ids, , drop = FALSE
    ]
    distribution <- if (spec$estimation) {
      hybrid_annual[hybrid_annual$ensemble_id %in% ids, , drop = FALSE]
    } else {
      structural
    }
    summary <- summarise_by(distribution, spec$column, "year")
    plot <- ggplot2::ggplot(summary, ggplot2::aes(x = .data$year)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$q025, ymax = .data$q975, fill = "95% interval"),
        alpha = 0.34
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$q10, ymax = .data$q90, fill = "80% interval"),
        alpha = 0.50
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$q25, ymax = .data$q75, fill = "50% interval"),
        alpha = 0.66
      ) +
      ggplot2::geom_line(
        data = structural,
        ggplot2::aes(
          x = .data$year, y = .data[[spec$column]],
          group = .data$ensemble_id
        ),
        inherit.aes = FALSE, colour = "#6F7F87", linewidth = 0.18,
        alpha = 0.11
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$median, colour = "Median"), linewidth = 0.92
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          "95% interval" = "#D5E9ED", "80% interval" = "#9CCFD8",
          "50% interval" = "#53AAB9"
        ), name = NULL
      ) +
      ggplot2::scale_colour_manual(
        values = c("Median" = "#07566B"), name = NULL
      ) +
      ggplot2::scale_x_continuous(breaks = seq(1960, 2020, 20)) +
      ggplot2::coord_cartesian(
        ylim = c(0, scope_history_upper[[spec$metric]])
      ) +
      ggplot2::labs(
        x = "Year", y = spec$metric,
        subtitle = if (spec$estimation) {
          "Structural + available Hessian estimation uncertainty"
        } else {
          "Structural uncertainty only; Hessian F draws unavailable"
        }
      ) + theme_rr(12.0)
    if (spec$column == "depletion") {
      plot <- plot + ggplot2::geom_hline(
        yintercept = 0.20, colour = "#B83232", linetype = "33", linewidth = 0.55
      )
    }
    plot
  })
  (panels[[1L]] | panels[[2L]]) / (panels[[3L]] | panels[[4L]]) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = paste0(group_labels[[key]], " — annual uncertainty"),
      subtitle = paste0(length(ids), " equal-weight retained source models"),
      tag_levels = "a"
    ) & ggplot2::theme(legend.position = "bottom")
}

current_status_limit_components <- lapply(
  standalone_scope_keys, function(scope_key) {
    scope_ids <- group_ids[[scope_key]]
    draws <- hybrid_management[
      hybrid_management$ensemble_id %in% scope_ids, , drop = FALSE
    ]
    central <- management_retained[
      management_retained$ensemble_id %in% scope_ids, , drop = FALSE
    ]
    kobe_bounds <- scope_hdr_bounds(scope_hdr_surface(
      draws, "sb_recent_sbmsy", "f_recent_fmsy"
    ))
    majuro_bounds <- scope_hdr_bounds(scope_hdr_surface(
      draws, "sb_recent_sb0", "f_recent_fmsy"
    ))
    assert_true(
      min(kobe_bounds[c("x_min", "y_min")]) >= 0 &&
        min(majuro_bounds[c("x_min", "y_min")]) >= 0,
      paste0("A current-status HDR crosses a zero axis for ", scope_key, ".")
    )
    c(
      kobe_x = max(
        stats::quantile(
          draws$sb_recent_sbmsy, 0.995, names = FALSE, na.rm = TRUE
        ),
        central$sb_recent_sbmsy, 1, kobe_bounds[["x_max"]], na.rm = TRUE
      ),
      majuro_x = max(
        stats::quantile(
          draws$sb_recent_sb0, 0.995, names = FALSE, na.rm = TRUE
        ),
        central$sb_recent_sb0, 0.20, majuro_bounds[["x_max"]], na.rm = TRUE
      ),
      y = max(
        stats::quantile(
          draws$f_recent_fmsy, 0.995, names = FALSE, na.rm = TRUE
        ),
        central$f_recent_fmsy, 1,
        kobe_bounds[["y_max"]], majuro_bounds[["y_max"]], na.rm = TRUE
      )
    )
  }
)
combined_current_status_limits <- list(
  kobe_x = max(vapply(
    current_status_limit_components, `[[`, numeric(1), "kobe_x"
  )) * 1.01,
  majuro_x = max(vapply(
    current_status_limit_components, `[[`, numeric(1), "majuro_x"
  )) * 1.01,
  y = max(vapply(
    current_status_limit_components, `[[`, numeric(1), "y"
  )) * 1.01
)

scope_current_status_figure <- function(key) {
  ids <- group_ids[[key]]
  draws <- hybrid_management[hybrid_management$ensemble_id %in% ids, , drop = FALSE]
  assert_true(
    nrow(draws) == length(ids) * 100L &&
      all(table(draws$ensemble_id) == 100L),
    paste0("Current-status mixture is not equal-model-weight for ", key, ".")
  )
  central <- Reduce(
    function(x, y) merge(x, y, by = "ensemble_id", sort = FALSE),
    list(
      management_retained[
        management_retained$ensemble_id %in% ids, , drop = FALSE
      ],
      fit_retained[
        fit_retained$ensemble_id %in% ids,
        c("ensemble_id", "maximum_gradient", "positive_definite_hessian"),
        drop = FALSE
      ]
    )
  )
  central$hessian_status <- ifelse(
    central$positive_definite_hessian, "PDH", "Near-PDH"
  )
  central$point_alpha <- ifelse(central$maximum_gradient <= 1e-4, 0.90, 0.46)
  kobe <- scope_status_panel(
    draws, central, "sb_recent_sbmsy", "f_recent_fmsy", 1, "kobe",
    expression(italic(SB)[recent] / italic(SB)[MSY]),
    expression(italic(F)[recent] / italic(F)[MSY]),
    combined_current_status_limits$kobe_x,
    combined_current_status_limits$y
  )
  majuro <- scope_status_panel(
    draws, central, "sb_recent_sb0", "f_recent_fmsy", 0.20, "majuro",
    expression(italic(SB)[recent] / italic(SB)[italic(F) == 0]),
    expression(italic(F)[recent] / italic(F)[MSY]),
    combined_current_status_limits$majuro_x,
    combined_current_status_limits$y
  )
  (kobe | majuro) + patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(tag_levels = "a") &
    ggplot2::theme(legend.position = "bottom", legend.box = "horizontal")
}

scope_dynamic_panel <- function(
    data, diagram = c("kobe", "majuro"), x_upper = NULL,
    y_upper = NULL, panel_title = NULL) {
  diagram <- match.arg(diagram)
  x_column <- if (diagram == "kobe") "sb_sbmsy" else "depletion"
  boundary <- if (diagram == "kobe") 1 else 0.20
  if (is.null(x_upper)) x_upper <- max(data[[x_column]], boundary) * 1.05
  if (is.null(y_upper)) y_upper <- max(data$f_fmsy, 1) * 1.06
  terminal_row <- data[data$year == max(data$year), , drop = FALSE]
  ggplot2::ggplot() +
    scope_status_background(boundary, diagram, x_upper) +
    ggplot2::geom_path(
      data = data,
      ggplot2::aes(
        x = .data[[x_column]], y = .data$f_fmsy, colour = .data$year
      ), linewidth = 0.84, alpha = 0.90
    ) +
    ggplot2::geom_point(
      data = data[data$year %% 4L == 0L | data$year %in% c(1952L, 2024L), ],
      ggplot2::aes(
        x = .data[[x_column]], y = .data$f_fmsy, fill = .data$year
      ), shape = 21, size = 1.9, stroke = 0.28, colour = "white"
    ) +
    ggplot2::geom_point(
      data = terminal_row,
      ggplot2::aes(
        x = .data[[x_column]], y = .data$f_fmsy, fill = .data$year
      ), shape = 21, size = 3.4, stroke = 0.75, colour = "#102A3A"
    ) +
    ggplot2::scale_colour_viridis_c(
      option = "C", begin = 0.12, end = 0.95, direction = -1, name = "Year",
      breaks = c(1960, 1980, 2000, 2020),
      guide = ggplot2::guide_colourbar(
        barwidth = grid::unit(5.0, "cm"), barheight = grid::unit(0.35, "cm")
      )
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "C", begin = 0.12, end = 0.95, direction = -1,
      guide = "none"
    ) +
    ggplot2::coord_cartesian(
      xlim = c(0, x_upper), ylim = c(0, y_upper), expand = FALSE
    ) +
    ggplot2::labs(
      title = panel_title,
      x = if (diagram == "kobe") "SBt / SBMSY,t" else "SBt / SBF=0,t",
      y = "Ft / FMSY,t"
    ) + theme_rr(11.5) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 12.5)
    )
}

combined_dynamic_status <- stats::aggregate(
  cbind(depletion, sb_sbmsy, f_fmsy) ~ year,
  series_retained, stats::median
)
combined_dynamic_limits <- list(
  y = max(diagnostic_dynamic$f_fmsy, combined_dynamic_status$f_fmsy, 1) * 1.06,
  majuro_x = max(
    diagnostic_dynamic$depletion, combined_dynamic_status$depletion, 0.20
  ) * 1.03,
  kobe_x = max(
    diagnostic_dynamic$sb_sbmsy, combined_dynamic_status$sb_sbmsy, 1
  ) * 1.04
)

scope_dynamic_figure <- function(key) {
  ids <- group_ids[[key]]
  scope_series <- series_retained[
    series_retained$ensemble_id %in% ids, , drop = FALSE
  ]
  scope_dynamic <- stats::aggregate(
    cbind(depletion, sb_sbmsy, f_fmsy) ~ year,
    scope_series, stats::median
  )
  common_y <- combined_dynamic_limits$y
  common_majuro_x <- combined_dynamic_limits$majuro_x
  common_kobe_x <- combined_dynamic_limits$kobe_x
  scope_title <- paste0(group_short_labels[[key]], " median")
  (
    scope_dynamic_panel(
      diagnostic_dynamic, "majuro", common_majuro_x, common_y,
      "Diagnostic model"
    ) |
      scope_dynamic_panel(
        scope_dynamic, "majuro", common_majuro_x, common_y, scope_title
      )
  ) / (
    scope_dynamic_panel(
      diagnostic_dynamic, "kobe", common_kobe_x, common_y
    ) |
      scope_dynamic_panel(
        scope_dynamic, "kobe", common_kobe_x, common_y
      )
  ) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = paste0(group_labels[[key]], " — time-dynamic status"),
      subtitle = "Shared diagnostic reference and annual scope median; central models only, with no Hessian estimation uncertainty",
      tag_levels = "a"
    ) & ggplot2::theme(legend.position = "bottom")
}

combined_steepness_breaks <- unique(stats::quantile(
  design_retained$steepness, probs = seq(0, 1, 0.25), names = FALSE
))
combined_mortality_breaks <- unique(stats::quantile(
  design_retained$m_age40_quarterly,
  probs = seq(0, 1, 0.25), names = FALSE
))

scope_quantile_factor <- function(value, prefix, breaks) {
  assert_true(length(breaks) >= 3L, paste0("Too few distinct ", prefix, " values."))
  assert_true(
    min(value) >= min(breaks) && max(value) <= max(breaks),
    paste0("Scope ", prefix, " values exceed the combined-scope breaks.")
  )
  cut(value, breaks = breaks, include.lowest = TRUE, dig.lab = 4)
}

scope_axis_long <- function(data, axes) {
  do.call(rbind, lapply(names(axes), function(axis_name) {
    categories <- as.character(data[[axes[[axis_name]]]])
    rbind(
      data.frame(
        ensemble_id = data$ensemble_id, Axis = axis_name,
        Category = categories, Quantity = "Frecent / FMSY",
        Value = data$f_recent_fmsy
      ),
      data.frame(
        ensemble_id = data$ensemble_id, Axis = axis_name,
        Category = categories, Quantity = "SBrecent / SBF=0",
        Value = data$sb_recent_sb0
      )
    )
  }))
}

scope_axis_plot <- function(long, combined_long, title) {
  axis_levels <- unique(combined_long$Axis)
  long$Axis <- factor(long$Axis, levels = axis_levels)
  combined_long$Axis <- factor(combined_long$Axis, levels = axis_levels)
  long$Quantity <- factor(
    long$Quantity, levels = c("Frecent / FMSY", "SBrecent / SBF=0")
  )
  combined_long$Quantity <- factor(
    combined_long$Quantity,
    levels = c("Frecent / FMSY", "SBrecent / SBF=0")
  )
  ggplot2::ggplot(long, ggplot2::aes(x = .data$Category, y = .data$Value)) +
    ggplot2::geom_blank(
      data = combined_long,
      ggplot2::aes(x = .data$Category, y = .data$Value)
    ) +
    ggplot2::geom_violin(
      fill = "#65AFE4", colour = "#264E63", alpha = 0.72,
      linewidth = 0.44, trim = TRUE, scale = "width"
    ) +
    ggplot2::geom_boxplot(
      width = 0.15, outlier.shape = NA, fill = "white", alpha = 0.78,
      colour = "#173B4D", linewidth = 0.40
    ) +
    ggplot2::facet_grid(Quantity ~ Axis, scales = "free") +
    ggplot2::labs(
      title = title,
      subtitle = "Available Hessian uncertainty; descriptive marginal distributions",
      x = NULL, y = NULL
    ) + theme_rr(12.0) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 24, hjust = 1, size = 10.3)
    )
}

scope_axis_figures <- function(key) {
  ids <- group_ids[[key]]
  draws <- hybrid_management[hybrid_management$ensemble_id %in% ids, , drop = FALSE]
  axis_data <- merge(
    draws,
    design_retained[c(
      "ensemble_id", "steepness", "m_age40_quarterly", "tag_tau",
      "tag_mixing_k_cutoff", "effort_creep_primary", "effort_creep_secondary"
    )],
    by = "ensemble_id", sort = FALSE
  )
  axis_data$steepness_group <- scope_quantile_factor(
    axis_data$steepness, "steepness", combined_steepness_breaks
  )
  axis_data$m_group <- scope_quantile_factor(
    axis_data$m_age40_quarterly, "natural-mortality",
    combined_mortality_breaks
  )
  axis_data$effort_group <- paste0(
    format(100 * axis_data$effort_creep_primary, trim = TRUE), "/",
    format(100 * axis_data$effort_creep_secondary, trim = TRUE), "%"
  )
  combined_axis_data <- merge(
    hybrid_management,
    design_retained[c(
      "ensemble_id", "steepness", "m_age40_quarterly", "tag_tau",
      "tag_mixing_k_cutoff", "effort_creep_primary", "effort_creep_secondary"
    )],
    by = "ensemble_id", sort = FALSE
  )
  combined_axis_data$steepness_group <- scope_quantile_factor(
    combined_axis_data$steepness, "steepness", combined_steepness_breaks
  )
  combined_axis_data$m_group <- scope_quantile_factor(
    combined_axis_data$m_age40_quarterly, "natural-mortality",
    combined_mortality_breaks
  )
  combined_axis_data$effort_group <- paste0(
    format(100 * combined_axis_data$effort_creep_primary, trim = TRUE), "/",
    format(100 * combined_axis_data$effort_creep_secondary, trim = TRUE), "%"
  )
  list(
    continuous = scope_axis_plot(
      scope_axis_long(axis_data, c(
        "Steepness quartile" = "steepness_group",
        "Natural mortality quartile" = "m_group"
      )),
      scope_axis_long(combined_axis_data, c(
        "Steepness quartile" = "steepness_group",
        "Natural mortality quartile" = "m_group"
      )),
      paste0(group_labels[[key]], " — continuous ensemble axes")
    ),
    tag = scope_axis_plot(
      scope_axis_long(axis_data, c(
        "Tag overdispersion τ" = "tag_tau",
        "Tag mixing cutoff" = "tag_mixing_k_cutoff"
      )),
      scope_axis_long(combined_axis_data, c(
        "Tag overdispersion τ" = "tag_tau",
        "Tag mixing cutoff" = "tag_mixing_k_cutoff"
      )),
      paste0(group_labels[[key]], " — tag-model axes")
    ),
    effort = scope_axis_plot(
      scope_axis_long(axis_data, c(
        "Effort creep (primary/secondary)" = "effort_group"
      )),
      scope_axis_long(combined_axis_data, c(
        "Effort creep (primary/secondary)" = "effort_group"
      )),
      paste0(group_labels[[key]], " — effort-creep axis")
    )
  )
}

combined_terminal_status_limits <- list(
  kobe_x = max(terminal$terminal_sb_sbmsy, 1) * 1.03,
  majuro_x = max(terminal$sb_recent_sb0, 0.20) * 1.03,
  y = max(terminal$terminal_f_fmsy, 1) * 1.03
)

scope_terminal_figure <- function(key) {
  data <- scope_products[[key]]$terminal_rows
  kobe <- scope_status_panel(
    data, data, "terminal_sb_sbmsy", "terminal_f_fmsy", 1, "kobe",
    expression(bar(italic(SB))[2051:2054] / italic(SB)[MSY]),
    expression(bar(italic(F))[2050:2053] / italic(F)[MSY]),
    combined_terminal_status_limits$kobe_x,
    combined_terminal_status_limits$y
  )
  majuro <- scope_status_panel(
    data, data, "sb_recent_sb0", "terminal_f_fmsy", 0.20, "majuro",
    expression(
      bar(italic(SB))[2051:2054] / italic(SB)[italic(F) == 0]
    ),
    expression(bar(italic(F))[2050:2053] / italic(F)[MSY]),
    combined_terminal_status_limits$majuro_x,
    combined_terminal_status_limits$y
  )
  (kobe | majuro) + patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(tag_levels = "a") &
    ggplot2::theme(legend.position = "bottom", legend.box = "horizontal")
}

combined_projection_f_upper <- max(c(1, unlist(lapply(
  standalone_scope_keys, function(scope_key) {
    scope_ids <- group_ids[[scope_key]]
    scope_f <- rbind(
      data.frame(
        Period = "Current",
        Value = hybrid_management$f_recent_fmsy[
          hybrid_management$ensemble_id %in% scope_ids
        ]
      ),
      data.frame(
        Period = "2050–2053",
        Value = terminal$terminal_f_fmsy[terminal$ensemble_id %in% scope_ids]
      )
    )
    max(summarise_by(scope_f, "Value", "Period")$q975)
  }
)))) * 1.05

scope_projection_key_figure <- function(key) {
  ids <- group_ids[[key]]
  projected <- projection_management$projected[
    projection_management$projected$ensemble_id %in% ids, , drop = FALSE
  ]
  risk <- stats::aggregate(below_lrp_020 ~ year, projected, mean)
  risk_plot <- ggplot2::ggplot(
    risk, ggplot2::aes(x = .data$year, y = .data$below_lrp_020)
  ) +
    ggplot2::geom_area(fill = "#9CCFD8", alpha = 0.70) +
    ggplot2::geom_line(colour = "#07566B", linewidth = 0.92) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1), limits = c(0, 1)
    ) +
    ggplot2::labs(
      x = "Year", y = "Simulation frequency below LRP",
      subtitle = "Projection recruitment variation; no Hessian draws"
    ) + theme_rr(12.0)

  current_f <- hybrid_management$f_recent_fmsy[
    hybrid_management$ensemble_id %in% ids
  ]
  terminal_f <- scope_products[[key]]$terminal_rows$terminal_f_fmsy
  fishing <- rbind(
    data.frame(Period = "Current", Value = current_f),
    data.frame(Period = "2050–2053", Value = terminal_f)
  )
  fishing$Period <- factor(fishing$Period, levels = c("Current", "2050–2053"))
  fishing_summary <- summarise_by(fishing, "Value", "Period")
  fishing_plot <- ggplot2::ggplot(
    fishing_summary, ggplot2::aes(x = .data$Period, y = .data$median)
  ) +
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$q025, ymax = .data$q975),
      colour = "#A9CFD6", linewidth = 3.5, lineend = "round"
    ) +
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$q10, ymax = .data$q90),
      colour = "#5EAAB7", linewidth = 5.3, lineend = "round"
    ) +
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$q25, ymax = .data$q75),
      colour = "#167789", linewidth = 7.0, lineend = "round"
    ) +
    ggplot2::geom_point(colour = "#082F43", size = 2.7) +
    ggplot2::geom_hline(
      yintercept = 1, colour = "#B83232", linetype = "33", linewidth = 0.55
    ) +
    ggplot2::coord_cartesian(ylim = c(0, combined_projection_f_upper)) +
    ggplot2::labs(
      x = NULL, y = "F / FMSY",
      subtitle = "Current: Hessian included · terminal: recruitment only"
    ) + theme_rr(12.0)
  (risk_plot | fishing_plot) +
    patchwork::plot_annotation(
      title = paste0(group_labels[[key]], " — projection key quantities"),
      tag_levels = "a"
    )
}

scope_trajectory_panel <- function(
    historical, projected, value, y_label, fixed_upper,
    reference = NULL, historical_trajectories = NULL,
    projected_trajectories = NULL) {
  h <- summarise_by(historical, value, "year")
  p <- summarise_by(projected, value, "year")
  historical_end <- h[which.max(h$year), c("year", "median"), drop = FALSE]
  projection_start <- p[which.min(p$year), c("year", "median"), drop = FALSE]
  assert_true(
    nrow(historical_end) == 1L && nrow(projection_start) == 1L &&
      historical_end$year == 2024L && projection_start$year == 2025L &&
      is.finite(historical_end$median) && is.finite(projection_start$median),
    paste0("Invalid 2024–2025 transition for ", value, ".")
  )
  transition <- data.frame(
    year_historical = historical_end$year,
    year_projected = projection_start$year,
    median_historical = historical_end$median,
    median_projected = projection_start$median
  )
  assert_true(
    is.data.frame(historical_trajectories) &&
      is.data.frame(projected_trajectories) &&
      all(c("trajectory_id", "year", "value") %in%
        names(historical_trajectories)) &&
      all(c("trajectory_id", "year", "value") %in%
        names(projected_trajectories)),
    paste0("Representative trajectories are missing for ", value, ".")
  )
  trajectory_transition <- merge(
    historical_trajectories[
      historical_trajectories$year == max(historical_trajectories$year),
      c("trajectory_id", "year", "value"), drop = FALSE
    ],
    projected_trajectories[
      projected_trajectories$year == min(projected_trajectories$year),
      c("trajectory_id", "year", "value"), drop = FALSE
    ],
    by = "trajectory_id", suffixes = c("_historical", "_projected")
  )
  assert_true(
    nrow(trajectory_transition) == 10L &&
      all(trajectory_transition$year_historical == 2024L) &&
      all(trajectory_transition$year_projected == 2025L) &&
      all(is.finite(trajectory_transition$value_historical)) &&
      all(is.finite(trajectory_transition$value_projected)),
    paste0("Representative 2024–2025 links are invalid for ", value, ".")
  )
  plot <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = h,
      ggplot2::aes(x = .data$year, ymin = .data$q025, ymax = .data$q975),
      fill = "#C8D0D4", alpha = 0.28
    ) +
    ggplot2::geom_ribbon(
      data = h,
      ggplot2::aes(x = .data$year, ymin = .data$q10, ymax = .data$q90),
      fill = "#9FABAF", alpha = 0.34
    ) +
    ggplot2::geom_ribbon(
      data = h,
      ggplot2::aes(x = .data$year, ymin = .data$q25, ymax = .data$q75),
      fill = "#75868D", alpha = 0.40
    ) +
    ggplot2::geom_ribbon(
      data = p,
      ggplot2::aes(
        x = .data$year, ymin = .data$q025, ymax = .data$q975,
        fill = "95% interval"
      ), alpha = 0.42
    ) +
    ggplot2::geom_ribbon(
      data = p,
      ggplot2::aes(
        x = .data$year, ymin = .data$q10, ymax = .data$q90,
        fill = "80% interval"
      ), alpha = 0.54
    ) +
    ggplot2::geom_ribbon(
      data = p,
      ggplot2::aes(
        x = .data$year, ymin = .data$q25, ymax = .data$q75,
        fill = "50% interval"
      ), alpha = 0.66
    ) +
    ggplot2::geom_line(
      data = historical_trajectories,
      ggplot2::aes(
        x = .data$year, y = .data$value, group = .data$trajectory_id
      ), colour = "#687A83", linewidth = 0.25, alpha = 0.27
    ) +
    ggplot2::geom_line(
      data = projected_trajectories,
      ggplot2::aes(
        x = .data$year, y = .data$value, group = .data$trajectory_id
      ), colour = "#218899", linewidth = 0.30, alpha = 0.40
    ) +
    ggplot2::geom_segment(
      data = trajectory_transition,
      ggplot2::aes(
        x = .data$year_historical, xend = .data$year_projected,
        y = .data$value_historical, yend = .data$value_projected,
        group = .data$trajectory_id
      ), colour = "#218899", linewidth = 0.28, alpha = 0.36
    ) +
    ggplot2::geom_line(
      data = h,
      ggplot2::aes(x = .data$year, y = .data$median, colour = "Historical"),
      linewidth = 0.72
    ) +
    ggplot2::geom_line(
      data = p,
      ggplot2::aes(x = .data$year, y = .data$median, colour = "Projection"),
      linewidth = 0.90
    ) +
    ggplot2::geom_segment(
      data = transition,
      ggplot2::aes(
        x = .data$year_historical, xend = .data$year_projected,
        y = .data$median_historical, yend = .data$median_projected
      ),
      inherit.aes = FALSE, colour = "#07566B", linewidth = 0.86
    ) +
    ggplot2::geom_vline(
      xintercept = 2024.5, colour = "#5D6C73", linewidth = 0.48,
      linetype = "33"
    ) +
    ggplot2::scale_colour_manual(
      values = c("Historical" = "#4F626C", "Projection" = "#07566B"),
      name = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "50% interval" = "#53AAB9", "80% interval" = "#9CCFD8",
        "95% interval" = "#D5E9ED"
      ), name = NULL
    ) +
    ggplot2::scale_x_continuous(breaks = c(1960, 1990, 2024, 2054)) +
    ggplot2::coord_cartesian(ylim = c(0, fixed_upper)) +
    ggplot2::labs(x = "Year", y = y_label) + theme_main_report(10.8) +
    ggplot2::theme(
      legend.position = "bottom", legend.box = "vertical",
      legend.text = ggplot2::element_text(size = 8.2),
      legend.key.width = grid::unit(0.75, "cm"),
      axis.title = ggplot2::element_text(size = 10.8),
      axis.text = ggplot2::element_text(size = 9.4),
      plot.margin = ggplot2::margin(4, 5, 4, 5)
    )
  if (!is.null(reference)) {
    reference_data <- data.frame(
      y = reference, reference = "Stock-wide LRP (0.20)"
    )
    plot <- plot +
      ggplot2::geom_hline(
        data = reference_data,
        ggplot2::aes(yintercept = .data$y, linetype = .data$reference),
        colour = "#B83232", linewidth = 0.54
      ) +
      ggplot2::scale_linetype_manual(
        values = c("Stock-wide LRP (0.20)" = "33"), name = NULL
      )
  }
  plot
}

projection_spawning_all <- projection$annual_stock
projection_spawning_all$spawning_potential <-
  projection_spawning_all$spawning_biomass_mt / 1000

scope_representative_keys <- function(key) {
  ids <- group_ids[[key]]
  terminal_rank <- projection_management$projected[
    projection_management$projected$ensemble_id %in% ids &
      projection_management$projected$year ==
        max(projection_management$projected$year),
    c("ensemble_id", "simulation", "sb_recent_sb0"), drop = FALSE
  ]
  terminal_rank <- terminal_rank[
    order(terminal_rank$sb_recent_sb0), , drop = FALSE
  ]
  representative_positions <- local({
    set.seed(20260806L + match(key, standalone_scope_keys))
    sort(sample.int(nrow(terminal_rank), size = 10L, replace = FALSE))
  })
  representative_keys <- terminal_rank[
    representative_positions, c("ensemble_id", "simulation"), drop = FALSE
  ]
  representative_keys$trajectory_id <- paste(
    representative_keys$ensemble_id,
    representative_keys$simulation,
    sep = " / "
  )
  representative_keys
}

scope_trajectory_upper <- function(historical, projected, value) {
  max(c(0, unlist(lapply(standalone_scope_keys, function(scope_key) {
    scope_ids <- group_ids[[scope_key]]
    historical_summary <- summarise_by(
      historical[historical$ensemble_id %in% scope_ids, , drop = FALSE],
      value, "year"
    )
    projected_summary <- summarise_by(
      projected[projected$ensemble_id %in% scope_ids, , drop = FALSE],
      value, "year"
    )
    representative_keys <- scope_representative_keys(scope_key)
    representative_historical <- merge(
      representative_keys["ensemble_id"],
      historical[c("ensemble_id", value)],
      by = "ensemble_id", sort = FALSE
    )
    representative_projected <- merge(
      representative_keys[c("ensemble_id", "simulation")],
      projected[c("ensemble_id", "simulation", value)],
      by = c("ensemble_id", "simulation"), sort = FALSE
    )
    c(
      historical_summary$q975, projected_summary$q975,
      representative_historical[[value]], representative_projected[[value]]
    )
  }))), na.rm = TRUE) * 1.01
}

combined_projection_trajectory_limits <- c(
  depletion = scope_trajectory_upper(
    projection_management$historical,
    projection_management$projected,
    "sb_recent_sb0"
  ),
  spawning = scope_trajectory_upper(
    series_retained,
    projection_spawning_all,
    "spawning_potential"
  )
)

scope_representative_paths <- function(
    keys, historical, projected, value) {
  historical_paths <- merge(
    keys[c("ensemble_id", "trajectory_id")],
    historical[c("ensemble_id", "year", value)],
    by = "ensemble_id", sort = FALSE
  )
  projected_paths <- merge(
    keys,
    projected[c("ensemble_id", "simulation", "year", value)],
    by = c("ensemble_id", "simulation"), sort = FALSE
  )
  names(historical_paths)[names(historical_paths) == value] <- "value"
  names(projected_paths)[names(projected_paths) == value] <- "value"
  assert_true(
    length(unique(historical_paths$trajectory_id)) == 10L &&
      length(unique(projected_paths$trajectory_id)) == 10L,
    paste0("Representative path coverage is incomplete for ", value, ".")
  )
  list(historical = historical_paths, projected = projected_paths)
}

scope_projection_stock_figure <- function(key) {
  ids <- group_ids[[key]]
  historical_depletion <- projection_management$historical[
    projection_management$historical$ensemble_id %in% ids, , drop = FALSE
  ]
  projected_depletion <- projection_management$projected[
    projection_management$projected$ensemble_id %in% ids, , drop = FALSE
  ]
  historical_spawning <- series_retained[
    series_retained$ensemble_id %in% ids,
    c("ensemble_id", "year", "spawning_potential"), drop = FALSE
  ]
  projected_spawning <- projection_spawning_all[
    projection_spawning_all$ensemble_id %in% ids, , drop = FALSE
  ]
  representative_keys <- scope_representative_keys(key)
  depletion_paths <- scope_representative_paths(
    representative_keys, historical_depletion, projected_depletion,
    "sb_recent_sb0"
  )
  spawning_paths <- scope_representative_paths(
    representative_keys, historical_spawning, projected_spawning,
    "spawning_potential"
  )
  depletion_plot <- scope_trajectory_panel(
    historical_depletion, projected_depletion, "sb_recent_sb0",
    expression(italic(SB)[recent] / italic(SB)[italic(F) == 0]),
    combined_projection_trajectory_limits[["depletion"]],
    reference = 0.20,
    historical_trajectories = depletion_paths$historical,
    projected_trajectories = depletion_paths$projected
  )
  spawning_plot <- scope_trajectory_panel(
    historical_spawning, projected_spawning, "spawning_potential",
    expression(Spawning~potential~(10^3~plain(t))),
    combined_projection_trajectory_limits[["spawning"]],
    historical_trajectories = spawning_paths$historical,
    projected_trajectories = spawning_paths$projected
  )
  (depletion_plot / spawning_plot) +
    patchwork::plot_layout(guides = "collect", heights = c(1, 1)) +
    patchwork::plot_annotation(tag_levels = "a") &
    ggplot2::theme(
      legend.position = "bottom", legend.box = "vertical",
      plot.tag = ggplot2::element_text(
        face = "bold", colour = "#183246", size = 12
      )
    )
}

scope_figure_files <- setNames(vector("list", length(group_keys)), group_keys)
for (key in standalone_scope_keys) {
  axis_plots <- scope_axis_figures(key)
  scope_figure_files[[key]] <- list(
    history = save_scope_plot(
      scope_history_figure(key), key,
      "combined-structural-estimation-uncertainty", 11.8, 8.2
    ),
    current_status = save_scope_plot(
      scope_current_status_figure(key), key,
      "combined-kobe-majuro-status", 7.1, 4.1
    ),
    dynamic_status = save_scope_plot(
      scope_dynamic_figure(key), key,
      "time-dynamic-kobe-majuro", 11.8, 8.2
    ),
    continuous_axes = save_scope_plot(
      axis_plots$continuous, key,
      "management-uncertainty-continuous-axes", 11.8, 7.4
    ),
    tag_axes = save_scope_plot(
      axis_plots$tag, key,
      "management-uncertainty-discrete-axes-a", 11.8, 7.4
    ),
    terminal_status = save_scope_plot(
      scope_terminal_figure(key), key,
      "projection-terminal-status", 7.1, 4.25
    ),
    projection_key = save_scope_plot(
      scope_projection_key_figure(key), key,
      "projection-key-quantities", 11.8, 5.0
    ),
    projection_stock = save_scope_plot(
      scope_projection_stock_figure(key), key,
      "projection-stock-trajectories", 10.8, 8.4
    )
  )
  assert_true(
    length(list.files(
      figure_dir,
      pattern = paste0("^", scope_prefixes[[key]], ".*[.]png$")
    )) == 8L &&
      length(list.files(
        figure_dir,
        pattern = paste0("^", scope_prefixes[[key]], ".*[.]pdf$")
      )) == 8L,
    paste0("Standalone scope figure count is not 8 PNG + 8 PDF for ", key, ".")
  )
}

standalone_report_paths <- c(
  inclusion = file.path(output_dir, "bet-2026-ensemble-report-rr0-inclusion.html"),
  exclusion = file.path(output_dir, "bet-2026-ensemble-report-rr1-exclusion.html")
)
for (key in standalone_scope_keys) {
  standalone_html <- scope_report_html(key)
  assert_true(
    !grepl("https?://", standalone_html) &&
      length(gregexpr("class='figure-card'", standalone_html, fixed = TRUE)[[1L]]) == 8L &&
      length(gregexpr("class='table-card'", standalone_html, fixed = TRUE)[[1L]]) == 8L &&
      length(gregexpr("Copy table for Word", standalone_html, fixed = TRUE)[[1L]]) == 8L &&
      length(gregexpr("Copy LaTeX", standalone_html, fixed = TRUE)[[1L]]) == 8L &&
      length(gregexpr("Copy caption", standalone_html, fixed = TRUE)[[1L]]) == 8L &&
      length(gregexpr("Copy figure for LaTeX", standalone_html, fixed = TRUE)[[1L]]) == 8L &&
      grepl("data-tab='figures'", standalone_html, fixed = TRUE) &&
      grepl("data-tab='tables'", standalone_html, fixed = TRUE) &&
      grepl("estimation uncertainty", standalone_html, fixed = TRUE) &&
      grepl("Hessian parameter uncertainty is excluded", standalone_html, fixed = TRUE),
    paste0("Standalone HTML structure/uncertainty labeling failed for ", key, ".")
  )
  writeLines(standalone_html, standalone_report_paths[[key]], useBytes = TRUE)
}

method_note <- paste0(
  "The combined result exactly preserves the existing equal-model mixture: each retained model has weight 1/80, ",
  "so RR=0 contributes 34/80 (42.5%) and RR=1 contributes 46/80 (57.5%). ",
  "The RR=0 and RR=1 results renormalize equal model weight within 34 and 46 models, respectively. ",
  "For estimation-inclusive results, each PDH model contributes 100 joint Hessian draws and each Near-PDH model contributes ",
  "its central estimate repeated 100 times. Projections use ten equal-weight recruitment paths per model and no Hessian draws."
)
caveat_note <- paste0(
  "The six ensemble axes were randomly paired and the reporting treatments experienced different completion and MGC retention. ",
  "Therefore RR=0 versus RR=1 differences are retained-subset sensitivity summaries. They are not an isolated causal effect, ",
  "a matched-pair contrast, or a controlled one-factor experiment. RR=0 denotes requested inclusion of pre-mixing reporting rates. ",
  "For mixing=0 rows, stored flag2=1 is inactive because no pre-mixing window exists; no tag event or recapture is removed."
)
v25_caveat_note <- paste0(
  "A subsequent runtime audit found that the retained fits used pre-fix MFCL v2.5 (f5bc1e23...), ",
  "which applied RR exclusion inconsistently between tag dynamics and likelihood. The 46 RR-exclusion fits ",
  "and all derived split/combined contrasts are provisional until refitted with the corrected v2.6 implementation ",
  "(a5a83cd); they must not be interpreted as a clean intended RR-exclusion sensitivity."
)

grouped_table09_display <- grouped_wp06_tables$table09
grouped_table09_display$Value <- sprintf("%.3f", grouped_table09_display$Value)
grouped_table10_display <- grouped_wp06_tables$table10
grouped_table10_display$Probability <- scales::percent(
  grouped_table10_display$Probability, accuracy = 0.1
)
grouped_table11_display <- grouped_wp06_tables$table11
for (column in c("Minimum", "10%", "Median", "Mean", "90%", "Maximum")) {
  grouped_table11_display[[column]] <- vapply(
    seq_len(nrow(grouped_table11_display)), function(index) {
      digits <- if (grouped_table11_display$Unit[[index]] == "thousand MT") 1L else 3L
      sprintf(
        paste0("%.", digits, "f"),
        grouped_wp06_tables$table11[[column]][[index]]
      )
    }, character(1)
  )
}
grouped_table12_display <- data.frame(
  Group = grouped_wp06_tables$table12$Group,
  Quantity = grouped_wp06_tables$table12$Quantity,
  `All models: median (80% interval)` = sprintf(
    "%.3f (%.3f–%.3f)", grouped_wp06_tables$table12$All_median,
    grouped_wp06_tables$table12$All_q10,
    grouped_wp06_tables$table12$All_q90
  ),
  `PDH only: median (80% interval)` = sprintf(
    "%.3f (%.3f–%.3f)", grouped_wp06_tables$table12$PDH_median,
    grouped_wp06_tables$table12$PDH_q10,
    grouped_wp06_tables$table12$PDH_q90
  ),
  `Max |50-draw − 100-draw quantile|` = sprintf(
    "%.4f", grouped_wp06_tables$table12$Max_50_100_difference
  ),
  check.names = FALSE
)
grouped_wp06_links <- paste0(
  "<div class='rr-actions'>",
  paste(vapply(names(grouped_wp06_filenames), function(product_name) {
    filename <- grouped_wp06_filenames[[product_name]]
    paste0(
      "<a href='rr-sensitivity/tables/", filename,
      "' download>", html_escape(filename), "</a>"
    )
  }, character(1)), collapse = ""),
  "</div>"
)

# Standalone three-scope comparison report --------------------------------
#
# This page is deliberately a presentation layer over the already validated
# cross-scope figures and grouped Tables 8--13 above. It must not create a
# second calculation path for any public management quantity.
comparison_table_card <- function(id, title, caption, display, filename) {
  word_id <- paste0("comparison-word-", id)
  latex_id <- paste0("comparison-latex-", id)
  paste0(
    "<article class='table-card' id='comparison-", html_escape(id), "'>",
    "<h2>", html_escape(title), "</h2>",
    "<p class='caption'><strong>Table.</strong> ", html_escape(caption), "</p>",
    "<div class='actions'><button onclick=\"copyText('", word_id,
    "',this)\">Copy table for Word</button>",
    "<button onclick=\"copyText('", latex_id,
    "',this)\">Copy LaTeX</button>",
    "<a href='rr-sensitivity/tables/", filename,
    "' download>Download CSV</a></div>",
    html_table(display),
    "<textarea id='", word_id, "' class='copy-source'>",
    html_escape(scope_word_table(caption, display)), "</textarea>",
    "<textarea id='", latex_id, "' class='copy-source'>",
    html_escape(scope_latex_table(caption, display)), "</textarea></article>"
  )
}

comparison_figure_card <- function(
    id, title, caption, files,
    relative_dir = file.path("rr-sensitivity", "figures")) {
  caption_id <- paste0("comparison-caption-", id)
  latex_id <- paste0("comparison-figure-latex-", id)
  relative_pdf <- file.path(relative_dir, basename(files[["pdf"]]))
  relative_png <- file.path(relative_dir, basename(files[["png"]]))
  latex_figure <- paste0(
    "\\begin{figure}[htbp]\n\\centering\n",
    "\\includegraphics[width=\\textwidth]{", relative_pdf, "}\n",
    "\\caption{", scope_latex_escape(caption), "}\n\\end{figure}"
  )
  paste0(
    "<article class='figure-card' id='comparison-", html_escape(id), "'>",
    "<h2>", html_escape(title), "</h2>",
    "<img src='", image_uri(files[["png"]]), "' alt='", html_escape(title), "'>",
    "<p class='caption'><strong>Figure.</strong> ", html_escape(caption), "</p>",
    "<div class='actions'><button onclick=\"copyText('", caption_id,
    "',this)\">Copy caption</button>",
    "<a href='", relative_pdf, "'>Open vector PDF</a>",
    "<a href='", relative_png, "' download>Save PNG</a>",
    "<button onclick=\"copyText('", latex_id,
    "',this)\">Copy figure for LaTeX</button></div>",
    "<textarea id='", caption_id, "' class='copy-source'>",
    html_escape(caption), "</textarea>",
    "<textarea id='", latex_id, "' class='copy-source'>",
    html_escape(latex_figure), "</textarea></article>"
  )
}

comparison_table_cards <- list(
  table08 = comparison_table_card(
    "table-08", "Table 8 · Management quantities with estimation uncertainty",
    paste0(
      "Combined, RR=0 and RR=1 management quantities from equal-model-weight mixtures. ",
      "PDH fits contribute 100 joint Hessian draws per model and Near-PDH fits contribute ",
      "their repeated central estimates with equal total model weight. Values are medians ",
      "and central 50%, 80% and 95% equal-tailed intervals."
    ),
    grouped_wp06_tables$table08_summary,
    grouped_wp06_filenames[["table08_summary"]]
  ),
  table09 = comparison_table_card(
    "table-09", "Table 9 · CMM depletion comparison",
    paste0(
      "Recent spawning depletion relative to the 2012–2015 average for Combined, RR=0 ",
      "and RR=1. Period values are arithmetic means across equal-weight central models; ",
      "the final row within each group is their ratio. Estimation uncertainty is excluded."
    ),
    grouped_table09_display, grouped_wp06_filenames[["table09"]]
  ),
  table10 = comparison_table_card(
    "table-10", "Table 10 · Status probabilities",
    paste0(
      "Stock-status probabilities for Combined, RR=0 and RR=1 from the same ",
      "equal-model-weight structural plus available Hessian mixture as Table 8."
    ),
    grouped_table10_display, grouped_wp06_filenames[["table10"]]
  ),
  table11 = comparison_table_card(
    "table-11", "Table 11 · Full structural reference-point quantities",
    paste0(
      "All reference-point rows for Combined, RR=0 and RR=1. Ratios and reference points ",
      "are calculated within each central model before summarising; intervals describe ",
      "structural uncertainty only and exclude Hessian estimation uncertainty."
    ),
    grouped_table11_display, grouped_wp06_filenames[["table11"]]
  ),
  table12 = comparison_table_card(
    "table-12", "Table 12 · Estimation-uncertainty audit",
    paste0(
      "All-model and PDH-only results for the three core stock-status quantities in each ",
      "scope, plus the largest q10, median or q90 change when using 50 rather than 100 ",
      "draws per model."
    ),
    grouped_table12_display, grouped_wp06_filenames[["table12"]]
  ),
  table13 = comparison_table_card(
    "table-13", "Table 13 · Projection summary",
    paste0(
      "Selected-year projection quantities for Combined, RR=0 and RR=1. Intervals include ",
      "structural differences and ten stochastic recruitment paths per model; Hessian ",
      "parameter uncertainty is excluded."
    ),
    grouped_wp06_tables$table13, grouped_wp06_filenames[["table13"]]
  )
)

combined_current_status_files <- c(
  png = file.path(output_dir, "figures", "combined-kobe-majuro-status.png"),
  pdf = file.path(output_dir, "figures", "combined-kobe-majuro-status.pdf")
)
assert_true(
  all(file.exists(combined_current_status_files)) &&
    all(file.info(combined_current_status_files)$size > 10000L),
  "The canonical Combined Kobe/Majuro figure set is unavailable."
)

comparison_figure_cards <- list(
  history = comparison_figure_card(
    "history", "Historical trajectories",
    paste0(
      "Combined 80-model, RR=0 inclusion and RR=1 exclusion trajectories. Lines are medians ",
      "and bands are central 80% intervals. Annual depletion, spawning potential and recruitment ",
      "include available Hessian estimation uncertainty; fishing mortality is structural-only."
    ), history_files
  ),
  management = comparison_figure_card(
    "management", "Current management distributions",
    paste0(
      "Equal-model-weight distributions of the three core management quantities for Combined, ",
      "RR=0 and RR=1. PDH joint Hessian draws and Near-PDH central point masses have equal total ",
      "model weight, so available estimation uncertainty is included."
    ), management_files
  ),
  status_combined = comparison_figure_card(
    "status-combined", "Combined 80 · current Kobe and Majuro status",
    paste0(
      "Canonical Combined current-status figure. Points are the 80 equal-weight central models; ",
      "nested 50%, 80% and 95% HDRs and region probabilities use the 62-PDH plus 18-Near-PDH ",
      "equal-model-weight mixture with available Hessian estimation uncertainty."
    ), combined_current_status_files, relative_dir = "figures"
  ),
  status_inclusion = comparison_figure_card(
    "status-inclusion", "RR=0 inclusion 34 · current Kobe and Majuro status",
    paste0(
      "Canonical RR=0 current-status figure. Points are the 34 equal-weight central models; ",
      "nested 50%, 80% and 95% HDRs and region probabilities use 25 PDH draw sets plus nine ",
      "Near-PDH central point masses with equal total model weight."
    ), scope_figure_files$inclusion$current_status
  ),
  status_exclusion = comparison_figure_card(
    "status-exclusion", "RR=1 exclusion 46 · current Kobe and Majuro status",
    paste0(
      "Canonical RR=1 current-status figure. Points are the 46 equal-weight central models; ",
      "nested 50%, 80% and 95% HDRs and region probabilities use 37 PDH draw sets plus nine ",
      "Near-PDH central point masses with equal total model weight. The interpretation remains ",
      "subject to the MFCL v2.5 provisional caveat."
    ), scope_figure_files$exclusion$current_status
  ),
  projection = comparison_figure_card(
    "projection", "Stochastic projections",
    paste0(
      "Projected depletion, spawning potential, catch relative to MSY and frequency below the ",
      "LRP for Combined, RR=0 and RR=1. Lines are medians and bands are central 80% intervals ",
      "across ten recruitment paths per model; Hessian parameter uncertainty is excluded."
    ), projection_files
  ),
  combined = comparison_figure_card(
    "key-combined", "Combined 80-model key quantities",
    paste0(
      "Key historical, current and projected quantities for the canonical Combined result. ",
      "Its weights remain 34/80 RR=0 and 46/80 RR=1; it is not reweighted to a 50:50 mixture."
    ), group_key_files$combined
  ),
  inclusion = comparison_figure_card(
    "key-inclusion", "RR=0 inclusion · 34-model key quantities",
    paste0(
      "Key quantities after renormalising equal model weight within the 34 retained RR=0 ",
      "inclusion models. This is a retained-subset summary rather than a matched causal contrast."
    ), group_key_files$inclusion
  ),
  exclusion = comparison_figure_card(
    "key-exclusion", "RR=1 exclusion · 46-model key quantities",
    paste0(
      "Key quantities after renormalising equal model weight within the 46 retained RR=1 ",
      "exclusion models. This provisional result is subject to the MFCL v2.5 caveat in Overview."
    ), group_key_files$exclusion
  )
)

comparison_figures_html <- paste0(
  comparison_figure_cards$history,
  comparison_figure_cards$management,
  "<section class='status-block'><h2>Canonical current status · three scopes</h2>",
  "<p>The three panels use the canonical status layout and exact equal-model weights and category probabilities for each scope.</p>",
  "<div class='status-grid'>",
  comparison_figure_cards$status_combined,
  comparison_figure_cards$status_inclusion,
  comparison_figure_cards$status_exclusion,
  "</div></section>",
  comparison_figure_cards$projection,
  comparison_figure_cards$combined,
  comparison_figure_cards$inclusion,
  comparison_figure_cards$exclusion
)

comparison_download_links <- paste(vapply(
  names(grouped_wp06_filenames), function(product_name) {
    filename <- grouped_wp06_filenames[[product_name]]
    paste0(
      "<a href='rr-sensitivity/tables/", filename, "' download>",
      html_escape(filename), "</a>"
    )
  }, character(1)), collapse = ""
)

comparison_html <- paste0(
  "<!doctype html><html lang='en'><head><meta charset='utf-8'>",
  "<meta name='viewport' content='width=device-width,initial-scale=1'>",
  "<title>BET 2026 · RR comparison</title><style>",
  ":root{--navy:#082f49;--teal:#087f8f;--orange:#d97706;--red:#b83232;--ink:#203846;--muted:#58707d;--line:#c8dce2;--paper:#f4f8fa}",
  "*{box-sizing:border-box}body{margin:0;background:var(--paper);color:var(--ink);font:16px/1.5 Georgia,'Times New Roman',serif}",
  "header{background:linear-gradient(120deg,#062d4a,#087f8f);color:white;padding:34px clamp(20px,5vw,72px) 28px}header h1{font-size:clamp(2rem,4vw,3.5rem);line-height:1.05;margin:.25rem 0}.kicker{font:700 .86rem Arial,sans-serif;letter-spacing:.11em;text-transform:uppercase;color:#bcecf2}header p{max-width:1050px;font-size:1.08rem;margin:.6rem 0}",
  ".shell{width:min(1500px,96vw);margin:0 auto;padding:24px 0 60px}.tabs{position:sticky;top:0;z-index:10;display:flex;gap:8px;flex-wrap:wrap;padding:10px;background:rgba(244,248,250,.96);border-bottom:1px solid var(--line)}",
  ".tabs button,.actions a,.actions button{border:0;border-radius:5px;background:var(--teal);color:white;padding:9px 13px;text-decoration:none;font:700 .86rem Arial,sans-serif;cursor:pointer}.tabs button.active{background:var(--navy)}",
  ".panel{display:none}.panel.active{display:block}.hero-grid{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:14px;margin:16px 0}.stat{background:white;border:1px solid var(--line);border-top:4px solid var(--teal);border-radius:8px;padding:16px;box-shadow:0 4px 14px #173b4d14}.stat strong{display:block;font:800 1.55rem Arial,sans-serif;color:var(--navy)}",
  ".boundary{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:10px;background:#e8f5f6;border-left:6px solid var(--teal);padding:15px;margin:16px 0}.boundary div{padding:7px}.boundary strong{display:block;color:#07566b;font-family:Arial,sans-serif}",
  ".context,.warning{padding:14px 18px;margin:16px 0}.context{background:#e8f5f6;border-left:6px solid var(--teal)}.warning{background:#fff2e6;border-left:6px solid var(--orange)}.warning strong{color:#8d4300}.context a{color:#07566b;font-weight:bold}",
  ".figure-card,.table-card,.downloads{background:white;border:1px solid var(--line);border-radius:9px;padding:clamp(14px,2vw,24px);margin:18px 0;box-shadow:0 5px 18px #173b4d12}.figure-card img{width:100%;height:auto;display:block;margin:10px auto}.figure-card h2,.table-card h2,.downloads h2{color:#07566b;margin:.1rem 0 .5rem;font-size:clamp(1.35rem,2.2vw,1.9rem)}",
  ".status-block{border-top:4px solid var(--teal);margin:28px 0 20px;padding-top:12px}.status-block>h2{color:var(--navy);font:800 clamp(1.5rem,2.5vw,2.1rem) Arial,sans-serif}.status-grid{display:grid;grid-template-columns:1fr;gap:14px}.status-grid .figure-card{margin:8px 0}",
  ".caption{color:#405e6d}.actions{display:flex;gap:8px;flex-wrap:wrap;margin:12px 0}.actions button.copied{background:#2a9d8f}.copy-source{position:absolute;left:-10000px;width:1px;height:1px}",
  ".rr-table-wrap{overflow:auto;max-height:650px;border:1px solid #d7e3e8;margin:12px 0}.rr-table-wrap table{width:100%;min-width:940px;border-collapse:collapse}.rr-table-wrap th{position:sticky;top:0;background:#0a5266;color:white;font-family:Arial,sans-serif}.rr-table-wrap th,.rr-table-wrap td{padding:9px 11px;border-bottom:1px solid #d7e3e8;text-align:left;white-space:nowrap}.rr-table-wrap tr:nth-child(even){background:#f1f7f8}",
  "#comparison-table-08 th:nth-child(n+4),#comparison-table-08 td:nth-child(n+4),#comparison-table-09 th:nth-child(n+4),#comparison-table-09 td:nth-child(n+4),#comparison-table-10 th:nth-child(3),#comparison-table-10 td:nth-child(3),#comparison-table-11 th:nth-child(n+5),#comparison-table-11 td:nth-child(n+5),#comparison-table-12 th:nth-child(n+3),#comparison-table-12 td:nth-child(n+3),#comparison-table-13 th:nth-child(n+2),#comparison-table-13 td:nth-child(n+2){text-align:right;font-variant-numeric:tabular-nums}",
  "footer{color:var(--muted);font-size:.92rem;border-top:1px solid var(--line);padding-top:18px;margin-top:28px}",
  "@media(max-width:900px){.hero-grid,.boundary{grid-template-columns:1fr 1fr}}@media(max-width:560px){.hero-grid,.boundary{grid-template-columns:1fr}.shell{width:98vw}.tabs{position:static}.rr-table-wrap table{min-width:760px}}",
  "@media print{.tabs,.actions{display:none!important}.panel{display:block!important}.shell{width:100%;padding:0}.figure-card,.table-card{break-inside:avoid;box-shadow:none}}",
  "</style></head><body><header>",
  "<div class='kicker'>BET 2026 · reporting-rate retained-subset sensitivity</div>",
  "<h1>Combined 80 vs RR=0 inclusion 34 vs RR=1 exclusion 46</h1>",
  "<p>One direct comparison of the validated figures and WP-06 Tables 8–13. No model fitting, Hessian calculation, projection or management-quantity calculation is repeated by this page.</p></header>",
  "<main class='shell'><nav class='tabs' aria-label='Comparison sections'>",
  "<button class='active' data-tab='overview'>Overview</button>",
  "<button data-tab='figures'>Figures</button>",
  "<button data-tab='tables'>Tables</button>",
  "<button onclick='window.print()'>Print / PDF</button></nav>",
  "<section class='panel active' data-panel='overview'>",
  "<div class='hero-grid'><div class='stat'><strong>80</strong>Combined models</div>",
  "<div class='stat'><strong>34</strong>RR=0 inclusion</div>",
  "<div class='stat'><strong>46</strong>RR=1 exclusion</div>",
  "<div class='stat'><strong>62 + 18</strong>PDH + Near-PDH</div></div>",
  "<div class='context'><strong>Weighting.</strong> ", html_escape(method_note), "</div>",
  "<div class='boundary'><div><strong>Tables 8 and 10</strong>Structure + available Hessian estimation uncertainty</div>",
  "<div><strong>Tables 9 and 11</strong>Central structural models only; no Hessian draws</div>",
  "<div><strong>Table 12</strong>All-model/PDH-only and 50/100-draw audit</div>",
  "<div><strong>Table 13</strong>Structure + stochastic recruitment; no Hessian draws</div></div>",
  "<div class='warning'><strong>Interpretation boundary.</strong> ", html_escape(caveat_note), "</div>",
  "<div class='warning'><strong>MFCL v2.5 provisional caveat.</strong> ", html_escape(v25_caveat_note), "</div>",
  "<div class='context'><strong>Related outputs.</strong> <a href='bet-2026-ensemble-report.html'>Combined 80-model report</a> · ",
  "<a href='bet-2026-ensemble-report-rr0-inclusion.html'>RR=0 report</a> · ",
  "<a href='bet-2026-ensemble-report-rr1-exclusion.html'>RR=1 report</a> · ",
  "<a href='bet-2026-ensemble-interactive-viewer.html'>Three-scope viewer</a></div>",
  "</section><section class='panel' data-panel='figures'>",
  comparison_figures_html,
  "</section><section class='panel' data-panel='tables'>",
  paste(unname(unlist(comparison_table_cards)), collapse = ""),
  "<div class='downloads'><h2>All grouped WP-06 table files</h2><div class='actions'>",
  comparison_download_links,
  "</div><p>The formatted and numeric Table 8 files are both retained. Every grouped file binds the already validated Combined, RR=0 and RR=1 products without recalculation.</p></div>",
  "</section><footer>Generated from the checksum-locked retained ensemble, Hessian cache and projection cache. Combined rows reproduce the canonical 80-model report.</footer></main>",
  "<script>function copyText(id,button){const source=document.getElementById(id);const old=button.textContent;const done=()=>{button.textContent='Copied';button.classList.add('copied');setTimeout(()=>{button.textContent=old;button.classList.remove('copied')},1400)};const fallback=()=>{source.focus();source.select();document.execCommand('copy');done()};if(navigator.clipboard&&window.isSecureContext){navigator.clipboard.writeText(source.value).then(done).catch(fallback)}else{fallback()}}(()=>{const buttons=[...document.querySelectorAll('[data-tab]')],panels=[...document.querySelectorAll('[data-panel]')];buttons.forEach(b=>b.addEventListener('click',()=>{buttons.forEach(x=>x.classList.toggle('active',x===b));panels.forEach(p=>p.classList.toggle('active',p.dataset.panel===b.dataset.tab));history.replaceState(null,'','#'+b.dataset.tab)}));const target=location.hash.slice(1);const button=buttons.find(b=>b.dataset.tab===target);if(button)button.click()})();</script>",
  "</body></html>"
)

comparison_report_path <- file.path(
  output_dir, "bet-2026-ensemble-report-rr-comparison.html"
)
assert_true(
  !grepl("https?://", comparison_html) &&
    length(gregexpr("class='figure-card'", comparison_html, fixed = TRUE)[[1L]]) == 9L &&
    length(gregexpr("class='table-card'", comparison_html, fixed = TRUE)[[1L]]) == 6L &&
    length(gregexpr("data:image/png;base64,", comparison_html, fixed = TRUE)[[1L]]) == 9L &&
    grepl("MFCL v2.5 provisional caveat", comparison_html, fixed = TRUE) &&
    grepl("Table 11 · Full structural reference-point quantities", comparison_html, fixed = TRUE),
  "Standalone RR comparison HTML structure or caveat is incomplete."
)
writeLines(comparison_html, comparison_report_path, useBytes = TRUE)

rr_html <- paste0(
  "<!-- RR_SENSITIVITY_START -->",
  "<section id='rr-sensitivity' class='rr-sensitivity'>",
  "<style>",
  ".rr-sensitivity{border-top:5px solid #d97706;margin:46px 0 30px;padding-top:18px;scroll-margin-top:72px}",
  ".rr-sensitivity h2{border-top:0;padding-top:0}.rr-note{background:#fff7e8;border-left:5px solid #d97706;padding:14px 16px;margin:14px 0;font-size:1rem}",
  ".rr-method{background:#edf7f8;border-left:5px solid #0f8b8d;padding:14px 16px;margin:14px 0;font-size:1rem}",
  ".rr-grid{display:grid;grid-template-columns:1fr 1fr;gap:18px}.rr-figure{border:1px solid #c8dce2;border-radius:7px;padding:14px;background:#fbfdfe}",
  ".rr-figure.rr-wide{grid-column:1/-1}.rr-figure img{display:block;width:100%;height:auto;margin:8px auto}.rr-figure h3{color:#0a5266;margin:.2rem 0 .6rem}",
  ".rr-actions{display:flex;gap:8px;flex-wrap:wrap}.rr-actions a{background:#087f8f;color:#fff;border-radius:4px;padding:7px 10px;text-decoration:none;font:600 .82rem Arial,sans-serif}",
  ".rr-table-wrap{overflow:auto;max-height:520px;border:1px solid #d7e3e8;margin:10px 0 18px}.rr-table-wrap table{margin:0;min-width:920px}",
  "@media(max-width:850px){.rr-grid{grid-template-columns:1fr}.rr-figure.rr-wide{grid-column:auto}}",
  "</style>",
  "<h2>Reporting-rate retained-subset sensitivity</h2>",
  "<p>This section recalculates the report's central management, available Hessian uncertainty, stochastic projections and key figures for RR=0 only, RR=1 only, and both treatments combined.</p>",
  "<div class='rr-actions'><a href='bet-2026-ensemble-report-rr-comparison.html'>Open three-scope comparison report</a>",
  "<a href='bet-2026-ensemble-report-rr0-inclusion.html'>Open RR=0 inclusion analysis</a>",
  "<a href='bet-2026-ensemble-report-rr1-exclusion.html'>Open RR=1 exclusion analysis</a></div>",
  "<div class='rr-method'><strong>Weights.</strong> ", method_note, "</div>",
  "<div class='rr-note'><strong>Interpretation boundary.</strong> ", caveat_note, "</div>",
  "<h3>WP-06 Tables 8–13 · combined, RR=0 and RR=1</h3>",
  "<p>Each table below uses the original WP-06 definition and uncertainty boundary. Combined rows reproduce the canonical 80-model report exactly.</p>",
  "<h4>Table 8 · Management quantities with available Hessian uncertainty</h4>",
  html_table(grouped_wp06_tables$table08_summary),
  "<h4>Table 9 · Dynamic depletion relative to 2012–2015</h4>",
  html_table(grouped_table09_display),
  "<h4>Table 10 · Hessian-inclusive status probabilities</h4>",
  html_table(grouped_table10_display),
  "<h4>Table 11 · Structural reference-point quantities</h4>",
  html_table(grouped_table11_display),
  "<h4>Table 12 · Estimation-uncertainty and draw-count audit</h4>",
  html_table(grouped_table12_display),
  "<h4>Table 13 · Projection summary</h4>",
  html_table(grouped_wp06_tables$table13),
  grouped_wp06_links,
  "<h3>Counts and quality control</h3>", html_table(qc_display),
  "<p><strong>Count source.</strong> Planned, incomplete and MGC-excluded counts use the authoritative 100-model audit; the 88-row public payload omits two completed-PAR MGC failures.</p>",
  "<h3>Central management quantities (structural ensemble)</h3>",
  html_table(structural_display),
  "<h4>Structural status frequencies</h4>", html_table(structural_risk_display),
  "<h3>Management quantities with available estimation uncertainty</h3>",
  "<p>These intervals and risk probabilities use the equal-model-weight PDH/Near-PDH mixture described above.</p>",
  html_table(hessian_display),
  "<h4>Hessian-inclusive status probabilities</h4>",
  html_table(hessian_risk_display),
  "<h3>Projection terminal summaries</h3>",
  html_table(projection_terminal_display),
  "<p>The detailed machine-readable summaries and checks are available below.</p>",
  table_links,
  "<h3>Differences from the combined result</h3>",
  "<p>Differences are subset value minus the existing 80-model combined value; probabilities remain on a 0–1 scale.</p>",
  html_table(difference_display),
  "<div class='rr-grid'>",
  figure_card(
    "Historical comparison", method_note,
    history_files, wide = TRUE
  ),
  figure_card(
    "Current management distributions (Hessian-inclusive)", caveat_note,
    management_files, wide = TRUE
  ),
  figure_card(
    "Current Kobe and Majuro status", paste0(method_note, " ", caveat_note),
    status_files, wide = TRUE
  ),
  figure_card(
    "Projection comparison", paste0(
      "Each model contributes ten stochastic recruitment paths. Bands are central 80% intervals and exclude Hessian uncertainty. ",
      caveat_note
    ),
    projection_files, wide = TRUE
  ),
  paste(vapply(group_keys, function(key) {
    figure_card(
      paste0(group_labels[[key]], " key quantities"),
      paste0(
        "Historical and current quantities retain the report's scientific periods and definitions; projections use 2025–2054 paths. ",
        method_note, " ", caveat_note
      ),
      group_key_files[[key]]
    )
  }, character(1)), collapse = ""),
  "</div></section>",
  "<!-- RR_SENSITIVITY_END -->"
)

report_path <- file.path(output_dir, "bet-2026-ensemble-report.html")
assert_true(
  file.exists(report_path),
  paste0("Render the main report before inserting RR sensitivity: ", report_path)
)
html <- paste(readLines(report_path, warn = FALSE), collapse = "\n")
start_marker <- "<!-- RR_SENSITIVITY_START -->"
end_marker <- "<!-- RR_SENSITIVITY_END -->"
start_position <- regexpr(start_marker, html, fixed = TRUE)[[1L]]
end_position <- regexpr(end_marker, html, fixed = TRUE)[[1L]]
if (start_position > 0L || end_position > 0L) {
  assert_true(
    start_position > 0L && end_position > start_position,
    "The existing RR-sensitivity HTML markers are malformed."
  )
  suffix_start <- end_position + nchar(end_marker)
  html <- paste0(
    substr(html, 1L, start_position - 1L),
    rr_html,
    substr(html, suffix_start, nchar(html))
  )
} else {
  reference_marker <- "<section class='refs'><h2>References</h2>"
  insertion_position <- regexpr(reference_marker, html, fixed = TRUE)[[1L]]
  assert_true(
    insertion_position > 0L,
    "Could not locate the report reference section for RR-sensitivity insertion."
  )
  html <- paste0(
    substr(html, 1L, insertion_position - 1L),
    rr_html,
    substr(html, insertion_position, nchar(html))
  )
}
assert_true(
  length(gregexpr(start_marker, html, fixed = TRUE)[[1L]]) == 1L &&
    length(gregexpr(end_marker, html, fixed = TRUE)[[1L]]) == 1L,
  "RR-sensitivity insertion is not idempotent."
)
writeLines(html, report_path, useBytes = TRUE)

cat(sprintf(
  paste0(
    "Rendered RR sensitivity: combined %d (%d PDH + %d Near-PDH), ",
    "RR=0 %d (%d + %d), RR=1 %d (%d + %d); ",
    "wrote %d tables and %d PNG/PDF figure sets.\n"
  ),
  length(group_ids$combined), length(intersect(group_ids$combined, pdh_ids)),
  length(intersect(group_ids$combined, near_pdh_ids)),
  length(group_ids$inclusion), length(intersect(group_ids$inclusion, pdh_ids)),
  length(intersect(group_ids$inclusion, near_pdh_ids)),
  length(group_ids$exclusion), length(intersect(group_ids$exclusion, pdh_ids)),
  length(intersect(group_ids$exclusion, near_pdh_ids)),
  length(list.files(table_dir, pattern = "[.]csv$")),
  length(list.files(figure_dir, pattern = "[.]png$"))
))
