#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Install jsonlite to build the interactive viewer.", call. = FALSE)
}
source("report/model-labels.R")

series <- readRDS("data/ensemble/ensemble-timeseries.rds")
fit <- read.csv("data/ensemble/fit-diagnostics.csv", check.names = FALSE)
design <- read.csv("data/ensemble/successful-model-design.csv", check.names = FALSE)
management <- read.csv("data/ensemble/management-quantities.csv", check.names = FALSE)

required_reporting_columns <- c(
  "tag_reporting_flag2", "tag_reporting", "zero_mixing_events",
  "tag_reporting_zero_mixing_exclusions"
)
missing_reporting_columns <- setdiff(required_reporting_columns, names(design))
if (length(missing_reporting_columns)) {
  stop(
    "The viewer source is missing reporting-rate fields: ",
    paste(missing_reporting_columns, collapse = ", "), call. = FALSE
  )
}

completed_ids <- sort(unique(series$ensemble_id))
if (
  length(completed_ids) != 88L ||
  !identical(completed_ids, sort(fit$ensemble_id)) ||
  !identical(completed_ids, sort(design$ensemble_id)) ||
  !identical(completed_ids, sort(management$ensemble_id))
) {
  stop("The viewer requires the validated 88-model source payload.", call. = FALSE)
}

ids <- sort(fit$ensemble_id[fit$maximum_gradient <= 1e-4])
if (length(ids) != 80L) {
  stop("The viewer requires exactly 80 models passing MGC <= 1e-4.", call. = FALSE)
}
model_map <- sequential_model_map(ids)
display_ids <- model_map$ensemble_id

design <- design[match(ids, design$ensemble_id), ]
fit <- fit[match(ids, fit$ensemble_id), ]
management <- management[match(ids, management$ensemble_id), ]
series <- series[series$ensemble_id %in% ids, ]
series <- series[order(series$ensemble_id, series$year), ]

reporting_flag <- as.integer(design$tag_reporting_flag2)
if (
  anyNA(reporting_flag) ||
  !setequal(reporting_flag, c(0L, 1L)) ||
  any((reporting_flag == 0L) != (design$tag_reporting == "inclusion")) ||
  any((reporting_flag == 1L) != (design$tag_reporting == "exclusion")) ||
  any(
    reporting_flag == 0L &
      design$tag_reporting_zero_mixing_exclusions != design$zero_mixing_events
  )
) {
  stop(
    "The reporting-rate mapping must be flag2=0 inclusion and flag2=1 exclusion, with the mixing=0 compatibility sentinel preserved.",
    call. = FALSE
  )
}
rr0_ids <- ids[reporting_flag == 0L]
rr1_ids <- ids[reporting_flag == 1L]
if (
  length(ids) != 80L || length(rr0_ids) != 34L || length(rr1_ids) != 46L ||
  length(ids) != length(rr0_ids) + length(rr1_ids) ||
  length(intersect(rr0_ids, rr1_ids)) != 0L
) {
  stop(
    "The viewer requires 80 retained models split exactly into 34 flag2=0 inclusion and 46 flag2=1 exclusion models.",
    call. = FALSE
  )
}

scope_definitions <- list(
  list(
    id = "all", label = "All retained", reporting_flag2 = NA_integer_,
    ids = ids,
    detail = "34 inclusion + 46 exclusion; model-equal combined mixture"
  ),
  list(
    id = "rr0", label = "RR inclusion", reporting_flag2 = 0L,
    ids = rr0_ids,
    detail = "flag2 = 0; 34 retained models"
  ),
  list(
    id = "rr1", label = "RR exclusion", reporting_flag2 = 1L,
    ids = rr1_ids,
    detail = "flag2 = 1; 46 retained models"
  )
)

management_metrics <- list(
  list(
    id = "depletion", column = "sb_recent_sb0",
    label = "SBrecent / SBF=0", period = "2021–2024 / 2014–2023",
    risk_column = "below_lrp_020", risk_label = "Below LRP (0.20)"
  ),
  list(
    id = "sbmsy", column = "sb_recent_sbmsy",
    label = "SBrecent / SBMSY", period = "2021–2024",
    risk_column = "below_sbmsy", risk_label = "Below SBMSY"
  ),
  list(
    id = "fmsy", column = "f_recent_fmsy",
    label = "Frecent / FMSY", period = "2020–2023",
    risk_column = "above_fmsy", risk_label = "Above FMSY"
  )
)
required_management_columns <- unique(unlist(lapply(
  management_metrics, function(metric) c(metric$column, metric$risk_column)
)))
missing_management_columns <- setdiff(required_management_columns, names(management))
if (length(missing_management_columns)) {
  stop(
    "The viewer source is missing management-summary fields: ",
    paste(missing_management_columns, collapse = ", "), call. = FALSE
  )
}

summarise_management_scope <- function(scope) {
  values <- management[management$ensemble_id %in% scope$ids, , drop = FALSE]
  if (nrow(values) != length(scope$ids)) {
    stop("A reporting-rate scope is missing management rows.", call. = FALSE)
  }
  list(
    id = scope$id,
    model_count = nrow(values),
    metrics = lapply(management_metrics, function(metric) {
      x <- values[[metric$column]]
      interval <- stats::quantile(
        x, probs = c(0.10, 0.50, 0.90), names = FALSE, na.rm = FALSE
      )
      list(
        id = metric$id,
        label = metric$label,
        period = metric$period,
        q10 = interval[[1L]],
        median = interval[[2L]],
        q90 = interval[[3L]],
        risk_label = metric$risk_label,
        risk_proportion = mean(values[[metric$risk_column]])
      )
    })
  )
}

scope_summaries <- lapply(scope_definitions, summarise_management_scope)
full_medians <- vapply(
  scope_summaries[[1L]]$metrics, function(metric) metric$median, numeric(1L)
)
for (scope_index in seq_along(scope_summaries)) {
  for (metric_index in seq_along(scope_summaries[[scope_index]]$metrics)) {
    scope_summaries[[scope_index]]$metrics[[metric_index]]$median_difference_from_all <-
      scope_summaries[[scope_index]]$metrics[[metric_index]]$median -
        full_medians[[metric_index]]
  }
}

# Golden-angle hues keep adjacent model identifiers visually distinct. Alternating
# luminance and chroma provide additional separation across the 80 fixed colours.
colour_index <- seq_along(ids) - 1L
model_colours <- grDevices::hcl(
  h = (colour_index * 137.508) %% 360,
  c = c(72, 58, 66, 52)[colour_index %% 4L + 1L],
  l = c(48, 64, 40, 71)[colour_index %% 4L + 1L],
  fixup = TRUE
)

short_reporting <- ifelse(design$tag_reporting == "inclusion", "include", "exclude")
model_meta <- data.frame(
  id = display_ids,
  source_id = ids,
  label = sprintf(
    "%s | h=%.3f | tau=%.1f | K=%.2f | RR=%s | M0=%.4f",
    display_ids, design$steepness, design$tag_tau, design$tag_mixing_k_cutoff,
    short_reporting, design$m_age40_quarterly
  ),
  color = model_colours,
  h = round(design$steepness, 6),
  tau = round(design$tag_tau, 1),
  K = round(design$tag_mixing_k_cutoff, 2),
  reporting = short_reporting,
  reporting_flag2 = reporting_flag,
  M0 = round(design$m_age40_quarterly, 6),
  creep_primary = round(100 * design$effort_creep_primary, 2),
  creep_secondary = round(100 * design$effort_creep_secondary, 3),
  mgc = signif(fit$maximum_gradient, 7),
  hessian = ifelse(fit$positive_definite_hessian, "PDH", "Near-PDH"),
  objective = round(fit$objective_function, 3),
  depletion_recent = round(management$sb_recent_sb0, 6),
  sb_sbmsy_recent = round(management$sb_recent_sbmsy, 6),
  f_fmsy_recent = round(management$f_recent_fmsy, 6)
)

series_payload <- lapply(seq_along(ids), function(index) {
  source_id <- ids[[index]]
  value <- series[series$ensemble_id == source_id, ]
  list(
    id = display_ids[[index]],
    year = as.integer(value$year),
    depletion = round(value$depletion, 7),
    recruitment = round(value$recruitment, 5),
    spawning = round(value$spawning_potential, 5),
    fishing = round(value$fishing_mortality, 7)
  )
})

scope_payload <- lapply(scope_definitions, function(scope) {
  list(
    id = scope$id,
    label = scope$label,
    reporting_flag2 = scope$reporting_flag2,
    model_count = length(scope$ids),
    detail = scope$detail,
    model_ids = display_ids[match(scope$ids, ids)]
  )
})
payload <- list(
  metadata = list(
    retained_models = length(ids),
    reporting_flag_mapping = list(
      `0` = "inclusion",
      `1` = "exclusion"
    ),
    scope_note = paste0(
      "Retained-subset sensitivity; not an isolated reporting-rate effect. ",
      "The combined scope gives every retained model equal weight."
    ),
    zero_mixing_note = paste0(
      "Flag2=0 includes pre-mixing reporting rates and flag2=1 excludes them. ",
      "For mixing=0 rows, stored flag2=1 is an inactive compatibility sentinel; no tag event or recapture is removed."
    )
  ),
  scopes = scope_payload,
  summaries = scope_summaries,
  models = model_meta,
  series = series_payload
)
viewer_json <- jsonlite::toJSON(
  payload,
  dataframe = "rows",
  auto_unbox = TRUE,
  digits = 9,
  null = "null",
  na = "null"
)
viewer_json <- gsub("</", "<\\/", viewer_json, fixed = TRUE)

template_file <- file.path("report", "interactive-viewer-template.html")
if (!file.exists(template_file)) {
  stop("Missing the interactive-viewer template.", call. = FALSE)
}
template <- paste(readLines(template_file, warn = FALSE), collapse = "\n")
markers <- gregexpr("__VIEWER_DATA__", template, fixed = TRUE)[[1L]]
if (sum(markers >= 0L) != 1L) {
  stop("The interactive-viewer template must contain one payload marker.", call. = FALSE)
}
viewer_html <- sub("__VIEWER_DATA__", viewer_json, template, fixed = TRUE)

output_dir <- Sys.getenv("REPORT_OUTPUT_DIR", "results")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_file <- file.path(output_dir, "bet-2026-ensemble-interactive-viewer.html")
writeLines(viewer_html, output_file, useBytes = TRUE)
cat("Wrote ", output_file, " with ", length(ids), " models.\n", sep = "")
