#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Install jsonlite to build the interactive viewer.", call. = FALSE)
}

series <- readRDS("data/ensemble/ensemble-timeseries.rds")
fit <- read.csv("data/ensemble/fit-diagnostics.csv", check.names = FALSE)
design <- read.csv("data/ensemble/successful-model-design.csv", check.names = FALSE)
management <- read.csv("data/ensemble/management-quantities.csv", check.names = FALSE)

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

design <- design[match(ids, design$ensemble_id), ]
fit <- fit[match(ids, fit$ensemble_id), ]
management <- management[match(ids, management$ensemble_id), ]
series <- series[series$ensemble_id %in% ids, ]
series <- series[order(series$ensemble_id, series$year), ]

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
  id = ids,
  label = sprintf(
    "%s | h=%.3f | tau=%.1f | K=%.2f | RR=%s | M0=%.4f",
    ids, design$steepness, design$tag_tau, design$tag_mixing_k_cutoff,
    short_reporting, design$m_age40_quarterly
  ),
  color = model_colours,
  h = round(design$steepness, 6),
  tau = round(design$tag_tau, 1),
  K = round(design$tag_mixing_k_cutoff, 2),
  reporting = short_reporting,
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

series_payload <- lapply(ids, function(id) {
  value <- series[series$ensemble_id == id, ]
  list(
    id = id,
    year = as.integer(value$year),
    depletion = round(value$depletion, 7),
    recruitment = round(value$recruitment, 5),
    spawning = round(value$spawning_potential, 5),
    fishing = round(value$fishing_mortality, 7)
  )
})

payload <- list(models = model_meta, series = series_payload)
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
