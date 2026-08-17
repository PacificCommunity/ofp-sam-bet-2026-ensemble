options(stringsAsFactors = FALSE)

output_dir <- Sys.getenv("REPORT_OUTPUT_DIR", "results")
report_file <- file.path(output_dir, "bet-2026-ensemble-report.html")
viewer_file <- file.path(output_dir, "bet-2026-ensemble-interactive-viewer.html")
manifest_file <- file.path(output_dir, "report-manifest.csv")
rr_table_names <- c(
  "group-counts-qc.csv", "structural-management-summary.csv",
  "structural-management-risk.csv", "hessian-management-intervals.csv",
  "hessian-management-risk.csv", "projection-terminal-summary.csv",
  "projection-selected-years.csv", "differences-vs-combined.csv",
  "estimation-management-intervals.csv",
  "estimation-management-summary.csv", "cmm-depletion-comparison.csv",
  "estimation-management-risk.csv", "structural-reference-points.csv",
  "estimation-uncertainty-audit.csv", "projection-summary.csv"
)
rr_figure_stems <- c(
  "rr-sensitivity-history-comparison",
  "rr-sensitivity-management-distributions",
  "rr-sensitivity-current-status",
  "rr-sensitivity-projections",
  "rr-sensitivity-key-combined",
  "rr-sensitivity-key-inclusion",
  "rr-sensitivity-key-exclusion"
)
rr_scope_prefixes <- c("rr0-inclusion", "rr1-exclusion")
rr_scope_report_names <- c(
  "bet-2026-ensemble-report-rr0-inclusion.html",
  "bet-2026-ensemble-report-rr1-exclusion.html"
)
rr_scope_table_stems <- c(
  "management-summary", "management-risk", "ensemble-fit-diagnostics",
  "cmm-depletion-comparison", "estimation-management-intervals",
  "estimation-management-summary", "estimation-management-risk",
  "structural-reference-points", "estimation-uncertainty-audit",
  "projection-summary", "projection-terminal-management",
  "fit-hessian-summary"
)
rr_scope_figure_stems <- c(
  "combined-structural-estimation-uncertainty",
  "combined-kobe-majuro-status", "time-dynamic-kobe-majuro",
  "management-uncertainty-continuous-axes",
  "management-uncertainty-discrete-axes-a",
  "projection-terminal-status", "projection-key-quantities",
  "projection-stock-trajectories"
)
rr_scope_tables <- as.vector(outer(
  rr_scope_prefixes, rr_scope_table_stems, paste, sep = "-"
))
rr_scope_figures <- as.vector(outer(
  rr_scope_prefixes, rr_scope_figure_stems, paste, sep = "-"
))

required <- c(
  report_file, viewer_file, manifest_file,
  file.path(output_dir, rr_scope_report_names),
  file.path(output_dir, "tables", c(
    "management-summary.csv", "management-risk.csv",
    "estimation-management-summary.csv", "estimation-management-intervals.csv",
    "estimation-management-risk.csv", "projection-summary.csv",
    "projection-terminal-management.csv", "fit-hessian-summary.csv",
    "structural-reference-points.csv", "estimation-uncertainty-audit.csv",
    "model-id-map.csv", "cmm-depletion-comparison.csv"
  )),
  file.path(output_dir, "rr-sensitivity", "tables", rr_table_names),
  file.path(
    output_dir, "rr-sensitivity", "tables", paste0(rr_scope_tables, ".csv")
  ),
  file.path(
    output_dir, "rr-sensitivity", "figures",
    paste0(rep(rr_figure_stems, each = 2L), c(".png", ".pdf"))
  ),
  file.path(
    output_dir, "rr-sensitivity", "figures",
    paste0(rep(rr_scope_figures, each = 2L), c(".png", ".pdf"))
  )
)
if (any(!file.exists(required))) stop("The rendered public report is incomplete.")

pngs <- list.files(file.path(output_dir, "figures"), pattern = "[.]png$", full.names = TRUE)
pdfs <- list.files(file.path(output_dir, "figures"), pattern = "[.]pdf$", full.names = TRUE)
if (length(pngs) != 11L || length(pdfs) != 11L) {
  stop("Expected 11 publication figure sets in both PNG and vector PDF formats.")
}
if (any(file.info(c(pngs, pdfs))$size < 10000L)) {
  stop("A rendered report figure is unexpectedly small.")
}
rr_pngs <- list.files(
  file.path(output_dir, "rr-sensitivity", "figures"),
  pattern = "[.]png$", full.names = TRUE
)
rr_pdfs <- list.files(
  file.path(output_dir, "rr-sensitivity", "figures"),
  pattern = "[.]pdf$", full.names = TRUE
)
expected_rr_figure_stems <- c(rr_figure_stems, rr_scope_figures)
if (length(rr_pngs) != length(expected_rr_figure_stems) ||
    length(rr_pdfs) != length(expected_rr_figure_stems) ||
    !setequal(
      tools::file_path_sans_ext(basename(rr_pngs)), expected_rr_figure_stems
    ) ||
    !setequal(
      tools::file_path_sans_ext(basename(rr_pdfs)), expected_rr_figure_stems
    ) ||
    any(file.info(c(rr_pngs, rr_pdfs))$size < 10000L)) {
  stop("The 23 RR-sensitivity figure sets are incomplete or unexpectedly small.")
}
rr_csvs <- list.files(
  file.path(output_dir, "rr-sensitivity", "tables"),
  pattern = "[.]csv$", full.names = TRUE
)
expected_rr_tables <- c(rr_table_names, paste0(rr_scope_tables, ".csv"))
if (length(rr_csvs) != length(expected_rr_tables) ||
    !setequal(basename(rr_csvs), expected_rr_tables) ||
    any(file.info(rr_csvs)$size <= 0L)) {
  stop("The 39 RR-sensitivity tables are incomplete or unexpectedly empty.")
}

report <- paste(readLines(report_file, warn = FALSE), collapse = "\n")
viewer <- paste(readLines(viewer_file, warn = FALSE), collapse = "\n")
count_fixed <- function(text, pattern) {
  matches <- gregexpr(pattern, text, fixed = TRUE)[[1L]]
  if (length(matches) == 1L && matches[[1L]] == -1L) 0L else length(matches)
}
rr_scope_reports <- setNames(lapply(rr_scope_report_names, function(name) {
  paste(readLines(file.path(output_dir, name), warn = FALSE), collapse = "\n")
}), rr_scope_prefixes)
report_required <- c(
  "BET 2026 ensemble analysis",
  "Overview", "Intervals", "50%", "80%", "95%",
  "Open 80-model interactive viewer", "fishing mortality",
  "Projection summary", "highest-density regions",
  "Management quantities with available estimation uncertainty", "Terminal management quantities",
  "Supporting structural reference points", "Monte Carlo audit",
  "Time-dynamic Kobe and Majuro status",
  "All-region projection trajectories", "All-region LRP depletion statistic",
  "spawning potential in thousand metric tonnes",
  "Scope",
  "10.1093/icesjms/fsu131",
  "10.1016/j.fishres.2022.106477",
  "framework of Hamel 2015, updated practical formulation of Hamel and Cope 2022",
  "retained 34 inclusion models and 46 exclusion models",
  "data-report-tab='overview'", "data-report-tab='figures'",
  "data-report-tab='tables'", "id='figures-list'", "id='tables-list'",
  "Copy table for Word", "Copy LaTeX", "Open vector PDF",
  "ten did not meet the MGC criterion and ten were incomplete"
)
report_required <- c(
  report_required,
  "Reporting-rate retained-subset sensitivity",
  "Combined (RR=0 + RR=1)", "RR=0: inclusion", "RR=1: exclusion",
  "retained-subset sensitivity summaries",
  "not an isolated causal effect",
  "stored flag2=1 is inactive because no pre-mixing window exists; no tag event or recapture is removed",
  "34/80 (42.5%)", "46/80 (57.5%)",
  "const panel=target.closest('[data-report-panel]')",
  "RR_SENSITIVITY_START", "RR_SENSITIVITY_END"
)
for (value in report_required) {
  if (!grepl(value, report, fixed = TRUE)) stop("Missing public-report element: ", value)
}
if (length(gregexpr("class='figure-card'", report, fixed = TRUE)[[1L]]) != 11L ||
    length(gregexpr("class='table-card'", report, fixed = TRUE)[[1L]]) != 8L) {
  stop("The report tabs do not contain exactly 11 unique figures and 8 unique tables.")
}
if (length(gregexpr("<!-- RR_SENSITIVITY_START -->", report, fixed = TRUE)[[1L]]) != 1L ||
    length(gregexpr("<!-- RR_SENSITIVITY_END -->", report, fixed = TRUE)[[1L]]) != 1L ||
    length(gregexpr("class='rr-figure", report, fixed = TRUE)[[1L]]) != 7L) {
  stop("The RR-sensitivity report section is duplicated or incomplete.")
}

scope_counts <- list(
  `rr0-inclusion` = c(models = 34L, pdh = 25L, near = 9L, mixture = 3400L,
                      projection = 340L, flag = 0L),
  `rr1-exclusion` = c(models = 46L, pdh = 37L, near = 9L, mixture = 4600L,
                      projection = 460L, flag = 1L)
)
for (prefix in rr_scope_prefixes) {
  html <- rr_scope_reports[[prefix]]
  counts <- scope_counts[[prefix]]
  required_text <- c(
    paste0(counts[["models"]], " equal-weight retained models"),
    paste0(counts[["pdh"]], " PDH"), paste0(counts[["near"]], " Near-PDH"),
    paste0(counts[["projection"]], " model–recruitment combinations"),
    "retained-subset sensitivity, not a matched causal reporting-rate effect",
    paste0("MFCL tag flag column 2 = ", counts[["flag"]]),
    "For mixing=0 rows, stored flag2=1 is an inactive compatibility sentinel; no tag event or recapture is removed",
    paste0("Each of ", counts[["pdh"]], " PDH models contributes 100 joint draws"),
    paste0("each of ", counts[["near"]], " Near-PDH models contributes its central estimate"),
    "Estimation uncertainty is unavailable for the Near-PDH fits; their central estimates are repeated only to preserve equal total model weight.",
    "Projection uncertainty excludes Hessian parameter draws.",
    "Historical",
    "stable source ensemble IDs; models are not renumbered within RR subsets"
  )
  for (value in required_text) {
    if (!grepl(value, html, fixed = TRUE)) {
      stop("Missing ", prefix, " report element: ", value)
    }
  }
  if (count_fixed(html, "class='figure-card'") != 8L ||
      count_fixed(html, "class='table-card'") != 8L ||
      count_fixed(html, "data:image/png;base64,") != 8L ||
      count_fixed(html, "Open vector PDF") != 8L ||
      count_fixed(html, "Save PNG") != 8L ||
      count_fixed(html, "Copy caption") != 8L ||
      count_fixed(html, "Copy figure for LaTeX") != 8L ||
      count_fixed(html, "Copy table for Word") != 8L ||
      count_fixed(html, "Copy LaTeX") != 8L) {
    stop(prefix, " report does not match the main 8-figure/8-table control contract.")
  }
  if (grepl("<script[^>]+src=|<link[^>]+href=", html,
            ignore.case = TRUE, perl = TRUE) ||
      grepl("<img[^>]+src=['\"](?!data:)", html,
            ignore.case = TRUE, perl = TRUE)) {
    stop(prefix, " report is not self-contained.")
  }
  href_matches <- regmatches(
    html, gregexpr("href=['\"][^'\"]+['\"]", html, perl = TRUE)
  )[[1L]]
  hrefs <- sub("^href=['\"]", "", href_matches)
  hrefs <- sub("['\"]$", "", hrefs)
  local_hrefs <- hrefs[!grepl("^(https?:|mailto:|#)", hrefs)]
  if (any(!file.exists(file.path(output_dir, local_hrefs)))) {
    stop(prefix, " report contains a broken local download link.")
  }
}

viewer_required <- c(
  "BET 2026 ensemble model results", "80 assessment configurations retained",
  "depletion", "recruitment", "spawning", "fishing",
  "Models &middot; ensemble-001&ndash;ensemble-080", "Select all", "Clear", "Fit summary",
  "Near-PDH", "F (year⁻¹)", "modelList", "fitTable",
  "<sub>recent</sub>", "<sub>MSY</sub>", "&tau;",
  "ensemble-001", "ensemble-080"
)
viewer_required <- c(
  viewer_required,
  "Reporting-rate analysis scope", "All retained",
  "RR inclusion &middot; flag2=0", "RR exclusion &middot; flag2=1",
  "34 inclusion + 46 exclusion", "Retained-subset sensitivity; not an isolated RR effect",
  "For mixing=0 rows, stored flag2=1 is an inactive compatibility sentinel; no tag event or recapture is removed",
  "scopeButtons", "scopeSummary", "median_difference_from_all"
)
for (value in viewer_required) {
  if (!grepl(value, viewer, fixed = TRUE)) stop("Missing interactive-viewer element: ", value)
}
if (grepl("Ensemble median", viewer, fixed = TRUE) ||
    grepl("Filter table", viewer, fixed = TRUE)) {
  stop("The interactive viewer contains a removed median or filter-table control.")
}

fit_output <- read.csv(
  file.path(output_dir, "tables", "ensemble-fit-diagnostics.csv"),
  check.names = FALSE
)
fit_summary <- read.csv(
  file.path(output_dir, "tables", "fit-hessian-summary.csv"),
  check.names = FALSE
)
model_id_map <- read.csv(
  file.path(output_dir, "tables", "model-id-map.csv"),
  check.names = FALSE
)
source_fit <- read.csv("data/ensemble/fit-diagnostics.csv", check.names = FALSE)
included_ids <- source_fit$ensemble_id[source_fit$maximum_gradient <= 1e-4]
excluded_ids <- source_fit$ensemble_id[source_fit$maximum_gradient > 1e-4]
display_ids <- sprintf("ensemble-%03d", seq_len(80L))
if (nrow(fit_output) != 80L || nrow(fit_summary) != 80L ||
    nrow(model_id_map) != 80L ||
    !identical(fit_output$ensemble_id, display_ids) ||
    !identical(fit_summary$Model, display_ids) ||
    !identical(model_id_map$ensemble_id, display_ids) ||
    !setequal(fit_output$source_ensemble_id, included_ids) ||
    !setequal(model_id_map$source_ensemble_id, included_ids) ||
    any(model_id_map$source_ensemble_id %in% excluded_ids)) {
  stop("The rendered fit tables are not restricted to the 80 MGC-filtered models.")
}

base_management <- read.csv(
  file.path(output_dir, "tables", "management-summary.csv"),
  check.names = FALSE
)
base_risk <- read.csv(
  file.path(output_dir, "tables", "management-risk.csv"),
  check.names = FALSE
)
if (!identical(
      base_management$Period,
      c(
        "2021–2024 / 2014–2023", "2021–2024 / equilibrium SBMSY",
        "2020–2023 / equilibrium FMSY"
      )
    ) ||
    !identical(names(base_risk), c("Indicator", "Criterion", "Events", "Models", "Percent")) ||
    !identical(as.integer(base_risk$Events), c(15L, 8L, 19L)) ||
    !identical(as.integer(base_risk$Models), rep(80L, 3L)) ||
    any(abs(base_risk$Percent - 100 * base_risk$Events / base_risk$Models) > 1e-12)) {
  stop("The structural management CSV labels, periods or event accounting are invalid.")
}

# Lock the combined 80-model result to the values printed in WP-06 Tables 8--13.
# The report CSVs retain full precision; these checks deliberately validate the
# publication rounding as a second, independent contract.
wp06_table8 <- read.csv(
  file.path(output_dir, "tables", "estimation-management-summary.csv"),
  check.names = FALSE
)
wp06_table8_expected <- data.frame(
  Quantity = c("SBrecent / SBF=0", "SBrecent / SBMSY", "Frecent / FMSY"),
  Period = c(
    "2021–2024 / 2014–2023", "2021–2024 / equilibrium SBMSY",
    "2020–2023 / equilibrium FMSY"
  ),
  Median = c(0.277, 1.532, 0.838),
  `50% interval` = c("0.217–0.336", "1.136–2.024", "0.702–0.983"),
  `80% interval` = c("0.172–0.395", "0.822–2.572", "0.592–1.206"),
  `95% interval` = c("0.136–0.442", "0.635–3.519", "0.460–1.351"),
  check.names = FALSE
)
if (!identical(wp06_table8, wp06_table8_expected)) {
  stop("The combined result no longer reproduces WP-06 Table 8.")
}

wp06_table9 <- read.csv(
  file.path(output_dir, "tables", "cmm-depletion-comparison.csv"),
  check.names = FALSE
)
if (!identical(round(wp06_table9$Value, 3), c(0.278, 0.288, 0.967)) ||
    !identical(as.integer(wp06_table9$Models), rep(80L, 3L))) {
  stop("The combined central-model result no longer reproduces WP-06 Table 9.")
}

wp06_table10 <- read.csv(
  file.path(output_dir, "tables", "estimation-management-risk.csv"),
  check.names = FALSE
)
if (!identical(round(100 * wp06_table10$Probability, 1), c(18.6, 17.3, 24.1))) {
  stop("The combined estimation-inclusive result no longer reproduces WP-06 Table 10.")
}

wp06_table11 <- read.csv(
  file.path(output_dir, "tables", "structural-reference-points.csv"),
  check.names = FALSE
)
wp06_table11_expected <- list(
  "F multiplier at MSY" = c(0.695, 0.837, 1.192, 1.253, 1.682, 3.026),
  "Frecent / FMSY" = c(0.330, 0.594, 0.839, 0.860, 1.195, 1.440),
  "SBF=0" = c(874.7, 969.7, 1625.0, 1851.3, 3222.4, 3990.9),
  "SBlatest / SBF=0" = c(0.113, 0.151, 0.242, 0.248, 0.350, 0.428),
  "SBlatest / SBMSY" = c(0.657, 0.842, 1.348, 1.395, 1.994, 2.477),
  "SBrecent / SBF=0" = c(0.126, 0.175, 0.278, 0.280, 0.384, 0.503),
  "SBrecent / SBMSY" = c(0.737, 0.997, 1.507, 1.581, 2.236, 2.882),
  "SBMSY" = c(112.6, 152.6, 289.0, 346.1, 661.8, 1019.0),
  "SBMSY / SBF=0" = c(0.128, 0.156, 0.176, 0.179, 0.200, 0.255)
)
wp06_table11_columns <- c("Minimum", "10%", "Median", "Mean", "90%", "Maximum")
round_for_publication <- function(values, digits) {
  scale <- 10^digits
  sign(values) * floor(abs(values) * scale + 0.5 + 1e-12) / scale
}
for (quantity in names(wp06_table11_expected)) {
  row <- wp06_table11[wp06_table11$Quantity == quantity, wp06_table11_columns]
  expected <- wp06_table11_expected[[quantity]]
  digits <- if (quantity %in% c("SBF=0", "SBMSY")) 1L else 3L
  tolerance <- 0.5 * 10^(-digits) + 1e-10
  if (nrow(row) != 1L ||
      any(abs(as.numeric(row[1L, ]) - expected) > tolerance)) {
    stop("The combined structural result no longer reproduces WP-06 Table 11: ", quantity)
  }
}

wp06_table12 <- read.csv(
  file.path(output_dir, "tables", "estimation-uncertainty-audit.csv"),
  check.names = FALSE
)
wp06_table12_expected <- rbind(
  c(0.277, 0.172, 0.395, 0.275, 0.173, 0.393, 0.0005),
  c(1.532, 0.822, 2.572, 1.522, 0.828, 2.752, 0.0396),
  c(0.838, 0.592, 1.206, 0.838, 0.577, 1.201, 0.0034)
)
wp06_table12_observed <- cbind(
  wp06_table12$All_median, wp06_table12$All_q10, wp06_table12$All_q90,
  wp06_table12$PDH_median, wp06_table12$PDH_q10, wp06_table12$PDH_q90,
  wp06_table12$Max_50_100_difference
)
for (index in seq_len(nrow(wp06_table12_expected))) {
  digits <- c(3L, 3L, 3L, 3L, 3L, 3L, 4L)
  rounded <- mapply(round_for_publication, wp06_table12_observed[index, ], digits)
  if (!identical(as.numeric(rounded), as.numeric(wp06_table12_expected[index, ]))) {
    stop("The combined uncertainty audit no longer reproduces WP-06 Table 12.")
  }
}

wp06_table13 <- read.csv(
  file.path(output_dir, "tables", "projection-summary.csv"),
  check.names = FALSE
)
wp06_table13_expected <- data.frame(
  Year = c(2030L, 2040L, 2054L),
  d10 = c(0.225, 0.409, 0.455), d50 = c(0.355, 0.517, 0.545),
  d90 = c(0.514, 0.628, 0.641), risk = c("5.2%", "0.0%", "0.0%"),
  sb = c(523.1, 690.5, 720.6), c10 = c(0.638, 0.642, 0.648),
  c50 = c(0.811, 0.842, 0.847), c90 = c(1.003, 1.052, 1.053)
)
wp06_table13_observed <- data.frame(
  Year = as.integer(wp06_table13$Year),
  d10 = round(as.numeric(wp06_table13[[2L]]), 3),
  d50 = round(as.numeric(wp06_table13[[3L]]), 3),
  d90 = round(as.numeric(wp06_table13[[4L]]), 3),
  risk = as.character(wp06_table13[[5L]]),
  sb = round(as.numeric(wp06_table13[[6L]]), 1),
  c10 = round(as.numeric(wp06_table13[[7L]]), 3),
  c50 = round(as.numeric(wp06_table13[[8L]]), 3),
  c90 = round(as.numeric(wp06_table13[[9L]]), 3)
)
if (!identical(wp06_table13_observed, wp06_table13_expected)) {
  stop("The combined projection result no longer reproduces WP-06 Table 13.")
}

# Independently reconstruct every RR-sensitivity table from the checksum-locked
# source payloads.  This deliberately does not reuse objects written by
# rr-sensitivity.R: the public tables are the values under test.
assert_close <- function(observed, expected, tolerance, label) {
  if (length(observed) != length(expected) ||
      anyNA(observed) != anyNA(expected) ||
      any(abs(observed - expected) > tolerance, na.rm = TRUE)) {
    stop("RR-sensitivity numerical validation failed: ", label, ".")
  }
}

compare_table <- function(observed, expected, keys, numeric_columns, label) {
  if (!setequal(names(observed), names(expected))) {
    stop("RR-sensitivity table schema mismatch: ", label, ".")
  }
  observed <- observed[do.call(order, observed[keys]), names(expected), drop = FALSE]
  expected <- expected[do.call(order, expected[keys]), , drop = FALSE]
  rownames(observed) <- NULL
  rownames(expected) <- NULL
  character_columns <- setdiff(names(expected), numeric_columns)
  if (nrow(observed) != nrow(expected) ||
      !identical(observed[character_columns], expected[character_columns])) {
    stop("RR-sensitivity table identity mismatch: ", label, ".")
  }
  for (column in numeric_columns) {
    assert_close(
      as.numeric(observed[[column]]), as.numeric(expected[[column]]),
      5e-12, paste(label, column)
    )
  }
}

rr_table_dir <- file.path(output_dir, "rr-sensitivity", "tables")
rr_tables <- setNames(lapply(rr_table_names, function(name) {
  read.csv(file.path(rr_table_dir, name), check.names = FALSE)
}), rr_table_names)

planned <- read.csv("design/model-draws.csv", check.names = FALSE)
source_design <- read.csv(
  "data/ensemble/successful-model-design.csv", check.names = FALSE
)
source_management <- read.csv(
  "data/ensemble/management-quantities.csv", check.names = FALSE
)
source_series <- readRDS("data/ensemble/ensemble-timeseries.rds")
source_hessian <- readRDS("data/estimation/native-hessian-uncertainty.rds")
source_projection <- readRDS("data/projection/native-projections.rds")

if (nrow(planned) != 100L ||
    !identical(
      as.integer(table(factor(planned$tag_reporting_flag2, levels = 0:1))),
      c(50L, 50L)
    ) ||
    any(planned$tag_reporting != ifelse(
      planned$tag_reporting_flag2 == 0L, "inclusion", "exclusion"
    ))) {
  stop("The planned reporting-rate design is not the locked 50/50 flag2 design.")
}

retained_source_ids <- sort(included_ids)
rr_by_id <- setNames(
  as.integer(planned$tag_reporting_flag2), planned$ensemble_id
)
rr_group_ids <- list(
  `Combined (RR=0 + RR=1)` = retained_source_ids,
  `RR=0: inclusion` = retained_source_ids[rr_by_id[retained_source_ids] == 0L],
  `RR=1: exclusion` = retained_source_ids[rr_by_id[retained_source_ids] == 1L]
)
if (!identical(unname(vapply(rr_group_ids, length, integer(1L))), c(80L, 34L, 46L)) ||
    length(intersect(rr_group_ids[[2L]], rr_group_ids[[3L]])) != 0L ||
    !setequal(rr_group_ids[[1L]], c(rr_group_ids[[2L]], rr_group_ids[[3L]]))) {
  stop("The retained reporting-rate groups are not the exact 80/34/46 partition.")
}
if (any(source_design$tag_reporting_flag2 !=
      rr_by_id[source_design$ensemble_id]) ||
    any(source_design$tag_reporting != ifelse(
      source_design$tag_reporting_flag2 == 0L, "inclusion", "exclusion"
    )) ||
    any(source_design$tag_reporting_flag2 == 0L &
      source_design$tag_reporting_zero_mixing_exclusions !=
        source_design$zero_mixing_events)) {
  stop("The completed-model reporting-rate mapping or zero-mixing rule changed.")
}

documented_incomplete <- c(
  "ensemble-013", "ensemble-026", "ensemble-028", "ensemble-058",
  "ensemble-060", "ensemble-061", "ensemble-064", "ensemble-067",
  "ensemble-077", "ensemble-097"
)
documented_mgc <- c(
  "ensemble-003", "ensemble-016", "ensemble-017", "ensemble-020",
  "ensemble-037", "ensemble-048", "ensemble-059", "ensemble-083",
  "ensemble-096", "ensemble-100"
)
pdh_source_ids <- intersect(retained_source_ids, source_hessian$pdh_model_ids)
near_source_ids <- intersect(retained_source_ids, source_hessian$near_pdh_model_ids)
expected_group_counts <- do.call(rbind, lapply(seq_along(rr_group_ids), function(index) {
  ids <- rr_group_ids[[index]]
  flag <- c(NA_integer_, 0L, 1L)[[index]]
  planned_ids <- if (is.na(flag)) planned$ensemble_id else
    planned$ensemble_id[planned$tag_reporting_flag2 == flag]
  data.frame(
    Group = names(rr_group_ids)[[index]],
    RR_flag2 = c("0 + 1", "0", "1")[[index]],
    Planned_models = length(planned_ids),
    Public_payload_models = sum(source_fit$ensemble_id %in% planned_ids),
    Completed_final_PAR_models = length(setdiff(planned_ids, documented_incomplete)),
    Retained_models = length(ids),
    Documented_MGC_excluded = sum(documented_mgc %in% planned_ids),
    Documented_incomplete = sum(documented_incomplete %in% planned_ids),
    PDH_models = sum(ids %in% pdh_source_ids),
    Near_PDH_models = sum(ids %in% near_source_ids),
    Hessian_draws_per_PDH = as.integer(source_hessian$draws_per_pdh_model),
    Hessian_mixture_rows = length(ids) * source_hessian$draws_per_pdh_model,
    Projection_sequences_per_model = as.integer(source_projection$simulations_per_model),
    Projection_combinations = length(ids) * source_projection$simulations_per_model,
    Within_group_model_weight = 1 / length(ids),
    Share_of_combined = length(ids) / 80,
    check.names = FALSE
  )
}))
compare_table(
  rr_tables[["group-counts-qc.csv"]], expected_group_counts, "Group",
  setdiff(names(expected_group_counts), c("Group", "RR_flag2")), "group counts"
)

quantiles <- function(x, probabilities) {
  stats::quantile(x, probabilities, names = FALSE, type = 7)
}
management_specs <- data.frame(
  column = c("sb_recent_sb0", "sb_recent_sbmsy", "f_recent_fmsy"),
  Quantity = c("SBrecent / SBF=0", "SBrecent / SBMSY", "Frecent / FMSY"),
  Period = c(
    "2021–2024 / 2014–2023", "2021–2024 / equilibrium SBMSY",
    "2020–2023 / equilibrium FMSY"
  ), stringsAsFactors = FALSE
)
risk_specs <- data.frame(
  column = c("below_lrp_020", "below_sbmsy", "above_fmsy"),
  Criterion = c(
    "SBrecent/SBF=0 < 0.20", "SBrecent/SBMSY < 1", "Frecent/FMSY > 1"
  ), stringsAsFactors = FALSE
)

expected_structural <- do.call(rbind, lapply(names(rr_group_ids), function(group) {
  values <- source_management[
    source_management$ensemble_id %in% rr_group_ids[[group]], , drop = FALSE
  ]
  do.call(rbind, lapply(seq_len(nrow(management_specs)), function(index) {
    x <- values[[management_specs$column[[index]]]]
    q <- quantiles(x, c(.1, .5, .9))
    data.frame(
      Group = group, Quantity = management_specs$Quantity[[index]],
      Period = management_specs$Period[[index]], Models = nrow(values),
      `10%` = q[[1L]], Median = q[[2L]], Mean = mean(x), `90%` = q[[3L]],
      check.names = FALSE
    )
  }))
}))
compare_table(
  rr_tables[["structural-management-summary.csv"]], expected_structural,
  c("Group", "Quantity"), c("Models", "10%", "Median", "Mean", "90%"),
  "structural management"
)

expected_structural_risk <- do.call(rbind, lapply(names(rr_group_ids), function(group) {
  values <- source_management[
    source_management$ensemble_id %in% rr_group_ids[[group]], , drop = FALSE
  ]
  do.call(rbind, lapply(seq_len(nrow(risk_specs)), function(index) {
    events <- as.logical(values[[risk_specs$column[[index]]]])
    data.frame(
      Group = group, Criterion = risk_specs$Criterion[[index]],
      Models = nrow(values), Events = sum(events), Probability = mean(events)
    )
  }))
}))
compare_table(
  rr_tables[["structural-management-risk.csv"]], expected_structural_risk,
  c("Group", "Criterion"), c("Models", "Events", "Probability"),
  "structural risk"
)

draws_per_model <- as.integer(source_hessian$draws_per_pdh_model)
pdh_management <- source_hessian$management_draws[
  source_hessian$management_draws$ensemble_id %in% pdh_source_ids,
  c("ensemble_id", "draw", management_specs$column), drop = FALSE
]
near_management <- merge(
  source_management[
    source_management$ensemble_id %in% near_source_ids,
    c("ensemble_id", management_specs$column), drop = FALSE
  ],
  data.frame(draw = seq_len(draws_per_model)), by = NULL
)
hybrid_management <- rbind(
  pdh_management,
  near_management[c("ensemble_id", "draw", management_specs$column)]
)
if (length(table(hybrid_management$ensemble_id)) != 80L ||
    any(table(hybrid_management$ensemble_id) != 100L)) {
  stop("The independent Hessian mixture is not model-equal across all 80 fits.")
}

expected_hessian <- do.call(rbind, lapply(names(rr_group_ids), function(group) {
  ids <- rr_group_ids[[group]]
  values <- hybrid_management[hybrid_management$ensemble_id %in% ids, , drop = FALSE]
  do.call(rbind, lapply(seq_len(nrow(management_specs)), function(index) {
    x <- values[[management_specs$column[[index]]]]
    q <- quantiles(x, c(.025, .10, .25, .50, .75, .90, .975))
    data.frame(
      Group = group, Quantity = management_specs$Quantity[[index]],
      Period = management_specs$Period[[index]], Models = length(ids),
      PDH_models = sum(ids %in% pdh_source_ids),
      Near_PDH_models = sum(ids %in% near_source_ids), Mixture_rows = nrow(values),
      `2.5%` = q[[1L]], `10%` = q[[2L]], `25%` = q[[3L]],
      Median = q[[4L]], `75%` = q[[5L]], `90%` = q[[6L]],
      `97.5%` = q[[7L]], check.names = FALSE
    )
  }))
}))
compare_table(
  rr_tables[["hessian-management-intervals.csv"]], expected_hessian,
  c("Group", "Quantity"),
  c("Models", "PDH_models", "Near_PDH_models", "Mixture_rows",
    "2.5%", "10%", "25%", "Median", "75%", "90%", "97.5%"),
  "Hessian-inclusive management"
)

expected_hessian_risk <- do.call(rbind, lapply(names(rr_group_ids), function(group) {
  ids <- rr_group_ids[[group]]
  values <- hybrid_management[hybrid_management$ensemble_id %in% ids, , drop = FALSE]
  events <- list(
    values$sb_recent_sb0 < .20, values$sb_recent_sbmsy < 1,
    values$f_recent_fmsy > 1
  )
  do.call(rbind, lapply(seq_len(nrow(risk_specs)), function(index) {
    data.frame(
      Group = group, Criterion = risk_specs$Criterion[[index]],
      Models = length(ids), Mixture_rows = nrow(values),
      Probability = mean(events[[index]])
    )
  }))
}))
compare_table(
  rr_tables[["hessian-management-risk.csv"]], expected_hessian_risk,
  c("Group", "Criterion"), c("Models", "Mixture_rows", "Probability"),
  "Hessian-inclusive risk"
)

source("report/management-quantities.R")
projection <- source_projection
for (name in names(projection)) {
  value <- projection[[name]]
  if (is.data.frame(value) && "ensemble_id" %in% names(value)) {
    projection[[name]] <- value[
      value$ensemble_id %in% retained_source_ids, , drop = FALSE
    ]
  }
}
projection$ensemble_ids <- retained_source_ids
projection$projection_complete_models <- 80L
series_retained <- source_series[
  source_series$ensemble_id %in% retained_source_ids, , drop = FALSE
]
projection_management <- build_projection_management(series_retained, projection)
terminal_depletion <- projection_management$projected[
  projection_management$projected$year == max(projection$projection_years),
  c("ensemble_id", "simulation", "sb_recent_sb0"), drop = FALSE
]
terminal <- merge(
  projection$terminal_msy, terminal_depletion,
  by = c("ensemble_id", "simulation"), sort = FALSE
)
terminal_specs <- data.frame(
  column = c("terminal_sb_sbmsy", "sb_recent_sb0", "terminal_f_fmsy"),
  Quantity = c(
    "SB2051–2054 / SBMSY", "SB2051–2054 / SBF=0", "F2050–2053 / FMSY"
  ),
  Criterion = c(
    "SB2051–2054/SBMSY < 1", "SB2051–2054/SBF=0 < 0.20",
    "F2050–2053/FMSY > 1"
  ),
  direction = c("below1", "below020", "above1"), stringsAsFactors = FALSE
)
expected_projection_terminal <- do.call(rbind, lapply(names(rr_group_ids), function(group) {
  ids <- rr_group_ids[[group]]
  values <- terminal[terminal$ensemble_id %in% ids, , drop = FALSE]
  do.call(rbind, lapply(seq_len(nrow(terminal_specs)), function(index) {
    x <- values[[terminal_specs$column[[index]]]]
    q <- quantiles(x, c(.1, .5, .9))
    beyond <- switch(
      terminal_specs$direction[[index]], below1 = x < 1,
      below020 = x < .20, above1 = x > 1
    )
    data.frame(
      Group = group, Quantity = terminal_specs$Quantity[[index]],
      Models = length(ids), Projection_combinations = nrow(values),
      `10%` = q[[1L]], Median = q[[2L]], Mean = mean(x), `90%` = q[[3L]],
      Criterion = terminal_specs$Criterion[[index]],
      Beyond_criterion_probability = mean(beyond), check.names = FALSE
    )
  }))
}))
compare_table(
  rr_tables[["projection-terminal-summary.csv"]], expected_projection_terminal,
  c("Group", "Quantity"),
  c("Models", "Projection_combinations", "10%", "Median", "Mean", "90%",
    "Beyond_criterion_probability"), "projection terminal"
)

expected_projection_years <- do.call(rbind, lapply(names(rr_group_ids), function(group) {
  ids <- rr_group_ids[[group]]
  do.call(rbind, lapply(c(2030L, 2040L, 2054L), function(year) {
    d <- projection_management$projected[
      projection_management$projected$ensemble_id %in% ids &
        projection_management$projected$year == year, , drop = FALSE
    ]
    s <- projection$annual_stock[
      projection$annual_stock$ensemble_id %in% ids &
        projection$annual_stock$year == year, , drop = FALSE
    ]
    c <- projection$catch_msy[
      projection$catch_msy$ensemble_id %in% ids &
        projection$catch_msy$year == year, , drop = FALSE
    ]
    dq <- quantiles(d$sb_recent_sb0, c(.1, .5, .9))
    sq <- quantiles(s$spawning_biomass_mt / 1000, c(.1, .5, .9))
    cq <- quantiles(c$catch_msy, c(.1, .5, .9))
    data.frame(
      Group = group, Year = year,
      Depletion_q10 = dq[[1L]], Depletion_median = dq[[2L]],
      Depletion_q90 = dq[[3L]], Probability_below_LRP = mean(d$below_lrp_020),
      Spawning_q10_kt = sq[[1L]], Spawning_median_kt = sq[[2L]],
      Spawning_q90_kt = sq[[3L]], Catch_MSY_q10 = cq[[1L]],
      Catch_MSY_median = cq[[2L]], Catch_MSY_q90 = cq[[3L]]
    )
  }))
}))
compare_table(
  rr_tables[["projection-selected-years.csv"]], expected_projection_years,
  c("Group", "Year"), setdiff(names(expected_projection_years), "Group"),
  "projection selected years"
)

# The two standalone reports must reproduce the main-report table contract for
# their exact retained subsets.  Recompute those tables here from the frozen
# inputs rather than trusting the values written by rr-sensitivity.R.
scope_groups <- c(
  `rr0-inclusion` = "RR=0: inclusion",
  `rr1-exclusion` = "RR=1: exclusion"
)
read_scope_table <- function(prefix, stem) {
  read.csv(
    file.path(rr_table_dir, paste0(prefix, "-", stem, ".csv")),
    check.names = FALSE
  )
}

indicator_labels <- c("Below the LRP", "Below SBMSY", "Above FMSY")
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
  ), stringsAsFactors = FALSE
)
terminal_series <- source_series[
  source_series$year == 2024L,
  c("ensemble_id", "spawning_potential", "sb_sbmsy"), drop = FALSE
]
names(terminal_series)[-1L] <- c("sb_latest_kt", "sb_latest_sbmsy")
structural_reference <- merge(
  source_management, terminal_series, by = "ensemble_id", sort = FALSE
)
structural_reference$sb_latest_sb0 <- with(
  structural_reference, sb_latest_kt / sb0_recent_kt
)
structural_reference$f_multiplier_at_msy <-
  1 / structural_reference$f_recent_fmsy
structural_reference$sbmsy_kt <- with(
  structural_reference, sb_recent_kt / sb_recent_sbmsy
)
structural_reference$sbmsy_sb0 <- with(
  structural_reference, sbmsy_kt / sb0_recent_kt
)

expected_scope_reference <- function(ids) {
  values <- structural_reference[
    structural_reference$ensemble_id %in% ids, , drop = FALSE
  ]
  statistics <- t(vapply(reference_specs$Column, function(column) {
    x <- values[[column]]
    c(
      Minimum = min(x), `10%` = quantiles(x, .10), Median = stats::median(x),
      Mean = mean(x), `90%` = quantiles(x, .90), Maximum = max(x)
    )
  }, numeric(6L)))
  cbind(
    reference_specs[c("Quantity", "Period", "Unit")],
    as.data.frame(statistics, check.names = FALSE)
  )
}

expected_scope_audit <- function(ids) {
  scope_hybrid <- hybrid_management[
    hybrid_management$ensemble_id %in% ids, , drop = FALSE
  ]
  scope_hybrid_50 <- scope_hybrid[scope_hybrid$draw <= 50L, , drop = FALSE]
  scope_pdh <- pdh_management[
    pdh_management$ensemble_id %in% ids, , drop = FALSE
  ]
  summarize <- function(x) c(
    q10 = quantiles(x, .10), median = stats::median(x), q90 = quantiles(x, .90)
  )
  do.call(rbind, lapply(seq_len(nrow(management_specs)), function(index) {
    column <- management_specs$column[[index]]
    all_100 <- summarize(scope_hybrid[[column]])
    all_50 <- summarize(scope_hybrid_50[[column]])
    pdh_only <- summarize(scope_pdh[[column]])
    data.frame(
      Quantity = management_specs$Quantity[[index]],
      All_q10 = all_100[["q10"]], All_median = all_100[["median"]],
      All_q90 = all_100[["q90"]], PDH_q10 = pdh_only[["q10"]],
      PDH_median = pdh_only[["median"]], PDH_q90 = pdh_only[["q90"]],
      Max_50_100_difference = max(abs(all_50 - all_100)),
      check.names = FALSE
    )
  }))
}

scope_cmm_values <- list()
for (prefix in names(scope_groups)) {
  group <- scope_groups[[prefix]]
  ids <- rr_group_ids[[group]]
  model_count <- length(ids)

  structural_rows <- expected_structural[
    expected_structural$Group == group,
    c("Quantity", "Period", "Models", "10%", "Median", "90%"),
    drop = FALSE
  ]
  compare_table(
    read_scope_table(prefix, "management-summary"), structural_rows,
    "Quantity", c("Models", "10%", "Median", "90%"),
    paste(prefix, "management summary")
  )

  risk_rows <- expected_structural_risk[
    expected_structural_risk$Group == group, , drop = FALSE
  ]
  structural_risk <- data.frame(
    Indicator = indicator_labels, Criterion = risk_rows$Criterion,
    Events = risk_rows$Events, Models = risk_rows$Models,
    Percent = 100 * risk_rows$Probability, check.names = FALSE
  )
  compare_table(
    read_scope_table(prefix, "management-risk"), structural_risk,
    "Criterion", c("Events", "Models", "Percent"),
    paste(prefix, "management risk")
  )

  management_values <- source_management[
    source_management$ensemble_id %in% ids, , drop = FALSE
  ]
  cmm_recent <- mean(management_values$recent_mean_depletion)
  cmm_historical <- mean(management_values$historical_target_depletion)
  cmm_ratio <- cmm_recent / cmm_historical
  scope_cmm_values[[prefix]] <- c(
    recent = cmm_recent, historical = cmm_historical, ratio = cmm_ratio
  )
  cmm_expected <- data.frame(
    Quantity = c(
      "Mean annual spawning depletion, 2021–2024",
      "Mean annual spawning depletion, 2012–2015",
      "Recent-to-2012–2015 spawning depletion ratio"
    ),
    Aggregation = c(
      paste0("Arithmetic mean across ", model_count, " central models"),
      paste0("Arithmetic mean across ", model_count, " central models"),
      "Ratio of the preceding two arithmetic means"
    ),
    Models = rep(model_count, 3L),
    Value = c(cmm_recent, cmm_historical, cmm_ratio),
    check.names = FALSE
  )
  compare_table(
    read_scope_table(prefix, "cmm-depletion-comparison"), cmm_expected,
    "Quantity", c("Models", "Value"), paste(prefix, "CMM comparison")
  )

  hessian_rows <- expected_hessian[
    expected_hessian$Group == group, , drop = FALSE
  ]
  hessian_intervals <- hessian_rows[c(
    "Quantity", "Period", "2.5%", "10%", "25%", "Median", "75%", "90%",
    "97.5%"
  )]
  compare_table(
    read_scope_table(prefix, "estimation-management-intervals"),
    hessian_intervals, "Quantity",
    c("2.5%", "10%", "25%", "Median", "75%", "90%", "97.5%"),
    paste(prefix, "estimation intervals")
  )
  hessian_summary <- data.frame(
    Quantity = hessian_rows$Quantity, Period = hessian_rows$Period,
    Median = round(hessian_rows$Median, 3),
    `50% interval` = sprintf(
      "%.3f–%.3f", hessian_rows[["25%"]], hessian_rows[["75%"]]
    ),
    `80% interval` = sprintf(
      "%.3f–%.3f", hessian_rows[["10%"]], hessian_rows[["90%"]]
    ),
    `95% interval` = sprintf(
      "%.3f–%.3f", hessian_rows[["2.5%"]], hessian_rows[["97.5%"]]
    ), check.names = FALSE
  )
  compare_table(
    read_scope_table(prefix, "estimation-management-summary"),
    hessian_summary, "Quantity", "Median", paste(prefix, "estimation summary")
  )
  hessian_risk_rows <- expected_hessian_risk[
    expected_hessian_risk$Group == group, c("Criterion", "Probability"),
    drop = FALSE
  ]
  compare_table(
    read_scope_table(prefix, "estimation-management-risk"),
    hessian_risk_rows, "Criterion", "Probability",
    paste(prefix, "estimation risk")
  )

  reference_expected <- expected_scope_reference(ids)
  compare_table(
    read_scope_table(prefix, "structural-reference-points"),
    reference_expected, "Quantity",
    c("Minimum", "10%", "Median", "Mean", "90%", "Maximum"),
    paste(prefix, "structural references")
  )
  compare_table(
    read_scope_table(prefix, "estimation-uncertainty-audit"),
    expected_scope_audit(ids), "Quantity",
    c(
      "All_q10", "All_median", "All_q90", "PDH_q10", "PDH_median",
      "PDH_q90", "Max_50_100_difference"
    ), paste(prefix, "estimation audit")
  )

  projection_rows <- expected_projection_years[
    expected_projection_years$Group == group, , drop = FALSE
  ]
  projection_display <- data.frame(
    Year = projection_rows$Year,
    `SBrecent/SBF=0 10%` = round(projection_rows$Depletion_q10, 3),
    `SBrecent/SBF=0 median` = round(projection_rows$Depletion_median, 3),
    `SBrecent/SBF=0 90%` = round(projection_rows$Depletion_q90, 3),
    `Simulation frequency below LRP` = sprintf(
      "%.1f%%", 100 * projection_rows$Probability_below_LRP
    ),
    `Spawning potential median (10^3 MT)` = round(
      projection_rows$Spawning_median_kt, 1
    ),
    `Catch/MSY 10%` = round(projection_rows$Catch_MSY_q10, 3),
    `Catch/MSY median` = round(projection_rows$Catch_MSY_median, 3),
    `Catch/MSY 90%` = round(projection_rows$Catch_MSY_q90, 3),
    check.names = FALSE
  )
  compare_table(
    read_scope_table(prefix, "projection-summary"), projection_display,
    "Year", setdiff(names(projection_display),
      c("Year", "Simulation frequency below LRP")),
    paste(prefix, "projection summary")
  )

  terminal_rows <- expected_projection_terminal[
    expected_projection_terminal$Group == group &
      expected_projection_terminal$Quantity != "SB2051–2054 / SBF=0",
    , drop = FALSE
  ]
  terminal_display <- data.frame(
    Quantity = terminal_rows$Quantity,
    `10%` = round(terminal_rows[["10%"]], 3),
    Median = round(terminal_rows$Median, 3),
    `90%` = round(terminal_rows[["90%"]], 3),
    Criterion = terminal_rows$Criterion,
    `Beyond criterion` = sprintf(
      "%.1f%%", 100 * terminal_rows$Beyond_criterion_probability
    ), check.names = FALSE
  )
  compare_table(
    read_scope_table(prefix, "projection-terminal-management"),
    terminal_display, "Quantity", c("10%", "Median", "90%"),
    paste(prefix, "projection terminal")
  )

  public_ids <- fit_output$ensemble_id[
    fit_output$source_ensemble_id %in% ids
  ]
  expected_fit_output <- fit_output[fit_output$ensemble_id %in% public_ids, , drop = FALSE]
  observed_fit_output <- read_scope_table(prefix, "ensemble-fit-diagnostics")
  rownames(expected_fit_output) <- NULL
  rownames(observed_fit_output) <- NULL
  if (!identical(observed_fit_output, expected_fit_output)) {
    stop(prefix, " fit diagnostics are not the exact canonical-ID subset.")
  }
  expected_fit_summary <- fit_summary[fit_summary$Model %in% public_ids, , drop = FALSE]
  observed_fit_summary <- read_scope_table(prefix, "fit-hessian-summary")
  rownames(expected_fit_summary) <- NULL
  rownames(observed_fit_summary) <- NULL
  if (!identical(observed_fit_summary, expected_fit_summary)) {
    stop(prefix, " fit/Hessian display is not the exact canonical-ID subset.")
  }
}

# The unprefixed RR tables bind the exact canonical Combined rows and the two
# independently checked scope tables, preserving the original Table 8–13
# schemas and row order for direct three-scope comparison.
grouped_wp06_stems <- c(
  "estimation-management-intervals", "estimation-management-summary",
  "cmm-depletion-comparison", "estimation-management-risk",
  "structural-reference-points", "estimation-uncertainty-audit",
  "projection-summary"
)
for (stem in grouped_wp06_stems) {
  pieces <- list(
    `Combined (RR=0 + RR=1)` = read.csv(
      file.path(output_dir, "tables", paste0(stem, ".csv")),
      check.names = FALSE
    ),
    `RR=0: inclusion` = read_scope_table("rr0-inclusion", stem),
    `RR=1: exclusion` = read_scope_table("rr1-exclusion", stem)
  )
  expected_grouped <- do.call(rbind, lapply(names(pieces), function(group) {
    data.frame(Group = group, pieces[[group]], check.names = FALSE)
  }))
  rownames(expected_grouped) <- NULL
  observed_grouped <- rr_tables[[paste0(stem, ".csv")]]
  if (!identical(names(observed_grouped), names(expected_grouped)) ||
      nrow(observed_grouped) != nrow(expected_grouped)) {
    stop("Grouped WP-06 table schema mismatch: ", stem, ".")
  }
  for (column in names(expected_grouped)) {
    if (is.numeric(expected_grouped[[column]]) ||
        is.integer(expected_grouped[[column]])) {
      assert_close(
        as.numeric(observed_grouped[[column]]),
        as.numeric(expected_grouped[[column]]), 5e-12,
        paste("grouped WP-06", stem, column)
      )
    } else if (!identical(
      as.character(observed_grouped[[column]]),
      as.character(expected_grouped[[column]])
    )) {
      stop("Grouped WP-06 table identity mismatch: ", stem, " / ", column, ".")
    }
  }
}

# Full-precision combined parity: the canonical 80-model tables must be the
# same calculation as the independently reconstructed Combined rows above.
combined_group <- "Combined (RR=0 + RR=1)"
combined_ids <- rr_group_ids[[combined_group]]
combined_structural <- expected_structural[
  expected_structural$Group == combined_group,
  c("Quantity", "Period", "Models", "10%", "Median", "90%"), drop = FALSE
]
compare_table(
  base_management, combined_structural, "Quantity",
  c("Models", "10%", "Median", "90%"), "canonical combined management"
)
combined_structural_risk_rows <- expected_structural_risk[
  expected_structural_risk$Group == combined_group, , drop = FALSE
]
combined_structural_risk <- data.frame(
  Indicator = indicator_labels,
  Criterion = combined_structural_risk_rows$Criterion,
  Events = combined_structural_risk_rows$Events,
  Models = combined_structural_risk_rows$Models,
  Percent = 100 * combined_structural_risk_rows$Probability,
  check.names = FALSE
)
compare_table(
  base_risk, combined_structural_risk, "Criterion",
  c("Events", "Models", "Percent"), "canonical combined structural risk"
)

combined_management_values <- source_management[
  source_management$ensemble_id %in% combined_ids, , drop = FALSE
]
combined_recent <- mean(combined_management_values$recent_mean_depletion)
combined_historical <- mean(
  combined_management_values$historical_target_depletion
)
combined_cmm <- data.frame(
  Quantity = c(
    "Mean annual spawning depletion, 2021–2024",
    "Mean annual spawning depletion, 2012–2015",
    "Recent-to-2012–2015 spawning depletion ratio"
  ),
  Aggregation = c(
    "Arithmetic mean across 80 central models",
    "Arithmetic mean across 80 central models",
    "Ratio of the preceding two arithmetic means"
  ),
  Models = rep(80L, 3L),
  Value = c(combined_recent, combined_historical,
            combined_recent / combined_historical),
  check.names = FALSE
)
compare_table(
  wp06_table9, combined_cmm, "Quantity", c("Models", "Value"),
  "canonical combined CMM"
)

combined_hessian_rows <- expected_hessian[
  expected_hessian$Group == combined_group, , drop = FALSE
]
combined_intervals <- combined_hessian_rows[c(
  "Quantity", "Period", "2.5%", "10%", "25%", "Median", "75%", "90%",
  "97.5%"
)]
compare_table(
  read.csv(
    file.path(output_dir, "tables", "estimation-management-intervals.csv"),
    check.names = FALSE
  ), combined_intervals, "Quantity",
  c("2.5%", "10%", "25%", "Median", "75%", "90%", "97.5%"),
  "canonical combined estimation intervals"
)
combined_hessian_risk <- expected_hessian_risk[
  expected_hessian_risk$Group == combined_group,
  c("Criterion", "Probability"), drop = FALSE
]
compare_table(
  wp06_table10, combined_hessian_risk, "Criterion", "Probability",
  "canonical combined estimation risk"
)
compare_table(
  wp06_table11, expected_scope_reference(combined_ids), "Quantity",
  c("Minimum", "10%", "Median", "Mean", "90%", "Maximum"),
  "canonical combined structural references"
)
compare_table(
  wp06_table12, expected_scope_audit(combined_ids), "Quantity",
  c(
    "All_q10", "All_median", "All_q90", "PDH_q10", "PDH_median",
    "PDH_q90", "Max_50_100_difference"
  ), "canonical combined estimation audit"
)

combined_projection_rows <- expected_projection_years[
  expected_projection_years$Group == combined_group, , drop = FALSE
]
combined_projection_display <- data.frame(
  Year = combined_projection_rows$Year,
  `SBrecent/SBF=0 10%` = round(combined_projection_rows$Depletion_q10, 3),
  `SBrecent/SBF=0 median` = round(
    combined_projection_rows$Depletion_median, 3
  ),
  `SBrecent/SBF=0 90%` = round(combined_projection_rows$Depletion_q90, 3),
  `Simulation frequency below LRP` = sprintf(
    "%.1f%%", 100 * combined_projection_rows$Probability_below_LRP
  ),
  `Spawning potential median (10^3 MT)` = round(
    combined_projection_rows$Spawning_median_kt, 1
  ),
  `Catch/MSY 10%` = round(combined_projection_rows$Catch_MSY_q10, 3),
  `Catch/MSY median` = round(combined_projection_rows$Catch_MSY_median, 3),
  `Catch/MSY 90%` = round(combined_projection_rows$Catch_MSY_q90, 3),
  check.names = FALSE
)
compare_table(
  wp06_table13, combined_projection_display, "Year",
  setdiff(
    names(combined_projection_display),
    c("Year", "Simulation frequency below LRP")
  ), "canonical combined projection summary"
)
combined_terminal_rows <- expected_projection_terminal[
  expected_projection_terminal$Group == combined_group &
    expected_projection_terminal$Quantity != "SB2051–2054 / SBF=0",
  , drop = FALSE
]
combined_terminal_display <- data.frame(
  Quantity = combined_terminal_rows$Quantity,
  `10%` = round(combined_terminal_rows[["10%"]], 3),
  Median = round(combined_terminal_rows$Median, 3),
  `90%` = round(combined_terminal_rows[["90%"]], 3),
  Criterion = combined_terminal_rows$Criterion,
  `Beyond criterion` = sprintf(
    "%.1f%%", 100 * combined_terminal_rows$Beyond_criterion_probability
  ), check.names = FALSE
)
compare_table(
  read.csv(
    file.path(output_dir, "tables", "projection-terminal-management.csv"),
    check.names = FALSE
  ), combined_terminal_display, "Quantity", c("10%", "Median", "90%"),
  "canonical combined projection terminal"
)

# Combined CMM is the ratio of the combined period means.  It must not be a
# 50/50 or retained-share weighted average of the two subgroup ratios.
scope_weights <- c(34, 46) / 80
combined_cmm_recent <- sum(scope_weights * vapply(
  scope_cmm_values, `[[`, numeric(1L), "recent"
))
combined_cmm_historical <- sum(scope_weights * vapply(
  scope_cmm_values, `[[`, numeric(1L), "historical"
))
combined_cmm_ratio <- combined_cmm_recent / combined_cmm_historical
retained_management <- source_management[
  source_management$ensemble_id %in% retained_source_ids, , drop = FALSE
]
if (abs(combined_cmm_recent -
      mean(retained_management$recent_mean_depletion)) > 5e-12 ||
    abs(combined_cmm_historical -
      mean(retained_management$historical_target_depletion)) > 5e-12 ||
    abs(combined_cmm_ratio - wp06_table9$Value[[3L]]) > 5e-12 ||
    abs(sum(scope_weights * vapply(
      scope_cmm_values, `[[`, numeric(1L), "ratio"
    )) - combined_cmm_ratio) < 1e-8) {
  stop("Combined CMM aggregation was reweighted or confused with subgroup ratios.")
}

differences <- rr_tables[["differences-vs-combined.csv"]]
if (any(abs(differences$Difference -
      (differences$Value - differences$Combined_value)) > 5e-12) ||
    any(abs(differences$Relative_difference -
      differences$Difference / differences$Combined_value) > 5e-12,
      na.rm = TRUE) ||
    !identical(sort(unique(differences$Group)),
      sort(c("RR=0: inclusion", "RR=1: exclusion")))) {
  stop("The RR difference table is not algebraically consistent with combined values.")
}

viewer_match <- regexec(
  '<script type="application/json" id="viewer-data">([\\s\\S]*?)</script>',
  viewer, perl = TRUE
)
viewer_parts <- regmatches(viewer, viewer_match)[[1L]]
if (length(viewer_parts) != 2L) stop("Cannot extract the viewer JSON payload.")
viewer_payload <- jsonlite::fromJSON(viewer_parts[[2L]], simplifyVector = FALSE)
viewer_counts <- vapply(viewer_payload$scopes, function(scope) {
  as.integer(scope$model_count)
}, integer(1L))
viewer_flags <- vapply(viewer_payload$models, function(model) {
  as.integer(model$reporting_flag2)
}, integer(1L))
if (!identical(viewer_counts, c(80L, 34L, 46L)) ||
    !identical(as.integer(table(factor(viewer_flags, levels = 0:1))), c(34L, 46L)) ||
    length(viewer_payload$models) != 80L || length(viewer_payload$series) != 80L) {
  stop("The viewer JSON does not preserve the exact 80/34/46 RR scopes.")
}

forbidden <- c(
  "/home/", "corp.spc.int", "ghp_", "github_pat_", "Job ",
  "Native MFCL", "native MFCL", "native-MFCL"
)
for (value in forbidden) {
  if (grepl(value, report, fixed = TRUE)) stop("Public report contains forbidden text: ", value)
  if (grepl(value, viewer, fixed = TRUE)) stop("Public viewer contains forbidden text: ", value)
  for (prefix in names(rr_scope_reports)) {
    if (grepl(value, rr_scope_reports[[prefix]], fixed = TRUE)) {
      stop(prefix, " report contains forbidden text: ", value)
    }
  }
}
if (grepl("<script[^>]+src=|<link[^>]+href=", viewer, ignore.case = TRUE, perl = TRUE)) {
  stop("The interactive viewer depends on an external script or stylesheet.")
}
if (grepl("<img[^>]+src=['\"](?!data:)", report, ignore.case = TRUE, perl = TRUE)) {
  stop("The report contains a non-embedded image.")
}

manifest <- read.csv(manifest_file, check.names = FALSE)
expected <- list.files(output_dir, recursive = TRUE, full.names = FALSE)
expected <- sort(expected[
  expected != "report-manifest.csv" &
    !grepl("[.]pre-rev1-root-owned$", expected)
])
if (!identical(manifest$file, expected) || anyDuplicated(manifest$file)) {
  stop("The final report manifest does not enumerate every output exactly once.")
}
actual <- vapply(file.path(output_dir, manifest$file), function(path) {
  output <- system2("sha256sum", path, stdout = TRUE)
  strsplit(output[[1L]], "[[:space:]]+")[[1L]][[1L]]
}, character(1L))
if (!identical(manifest$sha256, unname(actual))) {
  stop("A final report manifest checksum does not match.")
}

cat(paste0(
  "Validated the self-contained 80-model report and three-scope viewer, ",
  "two standalone subgroup reports, 11 main figure sets, 8 main tables, ",
  "23 RR figure sets and 39 RR tables.\n"
))
