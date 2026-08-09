options(stringsAsFactors = FALSE)

output_dir <- Sys.getenv("REPORT_OUTPUT_DIR", "results")
report_file <- file.path(output_dir, "bet-2026-ensemble-report.html")
viewer_file <- file.path(output_dir, "bet-2026-ensemble-interactive-viewer.html")
manifest_file <- file.path(output_dir, "report-manifest.csv")

required <- c(
  report_file, viewer_file, manifest_file,
  file.path(output_dir, "tables", c(
    "management-summary.csv", "management-risk.csv",
    "estimation-management-summary.csv", "estimation-management-intervals.csv",
    "estimation-management-risk.csv", "projection-summary.csv",
    "projection-terminal-management.csv", "fit-hessian-summary.csv",
    "structural-reference-points.csv", "estimation-uncertainty-audit.csv",
    "model-id-map.csv", "cmm-depletion-comparison.csv"
  ))
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

report <- paste(readLines(report_file, warn = FALSE), collapse = "\n")
viewer <- paste(readLines(viewer_file, warn = FALSE), collapse = "\n")
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
for (value in report_required) {
  if (!grepl(value, report, fixed = TRUE)) stop("Missing public-report element: ", value)
}
if (length(gregexpr("class='figure-card'", report, fixed = TRUE)[[1L]]) != 11L ||
    length(gregexpr("class='table-card'", report, fixed = TRUE)[[1L]]) != 8L) {
  stop("The report tabs do not contain exactly 11 unique figures and 8 unique tables.")
}

viewer_required <- c(
  "BET 2026 ensemble model results", "80 assessment configurations retained",
  "depletion", "recruitment", "spawning", "fishing",
  "Models &middot; ensemble-001&ndash;ensemble-080", "Select all", "Clear", "Fit summary",
  "Near-PDH", "F (year⁻¹)", "modelList", "fitTable",
  "<sub>recent</sub>", "<sub>MSY</sub>", "&tau;",
  "ensemble-001", "ensemble-080"
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

forbidden <- c(
  "/home/", "corp.spc.int", "ghp_", "github_pat_", "Job ",
  "Native MFCL", "native MFCL", "native-MFCL"
)
for (value in forbidden) {
  if (grepl(value, report, fixed = TRUE)) stop("Public report contains forbidden text: ", value)
  if (grepl(value, viewer, fixed = TRUE)) stop("Public viewer contains forbidden text: ", value)
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

cat("Validated the self-contained 80-model report, viewer, 11 figure sets and 8 copy-ready tables.\n")
