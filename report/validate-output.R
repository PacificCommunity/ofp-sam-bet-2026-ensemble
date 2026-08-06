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
    "projection-terminal-management.csv", "projection-steepness-audit.csv",
    "projection-fishery-conditioning.csv", "fit-hessian-summary.csv",
    "structural-reference-points.csv", "estimation-uncertainty-audit.csv",
    "status-category-probabilities.csv",
    "projection-fishery-quarter-conditioning.csv",
    "projection-zero-quarter-audit.csv"
  ))
)
if (any(!file.exists(required))) stop("The rendered public report is incomplete.")

pngs <- list.files(file.path(output_dir, "figures"), pattern = "[.]png$", full.names = TRUE)
pdfs <- list.files(file.path(output_dir, "figures"), pattern = "[.]pdf$", full.names = TRUE)
if (length(pngs) != 14L || length(pdfs) != 14L) {
  stop("Expected 14 publication figure sets in both PNG and vector PDF formats.")
}
if (any(file.info(c(pngs, pdfs))$size < 10000L)) {
  stop("A rendered report figure is unexpectedly small.")
}

report <- paste(readLines(report_file, warn = FALSE), collapse = "\n")
viewer <- paste(readLines(viewer_file, warn = FALSE), collapse = "\n")
report_required <- c(
  "BET 2026 ensemble analysis",
  "Overview", "Interval convention", "50%", "80%", "95%",
  "Open 80-model interactive viewer", "Fishing mortality",
  "Projected depletion and Catch/MSY", "highest-density regions",
  "Management quantities with available estimation uncertainty", "Terminal management quantities",
  "Supporting structural reference points", "Monte Carlo audit",
  "Kobe and Majuro category probabilities",
  "Zero-quarter conditioning audit",
  "Regional spawning potential", "Scope and limitations",
  "Copy table for Word", "Copy LaTeX", "Open vector PDF",
  "Ten did not meet the MGC", "extended optimization runs"
)
for (value in report_required) {
  if (!grepl(value, report, fixed = TRUE)) stop("Missing public-report element: ", value)
}

viewer_required <- c(
  "BET 2026 ensemble model results", "80 assessment configurations retained",
  "depletion", "recruitment", "spawning", "fishing",
  "Models &middot; 80 included", "Select all", "Clear", "Fit summary",
  "Near-PDH", "F (year⁻¹)", "modelList", "fitTable",
  "<sub>recent</sub>", "<sub>MSY</sub>", "&tau;"
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
source_fit <- read.csv("data/ensemble/fit-diagnostics.csv", check.names = FALSE)
included_ids <- source_fit$ensemble_id[source_fit$maximum_gradient <= 1e-4]
excluded_ids <- source_fit$ensemble_id[source_fit$maximum_gradient > 1e-4]
if (nrow(fit_output) != 80L || nrow(fit_summary) != 80L ||
    !setequal(fit_output$ensemble_id, included_ids)) {
  stop("The rendered fit tables are not restricted to the 80 MGC-filtered models.")
}
for (id in excluded_ids) {
  if (grepl(id, report, fixed = TRUE) || grepl(id, viewer, fixed = TRUE)) {
    stop("An excluded model identifier appears in a public output.")
  }
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
expected <- sort(expected[expected != "report-manifest.csv"])
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

cat("Validated the self-contained 80-model report, viewer, 14 figure sets and copy-ready tables.\n")
