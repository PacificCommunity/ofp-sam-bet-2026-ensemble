# Export the two panels of the report's current-stock-status figure separately.
# Run from the repository root: Rscript extra/export-separate-status.R [output-directory]
# Plotting functions are loaded from the report script to keep their definitions
# and the HDR/category calculations identical to the combined figure.
args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) args[[1]] else "extra/figures"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

script <- readLines("report/augment-uncertainty-projection.R", warn = FALSE)
extract_section <- function(start, end) {
  first <- grep(start, script, fixed = TRUE)
  last <- grep(end, script, fixed = TRUE)
  if (length(first) != 1L || length(last) != 1L || first >= last) {
    stop("Could not locate the original plotting code; inspect ",
         "report/augment-uncertainty-projection.R before exporting.")
  }
  parse(text = script[first:(last - 1L)])
}
eval(extract_section("theme_report <- function", "summarise_by <- function"))

fit_all <- read.csv("data/ensemble/fit-diagnostics.csv", check.names = FALSE)
management_all <- read.csv("data/ensemble/management-quantities.csv", check.names = FALSE)
hessian <- readRDS("data/estimation/native-hessian-uncertainty.rds")
included_ids <- sort(fit_all$ensemble_id[fit_all$maximum_gradient <= 1e-4])
fit <- fit_all[fit_all$ensemble_id %in% included_ids, , drop = FALSE]
management <- management_all[management_all$ensemble_id %in% included_ids, , drop = FALSE]
included_pdh_ids <- intersect(hessian$pdh_model_ids, included_ids)
included_near_pdh_ids <- intersect(hessian$near_pdh_model_ids, included_ids)
if (length(included_ids) != 80L || length(included_pdh_ids) != 62L ||
    length(included_near_pdh_ids) != 18L) {
  stop("Expected 80 retained models: 62 PDH and 18 Near-PDH.")
}
# Reuse the exact mixture construction and panel functions from the report.
eval(extract_section("management_columns <- c(", "# The recent-to-2012"))
eval(extract_section("hdr_surface <- function", "current_status_plot <-"))

for (name in c("kobe", "majuro")) {
  plot <- get(paste0(name, "_combined_plot")) +
    ggplot2::theme(legend.box = "vertical")
  ggplot2::ggsave(
    file.path(output_dir, paste0(name, "-status.png")), plot,
    width = 5.4, height = 5.2, units = "in", dpi = 300, bg = "white",
    device = ragg::agg_png
  )
}
