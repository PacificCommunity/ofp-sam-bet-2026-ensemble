# Export each panel of the report's time-dynamic status figure separately.
# Run from the repository root: Rscript extra/export-separate-dynamic-status.R [output-directory]
args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) args[[1]] else "extra/figures"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

script <- readLines("report/augment-uncertainty-projection.R", warn = FALSE)
extract_section <- function(start, end) {
  first <- grep(start, script, fixed = TRUE)
  last <- grep(end, script, fixed = TRUE)
  if (length(first) != 1L || length(last) != 1L || first >= last) {
    stop("Could not locate original plotting code; inspect ",
         "report/augment-uncertainty-projection.R before exporting.")
  }
  parse(text = script[first:(last - 1L)])
}
eval(extract_section("theme_report <- function", "summarise_by <- function"))
fit <- read.csv("data/ensemble/fit-diagnostics.csv", check.names = FALSE)
included_ids <- sort(fit$ensemble_id[fit$maximum_gradient <= 1e-4])
if (length(included_ids) != 80L) stop("Expected 80 retained models.")
series_all <- readRDS("data/ensemble/ensemble-timeseries.rds")
series <- series_all[series_all$ensemble_id %in% included_ids, , drop = FALSE]
diagnostic_dynamic <- read.csv("data/diagnostic/dynamic-status.csv", check.names = FALSE)
# Reuse the report's validation, median calculation, panel functions and shared axes.
eval(extract_section("if (!identical(names(diagnostic_dynamic)", "dynamic_status_plot <-"))

for (diagram in c("majuro", "kobe")) {
  x_upper <- if (diagram == "majuro") dynamic_majuro_x_upper else dynamic_kobe_x_upper
  for (model in c("diagnostic", "median")) {
    data <- if (model == "diagnostic") diagnostic_dynamic else ensemble_dynamic
    title <- if (model == "diagnostic") "Diagnostic model" else "80-model median"
    plot <- dynamic_status_panel(data, diagram, x_upper, dynamic_y_upper,
                                 panel_title = title)
    ggplot2::ggsave(
      file.path(output_dir, paste0("dynamic-", diagram, "-", model, ".png")),
      plot, width = 5.4, height = 5.0, units = "in", dpi = 300,
      bg = "white", device = ragg::agg_png
    )
  }
}
