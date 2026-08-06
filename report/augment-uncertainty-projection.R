options(stringsAsFactors = FALSE)
set.seed(20260806)

required_packages <- c("ggplot2", "patchwork", "ragg", "scales", "jsonlite", "MASS")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install report dependencies: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}
source("report/model-labels.R")

output_dir <- Sys.getenv("REPORT_OUTPUT_DIR", "results")
figure_dir <- file.path(output_dir, "figures")
table_dir <- file.path(output_dir, "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
obsolete_projection_figures <- file.path(
  figure_dir,
  c(
    "projection-depletion-catch-msy.png",
    "projection-depletion-catch-msy.pdf",
    "projection-catch-msy.png",
    "projection-catch-msy.pdf",
    "projection-spawning-risk.png",
    "projection-spawning-risk.pdf",
    "projected-regional-depletion.png",
    "projected-regional-depletion.pdf",
    "regional-spawning-biomass-regions-1-3.png",
    "regional-spawning-biomass-regions-1-3.pdf",
    "regional-spawning-biomass-regions-4-5-stock.png",
    "regional-spawning-biomass-regions-4-5-stock.pdf"
  )
)
invisible(file.remove(
  obsolete_projection_figures[file.exists(obsolete_projection_figures)]
))

series_all <- readRDS("data/ensemble/ensemble-timeseries.rds")
fit_all <- read.csv("data/ensemble/fit-diagnostics.csv", check.names = FALSE)
management_all <- read.csv("data/ensemble/management-quantities.csv", check.names = FALSE)
design_all <- read.csv("data/ensemble/successful-model-design.csv", check.names = FALSE)
diagnostic_dynamic <- read.csv(
  "data/diagnostic/dynamic-status.csv", check.names = FALSE
)
hessian <- readRDS("data/estimation/native-hessian-uncertainty.rds")
projection_all <- readRDS("data/projection/native-projections.rds")
quarterly_conditioning <- read.csv(
  "data/projection/fishery-quarter-conditioning.csv", check.names = FALSE
)
source("report/management-quantities.R")

included_ids <- sort(fit_all$ensemble_id[fit_all$maximum_gradient <= 1e-4])
excluded_ids <- sort(fit_all$ensemble_id[fit_all$maximum_gradient > 1e-4])
if (length(included_ids) != 80L || length(excluded_ids) != 8L) {
  stop("The locked MGC <= 1e-4 rule must retain 80 of 88 completed models.")
}
model_id_map <- sequential_model_map(included_ids)
fit <- fit_all[fit_all$ensemble_id %in% included_ids, , drop = FALSE]
series <- series_all[series_all$ensemble_id %in% included_ids, , drop = FALSE]
management <- management_all[
  management_all$ensemble_id %in% included_ids, , drop = FALSE
]
design <- design_all[design_all$ensemble_id %in% included_ids, , drop = FALSE]

included_pdh_ids <- intersect(hessian$pdh_model_ids, included_ids)
included_near_pdh_ids <- intersect(hessian$near_pdh_model_ids, included_ids)
if (length(included_pdh_ids) != 62L || length(included_near_pdh_ids) != 18L) {
  stop("Expected 62 PDH and 18 Near-PDH fits after MGC filtering.")
}

projection <- projection_all
for (name in names(projection)) {
  value <- projection[[name]]
  if (is.data.frame(value) && "ensemble_id" %in% names(value)) {
    projection[[name]] <- value[value$ensemble_id %in% included_ids, , drop = FALSE]
  }
}
projection$ensemble_ids <- included_ids
projection$projection_complete_models <- length(included_ids)

theme_report <- function(base_size = 10.8) {
  ggplot2::theme_bw(base_size = base_size, base_family = "serif") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#E2E8EB", linewidth = 0.28),
      panel.border = ggplot2::element_rect(colour = "#263844", fill = NA, linewidth = 0.45),
      axis.title = ggplot2::element_text(face = "bold", colour = "#183246"),
      axis.text = ggplot2::element_text(colour = "#36566A"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      legend.key.width = grid::unit(1.1, "cm"),
      plot.tag = ggplot2::element_text(face = "bold", colour = "#183246", size = 12),
      plot.margin = ggplot2::margin(7, 9, 7, 8)
    )
}

summarise_by <- function(data, value, groups = "year") {
  key <- interaction(data[groups], drop = TRUE, lex.order = TRUE)
  pieces <- split(seq_len(nrow(data)), key)
  out <- do.call(rbind, lapply(pieces, function(index) {
    values <- data[[value]][index]
    row <- data[index[[1L]], groups, drop = FALSE]
    row$q025 <- stats::quantile(values, 0.025, names = FALSE, na.rm = TRUE)
    row$q05 <- stats::quantile(values, 0.05, names = FALSE, na.rm = TRUE)
    row$q10 <- stats::quantile(values, 0.10, names = FALSE, na.rm = TRUE)
    row$q25 <- stats::quantile(values, 0.25, names = FALSE, na.rm = TRUE)
    row$median <- stats::median(values, na.rm = TRUE)
    row$q75 <- stats::quantile(values, 0.75, names = FALSE, na.rm = TRUE)
    row$q90 <- stats::quantile(values, 0.90, names = FALSE, na.rm = TRUE)
    row$q95 <- stats::quantile(values, 0.95, names = FALSE, na.rm = TRUE)
    row$q975 <- stats::quantile(values, 0.975, names = FALSE, na.rm = TRUE)
    row
  }))
  rownames(out) <- NULL
  out[do.call(order, out[groups]), , drop = FALSE]
}

save_plot <- function(plot, stem, width = 7.1, height = 7.8) {
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

image_uri <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  raw <- readBin(con, what = "raw", n = file.info(path)$size)
  paste0("data:image/png;base64,", jsonlite::base64_enc(raw))
}

html_escape <- function(value) {
  value <- gsub("&", "&amp;", as.character(value), fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  gsub(">", "&gt;", value, fixed = TRUE)
}

replace_math_tokens <- function(value) {
  replacements <- c(
    "F2050–2053" = "<i>F</i><sub>2050–2053</sub>",
    "SB2051–2054" = "<i>SB</i><sub>2051–2054</sub>",
    "SBt/SBF=0,t" = "<i>SB</i><sub><i>t</i></sub>/<i>SB</i><sub><i>F</i>=0,<i>t</i></sub>",
    "SBt/SBMSY,t" = "<i>SB</i><sub><i>t</i></sub>/<i>SB</i><sub>MSY,<i>t</i></sub>",
    "Ft/FMSY,t" = "<i>F</i><sub><i>t</i></sub>/<i>F</i><sub>MSY,<i>t</i></sub>",
    "SBrecent" = "<i>SB</i><sub>recent</sub>",
    "SBlatest" = "<i>SB</i><sub>latest</sub>",
    "SBF=0" = "<i>SB</i><sub><i>F</i>=0</sub>",
    "SBMSY" = "<i>SB</i><sub>MSY</sub>",
    "Frecent" = "<i>F</i><sub>recent</sub>",
    "F/FMSY" = "<i>F</i>/<i>F</i><sub>MSY</sub>",
    "FMSY" = "<i>F</i><sub>MSY</sub>",
    "Drecent" = "<i>D</i><sub>recent</sub>"
  )
  for (target in names(replacements)) {
    value <- gsub(target, replacements[[target]], value, fixed = TRUE)
  }
  value
}

html_math <- function(value) replace_math_tokens(html_escape(value))
caption_math <- function(value) replace_math_tokens(value)

latex_escape <- function(value) {
  value <- as.character(value)
  value <- gsub("–", "--", value, fixed = TRUE)
  replacements <- c(
    "&" = "\\&", "%" = "\\%", "$" = "\\$", "#" = "\\#",
    "_" = "\\_", "{" = "\\{", "}" = "\\}"
  )
  for (target in names(replacements)) {
    value <- gsub(target, replacements[[target]], value, fixed = TRUE)
  }
  value <- gsub("≤", "$\\leq$", value, fixed = TRUE)
  value <- gsub("≥", "$\\geq$", value, fixed = TRUE)
  value <- gsub("×", "$\\times$", value, fixed = TRUE)
  value <- gsub("τ", "$\\tau$", value, fixed = TRUE)
  value <- gsub("−", "$-$", value, fixed = TRUE)
  value
}

html_table <- function(data) {
  heads <- paste0("<th>", html_math(names(data)), "</th>", collapse = "")
  rows <- apply(data, 1, function(row) {
    paste0("<tr>", paste0("<td>", html_math(row), "</td>", collapse = ""), "</tr>")
  })
  paste0("<table><thead><tr>", heads, "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table>")
}

word_table <- function(caption, data) {
  paste(c(caption, paste(names(data), collapse = "\t"), apply(data, 1, paste, collapse = "\t")), collapse = "\n")
}

copy_table_block <- function(id, title, caption, display, latex) {
  paste0(
    "<section class='table-card' id='table-", id, "'><h2>", title,
    "</h2><p><strong>Table XX.</strong> ", caption_math(caption), "</p>",
    "<div class='actions'><button onclick=\"copyText('word-", id, "',this)\">Copy table for Word</button>",
    "<button onclick=\"copyText('latex-", id, "',this)\">Copy LaTeX</button>",
    "<a href='tables/", id, ".csv'>Download CSV</a></div>", html_table(display),
    "<textarea id='word-", id, "' class='copy-source'>", html_escape(word_table(caption, display)), "</textarea>",
    "<textarea id='latex-", id, "' class='copy-source'>", html_escape(latex), "</textarea></section>"
  )
}

figure_block <- function(id, title, caption, latex_caption, files) {
  latex <- paste0(
    "% Requires \\usepackage{graphicx}\n",
    "\\begin{figure}[htbp]\n\\centering\n",
    "\\includegraphics[width=\\textwidth]{figures/", basename(files[["pdf"]]), "}\n",
    "\\caption{", latex_caption, "}\n\\end{figure}"
  )
  paste0(
    "<section class='figure-card' id='", id, "'><h2>", title, "</h2>",
    "<img src='", image_uri(files[["png"]]), "' alt='", html_escape(title), "'>",
    "<p class='caption'><strong>Figure XX.</strong> ", caption_math(caption), "</p>",
    "<div class='actions'><button onclick=\"copyText('cap-", id, "',this)\">Copy caption</button>",
    "<a download='", basename(files[["png"]]), "' href='", image_uri(files[["png"]]), "'>Save PNG</a>",
    "<a href='figures/", basename(files[["pdf"]]), "'>Open vector PDF</a>",
    "<button onclick=\"copyText('tex-", id, "',this)\">Copy figure for LaTeX</button></div>",
    "<textarea id='cap-", id, "' class='copy-source'>", html_escape(gsub("<[^>]+>", "", caption)), "</textarea>",
    "<textarea id='tex-", id, "' class='copy-source'>", html_escape(latex), "</textarea></section>"
  )
}

# Give every assessment model equal weight. PDH models contribute 100 correlated
# Hessian draws; each Near-PDH central estimate is repeated 100 times.
pdh_annual <- hessian$annual_draws[, c(
  "ensemble_id", "draw", "year", "depletion", "spawning_potential", "recruitment"
)]
pdh_annual <- pdh_annual[
  pdh_annual$ensemble_id %in% included_pdh_ids, , drop = FALSE
]
near_annual <- merge(
  series[series$ensemble_id %in% included_near_pdh_ids,
         c("ensemble_id", "year", "depletion", "spawning_potential", "recruitment")],
  data.frame(draw = seq_len(hessian$draws_per_pdh_model)), by = NULL
)
hybrid_annual <- rbind(pdh_annual, near_annual[names(pdh_annual)])

management_columns <- c(
  "sb_recent_sb0", "sb_recent_sbmsy", "f_recent_fmsy",
  "historical_target_depletion", "recent_historical_target_ratio"
)
if (!all(management_columns %in% names(hessian$management_draws))) {
  stop("The exact estimation-inclusive management quantities are unavailable.")
}
pdh_management <- hessian$management_draws[, c(
  "ensemble_id", "draw", management_columns
)]
pdh_management <- pdh_management[
  pdh_management$ensemble_id %in% included_pdh_ids, , drop = FALSE
]
near_management <- merge(
  management[management$ensemble_id %in% included_near_pdh_ids,
             c("ensemble_id", management_columns)],
  data.frame(draw = seq_len(hessian$draws_per_pdh_model)), by = NULL
)
hybrid_management <- rbind(pdh_management, near_management[names(pdh_management)])

management_uncertainty_values <- lapply(
  management_columns, function(column) hybrid_management[[column]]
)
management_uncertainty_numeric <- data.frame(
  Quantity = c(
    "SBrecent / SBF=0", "SBrecent / SBMSY", "Frecent / FMSY",
    "Mean depletion, 2012–2015",
    "Recent / 2012–2015 depletion"
  ),
  Period = c(
    "2021–2024 / 2014–2023", "2021–2024 / equilibrium SBMSY",
    "2020–2023 / equilibrium FMSY", "2012–2015",
    "Recent / 2012–2015"
  ),
  `2.5%` = vapply(management_uncertainty_values, stats::quantile, numeric(1), probs = 0.025, names = FALSE),
  `10%` = vapply(management_uncertainty_values, stats::quantile, numeric(1), probs = 0.10, names = FALSE),
  `25%` = vapply(management_uncertainty_values, stats::quantile, numeric(1), probs = 0.25, names = FALSE),
  Median = vapply(management_uncertainty_values, stats::median, numeric(1)),
  `75%` = vapply(management_uncertainty_values, stats::quantile, numeric(1), probs = 0.75, names = FALSE),
  `90%` = vapply(management_uncertainty_values, stats::quantile, numeric(1), probs = 0.90, names = FALSE),
  `97.5%` = vapply(management_uncertainty_values, stats::quantile, numeric(1), probs = 0.975, names = FALSE),
  check.names = FALSE
)
write.csv(
  management_uncertainty_numeric,
  file.path(table_dir, "estimation-management-intervals.csv"), row.names = FALSE
)
format_interval <- function(lower, upper) sprintf("%.3f–%.3f", lower, upper)
management_uncertainty_display <- data.frame(
  Quantity = management_uncertainty_numeric$Quantity,
  Period = management_uncertainty_numeric$Period,
  Median = sprintf("%.3f", management_uncertainty_numeric$Median),
  `50% interval` = format_interval(
    management_uncertainty_numeric$`25%`, management_uncertainty_numeric$`75%`
  ),
  `80% interval` = format_interval(
    management_uncertainty_numeric$`10%`, management_uncertainty_numeric$`90%`
  ),
  `95% interval` = format_interval(
    management_uncertainty_numeric$`2.5%`, management_uncertainty_numeric$`97.5%`
  ),
  check.names = FALSE
)
write.csv(
  management_uncertainty_display,
  file.path(table_dir, "estimation-management-summary.csv"), row.names = FALSE
)
management_uncertainty_caption <- paste0(
  "Management quantities from the equal-model-weight structural mixture with available Hessian estimation uncertainty. ",
  "Each of 62 PDH models contributes 100 joint Hessian draws; each of 18 Near-PDH models contributes its central estimate with equal total model weight. ",
  "Values are the median and central 50%, 80% and 95% equal-tailed intervals; the 80% interval is primary. Estimation uncertainty is unavailable for the Near-PDH fits."
)
management_latex_quantity <- c(
  "$SB_{\\mathrm{recent}}/SB_{F=0}$",
  "$SB_{\\mathrm{recent}}/SB_{MSY}$",
  "$F_{\\mathrm{recent}}/F_{MSY}$",
  "$\\overline{D}_{2012--2015}$",
  "$D_{\\mathrm{recent}}/\\overline{D}_{2012--2015}$"
)
management_uncertainty_rows <- vapply(seq_len(nrow(management_uncertainty_display)), function(i) {
  remainder <- vapply(management_uncertainty_display[i, -1L], latex_escape, character(1))
  paste0(paste(c(management_latex_quantity[[i]], remainder), collapse = " & "), " \\\\")
}, character(1))
management_uncertainty_latex <- paste0(
  "% Requires \\usepackage{booktabs,tabularx}\n",
  "\\begin{table}[htbp]\n\\centering\n\\caption{", latex_escape(management_uncertainty_caption), "}\n",
  "\\small\n\\begin{tabularx}{\\textwidth}{@{}lXrrrr@{}}\n",
  "\\toprule\nQuantity & Period & Median & 50\\% interval & 80\\% interval & 95\\% interval \\\\\n\\midrule\n",
  paste(management_uncertainty_rows, collapse = "\n"),
  "\n\\bottomrule\n\\end{tabularx}\n\\end{table}"
)

management_risk_numeric <- data.frame(
  Criterion = c(
    "SBrecent/SBF=0 < 0.20", "SBrecent/SBMSY < 1",
    "Frecent/FMSY > 1", "Recent depletion below 2012–2015 objective"
  ),
  Probability = c(
    mean(hybrid_management$sb_recent_sb0 < 0.20),
    mean(hybrid_management$sb_recent_sbmsy < 1),
    mean(hybrid_management$f_recent_fmsy > 1),
    mean(hybrid_management$recent_historical_target_ratio < 1)
  ),
  check.names = FALSE
)
write.csv(
  management_risk_numeric,
  file.path(table_dir, "estimation-management-risk.csv"), row.names = FALSE
)
management_risk_display <- management_risk_numeric
management_risk_display$Probability <- scales::percent(
  management_risk_display$Probability, accuracy = 0.1
)
management_risk_caption <- paste0(
  "Status probabilities and the model-specific 2012–2015 depletion objective. ",
  "They use the same equal-model-weight mixture as the management summary: Hessian estimation uncertainty is included for the 62 retained PDH models, while the 18 retained Near-PDH models are represented by point estimates."
)
management_risk_latex_criterion <- c(
  "$SB_{\\mathrm{recent}}/SB_{F=0}<0.20$",
  "$SB_{\\mathrm{recent}}/SB_{\\mathrm{MSY}}<1$",
  "$F_{\\mathrm{recent}}/F_{\\mathrm{MSY}}>1$",
  "$D_{\\mathrm{recent}}<\\overline{D}_{2012--2015}$"
)
management_risk_rows <- vapply(seq_len(nrow(management_risk_display)), function(i) {
  paste0(
    management_risk_latex_criterion[[i]], " & ",
    latex_escape(management_risk_display$Probability[[i]]),
    " \\\\"
  )
}, character(1))
management_risk_latex <- paste0(
  "% Requires \\usepackage{booktabs,tabularx}\n",
  "\\begin{table}[htbp]\n\\centering\n\\caption{",
  latex_escape(management_risk_caption), "}\n",
  "\\begin{tabularx}{\\textwidth}{@{}Xr@{}}\n\\toprule\nCriterion & Probability \\\\\n\\midrule\n",
  paste(management_risk_rows, collapse = "\n"),
  "\n\\bottomrule\n\\end{tabularx}\n\\end{table}"
)

# Supporting structural reference points. Only quantities that reproduce from
# the committed public central estimates are included; absolute MSY, FMSY,
# SB0, YFrecent and latest catch are not inferred from unavailable fields.
terminal_series <- series[series$year == 2024L, c(
  "ensemble_id", "spawning_potential", "sb_sbmsy"
)]
names(terminal_series)[-1L] <- c("sb_latest_kt", "sb_latest_sbmsy")
structural_reference <- merge(
  management, terminal_series, by = "ensemble_id", sort = FALSE
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
if (max(abs(
  structural_reference$f_multiplier_at_msy *
    structural_reference$f_recent_fmsy - 1
)) > 1e-12) {
  stop("The per-model F multiplier does not invert Frecent/FMSY.")
}
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
  Unit = c("multiplier", "ratio", "thousand MT", rep("ratio", 4L),
           "thousand MT", "ratio"),
  Column = c(
    "f_multiplier_at_msy", "f_recent_fmsy", "sb0_recent_kt",
    "sb_latest_sb0", "sb_latest_sbmsy", "sb_recent_sb0",
    "sb_recent_sbmsy", "sbmsy_kt", "sbmsy_sb0"
  ),
  stringsAsFactors = FALSE
)
reference_statistics <- t(vapply(reference_specs$Column, function(column) {
  value <- structural_reference[[column]]
  c(
    Minimum = min(value), `10%` = stats::quantile(value, 0.10, names = FALSE),
    Median = stats::median(value), Mean = mean(value),
    `90%` = stats::quantile(value, 0.90, names = FALSE), Maximum = max(value)
  )
}, numeric(6L)))
if (any(
  reference_statistics[, "Minimum"] > reference_statistics[, "10%"] |
    reference_statistics[, "10%"] > reference_statistics[, "Median"] |
    reference_statistics[, "Median"] > reference_statistics[, "90%"] |
    reference_statistics[, "90%"] > reference_statistics[, "Maximum"]
)) {
  stop("A structural reference-point summary is not monotone.")
}
structural_reference_numeric <- cbind(
  reference_specs[c("Quantity", "Period", "Unit")],
  as.data.frame(reference_statistics, check.names = FALSE)
)
write.csv(
  structural_reference_numeric,
  file.path(table_dir, "structural-reference-points.csv"), row.names = FALSE
)
structural_reference_display <- structural_reference_numeric
for (column in colnames(reference_statistics)) {
  structural_reference_display[[column]] <- vapply(
    seq_len(nrow(structural_reference_display)), function(index) {
      digits <- if (structural_reference_display$Unit[[index]] == "thousand MT") 1L else 3L
      sprintf(paste0("%.", digits, "f"), structural_reference_numeric[[column]][[index]])
    }, character(1L)
  )
}
structural_reference_caption <- paste0(
  "Supporting reference-point quantities across the 80 retained central model estimates, with equal structural weight. ",
  "All ratios and reference points are calculated within each model before summarizing; periods and units are listed in the table. ",
  "Intervals describe structural uncertainty only. Quantities without propagated estimation uncertainty are omitted."
)
structural_reference_latex_quantity <- c(
  "$F_{\\mathrm{mult}}$", "$F_{\\mathrm{recent}}/F_{\\mathrm{MSY}}$",
  "$SB_{F=0}$", "$SB_{\\mathrm{latest}}/SB_{F=0}$",
  "$SB_{\\mathrm{latest}}/SB_{\\mathrm{MSY}}$",
  "$SB_{\\mathrm{recent}}/SB_{F=0}$",
  "$SB_{\\mathrm{recent}}/SB_{\\mathrm{MSY}}$", "$SB_{\\mathrm{MSY}}$",
  "$SB_{\\mathrm{MSY}}/SB_{F=0}$"
)
structural_reference_rows <- vapply(
  seq_len(nrow(structural_reference_display)), function(index) {
    remainder <- vapply(
      structural_reference_display[index, -1L], latex_escape, character(1)
    )
    paste0(
      paste(c(structural_reference_latex_quantity[[index]], remainder),
            collapse = " & "),
      " \\\\"
    )
  }, character(1L)
)
structural_reference_latex <- paste0(
  "% Requires \\usepackage{booktabs,tabularx}\n",
  "\\begin{table}[htbp]\n\\centering\n\\caption{",
  latex_escape(structural_reference_caption), "}\n",
  "\\scriptsize\n\\setlength{\\tabcolsep}{2.5pt}\n",
  "\\begin{tabularx}{\\textwidth}{@{}XXXrrrrrr@{}}\n",
  "\\toprule\nQuantity & Period & Unit & Min & 10\\% & Median & Mean & 90\\% & Max \\\\\n\\midrule\n",
  paste(structural_reference_rows, collapse = "\n"),
  "\n\\bottomrule\n\\end{tabularx}\n\\end{table}"
)

# Audit the 100-draw choice against the first 50 deterministic draws, and show
# the impact of excluding the 18 Near-PDH point masses. This is a Monte Carlo
# stability diagnostic rather than a proof of convergence.
core_management_columns <- management_columns[1:3]
near_management_50 <- near_management[near_management$draw <= 50L, ]
pdh_management_50 <- pdh_management[pdh_management$draw <= 50L, ]
hybrid_management_50 <- rbind(
  pdh_management_50, near_management_50[names(pdh_management_50)]
)
uncertainty_audit_numeric <- do.call(rbind, lapply(
  seq_along(core_management_columns), function(index) {
    column <- core_management_columns[[index]]
    summarize <- function(value) c(
      q10 = stats::quantile(value, 0.10, names = FALSE),
      median = stats::median(value),
      q90 = stats::quantile(value, 0.90, names = FALSE)
    )
    all_100 <- summarize(hybrid_management[[column]])
    all_50 <- summarize(hybrid_management_50[[column]])
    pdh_only <- summarize(pdh_management[[column]])
    data.frame(
      Quantity = c("SBrecent / SBF=0", "SBrecent / SBMSY", "Frecent / FMSY")[[index]],
      All_q10 = all_100[["q10"]], All_median = all_100[["median"]],
      All_q90 = all_100[["q90"]],
      PDH_q10 = pdh_only[["q10"]], PDH_median = pdh_only[["median"]],
      PDH_q90 = pdh_only[["q90"]],
      Max_50_100_difference = max(abs(all_50 - all_100)),
      check.names = FALSE
    )
  }
))
write.csv(
  uncertainty_audit_numeric,
  file.path(table_dir, "estimation-uncertainty-audit.csv"), row.names = FALSE
)
uncertainty_audit_display <- data.frame(
  Quantity = uncertainty_audit_numeric$Quantity,
  `All models: median (80% interval)` = sprintf(
    "%.3f (%.3f–%.3f)", uncertainty_audit_numeric$All_median,
    uncertainty_audit_numeric$All_q10, uncertainty_audit_numeric$All_q90
  ),
  `PDH only: median (80% interval)` = sprintf(
    "%.3f (%.3f–%.3f)", uncertainty_audit_numeric$PDH_median,
    uncertainty_audit_numeric$PDH_q10, uncertainty_audit_numeric$PDH_q90
  ),
  `Max |50-draw − 100-draw quantile|` = sprintf(
    "%.4f", uncertainty_audit_numeric$Max_50_100_difference
  ),
  check.names = FALSE
)
uncertainty_audit_caption <- paste0(
  "Estimation-uncertainty sensitivity and Monte Carlo audit for the three core status quantities. ",
  "The all-model column gives 62 retained PDH models with 100 joint draws plus 18 retained Near-PDH point masses at equal model weight; the PDH-only column excludes the point masses. ",
  "The final column is the largest absolute change in q10, median or q90 when the first 50 rather than all 100 draws per model are used, with matching Near-PDH point-mass replication."
)
uncertainty_audit_rows <- vapply(
  seq_len(nrow(uncertainty_audit_display)), function(index) {
    paste0(
      paste(vapply(
        uncertainty_audit_display[index, ], latex_escape, character(1)
      ), collapse = " & "),
      " \\\\"
    )
  }, character(1L)
)
uncertainty_audit_latex <- paste0(
  "% Requires \\usepackage{booktabs,tabularx}\n",
  "\\begin{table}[htbp]\n\\centering\n\\caption{",
  latex_escape(uncertainty_audit_caption), "}\n",
  "\\small\n\\begin{tabularx}{\\textwidth}{@{}Xrrr@{}}\n",
  "\\toprule\nQuantity & All models & PDH only & Max 50--100 difference \\\\\n\\midrule\n",
  paste(uncertainty_audit_rows, collapse = "\n"),
  "\n\\bottomrule\n\\end{tabularx}\n\\end{table}"
)

uncertainty_specs <- list(
  list(column = "depletion", label = expression(italic(SB)[italic(t)] / italic(SB)[italic(F) == 0]), lrp = TRUE, estimation = TRUE),
  list(column = "spawning_potential", label = expression(Spawning~potential~(10^3~plain(MT))), lrp = FALSE, estimation = TRUE),
  list(column = "recruitment", label = "Recruitment (millions of fish)", lrp = FALSE, estimation = TRUE),
  list(column = "fishing_mortality", label = expression(italic(F)~(year^{-1})), lrp = FALSE, estimation = FALSE)
)
uncertainty_panels <- lapply(uncertainty_specs, function(spec) {
  structural <- summarise_by(series, spec$column)
  combined <- if (spec$estimation) {
    summarise_by(hybrid_annual, spec$column)
  } else {
    structural
  }
  p <- ggplot2::ggplot(combined, ggplot2::aes(x = .data$year)) +
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
      data = series,
      ggplot2::aes(
        x = .data$year, y = .data[[spec$column]],
        group = .data$ensemble_id
      ),
      inherit.aes = FALSE, colour = "#6F7F87", linewidth = 0.18,
      alpha = 0.10
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$median, colour = "All-model median"), linewidth = 0.88
    ) +
    ggplot2::scale_fill_manual(values = c(
      "95% interval" = "#D5E9ED",
      "80% interval" = "#9CCFD8",
      "50% interval" = "#53AAB9"
    ), name = NULL) +
    ggplot2::scale_colour_manual(values = c(
      "All-model median" = "#07566B", "Structural median" = "#D27A1D"
    ), breaks = c("All-model median", "Structural median"), name = NULL) +
    ggplot2::scale_x_continuous(breaks = seq(1960, 2020, 20)) +
    ggplot2::coord_cartesian(ylim = c(0, NA)) +
    ggplot2::labs(x = "Year", y = spec$label) + theme_report(10.6)
  p <- p + ggplot2::geom_line(
    data = structural,
    ggplot2::aes(y = .data$median, colour = "Structural median"),
    linewidth = 0.60, linetype = "22"
  )
  if (!spec$estimation) {
    p <- p + ggplot2::guides(fill = "none", colour = "none")
  }
  if (spec$lrp) {
    p <- p +
      ggplot2::geom_hline(yintercept = 0.2, colour = "#B83232", linewidth = 0.55, linetype = "33") +
      ggplot2::annotate("text", x = 1955, y = 0.215, label = "LRP", colour = "#B83232", hjust = 0, size = 3.0, fontface = "bold")
  }
  p
})
uncertainty_plot <- (
  uncertainty_panels[[1L]] | uncertainty_panels[[2L]]
) / (
  uncertainty_panels[[3L]] | uncertainty_panels[[4L]]
) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(tag_levels = "a") &
  ggplot2::theme(
    legend.position = "bottom", legend.box = "vertical",
    legend.text = ggplot2::element_text(size = 8.4),
    legend.key.width = grid::unit(0.72, "cm"),
    axis.title.y = ggplot2::element_text(face = "plain"),
    plot.tag.position = c(0.075, 0.955),
    plot.tag = ggplot2::element_text(
      face = "bold", colour = "#183246", size = 11,
      hjust = 0, vjust = 1
    )
  )
uncertainty_files <- save_plot(
  uncertainty_plot, "combined-structural-estimation-uncertainty", height = 6.7
)

# One-dimensional summaries above are equal-tailed intervals. Status diagrams
# instead use bivariate kernel highest-density regions so the covariance
# produced by each joint Hessian draw remains visible.
hdr_surface <- function(data, x, y, probabilities = c(0.95, 0.80, 0.50), n = 220L) {
  x_values <- data[[x]]
  y_values <- data[[y]]
  keep <- is.finite(x_values) & is.finite(y_values)
  x_values <- x_values[keep]
  y_values <- y_values[keep]
  if (length(x_values) < 20L || stats::sd(x_values) <= 0 || stats::sd(y_values) <= 0) {
    stop("Insufficient variation for a two-dimensional HDR.")
  }
  x_bandwidth <- MASS::bandwidth.nrd(x_values)
  y_bandwidth <- MASS::bandwidth.nrd(y_values)
  if (!is.finite(x_bandwidth) || x_bandwidth <= 0 ||
      !is.finite(y_bandwidth) || y_bandwidth <= 0) {
    stop("Invalid normal-reference bandwidth for a two-dimensional HDR.")
  }
  # Four bandwidths beyond the sample extrema retain effectively all of the
  # Gaussian-kernel tail mass used to define the HDR probability content.
  x_margin <- max(diff(range(x_values)) * 0.12, 4 * x_bandwidth)
  y_margin <- max(diff(range(y_values)) * 0.12, 4 * y_bandwidth)
  estimate <- MASS::kde2d(
    x_values, y_values, n = n, h = c(x_bandwidth, y_bandwidth),
    lims = c(
      min(x_values) - x_margin, max(x_values) + x_margin,
      min(y_values) - y_margin, max(y_values) + y_margin
    )
  )
  density <- as.vector(estimate$z)
  ordered <- order(density, decreasing = TRUE)
  cumulative_mass <- cumsum(density[ordered]) / sum(density)
  thresholds <- vapply(probabilities, function(probability) {
    density[ordered][which(cumulative_mass >= probability)[[1L]]]
  }, numeric(1))
  thresholds <- sort(unique(thresholds))
  if (length(thresholds) != length(probabilities)) {
    stop("HDR density thresholds are not distinct.")
  }
  surface <- expand.grid(x = estimate$x, y = estimate$y)
  surface$density <- density
  attr(surface, "breaks") <- c(
    thresholds, max(density) + .Machine$double.eps * max(density)
  )
  surface
}

central_status <- Reduce(
  function(x, y) merge(x, y, by = "ensemble_id", sort = FALSE),
  list(
    management,
    fit[c("ensemble_id", "maximum_gradient", "positive_definite_hessian")]
  )
)
central_status$hessian_status <- ifelse(
  central_status$positive_definite_hessian, "PDH", "Near-PDH"
)
central_status$point_alpha <- ifelse(
  central_status$maximum_gradient <= 1e-4, 0.90, 0.46
)
hdr_colours <- c("#D4EDF1", "#8BC9D3", "#2F93A5")

kobe_category <- function(sb, fishing) {
  ifelse(
    sb >= 1 & fishing <= 1, "Green",
    ifelse(
      sb < 1 & fishing <= 1, "Yellow",
      ifelse(sb >= 1 & fishing > 1, "Orange", "Red")
    )
  )
}
majuro_category <- function(depletion, fishing) {
  ifelse(
    depletion < 0.20, "Red",
    ifelse(fishing <= 1, "Green", "Orange")
  )
}
category_probability <- function(value, levels) {
  table_value <- table(factor(value, levels = levels))
  as.numeric(table_value) / sum(table_value)
}
kobe_levels <- c("Green", "Yellow", "Orange", "Red")
majuro_levels <- c("Green", "Orange", "Red")

status_panel <- function(draws, central, x_column, x_boundary, x_label,
                         diagram = c("kobe", "majuro")) {
  diagram <- match.arg(diagram)
  surface <- hdr_surface(draws, x_column, "f_recent_fmsy")
  # Keep the main HDR and every central model visible without allowing a few
  # extreme first-order draws to compress the status diagram.
  x_upper <- max(
    stats::quantile(draws[[x_column]], 0.995, na.rm = TRUE),
    central[[x_column]], na.rm = TRUE
  ) * 1.04
  y_upper <- max(
    stats::quantile(draws$f_recent_fmsy, 0.995, na.rm = TRUE),
    central$f_recent_fmsy, na.rm = TRUE
  ) * 1.04
  category <- if (diagram == "kobe") {
    kobe_category(draws[[x_column]], draws$f_recent_fmsy)
  } else {
    majuro_category(draws[[x_column]], draws$f_recent_fmsy)
  }
  category_levels <- if (diagram == "kobe") kobe_levels else majuro_levels
  probability <- category_probability(category, category_levels)
  if (diagram == "kobe") {
    probability_labels <- data.frame(
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
    probability_labels <- data.frame(
      x = c(mean(c(x_boundary, x_upper)), mean(c(x_boundary, x_upper)), x_boundary * 0.50),
      y = c(min(0.48, y_upper * 0.24), mean(c(1, y_upper)), y_upper * 0.50),
      label = scales::percent(probability, accuracy = 0.1)
    )
  }
  background <- if (diagram == "kobe") {
    list(
      ggplot2::annotate(
        "rect", xmin = x_boundary, xmax = Inf, ymin = -Inf, ymax = 1,
        fill = "#2a9d8f", alpha = 0.72
      ),
      ggplot2::annotate(
        "rect", xmin = -Inf, xmax = x_boundary, ymin = -Inf, ymax = 1,
        fill = "#e9c46a", alpha = 0.72
      ),
      ggplot2::annotate(
        "rect", xmin = x_boundary, xmax = Inf, ymin = 1, ymax = Inf,
        fill = "#f4a261", alpha = 0.70
      ),
      ggplot2::annotate(
        "rect", xmin = -Inf, xmax = x_boundary, ymin = 1, ymax = Inf,
        fill = "#e76f51", alpha = 0.72
      ),
      ggplot2::geom_hline(
        yintercept = 1, colour = "#1f2937", linewidth = 0.48
      ),
      ggplot2::geom_vline(
        xintercept = x_boundary, colour = "#1f2937", linewidth = 0.48
      )
    )
  } else {
    list(
      ggplot2::annotate(
        "rect", xmin = -Inf, xmax = x_boundary, ymin = -Inf, ymax = Inf,
        fill = "#e76f51", alpha = 0.72
      ),
      ggplot2::annotate(
        "rect", xmin = x_boundary, xmax = Inf, ymin = -Inf, ymax = 1,
        fill = "#2a9d8f", alpha = 0.72
      ),
      ggplot2::annotate(
        "rect", xmin = x_boundary, xmax = Inf, ymin = 1, ymax = Inf,
        fill = "#f4a261", alpha = 0.70
      ),
      ggplot2::annotate(
        "segment", x = x_boundary, xend = x_upper, y = 1, yend = 1,
        colour = "#1f2937", linewidth = 0.48
      ),
      ggplot2::geom_vline(
        xintercept = x_boundary, colour = "#1f2937", linewidth = 0.48
      )
    )
  }
  ggplot2::ggplot() +
    background +
    ggplot2::geom_contour_filled(
      data = surface,
      ggplot2::aes(x = .data$x, y = .data$y, z = .data$density),
      breaks = attr(surface, "breaks"), alpha = 0.82,
      colour = "#285F6B", linewidth = 0.42
    ) +
    ggplot2::geom_point(
      data = central,
      ggplot2::aes(
        x = .data[[x_column]], y = .data$f_recent_fmsy,
        shape = .data$hessian_status, alpha = .data$point_alpha
      ), colour = "#0B6477", size = 2.25, stroke = 0.75
    ) +
    ggplot2::geom_label(
      data = probability_labels,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      inherit.aes = FALSE, colour = "#173042", fill = "white",
      alpha = 0.82, linewidth = 0.15, size = 3.05,
      fontface = "bold", label.padding = grid::unit(0.12, "lines")
    ) +
    ggplot2::scale_fill_manual(
      values = hdr_colours, labels = c("95% HDR", "80% HDR", "50% HDR"),
      name = NULL
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        nrow = 1, byrow = TRUE, order = 1
      ),
      shape = ggplot2::guide_legend(nrow = 1, byrow = TRUE, order = 2)
    ) +
    ggplot2::scale_shape_manual(
      values = c("PDH" = 16, "Near-PDH" = 1),
      breaks = c("PDH", "Near-PDH"),
      labels = c("PDH Hessian", "Near-PDH Hessian"), name = NULL
    ) +
    ggplot2::scale_alpha_identity() +
    ggplot2::coord_cartesian(
      xlim = c(0, x_upper), ylim = c(0, y_upper), expand = FALSE
    ) +
    ggplot2::labs(
      x = x_label,
      y = expression(italic(F)[recent] / italic(F)[MSY])
    ) +
    theme_report(11.6) +
    ggplot2::theme(
      legend.position = "bottom", legend.box = "horizontal",
      legend.text = ggplot2::element_text(size = 8.8),
      legend.key.width = grid::unit(0.66, "cm"),
      legend.spacing.x = grid::unit(0.14, "cm"),
      plot.margin = ggplot2::margin(3, 4, 3, 4)
    )
}

kobe_combined_plot <- status_panel(
  hybrid_management, central_status, "sb_recent_sbmsy", 1,
  expression(italic(SB)[recent] / italic(SB)[MSY]), "kobe"
)
majuro_combined_plot <- status_panel(
  hybrid_management, central_status, "sb_recent_sb0", 0.20,
  expression(italic(SB)[recent] / italic(SB)[italic(F) == 0]), "majuro"
)
current_status_plot <- (kobe_combined_plot | majuro_combined_plot) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(tag_levels = "a") &
  ggplot2::theme(legend.position = "bottom", legend.box = "horizontal")
current_status_files <- save_plot(
  current_status_plot, "combined-kobe-majuro-status", height = 4.1
)

if (!identical(names(diagnostic_dynamic), c(
  "year", "depletion", "sb_sbmsy", "f_fmsy"
)) || nrow(diagnostic_dynamic) != 73L ||
    !identical(as.integer(diagnostic_dynamic$year), 1952:2024) ||
    any(!is.finite(unlist(diagnostic_dynamic)))) {
  stop("The diagnostic dynamic-status series is invalid.")
}
ensemble_dynamic <- stats::aggregate(
  cbind(depletion, sb_sbmsy, f_fmsy) ~ year,
  series, stats::median
)
diagnostic_dynamic$source <- "Diagnostic model"
ensemble_dynamic$source <- "80-model median"

dynamic_status_background <- function(x_boundary, x_upper, y_upper,
                                      diagram = c("kobe", "majuro")) {
  diagram <- match.arg(diagram)
  if (diagram == "kobe") {
    list(
      ggplot2::annotate("rect", xmin = 1, xmax = x_upper, ymin = 0, ymax = 1, fill = "#2a9d8f", alpha = 0.72),
      ggplot2::annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "#e9c46a", alpha = 0.72),
      ggplot2::annotate("rect", xmin = 1, xmax = x_upper, ymin = 1, ymax = y_upper, fill = "#f4a261", alpha = 0.70),
      ggplot2::annotate("rect", xmin = 0, xmax = 1, ymin = 1, ymax = y_upper, fill = "#e76f51", alpha = 0.72),
      ggplot2::geom_hline(yintercept = 1, colour = "#1f2937", linewidth = 0.48),
      ggplot2::geom_vline(xintercept = 1, colour = "#1f2937", linewidth = 0.48)
    )
  } else {
    list(
      ggplot2::annotate("rect", xmin = 0, xmax = 0.2, ymin = 0, ymax = y_upper, fill = "#e76f51", alpha = 0.72),
      ggplot2::annotate("rect", xmin = 0.2, xmax = x_upper, ymin = 0, ymax = 1, fill = "#2a9d8f", alpha = 0.72),
      ggplot2::annotate("rect", xmin = 0.2, xmax = x_upper, ymin = 1, ymax = y_upper, fill = "#f4a261", alpha = 0.70),
      ggplot2::annotate("segment", x = 0.2, xend = x_upper, y = 1, yend = 1, colour = "#1f2937", linewidth = 0.48),
      ggplot2::geom_vline(xintercept = 0.2, colour = "#1f2937", linewidth = 0.48)
    )
  }
}

dynamic_status_panel <- function(data, diagram = c("kobe", "majuro"),
                                 x_upper, y_upper, show_year_legend = TRUE,
                                 panel_title = NULL) {
  diagram <- match.arg(diagram)
  x_column <- if (diagram == "kobe") "sb_sbmsy" else "depletion"
  x_boundary <- if (diagram == "kobe") 1 else 0.2
  x_label <- if (diagram == "kobe") {
    expression(italic(SB)[italic(t)] / italic(SB)[MSY * "," * italic(t)])
  } else {
    expression(italic(SB)[italic(t)] / italic(SB)[italic(F) == 0 * "," * italic(t)])
  }
  terminal <- data[data$year == max(data$year), ]
  ggplot2::ggplot() +
    dynamic_status_background(x_boundary, x_upper, y_upper, diagram) +
    ggplot2::geom_path(
      data = data,
      ggplot2::aes(
        x = .data[[x_column]], y = .data$f_fmsy,
        colour = .data$year, group = 1
      ), linewidth = 0.78, alpha = 0.86
    ) +
    ggplot2::geom_point(
      data = data[data$year %% 4L == 0L | data$year %in% c(1952L, 2024L), ],
      ggplot2::aes(
        x = .data[[x_column]], y = .data$f_fmsy, fill = .data$year
      ), shape = 21, size = 1.75, stroke = 0.24, colour = "white"
    ) +
    ggplot2::geom_point(
      data = terminal,
      ggplot2::aes(
        x = .data[[x_column]], y = .data$f_fmsy, fill = .data$year
      ), shape = 21, size = 3.2, stroke = 0.70, colour = "#102A3A"
    ) +
    ggplot2::scale_colour_viridis_c(
      option = "C", begin = 0.12, end = 0.95, direction = -1,
      name = "Year", guide = if (show_year_legend) "colourbar" else "none"
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "C", begin = 0.12, end = 0.95, direction = -1,
      name = "Year", guide = "none"
    ) +
    ggplot2::coord_cartesian(
      xlim = c(0, x_upper), ylim = c(0, y_upper), expand = FALSE
    ) +
    ggplot2::labs(
      title = panel_title,
      x = x_label,
      y = expression(italic(F)[italic(t)] / italic(F)[MSY * "," * italic(t)])
    ) + theme_report(10.3) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.key.width = grid::unit(1.6, "cm"),
      plot.title = ggplot2::element_text(
        hjust = 0.5, face = "bold", size = 10.5, margin = ggplot2::margin(b = 4)
      ),
      plot.margin = ggplot2::margin(6, 7, 6, 7)
    )
}

dynamic_y_upper <- max(
  diagnostic_dynamic$f_fmsy, ensemble_dynamic$f_fmsy, 1
) * 1.06
dynamic_majuro_x_upper <- max(
  diagnostic_dynamic$depletion, ensemble_dynamic$depletion, 0.2
) * 1.02
dynamic_kobe_x_upper <- max(
  diagnostic_dynamic$sb_sbmsy, ensemble_dynamic$sb_sbmsy, 1
) * 1.03
dynamic_status_plot <- (
  dynamic_status_panel(
    diagnostic_dynamic, "majuro", dynamic_majuro_x_upper, dynamic_y_upper,
    panel_title = "Diagnostic model"
  ) |
  dynamic_status_panel(
    ensemble_dynamic, "majuro", dynamic_majuro_x_upper, dynamic_y_upper,
    panel_title = "80-model median"
  )
) / (
  dynamic_status_panel(
    diagnostic_dynamic, "kobe", dynamic_kobe_x_upper, dynamic_y_upper
  ) |
  dynamic_status_panel(
    ensemble_dynamic, "kobe", dynamic_kobe_x_upper, dynamic_y_upper
  )
) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(tag_levels = "a") &
  ggplot2::theme(
    legend.position = "bottom",
    plot.tag.position = c(0.015, 0.985),
    plot.tag = ggplot2::element_text(
      face = "bold", colour = "#183246", size = 11,
      hjust = 0, vjust = 1
    )
  )
dynamic_status_files <- save_plot(
  dynamic_status_plot, "time-dynamic-kobe-majuro", height = 6.8
)

# Descriptive marginal distributions by uncertainty axis. All axes vary
# jointly in the ensemble, so these panels are summaries rather than causal
# one-factor contrasts.
axis_draws <- merge(
  hybrid_management,
  design[c(
    "ensemble_id", "steepness", "m_age40_quarterly", "tag_tau",
    "tag_mixing_k_cutoff", "tag_reporting", "effort_creep_primary",
    "effort_creep_secondary"
  )], by = "ensemble_id", sort = FALSE
)
axis_draws$steepness_group <- cut(
  axis_draws$steepness,
  breaks = unique(stats::quantile(axis_draws$steepness, seq(0, 1, 0.25))),
  include.lowest = TRUE, dig.lab = 3
)
axis_draws$m_group <- cut(
  axis_draws$m_age40_quarterly,
  breaks = unique(stats::quantile(axis_draws$m_age40_quarterly, seq(0, 1, 0.25))),
  include.lowest = TRUE, dig.lab = 3
)
axis_draws$effort_group <- paste0(
  format(100 * axis_draws$effort_creep_primary, trim = TRUE), "/",
  format(100 * axis_draws$effort_creep_secondary, trim = TRUE), "%"
)

axis_long <- function(data, axes) {
  pieces <- lapply(names(axes), function(axis_name) {
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
  })
  do.call(rbind, pieces)
}

axis_violin_plot <- function(long, ncol = 2L) {
  long$Axis <- factor(long$Axis, levels = unique(long$Axis))
  long$Quantity <- factor(
    long$Quantity, levels = c("Frecent / FMSY", "SBrecent / SBF=0")
  )
  ggplot2::ggplot(long, ggplot2::aes(x = .data$Category, y = .data$Value)) +
    ggplot2::geom_violin(
      fill = "#65AFE4", colour = "#264E63", alpha = 0.72,
      linewidth = 0.42, trim = TRUE, scale = "width"
    ) +
    ggplot2::geom_boxplot(
      width = 0.15, outlier.shape = NA, fill = "white", alpha = 0.76,
      colour = "#173B4D", linewidth = 0.38
    ) +
    ggplot2::facet_grid(
      Quantity ~ Axis, scales = "free",
      labeller = ggplot2::labeller(
        Quantity = ggplot2::as_labeller(c(
          "Frecent / FMSY" = "italic(F)[recent] / italic(F)[MSY]",
          "SBrecent / SBF=0" = "italic(SB)[recent] / italic(SB)[italic(F)==0]"
        ), ggplot2::label_parsed),
        Axis = ggplot2::label_value
      )
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_report(10.4) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 25, hjust = 1, size = 8.5),
      strip.background = ggplot2::element_rect(fill = "#E4ECEF", colour = "#607985"),
      strip.text = ggplot2::element_text(face = "bold", size = 9.5)
    )
}

continuous_axis_long <- axis_long(axis_draws, c(
  "Steepness quartile" = "steepness_group",
  "Natural mortality quartile" = "m_group"
))
discrete_axis_long_a <- axis_long(axis_draws, c(
  "Tag overdispersion" = "tag_tau",
  "Mixing cutoff" = "tag_mixing_k_cutoff"
))
discrete_axis_long_b <- axis_long(axis_draws, c(
  "Tag reporting" = "tag_reporting",
  "Effort creep (primary/secondary)" = "effort_group"
))
continuous_axis_files <- save_plot(
  axis_violin_plot(continuous_axis_long),
  "management-uncertainty-continuous-axes", height = 6.9
)
discrete_axis_a_files <- save_plot(
  axis_violin_plot(discrete_axis_long_a),
  "management-uncertainty-discrete-axes-a", height = 6.9
)
discrete_axis_b_files <- save_plot(
  axis_violin_plot(discrete_axis_long_b),
  "management-uncertainty-discrete-axes-b", height = 6.9
)

# Each assessment model and each of its ten projected recruitment sequences
# receives equal weight.
projection_management <- build_projection_management(series, projection)
proj_depletion <- summarise_by(
  projection_management$projected, "sb_recent_sb0"
)
proj_spawning <- projection$annual_stock
proj_spawning$spawning_potential_kt <- proj_spawning$spawning_biomass_mt / 1000
proj_spawning_summary <- summarise_by(proj_spawning, "spawning_potential_kt")
historical_depletion <- summarise_by(
  projection_management$historical, "sb_recent_sb0"
)
historical_spawning <- summarise_by(series, "spawning_potential")
proj_risk <- stats::aggregate(
  cbind(below_lrp_020, below_historical_objective) ~ year,
  data = projection_management$projected, FUN = mean
)
names(proj_risk)[2:3] <- c(
  "probability_below_lrp", "probability_below_historical_objective"
)

terminal_depletion_rank <- projection_management$projected[
  projection_management$projected$year == max(projection$projection_years),
  c("ensemble_id", "simulation", "sb_recent_sb0")
]
terminal_depletion_rank <- terminal_depletion_rank[
  order(terminal_depletion_rank$sb_recent_sb0),
]
set.seed(20260806)
representative_positions <- sort(sample.int(
  nrow(terminal_depletion_rank), size = 10L, replace = FALSE
))
representative_keys <- terminal_depletion_rank[
  representative_positions, c("ensemble_id", "simulation")
]
representative_keys$trajectory_id <- paste(
  representative_keys$ensemble_id, representative_keys$simulation, sep = " / "
)

representative_projection_depletion <- merge(
  representative_keys,
  projection_management$projected[c(
    "ensemble_id", "simulation", "year", "sb_recent_sb0"
  )],
  by = c("ensemble_id", "simulation"), sort = FALSE
)
names(representative_projection_depletion)[
  names(representative_projection_depletion) == "sb_recent_sb0"
] <- "value"
representative_historical_depletion <- merge(
  representative_keys[c("ensemble_id", "trajectory_id")],
  projection_management$historical[c("ensemble_id", "year", "sb_recent_sb0")],
  by = "ensemble_id", sort = FALSE
)
names(representative_historical_depletion)[
  names(representative_historical_depletion) == "sb_recent_sb0"
] <- "value"

representative_projection_spawning <- merge(
  representative_keys,
  projection$annual_stock[c(
    "ensemble_id", "simulation", "year", "spawning_biomass_mt"
  )],
  by = c("ensemble_id", "simulation"), sort = FALSE
)
representative_projection_spawning$value <-
  representative_projection_spawning$spawning_biomass_mt / 1000
representative_historical_spawning <- merge(
  representative_keys[c("ensemble_id", "trajectory_id")],
  series[c("ensemble_id", "year", "spawning_potential")],
  by = "ensemble_id", sort = FALSE
)
representative_historical_spawning$value <-
  representative_historical_spawning$spawning_potential

projection_panel <- function(
    historical, projected, y_label, lrp = FALSE,
    historical_trajectories = NULL, projected_trajectories = NULL) {
  transition <- data.frame(
    x = max(historical$year),
    xend = min(projected$year),
    y = historical$median[which.max(historical$year)],
    yend = projected$median[which.min(projected$year)]
  )
  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = historical,
      ggplot2::aes(
        x = .data$year, ymin = .data$q025, ymax = .data$q975,
        fill = "Historical intervals"
      ), alpha = 0.16
    ) +
    ggplot2::geom_ribbon(
      data = historical,
      ggplot2::aes(
        x = .data$year, ymin = .data$q10, ymax = .data$q90,
        fill = "Historical intervals"
      ), alpha = 0.30
    ) +
    ggplot2::geom_ribbon(
      data = historical,
      ggplot2::aes(
        x = .data$year, ymin = .data$q25, ymax = .data$q75,
        fill = "Historical intervals"
      ), alpha = 0.56
    ) +
    ggplot2::geom_ribbon(
      data = projected,
      ggplot2::aes(
        x = .data$year, ymin = .data$q025, ymax = .data$q975,
        fill = "Projection intervals"
      ), alpha = 0.24
    ) +
    ggplot2::geom_ribbon(
      data = projected,
      ggplot2::aes(
        x = .data$year, ymin = .data$q10, ymax = .data$q90,
        fill = "Projection intervals"
      ), alpha = 0.48
    ) +
    ggplot2::geom_ribbon(
      data = projected,
      ggplot2::aes(
        x = .data$year, ymin = .data$q25, ymax = .data$q75,
        fill = "Projection intervals"
      ), alpha = 0.70
    )
  if (!is.null(historical_trajectories) && !is.null(projected_trajectories)) {
    trajectory_transition <- merge(
      historical_trajectories[
        historical_trajectories$year == max(historical_trajectories$year),
        c("trajectory_id", "year", "value")
      ],
      projected_trajectories[
        projected_trajectories$year == min(projected_trajectories$year),
        c("trajectory_id", "year", "value")
      ],
      by = "trajectory_id", suffixes = c("_historical", "_projected")
    )
    p <- p +
      ggplot2::geom_line(
        data = historical_trajectories,
        ggplot2::aes(
          x = .data$year, y = .data$value, group = .data$trajectory_id
        ), colour = "#687A83", linewidth = 0.26, alpha = 0.30
      ) +
      ggplot2::geom_segment(
        data = trajectory_transition,
        ggplot2::aes(
          x = .data$year_historical, xend = .data$year_projected,
          y = .data$value_historical, yend = .data$value_projected,
          group = .data$trajectory_id
        ), colour = "#2B8C9B", linewidth = 0.30, alpha = 0.40
      ) +
      ggplot2::geom_line(
        data = projected_trajectories,
        ggplot2::aes(
          x = .data$year, y = .data$value, group = .data$trajectory_id
        ), colour = "#2B8C9B", linewidth = 0.31, alpha = 0.44
      )
  }
  p <- p +
    ggplot2::geom_line(
      data = historical,
      ggplot2::aes(
        x = .data$year, y = .data$median,
        colour = "Historical structural median"
      ), linewidth = 0.72
    ) +
    ggplot2::geom_segment(
      data = transition,
      ggplot2::aes(
        x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend
      ), colour = "#07566B", linewidth = 0.90
    ) +
    ggplot2::geom_line(
      data = projected,
      ggplot2::aes(
        x = .data$year, y = .data$median,
        colour = "Projection median"
      ), linewidth = 0.90
    ) +
    ggplot2::geom_vline(
      xintercept = 2024.5, colour = "#5D6C73", linewidth = 0.48,
      linetype = "33"
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Historical intervals" = "#AEBCC2",
        "Projection intervals" = "#62B3C1"
      ), name = NULL
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        "Historical structural median" = "#4F626C",
        "Projection median" = "#07566B"
      ), name = NULL, guide = "none"
    ) +
    ggplot2::guides(fill = "none") +
    ggplot2::scale_x_continuous(breaks = c(1960, 1980, 2000, 2024, 2054)) +
    ggplot2::coord_cartesian(ylim = c(0, NA)) +
    ggplot2::labs(x = "Year", y = y_label) + theme_report(10.8)
  if (lrp) {
    p <- p + ggplot2::geom_hline(yintercept = 0.2, colour = "#B83232", linewidth = 0.55, linetype = "33") +
      ggplot2::annotate(
        "text", x = min(historical$year) + 2, y = 0.212, label = "LRP",
        colour = "#B83232", hjust = 0, size = 3.0, fontface = "bold"
      )
  }
  p
}

p_proj_depletion <- projection_panel(
  historical_depletion, proj_depletion,
  expression(italic(SB)[recent] / italic(SB)[italic(F) == 0]), TRUE,
  representative_historical_depletion, representative_projection_depletion
)
p_proj_spawning <- projection_panel(
  historical_spawning, proj_spawning_summary,
  expression(Spawning~potential~(10^3~plain(MT))), FALSE,
  representative_historical_spawning, representative_projection_spawning
)
p_proj_risk <- ggplot2::ggplot(proj_risk, ggplot2::aes(x = .data$year)) +
  ggplot2::geom_line(
    ggplot2::aes(
      y = .data$probability_below_lrp,
      colour = "Below LRP"
    ), linewidth = 0.92
  ) +
  ggplot2::geom_line(
    ggplot2::aes(
      y = .data$probability_below_historical_objective,
      colour = "Below 2012–2015 objective"
    ), linewidth = 0.92
  ) +
  ggplot2::scale_colour_manual(
    values = c(
      "Below LRP" = "#A62929",
      "Below 2012–2015 objective" = "#D0791E"
    ), name = NULL
  ) +
  ggplot2::scale_x_continuous(breaks = seq(2030, 2050, 10)) +
  ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, 1)) +
  ggplot2::labs(x = "Year", y = "Probability below threshold") + theme_report(10.8)

catch_msy <- projection$catch_msy
catch_msy_summary <- summarise_by(catch_msy, "catch_msy")

terminal <- projection$terminal_msy
# Fmults reports equilibrium quantities for the configured recent fishing
# fishing-mortality configuration, but its biomass field ends one year too early for the
# terminal-biomass convention. Recalculate the numerator from the cached
# annual paths as mean SB over 2051--2054 and retain the matching equilibrium
# SBMSY denominator and recent-F ratio for each simulation.
terminal_recent_sb <- stats::aggregate(
  spawning_biomass_mt ~ ensemble_id + simulation,
  data = projection$annual_stock[
    projection$annual_stock$year %in% 2051:2054,
  ],
  FUN = mean
)
names(terminal_recent_sb)[names(terminal_recent_sb) == "spawning_biomass_mt"] <-
  "sb_recent_2051_2054_recomputed_mt"
terminal <- merge(
  terminal, terminal_recent_sb,
  by = c("ensemble_id", "simulation"), sort = FALSE
)
if ("sb_recent_2051_2054_mt" %in% names(terminal) &&
    max(abs(
      terminal$sb_recent_2051_2054_mt -
        terminal$sb_recent_2051_2054_recomputed_mt
    )) > 1e-8) {
  stop("Cached and recomputed terminal spawning biomass differ.")
}
terminal$sb_recent_2051_2054_mt <-
  terminal$sb_recent_2051_2054_recomputed_mt
terminal$terminal_sb_sbmsy <- with(
  terminal, sb_recent_2051_2054_mt / sbmsy_mt
)
terminal_status <- merge(
  terminal,
  terminal_depletion_rank[c("ensemble_id", "simulation", "sb_recent_sb0")],
  by = c("ensemble_id", "simulation"), sort = FALSE
)
if (nrow(terminal_status) != 800L || any(!is.finite(as.matrix(
  terminal_status[c("terminal_sb_sbmsy", "sb_recent_sb0", "terminal_f_fmsy")]
)))) {
  stop("Terminal Kobe and Majuro quantities are incomplete.")
}

projection_status_panel <- function(data, x_column, x_boundary, x_label,
                                    diagram = c("kobe", "majuro")) {
  diagram <- match.arg(diagram)
  surface <- hdr_surface(data, x_column, "terminal_f_fmsy")
  x_upper <- max(data[[x_column]], x_boundary) * 1.03
  y_upper <- max(data$terminal_f_fmsy, 1) * 1.03
  category <- if (diagram == "kobe") {
    kobe_category(data[[x_column]], data$terminal_f_fmsy)
  } else {
    majuro_category(data[[x_column]], data$terminal_f_fmsy)
  }
  category_levels <- if (diagram == "kobe") kobe_levels else majuro_levels
  probability <- category_probability(category, category_levels)
  if (diagram == "kobe") {
    probability_labels <- data.frame(
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
    probability_labels <- data.frame(
      x = c(
        mean(c(x_boundary, x_upper)), mean(c(x_boundary, x_upper)),
        x_boundary * 0.50
      ),
      y = c(min(0.48, y_upper * 0.24), mean(c(1, y_upper)), y_upper * 0.50),
      label = scales::percent(probability, accuracy = 0.1)
    )
  }
  background <- if (diagram == "kobe") {
    list(
      ggplot2::annotate("rect", xmin = x_boundary, xmax = Inf, ymin = -Inf, ymax = 1, fill = "#2a9d8f", alpha = 0.72),
      ggplot2::annotate("rect", xmin = -Inf, xmax = x_boundary, ymin = -Inf, ymax = 1, fill = "#e9c46a", alpha = 0.72),
      ggplot2::annotate("rect", xmin = x_boundary, xmax = Inf, ymin = 1, ymax = Inf, fill = "#f4a261", alpha = 0.70),
      ggplot2::annotate("rect", xmin = -Inf, xmax = x_boundary, ymin = 1, ymax = Inf, fill = "#e76f51", alpha = 0.72),
      ggplot2::geom_hline(yintercept = 1, colour = "#1f2937", linewidth = 0.48),
      ggplot2::geom_vline(xintercept = x_boundary, colour = "#1f2937", linewidth = 0.48)
    )
  } else {
    list(
      ggplot2::annotate("rect", xmin = -Inf, xmax = x_boundary, ymin = -Inf, ymax = Inf, fill = "#e76f51", alpha = 0.72),
      ggplot2::annotate("rect", xmin = x_boundary, xmax = Inf, ymin = -Inf, ymax = 1, fill = "#2a9d8f", alpha = 0.72),
      ggplot2::annotate("rect", xmin = x_boundary, xmax = Inf, ymin = 1, ymax = Inf, fill = "#f4a261", alpha = 0.70),
      ggplot2::annotate("segment", x = x_boundary, xend = x_upper, y = 1, yend = 1, colour = "#1f2937", linewidth = 0.48),
      ggplot2::geom_vline(xintercept = x_boundary, colour = "#1f2937", linewidth = 0.48)
    )
  }
  ggplot2::ggplot() +
    background +
    ggplot2::geom_contour_filled(
      data = surface,
      ggplot2::aes(x = .data$x, y = .data$y, z = .data$density),
      breaks = attr(surface, "breaks"), alpha = 0.78,
      colour = "#426D76", linewidth = 0.34
    ) +
    ggplot2::geom_point(
      data = data,
      ggplot2::aes(x = .data[[x_column]], y = .data$terminal_f_fmsy),
      colour = "#0B6477", size = 1.24, alpha = 0.28
    ) +
    ggplot2::geom_label(
      data = probability_labels,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      inherit.aes = FALSE, colour = "#173042", fill = "white",
      alpha = 0.84, linewidth = 0.15, size = 3.0, fontface = "bold",
      label.padding = grid::unit(0.12, "lines")
    ) +
    ggplot2::scale_fill_manual(
      values = hdr_colours, labels = c("95% HDR", "80% HDR", "50% HDR"),
      name = NULL
    ) +
    ggplot2::coord_cartesian(
      xlim = c(0, x_upper), ylim = c(0, y_upper), expand = FALSE
    ) +
    ggplot2::labs(
      x = x_label,
      y = expression(bar(italic(F))[2050:2053] / italic(F)[MSY])
    ) + theme_report(11.2) +
    ggplot2::theme(
      legend.position = "bottom", legend.box = "horizontal",
      legend.text = ggplot2::element_text(size = 8.8),
      legend.key.width = grid::unit(0.70, "cm"),
      plot.margin = ggplot2::margin(3, 5, 3, 5)
    )
}

p_terminal_kobe <- projection_status_panel(
  terminal_status, "terminal_sb_sbmsy", 1,
  expression(bar(italic(SB))[2051:2054] / italic(SB)[MSY]), "kobe"
)
p_terminal_majuro <- projection_status_panel(
  terminal_status, "sb_recent_sb0", 0.20,
  expression(bar(italic(SB))[2051:2054] / italic(SB)[italic(F) == 0]), "majuro"
)
p_terminal <- (p_terminal_kobe | p_terminal_majuro) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(tag_levels = "a") &
  ggplot2::theme(legend.position = "bottom", legend.box = "horizontal")
terminal_status_files <- save_plot(
  p_terminal, "projection-terminal-status", height = 4.25
)
unlink(file.path(
  figure_dir, paste0("projection-terminal-kobe-status", c(".png", ".pdf"))
))

fishing_status <- rbind(
  data.frame(
    period = "Current", value = hybrid_management$f_recent_fmsy,
    source = "Current structure + available estimation"
  ),
  data.frame(
    period = "2050–2053", value = terminal$terminal_f_fmsy,
    source = "Projected structure + recruitment"
  )
)
fishing_status$period <- factor(
  fishing_status$period,
  levels = c("Current", "2050–2053")
)
fishing_status_interval <- summarise_by(
  fishing_status, "value", "period"
)
p_fishing_key <- ggplot2::ggplot(
  fishing_status_interval,
  ggplot2::aes(x = .data$period, y = .data$median)
) +
  ggplot2::geom_linerange(
    ggplot2::aes(ymin = .data$q025, ymax = .data$q975),
    colour = "#A9CFD6", linewidth = 3.2, lineend = "round"
  ) +
  ggplot2::geom_linerange(
    ggplot2::aes(ymin = .data$q10, ymax = .data$q90),
    colour = "#5EAAB7", linewidth = 5.0, lineend = "round"
  ) +
  ggplot2::geom_linerange(
    ggplot2::aes(ymin = .data$q25, ymax = .data$q75),
    colour = "#167789", linewidth = 6.8, lineend = "round"
  ) +
  ggplot2::geom_point(colour = "#082F43", size = 2.4) +
  ggplot2::geom_hline(
    yintercept = 1, colour = "#B83232", linewidth = 0.52,
    linetype = "33"
  ) +
  ggplot2::coord_cartesian(ylim = c(0, NA)) +
  ggplot2::labs(
    x = NULL, y = expression(italic(F) / italic(F)[MSY])
  ) + theme_report(10.8)

projection_key_plot <- (p_proj_risk | p_fishing_key) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(tag_levels = "a") &
  ggplot2::theme(
    legend.position = "bottom", legend.box = "horizontal",
    legend.text = ggplot2::element_text(size = 8.5),
    plot.tag.position = c(0.075, 0.955),
    plot.tag = ggplot2::element_text(
      face = "bold", colour = "#183246", size = 11,
      hjust = 0, vjust = 1
    )
  )
projection_key_files <- save_plot(
  projection_key_plot, "projection-key-quantities", height = 3.8
)

unlink(file.path(
  figure_dir, paste0("fishing-mortality-status", c(".png", ".pdf"))
))

# Exact regional spawning biomass is available on a common historical and
# projection scale. The assessment model's separately reported normalized
# regional biomass is not regional depletion and is deliberately not relabelled.
historical_region <- projection$historical_region
historical_region$spawning_potential_kt <-
  historical_region$spawning_biomass_mt / 1000
historical_region$region_label <- paste("Region", historical_region$region)
historical_region_all <- series[c("ensemble_id", "year", "spawning_potential")]
names(historical_region_all)[names(historical_region_all) == "spawning_potential"] <-
  "spawning_potential_kt"
historical_region_all$region_label <- "All regions"
historical_region_long <- rbind(
  historical_region[c(
    "ensemble_id", "year", "region_label", "spawning_potential_kt"
  )],
  historical_region_all
)

regional_sum_check <- stats::aggregate(
  spawning_potential_kt ~ ensemble_id + year,
  historical_region, sum
)
stock_sum_check <- merge(
  regional_sum_check, historical_region_all,
  by = c("ensemble_id", "year"), suffixes = c("_regional", "_stock")
)
regional_relative_error <- with(
  stock_sum_check,
  abs(spawning_potential_kt_regional - spawning_potential_kt_stock) /
    pmax(abs(spawning_potential_kt_stock), .Machine$double.eps)
)
if (max(regional_relative_error) > 1e-3) {
  stop("Historical regional spawning biomass does not sum to the stock series.")
}

projected_region <- projection$annual_region
projected_region$spawning_potential_kt <- projected_region$spawning_biomass_mt / 1000
projected_region$region_label <- paste("Region", projected_region$region)
projected_region_all <- projection$annual_stock[c(
  "ensemble_id", "simulation", "year", "spawning_biomass_mt"
)]
projected_region_all$spawning_potential_kt <-
  projected_region_all$spawning_biomass_mt / 1000
projected_region_all$region_label <- "All regions"
projected_region_long <- rbind(
  projected_region[c(
    "ensemble_id", "simulation", "year", "region_label",
    "spawning_potential_kt"
  )],
  projected_region_all[c(
    "ensemble_id", "simulation", "year", "region_label",
    "spawning_potential_kt"
  )]
)

regional_plot_levels <- c(paste("Region", 1:5), "All regions")
historical_region_summary <- summarise_by(
  historical_region_long, "spawning_potential_kt", c("year", "region_label")
)
projected_region_summary <- summarise_by(
  projected_region_long, "spawning_potential_kt", c("year", "region_label")
)
representative_historical_region <- merge(
  representative_keys[c("ensemble_id", "trajectory_id")],
  historical_region_long, by = "ensemble_id", sort = FALSE
)
representative_projected_region <- merge(
  representative_keys,
  projected_region_long, by = c("ensemble_id", "simulation"), sort = FALSE
)
for (object_name in c(
  "historical_region_summary", "projected_region_summary",
  "representative_historical_region", "representative_projected_region"
)) {
  object <- get(object_name)
  object <- object[
    as.character(object$region_label) %in% regional_plot_levels, , drop = FALSE
  ]
  object$region_label <- factor(
    object$region_label, levels = regional_plot_levels, ordered = TRUE
  )
  object <- object[order(object$region_label, object$year), , drop = FALSE]
  assign(object_name, object)
}

regional_spawning_plot <- function(region_labels = regional_plot_levels) {
  historical_summary <- historical_region_summary[
    historical_region_summary$region_label %in% region_labels,
  ]
  projected_summary <- projected_region_summary[
    projected_region_summary$region_label %in% region_labels,
  ]
  historical_paths <- representative_historical_region[
    representative_historical_region$region_label %in% region_labels,
  ]
  projected_paths <- representative_projected_region[
    representative_projected_region$region_label %in% region_labels,
  ]
  median_transition <- merge(
    historical_summary[historical_summary$year == 2024,
                       c("region_label", "year", "median")],
    projected_summary[projected_summary$year == 2025,
                      c("region_label", "year", "median")],
    by = "region_label", suffixes = c("_historical", "_projected")
  )
  trajectory_transition <- merge(
    historical_paths[historical_paths$year == 2024,
                     c("trajectory_id", "region_label", "year", "spawning_potential_kt")],
    projected_paths[projected_paths$year == 2025,
                    c("trajectory_id", "region_label", "year", "spawning_potential_kt")],
    by = c("trajectory_id", "region_label"),
    suffixes = c("_historical", "_projected")
  )
  ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = historical_summary,
      ggplot2::aes(x = .data$year, ymin = .data$q025, ymax = .data$q975),
      fill = "#C8D0D4", alpha = 0.28
    ) +
    ggplot2::geom_ribbon(
      data = historical_summary,
      ggplot2::aes(x = .data$year, ymin = .data$q10, ymax = .data$q90),
      fill = "#9FABAF", alpha = 0.34
    ) +
    ggplot2::geom_ribbon(
      data = historical_summary,
      ggplot2::aes(x = .data$year, ymin = .data$q25, ymax = .data$q75),
      fill = "#75868D", alpha = 0.40
    ) +
    ggplot2::geom_ribbon(
      data = projected_summary,
      ggplot2::aes(
        x = .data$year, ymin = .data$q025, ymax = .data$q975,
        fill = "95% interval"
      ), alpha = 0.42
    ) +
    ggplot2::geom_ribbon(
      data = projected_summary,
      ggplot2::aes(
        x = .data$year, ymin = .data$q10, ymax = .data$q90,
        fill = "80% interval"
      ), alpha = 0.54
    ) +
    ggplot2::geom_ribbon(
      data = projected_summary,
      ggplot2::aes(
        x = .data$year, ymin = .data$q25, ymax = .data$q75,
        fill = "50% interval"
      ), alpha = 0.66
    ) +
    ggplot2::geom_line(
      data = historical_paths,
      ggplot2::aes(
        x = .data$year, y = .data$spawning_potential_kt,
        group = .data$trajectory_id
      ), colour = "#687A83", linewidth = 0.25, alpha = 0.27
    ) +
    ggplot2::geom_line(
      data = projected_paths,
      ggplot2::aes(
        x = .data$year, y = .data$spawning_potential_kt,
        group = .data$trajectory_id
      ), colour = "#218899", linewidth = 0.30, alpha = 0.40
    ) +
    ggplot2::geom_segment(
      data = trajectory_transition,
      ggplot2::aes(
        x = .data$year_historical, xend = .data$year_projected,
        y = .data$spawning_potential_kt_historical,
        yend = .data$spawning_potential_kt_projected,
        group = .data$trajectory_id
      ), colour = "#218899", linewidth = 0.28, alpha = 0.36
    ) +
    ggplot2::geom_line(
      data = historical_summary,
      ggplot2::aes(
        x = .data$year, y = .data$median, colour = "Historical"
      ), linewidth = 0.72
    ) +
    ggplot2::geom_line(
      data = projected_summary,
      ggplot2::aes(
        x = .data$year, y = .data$median, colour = "Projection"
      ), linewidth = 0.90
    ) +
    ggplot2::geom_segment(
      data = median_transition,
      ggplot2::aes(
        x = .data$year_historical, xend = .data$year_projected,
        y = .data$median_historical, yend = .data$median_projected
      ), colour = "#07566B", linewidth = 0.86
    ) +
    ggplot2::geom_vline(
      xintercept = 2024.5, colour = "#5D6C73", linewidth = 0.48,
      linetype = "33"
    ) +
    ggplot2::facet_wrap(
      ~ region_label, ncol = 3, scales = "free_y", drop = FALSE
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
    ggplot2::scale_x_continuous(
      breaks = c(1960, 1990, 2024, 2054)
    ) +
    ggplot2::coord_cartesian(ylim = c(0, NA)) +
    ggplot2::labs(
      x = "Year", y = expression(Spawning~potential~(10^3~plain(MT)))
    ) + theme_report(9.3) +
    ggplot2::theme(
      legend.position = "bottom", legend.box = "vertical",
      legend.text = ggplot2::element_text(size = 8.2),
      legend.key.width = grid::unit(0.75, "cm"),
      strip.background = ggplot2::element_rect(fill = "#E4ECEF", colour = "#607985"),
      strip.text = ggplot2::element_text(face = "bold", size = 8.8),
      axis.title = ggplot2::element_text(size = 9.4),
      axis.text = ggplot2::element_text(size = 7.8),
      plot.margin = ggplot2::margin(4, 5, 4, 5)
    )
}

regional_spawning_files <- save_plot(
  regional_spawning_plot(),
  "regional-spawning-potential", height = 5.6
)

if (!identical(projection$schema_version, "1.2.0") ||
    !all(c("spawning_biomass_noeff_mt", "depletion") %in%
         names(projection$historical_region))) {
  stop("Historical regional no-fishing biomass is required for regional depletion.")
}
historical_regional_depletion_input <- projection$historical_region[c(
  "ensemble_id", "year", "region", "spawning_biomass_mt",
  "spawning_biomass_noeff_mt"
)]
names(historical_regional_depletion_input)[names(
  historical_regional_depletion_input
) == "spawning_biomass_mt"] <- "spawning_potential"
names(historical_regional_depletion_input)[names(
  historical_regional_depletion_input
) == "spawning_biomass_noeff_mt"] <- "spawning_potential_nofish"
historical_regional_depletion_input$spawning_potential <-
  historical_regional_depletion_input$spawning_potential / 1000
historical_regional_depletion_input$spawning_potential_nofish <-
  historical_regional_depletion_input$spawning_potential_nofish / 1000
historical_regional_depletion_input$region_label <- paste(
  "Region", historical_regional_depletion_input$region
)
historical_stock_depletion_input <- series[c(
  "ensemble_id", "year", "spawning_potential", "spawning_potential_nofish"
)]
historical_stock_depletion_input$region <- 0L
historical_stock_depletion_input$region_label <- "All regions"
historical_regional_depletion_input <- rbind(
  historical_regional_depletion_input[c(
    "ensemble_id", "year", "region", "region_label",
    "spawning_potential", "spawning_potential_nofish"
  )],
  historical_stock_depletion_input[c(
    "ensemble_id", "year", "region", "region_label",
    "spawning_potential", "spawning_potential_nofish"
  )]
)
historical_regional_depletion <- rolling_recent_depletion(
  historical_regional_depletion_input,
  c("ensemble_id", "region", "region_label")
)

projected_regional_depletion_input <- projection$annual_region[c(
  "ensemble_id", "simulation", "year", "region",
  "spawning_biomass_mt", "spawning_biomass_noeff_mt"
)]
names(projected_regional_depletion_input)[names(
  projected_regional_depletion_input
) == "spawning_biomass_mt"] <- "spawning_potential"
names(projected_regional_depletion_input)[names(
  projected_regional_depletion_input
) == "spawning_biomass_noeff_mt"] <- "spawning_potential_nofish"
projected_regional_depletion_input$spawning_potential <-
  projected_regional_depletion_input$spawning_potential / 1000
projected_regional_depletion_input$spawning_potential_nofish <-
  projected_regional_depletion_input$spawning_potential_nofish / 1000
projected_regional_depletion_input$region_label <- paste(
  "Region", projected_regional_depletion_input$region
)
projected_stock_depletion_input <- projection$annual_stock[c(
  "ensemble_id", "simulation", "year",
  "spawning_biomass_mt", "spawning_biomass_noeff_mt"
)]
names(projected_stock_depletion_input)[names(
  projected_stock_depletion_input
) == "spawning_biomass_mt"] <- "spawning_potential"
names(projected_stock_depletion_input)[names(
  projected_stock_depletion_input
) == "spawning_biomass_noeff_mt"] <- "spawning_potential_nofish"
projected_stock_depletion_input$spawning_potential <-
  projected_stock_depletion_input$spawning_potential / 1000
projected_stock_depletion_input$spawning_potential_nofish <-
  projected_stock_depletion_input$spawning_potential_nofish / 1000
projected_stock_depletion_input$region <- 0L
projected_stock_depletion_input$region_label <- "All regions"
projected_regional_depletion_input <- rbind(
  projected_regional_depletion_input[c(
    "ensemble_id", "simulation", "year", "region", "region_label",
    "spawning_potential", "spawning_potential_nofish"
  )],
  projected_stock_depletion_input[c(
    "ensemble_id", "simulation", "year", "region", "region_label",
    "spawning_potential", "spawning_potential_nofish"
  )]
)
regional_simulation_keys <- unique(projected_regional_depletion_input[c(
  "ensemble_id", "simulation"
)])
historical_regional_for_projection <- merge(
  regional_simulation_keys,
  historical_regional_depletion_input,
  by = "ensemble_id", sort = FALSE
)
combined_regional_depletion_input <- rbind(
  historical_regional_for_projection[names(projected_regional_depletion_input)],
  projected_regional_depletion_input
)
projected_regional_depletion <- rolling_recent_depletion(
  combined_regional_depletion_input,
  c("ensemble_id", "simulation", "region", "region_label"),
  target_years = projection$projection_years
)

historical_regional_depletion_summary <- summarise_by(
  historical_regional_depletion, "sb_recent_sb0", c("year", "region_label")
)
projected_regional_depletion_summary <- summarise_by(
  projected_regional_depletion, "sb_recent_sb0", c("year", "region_label")
)
representative_historical_depletion <- merge(
  representative_keys[c("ensemble_id", "trajectory_id")],
  historical_regional_depletion,
  by = "ensemble_id", sort = FALSE
)
representative_projected_depletion <- merge(
  representative_keys,
  projected_regional_depletion,
  by = c("ensemble_id", "simulation"), sort = FALSE
)
regional_depletion_levels <- regional_plot_levels
for (object_name in c(
  "historical_regional_depletion_summary",
  "projected_regional_depletion_summary",
  "representative_historical_depletion",
  "representative_projected_depletion"
)) {
  object <- get(object_name)
  object <- object[
    as.character(object$region_label) %in% regional_depletion_levels, , drop = FALSE
  ]
  object$region_label <- factor(
    object$region_label, levels = regional_depletion_levels, ordered = TRUE
  )
  object <- object[order(object$region_label, object$year), , drop = FALSE]
  assign(object_name, object)
}

regional_depletion_transition <- merge(
  historical_regional_depletion_summary[
    historical_regional_depletion_summary$year == 2024L,
    c("region_label", "year", "median")
  ],
  projected_regional_depletion_summary[
    projected_regional_depletion_summary$year == 2025L,
    c("region_label", "year", "median")
  ],
  by = "region_label", suffixes = c("_historical", "_projected")
)
regional_depletion_path_transition <- merge(
  representative_historical_depletion[
    representative_historical_depletion$year == 2024L,
    c("trajectory_id", "region_label", "year", "sb_recent_sb0")
  ],
  representative_projected_depletion[
    representative_projected_depletion$year == 2025L,
    c("trajectory_id", "region_label", "year", "sb_recent_sb0")
  ],
  by = c("trajectory_id", "region_label"),
  suffixes = c("_historical", "_projected")
)

regional_lrp_reference <- data.frame(
  y = 0.20, reference = "Stock-wide LRP (0.20)"
)
regional_depletion_plot <- ggplot2::ggplot() +
  ggplot2::geom_ribbon(
    data = historical_regional_depletion_summary,
    ggplot2::aes(x = .data$year, ymin = .data$q025, ymax = .data$q975),
    fill = "#C8D0D4", alpha = 0.28
  ) +
  ggplot2::geom_ribbon(
    data = historical_regional_depletion_summary,
    ggplot2::aes(x = .data$year, ymin = .data$q10, ymax = .data$q90),
    fill = "#9FABAF", alpha = 0.34
  ) +
  ggplot2::geom_ribbon(
    data = historical_regional_depletion_summary,
    ggplot2::aes(x = .data$year, ymin = .data$q25, ymax = .data$q75),
    fill = "#75868D", alpha = 0.40
  ) +
  ggplot2::geom_ribbon(
    data = projected_regional_depletion_summary,
    ggplot2::aes(
      x = .data$year, ymin = .data$q025, ymax = .data$q975,
      fill = "95% interval"
    ), alpha = 0.42
  ) +
  ggplot2::geom_ribbon(
    data = projected_regional_depletion_summary,
    ggplot2::aes(
      x = .data$year, ymin = .data$q10, ymax = .data$q90,
      fill = "80% interval"
    ), alpha = 0.54
  ) +
  ggplot2::geom_ribbon(
    data = projected_regional_depletion_summary,
    ggplot2::aes(
      x = .data$year, ymin = .data$q25, ymax = .data$q75,
      fill = "50% interval"
    ), alpha = 0.66
  ) +
  ggplot2::geom_line(
    data = representative_historical_depletion,
    ggplot2::aes(
      x = .data$year, y = .data$sb_recent_sb0,
      group = .data$trajectory_id
    ), colour = "#687A83", linewidth = 0.25, alpha = 0.27
  ) +
  ggplot2::geom_line(
    data = representative_projected_depletion,
    ggplot2::aes(
      x = .data$year, y = .data$sb_recent_sb0,
      group = .data$trajectory_id
    ), colour = "#218899", linewidth = 0.30, alpha = 0.40
  ) +
  ggplot2::geom_segment(
    data = regional_depletion_path_transition,
    ggplot2::aes(
      x = .data$year_historical, xend = .data$year_projected,
      y = .data$sb_recent_sb0_historical,
      yend = .data$sb_recent_sb0_projected,
      group = .data$trajectory_id
    ), colour = "#218899", linewidth = 0.28, alpha = 0.36
  ) +
  ggplot2::geom_line(
    data = historical_regional_depletion_summary,
    ggplot2::aes(
      x = .data$year, y = .data$median, colour = "Historical"
    ), linewidth = 0.72
  ) +
  ggplot2::geom_line(
    data = projected_regional_depletion_summary,
    ggplot2::aes(
      x = .data$year, y = .data$median, colour = "Projection"
    ), linewidth = 0.90
  ) +
  ggplot2::geom_segment(
    data = regional_depletion_transition,
    ggplot2::aes(
      x = .data$year_historical, xend = .data$year_projected,
      y = .data$median_historical, yend = .data$median_projected
    ), colour = "#07566B", linewidth = 0.86
  ) +
  ggplot2::geom_hline(
    data = regional_lrp_reference,
    ggplot2::aes(yintercept = .data$y, linetype = .data$reference),
    colour = "#B83232", linewidth = 0.54
  ) +
  ggplot2::geom_vline(
    xintercept = 2024.5, colour = "#5D6C73", linewidth = 0.48,
    linetype = "33"
  ) +
  ggplot2::facet_wrap(~ region_label, ncol = 3, drop = FALSE) +
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
  ggplot2::scale_linetype_manual(
    values = c("Stock-wide LRP (0.20)" = "33"), name = NULL
  ) +
  ggplot2::scale_x_continuous(breaks = c(1960, 1990, 2024, 2054)) +
  ggplot2::coord_cartesian(ylim = c(0, NA)) +
  ggplot2::labs(
    x = "Year",
    y = expression(italic(SB)[recent] / italic(SB)[italic(F) == 0])
  ) + theme_report(9.3) +
  ggplot2::theme(
    legend.position = "bottom", legend.box = "vertical",
    legend.text = ggplot2::element_text(size = 8.2),
    legend.key.width = grid::unit(0.75, "cm"),
    strip.background = ggplot2::element_rect(fill = "#E4ECEF", colour = "#607985"),
    strip.text = ggplot2::element_text(face = "bold", size = 8.8),
    axis.title = ggplot2::element_text(size = 9.4),
    axis.text = ggplot2::element_text(size = 7.8),
    plot.margin = ggplot2::margin(4, 5, 4, 5)
  )
regional_depletion_files <- save_plot(
  regional_depletion_plot, "regional-depletion", height = 5.6
)

projection_years <- c(2030L, 2040L, 2054L)
projection_summary <- merge(
  proj_depletion[proj_depletion$year %in% projection_years, c("year", "q10", "median", "q90")],
  proj_risk[proj_risk$year %in% projection_years, ], by = "year"
)
projection_summary <- merge(
  projection_summary,
  proj_spawning_summary[proj_spawning_summary$year %in% projection_years, c("year", "q10", "median", "q90")],
  by = "year", suffixes = c("_depletion", "_spawning")
)
projection_summary <- merge(
  projection_summary,
  catch_msy_summary[catch_msy_summary$year %in% projection_years,
                    c("year", "q10", "median", "q90")],
  by = "year"
)
projection_display <- data.frame(
  Year = projection_summary$year,
  `SBrecent/SBF=0 10%` = sprintf("%.3f", projection_summary$q10_depletion),
  `SBrecent/SBF=0 median` = sprintf("%.3f", projection_summary$median_depletion),
  `SBrecent/SBF=0 90%` = sprintf("%.3f", projection_summary$q90_depletion),
  `Below LRP` = scales::percent(projection_summary$probability_below_lrp, accuracy = 0.1),
  `Below 2012–2015 objective` = scales::percent(
    projection_summary$probability_below_historical_objective, accuracy = 0.1
  ),
  `Spawning potential median (10^3 MT)` = sprintf("%.1f", projection_summary$median_spawning),
  `Catch/MSY 10%` = sprintf("%.3f", projection_summary$q10),
  `Catch/MSY median` = sprintf("%.3f", projection_summary$median),
  `Catch/MSY 90%` = sprintf("%.3f", projection_summary$q90),
  check.names = FALSE
)
write.csv(projection_display, file.path(table_dir, "projection-summary.csv"), row.names = FALSE)

projection_caption <- paste0(
  "Projected management quantities at selected years across ",
  projection$projection_complete_models,
  " models and ten recruitment sequences per model. Depletion is the four-year mean spawning biomass divided by the preceding ten-year mean unfished spawning biomass; the LRP is 0.20. ",
  "Catches are fixed by fishery to their 2022–2024 means, and intervals exclude Hessian parameter uncertainty."
)
projection_rows <- vapply(seq_len(nrow(projection_display)), function(i) {
  paste0(paste(vapply(projection_display[i, ], latex_escape, character(1)), collapse = " & "), " \\\\")
}, character(1))
projection_latex <- paste0(
  "% Requires \\usepackage{booktabs,tabularx}\n",
  "\\begin{table}[htbp]\n\\centering\n\\caption{", latex_escape(projection_caption), "}\n",
  "\\scriptsize\n\\setlength{\\tabcolsep}{1.5pt}\n\\begin{tabularx}{\\textwidth}{@{}rrrrrrXrrr@{}}\n",
  "\\toprule\nYear & $D$ 10\\% & $D$ median & $D$ 90\\% & Below LRP & Below objective & Spawning median ($10^3$ MT) & $C/MSY$ 10\\% & $C/MSY$ median & $C/MSY$ 90\\% \\\\\n\\midrule\n",
  paste(projection_rows, collapse = "\n"),
  "\n\\bottomrule\n\\end{tabularx}\n\\end{table}"
)

projection_model_depletion <- stats::aggregate(
  sb_recent_sb0 ~ ensemble_id + year,
  projection_management$projected, FUN = mean
)
projection_model_depletion <- reshape(
  projection_model_depletion[
    projection_model_depletion$year %in% c(2025L, 2030L, 2040L, 2054L),
  ],
  idvar = "ensemble_id", timevar = "year", direction = "wide"
)
names(projection_model_depletion) <- sub(
  "sb_recent_sb0.", "depletion_", names(projection_model_depletion), fixed = TRUE
)
terminal_by_model <- stats::aggregate(
  terminal_f_fmsy ~ ensemble_id, terminal, FUN = mean
)
projection_driver <- Reduce(
  function(x, y) merge(x, y, by = "ensemble_id"),
  list(
    projection_model_depletion,
    terminal_by_model,
    design[c("ensemble_id", "steepness")]
  )
)
steepness_breaks <- stats::quantile(
  projection_driver$steepness, probs = seq(0, 1, 0.25), names = FALSE
)
projection_driver$steepness_quartile <- cut(
  projection_driver$steepness, breaks = steepness_breaks,
  include.lowest = TRUE, labels = paste0("Q", 1:4)
)
projection_driver_summary <- stats::aggregate(
  cbind(
    steepness, depletion_2025, depletion_2030, depletion_2040,
    depletion_2054, terminal_f_fmsy
  ) ~ steepness_quartile,
  projection_driver, FUN = mean
)
projection_driver_display <- data.frame(
  `Steepness quartile` = paste0(
    projection_driver_summary$steepness_quartile, " (",
    sprintf("%.3f", steepness_breaks[1:4]), "–",
    sprintf("%.3f", steepness_breaks[2:5]), ")"
  ),
  `Mean h` = sprintf("%.3f", projection_driver_summary$steepness),
  `D 2025` = sprintf("%.3f", projection_driver_summary$depletion_2025),
  `D 2030` = sprintf("%.3f", projection_driver_summary$depletion_2030),
  `D 2040` = sprintf("%.3f", projection_driver_summary$depletion_2040),
  `D 2054` = sprintf("%.3f", projection_driver_summary$depletion_2054),
  `Mean Frecent(2050–2053)/FMSY` = sprintf(
    "%.3f", projection_driver_summary$terminal_f_fmsy
  ),
  check.names = FALSE
)
unlink(file.path(table_dir, "projection-steepness-audit.csv"))
projection_driver_caption <- paste0(
  "Projection audit by steepness quartile. Values are model-level means over ten recruitment sequences; D is the four-year mean spawning biomass ending in the listed year divided by the preceding ten-year mean unfished spawning biomass. ",
  "Recovery occurs in every quartile, while higher steepness generally increases its magnitude. Because the other ensemble axes vary jointly, this table is diagnostic rather than a controlled one-factor sensitivity analysis."
)
projection_driver_rows <- vapply(seq_len(nrow(projection_driver_display)), function(i) {
  paste0(
    paste(vapply(projection_driver_display[i, ], latex_escape, character(1)), collapse = " & "),
    " \\\\"
  )
}, character(1))
projection_driver_latex <- paste0(
  "% Requires \\usepackage{booktabs,tabularx}\n",
  "\\begin{table}[htbp]\n\\centering\n\\caption{",
  latex_escape(projection_driver_caption), "}\n",
  "\\small\n\\begin{tabularx}{\\textwidth}{@{}Xrrrrrr@{}}\n",
  "\\toprule\nSteepness quartile & Mean $h$ & $D_{2025}$ & $D_{2030}$ & $D_{2040}$ & $D_{2054}$ & Mean $F_{recent(2050--2053)}/F_{MSY}$ \\\\\n\\midrule\n",
  paste(projection_driver_rows, collapse = "\n"),
  "\n\\bottomrule\n\\end{tabularx}\n\\end{table}"
)

conditioning_display <- data.frame(
  Fishery = projection$conditioning$fishery,
  Name = projection$conditioning$fishery_name,
  Region = projection$conditioning$region,
  Unit = projection$conditioning$catch_unit,
  `2022–2024 mean annual catch` = formatC(
    projection$conditioning$mean_annual_catch,
    digits = 6, format = "fg", flag = "#"
  ),
  check.names = FALSE
)
unlink(file.path(table_dir, c(
  "projection-fishery-conditioning.csv",
  "projection-fishery-quarter-conditioning.csv"
)))
zero_quarterly <- quarterly_conditioning[quarterly_conditioning$exact_zero, ]
zero_by_fishery <- split(zero_quarterly$quarter, zero_quarterly$fishery)
zero_conditioning_display <- do.call(rbind, lapply(names(zero_by_fishery), function(id) {
  fishery <- as.integer(id)
  row <- quarterly_conditioning[quarterly_conditioning$fishery == fishery, ][1L, ]
  data.frame(
    Fishery = fishery,
    Name = row$fishery_name,
    Unit = row$catch_unit,
    `Zero quarters` = paste(zero_by_fishery[[id]], collapse = ", "),
    `Number of zero quarters` = length(zero_by_fishery[[id]]),
    check.names = FALSE
  )
}))
rownames(zero_conditioning_display) <- NULL
unlink(file.path(table_dir, "projection-zero-quarter-audit.csv"))
zero_fishery_text <- paste(zero_conditioning_display$Fishery, collapse = ", ")
conditioning_caption <- paste0(
  "Fishery-specific projection conditioning. Each annual value is the sum of four fishery–quarter means calculated over 2022–2024, with an absent observation represented as zero before averaging. ",
  "The model input flag retains number units for fisheries 1–11 and 29–33 and weight units for fisheries 12–28; unlike units are never combined into a scientific total. ",
  "A numerical placeholder of 0.000001 in the fishery's original input unit is used for each of the ",
  nrow(zero_quarterly), " fishery–quarter means that is exactly zero, spanning fisheries ",
  zero_fishery_text, "; fishery 9 is the only fishery whose four-quarter annual mean is zero. Audited conditioning values retain the exact zeros."
)
conditioning_rows <- vapply(seq_len(nrow(conditioning_display)), function(i) {
  paste0(
    paste(vapply(conditioning_display[i, ], latex_escape, character(1)), collapse = " & "),
    " \\\\"
  )
}, character(1))
conditioning_latex <- paste0(
  "% Requires \\usepackage{booktabs,longtable}\n",
  "\\begin{longtable}{@{}rlrrr@{}}\n\\caption{",
  latex_escape(conditioning_caption), "}\\\\\n",
  "\\toprule\nFishery & Name & Region & Unit & 2022--2024 mean annual catch \\\\\n\\midrule\n\\endfirsthead\n",
  "\\toprule\nFishery & Name & Region & Unit & 2022--2024 mean annual catch \\\\\n\\midrule\n\\endhead\n",
  paste(conditioning_rows, collapse = "\n"),
  "\n\\bottomrule\n\\end{longtable}"
)
zero_conditioning_caption <- paste0(
  "Zero fishery–quarter means in the 2022–2024 catch-conditioning audit. ",
  "Quarter numbers correspond to model months 2, 5, 8 and 11. The 0.000001 positive value used by the projection input writer is a numerical placeholder in each fishery's original number or weight unit; the scientific audit retains zero."
)
zero_conditioning_rows <- vapply(
  seq_len(nrow(zero_conditioning_display)), function(index) {
    paste0(
      paste(vapply(
        zero_conditioning_display[index, ], latex_escape, character(1)
      ), collapse = " & "),
      " \\\\"
    )
  }, character(1L)
)
zero_conditioning_latex <- paste0(
  "% Requires \\usepackage{booktabs,tabularx}\n",
  "\\begin{table}[htbp]\n\\centering\n\\caption{",
  latex_escape(zero_conditioning_caption), "}\n",
  "\\small\n\\begin{tabularx}{\\textwidth}{@{}rXXrr@{}}\n",
  "\\toprule\nFishery & Name & Unit & Zero quarters & Count \\\\\n\\midrule\n",
  paste(zero_conditioning_rows, collapse = "\n"),
  "\n\\bottomrule\n\\end{tabularx}\n\\end{table}"
)

terminal_values <- list(
  terminal$terminal_sb_sbmsy,
  terminal$terminal_f_fmsy
)
terminal_display <- data.frame(
  Quantity = c("SB2051–2054 / SBMSY", "F2050–2053 / FMSY"),
  q10 = sprintf("%.3f", vapply(
    terminal_values, stats::quantile, numeric(1), probs = 0.10, names = FALSE
  )),
  Median = sprintf("%.3f", vapply(terminal_values, stats::median, numeric(1))),
  q90 = sprintf("%.3f", vapply(
    terminal_values, stats::quantile, numeric(1), probs = 0.90, names = FALSE
  )),
  Criterion = c("SB2051–2054/SBMSY < 1", "F2050–2053/FMSY > 1"),
  beyond = scales::percent(c(
    mean(terminal$terminal_sb_sbmsy < 1),
    mean(terminal$terminal_f_fmsy > 1)
  ), accuracy = 0.1),
  check.names = FALSE
)
names(terminal_display) <- c(
  "Quantity", "10%", "Median", "90%", "Criterion", "Beyond criterion"
)
write.csv(
  terminal_display, file.path(table_dir, "projection-terminal-management.csv"),
  row.names = FALSE
)
terminal_caption <- paste0(
  "Terminal stock status for a projection ending in 2054 across 800 equally weighted model–recruitment combinations. Spawning biomass is averaged over 2051–2054, and fishing mortality is averaged over 2050–2053. ",
  "These MSY-based quantities are calculated by the assessment model's equilibrium-yield procedure; uncertainty includes model structure and stochastic recruitment, but not Hessian parameter draws."
)
terminal_rows <- vapply(seq_len(nrow(terminal_display)), function(i) {
  quantity <- c("$\\overline{SB}_{2051--2054}/SB_{MSY}$", "$\\overline{F}_{2050--2053}/F_{MSY}$")[[i]]
  values <- vapply(terminal_display[i, 2:4], latex_escape, character(1))
  criterion <- c(
    "$\\overline{SB}_{2051--2054}/SB_{MSY}<1$",
    "$\\overline{F}_{2050--2053}/F_{MSY}>1$"
  )[[i]]
  beyond <- latex_escape(terminal_display[i, 6L])
  paste0(paste(c(quantity, values, criterion, beyond), collapse = " & "), " \\\\")
}, character(1))
terminal_latex <- paste0(
  "% Requires \\usepackage{booktabs,tabularx}\n",
  "\\begin{table}[htbp]\n\\centering\n\\caption{", latex_escape(terminal_caption), "}\n",
  "\\small\n\\begin{tabularx}{\\textwidth}{@{}lrrrXr@{}}\n",
  "\\toprule\nQuantity & 10\\% & Median & 90\\% & Criterion & Beyond criterion \\\\\n\\midrule\n",
  paste(terminal_rows, collapse = "\n"),
  "\n\\bottomrule\n\\end{tabularx}\n\\end{table}"
)

current_kobe_probability <- category_probability(
  kobe_category(
    hybrid_management$sb_recent_sbmsy,
    hybrid_management$f_recent_fmsy
  ),
  kobe_levels
)
current_majuro_probability <- category_probability(
  majuro_category(
    hybrid_management$sb_recent_sb0,
    hybrid_management$f_recent_fmsy
  ),
  majuro_levels
)
terminal_kobe_probability <- category_probability(
  kobe_category(
    terminal_status$terminal_sb_sbmsy, terminal_status$terminal_f_fmsy
  ),
  kobe_levels
)
terminal_majuro_probability <- category_probability(
  majuro_category(
    terminal_status$sb_recent_sb0, terminal_status$terminal_f_fmsy
  ),
  majuro_levels
)
if (any(abs(c(
  sum(current_kobe_probability), sum(current_majuro_probability),
  sum(terminal_kobe_probability), sum(terminal_majuro_probability)
) - 1) > 1e-12)) {
  stop("Status-category probabilities do not sum to one.")
}
unlink(file.path(table_dir, "status-category-probabilities.csv"))

fit_display <- data.frame(
  Model = model_id_map$ensemble_id,
  `Objective function` = formatC(
    fit$objective_function[match(model_id_map$source_ensemble_id, fit$ensemble_id)],
    digits = 1, format = "f", big.mark = ","
  ),
  MGC = formatC(
    fit$maximum_gradient[match(model_id_map$source_ensemble_id, fit$ensemble_id)],
    digits = 2, format = "e"
  ),
  `Positive-definite Hessian` = ifelse(
    fit$positive_definite_hessian[match(model_id_map$source_ensemble_id, fit$ensemble_id)],
    "Yes", "No"
  ),
  check.names = FALSE
)
fit_display <- fit_display[order(fit_display$Model), ]
write.csv(fit_display, file.path(table_dir, "fit-hessian-summary.csv"), row.names = FALSE)
fit_caption <- paste0(
  "Fit and Hessian diagnostics for the 80 assessment models retained after applying MGC ≤ 1e-4. ",
  "Models are numbered sequentially from ensemble-001 to ensemble-080 after filtering. ",
  "Correlated estimation-uncertainty draws are included only for the 62 retained models with a positive-definite Hessian; the remaining 18 Hessians are not regularized."
)
fit_rows <- vapply(seq_len(nrow(fit_display)), function(i) {
  paste0(paste(vapply(fit_display[i, ], latex_escape, character(1)), collapse = " & "), " \\\\")
}, character(1))
fit_latex <- paste0(
  "% Requires \\usepackage{booktabs,longtable}\n",
  "\\begin{longtable}{@{}lrrl@{}}\n\\caption{", latex_escape(fit_caption), "}\\\\\n",
  "\\toprule\nModel & Objective function & MGC & Positive-definite Hessian \\\\\n\\midrule\n\\endfirsthead\n",
  "\\toprule\nModel & Objective function & MGC & Positive-definite Hessian \\\\\n\\midrule\n\\endhead\n",
  paste(fit_rows, collapse = "\n"), "\n\\bottomrule\n\\end{longtable}"
)

uncertainty_caption <- paste0(
  "Annual depletion (a), spawning potential (b), recruitment (c) and fishing mortality (d) across the 80 retained models. ",
  "For panels a–c, blue bands and the solid median combine structural uncertainty with 100 joint Hessian draws for each of 62 PDH models; the 18 Near-PDH models contribute central estimates only. Panel d shows structural uncertainty because annual fishing-mortality Hessian derivatives were not calculated. ",
  "Bands are pointwise central 50%, 80% and 95% intervals. Grey lines show central model trajectories and the orange dashed line is their structural median; in panel d it coincides with the solid median. ",
  "Individual trajectories can be examined in the <a href='https://github.com/PacificCommunity/ofp-sam-bet-2026-ensemble/releases/latest/download/bet-2026-ensemble-interactive-viewer.html'>interactive viewer</a>."
)
uncertainty_latex_caption <- paste0(
  "Annual depletion (a), spawning potential (b), recruitment (c) and fishing mortality (d) across the 80 retained models. ",
  "For panels a--c, blue bands and the solid median combine structural uncertainty with 100 joint Hessian draws for each of 62 PDH models; the 18 Near-PDH models contribute central estimates only. Panel d shows structural uncertainty because annual fishing-mortality Hessian derivatives were not calculated. ",
  "Bands are pointwise central 50\\%, 80\\% and 95\\% intervals. Grey lines show central model trajectories and the orange dashed line is their structural median; in panel d it coincides with the solid median. Individual trajectories can be examined in the interactive viewer accompanying this report."
)
plain_latex_caption <- function(value) {
  value <- gsub("<[^>]+>", "", value)
  replacements <- c(
    "F2050–2053/FMSY" = "@@FTERMINAL@@",
    "SB2051–2054/SBMSY" = "@@SBTERMINALMSY@@",
    "SBt/SBF=0,t" = "@@SBTDEP@@",
    "SBt/SBMSY,t" = "@@SBTMSY@@",
    "Ft/FMSY,t" = "@@FTMSY@@",
    "SBrecent/SBF=0" = "@@SBRECENTDEP@@",
    "SBrecent/SBMSY" = "@@SBRECENTMSY@@",
    "Frecent/FMSY" = "@@FRECENTMSY@@",
    "F/FMSY" = "@@RATIOF@@",
    "Drecent" = "@@DRECENT@@",
    "SBMSY" = "@@SBMSY@@",
    "FMSY" = "@@FMSY@@"
  )
  for (target in names(replacements)) {
    value <- gsub(target, replacements[[target]], value, fixed = TRUE)
  }
  value <- latex_escape(value)
  replacements <- c(
    "@@FTERMINAL@@" = "$\\overline{F}_{2050--2053}/F_{MSY}$",
    "@@SBTERMINALMSY@@" = "$\\overline{SB}_{2051--2054}/SB_{MSY}$",
    "@@SBTDEP@@" = "$SB_t/SB_{F=0,t}$",
    "@@SBTMSY@@" = "$SB_t/SB_{MSY,t}$",
    "@@FTMSY@@" = "$F_t/F_{MSY,t}$",
    "@@SBRECENTDEP@@" = "$SB_{recent}/SB_{F=0}$",
    "@@SBRECENTMSY@@" = "$SB_{recent}/SB_{MSY}$",
    "@@FRECENTMSY@@" = "$F_{recent}/F_{MSY}$",
    "@@RATIOF@@" = "$F/F_{MSY}$",
    "@@DRECENT@@" = "$D_{recent}$",
    "@@SBMSY@@" = "$SB_{MSY}$",
    "@@FMSY@@" = "$F_{MSY}$"
  )
  for (target in names(replacements)) {
    value <- gsub(target, replacements[[target]], value, fixed = TRUE)
  }
  value
}

current_status_caption <- paste0(
  "Current status based on mean spawning biomass for 2021–2024 and mean fishing mortality for 2020–2023. Panel (a) is relative to SBMSY and FMSY; panel (b) uses depletion with an LRP of 0.20. ",
  "Points are the 80 central model estimates; filled and open symbols distinguish PDH and Near-PDH fits. Nested 50%, 80% and 95% HDRs combine structural uncertainty with available joint Hessian uncertainty. ",
  "Background colours follow the Kobe four-category and Majuro three-category definitions. Labels give the probability in each category from the same equal-model-weight distribution used for the combined HDRs."
)
dynamic_status_caption <- paste0(
  "Time-dynamic Majuro and Kobe trajectories for the 2026 diagnostic model and the 80-model ensemble. Panels (a) and (b) are Majuro trajectories for the diagnostic model and the coordinate-wise annual ensemble median, respectively; panels (c) and (d) are the corresponding Kobe trajectories. ",
  "The axes are SBt/SBF=0,t and Ft/FMSY,t for Majuro, and SBt/SBMSY,t and Ft/FMSY,t for Kobe. Colour shows year and the outlined point marks 2024. The ensemble trajectory summarizes the 80 retained central models and therefore represents structural medians rather than Hessian estimation uncertainty."
)
continuous_axis_caption <- paste0(
  "All-model uncertainty distributions of Frecent/FMSY (top) and SBrecent/SBF=0 (bottom), grouped by quartiles of steepness and natural mortality at the reference length. ",
  "Violin width represents density and the inset box shows the interquartile range and median. Each model has equal total weight; PDH models contribute joint Hessian draws and Near-PDH models contribute their central estimates. ",
  "Because all ensemble axes vary jointly, differences among groups are descriptive and not one-factor causal effects."
)
discrete_axis_a_caption <- paste0(
  "All-model uncertainty distributions of Frecent/FMSY (top) and SBrecent/SBF=0 (bottom), grouped by fixed tag overdispersion and tag-mixing cutoff. ",
  "Each assessment model has equal total weight. Violins show density and boxes show the median and interquartile range; comparisons are marginal descriptions of a jointly varying ensemble."
)
discrete_axis_b_caption <- paste0(
  "All-model uncertainty distributions of Frecent/FMSY (top) and SBrecent/SBF=0 (bottom), grouped by tag-reporting treatment and paired annual effort-creep rates. ",
  "Each assessment model has equal total weight. Violins show density and boxes show the median and interquartile range; comparisons are marginal descriptions rather than controlled one-factor sensitivities."
)

projection_key_caption <- paste0(
  "Projection risk relative to the LRP and model-specific 2012–2015 objective (a), and current versus terminal F/FMSY (b). ",
  "Points in panel b are medians; dark, medium and light ranges are the central 50%, 80% and 95% intervals. ",
  "Projected probabilities and the mean 2050–2053 F/FMSY combine model structure and stochastic recruitment without Hessian draws; current Frecent/FMSY includes available Hessian uncertainty."
)
projection_catch_caption <- paste0(
  "Projected annual biomass catch relative to equilibrium MSY, 2025–2054. The median and central 50%, 80% and 95% intervals use all 800 model–recruitment realizations; ten thin lines show a reproducible random sample. ",
  "Number-conditioned fisheries are converted using model-predicted mean weight; number and weight inputs are not summed."
)
terminal_status_caption <- paste0(
  "Terminal Kobe (a) and Majuro (b) status for a projection ending in 2054 across 800 equally weighted model–recruitment combinations. Biomass is mean SB over 2051–2054; fishing mortality is averaged over 2050–2053 and divided by FMSY. Kobe biomass is relative to SBMSY, whereas Majuro depletion is relative to SBF=0 with an LRP of 0.20. ",
  "Points are individual combinations, nested shading gives the 50%, 80% and 95% bivariate kernel HDRs, and labels give the percentage in each background category. Projection uncertainty includes model structure and stochastic recruitment, not Hessian parameter draws."
)
regional_spawning_caption <- paste0(
  "Spawning potential for Regions 1–5 and all regions, joining estimates through 2024 to projections for 2025–2054. Medians and central 50%, 80% and 95% intervals use all 800 model–recruitment realizations; ten thin projection lines show a reproducible random sample. Historical intervals show structural uncertainty; projections add recruitment variability without Hessian parameter draws. Units are thousands of metric tonnes."
)
regional_depletion_caption <- paste0(
  "Depletion for Regions 1–5 and all regions, joining estimates through 2024 to projections for 2025–2054. Depletion is the four-year mean spawning biomass divided by the preceding ten-year mean no-fishing biomass. Medians and central 50%, 80% and 95% intervals use all 800 model–recruitment realizations; ten thin projection lines show a reproducible random sample. The 0.20 line is the stock-wide LRP shown for reference, not a region-specific threshold."
)

catch_below_years <- sum(catch_msy_summary$median < 1)
recovery_note <- sprintf(
  paste0(
    "<div class='note'><strong>Recovery audit.</strong> Mean projected depletion in 2040 ranges from %.3f to %.3f across steepness quartiles. ",
    "The annual median Catch/MSY ranges from %.3f to %.3f and is below 1 in %d of 30 projection years. ",
    "The terminal <i>F</i><sub>2050–2053</sub>/<i>F</i><sub>MSY</sub> median is %.3f, with %s of model–recruitment combinations above 1. ",
    "These diagnostics show that the recovery trajectory is a result of the specified catch-conditioned scenario and stochastic recruitment; steepness modifies its magnitude but is not its sole cause. This is a scenario evaluation, not a management recommendation.</div>"
  ),
  min(projection_driver_summary$depletion_2040),
  max(projection_driver_summary$depletion_2040),
  min(catch_msy_summary$median), max(catch_msy_summary$median),
  catch_below_years,
  stats::median(terminal$terminal_f_fmsy),
  scales::percent(mean(terminal$terminal_f_fmsy > 1), accuracy = 0.1)
)

main_insert <- paste0(
  figure_block("combined-uncertainty", "Structural and available estimation uncertainty", uncertainty_caption, uncertainty_latex_caption, uncertainty_files),
  copy_table_block("estimation-management-summary", "Management quantities with available estimation uncertainty", management_uncertainty_caption, management_uncertainty_display, management_uncertainty_latex),
  copy_table_block("estimation-management-risk", "Status probabilities with available estimation uncertainty", management_risk_caption, management_risk_display, management_risk_latex),
  figure_block("combined-current-status", "Kobe and Majuro status", current_status_caption, plain_latex_caption(current_status_caption), current_status_files),
  figure_block("dynamic-current-status", "Time-dynamic Kobe and Majuro status", dynamic_status_caption, plain_latex_caption(dynamic_status_caption), dynamic_status_files),
  figure_block("projection-key-quantities", "Projection summary", projection_key_caption, plain_latex_caption(projection_key_caption), projection_key_files),
  figure_block("regional-depletion", "Regional depletion", regional_depletion_caption, plain_latex_caption(regional_depletion_caption), regional_depletion_files),
  copy_table_block("projection-summary", "Projection summary", projection_caption, projection_display, projection_latex),
  copy_table_block("projection-terminal-management", "Terminal management quantities", terminal_caption, terminal_display, terminal_latex)
)

supplementary_insert <- paste0(
  copy_table_block("structural-reference-points", "Supporting structural reference points", structural_reference_caption, structural_reference_display, structural_reference_latex),
  copy_table_block("estimation-uncertainty-audit", "Estimation-uncertainty and Monte Carlo audit", uncertainty_audit_caption, uncertainty_audit_display, uncertainty_audit_latex),
  figure_block("continuous-axis-status", "Management quantities by continuous uncertainty axes", continuous_axis_caption, plain_latex_caption(continuous_axis_caption), continuous_axis_files),
  figure_block("discrete-axis-status-a", "Management quantities by tag uncertainty axes", discrete_axis_a_caption, plain_latex_caption(discrete_axis_a_caption), discrete_axis_a_files),
  figure_block("discrete-axis-status-b", "Management quantities by reporting and effort axes", discrete_axis_b_caption, plain_latex_caption(discrete_axis_b_caption), discrete_axis_b_files),
  figure_block("terminal-status", "Terminal projection status", terminal_status_caption, plain_latex_caption(terminal_status_caption), terminal_status_files),
  figure_block("regional-spawning", "Regional spawning potential", regional_spawning_caption, plain_latex_caption(regional_spawning_caption), regional_spawning_files),
  copy_table_block("fit-hessian-summary", "Fit and Hessian diagnostics", fit_caption, fit_display, fit_latex)
)

report_path <- file.path(output_dir, "bet-2026-ensemble-report.html")
html <- paste(readLines(report_path, warn = FALSE), collapse = "\n")
main_marker <- "<div id='analysis-results'></div>"
reference_marker <- "<section class='refs'><h2>References</h2>"
if (!grepl(main_marker, html, fixed = TRUE) ||
    !grepl(reference_marker, html, fixed = TRUE)) {
  stop("Could not locate report insertion points.", call. = FALSE)
}
html <- sub(main_marker, main_insert, html, fixed = TRUE)
html <- sub(
  reference_marker, paste0(supplementary_insert, reference_marker),
  html, fixed = TRUE
)
writeLines(html, report_path, useBytes = TRUE)

manifest_files <- c(
  "bet-2026-ensemble-report.html",
  file.path("figures", basename(c(
    uncertainty_files, current_status_files,
    continuous_axis_files, discrete_axis_a_files, discrete_axis_b_files,
    projection_key_files,
    terminal_status_files, regional_spawning_files,
    dynamic_status_files, regional_depletion_files
  ))),
  "tables/projection-summary.csv", "tables/fit-hessian-summary.csv",
  "tables/projection-terminal-management.csv",
  "tables/estimation-management-summary.csv",
  "tables/estimation-management-intervals.csv",
  "tables/estimation-management-risk.csv",
  "tables/structural-reference-points.csv",
  "tables/estimation-uncertainty-audit.csv",
  "tables/management-summary.csv", "tables/management-risk.csv",
  "tables/ensemble-fit-diagnostics.csv", "tables/model-id-map.csv"
)
existing_figures <- list.files(figure_dir, pattern = "\\.(png|pdf)$", full.names = FALSE)
manifest_files <- unique(c(manifest_files, file.path("figures", existing_figures)))
manifest <- data.frame(file = manifest_files, stringsAsFactors = FALSE)
manifest$sha256 <- vapply(file.path(output_dir, manifest$file), function(path) {
  output <- system2("sha256sum", path, stdout = TRUE)
  strsplit(output[[1]], "[[:space:]]+")[[1]][[1]]
}, character(1))
write.csv(manifest, file.path(output_dir, "report-manifest.csv"), row.names = FALSE)

cat(sprintf(
  "Added Hessian uncertainty and %d-model stochastic projections to the report.\n",
  projection$projection_complete_models
))
