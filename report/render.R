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

series_all <- readRDS("data/ensemble/ensemble-timeseries.rds")
fit_all <- read.csv("data/ensemble/fit-diagnostics.csv", check.names = FALSE)
management_all <- read.csv("data/ensemble/management-quantities.csv", check.names = FALSE)
design_all <- read.csv("data/ensemble/successful-model-design.csv", check.names = FALSE)
planned_design <- read.csv("design/model-draws.csv", check.names = FALSE)
parameters <- read.csv("design/distribution-parameters.csv", check.names = FALSE)

output_dir <- Sys.getenv("REPORT_OUTPUT_DIR", "results")
figure_dir <- file.path(output_dir, "figures")
table_dir <- file.path(output_dir, "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
obsolete_design_figures <- file.path(
  figure_dir,
  c(
    "completed-model-continuous-design.png",
    "completed-model-continuous-design.pdf",
    "completed-model-discrete-design.png",
    "completed-model-discrete-design.pdf"
  )
)
invisible(file.remove(
  obsolete_design_figures[file.exists(obsolete_design_figures)]
))

fit_all$mgc_pass <- fit_all$maximum_gradient <= 1e-4
fit_all$hessian_qc <- ifelse(
  fit_all$positive_definite_hessian, "PDH", "Near-PDH"
)
fit_all$ensemble_inclusion <- ifelse(
  fit_all$mgc_pass, "Included", "Excluded: MGC > 1e-4"
)
included_ids <- sort(fit_all$ensemble_id[fit_all$mgc_pass])
excluded_ids <- sort(fit_all$ensemble_id[!fit_all$mgc_pass])
if (length(included_ids) != 80L || length(excluded_ids) != 8L) {
  stop("The locked MGC <= 1e-4 rule must retain 80 of 88 completed models.")
}
model_id_map <- sequential_model_map(included_ids)
if (nrow(planned_design) != 100L || anyDuplicated(planned_design$ensemble_id) ||
    !setequal(planned_design$ensemble_id, sprintf("ensemble-%03d", seq_len(100L)))) {
  stop("The planned ensemble design must contain exactly 100 unique configurations.")
}

fit <- fit_all[fit_all$ensemble_id %in% included_ids, , drop = FALSE]
series <- series_all[series_all$ensemble_id %in% included_ids, , drop = FALSE]
management <- management_all[
  management_all$ensemble_id %in% included_ids, , drop = FALSE
]
design <- design_all[design_all$ensemble_id %in% included_ids, , drop = FALSE]
series <- merge(series, fit[c("ensemble_id", "mgc_pass", "hessian_qc")], by = "ensemble_id")
management <- merge(management, fit, by = "ensemble_id")
design <- merge(design, fit[c("ensemble_id", "mgc_pass", "hessian_qc")], by = "ensemble_id")

n_models <- nrow(fit)
n_pdh <- sum(fit$positive_definite_hessian)
n_near <- n_models - n_pdh
n_completed <- nrow(fit_all)
n_excluded <- length(excluded_ids)
n_additional_mgc_excluded <- 2L
n_total_mgc_excluded <- n_excluded + n_additional_mgc_excluded

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
      legend.key.width = grid::unit(1.15, "cm"),
      plot.tag = ggplot2::element_text(face = "bold", colour = "#183246", size = 12),
      plot.margin = ggplot2::margin(7, 9, 7, 8)
    )
}

quantile_series <- function(data, column) {
  split_values <- split(data[[column]], data$year)
  data.frame(
    year = as.integer(names(split_values)),
    q025 = vapply(split_values, stats::quantile, numeric(1), probs = 0.025, names = FALSE),
    q10 = vapply(split_values, stats::quantile, numeric(1), probs = 0.10, names = FALSE),
    q25 = vapply(split_values, stats::quantile, numeric(1), probs = 0.25, names = FALSE),
    median = vapply(split_values, stats::median, numeric(1)),
    q75 = vapply(split_values, stats::quantile, numeric(1), probs = 0.75, names = FALSE),
    q90 = vapply(split_values, stats::quantile, numeric(1), probs = 0.90, names = FALSE),
    q975 = vapply(split_values, stats::quantile, numeric(1), probs = 0.975, names = FALSE)
  )
}

metric_specs <- list(
  list(column = "depletion", label = bquote(italic(SB)[italic(t)] / italic(SB)[italic(F) == 0]), lrp = TRUE),
  list(column = "recruitment", label = "Recruitment (millions of fish)", lrp = FALSE),
  list(column = "spawning_potential", label = bquote(Spawning~potential~(10^3~plain(MT))), lrp = FALSE),
  list(column = "fishing_mortality", label = bquote(italic(F)~(year^{-1})), lrp = FALSE)
)

trajectory_panels <- lapply(metric_specs, function(spec) {
  summary_all <- quantile_series(series, spec$column)
  p <- ggplot2::ggplot(
    series,
    ggplot2::aes(x = .data$year, y = .data[[spec$column]], group = .data$ensemble_id)
  ) +
    ggplot2::geom_line(colour = "#8D9CA3", linewidth = 0.21, alpha = 0.18) +
    ggplot2::geom_ribbon(
      data = summary_all,
      ggplot2::aes(x = .data$year, ymin = .data$q025, ymax = .data$q975),
      inherit.aes = FALSE, fill = "#D5E9ED", alpha = 0.44
    ) +
    ggplot2::geom_ribbon(
      data = summary_all,
      ggplot2::aes(x = .data$year, ymin = .data$q10, ymax = .data$q90),
      inherit.aes = FALSE, fill = "#9CCFD8", alpha = 0.56
    ) +
    ggplot2::geom_ribbon(
      data = summary_all,
      ggplot2::aes(x = .data$year, ymin = .data$q25, ymax = .data$q75),
      inherit.aes = FALSE, fill = "#53AAB9", alpha = 0.66
    ) +
    ggplot2::geom_line(
      data = summary_all,
      ggplot2::aes(x = .data$year, y = .data$median),
      inherit.aes = FALSE, colour = "#07566B", linewidth = 0.92
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(1960, 2020, 20),
      expand = ggplot2::expansion(mult = c(0.01, 0.015))
    ) +
    ggplot2::labs(x = "Year", y = spec$label) +
    ggplot2::coord_cartesian(ylim = c(0, NA)) +
    theme_report()
  if (spec$lrp) {
    p <- p +
      ggplot2::geom_hline(yintercept = 0.2, colour = "#B83232", linewidth = 0.56, linetype = "33") +
      ggplot2::annotate(
        "text", x = 1955, y = 0.215, label = "LRP", colour = "#B83232",
        hjust = 0, size = 3.15, fontface = "bold"
      )
  }
  p
})

trajectory_stock_plot <- trajectory_panels[[1]] / trajectory_panels[[3]] +
  patchwork::plot_annotation(tag_levels = "a")
trajectory_process_plot <- trajectory_panels[[2]] / trajectory_panels[[4]] +
  patchwork::plot_annotation(tag_levels = "a")

distribution_panel <- function(data, column, label, reference, reference_label) {
  maximum <- max(data[[column]], na.rm = TRUE)
  p <- ggplot2::ggplot(data, ggplot2::aes(x = 1, y = .data[[column]])) +
    ggplot2::geom_boxplot(
      width = 0.28, outlier.shape = NA, fill = "#B8DDE5",
      colour = "#07566B", linewidth = 0.68
    ) +
    ggplot2::geom_jitter(
      width = 0.075, height = 0, size = 1.35, alpha = 0.50, colour = "#167A8B"
    ) +
    ggplot2::scale_x_continuous(breaks = NULL) +
    ggplot2::labs(x = NULL, y = label) +
    ggplot2::coord_cartesian(ylim = c(0, maximum * 1.08)) +
    theme_report(11.2)
  if (is.finite(reference)) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = reference, colour = "#B83232", linewidth = 0.55, linetype = "33"
      ) +
      ggplot2::annotate(
        "text", x = 0.82, y = reference, label = reference_label,
        colour = "#B83232", hjust = 0, vjust = -0.45, size = 3.0, fontface = "bold"
      )
  }
  p
}

management_plot <- (
  distribution_panel(
    management, "sb_recent_sb0",
    expression(italic(SB)[recent] / italic(SB)[italic(F) == 0]), 0.2, "LRP"
  ) |
  distribution_panel(
    management, "sb_recent_sbmsy",
    expression(italic(SB)[recent] / italic(SB)[MSY]), 1, "1.0"
  ) |
  distribution_panel(
    management, "f_recent_fmsy",
    expression(italic(F)[recent] / italic(F)[MSY]), 1, "1.0"
  ) |
  distribution_panel(
    management, "recent_historical_target_ratio",
    expression(italic(D)[recent] / bar(italic(D))[2012-2015]), 1, "2012–2015 objective"
  )
) + patchwork::plot_annotation(tag_levels = "a")

status_colours <- c("PDH" = "#167A5B", "Near-PDH" = "#A55B20")
tau_colours <- c("1.2" = "#0072B2", "1.3" = "#009E73", "1.4" = "#D55E00")
status_band_colours <- c("#DFEFF2", "#A8D5DC", "#56AAB7")

hdr_surface <- function(data, x, y, probabilities = c(0.95, 0.80, 0.50), n = 180L) {
  x_values <- data[[x]]
  y_values <- data[[y]]
  keep <- is.finite(x_values) & is.finite(y_values)
  x_values <- x_values[keep]
  y_values <- y_values[keep]
  if (length(x_values) < 10L || stats::sd(x_values) <= 0 || stats::sd(y_values) <= 0) {
    stop("Insufficient variation for a two-dimensional HDR.")
  }
  x_bandwidth <- MASS::bandwidth.nrd(x_values)
  y_bandwidth <- MASS::bandwidth.nrd(y_values)
  if (!is.finite(x_bandwidth) || x_bandwidth <= 0 ||
      !is.finite(y_bandwidth) || y_bandwidth <= 0) {
    stop("Invalid normal-reference bandwidth for a two-dimensional HDR.")
  }
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
  thresholds <- sort(thresholds)
  if (anyDuplicated(signif(thresholds, 12L))) {
    stop("HDR density thresholds are not distinct.")
  }
  surface <- expand.grid(x = estimate$x, y = estimate$y)
  surface$density <- density
  attr(surface, "breaks") <- c(
    thresholds, max(density) + .Machine$double.eps * max(density)
  )
  surface
}

management$tau_factor <- factor(format(management$tau, nsmall = 1), levels = names(tau_colours))
management$mgc_alpha <- ifelse(management$maximum_gradient <= 1e-4, 0.86, 0.45)
kobe_hdr <- hdr_surface(management, "sb_recent_sbmsy", "f_recent_fmsy")
majuro_hdr <- hdr_surface(management, "sb_recent_sb0", "f_recent_fmsy")
historical_objective_hdr <- hdr_surface(
  management, "historical_target_depletion", "sb_recent_sb0"
)

status_theme <- theme_report(10.8) +
  ggplot2::theme(legend.position = "bottom", legend.box = "horizontal")

kobe_plot <- ggplot2::ggplot(
  management,
  ggplot2::aes(
    x = .data$sb_recent_sbmsy, y = .data$f_recent_fmsy,
    colour = .data$tau_factor, shape = .data$hessian_qc
  )
) +
  ggplot2::annotate("rect", xmin = 1, xmax = Inf, ymin = -Inf, ymax = 1, fill = "#2a9d8f", alpha = 0.72) +
  ggplot2::annotate("rect", xmin = -Inf, xmax = 1, ymin = -Inf, ymax = 1, fill = "#e9c46a", alpha = 0.72) +
  ggplot2::annotate("rect", xmin = 1, xmax = Inf, ymin = 1, ymax = Inf, fill = "#f4a261", alpha = 0.70) +
  ggplot2::annotate("rect", xmin = -Inf, xmax = 1, ymin = 1, ymax = Inf, fill = "#e76f51", alpha = 0.72) +
  ggplot2::geom_contour_filled(
    data = kobe_hdr,
    ggplot2::aes(x = .data$x, y = .data$y, z = .data$density),
    breaks = attr(kobe_hdr, "breaks"), alpha = 0.68,
    colour = "#4F7C86", linewidth = 0.28, inherit.aes = FALSE
  ) +
  ggplot2::geom_hline(yintercept = 1, colour = "#1f2937", linewidth = 0.70) +
  ggplot2::geom_vline(xintercept = 1, colour = "#1f2937", linewidth = 0.70) +
  ggplot2::geom_point(ggplot2::aes(alpha = .data$mgc_alpha), size = 2.45, stroke = 0.75) +
  ggplot2::scale_colour_manual(values = tau_colours, name = expression(tau)) +
  ggplot2::scale_fill_manual(
    values = status_band_colours,
    labels = c("95% HDR", "80% HDR", "50% HDR"), name = "Structural HDR"
  ) +
  ggplot2::scale_shape_manual(values = c("PDH" = 16, "Near-PDH" = 1), name = "Hessian") +
  ggplot2::scale_alpha_identity() +
  ggplot2::labs(
    x = expression(italic(SB)[recent] / italic(SB)[MSY]),
    y = expression(italic(F)[recent] / italic(F)[MSY])
  ) +
  ggplot2::coord_cartesian(xlim = c(0, NA), ylim = c(0, NA)) + status_theme

majuro_plot <- ggplot2::ggplot(
  management,
  ggplot2::aes(
    x = .data$sb_recent_sb0, y = .data$f_recent_fmsy,
    colour = .data$tau_factor, shape = .data$hessian_qc
  )
) +
  ggplot2::annotate("rect", xmin = -Inf, xmax = 0.2, ymin = -Inf, ymax = Inf, fill = "#e76f51", alpha = 0.72) +
  ggplot2::annotate("rect", xmin = 0.2, xmax = Inf, ymin = -Inf, ymax = 1, fill = "#2a9d8f", alpha = 0.72) +
  ggplot2::annotate("rect", xmin = 0.2, xmax = Inf, ymin = 1, ymax = Inf, fill = "#f4a261", alpha = 0.70) +
  ggplot2::geom_contour_filled(
    data = majuro_hdr,
    ggplot2::aes(x = .data$x, y = .data$y, z = .data$density),
    breaks = attr(majuro_hdr, "breaks"), alpha = 0.68,
    colour = "#4F7C86", linewidth = 0.28, inherit.aes = FALSE
  ) +
  ggplot2::annotate(
    "segment", x = 0.2, xend = max(management$sb_recent_sb0) * 1.04,
    y = 1, yend = 1, colour = "#1f2937", linewidth = 0.70
  ) +
  ggplot2::geom_vline(xintercept = 0.2, colour = "#1f2937", linewidth = 0.70) +
  ggplot2::geom_point(ggplot2::aes(alpha = .data$mgc_alpha), size = 2.45, stroke = 0.75) +
  ggplot2::scale_colour_manual(values = tau_colours, name = expression(tau)) +
  ggplot2::scale_fill_manual(
    values = status_band_colours,
    labels = c("95% HDR", "80% HDR", "50% HDR"), name = "Structural HDR"
  ) +
  ggplot2::scale_shape_manual(values = c("PDH" = 16, "Near-PDH" = 1), name = "Hessian") +
  ggplot2::scale_alpha_identity() +
  ggplot2::labs(
    x = expression(italic(SB)[recent] / italic(SB)[italic(F) == 0]),
    y = expression(italic(F)[recent] / italic(F)[MSY])
  ) +
  ggplot2::coord_cartesian(xlim = c(0, NA), ylim = c(0, NA)) + status_theme

target_limit <- max(c(
  management$historical_target_depletion,
  management$sb_recent_sb0
)) * 1.06
historical_plot <- ggplot2::ggplot(
  management,
  ggplot2::aes(
    x = .data$historical_target_depletion, y = .data$sb_recent_sb0,
    colour = .data$tau_factor, shape = .data$hessian_qc
  )
) +
  ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0.2, fill = "#F2D6D3", alpha = 0.54) +
  ggplot2::geom_contour_filled(
    data = historical_objective_hdr,
    ggplot2::aes(x = .data$x, y = .data$y, z = .data$density),
    breaks = attr(historical_objective_hdr, "breaks"), alpha = 0.68,
    colour = "#4F7C86", linewidth = 0.28, inherit.aes = FALSE
  ) +
  ggplot2::geom_abline(slope = 1, intercept = 0, colour = "#5D6C73", linewidth = 0.48, linetype = "33") +
  ggplot2::geom_hline(yintercept = 0.2, colour = "#B83232", linewidth = 0.55, linetype = "33") +
  ggplot2::geom_point(ggplot2::aes(alpha = .data$mgc_alpha), size = 2.45, stroke = 0.75) +
  ggplot2::scale_colour_manual(values = tau_colours, name = expression(tau)) +
  ggplot2::scale_fill_manual(
    values = status_band_colours,
    labels = c("95% HDR", "80% HDR", "50% HDR"), name = "Structural HDR"
  ) +
  ggplot2::scale_shape_manual(values = c("PDH" = 16, "Near-PDH" = 1), name = "Hessian") +
  ggplot2::scale_alpha_identity() +
  ggplot2::labs(
    x = expression(bar(italic(D))[2012-2015]),
    y = expression(italic(D)[recent])
  ) +
  ggplot2::coord_equal(xlim = c(0, target_limit), ylim = c(0, target_limit)) + status_theme

status_plot <- patchwork::wrap_plots(
  list(kobe_plot, majuro_plot, historical_plot), ncol = 3, guides = "collect"
) +
  patchwork::plot_annotation(tag_levels = "a") &
  ggplot2::theme(
    legend.position = "bottom", legend.box = "horizontal",
    legend.text = ggplot2::element_text(size = 8.8),
    legend.key.width = grid::unit(0.80, "cm")
  )

parameter_value <- function(axis, parameter) {
  row <- parameters[parameters$axis == axis & parameters$parameter == parameter, , drop = FALSE]
  if (nrow(row) != 1L) stop("Missing design parameter: ", axis, " / ", parameter)
  as.numeric(row$value[[1]])
}

h_lower <- parameter_value("Steepness", "lower")
h_upper <- parameter_value("Steepness", "upper")
h_alpha <- parameter_value("Steepness", "beta_alpha")
h_beta <- parameter_value("Steepness", "beta_beta")
h_grid <- data.frame(x = seq(0.60, 1.00, length.out = 400))
h_grid$density <- stats::dbeta((h_grid$x - h_lower) / (h_upper - h_lower), h_alpha, h_beta) /
  (h_upper - h_lower)

m_lower <- parameter_value("Ensemble quarterly M at reference length", "lower")
m_upper <- parameter_value("Ensemble quarterly M at reference length", "upper")
m_mu <- parameter_value("Ensemble quarterly M at reference length", "logit_mean")
m_sigma <- parameter_value("Ensemble quarterly M at reference length", "logit_sd")
m_grid <- data.frame(x = seq(m_lower + 1e-5, m_upper - 1e-5, length.out = 500))
m_scaled <- (m_grid$x - m_lower) / (m_upper - m_lower)
m_grid$selected <- stats::dnorm(stats::qlogis(m_scaled), m_mu, m_sigma) /
  (m_scaled * (1 - m_scaled) * (m_upper - m_lower))
m_grid$hamel_cope <- stats::dlnorm(
  m_grid$x,
  log(parameter_value("Hamel-Cope model-aligned M0", "quarterly_median")),
  parameter_value("Hamel-Cope Amax prior", "log_sd")
)
m_tag <- parameter_value("Tag-analysis M0", "estimate")
m_tag_lower <- parameter_value("Tag-analysis M0", "lower_90")
m_tag_upper <- parameter_value("Tag-analysis M0", "upper_90")

design_fill_colours <- c("Planned 100" = "#D2A447", "Included 80" = "#16899A")
paired_histogram_counts <- function(planned, included, breaks) {
  one_set <- function(values, label) {
    estimate <- graphics::hist(values, breaks = breaks, plot = FALSE)
    data.frame(
      midpoint = estimate$mids,
      count = estimate$counts,
      bin_width = diff(estimate$breaks),
      set = label
    )
  }
  result <- rbind(
    one_set(planned, "Planned 100"),
    one_set(included, "Included 80")
  )
  result$set <- factor(result$set, levels = names(design_fill_colours))
  result
}

h_histogram <- paired_histogram_counts(
  planned_design$steepness, design$steepness,
  seq(h_lower, h_upper, length.out = 11L)
)
m_histogram <- paired_histogram_counts(
  planned_design$m_age40_quarterly, design$m_age40_quarterly,
  seq(m_lower, m_upper, length.out = 11L)
)
h_bin_width <- stats::median(h_histogram$bin_width)
m_bin_width <- stats::median(m_histogram$bin_width)
h_grid$count <- h_grid$density * nrow(planned_design) * h_bin_width
m_grid$selected_count <- m_grid$selected * nrow(planned_design) * m_bin_width
m_grid$hamel_cope_count <- m_grid$hamel_cope * nrow(planned_design) * m_bin_width

h_panel <- ggplot2::ggplot(
  h_histogram,
  ggplot2::aes(x = .data$midpoint, y = .data$count, fill = .data$set)
) +
  ggplot2::geom_col(
    data = h_histogram[h_histogram$set == "Planned 100", ],
    width = h_bin_width * 0.90, colour = "white", linewidth = 0.18,
    alpha = 0.88
  ) +
  ggplot2::geom_col(
    data = h_histogram[h_histogram$set == "Included 80", ],
    width = h_bin_width * 0.58, colour = "white", linewidth = 0.18,
    alpha = 0.96
  ) +
  ggplot2::geom_line(
    data = h_grid, ggplot2::aes(x = .data$x, y = .data$count),
    inherit.aes = FALSE, colour = "#0072B2", linewidth = 0.95
  ) +
  ggplot2::geom_rug(
    data = design, ggplot2::aes(x = .data$steepness),
    inherit.aes = FALSE, colour = "#0072B2", alpha = 0.35, sides = "b"
  ) +
  ggplot2::scale_fill_manual(
    values = design_fill_colours,
    name = NULL
  ) +
  ggplot2::labs(x = "Steepness, h", y = "Models per bin") + theme_report(10.4)

m_y <- max(c(m_grid$selected_count, m_grid$hamel_cope_count), na.rm = TRUE)
m_tag_interval <- data.frame(
  x = m_tag_lower, xend = m_tag_upper,
  y = 0.06 * m_y, yend = 0.06 * m_y
)
m_tag_point <- data.frame(x = m_tag, y = 0.06 * m_y)
m_panel <- ggplot2::ggplot(
  m_histogram,
  ggplot2::aes(x = .data$midpoint, y = .data$count, fill = .data$set)
) +
  ggplot2::geom_col(
    data = m_histogram[m_histogram$set == "Planned 100", ],
    width = m_bin_width * 0.90, colour = "white", linewidth = 0.18,
    alpha = 0.88
  ) +
  ggplot2::geom_col(
    data = m_histogram[m_histogram$set == "Included 80", ],
    width = m_bin_width * 0.58, colour = "white", linewidth = 0.18,
    alpha = 0.96
  ) +
  ggplot2::geom_line(
    data = m_grid, ggplot2::aes(x = .data$x, y = .data$selected_count, colour = "Selected distribution"),
    inherit.aes = FALSE, linewidth = 0.95
  ) +
  ggplot2::geom_line(
    data = m_grid, ggplot2::aes(x = .data$x, y = .data$hamel_cope_count, colour = "Hamel–Cope, scaled to M₀"),
    inherit.aes = FALSE, linewidth = 0.72, linetype = "22"
  ) +
  ggplot2::geom_segment(
    data = m_tag_interval,
    ggplot2::aes(
      x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend,
      colour = "Tag analysis (90% CI)"
    ), inherit.aes = FALSE, linewidth = 1.0
  ) +
  ggplot2::geom_point(
    data = m_tag_point,
    ggplot2::aes(x = .data$x, y = .data$y, colour = "Tag analysis (90% CI)"),
    inherit.aes = FALSE, size = 2.2
  ) +
  ggplot2::geom_rug(
    data = design, ggplot2::aes(x = .data$m_age40_quarterly),
    inherit.aes = FALSE, colour = "#D66B00", alpha = 0.34, sides = "b"
  ) +
  ggplot2::scale_colour_manual(
    values = c(
      "Selected distribution" = "#D66B00",
      "Hamel–Cope, scaled to M₀" = "#5E6366",
      "Tag analysis (90% CI)" = "#168C67"
    ),
    name = NULL
  ) +
  ggplot2::scale_fill_manual(
    values = design_fill_colours,
    name = NULL, guide = "none"
  ) +
  ggplot2::labs(
    x = expression(italic(M)[0]~at~italic(L)(40.5)~(quarter^{-1})),
    y = "Models per bin"
  ) + theme_report(10.4)

# Standalone age-specific comparison used by the assessment report. All three
# curves use the Diagnostic growth schedule and fixed Lorenzen exponent (-1),
# so they differ only in the M0 evidence applied at the reference length.
m_diagnostic <- parameter_value("Ensemble quarterly M at reference length", "median")
m_hamel_cope <- parameter_value("Hamel-Cope model-aligned M0", "quarterly_median")
growth_l1 <- parameter_value("Diagnostic growth", "mean_length_age_1_cm")
growth_l40 <- parameter_value("Diagnostic growth", "mean_length_age_40_cm")
growth_kappa <- parameter_value("Diagnostic growth", "kappa_quarterly")
n_age <- as.integer(parameter_value("Diagnostic growth", "age_classes"))
age_class <- seq_len(n_age)
growth_rho <- exp(-growth_kappa)
relative_growth <- (1 - growth_rho^(age_class - 1)) / (1 - growth_rho^(n_age - 1))
mean_length_at_age <- growth_l1 + (growth_l40 - growth_l1) * relative_growth
lorenzen_shape <- growth_l40 / mean_length_at_age
m_age <- rbind(
  data.frame(
    age_years = (age_class - 1) / 4,
    mortality = m_diagnostic * lorenzen_shape,
    evidence = "2026 diagnostic"
  ),
  data.frame(
    age_years = (age_class - 1) / 4,
    mortality = m_hamel_cope * lorenzen_shape,
    evidence = "Hamel–Cope, model-aligned"
  ),
  data.frame(
    age_years = (age_class - 1) / 4,
    mortality = m_tag * lorenzen_shape,
    evidence = "Tag estimate"
  )
)
m_age$evidence <- factor(
  m_age$evidence,
  levels = c("2026 diagnostic", "Hamel–Cope, model-aligned", "Tag estimate")
)
m_tag_age_interval <- data.frame(
  age_years = (age_class - 1) / 4,
  lower = m_tag_lower * lorenzen_shape,
  upper = m_tag_upper * lorenzen_shape
)
m_evidence_panel <- ggplot2::ggplot() +
  ggplot2::geom_ribbon(
    data = m_tag_age_interval,
    ggplot2::aes(.data$age_years, ymin = .data$lower, ymax = .data$upper),
    fill = "#75C7B0", alpha = 0.20
  ) +
  ggplot2::geom_line(
    data = m_age,
    ggplot2::aes(
      .data$age_years, .data$mortality,
      colour = .data$evidence, linetype = .data$evidence
    ),
    linewidth = 1.05, lineend = "round"
  ) +
  ggplot2::scale_colour_manual(
    values = c(
      "2026 diagnostic" = "#0B5267",
      "Hamel–Cope, model-aligned" = "#D66B00",
      "Tag estimate" = "#168C67"
    ),
    name = NULL,
    guide = ggplot2::guide_legend(
      nrow = 1, byrow = TRUE,
      override.aes = list(
        linewidth = c(1.25, 1.05, 1.05),
        linetype = c("solid", "22", "42")
      )
    )
  ) +
  ggplot2::scale_linetype_manual(
    values = c(
      "2026 diagnostic" = "solid",
      "Hamel–Cope, model-aligned" = "22",
      "Tag estimate" = "42"
    ), name = NULL, guide = "none"
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.035))
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 10, by = 2),
    expand = ggplot2::expansion(mult = c(0.01, 0.01))
  ) +
  ggplot2::labs(
    x = "Age (years)",
    y = expression("Quarterly natural mortality,"~italic(M)[a]~(quarter^{-1}))
  ) +
  theme_report(10.8) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.key.width = grid::unit(1.15, "cm"),
    legend.text = ggplot2::element_text(size = 8.7)
  )

count_panel <- function(planned, retained, column, x_label, fill, levels = NULL) {
  count_set <- function(data, set_label) {
    x <- data[[column]]
    if (!is.null(levels)) x <- factor(x, levels = levels)
    out <- as.data.frame(table(x), stringsAsFactors = FALSE)
    names(out) <- c("level", "count")
    out$set <- set_label
    out
  }
  counts <- rbind(
    count_set(planned, "Planned 100"),
    count_set(retained, "Included 80")
  )
  if (!is.null(levels)) {
    counts$level <- factor(as.character(counts$level), levels = levels)
  }
  counts$set <- factor(counts$set, levels = c("Planned 100", "Included 80"))
  ggplot2::ggplot(
    counts,
    ggplot2::aes(x = .data$level, y = .data$count, fill = .data$set)
  ) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.78), width = 0.72) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$count),
      position = ggplot2::position_dodge(width = 0.78),
      vjust = -0.35, size = 2.85
    ) +
    ggplot2::scale_fill_manual(
      values = design_fill_colours,
      name = NULL, guide = "none"
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = x_label, y = "Models") + theme_report(10.4) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = if (column == "effort_label") 28 else 0,
        hjust = if (column == "effort_label") 1 else 0.5,
        vjust = if (column == "effort_label") 1 else 0.5
      )
    )
}

add_design_labels <- function(data) {
  data$tag_tau_label <- format(data$tag_tau, nsmall = 1)
  data$k_label <- sprintf("%.2f", data$tag_mixing_k_cutoff)
  data$reporting_label <- ifelse(data$tag_reporting == "inclusion", "Included", "Excluded")
  data$effort_label <- sprintf(
    "%.1f / %.2f%%",
    100 * data$effort_creep_primary,
    100 * data$effort_creep_secondary
  )
  data
}
planned_design <- add_design_labels(planned_design)
design <- add_design_labels(design)

design_plot <- patchwork::wrap_plots(list(
  h_panel,
  m_panel,
  count_panel(planned_design, design, "tag_tau_label", expression("Tag overdispersion, " * tau), "#16899A", c("1.2", "1.3", "1.4")),
  count_panel(planned_design, design, "k_label", "KS dissimilarity cutoff, K", "#16899A", sprintf("%.2f", seq(0.05, 0.35, 0.05))),
  count_panel(planned_design, design, "reporting_label", "Pre-mixing reporting rates", "#16899A", c("Included", "Excluded")),
  count_panel(planned_design, design, "effort_label", "Primary / secondary effort creep", "#16899A", c("0.5 / 0.25%", "1.0 / 0.50%", "1.5 / 0.75%", "2.0 / 1.00%", "2.5 / 1.25%"))
), ncol = 2, guides = "collect") +
  patchwork::plot_annotation(tag_levels = "a")
design_plot <- design_plot & ggplot2::theme(
  legend.position = "bottom", legend.box = "vertical",
  legend.text = ggplot2::element_text(size = 8.2),
  legend.key.width = grid::unit(0.72, "cm"),
  axis.text.x = ggplot2::element_text(size = 8.2)
)

save_plot <- function(plot, stem, width = 7.1, height = 8.8) {
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

figures <- list(
  design = save_plot(
    design_plot, "ensemble-design-retention", height = 7.5
  ),
  natural_mortality = save_plot(
    m_evidence_panel, "natural-mortality-evidence", width = 7.1, height = 5.2
  )
)
unlink(file.path(figure_dir, paste0("convergence-hessian-qc", c(".png", ".pdf"))))

quantity_values <- list(
  management$sb_recent_sb0,
  management$sb_recent_sbmsy,
  management$f_recent_fmsy,
  management$historical_target_depletion,
  management$recent_historical_target_ratio
)
summary_rows <- data.frame(
  Quantity = c(
    "SBrecent / SBF=0", "SBrecent / SBMSY", "Frecent / FMSY",
    "Mean depletion, 2012–2015", "Recent / 2012–2015 depletion"
  ),
  Period = c(
    "2021–2024 / 2014–2023", "2021–2024", "2020–2023",
    "2012–2015", "Recent / 2012–2015"
  ),
  Models = n_models,
  `10%` = vapply(quantity_values, stats::quantile, numeric(1), probs = 0.10, names = FALSE),
  Median = vapply(quantity_values, stats::median, numeric(1)),
  `90%` = vapply(quantity_values, stats::quantile, numeric(1), probs = 0.90, names = FALSE),
  check.names = FALSE
)

risk_rows <- data.frame(
  Indicator = c(
    "Below the LRP", "Below SBMSY", "Above FMSY",
    "Below the model-specific 2012–2015 objective"
  ),
  Criterion = c(
    "SBrecent/SBF=0 < 0.20", "SBrecent/SBMSY < 1",
    "Frecent/FMSY > 1", "Drecent / mean(D2012–2015) < 1"
  ),
  Models = c(
    sum(management$below_lrp_020), sum(management$below_sbmsy),
    sum(management$above_fmsy), sum(management$recent_historical_target_ratio < 1)
  ),
  stringsAsFactors = FALSE
)
risk_rows$Percent <- 100 * risk_rows$Models / n_models

write.csv(summary_rows, file.path(table_dir, "management-summary.csv"), row.names = FALSE)
write.csv(risk_rows, file.path(table_dir, "management-risk.csv"), row.names = FALSE)
fit_public <- fit[match(model_id_map$source_ensemble_id, fit$ensemble_id), , drop = FALSE]
fit_public$source_ensemble_id <- fit_public$ensemble_id
fit_public$ensemble_id <- model_id_map$ensemble_id
fit_public <- fit_public[c(
  "ensemble_id", "source_ensemble_id",
  setdiff(names(fit_public), c("ensemble_id", "source_ensemble_id"))
)]
write.csv(fit_public, file.path(table_dir, "ensemble-fit-diagnostics.csv"), row.names = FALSE)
write.csv(model_id_map, file.path(table_dir, "model-id-map.csv"), row.names = FALSE)

html_escape <- function(value) {
  value <- gsub("&", "&amp;", as.character(value), fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  gsub(">", "&gt;", value, fixed = TRUE)
}

html_math <- function(value) {
  value <- html_escape(value)
  replacements <- c(
    "SBrecent" = "<i>SB</i><sub>recent</sub>",
    "SBF=0" = "<i>SB</i><sub><i>F</i>=0</sub>",
    "SBMSY" = "<i>SB</i><sub>MSY</sub>",
    "Frecent" = "<i>F</i><sub>recent</sub>",
    "FMSY" = "<i>F</i><sub>MSY</sub>",
    "Drecent" = "<i>D</i><sub>recent</sub>"
  )
  for (target in names(replacements)) {
    value <- gsub(target, replacements[[target]], value, fixed = TRUE)
  }
  value
}

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

image_uri <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  raw <- readBin(con, what = "raw", n = file.info(path)$size)
  paste0("data:image/png;base64,", jsonlite::base64_enc(raw))
}

format_num <- function(x, digits = 3) formatC(x, digits = digits, format = "f")

html_table <- function(data) {
  heads <- paste0("<th>", html_math(names(data)), "</th>", collapse = "")
  rows <- apply(data, 1, function(row) {
    paste0("<tr>", paste0("<td>", html_math(row), "</td>", collapse = ""), "</tr>")
  })
  paste0("<table><thead><tr>", heads, "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table>")
}

summary_display <- summary_rows
for (name in c("10%", "Median", "90%")) summary_display[[name]] <- format_num(summary_display[[name]])
risk_display <- risk_rows
risk_display$Percent <- paste0(format_num(risk_display$Percent, 1), "%")

word_table <- function(caption, data) {
  paste(c(caption, paste(names(data), collapse = "\t"), apply(data, 1, paste, collapse = "\t")), collapse = "\n")
}

summary_caption <- paste0(
  "Distribution of stock-status quantities across the 80 BET 2026 ensemble models retained after applying MGC ≤ 1e-4. ",
  "Recent spawning biomass is averaged over 2021–2024, unfished spawning biomass over 2014–2023, ",
  "and recent fishing mortality over 2020–2023."
)
risk_caption <- paste0(
  "Equal-weight structural-ensemble frequencies for the reported stock-status thresholds and ",
  "the 2012–2015 historical depletion objective. These percentages do not yet include parameter-estimation uncertainty."
)

summary_math <- c(
  "$SB_{recent}/SB_{F=0}$", "$SB_{recent}/SB_{MSY}$", "$F_{recent}/F_{MSY}$",
  "$\\overline{D}_{2012--2015}$", "$D_{recent}/\\overline{D}_{2012--2015}$"
)
summary_latex_rows <- vapply(seq_len(nrow(summary_display)), function(i) {
  cells <- c(summary_math[[i]], vapply(summary_display[i, -1, drop = FALSE], latex_escape, character(1)))
  paste0(paste(cells, collapse = " & "), " \\\\")
}, character(1))
latex_summary <- paste0(
  "\\begin{table}[htbp]\n\\centering\n\\caption{", latex_escape(summary_caption), "}\n",
  "\\small\n\\setlength{\\tabcolsep}{4pt}\n\\begin{tabularx}{\\textwidth}{@{}lXrrrr@{}}\n",
  "\\toprule\nQuantity & Period & Models & 10\\% & Median & 90\\% \\\\\n\\midrule\n",
  paste(summary_latex_rows, collapse = "\n"),
  "\n\\bottomrule\n\\end{tabularx}\n\\end{table}"
)

risk_latex_rows <- vapply(seq_len(nrow(risk_display)), function(i) {
  cells <- vapply(risk_display[i, , drop = FALSE], latex_escape, character(1))
  paste0(paste(cells, collapse = " & "), " \\\\")
}, character(1))
latex_risk <- paste0(
  "\\begin{table}[htbp]\n\\centering\n\\caption{", latex_escape(risk_caption), "}\n",
  "\\small\n\\begin{tabularx}{\\textwidth}{@{}XXrr@{}}\n\\toprule\n",
  "Indicator & Criterion & Models & Percent \\\\\n\\midrule\n",
  paste(risk_latex_rows, collapse = "\n"),
  "\n\\bottomrule\n\\end{tabularx}\n\\end{table}"
)

figure_card <- function(id, title, caption, latex_caption, files) {
  latex_figure <- paste0(
    "\\begin{figure}[htbp]\n\\centering\n",
    "\\includegraphics[width=\\textwidth]{figures/", basename(files[["pdf"]]), "}\n",
    "\\caption{", latex_caption, "}\n\\end{figure}"
  )
  paste0(
    "<section class='figure-card' id='", id, "'><h2>", title, "</h2>",
    "<img src='", image_uri(files[["png"]]), "' alt='", html_escape(title), "'>",
    "<p class='caption'><strong>Figure.</strong> ", caption, "</p>",
    "<div class='actions'><button onclick=\"copyText('cap-", id, "',this)\">Copy caption</button>",
    "<a download='", basename(files[["png"]]), "' href='", image_uri(files[["png"]]), "'>Save PNG</a>",
    "<a href='figures/", basename(files[["pdf"]]), "'>Open vector PDF</a>",
    "<button onclick=\"copyText('tex-", id, "',this)\">Copy figure for LaTeX</button></div>",
    "<textarea id='cap-", id, "' class='copy-source'>", html_escape(gsub("<[^>]+>", "", caption)), "</textarea>",
    "<textarea id='tex-", id, "' class='copy-source'>", html_escape(latex_figure), "</textarea></section>"
  )
}

captions <- list(
  trajectories = paste0(
    "Annual estimates across the 80 models retained after applying MGC ≤ 1 × 10<sup>−4</sup>. Grey lines are individual models; nested blue bands are pointwise central 50%, 80% and 95% structural intervals. The dark-blue line is the equal-weight median. The depletion line marks the limit reference point (LRP = 0.2)."
  ),
  management = paste0(
    "Equal-weight structural distributions of recent depletion, spawning biomass relative to <i>SB</i><sub>MSY</sub>, fishing mortality relative to <i>F</i><sub>MSY</sub>, and recent depletion relative to each model’s mean depletion during 2012–2015. Recent periods are 2021–2024 for spawning biomass, 2014–2023 for unfished spawning biomass and 2020–2023 for fishing mortality."
  ),
  status = paste0(
    "Kobe (a), Majuro (b) and a supplementary model-specific historical-objective diagnostic (c) for the 80 retained models. <i>D</i><sub>recent</sub> is mean spawning biomass for 2021–2024 divided by mean unfished spawning biomass for 2014–2023. In panel a the biomass boundary is <i>SB</i><sub>MSY</sub>; in panel b it is the depletion LRP of 0.20. Backgrounds denote green (biomass criterion met and <i>F</i>/<i>F</i><sub>MSY</sub> ≤ 1), yellow (biomass criterion not met and <i>F</i>/<i>F</i><sub>MSY</sub> ≤ 1), orange (biomass criterion met and <i>F</i>/<i>F</i><sub>MSY</sub> &gt; 1), and red (neither criterion met). Shaded contours are 50%, 80% and 95% bivariate highest-density regions (HDRs), calculated from an equal-weight Gaussian kernel-density estimate of the central model points using normal-reference bandwidths. These structural two-dimensional HDRs are distinct from the one-dimensional equal-tailed reporting interval. Point colours identify fixed tag overdispersion τ; filled and open symbols distinguish PDH and Near-PDH fits. Panel c is a supplementary diagnostic that plots each model's <i>D</i><sub>recent</sub> against its own mean annual depletion during 2012–2015; the diagonal denotes equality."
  ),
  design = paste0(
    "Realized inputs for the 80 retained models. Histograms and bars show included fits only. Continuous curves show the specified steepness and natural-mortality distributions; the dashed natural-mortality curve is the Hamel–Cope adult-mortality prior transformed to the assessment-model <i>M</i><sub>0</sub> scale, and the green point and interval show the tag-based estimate and 90% confidence interval."
  ),
  natural_mortality = paste0(
    "Age-specific quarterly natural mortality obtained by applying three assessment-relevant <i>M</i><sub>0</sub> values to the same 2026 Diagnostic growth schedule and fixed Lorenzen exponent (−1). Curves show the Diagnostic value (0.07814), the model-aligned Hamel–Cope median (0.08082), and the Ducharme-Barth et al. (2026) tag estimate (0.0624). Shading is the tag estimate's 90% confidence interval (0.0500–0.0749) propagated over age."
  )
)

latex_captions <- list(
  trajectories = paste0(
    "Annual estimates across the 80 models retained after applying $\\mathrm{MGC}\\leq1\\times10^{-4}$. Grey lines are individual models; nested blue bands are pointwise central 50\\%, 80\\% and 95\\% structural intervals. The dark-blue line is the equal-weight median. The depletion line marks the limit reference point (LRP $=0.2$)."
  ),
  management = paste0(
    "Equal-weight structural distributions of $SB_{recent}/SB_{F=0}$, $SB_{recent}/SB_{MSY}$, $F_{recent}/F_{MSY}$, and $D_{recent}/\\overline{D}_{2012--2015}$. Recent periods are 2021--2024 for spawning biomass, 2014--2023 for unfished spawning biomass and 2020--2023 for fishing mortality."
  ),
  status = paste0(
    "Kobe (a), Majuro (b) and a supplementary model-specific historical-objective diagnostic (c) for the 80 retained models. $D_{recent}$ is mean spawning biomass for 2021--2024 divided by mean unfished spawning biomass for 2014--2023. In panel a the biomass boundary is $SB_{MSY}$; in panel b it is the depletion LRP of 0.20. Backgrounds denote green (biomass criterion met and $F/F_{MSY}\\leq1$), yellow (biomass criterion not met and $F/F_{MSY}\\leq1$), orange (biomass criterion met and $F/F_{MSY}>1$), and red (neither criterion met). Shaded contours are 50\\%, 80\\% and 95\\% bivariate highest-density regions (HDRs), calculated from an equal-weight Gaussian kernel-density estimate of the central model points using normal-reference bandwidths. These structural two-dimensional HDRs are distinct from the one-dimensional equal-tailed reporting interval. Point colours identify fixed tag overdispersion $\\tau$; filled and open symbols distinguish PDH and Near-PDH fits. Panel c is a supplementary diagnostic that plots each model's $D_{recent}$ against its own mean annual depletion during 2012--2015; the diagonal denotes equality."
  ),
  design = paste0(
    "Realized inputs for the 80 retained models. Histograms and bars show included fits only. Continuous curves show the specified steepness and natural-mortality distributions; the dashed natural-mortality curve is the Hamel--Cope adult-mortality prior transformed to the assessment-model $M_0$ scale, and the green point and interval show the tag-based estimate and 90\\% confidence interval."
  ),
  natural_mortality = paste0(
    "Age-specific quarterly natural mortality obtained by applying three assessment-relevant $M_0$ values to the same 2026 Diagnostic growth schedule and fixed Lorenzen exponent ($-1$). Curves show the Diagnostic value (0.07814), the model-aligned Hamel--Cope median (0.08082), and the Ducharme-Barth et al. (2026) tag estimate (0.0624). Shading is the tag estimate's 90\\% confidence interval (0.0500--0.0749) propagated over age."
  )
)

captions$trajectories_stock <- paste0(
  "Annual depletion (a) and spawning potential (b) across the 80 retained assessment models. ",
  "Grey lines are individual models; nested blue bands are pointwise central 50%, 80% and 95% structural intervals. ",
  "The dark-blue line is the equal-weight median. The depletion reference line is the limit reference point (LRP = 0.20)."
)
captions$trajectories_process <- paste0(
  "Annual recruitment (a) and fishing mortality (b) across the 80 retained assessment models. ",
  "Grey lines are individual models; nested blue bands are pointwise 50%, 80% and 95% structural intervals. The dark-blue line is the equal-weight median. ",
  "Recruitment is in millions of fish and fishing mortality is annual."
)
captions$design_continuous <- paste0(
  "Planned and retained continuous ensemble inputs. Muted-gold bars show the 100 planned configurations and teal bars show the 80 models retained after the convergence filter: steepness (a) and natural mortality at the reference length (b). Curves show the specified input distributions. ",
  "In panel b the dashed curve is the Hamel–Cope adult-mortality prior transformed to the assessment-model <i>M</i><sub>0</sub> scale; the green point and line show the tag-based estimate and 90% confidence interval."
)
captions$design_discrete <- paste0(
  "Planned and retained counts for the discrete ensemble axes: tag overdispersion (a), tag-mixing cutoff (b), pre-mixing tag-reporting treatment (c), and paired effort-creep rates (d). Muted-gold bars show all 100 planned configurations and teal bars show the 80 models retained after the convergence filter."
)
captions$design <- paste0(
  "Planned and retained ensemble inputs: steepness (a), natural mortality at the reference length (b), tag overdispersion (c), tag-mixing cutoff (d), pre-mixing reporting treatment (e), and effort creep (f). ",
  "Muted gold shows the 100 planned configurations and teal shows the 80 retained models. Curves in panels a–b show the specified continuous input distributions; the green point and line in panel b are the tag-based estimate and 90% confidence interval from Ducharme-Barth et al. (2026; WCPFC-SC22-2026-SA-IP14)."
)
latex_captions$trajectories_stock <- paste0(
  "Annual depletion (a) and spawning potential (b) across the 80 retained assessment models. Grey lines are individual models; nested blue bands are pointwise central 50\\%, 80\\% and 95\\% structural intervals. ",
  "The dark-blue line is the equal-weight median. The depletion reference line is the limit reference point (LRP $=0.20$)."
)
latex_captions$trajectories_process <- paste0(
  "Annual recruitment (a) and fishing mortality (b) across the 80 retained assessment models. Grey lines are individual models; nested blue bands are pointwise 50\\%, 80\\% and 95\\% structural intervals. The dark-blue line is the equal-weight median. Recruitment is in millions of fish and fishing mortality is annual."
)
latex_captions$design_continuous <- paste0(
  "Planned and retained continuous ensemble inputs. Muted-gold bars show the 100 planned configurations and teal bars show the 80 models retained after the convergence filter: steepness (a) and natural mortality at the reference length (b). Curves show the specified input distributions. In panel b the dashed curve is the Hamel--Cope adult-mortality prior transformed to the assessment-model $M_0$ scale; the green point and line show the tag-based estimate and 90\\% confidence interval."
)
latex_captions$design_discrete <- paste0(
  "Planned and retained counts for the discrete ensemble axes: tag overdispersion (a), tag-mixing cutoff (b), pre-mixing tag-reporting treatment (c), and paired effort-creep rates (d). Muted-gold bars show all 100 planned configurations and teal bars show the 80 models retained after the convergence filter."
)
latex_captions$design <- paste0(
  "Planned and retained ensemble inputs: steepness (a), natural mortality at the reference length (b), tag overdispersion (c), tag-mixing cutoff (d), pre-mixing reporting treatment (e), and effort creep (f). ",
  "Muted gold shows the 100 planned configurations and teal shows the 80 retained models. Curves in panels a--b show the specified continuous input distributions; the green point and line in panel b are the tag-based estimate and 90\\% confidence interval from Ducharme-Barth et al. (2026; WCPFC-SC22-2026-SA-IP14)."
)

html <- paste0(
  "<!doctype html><html lang='en'><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'>",
  "<title>BET 2026 ensemble analysis</title><style>",
  "body{margin:0;background:#edf2f4;color:#173042;font-family:Georgia,serif;line-height:1.46}main{max-width:1160px;margin:auto;background:#fff;padding:28px 42px 60px}",
  "h1{font-size:2rem;margin:.15rem 0;color:#0a405a}h2{font-size:1.35rem;color:#0a5266;border-top:3px solid #11899a;padding-top:16px}.lede{font-size:1.08rem;max-width:960px}",
  ".summary{display:grid;grid-template-columns:repeat(4,1fr);gap:12px;margin:22px 0}.stat{background:#edf7f8;border:1px solid #bddce1;border-radius:8px;padding:13px}.stat b{display:block;font-size:1.55rem;color:#07566b}",
  ".method-grid{display:grid;grid-template-columns:repeat(2,1fr);gap:12px;margin:16px 0 24px}.method{border:1px solid #c8dce2;border-top:4px solid #11899a;border-radius:7px;padding:13px 15px;background:#f8fbfc}.method h3{margin:0 0 6px;color:#0a5266;font:700 1rem Arial,sans-serif}.method p{margin:0;font-size:.94rem}",
  ".note{background:#fff8e7;border-left:5px solid #d28a24;padding:13px 16px;margin:18px 0}.definition{background:#edf7f8;border-left:5px solid #11899a;padding:13px 16px;margin:18px 0}",
  ".report-tabs{position:sticky;top:0;z-index:20;display:flex;gap:7px;margin:20px -4px 26px;padding:8px 4px;background:rgba(255,255,255,.96);border-bottom:1px solid #c8dce2;backdrop-filter:blur(8px)}.report-tab{border:1px solid #a9cbd3;border-radius:5px;background:#f4fafb;color:#0a5266;padding:9px 18px;font:700 .92rem Arial,sans-serif;cursor:pointer}.report-tab:hover{background:#e7f4f6}.report-tab.active{border-color:#087f8f;background:#087f8f;color:#fff;box-shadow:0 5px 14px rgba(8,127,143,.18)}.report-panel{display:none}.report-panel.active{display:block}.panel-intro{max-width:900px;color:#4d6877;font-size:.97rem;margin:-4px 0 22px}.output-list>.figure-card:first-child,.output-list>.table-card:first-child{margin-top:8px}.table-card{margin:28px 0 46px;scroll-margin-top:76px}",
  ".figure-card{page-break-after:always;margin:34px 0 48px}.figure-card img{display:block;width:88%;height:auto;margin:12px auto 8px}.caption{font-size:.98rem}.actions{display:flex;gap:8px;flex-wrap:wrap;margin:10px 0}.actions button,.actions a{background:#087f8f;color:white;border:0;border-radius:4px;padding:8px 12px;text-decoration:none;font:600 .9rem sans-serif;cursor:pointer}.copy-source{position:absolute;left:-10000px}",
  "table{border-collapse:collapse;width:100%;font-family:Arial,sans-serif;font-size:.86rem;margin:12px 0 20px}th{background:#0b586d;color:white}th,td{padding:7px 8px;border-bottom:1px solid #cbd8de;text-align:right}th:first-child,td:first-child,th:nth-child(2),td:nth-child(2){text-align:left}",
  ".refs{font-size:.94rem}.refs li{margin:.55rem 0}@media(max-width:760px){main{padding:20px}.summary,.method-grid{grid-template-columns:1fr}.report-tabs{margin-left:0;margin-right:0}.report-tab{flex:1;padding:9px 6px}}",
  "@media print{@page{size:A4 portrait;margin:13mm}body{background:white}main{max-width:none;padding:0}.actions,.report-tabs{display:none}.report-panel{display:block!important}.figure-card{break-after:page;break-inside:avoid;margin:0}.figure-card h2{margin-top:0}.figure-card img{width:86%;max-height:176mm;object-fit:contain}.table-card{break-inside:avoid}h1{font-size:18pt}h2{font-size:14pt}table{font-size:8.5pt}}",
  "</style></head><body><main><h1>BET 2026 ensemble analysis</h1>",
  "<p class='lede'>Equal-weight results from 80 bigeye tuna assessment models retained after applying MGC ≤ 1 × 10<sup>−4</sup>, with structural uncertainty, available Hessian-based estimation uncertainty and stochastic projections kept explicit.</p>",
  "<div class='actions'><a href='https://pacificcommunity.github.io/ofp-sam-bet-2026-ensemble/bet-2026-ensemble-interactive-viewer.html' target='_blank' rel='noopener'>Open 80-model interactive viewer</a></div>",
  "<div class='summary'><div class='stat'><b>100 &rarr; ", n_models, "</b>planned &rarr; included</div><div class='stat'><b>", n_pdh, "</b>PDH</div><div class='stat'><b>", n_near, "</b>Near-PDH</div><div class='stat'><b>20</b>not retained</div></div>",
  "<nav class='report-tabs' aria-label='Report sections'><button class='report-tab active' type='button' data-report-tab='overview'>Overview</button><button class='report-tab' type='button' data-report-tab='figures'>Figures</button><button class='report-tab' type='button' data-report-tab='tables'>Tables</button></nav>",
  "<section class='report-panel active' id='overview-panel' data-report-panel='overview'>",
  "<h2>Overview</h2><div class='method-grid'>",
  "<article class='method'><h3>Ensemble</h3><p>Of 100 planned configurations, 80 met MGC ≤ 1 × 10<sup>−4</sup> and were retained with equal model weight; ten failed the criterion and ten were incomplete.</p></article>",
  "<article class='method'><h3>Uncertainty</h3><p>All 80 central fits represent structural uncertainty. For 62 PDH fits, 100 correlated inverse-Hessian draws were propagated jointly by the multivariate delta method; 18 Near-PDH fits contribute central estimates only. Estimation uncertainty is therefore available, but not complete for all models.</p></article>",
  "<article class='method'><h3>Projections</h3><p>Each model contributes ten recruitment paths for 2025–2054, sampled from its estimated 1972–2023 recruitment deviations; the first 20 assessment years and terminal 2024 are excluded. All 33 fisheries are catch-conditioned separately by fishery and quarter at the 2022–2024 mean, with missing observations set to zero. Projection bands exclude Hessian draws.</p></article>",
  "<article class='method'><h3>Management quantities</h3><p><i>SB</i><sub>recent</sub> uses 2021–2024, <i>SB</i><sub><i>F</i>=0</sub> 2014–2023 and <i>F</i><sub>recent</sub> 2020–2023. The LRP is 0.2<i>SB</i><sub><i>F</i>=0</sub>; MSY reference points are model-specific equilibrium quantities. Projected depletion is the rolling four-year biomass mean divided by the preceding ten-year unfished mean.</p></article>",
  "<article class='method'><h3>Intervals</h3><p>One-dimensional results report the median and central 80% equal-tailed interval; 50% and 95% bands are supplementary. Kobe and Majuro plots use two-dimensional 50%, 80% and 95% kernel highest-density regions.</p></article>",
  "<article class='method'><h3>Scope</h3><p>Hessian intervals are first-order normal approximations. Regional results cover Regions 1–5, with the stock-wide LRP shown only as a reference. Axis summaries are descriptive; no causal attribution, variance decomposition or posterior correlation analysis is made.</p></article></div>",
  "<div class='note'><strong>Pre-mixing reporting-rate stability.</strong> The design assigned 50 models to each treatment, but retained 34 inclusion models and 46 exclusion models. MFCL reconstructs pre-mixing tag catches under inclusion by dividing by the estimated reporting rate; rates approaching zero can create very large intermediate values and numerical overflow. The MFCL manual therefore cautions that these rates may be poorly determined during mixing and recommends exclusion. The imbalance is interpreted as differential numerical stability, not as an intended difference in model weight.</div>",
  "<div id='analysis-results'></div>",
  "<h2 id='supplementary-material'>Supporting outputs</h2><p>Ensemble-design, uncertainty-axis, regional and projection outputs are organized in the Figures and Tables tabs.</p>",
  figure_card("ensemble-design", "Planned and retained ensemble inputs", captions$design, latex_captions$design, figures$design),
  figure_card("natural-mortality-evidence", "Natural-mortality evidence", captions$natural_mortality, latex_captions$natural_mortality, figures$natural_mortality),
  "<section class='refs'><h2>References</h2><ol>",
  "<li><a href='https://meetings.wcpfc.int/system/files/2023-09/SC19-SA-WP-05_BET_2023_Rev2%20%28Posted%20on%2015Sep2023%29.pdf'>2023 WCPO bigeye tuna stock assessment</a>.</li>",
  "<li><a href='https://meetings.wcpfc.int/system/files/2023-12/WCPFC20-2023-15_Rev01_CMM_2021-01_eval_SPC-OFP%20-%20rev1%20%287%20Dec%202023%29.pdf'>2023 bigeye assessment projections and tropical-tuna measure evaluation</a>.</li>",
  "<li><a href='https://cmm.wcpfc.int/sites/default/files/cmm_attachments/CMM%202025-02%20Conservation%20and%20Management%20Measure%20for%20Bigeye%2C%20Yellowfin%20and%20Skipjack%20Tuna%20in%20the%20Western%20And%20Central%20Pacific%20Ocean%20%281%29.pdf'>WCPFC CMM 2025-02 for bigeye, yellowfin and skipjack tuna</a>.</li>",
  "<li><a href='https://meetings.wcpfc.int/file/19559/download'>WCPFC stock-status and management-advice definitions for tropical tunas</a>.</li>",
  "<li><a href='https://meetings.wcpfc.int/taxonomy/term/2786'>WCPFC First Bigeye Management Workshop: 2012–2015 depletion objective and candidate TRP process</a>.</li>",
  "<li><a href='https://doi.org/10.1016/j.fishres.2022.106477'>Hamel and Cope (2022): longevity-based natural-mortality prior</a>.</li>",
  "<li><a href='https://meetings.wcpfc.int/node/32286'>Ducharme-Barth, N., Peatman, T., Scutt Phillips, J., and Minte-Vera, C. (2026). Natural mortality estimation for WCPO bigeye tuna: a joint cohort analysis of Coral Sea and Region 4 tagging data (WCPFC-SC22-2026-SA-IP14)</a>.</li>",
  "<li><a href='https://meetings.wcpfc.int/file/4756/download'>Pilling et al. (2016). Approaches for balancing biological model uncertainty in stock-assessment projections for tropical tunas</a>.</li>",
  "</ol></section></section>",
  "<section class='report-panel' id='figures-panel' data-report-panel='figures'><h2>Figures</h2><p class='panel-intro'>Publication figures are grouped here once, with report-ready captions and PNG and vector-PDF downloads.</p><div class='output-list' id='figures-list'></div></section>",
  "<section class='report-panel' id='tables-panel' data-report-panel='tables'><h2>Tables</h2><p class='panel-intro'>Report tables are grouped here once, with Word, LaTeX and CSV outputs.</p><div class='output-list' id='tables-list'></div></section>",
  "<script>function copyText(id,button){const x=document.getElementById(id);navigator.clipboard.writeText(x.value).then(()=>{const old=button.textContent;button.textContent='Copied';button.classList.add('copied');setTimeout(()=>{button.textContent=old;button.classList.remove('copied')},1400);});}function setReportTab(name,scroll=true,updateHash=true){document.querySelectorAll('[data-report-tab]').forEach(button=>button.classList.toggle('active',button.dataset.reportTab===name));document.querySelectorAll('[data-report-panel]').forEach(panel=>panel.classList.toggle('active',panel.dataset.reportPanel===name));if(updateHash)history.replaceState(null,'','#'+name);if(scroll)window.scrollTo({top:0,behavior:'smooth'});}function organizeReportOutputs(){const figures=document.getElementById('figures-list'),tables=document.getElementById('tables-list');document.querySelectorAll('.figure-card').forEach(card=>figures.appendChild(card));document.querySelectorAll('.table-card').forEach(card=>tables.appendChild(card));document.querySelectorAll('[data-report-tab]').forEach(button=>button.addEventListener('click',()=>setReportTab(button.dataset.reportTab)));document.querySelectorAll(\"a[href^='http']\").forEach(a=>{a.target='_blank';a.rel='noopener noreferrer';});const requested=location.hash.slice(1);if(['overview','figures','tables'].includes(requested)){setReportTab(requested,false,false);return;}const target=requested?document.getElementById(requested):null;if(target){setReportTab(target.classList.contains('table-card')?'tables':'figures',false,false);setTimeout(()=>target.scrollIntoView({block:'start'}),0);}else{setReportTab('overview',false,false);}}document.addEventListener('DOMContentLoaded',organizeReportOutputs);</script>",
  "</main></body></html>"
)

writeLines(html, file.path(output_dir, "bet-2026-ensemble-report.html"), useBytes = TRUE)

manifest_files <- c(
  "bet-2026-ensemble-report.html",
  unlist(lapply(figures, function(x) file.path("figures", basename(x)))),
  "tables/management-summary.csv",
  "tables/management-risk.csv",
  "tables/ensemble-fit-diagnostics.csv",
  "tables/model-id-map.csv"
)
manifest <- data.frame(file = manifest_files, stringsAsFactors = FALSE)
manifest$sha256 <- vapply(file.path(output_dir, manifest$file), function(path) {
  output <- system2("sha256sum", path, stdout = TRUE)
  strsplit(output[[1]], "[[:space:]]+")[[1]][[1]]
}, character(1))
write.csv(manifest, file.path(output_dir, "report-manifest.csv"), row.names = FALSE)

cat(sprintf(
  "Rendered self-contained BET 2026 ensemble report for %d MGC-filtered models.\n",
  n_models
))
