#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_arg) != 1L) stop("Cannot locate repository root.", call. = FALSE)
repo <- normalizePath(file.path(dirname(sub("^--file=", "", script_arg)), ".."), mustWork = TRUE)
output_dir <- file.path(repo, "design")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

n_models <- 100L

# 2024 South Pacific albacore assessment: h = 0.2 + 0.8Y,
# Y ~ Beta(alpha, beta), with E[h] = 0.87 and SD[h] = 0.063.
h_lower <- 0.2
h_upper <- 1.0
h_mean <- 0.87
h_sd <- 0.063
y_mean <- (h_mean - h_lower) / (h_upper - h_lower)
y_var <- (h_sd / (h_upper - h_lower))^2
beta_total <- y_mean * (1 - y_mean) / y_var - 1
h_alpha <- y_mean * beta_total
h_beta <- (1 - y_mean) * beta_total
h_probability <- (seq_len(n_models) - 0.5) / n_models
h_draw <- h_lower + (h_upper - h_lower) * qbeta(h_probability, h_alpha, h_beta)

# Evidence-synthesised quarterly M0 at the MFCL Lorenzen reference length
# L(40.5 quarters). Ducharme-Barth et al. (2026) estimate M0 = 0.0624
# (SE 0.0076; approximate 90% CI 0.0500-0.0749). The Diagnostic model has
# M0 = exp(-2.54930339768360) and a fixed length exponent of -1.
#
# The selected distribution is a bounded logit-normal. Its mode is the
# requested midpoint of the tag estimate and previous-assessment value, its
# median is the Diagnostic M0, and its elicited design range is 0.050-0.165.
# Unlike a truncated lognormal, its density approaches zero smoothly at both
# range limits.
m_min <- 0.050
m_mode <- 0.0702
m_median <- exp(-2.54930339768360)
m_max <- 0.165
m_tag_estimate <- 0.0624
m_tag_se <- 0.0076
m_tag_lower90 <- 0.0500
m_tag_upper90 <- 0.0749
m_previous <- 0.0780
m_hc_amax_years <- 15
m_hc_annual_median <- 5.40 / m_hc_amax_years
m_hc_quarterly_median <- m_hc_annual_median / 4
m_log_sd_hamel_cope <- 0.31
m_hc_quarterly_mean <- m_hc_quarterly_median * exp(m_log_sd_hamel_cope^2 / 2)
m_hc_lower95 <- qlnorm(0.025, log(m_hc_quarterly_median), m_log_sd_hamel_cope)
m_hc_upper95 <- qlnorm(0.975, log(m_hc_quarterly_median), m_log_sd_hamel_cope)

m_mode_scaled <- (m_mode - m_min) / (m_max - m_min)
m_median_scaled <- (m_median - m_min) / (m_max - m_min)
m_logit_mean <- qlogis(m_median_scaled)
m_logit_sd <- sqrt(
  (m_logit_mean - qlogis(m_mode_scaled)) / (1 - 2 * m_mode_scaled)
)
qbounded_logit_normal <- function(p) {
  m_min + (m_max - m_min) * plogis(qnorm(p, m_logit_mean, m_logit_sd))
}
dbounded_logit_normal <- function(x) {
  ans <- numeric(length(x))
  inside <- x > m_min & x < m_max
  z <- (x[inside] - m_min) / (m_max - m_min)
  ans[inside] <- dnorm(qlogis(z), m_logit_mean, m_logit_sd) /
    (z * (1 - z) * (m_max - m_min))
  ans
}
m_lower95 <- qbounded_logit_normal(0.025)
m_upper95 <- qbounded_logit_normal(0.975)
# As for steepness, represent 100 equal-probability strata by their midpoints.
# Do not assign the elicited finite controls themselves as ensemble draws.
m_probability <- (seq_len(n_models) - 0.5) / n_models
m_draw <- qbounded_logit_normal(m_probability)

# Hamel-Cope estimates an age-invariant adult M from longevity, whereas MFCL
# estimates M0 at L(40.5). Under the fixed -1 Lorenzen slope, the Diagnostic
# growth curve and its maturity-at-age ogive imply that the maturity-weighted
# mean adult M is 1.113625591117 * M0. Therefore the model-aligned equivalent
# of an adult-M draw is M0 = adult M / 1.113625591117. This is an external
# calibration for comparison; it does not determine the selected distribution.
m_hc_adult_to_m0_ratio <- 1.113625591117
m_hc_m0_scale <- 1 / m_hc_adult_to_m0_ratio
m_hc_m0_median <- m_hc_quarterly_median * m_hc_m0_scale
m_hc_m0_mean <- m_hc_quarterly_mean * m_hc_m0_scale
m_hc_m0_lower95 <- m_hc_lower95 * m_hc_m0_scale
m_hc_m0_upper95 <- m_hc_upper95 * m_hc_m0_scale

hc_amax_years <- c(13, 15, 16)
hc_amax_sensitivity <- data.frame(
  amax_years = hc_amax_years,
  adult_m_annual_median = 5.40 / hc_amax_years,
  adult_m_quarterly_median = (5.40 / hc_amax_years) / 4,
  m0_quarterly_median = ((5.40 / hc_amax_years) / 4) * m_hc_m0_scale,
  m0_quarterly_lower95 = qlnorm(0.025, log((5.40 / hc_amax_years) / 4),
                                  m_log_sd_hamel_cope) * m_hc_m0_scale,
  m0_quarterly_upper95 = qlnorm(0.975, log((5.40 / hc_amax_years) / 4),
                                  m_log_sd_hamel_cope) * m_hc_m0_scale,
  adult_mean_to_m0_divisor = m_hc_adult_to_m0_ratio,
  stringsAsFactors = FALSE
)

# Exact finite-sample counts for the SC22-IP10 Kolmogorov dissimilarity (K)
# cutoff. These are not mixing periods. Each cutoff selects an upstream INI
# containing the release-group-specific mixing periods derived at that cutoff.
# The weights approximate a symmetric 1:2:3:4:3:2:1 distribution while making
# K = 0.20 clearly modal.
k_cutoff_levels <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35)
k_cutoff_counts <- c(6L, 12L, 19L, 26L, 19L, 12L, 6L)
k_cutoff_draw <- rep(k_cutoff_levels, k_cutoff_counts)
mixing_source_file <- setNames(
  paste0("bet.2026.mix-", sub("0$", "", sprintf("%.2f", k_cutoff_levels)), ".ini"),
  sprintf("%.2f", k_cutoff_levels)
)
mixing_source_sha256 <- c(
  "ff2ed1786eeebb61f366a85289aa52ab67293e8c9a449c2e636093dfa62ba25a",
  "e0b6313a8bd0239dd0ba0305ecad0b58f286ef4a2c6e75a7da5fd5dae957ca03",
  "d39f4cf4243d7cf1d8ce626d048bfba6493c234d65743c24fb6e70098714b54f",
  "1e8c589854274248efcb8b08cc85b476e718d2f5d985e03873e973181ae11e94",
  "d1925f39ba2a75b2dd56c38bbb56ad36dfaea54de36e85d6d0f1a62da674051e",
  "0d0b797d8439de174585de479e2f0211031f1a31af88d315cc3ce2821e4ab0fc",
  "1ee630abfb044702581ca2b7956a5262ba2a29547eb47a1ae6f5c65005aaa661"
)
mixing_zero_events <- c(0L, 0L, 1L, 2L, 6L, 14L, 18L)
mixing_sources <- data.frame(
  tag_mixing_k_cutoff = k_cutoff_levels,
  tag_mixing_source_file = unname(mixing_source_file),
  source_sha256 = mixing_source_sha256,
  zero_mixing_events = mixing_zero_events,
  source_branch = "SC22-IP10-regionMean",
  source_commit = "efe3107c72774ee73b5e6dc45e44cf51f0fc20e8",
  stringsAsFactors = FALSE
)

# MFCL tag flag column 2: 0 includes pre-mixing reporting, 1 excludes it.
rr_draw <- rep(c(0L, 1L), each = n_models / 2L)

# Direct negative-binomial tag overdispersion. The Diagnostic reference fixes
# tau=2; the new structural axis fixes tau at the three requested lower values.
# Counts are exact and symmetric apart from the unavoidable centre allocation.
tau_levels <- c(1.2, 1.3, 1.4)
tau_counts <- c(33L, 34L, 33L)
tau_draw <- rep(tau_levels, tau_counts)

effort <- data.frame(
  effort_level = seq_len(5L),
  effort_creep_primary = c(0.005, 0.010, 0.015, 0.020, 0.025),
  effort_creep_secondary = c(0.0025, 0.0050, 0.0075, 0.0100, 0.0125),
  effort_source_file = c(
    "bet.2026.new-strucure.regional-cpue.wt-as-len-plus-len.eff.creep.0.005-0.0025.frq",
    "bet.2026.new-strucure.regional-cpue.wt-as-len-plus-len.eff.creep.0.01-0.005.frq",
    "bet.2026.new-strucure.regional-cpue.wt-as-len-plus-len.eff.creep.0.015-0.0075.frq",
    "bet.2026.new-strucure.regional-cpue.wt-as-len-plus-len.eff.creep.0.02-0.01.frq",
    "bet.2026.new-strucure.regional-cpue.wt-as-len-plus-len.eff.creep.0.025-0.0125.frq"
  ),
  source_sha256 = c(
    "c96fe33c6ebc6ecdbb9c2a4fde062102145d90cd7dc8f81bb82ff62176f03b59",
    "e100402b22ae33a2c218781e09957b02aa8c4747bac880f1a7f83150b5b86168",
    "471a84b1e251a7b2879bad73628eee5fcf6931735d033440966f3b1a27db8203",
    "0da5ebd90d9bb95acb17b33397bdaa1c894416616052cb95c423f4f17ea2a487",
    "82c417ca877de61f94b305956d994b1efffd033d496264d46019149842f525a0"
  ),
  stringsAsFactors = FALSE
)
effort_draw <- rep(effort$effort_level, each = n_models / nrow(effort))

# Couple the six fixed margins using the independently randomized permutations
# frozen in design/pairing-map.csv. The map is generated once, without a fixed
# RNG seed, by scripts/randomize-pairing.R; normal design recreation never
# resamples it. This removes modular/lattice structure while keeping every
# committed model exactly reproducible.
pairing_map_path <- file.path(output_dir, "pairing-map.csv")
if (!file.exists(pairing_map_path)) {
  stop("Missing frozen randomized pairing map: design/pairing-map.csv", call. = FALSE)
}
pairing_map <- read.csv(pairing_map_path, stringsAsFactors = FALSE, check.names = FALSE)
expected_pairing_columns <- c(
  "ensemble_id", "steepness_rank", "tag_mixing_rank", "tag_reporting_rank",
  "natural_mortality_rank", "effort_creep_rank", "tag_overdispersion_rank"
)
if (!identical(names(pairing_map), expected_pairing_columns) ||
    !identical(pairing_map$ensemble_id, sprintf("ensemble-%03d", seq_len(n_models)))) {
  stop("Frozen randomized pairing map has an invalid schema or model order.", call. = FALSE)
}
pairing <- list(
  h = pairing_map$steepness_rank,
  mixing = pairing_map$tag_mixing_rank,
  rr = pairing_map$tag_reporting_rank,
  m = pairing_map$natural_mortality_rank,
  effort = pairing_map$effort_creep_rank,
  tau = pairing_map$tag_overdispersion_rank
)
stopifnot(all(vapply(pairing, function(x) identical(sort(x), seq_len(n_models)), logical(1))))

effort_index <- effort_draw[pairing$effort]
design <- data.frame(
  ensemble_id = sprintf("ensemble-%03d", seq_len(n_models)),
  steepness = h_draw[pairing$h],
  steepness_prior_quantile = h_probability[pairing$h],
  tag_mixing_k_cutoff = k_cutoff_draw[pairing$mixing],
  tag_mixing_source_file = unname(mixing_source_file[
    sprintf("%.2f", k_cutoff_draw[pairing$mixing])
  ]),
  tag_reporting_flag2 = rr_draw[pairing$rr],
  tag_reporting = ifelse(rr_draw[pairing$rr] == 0L, "inclusion", "exclusion"),
  tag_tau = tau_draw[pairing$tau],
  tau_fish_pars4 = log(tau_draw[pairing$tau] - 1),
  m_age40_quarterly = m_draw[pairing$m],
  lorenzen_log_intercept = log(m_draw[pairing$m]),
  m_prior_quantile = m_probability[pairing$m],
  effort_creep_primary = effort$effort_creep_primary[effort_index],
  effort_creep_secondary = effort$effort_creep_secondary[effort_index],
  effort_source_file = effort$effort_source_file[effort_index],
  initialization = "Job 21641 ordinary makepar (no seed)",
  pairing_version = "random-frozen-v1",
  stringsAsFactors = FALSE
)
design$zero_mixing_events <- unname(setNames(
  mixing_zero_events,
  sprintf("%.2f", k_cutoff_levels)
)[sprintf("%.2f", design$tag_mixing_k_cutoff)])
design$tag_reporting_zero_mixing_exclusions <- ifelse(
  design$tag_reporting_flag2 == 0L,
  design$zero_mixing_events,
  0L
)
design$model_label <- sprintf(
  "E%03d | h=%.3f | tau=%.1f | K=%.2f | RR=%s | M0=%.4f/qtr | creep=%.1f/%.2f%%",
  seq_len(n_models), design$steepness, design$tag_tau, design$tag_mixing_k_cutoff,
  ifelse(design$tag_reporting_flag2 == 0L, "include", "exclude"),
  design$m_age40_quarterly,
  100 * design$effort_creep_primary,
  100 * design$effort_creep_secondary
)

numeric_design <- data.frame(
  steepness = design$steepness,
  tag_tau = design$tag_tau,
  tag_mixing_k_cutoff = design$tag_mixing_k_cutoff,
  reporting_flag2 = design$tag_reporting_flag2,
  m_age40_quarterly = design$m_age40_quarterly,
  effort_primary = design$effort_creep_primary
)
rank_correlation <- cor(numeric_design, method = "spearman")
pairing_max_abs_spearman <- max(abs(rank_correlation[upper.tri(rank_correlation)]))

stopifnot(
  nrow(design) == 100L,
  identical(as.integer(table(design$tag_tau)), tau_counts),
  all(abs(design$tag_tau - (1 + exp(design$tau_fish_pars4))) < 1e-12),
  identical(as.integer(table(design$tag_mixing_k_cutoff)), k_cutoff_counts),
  identical(as.integer(table(design$tag_reporting_flag2)), c(50L, 50L)),
  identical(as.integer(table(design$effort_creep_primary)), rep(20L, 5L)),
  identical(sort(design$steepness_prior_quantile), (seq_len(n_models) - 0.5) / n_models),
  length(unique(design$steepness)) == n_models,
  abs(mean(design$steepness) - h_mean) < 0.001,
  abs(sd(design$steepness) - h_sd) < 0.002,
  abs(min(design$steepness) - 0.668842859607944) < 1e-12,
  abs(max(design$steepness) - 0.980467458423588) < 1e-12,
  identical(sort(design$m_prior_quantile), (seq_len(n_models) - 0.5) / n_models),
  length(unique(design$m_age40_quarterly)) == n_models,
  pairing_max_abs_spearman <= 0.10,
  abs(min(design$m_age40_quarterly) - qbounded_logit_normal(0.005)) < 1e-12,
  abs(max(design$m_age40_quarterly) - qbounded_logit_normal(0.995)) < 1e-12,
  abs(m_min + (m_max - m_min) * plogis(
    m_logit_mean - m_logit_sd^2 * (1 - 2 * m_mode_scaled)
  ) - m_mode) < 1e-12,
  abs(m_hc_m0_median - 0.080817108297) < 1e-10
)

options(digits = 15)
write.csv(design, file.path(output_dir, "model-draws.csv"), row.names = FALSE, quote = TRUE)
write.csv(effort, file.path(output_dir, "effort-creep-sources.csv"), row.names = FALSE, quote = TRUE)
write.csv(mixing_sources, file.path(output_dir, "mixing-sources.csv"), row.names = FALSE, quote = TRUE)
write.csv(hc_amax_sensitivity,
          file.path(output_dir, "hamel-cope-amax-sensitivity.csv"),
          row.names = FALSE, quote = TRUE)

distribution_parameters <- data.frame(
  axis = c(
    rep("Steepness", 6L),
    rep("Ensemble quarterly M at reference length", 8L),
    rep("Tag-analysis M0", 4L),
    rep("Hamel-Cope Amax prior", 7L),
    rep("Hamel-Cope model-aligned M0", 5L),
    rep("Design", 8L)
  ),
  parameter = c(
    "lower", "upper", "mean", "sd", "beta_alpha", "beta_beta",
    "lower", "upper", "mode", "median", "logit_mean", "logit_sd", "lower_95", "upper_95",
    "estimate", "se", "lower_90", "upper_90",
    "amax_years", "annual_median", "quarterly_median", "quarterly_mean", "log_sd", "lower_95", "upper_95",
    "adult_mean_to_m0_divisor", "quarterly_median", "quarterly_mean", "lower_95", "upper_95",
    "models", "randomized_pairing_axes", "pairing_map_rows", "pairing_rank_min",
    "pairing_rank_max", "max_abs_spearman_limit", "max_abs_spearman_observed",
    "pairing_version"
  ),
  value = c(
    h_lower, h_upper, h_mean, h_sd, h_alpha, h_beta,
    m_min, m_max, m_mode, m_median, m_logit_mean, m_logit_sd, m_lower95, m_upper95,
    m_tag_estimate, m_tag_se, m_tag_lower90, m_tag_upper90,
    m_hc_amax_years, m_hc_annual_median, m_hc_quarterly_median, m_hc_quarterly_mean,
    m_log_sd_hamel_cope, m_hc_lower95, m_hc_upper95,
    m_hc_adult_to_m0_ratio, m_hc_m0_median, m_hc_m0_mean,
    m_hc_m0_lower95, m_hc_m0_upper95,
    n_models, length(pairing), nrow(pairing_map),
    min(unlist(pairing, use.names = FALSE)), max(unlist(pairing, use.names = FALSE)),
    0.10, pairing_max_abs_spearman, 1L
  ),
  stringsAsFactors = FALSE
)
write.csv(distribution_parameters, file.path(output_dir, "distribution-parameters.csv"),
          row.names = FALSE, quote = TRUE)

m_evidence <- data.frame(
  source = c(
    "Ducharme-Barth et al. (2026) tag analysis",
    "2023 WCPO BET diagnostic",
    "Hamel and Cope (2022), Amax = 15 years, adult M",
    "Hamel and Cope (2022), Amax = 15 years, model-aligned M0",
    "Selected ensemble distribution"
  ),
  statistic = c("estimate", "point value", "median", "median", "median / mode"),
  central = c(m_tag_estimate, m_previous, m_hc_quarterly_median,
              m_hc_m0_median, m_median),
  secondary_central = c(NA_real_, NA_real_, m_hc_quarterly_mean,
                        m_hc_m0_mean, m_mode),
  lower = c(m_tag_lower90, NA_real_, m_hc_lower95, m_hc_m0_lower95, m_lower95),
  upper = c(m_tag_upper90, NA_real_, m_hc_upper95, m_hc_m0_upper95, m_upper95),
  interval = c("90% delta-method CI", NA, "95% prior interval",
               "95% model-aligned interval", "95% distribution interval"),
  units = "quarter^-1",
  stringsAsFactors = FALSE
)
write.csv(m_evidence, file.path(output_dir, "m-evidence.csv"), row.names = FALSE, quote = TRUE,
          na = "")

continuous_summary <- rbind(
  data.frame(
    axis = "Steepness", distribution = "0.2 + 0.8 * Beta(17.541500, 3.403575)",
    minimum = min(design$steepness), q25 = unname(quantile(design$steepness, 0.25)),
    median = median(design$steepness), mean = mean(design$steepness),
    q75 = unname(quantile(design$steepness, 0.75)), maximum = max(design$steepness),
    sd = sd(design$steepness)
  ),
  data.frame(
    axis = "Quarterly M0 at reference length",
    distribution = sprintf(
      "Bounded logit-normal(%.3f, %.3f; median %.6f; mode %.4f; logit-SD %.6f)",
      m_min, m_max, m_median, m_mode, m_logit_sd
    ),
    minimum = min(design$m_age40_quarterly), q25 = unname(quantile(design$m_age40_quarterly, 0.25)),
    median = median(design$m_age40_quarterly), mean = mean(design$m_age40_quarterly),
    q75 = unname(quantile(design$m_age40_quarterly, 0.75)), maximum = max(design$m_age40_quarterly),
    sd = sd(design$m_age40_quarterly)
  )
)
write.csv(continuous_summary, file.path(output_dir, "continuous-summary.csv"), row.names = FALSE, quote = TRUE)

discrete_summary <- rbind(
  data.frame(axis = "Tag overdispersion tau", level = format(tau_levels, nsmall = 1),
             count = as.integer(table(factor(design$tag_tau, levels = tau_levels)))),
  data.frame(axis = "Tag mixing periods (K cutoff)", level = names(table(design$tag_mixing_k_cutoff)),
             count = as.integer(table(design$tag_mixing_k_cutoff))),
  data.frame(axis = "Tag reporting", level = names(table(design$tag_reporting)),
             count = as.integer(table(design$tag_reporting))),
  data.frame(axis = "Effort creep", level = sprintf("%.1f%% / %.2f%%",
             100 * effort$effort_creep_primary, 100 * effort$effort_creep_secondary),
             count = as.integer(table(factor(design$effort_creep_primary,
                                             levels = effort$effort_creep_primary))))
)
discrete_summary$proportion <- discrete_summary$count / n_models
write.csv(discrete_summary, file.path(output_dir, "discrete-summary.csv"), row.names = FALSE, quote = TRUE)

write.csv(rank_correlation, file.path(output_dir, "rank-correlation.csv"), quote = TRUE)

draw_publication_figure <- function() {
  blue <- "#0072B2"
  blue_fill <- grDevices::adjustcolor("#56B4E9", alpha.f = 0.58)
  green <- "#009E73"
  purple <- "#7B6DB1"
  orange <- "#D55E00"
  orange_fill <- grDevices::adjustcolor("#E69F00", alpha.f = 0.55)
  grey <- "#555555"
  light_grey <- "#E6E6E6"

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  layout(matrix(seq_len(6L), nrow = 2L, byrow = TRUE))
  par(mar = c(4.2, 4.4, 2.4, 0.8), oma = c(0.2, 0.2, 0.2, 0.2),
      mgp = c(2.45, 0.72, 0), tcl = -0.25, las = 1, bty = "l",
      cex.axis = 0.83, cex.lab = 0.91, cex.main = 0.98,
      col.axis = "#333333", col.lab = "#222222")

  panel_title <- function(label, title) {
    title(main = paste0("(", label, ")  ", title), adj = 0, font.main = 2)
  }
  labelled_barplot <- function(values, names, colour, ylim, ylab, xlab, label, title,
                               cex_names = 0.78) {
    positions <- barplot(values, names.arg = names, col = colour, border = NA,
                         ylim = ylim, ylab = ylab, xlab = xlab, cex.names = cex_names,
                         axes = FALSE, axisnames = FALSE)
    abline(h = axTicks(2), col = light_grey, lwd = 0.8)
    axis(2)
    axis(1, at = positions, labels = names, tick = FALSE, line = 0.15,
         cex.axis = cex_names)
    box(bty = "l")
    text(positions, values, labels = values, pos = 3, cex = 0.76, xpd = NA)
    panel_title(label, title)
  }

  h_x <- seq(0.60, 1.00, length.out = 600L)
  h_density <- dbeta((h_x - h_lower) / (h_upper - h_lower), h_alpha, h_beta) /
    (h_upper - h_lower)
  h_hist <- hist(design$steepness, breaks = seq(0.60, 1.00, by = 0.025),
                 plot = FALSE, right = FALSE)
  plot(h_x, h_density, type = "n", xlim = c(0.60, 1.00),
       ylim = c(0, max(c(h_density, h_hist$density)) * 1.12),
       xlab = expression("Steepness, " * h), ylab = "Density", yaxs = "i")
  abline(h = axTicks(2), col = light_grey, lwd = 0.8)
  polygon(c(h_x, rev(h_x)), c(h_density, rep(0, length(h_density))),
          col = grDevices::adjustcolor(blue_fill, alpha.f = 0.42), border = NA)
  rect(h_hist$breaks[-length(h_hist$breaks)], 0,
       h_hist$breaks[-1L], h_hist$density,
       col = grDevices::adjustcolor(blue, alpha.f = 0.32), border = "white", lwd = 0.6)
  lines(h_x, h_density, col = blue, lwd = 2.0)
  rug(design$steepness, col = grDevices::adjustcolor(blue, alpha.f = 0.42),
      ticksize = 0.025, lwd = 0.7)
  abline(v = h_mean, col = orange, lwd = 1.6, lty = 2)
  legend("topleft",
         legend = c("Actual 100 values", "Prior density", sprintf("Mean = %.2f", h_mean)),
         col = c(blue, blue, orange), lty = c(NA, 1, 2), lwd = c(NA, 2.0, 1.6),
         pch = c(15, NA, NA), pt.cex = 1.2, bty = "n", cex = 0.72,
         inset = c(0.02, 0.02))
  panel_title("a", "Steepness")

  mixing_values <- as.integer(table(factor(
    design$tag_mixing_k_cutoff, levels = k_cutoff_levels
  )))
  labelled_barplot(mixing_values, format(k_cutoff_levels, nsmall = 2), green,
                   c(0, 30), "Models", "Kolmogorov dissimilarity cutoff, K",
                   "b", "Tag mixing periods: K cutoff")

  reporting_values <- as.integer(table(factor(
    design$tag_reporting, levels = c("inclusion", "exclusion")
  )))
  labelled_barplot(reporting_values, c("Include\n(flag 2 = 0)", "Exclude\n(flag 2 = 1)"),
                   c(purple, grDevices::adjustcolor(purple, alpha.f = 0.68)),
                   c(0, 57), "Models", "Pre-mixing reporting", "c", "Tag reporting",
                   cex_names = 0.72)

  m_x <- seq(0.035, 0.180, length.out = 800L)
  selected_density <- dbounded_logit_normal(m_x)
  hc_density <- dlnorm(m_x, log(m_hc_m0_median), m_log_sd_hamel_cope)
  m_hist <- hist(design$m_age40_quarterly, breaks = seq(0.05, 0.135, by = 0.005),
                 plot = FALSE, right = FALSE)
  m_ymax <- max(c(selected_density, hc_density, m_hist$density)) * 1.17
  plot(m_x, selected_density, type = "n", xlim = range(m_x), ylim = c(0, m_ymax),
       xlab = expression("Quarterly natural mortality at reference length,"~M[0]~(quarter^{-1})),
       ylab = "Density", yaxs = "i")
  abline(h = axTicks(2), col = light_grey, lwd = 0.8)
  polygon(c(m_x, rev(m_x)), c(selected_density, rep(0, length(selected_density))),
          col = grDevices::adjustcolor(orange_fill, alpha.f = 0.42), border = NA)
  rect(m_hist$breaks[-length(m_hist$breaks)], 0,
       m_hist$breaks[-1L], m_hist$density,
       col = grDevices::adjustcolor(orange, alpha.f = 0.32), border = "white", lwd = 0.6)
  lines(m_x, selected_density, col = orange, lwd = 2.0)
  lines(m_x, hc_density, col = grey, lwd = 1.6, lty = 2)
  rug(design$m_age40_quarterly, col = grDevices::adjustcolor(orange, alpha.f = 0.42),
      ticksize = 0.025, lwd = 0.7)
  tag_y <- m_ymax * 0.075
  segments(m_tag_lower90, tag_y, m_tag_upper90, tag_y, col = green, lwd = 2.2)
  points(m_tag_estimate, tag_y, pch = 19, col = green, cex = 0.9)
  abline(v = m_previous, col = blue, lwd = 1.4, lty = 3)
  legend("topright",
         legend = c("Actual 100 values", "Selected distribution",
                    "Hamel-Cope, scaled to M0", "Tag estimate (90% CI)",
                    "2023 assessment"),
         col = c(orange, orange, grey, green, blue), lty = c(NA, 1, 2, 1, 3),
         lwd = c(NA, 2.0, 1.6, 2.2, 1.4), pch = c(15, NA, NA, 19, NA),
         pt.cex = 1.1, bty = "n", cex = 0.63, inset = c(0.01, 0.01))
  panel_title("d", "Natural mortality")

  effort_values <- as.integer(table(factor(
    design$effort_creep_primary, levels = effort$effort_creep_primary
  )))
  effort_names <- sprintf("%.1f / %.2f", 100 * effort$effort_creep_primary,
                          100 * effort$effort_creep_secondary)
  labelled_barplot(effort_values, effort_names, blue, c(0, 24), "Models",
                   "Primary / secondary effort creep (%)", "e", "Effort creep",
                   cex_names = 0.68)

  tau_values <- as.integer(table(factor(design$tag_tau, levels = tau_levels)))
  labelled_barplot(tau_values, format(tau_levels, nsmall = 1), orange,
                   c(0, 39), "Models", expression("Fixed tag overdispersion, " * tau),
                   "f", "Tag overdispersion")
}

png(file.path(output_dir, "distributions.png"), width = 3200, height = 1900, res = 300)
draw_publication_figure()
dev.off()

pdf(file.path(output_dir, "distributions.pdf"), width = 10.67, height = 6.33,
    useDingbats = FALSE, title = "BET 2026 structural ensemble marginal distributions")
draw_publication_figure()
dev.off()

draw_pairing_diagnostic <- function() {
  panel_histogram <- function(x, ...) {
    usr <- par("usr")
    on.exit(par(usr = usr), add = TRUE)
    par(usr = c(usr[1:2], 0, 1.08))
    bins <- hist(x, plot = FALSE, breaks = "FD")
    height <- bins$counts / max(bins$counts)
    rect(bins$breaks[-length(bins$breaks)], 0, bins$breaks[-1L], height,
         col = grDevices::adjustcolor("#0072B2", alpha.f = 0.55), border = "white")
  }
  panel_points <- function(x, y, ...) {
    points(x, y, pch = 16, cex = 0.42,
           col = grDevices::adjustcolor("#1B4965", alpha.f = 0.34))
  }
  panel_correlation <- function(x, y, ...) {
    usr <- par("usr")
    on.exit(par(usr = usr), add = TRUE)
    par(usr = c(0, 1, 0, 1))
    rho <- suppressWarnings(cor(x, y, method = "spearman"))
    text(0.5, 0.5, sprintf("rho = %.3f", rho),
         cex = 0.82 + 0.9 * abs(rho), col = "#17324D")
  }
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(mar = c(1.4, 1.4, 1.4, 1.4), oma = c(0.4, 0.4, 0.4, 0.4),
      mgp = c(1.2, 0.35, 0), tcl = -0.18, cex.axis = 0.55)
  pairs(
    numeric_design,
    labels = c("Steepness", "Tau", "K cutoff", "RR flag", "M0 / qtr", "Effort creep"),
    lower.panel = panel_points,
    upper.panel = panel_correlation,
    diag.panel = panel_histogram,
    gap = 0.35,
    cex.labels = 0.82,
    font.labels = 2
  )
}

png(file.path(output_dir, "pairing-diagnostics.png"), width = 2400, height = 2400,
    res = 300)
draw_pairing_diagnostic()
dev.off()

pdf(file.path(output_dir, "pairing-diagnostics.pdf"), width = 8, height = 8,
    useDingbats = FALSE, title = "BET 2026 randomized ensemble pairing diagnostic")
draw_pairing_diagnostic()
dev.off()

cat("Created ", n_models, " ensemble draws in ", output_dir, "\n", sep = "")
cat(sprintf("Steepness: mean %.4f, SD %.4f, range %.4f-%.4f\n",
            mean(design$steepness), sd(design$steepness),
            min(design$steepness), max(design$steepness)))
cat(sprintf("Quarterly M0 at reference length: mean %.4f, median %.5f, mode %.4f, finite range %.3f-%.3f\n",
            mean(design$m_age40_quarterly), median(design$m_age40_quarterly), m_mode,
            min(design$m_age40_quarterly), max(design$m_age40_quarterly)))
cat(sprintf("M logit-SD: %.6f (bounded logit-normal)\n", m_logit_sd))
cat(sprintf("Tag tau: %s models at %s\n",
            paste(tau_counts, collapse = "/"), paste(tau_levels, collapse = "/")))
cat(sprintf("Frozen random pairing: maximum absolute pairwise Spearman correlation %.4f\n",
            pairing_max_abs_spearman))
