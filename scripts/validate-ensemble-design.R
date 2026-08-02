#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_arg) != 1L) stop("Cannot locate repository root.", call. = FALSE)
repo <- normalizePath(file.path(dirname(sub("^--file=", "", script_arg)), ".."), mustWork = TRUE)
design_dir <- file.path(repo, "design")

draws <- read.csv(file.path(design_dir, "model-draws.csv"), check.names = FALSE)
continuous <- read.csv(file.path(design_dir, "continuous-summary.csv"), check.names = FALSE)
discrete <- read.csv(file.path(design_dir, "discrete-summary.csv"), check.names = FALSE)
effort <- read.csv(file.path(design_dir, "effort-creep-sources.csv"), check.names = FALSE)
parameters <- read.csv(file.path(design_dir, "distribution-parameters.csv"), check.names = FALSE)
m_evidence <- read.csv(file.path(design_dir, "m-evidence.csv"), check.names = FALSE)
script_text <- readLines(file.path(repo, "scripts", "create-ensemble-design.R"), warn = FALSE)

stopifnot(
  nrow(draws) == 100L,
  length(unique(draws$ensemble_id)) == 100L,
  identical(draws$ensemble_id, sprintf("ensemble-%03d", seq_len(100L))),
  identical(as.integer(table(draws$tag_mixing_period)), c(6L, 12L, 19L, 26L, 19L, 12L, 6L)),
  identical(as.integer(table(draws$tag_reporting_flag2)), c(50L, 50L)),
  identical(as.integer(table(draws$effort_creep_primary)), rep(20L, 5L)),
  abs(mean(draws$steepness) - 0.87) < 0.001,
  abs(sd(draws$steepness) - 0.063) < 0.002,
  abs(min(draws$m_age40_quarterly) - 0.050) < 1e-12,
  abs(max(draws$m_age40_quarterly) - 0.165) < 1e-12,
  all(abs(draws$lorenzen_log_intercept - log(draws$m_age40_quarterly)) < 1e-12),
  nrow(continuous) == 2L,
  nrow(discrete) == 14L,
  nrow(effort) == 5L,
  nrow(parameters) == 30L,
  nrow(m_evidence) == 4L,
  all(nchar(effort$source_sha256) == 64L),
  all(draws$initialization == "Diagnostic seed-23 path"),
  all(draws$pairing_version == "mod101-v1"),
  !"design_seed" %in% names(draws),
  !any(grepl("set[.]seed|RNGkind|sample[.(]", script_text)),
  abs(m_evidence$central[1] - 0.0624) < 1e-12,
  abs(m_evidence$lower[1] - 0.0500) < 1e-12,
  abs(m_evidence$upper[1] - 0.0749) < 1e-12,
  abs(m_evidence$central[3] - 0.0900) < 1e-12,
  abs(m_evidence$secondary_central[3] - 0.094430080) < 1e-8,
  abs(m_evidence$lower[3] - 0.049019630) < 1e-8,
  abs(m_evidence$upper[3] - 0.165239924) < 1e-8,
  file.info(file.path(design_dir, "distributions.png"))$size > 10000,
  file.info(file.path(design_dir, "distributions.pdf"))$size > 10000
)

cat("Validated 100 deterministic BET 2026 ensemble configurations.\n")
