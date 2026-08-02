#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_arg) != 1L) stop("Cannot locate repository root.", call. = FALSE)
repo <- normalizePath(file.path(dirname(sub("^--file=", "", script_arg)), ".."), mustWork = TRUE)
design_dir <- file.path(repo, "design")

draws <- read.csv(file.path(design_dir, "model-draws.csv"), check.names = FALSE)
continuous <- read.csv(file.path(design_dir, "continuous-summary.csv"), check.names = FALSE)
discrete <- read.csv(file.path(design_dir, "discrete-summary.csv"), check.names = FALSE)
effort <- read.csv(file.path(design_dir, "effort-creep-sources.csv"), check.names = FALSE)

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
  all(nchar(effort$source_sha256) == 64L),
  all(draws$initialization == "Diagnostic seed-23 path"),
  all(draws$design_seed == 20260802L)
)

cat("Validated 100 deterministic BET 2026 ensemble configurations.\n")
