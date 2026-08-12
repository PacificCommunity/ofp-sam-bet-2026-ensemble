#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(FLR4MFCL))

script_argument <- grep("^--file=", commandArgs(), value = TRUE)
script_file <- normalizePath(sub("^--file=", "", script_argument[[1L]]), mustWork = TRUE)
source(file.path(dirname(script_file), "native-projection-par.R"), local = TRUE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 6L) {
  stop(
    paste(
      "Usage: configure-native-par.R final.par 00.par proj.frq",
      "output-dir nsims recruitment-seed"
    ),
    call. = FALSE
  )
}

final_file <- normalizePath(args[[1]], mustWork = TRUE)
zero_file <- normalizePath(args[[2]], mustWork = TRUE)
frq_file <- normalizePath(args[[3]], mustWork = TRUE)
output_dir <- normalizePath(args[[4]], mustWork = TRUE)
nsims <- as.integer(args[[5]])
recruitment_seed <- as.integer(args[[6]])
if (!is.finite(recruitment_seed) || recruitment_seed < 1L) {
  stop("recruitment-seed must be a positive integer.", call. = FALSE)
}

frq <- read.MFCLFrq(frq_file)
first_year <- range(frq)["minyear"]
final_par <- read.MFCLPar(final_file, first.yr = first_year)
zero_par <- read.MFCLPar(zero_file, first.yr = first_year)

recruitment_period <- recPeriod(
  final_par,
  af199 = flagval(final_par, 2, 199)$value,
  af200 = flagval(final_par, 2, 200)$value
)
if (any(!is.finite(recruitment_period[c("pf232", "pf233")]))) {
  stop("Could not map the fitted SRR period to MFCL stochastic indices.")
}
periods_per_year <- as.integer(flagval(final_par, 2, 57)$value)
annualised_srr <- as.integer(flagval(final_par, 2, 182)$value)
if (annualised_srr != 1L || periods_per_year < 2L) {
  stop("This verified BET projection expects an annualised, seasonal SRR.")
}
annual_index_start <- recruitment_period[["pf232"]] %/% periods_per_year + 1L
annual_index_end <- (recruitment_period[["pf233"]] - 1L) %/% periods_per_year + 1L
recruitment_year_start <- as.integer(range(frq)["minyear"]) + annual_index_start - 1L
recruitment_year_end <- as.integer(range(frq)["minyear"]) + annual_index_end - 1L
fitted_terminal_year <- as.integer(range(final_par)["maxyear"])
if (recruitment_year_end > fitted_terminal_year) {
  stop("Recruitment sampling window extends beyond the fitted model.")
}

proj_par <- extend_native_projection_par(final_par, zero_par, frq)
state_audit <- attr(proj_par, "projection_state_audit")
write.csv(
  state_audit,
  file.path(output_dir, "projection-state-audit.csv"),
  row.names = FALSE
)

flagval(proj_par, 1, 1) <- 1
flagval(proj_par, 2, 20) <- nsims
# Preserve the fitted tag-recapture horizon.  This is part of the assessment
# model, not a projection scenario control; shortening it can make an otherwise
# valid mixing period extend beyond a release group's terminal tag period.
fitted_tag_horizon <- as.integer(flagval(final_par, 2, 96)$value)
if (!is.finite(fitted_tag_horizon) || fitted_tag_horizon < 1L) {
  stop("The fitted model has an invalid tag-recapture horizon (age flag 96).")
}
flagval(proj_par, 2, 96) <- fitted_tag_horizon
flagval(proj_par, 1, c(186, 189)) <- 1
flagval(proj_par, 2, c(148, 155)) <- c(20, 4)
flagval(proj_par, 2, 161) <- 1
flagval(proj_par, 2, 183) <- 1
flagval(proj_par, 1, c(232, 233)) <- recruitment_period[c("pf232", "pf233")]
flagval(proj_par, 1, 231) <- recruitment_seed
flagval(proj_par, 1, c(234, 237, 238, 241, 242)) <- 0
flagval(proj_par, 1, 239) <- 1
# Option 7 must be run once for the fitted fishing state and once for the
# no-fishing state.  Native option 8 then reads both sets of dependent values.
flagval(proj_par, -(1:33), 55) <- 0

# Preserve the fitted negative-binomial tag-overdispersion level for each
# ensemble member.  The ensemble uses fixed tau values of 4.96, 5.14 or 5.20,
# represented natively by fish_pars(4) = log(tau - 1).
if (!identical(as.numeric(flagval(proj_par, 1, 305)$value), 1)) {
  stop("Native negative-binomial tag likelihood (parest 305=1) was not retained.")
}
tau_row <- fish_params(proj_par)[4, ]
fitted_tau_row <- fish_params(final_par)[4, ]
if (
  any(!is.finite(tau_row)) || any(!is.finite(fitted_tau_row)) ||
    length(tau_row) != length(fitted_tau_row) ||
    max(abs(tau_row - fitted_tau_row)) > 1e-10
) {
  stop("The generated projection par did not preserve fitted fish_pars(4).")
}
tau_value <- 1 + exp(unique(round(tau_row, 12)))
if (length(tau_value) != 1L || !any(abs(tau_value - c(4.96, 5.14, 5.20)) < 1e-8)) {
  stop("The fitted ensemble tau is not one of 4.96, 5.14 or 5.20.")
}

option7_fished <- proj_par
flagval(option7_fished, 1, 145) <- 7
flagval(option7_fished, -(1:33), 55) <- 0
write(option7_fished, file.path(output_dir, "proj7-fished.par"))
invisible(restore_projection_compilation_version(
  zero_file,
  file.path(output_dir, "proj7-fished.par")
))

option7_noeff <- proj_par
flagval(option7_noeff, 1, 145) <- 7
flagval(option7_noeff, -(1:33), 55) <- 1
write(option7_noeff, file.path(output_dir, "proj7-noeff.par"))
invisible(restore_projection_compilation_version(
  zero_file,
  file.path(output_dir, "proj7-noeff.par")
))

option8 <- proj_par
flagval(option8, 1, 145) <- 8
flagval(option8, -(1:33), 55) <- 1
write(option8, file.path(output_dir, "proj8.par"))
invisible(restore_projection_compilation_version(
  zero_file,
  file.path(output_dir, "proj8.par")
))

projection <- proj_par
flagval(projection, 1, 145) <- 0
flagval(projection, 1, c(187, 188)) <- 0
flagval(projection, -(1:33), 55) <- 1
write(projection, file.path(output_dir, "proj.par"))
compilation_version <- restore_projection_compilation_version(
  zero_file,
  file.path(output_dir, "proj.par")
)

audit <- data.frame(
  flag_type = c(rep("parest", 13), rep("age", 6), "fish_pars", "derived"),
  flag = c(1, 145, 186, 190, 231, 232, 233, 234, 237, 238, 239, 241, 305,
           20, 96, 161, 182, 183, 199, 4, NA),
  value = c(
    flagval(projection, 1, 1)$value,
    flagval(projection, 1, 145)$value,
    flagval(projection, 1, 186)$value,
    flagval(projection, 1, 190)$value,
    flagval(projection, 1, 231)$value,
    flagval(projection, 1, 232)$value,
    flagval(projection, 1, 233)$value,
    flagval(projection, 1, 234)$value,
    flagval(projection, 1, 237)$value,
    flagval(projection, 1, 238)$value,
    flagval(projection, 1, 239)$value,
    flagval(projection, 1, 241)$value,
    flagval(projection, 1, 305)$value,
    flagval(projection, 2, 20)$value,
    flagval(projection, 2, 96)$value,
    flagval(projection, 2, 161)$value,
    flagval(projection, 2, 182)$value,
    flagval(projection, 2, 183)$value,
    flagval(projection, 2, 199)$value,
    paste(format(tau_row, scientific = FALSE, trim = TRUE), collapse = ","),
    format(tau_value, nsmall = 2, trim = TRUE)
  ),
  stringsAsFactors = FALSE
)
audit <- rbind(
  audit,
  data.frame(
    flag_type = "file",
    flag = NA_real_,
    value = compilation_version,
    stringsAsFactors = FALSE
  )
)
write.csv(audit, file.path(output_dir, "native-flag-audit.csv"), row.names = FALSE)
write.csv(
  data.frame(
    basis = "period used to estimate the stock-recruitment relationship",
    pf232 = recruitment_period[["pf232"]],
    pf233 = recruitment_period[["pf233"]],
    annual_index_start = annual_index_start,
    annual_index_end = annual_index_end,
    calendar_year_start = recruitment_year_start,
    calendar_year_end = recruitment_year_end,
    stringsAsFactors = FALSE
  ),
  file.path(output_dir, "recruitment-sampling-window.csv"),
  row.names = FALSE
)

cat(sprintf(
  paste0(
    "Configured MFCL options 7/8 and final pars; recruitment indices ",
    "%d--%d (%d--%d); seed %d; tau=%.2f fixed.\n"
  ),
  recruitment_period[["pf232"]], recruitment_period[["pf233"]],
  recruitment_year_start, recruitment_year_end, recruitment_seed, tau_value
))
