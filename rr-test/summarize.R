#!/usr/bin/env Rscript

# Transparent reconstruction from the historical v2.5 native outputs.
# No FLR packages, model execution, Hessian estimates or known stock truth.
# Usage: Rscript rr-test/summarize.R [output-root]
# The optional new output root receives results/ and figures/; default: outputs/rr-test-summary/.
options(stringsAsFactors = FALSE, warn = 2)
argv <- commandArgs(trailingOnly = TRUE)
if (length(argv) > 1L) stop("Usage: Rscript rr-test/summarize.R [output-root]")
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_arg) != 1L) stop("Run this script with Rscript.")
script <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
repo <- dirname(dirname(script))
out <- normalizePath(if (length(argv)) argv[[1L]] else file.path(repo, "outputs", "rr-test-summary"), mustWork = FALSE)
out_link <- Sys.readlink(out)
if (file.exists(out) || dir.exists(out) || (!is.na(out_link) && nzchar(out_link))) {
  stop("Choose a new summary output folder; existing paths are refused.", call. = FALSE)
}
if (identical(out, repo) || (startsWith(out, paste0(repo, .Platform$file.sep)) &&
    !startsWith(out, paste0(repo, .Platform$file.sep, "outputs", .Platform$file.sep)))) {
  stop("Summary outputs inside the checkout must be beneath outputs/.", call. = FALSE)
}
result_dir <- file.path(out, "results")
figure_dir <- file.path(out, "figures")

fail <- function(...) stop(..., call. = FALSE)
assert <- function(test, ...) if (!isTRUE(test)) fail(...)
read_csv <- function(path) utils::read.csv(path, check.names = FALSE)
path <- function(...) file.path(repo, ...)

# Read the ordinary saved MFCL files in the standalone kit. No model is executed.
saved_root <- tempfile("bet-rr-saved-")
dir.create(saved_root)
utils::unzip(path("rr-test", "standalone.zip"), exdir = saved_root)
kit <- file.path(saved_root, "bet-2026-rr-standalone")
status <- system2(file.path(R.home("bin"), "Rscript"),
                  c(shQuote(file.path(kit, "run-final.R")), "verify"))
assert(status == 0L, "Saved MFCL file verification failed.")
saved <- read_csv(file.path(kit, "models.csv"))
saved_file <- function(model, column) {
  # The four failed RR1 attempts still have their original RR0 anchors.
  if (startsWith(model, "ensemble-")) {
    filename <- if (column == "final_par") "final.par" else "plot-11.par.rep"
    return(path("final-par", model, filename))
  }
  rows <- saved[saved$model == model, , drop = FALSE]
  assert(nrow(rows) == 1L, "Expected a unique saved model: ", model)
  filename <- if (column == "final_par") "final.par" else "reference.rep"
  file.path(kit, "models", model, filename)
}

# Section boundaries come from headers, never fixed line offsets. In this
# historical single-species report, biomass rows are quarters and columns are
# regions. Assert both row and column counts before aggregating.
read_sections <- function(filename) {
  x <- readLines(filename, warn = FALSE)
  h <- which(grepl("^[[:space:]]*#", x))
  labels <- trimws(sub("^[[:space:]]*#[[:space:]]*", "", x[h]))
  function(label, nrow, ncol = 1L) {
    k <- which(labels == label)
    assert(length(k) == 1L, filename, ": expected exactly one header: ", label)
    first <- h[k] + 1L
    last <- if (k < length(h)) h[k + 1L] - 1L else length(x)
    assert(first <= last, filename, ": empty section: ", label)
    rows <- trimws(x[seq.int(first, last)])
    rows <- rows[nzchar(rows)]
    assert(length(rows) == nrow, filename, ": ", label, " has ",
           length(rows), " rows; expected ", nrow)
    tokens <- strsplit(rows, "[[:space:]]+")
    assert(all(lengths(tokens) == ncol), filename, ": ", label,
           " has an unexpected column count; expected ", ncol)
    values <- suppressWarnings(as.numeric(unlist(tokens, use.names = FALSE)))
    assert(length(values) == nrow * ncol && all(is.finite(values)),
           filename, ": non-finite/non-numeric values in ", label)
    matrix(values, nrow = nrow, ncol = ncol, byrow = TRUE)
  }
}

read_rep <- function(filename) {
  s <- read_sections(filename)
  scalar <- function(label) as.numeric(s(label, 1L))
  nperiod <- scalar("Number of time periods")
  first_year <- scalar("Year 1")
  nregion <- scalar("Number of regions")
  seasons <- scalar("Number of recruitments per year")
  assert(nperiod == 292 && first_year == 1952 && nregion == 5 && seasons == 4 &&
           scalar("Number of species") == 1 &&
           scalar("Number of age classes") == 40,
         filename, ": report dimensions differ from this historical campaign.")
  version <- readLines(filename, n = 6L, warn = FALSE)
  assert(any(trimws(version) == "# MULTIFAN-CL version number: 2.2.7.9"),
         filename, ": unexpected MULTIFAN-CL version.")
  sbq <- rowSums(s("Adult biomass", nperiod, nregion))
  sb0q <- rowSums(s("Adult biomass in absence of fishing", nperiod, nregion))
  assert(all(sbq > 0) && all(sb0q > 0), filename, ": non-positive biomass.")
  years <- seq.int(first_year, length.out = nperiod / seasons)
  # Sum regions within each quarter, then take the mean of four quarters/year.
  sb <- stats::setNames(colMeans(matrix(sbq, nrow = seasons)), years)
  sb0 <- stats::setNames(colMeans(matrix(sb0q, nrow = seasons)), years)
  sbmsy <- scalar("Adult biomass at MSY")
  fmult <- scalar("F multiplier at MSY")
  assert(sbmsy > 0 && fmult > 0, filename, ": non-positive MSY reference.")
  list(year = years, sb = sb, sb0 = sb0, sbmsy = sbmsy, fmult = fmult)
}

read_par <- function(filename) {
  s <- read_sections(filename)
  objective <- as.numeric(s("Objective function value", 1L))
  mgc <- as.numeric(s("Maximum magnitude gradient value", 1L))
  assert(objective > 0 && mgc >= 0, filename, ": invalid objective or gradient.")
  list(objective = objective, mgc = mgc)
}

# These definitions reproduce scripts/build-ensemble-public-data.R. The recent
# LRP ratio uses two separate period means; it is not an average annual ratio.
# The rolling annual-depletion quantities remain separate for reference parity.
quantities <- function(rep) {
  period <- function(v, years) {
    assert(all(as.character(years) %in% names(v)), "Incomplete period.")
    mean(v[as.character(years)])
  }
  rolling_depletion <- function(target) {
    mean(vapply(target, function(y) {
      rep$sb[[as.character(y)]] / period(rep$sb0, (y - 10L):(y - 1L))
    }, numeric(1)))
  }
  recent <- period(rep$sb, 2021:2024)
  recent0 <- period(rep$sb0, 2014:2023)
  recent_rolling <- rolling_depletion(2021:2024)
  historical_rolling <- rolling_depletion(2012:2015)
  c(sb_recent_kt = recent / 1000,
    sb0_recent_kt = recent0 / 1000,
    sb_recent_sb0 = recent / recent0,
    sb_recent_sbmsy = recent / rep$sbmsy,
    f_recent_fmsy = 1 / rep$fmult,
    recent_mean_depletion = recent_rolling,
    historical_target_depletion = historical_rolling,
    recent_historical_target_ratio = recent_rolling / historical_rolling)
}

# Values in the published CSVs were built from rounded native REP output.
# 1e-10 absolute/relative tolerance permits only floating-point arithmetic noise,
# not a different depletion definition or a higher-precision substitute.
parity_error <- 0
check_equal <- function(actual, expected, label, tolerance = 1e-10) {
  assert(length(actual) == length(expected) && all(is.finite(expected)),
         label, ": missing/non-finite reference values.")
  error <- max(abs(actual - expected))
  parity_error <<- max(parity_error, error)
  assert(all(abs(actual - expected) <= tolerance * pmax(1, abs(expected))),
         label, ": reference parity failed; maximum absolute difference = ", error)
  error
}

design <- read_csv(path("rr-test", "model-draws.csv"))
manifest <- read_csv(path("data", "ensemble", "retained-final-par-manifest.csv"))
split_manifest <- read_csv(path("rr-test", "reference", "retained-final-par-rr-split-manifest.csv"))
baseline <- read_csv(path("data", "ensemble", "management-quantities.csv"))
baseline_ts <- read_csv(path("data", "ensemble", "ensemble-timeseries.csv"))
reference <- read_csv(path("rr-test", "reference", "rr-completed-aggregate-paired-model-values.csv"))
drivers <- read_csv(path("rr-test", "reference", "rr-paired-effect-drivers-model-level.csv"))
assert(nrow(design) == 34L && !anyDuplicated(design$ensemble_id) &&
         !anyDuplicated(design$anchor_ensemble_id), "Expected 34 unique planned pairs.")
assert(nrow(reference) == 30L && !anyDuplicated(reference$model) &&
         nrow(drivers) == 30L && !anyDuplicated(drivers$model),
       "Expected the two recovered original 30-pair reference tables.")
fits <- saved$model[grepl("^rrtest-[0-9]{3}-rr1$", saved$model)]
assert(length(fits) == 30L && all(fits %in% design$ensemble_id),
       "Expected exactly the 30 recovered RR1 saved models.")
failed <- sort(design$anchor_ensemble_id[!design$ensemble_id %in% fits])
assert(identical(failed, paste0("ensemble-", c("001", "022", "025", "080"))),
       "Recovered/missing IDs differ from the documented campaign.")
assert(setequal(reference$model, design$anchor_ensemble_id[design$ensemble_id %in% fits]) &&
         setequal(drivers$model, reference$model), "Original reference pair IDs differ.")

pair_rows <- list()
series_rows <- list()
baseline_errors <- numeric()
original_errors <- numeric()
for (i in seq_len(nrow(design))) {
  draw <- design[i, , drop = FALSE]
  id0 <- draw$anchor_ensemble_id
  id1 <- draw$ensemble_id
  source <- split_manifest[split_manifest$ensemble_id == id0, , drop = FALSE]
  ledger <- manifest[manifest$ensemble_id == id0, , drop = FALSE]
  assert(nrow(source) == 1L && nrow(ledger) == 1L && source$rr_group == "inclusion",
         id0, ": missing unique original RR0 manifest anchor.")
  rep0 <- read_rep(saved_file(id0, "rep"))
  par0 <- read_par(saved_file(id0, "final_par"))
  check_equal(par0$mgc, ledger$maximum_gradient_component, paste(id0, "manifest MGC"), 1e-12)
  check_equal(par0$objective, ledger$objective_function, paste(id0, "manifest objective"), 1e-12)
  assert(par0$mgc <= 1e-4, id0, ": RR0 MGC fails retention.")
  q0 <- quantities(rep0)
  ref0 <- baseline[baseline$ensemble_id == id0, , drop = FALSE]
  assert(nrow(ref0) == 1L, id0, ": missing baseline management row.")
  baseline_errors <- c(baseline_errors, check_equal(q0, as.numeric(ref0[names(q0)]),
                                                   paste(id0, "RR0 management quantities")))
  ts0 <- baseline_ts[baseline_ts$ensemble_id == id0, , drop = FALSE]
  ts0 <- ts0[match(rep0$year, ts0$year), , drop = FALSE]
  assert(nrow(ts0) == length(rep0$year) && identical(as.integer(ts0$year), as.integer(rep0$year)),
         id0, ": incomplete baseline time series.")
  baseline_errors <- c(baseline_errors,
    check_equal(as.numeric(rep0$sb) / 1000, ts0$spawning_potential, paste(id0, "annual SB")),
    check_equal(as.numeric(rep0$sb0) / 1000, ts0$spawning_potential_nofish, paste(id0, "annual SBF0")),
    check_equal(as.numeric(rep0$sb / rep0$sb0), ts0$depletion, paste(id0, "annual depletion")))
  if (!id1 %in% fits) next
  rep1 <- read_rep(saved_file(id1, "rep"))
  par1 <- read_par(saved_file(id1, "final_par"))
  assert(par1$mgc <= 1e-4, id1, ": RR1 MGC fails retention.")
  q1 <- quantities(rep1)
  orig <- reference[reference$model == id0, , drop = FALSE]
  drv <- drivers[drivers$model == id0, , drop = FALSE]
  # Retain original job identity as a source reference, not a local job ID.
  assert(orig$kflow_job == drv$kflow_job && isTRUE(orig$mgc_pass_rr0) &&
           isTRUE(orig$mgc_pass_rr1), id0, ": inconsistent original reference identity/retention.")
  reference_names <- c(sb_recent_kt = "sb_recent_kt", sb0_recent_kt = "sb0_recent_kt",
    sb_recent_sb0 = "sb_recent_sb0", sb_recent_sbmsy = "sb_recent_sbmsy",
    f_recent_fmsy = "f_recent_fmsy", recent_mean_depletion = "cmm_recent",
    historical_target_depletion = "cmm_historical", recent_historical_target_ratio = "cmm_ratio")
  for (rr in 0:1) {
    q <- if (rr == 0) q0 else q1
    original_errors <- c(original_errors,
      check_equal(q, as.numeric(orig[paste0(reference_names, "_rr", rr)]),
                  paste(id0, "original aggregate RR", rr)))
  }
  check_equal(c(par0$mgc, par1$mgc), c(orig$mgc_rr0, orig$mgc_rr1),
              paste(id0, "original gradients"), 1e-12)
  driver_fields <- c("tag_mixing_k_cutoff", "zero_mixing_events", "steepness",
    "m_age40_quarterly", "tag_tau", "effort_creep_primary", "effort_creep_secondary")
  check_equal(as.numeric(draw[driver_fields]), as.numeric(drv[driver_fields]),
              paste(id0, "original design settings"))
  check_equal(100 * c(q1["sb_recent_sb0"] - q0["sb_recent_sb0"],
                     q1["sb_recent_sbmsy"] - q0["sb_recent_sbmsy"],
                     q1["f_recent_fmsy"] - q0["f_recent_fmsy"],
                     q1["recent_mean_depletion"] - q0["recent_mean_depletion"]),
              as.numeric(drv[c("delta_D_pp", "delta_SBMSY_pp", "delta_FMSY_pp", "delta_CMM_pp")]),
              paste(id0, "original driver deltas"))
  row <- data.frame(anchor_ensemble_id = id0, rr1_ensemble_id = id1,
    rr0_kflow_job = ledger$kflow_job, rr1_kflow_job = orig$kflow_job,
    steepness = draw$steepness, tag_tau = draw$tag_tau,
    m_age40_quarterly = draw$m_age40_quarterly, tag_mixing_k_cutoff = draw$tag_mixing_k_cutoff,
    effort_creep_primary = draw$effort_creep_primary,
    effort_creep_secondary = draw$effort_creep_secondary,
    zero_mixing_events = draw$zero_mixing_events,
    mgc_rr0 = par0$mgc, mgc_rr1 = par1$mgc,
    objective_function_rr0 = par0$objective, objective_function_rr1 = par1$objective,
    sb_recent_period = "2021-2024", sb0_recent_period = "2014-2023", f_recent_period = "2020-2023")
  for (quantity in names(q0)) {
    row[[paste0(quantity, "_rr0")]] <- q0[[quantity]]
    row[[paste0(quantity, "_rr1")]] <- q1[[quantity]]
    row[[paste0("delta_", quantity, "_rr1_minus_rr0")]] <- q1[[quantity]] - q0[[quantity]]
  }
  pair_rows[[id0]] <- row
  series_rows[[id0]] <- data.frame(anchor_ensemble_id = id0, rr1_ensemble_id = id1,
    year = rep0$year, m_age40_quarterly = draw$m_age40_quarterly,
    tag_mixing_k_cutoff = draw$tag_mixing_k_cutoff,
    sb_annual_kt_rr0 = as.numeric(rep0$sb) / 1000, sb_annual_kt_rr1 = as.numeric(rep1$sb) / 1000,
    sbf0_annual_kt_rr0 = as.numeric(rep0$sb0) / 1000, sbf0_annual_kt_rr1 = as.numeric(rep1$sb0) / 1000,
    depletion_rr0 = as.numeric(rep0$sb / rep0$sb0), depletion_rr1 = as.numeric(rep1$sb / rep1$sb0),
    delta_depletion_rr1_minus_rr0 = as.numeric(rep1$sb / rep1$sb0 - rep0$sb / rep0$sb0))
}
pairs <- do.call(rbind, pair_rows)
series <- do.call(rbind, series_rows)
pairs <- pairs[order(pairs$m_age40_quarterly, pairs$anchor_ensemble_id), , drop = FALSE]
series <- series[order(match(series$anchor_ensemble_id, pairs$anchor_ensemble_id), series$year), , drop = FALSE]
row.names(pairs) <- row.names(series) <- NULL
assert(nrow(pairs) == 30L && nrow(series) == 30L * 73L, "Incomplete reconstructed pair tables.")
summary <- do.call(rbind, lapply(names(q0), function(quantity) {
  rr0 <- pairs[[paste0(quantity, "_rr0")]]
  rr1 <- pairs[[paste0(quantity, "_rr1")]]
  delta <- rr1 - rr0
  data.frame(quantity = quantity, n_pairs = length(delta), median_rr0 = median(rr0),
    median_rr1 = median(rr1), median_delta_rr1_minus_rr0 = median(delta),
    mean_delta_rr1_minus_rr0 = mean(delta), minimum_delta_rr1_minus_rr0 = min(delta),
    maximum_delta_rr1_minus_rr0 = max(delta), n_positive = sum(delta > 0),
    n_negative = sum(delta < 0), n_zero = sum(delta == 0))
}))
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(pairs, file.path(result_dir, "paired-quantities.csv"), row.names = FALSE)
utils::write.csv(series, file.path(result_dir, "paired-timeseries.csv"), row.names = FALSE)
utils::write.csv(summary, file.path(result_dir, "summary.csv"), row.names = FALSE)

teal <- "#007C83"
orange <- "#D55E00"
draw_annual <- function() {
  par(mfrow = c(6, 5), mar = c(1.9, 2.5, 1.8, 0.5), oma = c(3.6, 3.2, 5.0, 0.5),
      mgp = c(1.4, 0.45, 0), tcl = -0.25, cex = 0.78, family = "sans")
  ymax <- max(1.05, max(series$depletion_rr0, series$depletion_rr1) * 1.02)
  for (i in seq_len(nrow(pairs))) {
    p <- pairs[i, ]
    d <- series[series$anchor_ensemble_id == p$anchor_ensemble_id, ]
    plot(d$year, d$depletion_rr0, type = "n", xlim = c(1952, 2024), ylim = c(0, ymax),
         axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
    abline(h = seq(0.2, 1, 0.2), col = "#E8E8E8", lwd = 0.6)
    axis(1, at = c(1960, 1980, 2000, 2020), cex.axis = 0.95)
    axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), las = 1, cex.axis = 0.95)
    box(col = "#888888", lwd = 0.6)
    lines(d$year, d$depletion_rr0, col = teal, lwd = 1.6)
    lines(d$year, d$depletion_rr1, col = orange, lty = 2, lwd = 1.6)
    title(sprintf("%s | M=%.4f | K=%.2f", sub("ensemble-", "", p$anchor_ensemble_id),
                  p$m_age40_quarterly, p$tag_mixing_k_cutoff), cex.main = 1.0, line = 0.6)
  }
  mtext("Year", side = 1, outer = TRUE, line = 1.0, cex = 0.95)
  mtext("Annual SB / same-year SB_F=0", side = 2, outer = TRUE, line = 1.4, cex = 0.95)
  mtext("Annual depletion in 30 exact BET pairs", side = 3, outer = TRUE,
        line = 3.0, cex = 1.2, font = 2)
  mtext("Ordered by quarterly M at age 40; all 60 fits have MGC <= 1e-4", side = 3,
        outer = TRUE, line = 1.6, cex = 0.9)
  mtext("RR0: solid teal     RR1: dashed orange", side = 3, outer = TRUE, line = 0.3, cex = 0.9)
  mtext("Central estimates for 30/34 completed reruns; Hessian uncertainty is not shown.",
        side = 1, outer = TRUE, line = 2.3, cex = 0.8)
}

draw_deltas <- function() {
  layout(matrix(1:4, ncol = 1), heights = c(0.85, 3.7, 3.7, 0.6))
  par(oma = rep(0, 4), mgp = c(3.0, 0.8, 0), tcl = -0.25,
      cex = 0.95, family = "sans")
  k <- sort(unique(design$tag_mixing_k_cutoff))
  colors <- stats::setNames(grDevices::hcl.colors(length(k), "viridis"), as.character(k))
  point_colors <- colors[as.character(pairs$tag_mixing_k_cutoff)]
  ids <- sub("ensemble-", "", pairs$anchor_ensemble_id)
  par(mar = c(0, 0, 0.4, 0))
  plot.new()
  text(0.5, 0.73, "Changes in central estimates for the 30 exact BET pairs", cex = 1.2, font = 2)
  legend(x = 0.5, y = 0.22, legend = sprintf("K=%.2f", k),
         pch = 21, pt.bg = colors, col = "#222222", horiz = TRUE,
         xjust = 0.5, yjust = 0.5, bty = "n", cex = 0.85, x.intersp = 0.7)
  quantities <- c("sb_recent_sb0", "f_recent_fmsy")
  titles <- c("Recent SB / SB_F=0: 2021-2024 SB / 2014-2023 SB_F=0",
              "Recent F / F_MSY: reciprocal of the MSY F multiplier")
  for (i in seq_along(quantities)) {
    par(mar = c(3.8, 5.1, 2.0, 1.0))
    delta <- pairs[[paste0("delta_", quantities[i], "_rr1_minus_rr0")]]
    reversed <- if (i == 1L) delta < 0 else delta > 0
    yrange <- range(c(0, delta))
    padding <- diff(yrange) * 0.16
    plot(seq_len(nrow(pairs)), delta, type = "n", axes = FALSE,
         xlim = c(0.4, nrow(pairs) + 0.6), ylim = yrange + c(-padding, padding),
         xlab = "Pair ordered by M (quarterly, age 40)", ylab = "RR1 - RR0", xaxs = "i")
    abline(h = pretty(yrange), col = "#E8E8E8", lwd = 0.7)
    abline(h = 0, col = "#555555", lwd = 1.0)
    abline(h = median(delta), col = "#555555", lty = 2, lwd = 1.0)
    axis(1, at = seq_len(nrow(pairs)), labels = ids, las = 2, cex.axis = 0.75)
    axis(2, las = 1)
    box(col = "#888888", lwd = 0.6)
    points(seq_len(nrow(pairs)), delta, pch = ifelse(reversed, 24, 21),
           bg = point_colors, col = "#222222", cex = 1.25, lwd = 0.8)
    title(titles[i], cex.main = 1.0)
    legend("topright", sprintf("Paired median: %+.5f", median(delta)),
           lty = 2, col = "#555555", bty = "n", cex = 0.85)
  }
  par(mar = rep(0, 4))
  plot.new()
  text(0.5, 0.72, sprintf(
    "Triangles: opposite direction to the majority (%d lower SB/SB_F=0; %d higher F/F_MSY).",
    sum(pairs$delta_sb_recent_sb0_rr1_minus_rr0 < 0),
    sum(pairs$delta_f_recent_fmsy_rr1_minus_rr0 > 0)), cex = 0.82)
  text(0.5, 0.25,
    "These 30/34 completed reruns have no known stock truth; Hessian uncertainty is not shown.",
    cex = 0.82)
}

render <- function(name, draw, width, height) {
  grDevices::pdf(file.path(figure_dir, paste0(name, ".pdf")), width = width,
                 height = height, useDingbats = FALSE, family = "sans")
  draw()
  grDevices::dev.off()
  # macOS Quartz avoids requiring optional XQuartz libraries. Other systems use
  # R's configured default bitmap device; all devices are in base R.
  bitmap_type <- if (Sys.info()[["sysname"]] == "Darwin") "quartz" else getOption("bitmapType")
  grDevices::png(file.path(figure_dir, paste0(name, ".png")), width = width,
                 height = height, units = "in", res = 180, type = bitmap_type)
  draw()
  grDevices::dev.off()
}
render("paired-annual-depletion", draw_annual, 12, 14)
render("paired-recent-deltas", draw_deltas, 11, 8.5)

cat(sprintf("Reconstructed %d exact pairs (%d annual rows); missing RR1 anchors: %s.\n",
            nrow(pairs), nrow(series), paste(failed, collapse = ", ")))
cat(sprintf("All 60 paired fits passed MGC <= 1e-4; RR0 max %.15g, RR1 max %.15g.\n",
            max(pairs$mgc_rr0), max(pairs$mgc_rr1)))
cat(sprintf("34 RR0 management/annual-series checks: max absolute error %.6g.\n",
            max(baseline_errors)))
cat(sprintf("Original 30-pair aggregate quantities: max absolute error %.6g.\n",
            max(original_errors)))
print(summary[, c("quantity", "median_delta_rr1_minus_rr0", "n_positive", "n_negative")],
      row.names = FALSE, digits = 12)
cat("Opposite depletion IDs:", paste(pairs$anchor_ensemble_id[
  pairs$delta_sb_recent_sb0_rr1_minus_rr0 < 0], collapse = ", "), "\n")
cat("Opposite F/FMSY IDs:", paste(pairs$anchor_ensemble_id[
  pairs$delta_f_recent_fmsy_rr1_minus_rr0 > 0], collapse = ", "), "\n")
cat("Runtime:", R.version.string, "|", Sys.info()[["sysname"]], Sys.info()[["machine"]], "\n")

unlink(saved_root, recursive = TRUE, force = TRUE)
