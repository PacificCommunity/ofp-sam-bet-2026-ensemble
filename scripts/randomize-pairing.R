#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_arg) != 1L) stop("Cannot locate repository root.", call. = FALSE)
repo <- normalizePath(file.path(dirname(sub("^--file=", "", script_arg)), ".."), mustWork = TRUE)
output <- file.path(repo, "design", "pairing-map.csv")
args <- commandArgs(trailingOnly = TRUE)
if (file.exists(output) && !identical(args, "--replace")) {
  stop("pairing-map.csv already exists; pass --replace to intentionally redraw it.", call. = FALSE)
}

n <- 100L
k <- rep(c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35),
         c(6L, 12L, 19L, 26L, 19L, 12L, 6L))
rr <- rep(c(0L, 1L), each = 50L)
effort <- rep(c(0.005, 0.010, 0.015, 0.020, 0.025), each = 20L)
tau <- rep(c(4.96, 5.14, 5.20), c(33L, 34L, 33L))
limit <- 0.10
attempt <- 0L

repeat {
  attempt <- attempt + 1L
  ranks <- replicate(6L, sample.int(n), simplify = FALSE)
  names(ranks) <- c("h", "mixing", "rr", "m", "effort", "tau")
  realised <- data.frame(
    h_rank = ranks$h,
    K = k[ranks$mixing],
    RR = rr[ranks$rr],
    M_rank = ranks$m,
    effort = effort[ranks$effort],
    tau = tau[ranks$tau]
  )
  correlations <- cor(realised, method = "spearman")
  maximum <- max(abs(correlations[upper.tri(correlations)]))
  if (maximum <= limit) break
}

pairing <- data.frame(
  ensemble_id = sprintf("ensemble-%03d", seq_len(n)),
  steepness_rank = ranks$h,
  tag_mixing_rank = ranks$mixing,
  tag_reporting_rank = ranks$rr,
  natural_mortality_rank = ranks$m,
  effort_creep_rank = ranks$effort,
  tag_overdispersion_rank = ranks$tau,
  stringsAsFactors = FALSE
)
stopifnot(all(vapply(pairing[-1L], function(x) identical(sort(x), seq_len(n)), logical(1))))
write.csv(pairing, output, row.names = FALSE, quote = TRUE)
cat(sprintf(
  "Wrote one-time unseeded randomized pairing after %d attempt(s); max |Spearman rho| = %.4f.\n",
  attempt, maximum
))
