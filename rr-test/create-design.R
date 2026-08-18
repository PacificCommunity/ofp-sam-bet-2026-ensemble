#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_arg) != 1L) stop("Cannot locate repository root.", call. = FALSE)
repo <- normalizePath(file.path(dirname(sub("^--file=", "", script_arg)), ".."), mustWork = TRUE)

source_design <- read.csv(
  file.path(repo, "design", "model-draws.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
retained <- read.csv(
  file.path(repo, "data", "ensemble", "retained-final-par-manifest.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

stopifnot(
  nrow(source_design) == 100L,
  length(unique(source_design$ensemble_id)) == 100L,
  nrow(retained) == 80L,
  length(unique(retained$ensemble_id)) == 80L,
  all(retained$ensemble_id %in% source_design$ensemble_id)
)

anchors <- source_design[
  source_design$ensemble_id %in% retained$ensemble_id &
    source_design$tag_reporting_flag2 == 0L,
  , drop = FALSE
]
anchors <- anchors[order(anchors$ensemble_id), , drop = FALSE]

stopifnot(
  nrow(anchors) == 34L,
  length(unique(anchors$ensemble_id)) == 34L,
  all(anchors$tag_reporting_flag2 == 0L),
  all(anchors$tag_reporting == "inclusion"),
  identical(anchors$ensemble_id, sort(anchors$ensemble_id))
)

test <- anchors
test$anchor_ensemble_id <- anchors$ensemble_id
test$ensemble_id <- sub("^ensemble-", "rrtest-", anchors$ensemble_id)
test$ensemble_id <- paste0(test$ensemble_id, "-rr1")
test$tag_reporting_flag2 <- 1L
test$tag_reporting <- "exclusion"
test$tag_reporting_zero_mixing_exclusions <- 0L
test$pairing_version <- "retained-rr0-paired-rerun-v1"
test$model_label <- sprintf(
  "RR paired test | anchor=%s | h=%.3f | tau=%.1f | K=%.2f | RR=exclude | M0=%.4f/qtr | creep=%.1f/%.2f%%",
  test$anchor_ensemble_id,
  test$steepness,
  test$tag_tau,
  test$tag_mixing_k_cutoff,
  test$m_age40_quarterly,
  100 * test$effort_creep_primary,
  100 * test$effort_creep_secondary
)

front <- c("ensemble_id", "anchor_ensemble_id")
test <- test[, c(front, setdiff(names(test), front)), drop = FALSE]

stopifnot(
  nrow(test) == 34L,
  length(unique(test$ensemble_id)) == 34L,
  all(test$tag_reporting_flag2 == 1L),
  all(test$tag_reporting == "exclusion"),
  all(test$tag_reporting_zero_mixing_exclusions == 0L)
)

write.csv(
  test,
  file.path(repo, "rr-test", "model-draws.csv"),
  row.names = FALSE,
  quote = TRUE
)

cat("Created 34 exact RR1 counterparts for the 34 retained RR0 anchors.\n")
