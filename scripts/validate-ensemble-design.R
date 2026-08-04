#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_arg) != 1L) stop("Cannot locate repository root.", call. = FALSE)
repo <- normalizePath(file.path(dirname(sub("^--file=", "", script_arg)), ".."), mustWork = TRUE)
design_dir <- file.path(repo, "design")

draws <- read.csv(file.path(design_dir, "model-draws.csv"), check.names = FALSE)
continuous <- read.csv(file.path(design_dir, "continuous-summary.csv"), check.names = FALSE)
discrete <- read.csv(file.path(design_dir, "discrete-summary.csv"), check.names = FALSE)
effort <- read.csv(file.path(design_dir, "effort-creep-sources.csv"), check.names = FALSE)
mixing_sources <- read.csv(file.path(design_dir, "mixing-sources.csv"), check.names = FALSE)
parameters <- read.csv(file.path(design_dir, "distribution-parameters.csv"), check.names = FALSE)
m_evidence <- read.csv(file.path(design_dir, "m-evidence.csv"), check.names = FALSE)
hc_amax <- read.csv(file.path(design_dir, "hamel-cope-amax-sensitivity.csv"),
                    check.names = FALSE)
script_text <- readLines(file.path(repo, "scripts", "create-ensemble-design.R"), warn = FALSE)
doitall_text <- readLines(file.path(repo, "model", "doitall.sh"), warn = FALSE)
model_input_text <- readLines(file.path(repo, "model", "model-inputs", "S0.90-F2.conf"), warn = FALSE)
runner_text <- readLines(file.path(repo, "scripts", "run-ensemble"), warn = FALSE)
registrar_text <- readLines(file.path(repo, "scripts", "register-kflow-task.py"), warn = FALSE)
kflow_text <- readLines(file.path(repo, "kflow.yaml"), warn = FALSE)
model_paths <- list.files(file.path(repo, "model"), recursive = TRUE, all.files = TRUE)
source(file.path(repo, "scripts", "ensemble-inputs.R"))
mixing_paths <- file.path(repo, "sources", "mixing", mixing_sources$tag_mixing_source_file)

stopifnot(
  nrow(draws) == 100L,
  length(unique(draws$ensemble_id)) == 100L,
  identical(draws$ensemble_id, sprintf("ensemble-%03d", seq_len(100L))),
  identical(as.integer(table(draws$tag_tau)), c(33L, 34L, 33L)),
  identical(as.numeric(names(table(draws$tag_tau))), c(1.2, 1.3, 1.4)),
  all(abs(draws$tag_tau - (1 + exp(draws$tau_fish_pars4))) < 1e-12),
  max(abs(cor(
    draws[c("steepness", "tag_mixing_k_cutoff", "tag_reporting_flag2",
            "m_age40_quarterly", "effort_creep_primary")],
    draws$tag_tau,
    method = "spearman"
  ))) < 0.05,
  identical(as.integer(table(draws$tag_mixing_k_cutoff)), c(6L, 12L, 19L, 26L, 19L, 12L, 6L)),
  identical(as.integer(table(draws$tag_reporting_flag2)), c(50L, 50L)),
  identical(as.integer(table(draws$effort_creep_primary)), rep(20L, 5L)),
  abs(mean(draws$steepness) - 0.87) < 0.001,
  abs(sd(draws$steepness) - 0.063) < 0.002,
  abs(min(draws$m_age40_quarterly) - 0.050) < 1e-12,
  abs(max(draws$m_age40_quarterly) - 0.165) < 1e-12,
  all(abs(draws$lorenzen_log_intercept - log(draws$m_age40_quarterly)) < 1e-12),
  nrow(continuous) == 2L,
  nrow(discrete) == 17L,
  nrow(effort) == 5L,
  nrow(mixing_sources) == 7L,
  all(mixing_sources$source_branch == "SC22-IP10-regionMean"),
  all(mixing_sources$source_commit == "efe3107c72774ee73b5e6dc45e44cf51f0fc20e8"),
  nrow(parameters) == 38L,
  parameters$value[parameters$parameter == "tag_overdispersion_multiplier"] == 14,
  nrow(m_evidence) == 5L,
  identical(hc_amax$amax_years, c(13L, 15L, 16L)),
  all(nchar(effort$source_sha256) == 64L),
  all(nchar(mixing_sources$source_sha256) == 64L),
  all(draws$tag_mixing_source_file %in% mixing_sources$tag_mixing_source_file),
  all(draws$zero_mixing_events == mixing_sources$zero_mixing_events[
    match(draws$tag_mixing_source_file, mixing_sources$tag_mixing_source_file)
  ]),
  all(draws$tag_reporting_zero_mixing_exclusions == ifelse(
    draws$tag_reporting_flag2 == 0L,
    draws$zero_mixing_events,
    0L
  )),
  length(unique(draws$model_label)) == 100L,
  all(draws$initialization == "Job 21641 ordinary makepar (no seed)"),
  all(draws$pairing_version == "mod101-v2"),
  !"design_seed" %in% names(draws),
  !any(grepl("set[.]seed|RNGkind|sample[.(]", script_text)),
  !dir.exists(file.path(repo, "model", "checkpoints")),
  !any(grepl("seed[-_]?23|checkpoints/", c(doitall_text, runner_text), ignore.case = TRUE)),
  !any(grepl("seed|checkpoint", model_paths, ignore.case = TRUE)),
  sum(grepl("^[[:space:]]*1[[:space:]]+111[[:space:]]+4([[:space:]]|$)", doitall_text)) == 1L,
  sum(grepl("^[[:space:]]*1[[:space:]]+305[[:space:]]+1([[:space:]]|$)", doitall_text)) == 1L,
  sum(grepl("^[[:space:]]*1[[:space:]]+306[[:space:]]+0([[:space:]]|$)", doitall_text)) == 1L,
  sum(grepl("^[[:space:]]*-999[[:space:]]+43[[:space:]]+0([[:space:]]|$)", doitall_text)) == 1L,
  sum(grepl("^[[:space:]]*-999[[:space:]]+44[[:space:]]+0([[:space:]]|$)", doitall_text)) == 1L,
  sum(model_input_text == "TAU=2.0") == 1L,
  sum(model_input_text == "TAU_FISH_PARS4=0") == 1L,
  sum(grepl('fixed_tau_fish_pars4=[$]TAU_FISH_PARS4', doitall_text)) == 1L,
  sum(grepl("^audit_tau_value_fixed 00[.]fixed[.]par ", doitall_text)) == 1L,
  sum(grepl("^audit_tau_fixed (0[1-9]|1[01])[.]par ", doitall_text)) == 11L,
  sum(grepl("^audit_steepness_fixed (00[.]fixed|0[1-9]|1[01])[.]par ", doitall_text)) == 12L,
  sum(grepl("^audit_natural_mortality_fixed (00[.]fixed|0[1-9]|1[01])[.]par ", doitall_text)) == 12L,
  sum(grepl("^audit_dm_concentration_fixed (00[.]fixed|0[1-9]|1[01])[.]par ", doitall_text)) == 12L,
  sum(grepl("^audit_selectivity_model (0[1-9]|1[01])[.]par ", doitall_text)) == 11L,
  sum(grepl("^MODEL_ID=S0[.]90-F2 PROGRAM_PATH=", runner_text)) == 1L,
  sum(kflow_text == "name: bet-2026-ensemble-tau-axis") == 1L,
  sum(grepl("title = f\"BET Diagnostic | {row['model_label']}\"", registrar_text, fixed = TRUE)) == 1L,
  !any(grepl("tau=2/F2|tau2/F2|Job-21641-S0[.]90-F2", c(kflow_text, registrar_text))),
  abs(m_evidence$central[1] - 0.0624) < 1e-12,
  abs(m_evidence$lower[1] - 0.0500) < 1e-12,
  abs(m_evidence$upper[1] - 0.0749) < 1e-12,
  abs(m_evidence$central[3] - 0.0900) < 1e-12,
  abs(m_evidence$secondary_central[3] - 0.094430080) < 1e-8,
  abs(m_evidence$lower[3] - 0.049019630) < 1e-8,
  abs(m_evidence$upper[3] - 0.165239924) < 1e-8,
  abs(m_evidence$central[4] - 0.080817108) < 1e-8,
  abs(m_evidence$lower[4] - 0.044018053) < 1e-8,
  abs(m_evidence$upper[4] - 0.148380143) < 1e-8,
  abs(m_evidence$central[5] - exp(-2.54930339768360)) < 1e-12,
  abs(m_evidence$secondary_central[5] - 0.0702) < 1e-12,
  abs(m_evidence$lower[5] - 0.0572276723066398) < 1e-12,
  abs(m_evidence$upper[5] - 0.120155781738336) < 1e-12,
  file.info(file.path(design_dir, "distributions.png"))$size > 10000,
  file.info(file.path(design_dir, "distributions.pdf"))$size > 10000
)

ensemble_source_hashes(repo)
invisible(lapply(mixing_paths, ensemble_validate_mixing_source))
stopifnot(all(vapply(
  mixing_paths,
  function(path) sum(ensemble_tag_matrix(path)[, 1L] == "0"),
  integer(1)
) == mixing_sources$zero_mixing_events))

cat("Validated 100 deterministic BET 2026 ensemble configurations.\n")
