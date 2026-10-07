#!/usr/bin/env Rscript
# Independent checks of preserved RR/ensemble files and native Linux evaluations.
options(stringsAsFactors = FALSE, warn = 2)
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")

engine_sha <- "f5bc1e232a86e51f920bce7271d8e0930d0b160e4d18dc46de44078f0fa24cd0"
pair_ids <- sprintf("%03d", c(5,6,7,8,14,27,29,32,33,34,35,40,42,43,44,45,
                              46,49,52,62,63,65,71,74,78,86,89,92,94,95))
rr_models <- sort(c(paste0("ensemble-", pair_ids), paste0("rrtest-", pair_ids, "-rr1")))
expected_models <- rr_models
family <- "rr"
archive_relative <- "rr-test/standalone.zip"
kit_prefix <- "bet-2026-rr-standalone"
input_ledger <- "original-INPUTS.sha256"
model_count <- 60L
file_count <- 720L
shard_count <- 3L
configure_family <- function(value, repo) {
  assert(value %in% c("rr", "ensemble"), "Choose family rr or ensemble")
  family <<- value
  if (value == "ensemble") {
    source <- file.path(repo, "data/ensemble/retained-final-par-manifest.csv")
    assert(sha256(source) == "655d5ea38648b28ba946ea7e7922d69ba8cc8fd775caba2202b87a9a887e6255",
           "Original retained-fit ledger differs")
    index <- read_csv(source)
    assert(nrow(index) == 80L && !anyDuplicated(index$ensemble_id), "Expected 80 retained fits")
    expected_models <<- sort(index$ensemble_id)
    archive_relative <<- "reproduce/standalone.zip"
    kit_prefix <<- "bet-2026-ensemble-standalone"
    input_ledger <<- "runtime-INPUTS.sha256"
    model_count <<- 80L; file_count <<- 960L; shard_count <<- 4L
  } else {
    expected_models <<- rr_models; archive_relative <<- "rr-test/standalone.zip"
    kit_prefix <<- "bet-2026-rr-standalone"; input_ledger <<- "original-INPUTS.sha256"
    model_count <<- 60L; file_count <<- 720L; shard_count <<- 3L
  }
}
dimensions <- c("Number of time periods" = 292, "Year 1" = 1952,
                "Number of regions" = 5, "Number of species" = 1,
                "Number of age classes" = 40, "Number of recruitments per year" = 4)
rep_fields <- c(names(dimensions), "Adult biomass", "Adult biomass in absence of fishing",
                "Adult biomass at MSY", "F multiplier at MSY")
kit_names <- c("Makefile", "run-final.R", "run-final", "README.md", "mfclo64",
               "models.csv", "FILES.csv", "CONTENTS.sha256", "native.tar.xz", "source-manifest.json")

assert <- function(ok, message) {
  if (length(ok) != 1L || is.na(ok) || !ok) stop(message, call. = FALSE)
}
safe_name <- function(x) {
  is.character(x) && length(x) > 0L && all(nzchar(x)) &&
    !any(grepl("[\\\\[:cntrl:]]|^/", x)) &&
    all(vapply(strsplit(x, "/", fixed = TRUE), function(p) all(!p %in% c("", ".", "..")), logical(1)))
}
regular <- function(path, bound = 1e9) {
  info <- file.info(path)
  assert(nrow(info) == 1L && !is.na(info$size) && !info$isdir &&
           identical(Sys.readlink(path), "") && info$size >= 0 && info$size <= bound,
         paste("Expected a bounded regular file:", path))
  info
}
command <- function(program, args, log = NULL, timeout = 120L) {
  if (is.null(log)) {
    out <- suppressWarnings(system2(program, vapply(args, shQuote, ""), stdout = TRUE,
                                    stderr = FALSE, timeout = timeout))
    status <- attr(out, "status")
    if (is.null(status)) status <- 0L
    assert(identical(as.integer(status), 0L), paste("Command failed:", program, paste(args, collapse = " ")))
    return(out)
  }
  assert(!file.exists(log), paste("Existing log refused:", log))
  status <- suppressWarnings(system2(program, vapply(args, shQuote, ""), stdout = log,
                                    stderr = log, timeout = timeout))
  assert(identical(as.integer(status), 0L), paste("Command failed; see", log))
  invisible(status)
}
sha256 <- function(path) {
  regular(path)
  out <- command("sha256sum", c("--", path))
  assert(length(out) == 1L && grepl("^[0-9a-f]{64}  ", out), paste("Invalid SHA256 result:", path))
  substr(out, 1L, 64L)
}
read_csv <- function(path, columns = NULL) {
  regular(path, 20e6)
  d <- read.csv(path, colClasses = "character", check.names = FALSE, na.strings = NULL)
  assert(!anyDuplicated(names(d)), paste("Duplicate CSV columns:", path))
  if (!is.null(columns)) assert(identical(names(d), columns), paste("CSV schema differs:", path))
  d
}
write_csv <- function(d, path) {
  assert(!file.exists(path), paste("Existing proof refused:", path))
  write.csv(d, path, row.names = FALSE, na = "")
}
number <- function(x, label, positive = FALSE) {
  assert(length(x) >= 1L && all(grepl("^[+-]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?$", x)),
         paste("Invalid numeric", label))
  n <- as.numeric(x)
  assert(all(is.finite(n)) && (!positive || all(n > 0)), paste("Nonfinite or invalid", label))
  n
}
scalar <- function(path, label) {
  lines <- trimws(readLines(path, warn = FALSE))
  where <- which(lines == paste("#", label))
  assert(length(where) == 1L && where < length(lines), paste("Missing/duplicate PAR scalar:", label))
  n <- number(lines[where + 1L], label)
  assert(length(n) == 1L, paste("Nonscalar PAR field:", label))
  n
}
rep_sections <- function(path) {
  regular(path, 40e6)
  result <- setNames(vector("list", length(rep_fields)), rep_fields)
  seen <- character(); current <- NULL
  for (line in readLines(path, warn = FALSE)) {
    if (grepl("^\\s*#", line)) {
      label <- trimws(sub("^\\s*#", "", line))
      current <- NULL
      if (label %in% rep_fields) {
        assert(!label %in% seen, paste("Duplicate REP section:", label))
        seen <- c(seen, label); current <- label
      }
    } else if (!is.null(current) && nzchar(trimws(line))) {
      result[[current]] <- c(result[[current]], list(strsplit(trimws(line), "[[:space:]]+")[[1L]]))
    }
  }
  assert(setequal(seen, rep_fields), paste("Incomplete central REP:", path))
  for (label in rep_fields) {
    rows <- result[[label]]
    matrix_field <- label %in% c("Adult biomass", "Adult biomass in absence of fishing")
    assert(length(rows) == if (matrix_field) 292L else 1L, paste("REP row count:", label))
    assert(all(lengths(rows) == if (matrix_field) 5L else 1L), paste("REP column count:", label))
    result[[label]] <- number(unlist(rows, use.names = FALSE), label, positive = TRUE)
    if (label %in% names(dimensions)) {
      assert(identical(result[[label]], unname(dimensions[label])), paste("REP model dimensions:", label))
    }
  }
  result
}
compare_rep <- function(actual, reference) {
  a <- rep_sections(actual); b <- rep_sections(reference)
  errors <- vapply(rep_fields, function(label) {
    difference <- abs(a[[label]] - b[[label]])
    assert(all(difference <= 1e-10 * pmax(1, abs(b[[label]]))), paste("Central REP changed:", label))
    max(difference / pmax(1, abs(b[[label]])))
  }, numeric(1))
  list(values = a, max_scaled_diff = max(errors),
       max_abs_diff = max(vapply(rep_fields, function(label) max(abs(a[[label]] - b[[label]])), numeric(1))))
}
check_central_csv <- function(path, model, values) {
  d <- read_csv(path, c("model", "year", "globalSB", "SBF0", "depletion"))
  assert(nrow(d) == 73L && all(d$model == model) && identical(number(d$year, "central year"), as.numeric(1952:2024)),
         "Annual central result identities differ")
  annual <- function(x) colMeans(matrix(rowSums(matrix(x, nrow = 292L, ncol = 5L, byrow = TRUE)), nrow = 4L))
  sb <- annual(values[["Adult biomass"]]); sb0 <- annual(values[["Adult biomass in absence of fishing"]])
  expected <- list(globalSB = sb, SBF0 = sb0, depletion = sb / sb0)
  for (field in names(expected)) {
    observed <- number(d[[field]], field, TRUE)
    assert(all(abs(observed - expected[[field]]) <= 1e-10 * pmax(1, abs(expected[[field]]))),
           paste("Annual central CSV differs from raw REP:", field))
  }
  invisible(d)
}
check_management_csv <- function(path, model, central, sbmsy, fmult) {
  d <- read_csv(path, c("model", "SB_recent_SB0", "SB_recent_SBMSY", "F_recent_FMSY"))
  assert(nrow(d) == 1L && d$model == model, "Management result identity differs")
  sb <- number(central$globalSB, "annual SB", TRUE)
  sb0 <- number(central$SBF0, "annual SBF0", TRUE)
  years <- number(central$year, "annual years")
  assert(identical(years, as.numeric(1952:2024)), "Incomplete annual management periods")
  recent <- mean(sb[years %in% 2021:2024])
  baseline <- mean(sb0[years %in% 2014:2023])
  expected <- c(SB_recent_SB0 = recent / baseline, SB_recent_SBMSY = recent / sbmsy, F_recent_FMSY = 1 / fmult)
  for (field in names(expected)) {
    observed <- number(d[[field]], field, TRUE)
    assert(abs(observed - expected[[field]]) <= 1e-10 * max(1, abs(expected[[field]])),
           paste("Management CSV differs from raw REP:", field))
  }
  invisible(d)
}
native_log <- function(path) {
  regular(path, 100e6)
  lines <- readLines(path, warn = FALSE)
  opt <- lines[grepl("^\\s*optfile\\.cpp\\s+", lines)]
  limits <- character()
  for (line in opt) {
    tokens <- strsplit(trimws(sub("^\\s*optfile\\.cpp\\s+", "", line)), "[[:space:]]+")[[1L]]
    assert(length(tokens) >= 3L && all(grepl("^[+-]?[0-9]+$", tokens[1:3])), "Malformed native control")
    if (identical(as.integer(tokens[1:2]), c(1L, 1L))) {
      assert(as.integer(tokens[3L]) == 1L, "Native evaluation ceiling differs from one")
      limits <- c(limits, trimws(line))
    }
  }
  counters <- lines[grepl("variables;.*function[[:space:]]+evaluation", lines)]
  assert(length(limits) > 0L && length(counters) > 0L, "Native ceiling/counter evidence missing")
  pattern <- "^\\s*([0-9]+)\\s+variables;\\s+iteration\\s+([0-9]+);\\s+function\\s+evaluation\\s+([0-9]+)\\s*$"
  for (line in counters) {
    match <- regmatches(line, regexec(pattern, line))[[1L]]
    assert(length(match) == 4L && identical(as.integer(match[2:4]), c(1997L, 0L, 0L)),
           "Native variables/iteration/function counter differs from 1997/0/0")
  }
  objectives <- lines[grepl("^\\s*Total func\\s+\\S+\\s*$", lines)]
  assert(length(objectives) > 0L, "Native objective absent")
  objective <- number(trimws(sub("^\\s*Total func\\s+", "", objectives[1L])), "logged objective", TRUE)
  list(objective = objective, lines = c(limits, trimws(counters), trimws(objectives[1L])),
       control_records = length(limits), counter_records = length(counters))
}
file_mode <- function(path) {
  mode <- as.integer(regular(path)$mode)
  assert(length(mode) == 1L && !is.na(mode) && mode >= 0L && mode <= 511L, "Invalid file mode")
  mode
}
verify_file <- function(path, row) {
  info <- regular(path)
  assert(info$size == number(row$bytes, "file size") && sha256(path) == row$sha256 &&
           file_mode(path) == number(row$mode, "file mode"), paste("Saved bytes/mode changed:", path))
}
case_bindings <- function(files, model) {
  prefix <- paste0("models/", model, "/")
  d <- files[startsWith(files$path, prefix), , drop = FALSE]
  d$relative <- substring(d$path, nchar(prefix) + 1L)
  required <- c("final.par", "bet.frq", "bet.ini", "bet.tag", "bet.age_length", "bet.reg_scaling",
                "mfcl.cfg", "doitall.sh", "reference.rep", input_ledger,
                "model-inputs/S0.90-F2.conf", "selectivity-models/F2.csv")
  assert(nrow(d) == 12L && setequal(required, d$relative),
         paste("Incomplete saved case:", model))
  d
}
check_copy <- function(output, bindings, row, kit) {
  assert(dir.exists(output) && identical(Sys.readlink(output), ""), "Case output directory missing or linked")
  for (i in seq_len(nrow(bindings))) verify_file(file.path(output, bindings$relative[i]), bindings[i, ])
  assert(sha256(file.path(output, "mfclo64")) == engine_sha && file_mode(file.path(output, "mfclo64")) == 493L,
         "Staged engine bytes/mode changed")
  assert(sha256(file.path(output, "final.par")) == row$final_par_sha256 &&
           sha256(file.path(output, "reference.rep")) == row$reference_rep_sha256,
         "Model row PAR/reference binding differs")
  assert(sha256(file.path(kit, "mfclo64")) == engine_sha, "Kit engine changed")
  if (file.exists(file.path(output, "input.par"))) {
    assert(sha256(file.path(output, "input.par")) == row$final_par_sha256 &&
             file_mode(file.path(output, "input.par")) == number(bindings$mode[match("final.par", bindings$relative)], "input PAR mode"),
           "Evaluation input PAR bytes/mode changed")
  }
  lines <- readLines(file.path(output, input_ledger), warn = FALSE)
  assert(length(lines) > 0L && all(grepl("^[0-9a-f]{64}[[:space:]]+[^[:space:]]+$", lines)),
         "Invalid original input checksum ledger")
  names <- sub("^[0-9a-f]{64}[[:space:]]+", "", lines)
  inputs <- c("bet.frq", "bet.ini", "bet.tag", "bet.age_length", "bet.reg_scaling", "mfcl.cfg")
  assert(!anyDuplicated(names) && all(inputs %in% names), "Incomplete original six-input ledger")
  for (input in inputs) {
    assert(substr(lines[match(input, names)], 1L, 64L) == bindings$sha256[match(input, bindings$relative)],
           paste("Original input binding differs:", input))
  }
  # An engine entry here describes source provenance; FILES/engine_sha bind the selected runtime.
}
output_inventory <- function(output, bindings) {
  names <- list.files(output, recursive = TRUE, all.files = TRUE, include.dirs = TRUE, no.. = TRUE)
  assert(safe_name(names), "Unsafe generated path")
  paths <- file.path(output, names)
  assert(all(Sys.readlink(paths) == ""), "Linked generated path refused")
  names <- names[!file.info(paths)$isdir]
  excluded <- c(bindings$relative, "mfclo64", "input.par", "mfcl-evaluation.log",
                "evaluation-controls.txt", "evaluation-check.csv", "central-results.csv", "management-quantities.csv")
  names <- sort(setdiff(names, excluded))
  assert(all(c("evaluated.par", "plot-evaluated.par.rep") %in% names), "Required native outputs missing")
  result <- data.frame(path = names, bytes = vapply(file.path(output, names), function(p) regular(p)$size, numeric(1)),
                       sha256 = vapply(file.path(output, names), sha256, ""))
  assert(all(result$bytes[result$path %in% c("evaluated.par", "plot-evaluated.par.rep")] > 0),
         "Required native outputs empty")
  result
}
outside_repo <- function(path, repo, existing = FALSE) {
  assert(length(path) == 1L && startsWith(path, "/"), "An absolute external directory is required")
  parent <- normalizePath(if (existing) path else dirname(path), mustWork = TRUE)
  target <- if (existing) parent else file.path(parent, basename(path))
  assert(target != repo && !startsWith(target, paste0(repo, "/")), "Generated files must be outside repository")
  assert(!grepl("[\r\n]", target), "Invalid generated directory")
  target
}
fresh_directory <- function(path) {
  link <- Sys.readlink(path)
  assert(!file.exists(path) && !dir.exists(path) && (is.na(link) || !nzchar(link)), paste("Existing directory refused:", path))
  assert(dir.create(path, mode = "0700"), paste("Cannot create", path))
  path
}
verify_kit <- function(kit) {
  contents <- readLines(file.path(kit, "CONTENTS.sha256"), warn = FALSE)
  assert(length(contents) == length(kit_names) - 1L && all(grepl("^[0-9a-f]{64}  [^/]+$", contents)),
         "Invalid top-level checksum ledger")
  names <- substring(contents, 67L)
  assert(!anyDuplicated(names) && setequal(names, setdiff(kit_names, "CONTENTS.sha256")), "Incomplete top-level ledger")
  for (i in seq_along(names)) assert(sha256(file.path(kit, names[i])) == substr(contents[i], 1L, 64L),
                                   paste("Kit file changed:", names[i]))
  assert(sha256(file.path(kit, "mfclo64")) == engine_sha && file_mode(file.path(kit, "mfclo64")) == 493L,
         "Expected engine bytes or executable mode differ")
  invisible(contents)
}
check_models <- function(models) {
  assert(identical(names(models), c("model", "arm", "anchor", "paired_model", "objective", "parameters",
                                    "final_par_sha256", "reference_rep_sha256", "source_whole_rep_sha256")),
         "Model index schema differs")
  assert(nrow(models) == model_count && !anyDuplicated(models$model) && identical(sort(models$model), expected_models),
         "Expected exact saved-case roster")
  if (family == "rr") {
  rr0 <- startsWith(models$model, "ensemble-")
  ids <- ifelse(rr0, sub("^ensemble-", "", models$model), sub("^rrtest-([0-9]{3})-rr1$", "\\1", models$model))
  assert(identical(models$arm, ifelse(rr0, "RR0", "RR1")) &&
           identical(models$anchor, paste0("ensemble-", ids)) &&
           identical(models$paired_model, ifelse(rr0, paste0("rrtest-", ids, "-rr1"), paste0("ensemble-", ids))),
         "Pair/arm/anchor mapping differs")
  } else {
    assert(all(models$arm == "") && all(models$paired_model == "") && identical(models$anchor, models$model),
           "Original ensemble fits must remain unpaired")
  }
  assert(all(number(models$parameters, "parameter count") == 1997) &&
           all(number(models$objective, "saved objective", TRUE) > 0) &&
           all(grepl("^[0-9a-f]{64}$", unlist(models[c("final_par_sha256", "reference_rep_sha256", "source_whole_rep_sha256")]))),
         "Model scalar/hash binding differs")
  invisible(models)
}
load_kit <- function(repo, root, zip_sha) {
  archive <- file.path(repo, archive_relative)
  assert(sha256(archive) == zip_sha, "Pinned standalone ZIP differs")
  listing <- utils::unzip(archive, list = TRUE)
  names <- paste0(kit_prefix, "/", kit_names)
  assert(nrow(listing) == length(names) && !anyDuplicated(listing$Name) && setequal(listing$Name, names) &&
           all(listing$Length > 0 & listing$Length <= 50e6), "ZIP inventory differs")
  command("unzip", c("-q", archive, "-d", root))
  kit <- file.path(root, kit_prefix)
  assert(identical(Sys.readlink(kit), ""), "Linked kit root refused")
  verify_kit(kit)
  models <- read_csv(file.path(kit, "models.csv"), c("model", "arm", "anchor", "paired_model", "objective",
                     "parameters", "final_par_sha256", "reference_rep_sha256", "source_whole_rep_sha256"))
  check_models(models)
  files <- read_csv(file.path(kit, "FILES.csv"), c("path", "bytes", "sha256", "mode"))
  assert(nrow(files) == file_count && safe_name(files$path) && !anyDuplicated(files$path) &&
           all(grepl("^[0-9a-f]{64}$", files$sha256)), "Invalid native file ledger")
  assert(all(grepl("^[0-9]+$", files$bytes)) && all(number(files$bytes, "saved file size") > 0) &&
           all(grepl("^[0-9]+$", files$mode)) && all(number(files$mode, "saved file mode") >= 0 & number(files$mode, "saved file mode") <= 511),
         "Invalid native file size/mode")
  tar_names <- command("tar", c("-tJf", file.path(kit, "native.tar.xz")))
  tar_types <- command("tar", c("-tvJf", file.path(kit, "native.tar.xz")))
  assert(length(tar_names) == file_count && !anyDuplicated(tar_names) && safe_name(tar_names) &&
           setequal(tar_names, files$path) && length(tar_types) == file_count && all(startsWith(tar_types, "-")),
         "Native archive must contain only the exact regular file roster")
  for (model in expected_models) case_bindings(files, model)
  command("make", c("-s", "--no-print-directory", "-C", kit, "verify"))
  listed <- command("make", c("-s", "--no-print-directory", "-C", kit, "list"))
  assert(identical(listed, expected_models), "Make list differs from exact case roster")
  list(path = kit, models = models, files = files)
}

run_shard <- function(shard, repo, work_parent, proof, zip_sha, commit) {
  assert(Sys.info()[["sysname"]] == "Linux" && Sys.info()[["machine"]] == "x86_64", "Native CI requires Linux x86-64")
  root <- fresh_directory(tempfile(paste0("rr-native-shard-", shard, "-"), tmpdir = work_parent))
  kit <- load_kit(repo, root, zip_sha)
  expected <- expected_models[seq.int(shard * 20L + 1L, shard * 20L + 20L)]
  uid <- command("id", "-u"); gid <- command("id", "-g")
  assert(length(uid) == 1L && length(gid) == 1L && grepl("^[0-9]+$", uid) && grepl("^[0-9]+$", gid), "Invalid CI UID/GID")
  passed <- character()
  output <- NULL; model <- NULL
  on.exit(write_csv(data.frame(shard = shard, commit = commit, zip_sha256 = zip_sha,
                               engine_sha256 = engine_sha, status = if (length(passed) == 20L) "passed" else "failed",
                               expected_models = paste(expected, collapse = ";"), passed_models = paste(passed, collapse = ";")),
                     file.path(proof, "shard.csv")), add = TRUE)
  on.exit({
    if (length(passed) < 20L && !is.null(output) && dir.exists(output)) {
      for (name in c("mfcl-evaluation.log", "evaluation-check.csv", "central-results.csv", "management-quantities.csv", "evaluated.par")) {
        path <- file.path(output, name)
        if (file.exists(path)) {
          tryCatch({
            regular(path, if (name == "mfcl-evaluation.log") 100e6 else 40e6)
            target <- file.path(proof, paste0(model, ".failed-", name))
            if (!file.exists(target)) file.copy(path, target, overwrite = FALSE)
          }, error = function(e) message("Failure diagnostic unavailable: ", conditionMessage(e)))
        }
      }
    }
  }, add = TRUE)
  for (model in expected) {
    row <- kit$models[kit$models$model == model, , drop = FALSE]
    bindings <- case_bindings(kit$files, model)
    prepared <- file.path(root, paste0(model, "-prepared"))
    output <- file.path(root, paste0(model, "-evaluated"))
    command("Rscript", c(file.path(kit$path, "run-final.R"), "prepare", model, prepared),
            file.path(proof, paste0(model, ".prepare.log")))
    check_copy(prepared, bindings, row, kit$path)
    prepared_paths <- list.files(prepared, recursive = TRUE, all.files = TRUE, include.dirs = TRUE, no.. = TRUE)
    assert(all(Sys.readlink(file.path(prepared, prepared_paths)) == ""), "Linked prepared path")
    prepared_files <- prepared_paths[!file.info(file.path(prepared, prepared_paths))$isdir]
    assert(setequal(prepared_files, c(bindings$relative, "mfclo64")), "Preparation generated unexpected files")
    assert(!file.exists(file.path(prepared, "evaluated.par")) && !file.exists(file.path(prepared, "mfcl-evaluation.log")),
           "Prepare performed a native evaluation")
    reference_objective <- scalar(file.path(prepared, "final.par"), "Objective function value")
    assert(abs(reference_objective - number(row$objective, "model objective")) <= 1e-6 &&
             scalar(file.path(prepared, "final.par"), "The number of parameters") == 1997,
           "Saved PAR differs from model index")
    # Preparation is checked separately; rerun must create its own fresh directory.
    command("sudo", c("-n", "unshare", "--net", "--setgid", gid, "--setuid", uid, "--",
                        "make", "--no-print-directory", "-C", kit$path, "rerun", paste0("CASE=", model), paste0("OUT=", output)),
            file.path(proof, paste0(model, ".wrapper.log")), timeout = 180L)
    check_copy(output, bindings, row, kit$path)
    assert(file.exists(file.path(output, "input.par")), "Native input PAR absent")
    observed <- scalar(file.path(output, "evaluated.par"), "Objective function value")
    assert(scalar(file.path(output, "evaluated.par"), "The number of parameters") == 1997, "Evaluated parameters differ")
    native <- native_log(file.path(output, "mfcl-evaluation.log"))
    objective_diff <- max(abs(c(observed, native$objective) - reference_objective))
    assert(is.finite(objective_diff) && objective_diff <= 1e-6, "Native objective differs from saved model")
    difference <- compare_rep(file.path(output, "plot-evaluated.par.rep"), file.path(prepared, "reference.rep"))
    receipt <- read_csv(file.path(output, "evaluation-check.csv"), c("model", "objective_saved", "objective_evaluated",
                          "objective_logged", "objective_max_abs_diff", "parameters", "function_evaluation_ceiling",
                          "iteration", "function_counter", "native_exit_code", "central_rep_max_abs_diff", "input_unchanged"))
    assert(nrow(receipt) == 1L && receipt$model == model && receipt$input_unchanged == "TRUE" &&
             number(receipt$parameters, "receipt parameters") == 1997 && number(receipt$function_evaluation_ceiling, "receipt ceiling") == 1 &&
             number(receipt$iteration, "receipt iteration") == 0 && number(receipt$function_counter, "receipt counter") == 0 &&
             number(receipt$native_exit_code, "native exit") %in% c(0, 3), "Native controller receipt differs")
    values <- number(unlist(receipt[c("objective_saved", "objective_evaluated", "objective_logged")]), "receipt objective", TRUE)
    assert(max(abs(values - c(reference_objective, observed, native$objective))) <= 1e-6 &&
             abs(number(receipt$central_rep_max_abs_diff, "receipt central difference") - difference$max_abs_diff) <= 1e-10 * max(1, difference$max_abs_diff),
           "Controller receipt differs from independent arithmetic")
    central <- check_central_csv(file.path(output, "central-results.csv"), model, difference$values)
    sbmsy <- difference$values[["Adult biomass at MSY"]]
    fmult <- difference$values[["F multiplier at MSY"]]
    check_management_csv(file.path(output, "management-quantities.csv"), model, central, sbmsy, fmult)
    inventory <- output_inventory(output, bindings)
    write_csv(inventory, file.path(proof, paste0(model, ".output-files.csv")))
    writeLines(native$lines, file.path(proof, paste0(model, ".native-counter-lines.txt")))
    assert(file.copy(file.path(output, "mfcl-evaluation.log"), file.path(proof, paste0(model, ".native.log")), overwrite = FALSE),
           "Could not retain native log")
    for (name in c("evaluation-check.csv", "central-results.csv", "management-quantities.csv")) {
      assert(file.copy(file.path(output, name), file.path(proof, paste0(model, ".", name)), overwrite = FALSE),
             paste("Could not retain", name))
    }
    write_csv(data.frame(model = model, arm = row$arm, anchor = row$anchor, paired_model = row$paired_model,
                         commit = commit, zip_sha256 = zip_sha, engine_sha256 = engine_sha, status = "passed",
                         parameters = 1997, function_evaluation_ceiling = 1, iteration = 0, function_counter = 0,
                         native_exit_code = number(receipt$native_exit_code, "native exit"),
                         native_control_records = native$control_records, native_counter_records = native$counter_records,
                         saved_objective = reference_objective, evaluated_objective = observed, logged_objective = native$objective,
                         objective_max_abs_diff = objective_diff, central_max_scaled_diff = difference$max_scaled_diff,
                         central_max_abs_diff = difference$max_abs_diff, input_unchanged = TRUE,
                         adult_biomass_at_msy = sbmsy, f_multiplier_at_msy = fmult,
                         final_par_sha256 = row$final_par_sha256, reference_rep_sha256 = row$reference_rep_sha256,
                         source_whole_rep_sha256 = row$source_whole_rep_sha256,
                         native_log_sha256 = sha256(file.path(output, "mfcl-evaluation.log")),
                         evaluated_par_sha256 = sha256(file.path(output, "evaluated.par")),
                         evaluated_rep_sha256 = sha256(file.path(output, "plot-evaluated.par.rep")),
                         output_file_count = nrow(inventory)), file.path(proof, paste0(model, ".csv")))
    verify_kit(kit$path)
    assert(sha256(file.path(repo, archive_relative)) == zip_sha, "Repository ZIP changed")
    passed <- c(passed, model)
    # Only these case directories were created by this run beneath its fresh root.
    assert(dirname(output) == root && dirname(prepared) == root && all(Sys.readlink(c(output, prepared)) == ""),
           "Generated cleanup ownership differs")
    unlink(c(output, prepared), recursive = TRUE)
    assert(!dir.exists(output) && !dir.exists(prepared), "Generated case cleanup failed")
  }
  assert(identical(passed, expected), "Shard coverage differs")
}

aggregate <- function(directory, repo, proof, zip_sha, commit) {
  assert(Sys.getenv("NATIVE_MATRIX_RESULT") == "success", "Native matrix did not all pass")
  assert(sha256(file.path(repo, archive_relative)) == zip_sha, "Aggregate ZIP pin differs")
  con <- unz(file.path(repo, archive_relative), paste0(kit_prefix, "/models.csv"), open = "r")
  models_index <- tryCatch(read.csv(con, colClasses = "character", check.names = FALSE, na.strings = NULL),
                           finally = close(con))
  check_models(models_index)
  paths <- list.files(directory, pattern = "^shard\\.csv$", recursive = TRUE, full.names = TRUE)
  assert(length(paths) == shard_count, "Expected exact shard proofs")
  seen <- integer(); cases <- list()
  for (path in paths) {
    s <- read_csv(path)
    assert(nrow(s) == 1L && all(c("shard", "commit", "zip_sha256", "engine_sha256", "status", "expected_models", "passed_models") %in% names(s)),
           "Invalid shard proof")
    n <- number(s$shard, "shard")
    assert(n %in% seq.int(0L, shard_count - 1L) && !n %in% seen, "Duplicate/invalid shard")
    seen <- c(seen, n)
    models <- expected_models[seq.int(n * 20L + 1L, n * 20L + 20L)]
    assert(s$status == "passed" && s$commit == commit && s$zip_sha256 == zip_sha && s$engine_sha256 == engine_sha &&
             s$expected_models == paste(models, collapse = ";") && s$passed_models == s$expected_models,
           "Shard proof differs or is incomplete")
    case_files <- list.files(dirname(path), pattern = "\\.csv$", full.names = FALSE)
    assert(setequal(case_files, c("shard.csv", paste0(models, ".csv"), paste0(models, ".output-files.csv"),
                                 paste0(models, ".evaluation-check.csv"), paste0(models, ".central-results.csv"),
                                 paste0(models, ".management-quantities.csv"))),
           "Unexpected/extra per-case CSV proof")
    for (model in models) {
      row <- read_csv(file.path(dirname(path), paste0(model, ".csv")))
      assert(nrow(row) == 1L && row$model == model && row$status == "passed" && row$commit == commit &&
               row$zip_sha256 == zip_sha && row$engine_sha256 == engine_sha && row$input_unchanged == "TRUE",
             "Per-case proof identity differs")
      source <- models_index[models_index$model == model, , drop = FALSE]
      assert(row$arm == source$arm && row$anchor == source$anchor && row$paired_model == source$paired_model &&
               row$final_par_sha256 == source$final_par_sha256 && row$reference_rep_sha256 == source$reference_rep_sha256 &&
               row$source_whole_rep_sha256 == source$source_whole_rep_sha256 &&
               abs(number(row$saved_objective, "saved objective", TRUE) - number(source$objective, "indexed objective", TRUE)) <= 1e-6,
             "Per-case source binding differs from pinned ZIP")
      assert(number(row$parameters, "parameters") == 1997 && number(row$function_evaluation_ceiling, "ceiling") == 1 &&
               number(row$iteration, "iteration") == 0 && number(row$function_counter, "counter") == 0 &&
               number(row$native_exit_code, "native exit") %in% c(0, 3) &&
               number(row$native_control_records, "control records") >= 1 && number(row$native_counter_records, "counter records") >= 1,
             "Native zero-counter proof differs")
      values <- number(unlist(row[c("saved_objective", "evaluated_objective", "logged_objective")]), "objective", TRUE)
      assert(max(abs(values - values[1L])) <= 1e-6 && number(row$objective_max_abs_diff, "objective difference") <= 1e-6 &&
               number(row$objective_max_abs_diff, "objective difference") >= 0 &&
               number(row$central_max_scaled_diff, "central difference") >= 0 &&
               number(row$central_max_scaled_diff, "central difference") <= 1e-10 &&
               number(row$central_max_abs_diff, "absolute central difference") >= 0, "Numerical tolerance failed")
      log <- file.path(dirname(path), paste0(model, ".native.log"))
      assert(sha256(log) == row$native_log_sha256, "Native log proof hash differs")
      native <- native_log(log)
      assert(native$control_records == number(row$native_control_records, "controls") &&
               native$counter_records == number(row$native_counter_records, "counters") &&
               abs(native$objective - values[1L]) <= 1e-6 &&
               identical(readLines(file.path(dirname(path), paste0(model, ".native-counter-lines.txt")), warn = FALSE), native$lines),
             "Native raw evidence differs from proof")
      central <- read_csv(file.path(dirname(path), paste0(model, ".central-results.csv")), c("model", "year", "globalSB", "SBF0", "depletion"))
      assert(nrow(central) == 73L && all(central$model == model) && identical(number(central$year, "annual year"), as.numeric(1952:2024)),
             "Annual proof coverage differs")
      sb <- number(central$globalSB, "annual SB", TRUE); sb0 <- number(central$SBF0, "annual SBF0", TRUE)
      assert(all(abs(number(central$depletion, "annual depletion", TRUE) - sb / sb0) <= 1e-10 * pmax(1, abs(sb / sb0))),
             "Annual proof depletion differs")
      check_management_csv(file.path(dirname(path), paste0(model, ".management-quantities.csv")), model, central,
                           number(row$adult_biomass_at_msy, "SBMSY", TRUE), number(row$f_multiplier_at_msy, "F multiplier", TRUE))
      inventory <- read_csv(file.path(dirname(path), paste0(model, ".output-files.csv")), c("path", "bytes", "sha256"))
      assert(safe_name(inventory$path) && !anyDuplicated(inventory$path) &&
               all(grepl("^[0-9a-f]{64}$", inventory$sha256)) && all(number(inventory$bytes, "output bytes") >= 0) &&
               nrow(inventory) == number(row$output_file_count, "output count"), "Invalid output inventory")
      for (name in c("evaluated.par", "plot-evaluated.par.rep")) {
        x <- inventory[inventory$path == name, , drop = FALSE]
        expected_sha <- if (name == "evaluated.par") row$evaluated_par_sha256 else row$evaluated_rep_sha256
        assert(nrow(x) == 1L && number(x$bytes, "required output size") > 0 && x$sha256 == expected_sha,
               "Required native output inventory differs")
      }
      cases[[length(cases) + 1L]] <- row
    }
  }
  result <- do.call(rbind, cases)
  assert(nrow(result) == model_count && !anyDuplicated(result$model) && identical(sort(result$model), expected_models),
         "Aggregate must cover the exact saved-case roster")
  result <- result[order(result$model), , drop = FALSE]
  write_csv(result, file.path(proof, "coverage.csv"))
  write_csv(data.frame(status = "passed", count = model_count, commit = commit, zip_sha256 = zip_sha, engine_sha256 = engine_sha,
                       parameters = 1997, function_evaluation_ceiling = 1, iteration = 0, function_counter = 0,
                       max_objective_abs_diff = max(number(result$objective_max_abs_diff, "objective differences")),
                       max_central_scaled_diff = max(number(result$central_max_scaled_diff, "central differences"))),
            file.path(proof, paste0("all-", model_count, ".csv")))
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  assert(length(args) %% 2L == 0L && length(args) >= 4L, "Use --shard 0/1/2 --work-parent ABS --proof-dir ABS, or --aggregate ABS --proof-dir ABS")
  keys <- args[seq.int(1L, length(args), 2L)]; values <- args[seq.int(2L, length(args), 2L)]
  assert(!anyDuplicated(keys) && all(keys %in% c("--shard", "--work-parent", "--proof-dir", "--aggregate", "--family")), "Invalid arguments")
  opts <- setNames(values, keys)
  assert("--proof-dir" %in% keys && xor("--shard" %in% keys, "--aggregate" %in% keys), "Choose shard or aggregate")
  script <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())])
  assert(length(script) == 1L, "Cannot identify this CI script")
  repo <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
  configure_family(if ("--family" %in% keys) opts[["--family"]] else "rr", repo)
  zip_sha <- Sys.getenv(if (family == "rr") "RR_STANDALONE_ZIP_SHA256" else "ENSEMBLE_STANDALONE_ZIP_SHA256")
  commit <- Sys.getenv("GITHUB_SHA")
  assert(grepl("^[0-9a-f]{64}$", zip_sha), "Set the workflow's pinned standalone ZIP SHA256")
  assert(grepl("^[0-9a-f]{40}$", commit) && identical(command("git", c("-C", repo, "rev-parse", "HEAD")), commit), "GITHUB_SHA must match checkout")
  proof <- fresh_directory(outside_repo(opts[["--proof-dir"]], repo))
  if ("--aggregate" %in% keys) {
    assert(!"--work-parent" %in% keys, "Aggregate does not take work-parent")
    aggregate(outside_repo(opts[["--aggregate"]], repo, TRUE), repo, proof, zip_sha, commit)
  } else {
    assert("--work-parent" %in% keys && grepl("^[0-9]$", opts[["--shard"]]) && as.integer(opts[["--shard"]]) < shard_count, "Choose a valid shard and existing work-parent")
    run_shard(as.integer(opts[["--shard"]]), repo, outside_repo(opts[["--work-parent"]], repo, TRUE), proof, zip_sha, commit)
  }
  cat(family, "saved-file and native checks passed.\n")
}
if (sys.nframe() == 0L) {
  tryCatch(main(), error = function(e) { message(conditionMessage(e)); quit(status = 1L) })
}
