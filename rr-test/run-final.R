#!/usr/bin/env Rscript
# Offline, base-R access to the preserved paired BET native files.
# Rscript run-final.R ACTION [CASE] [absolute-fresh-OUT]

options(stringsAsFactors = FALSE)
fail <- function(...) stop(..., call. = FALSE)
require_true <- function(x, ...) if (!isTRUE(x)) fail(...)
native_engine_sha <- "f5bc1e232a86e51f920bce7271d8e0930d0b160e4d18dc46de44078f0fa24cd0"
native_inputs <- c("bet.frq", "bet.ini", "bet.tag", "bet.age_length", "bet.reg_scaling", "mfcl.cfg")
native_files <- c(native_inputs, "final.par", "doitall.sh", "model-inputs/S0.90-F2.conf",
                  "selectivity-models/F2.csv", "reference.rep", "original-INPUTS.sha256")
pair_numbers <- c(5, 6, 7, 8, 14, 27, 29, 32, 33, 34, 35, 40, 42, 43, 44, 45,
                  46, 49, 52, 62, 63, 65, 71, 74, 78, 86, 89, 92, 94, 95)
expected_models <- sort(c(sprintf("ensemble-%03d", pair_numbers),
                         sprintf("rrtest-%03d-rr1", pair_numbers)))
controls <- c("1 1 1", "1 50 -4", "1 121 0", "1 186 0", "1 187 0",
              "1 188 0", "1 189 0", "1 190 1", "1 246 1")
dimension_values <- c("Number of time periods" = 292, "Year 1" = 1952,
                      "Number of regions" = 5, "Number of species" = 1,
                      "Number of age classes" = 40, "Number of recruitments per year" = 4)
central_labels <- c(names(dimension_values), "Adult biomass",
                    "Adult biomass in absence of fishing", "Adult biomass at MSY",
                    "F multiplier at MSY")

exists_path <- function(path) {
  link <- Sys.readlink(path)
  file.exists(path) || dir.exists(path) || (!is.na(link) && nzchar(link))
}

regular_file <- function(path) {
  info <- file.info(path)
  link <- Sys.readlink(path)
  require_true(nrow(info) == 1L && !is.na(info$isdir) && !info$isdir && file_test("-f", path) &&
                 (is.na(link) || !nzchar(link)), "Expected a regular file: ", path)
  invisible(info)
}

directory <- function(path) {
  info <- file.info(path)
  link <- Sys.readlink(path)
  require_true(nrow(info) == 1L && !is.na(info$isdir) && info$isdir &&
                 (is.na(link) || !nzchar(link)), "Expected an ordinary directory: ", path)
  invisible(path)
}

checked_command <- function(program, args, ...) {
  found <- Sys.which(program)
  require_true(nzchar(found), "Required system command is unavailable: ", program)
  result <- system2(found, args = args, stdout = TRUE, stderr = TRUE, ...)
  status <- attr(result, "status")
  require_true(is.null(status) || status == 0L,
               program, " failed: ", paste(result, collapse = "\n"))
  result
}

sha256 <- function(paths) {
  invisible(lapply(paths, regular_file))
  program <- if (nzchar(Sys.which("sha256sum"))) "sha256sum" else "shasum"
  args <- c(if (program == "shasum") c("-a", "256"), "--", shQuote(paths))
  result <- checked_command(program, args)
  require_true(length(result) == length(paths) &&
                 all(grepl("^[0-9a-f]{64}[[:space:]]", result)), "Invalid SHA-256 output.")
  substring(result, 1L, 64L)
}

check_single_links <- function(paths) {
  args <- if (Sys.info()[["sysname"]] == "Darwin") c("-f", "%l") else c("-c", "%h")
  counts <- checked_command("stat", c(args, shQuote(paths)))
  require_true(length(counts) == length(paths) && all(counts == "1"),
               "Native files must be ordinary files with one link.")
}

read_csv <- function(path) {
  regular_file(path)
  utils::read.csv(path, check.names = FALSE, colClasses = "character", na.strings = NULL)
}

read_sections <- function(path) {
  regular_file(path)
  lines <- readLines(path, warn = FALSE)
  headers <- which(grepl("^[[:space:]]*#", lines))
  labels <- trimws(sub("^[[:space:]]*#[[:space:]]*", "", lines[headers]))
  function(label, rows = 1L, columns = 1L) {
    index <- which(labels == label)
    require_true(length(index) == 1L, path, ": expected one section: ", label)
    first <- headers[index] + 1L
    last <- if (index < length(headers)) headers[index + 1L] - 1L else length(lines)
    require_true(first <= last, path, ": empty section: ", label)
    text <- trimws(lines[seq.int(first, last)])
    text <- text[nzchar(text)]
    tokens <- strsplit(text, "[[:space:]]+")
    require_true(length(text) == rows && all(lengths(tokens) == columns),
                 path, ": invalid row/column count for ", label)
    values <- suppressWarnings(as.numeric(unlist(tokens, use.names = FALSE)))
    require_true(length(values) == rows * columns && all(is.finite(values)),
                 path, ": non-finite or non-numeric values for ", label)
    matrix(values, nrow = rows, ncol = columns, byrow = TRUE)
  }
}

par_values <- function(path) {
  sections <- read_sections(path)
  values <- c(objective = as.numeric(sections("Objective function value")),
              parameters = as.numeric(sections("The number of parameters")))
  require_true(values[["objective"]] > 0 && values[["parameters"]] == 1997,
               path, ": expected a positive objective and 1,997 parameters.")
  values
}

rep_values <- function(path) {
  sections <- read_sections(path)
  result <- lapply(central_labels, function(label) {
    shape <- if (label %in% c("Adult biomass", "Adult biomass in absence of fishing")) c(292L, 5L) else c(1L, 1L)
    value <- sections(label, shape[1L], shape[2L])
    require_true(all(value > 0), path, ": non-positive values for ", label)
    if (label %in% names(dimension_values))
      require_true(as.numeric(value) == dimension_values[[label]], path, ": unexpected ", label)
    value
  })
  names(result) <- central_labels
  result
}

compare_rep <- function(actual, original) {
  observed <- rep_values(actual)
  reference <- rep_values(original)
  differences <- vapply(central_labels, function(label) {
    a <- observed[[label]]
    b <- reference[[label]]
    require_true(all(abs(a - b) <= 1e-10 * pmax(1, abs(b))),
                 "Central REP differs from the original: ", label)
    max(abs(a - b))
  }, numeric(1))
  list(values = observed, max_abs_diff = max(differences))
}

native_log <- function(path) {
  regular_file(path)
  lines <- readLines(path, warn = FALSE)
  control_lines <- grep("^[[:space:]]*optfile\\.cpp[[:space:]]+", lines, value = TRUE)
  limits <- 0L
  for (line in control_lines) {
    fields <- strsplit(trimws(sub("^[[:space:]]*optfile\\.cpp[[:space:]]+", "", line)), "[[:space:]]+")[[1L]]
    require_true(length(fields) >= 3L && all(grepl("^[-+]?[0-9]+$", fields[1:3])),
                 "Malformed native optfile.cpp control record.")
    values <- as.numeric(fields[1:3])
    if (identical(values[1:2], c(1, 1))) {
      require_true(identical(values, c(1, 1, 1)), "Native function-evaluation ceiling differs from one.")
      limits <- limits + 1L
    }
  }
  candidates <- lines[grepl("variables;", lines, fixed = TRUE) &
                        grepl("function[[:space:]]+evaluation", lines)]
  pattern <- "^[[:space:]]*([0-9]+)[[:space:]]+variables;[[:space:]]+iteration[[:space:]]+([0-9]+);[[:space:]]+function[[:space:]]+evaluation[[:space:]]+([0-9]+)[[:space:]]*$"
  for (line in candidates) {
    match <- regmatches(line, regexec(pattern, line))[[1L]]
    require_true(length(match) == 4L && identical(as.numeric(match[2:4]), c(1997, 0, 0)),
                 "Native counter differs from 1,997 variables, iteration 0, function evaluation 0.")
  }
  require_true(limits > 0L && length(candidates) > 0L,
               "Native ceiling-one control or actual zero-counter records are absent.")
  objectives <- grep("^[[:space:]]*Total func[[:space:]]+[^[:space:]]+[[:space:]]*$", lines, value = TRUE)
  require_true(length(objectives) > 0L, "Native Total func record is absent.")
  objective <- suppressWarnings(as.numeric(trimws(sub("^[[:space:]]*Total func[[:space:]]+", "", objectives[1L]))))
  require_true(is.finite(objective), "Native logged objective is non-finite.")
  list(objective = objective, control_records = limits, counter_records = length(candidates))
}

check_contents <- function(root) {
  path <- file.path(root, "CONTENTS.sha256")
  regular_file(path)
  lines <- readLines(path, warn = FALSE)
  require_true(length(lines) > 0L && all(grepl("^[0-9a-f]{64}  [A-Za-z0-9._-]+$", lines)),
               "Invalid CONTENTS.sha256 inventory.")
  names <- substring(lines, 67L)
  expected <- c("Makefile", "run-final.R", "run-final", "README.md", "mfclo64",
                "models.csv", "FILES.csv", "native.tar.xz", "source-manifest.json")
  require_true(!anyDuplicated(names) && setequal(names, expected), "Top-level checksum inventory differs.")
  require_true(identical(sha256(file.path(root, names)), substring(lines, 1L, 64L)),
               "Top-level content checksum differs.")
  check_single_links(file.path(root, c(names, "CONTENTS.sha256")))
  require_true(sha256(file.path(root, "mfclo64")) == native_engine_sha, "Native engine checksum differs.")
}

read_inventory <- function(root) {
  check_contents(root)
  models <- read_csv(file.path(root, "models.csv"))
  columns <- c("model", "arm", "anchor", "paired_model", "objective", "parameters",
               "final_par_sha256", "reference_rep_sha256", "source_whole_rep_sha256")
  require_true(all(columns %in% names(models)) && nrow(models) == 60L &&
                 !anyDuplicated(models$model) && setequal(models$model, expected_models),
               "Expected exactly the 30 paired RR0/RR1 saved-PAR cases.")
  require_true(all(models$paired_model %in% models$model) &&
                 all(models$parameters == "1997") &&
                 all(is.finite(suppressWarnings(as.numeric(models$objective)))) &&
                 all(suppressWarnings(as.numeric(models$objective)) > 0), "Invalid saved model metadata.")
  anchors <- sub("^rrtest-([0-9]{3})-rr1$", "ensemble-\\1", models$model)
  is_anchor <- startsWith(models$model, "ensemble-")
  paired <- ifelse(is_anchor, sub("^ensemble-([0-9]{3})$", "rrtest-\\1-rr1", models$model), anchors)
  require_true(all(models$anchor == anchors) && all(models$paired_model == paired) &&
                 all(models$arm == ifelse(is_anchor, "RR0", "RR1")), "Saved model pairing/arm metadata differs.")
  for (column in c("final_par_sha256", "reference_rep_sha256", "source_whole_rep_sha256"))
    require_true(all(grepl("^[0-9a-f]{64}$", models[[column]])), "Invalid model checksum: ", column)
  files <- read_csv(file.path(root, "FILES.csv"))
  require_true(identical(names(files), c("path", "bytes", "sha256", "mode")), "FILES.csv columns differ.")
  expected <- unlist(lapply(expected_models, function(model) paste("models", model, native_files, sep = "/")), use.names = FALSE)
  require_true(nrow(files) == length(expected) && !anyDuplicated(files$path) && setequal(files$path, expected),
               "Native file inventory differs from the exact 720 model files.")
  require_true(all(grepl("^[0-9]+$", files$bytes)) && all(as.numeric(files$bytes) > 0) &&
                 all(grepl("^[0-9a-f]{64}$", files$sha256)) && all(grepl("^[0-9]+$", files$mode)) &&
                 all(as.numeric(files$mode) >= 0 & as.numeric(files$mode) <= 511), "Invalid native file metadata.")
  for (i in seq_len(nrow(models))) {
    row <- models[i, ]
    for (name in c("final.par", "reference.rep")) {
      index <- match(paste("models", row$model, name, sep = "/"), files$path)
      column <- if (name == "final.par") "final_par_sha256" else "reference_rep_sha256"
      require_true(files$sha256[index] == row[[column]], "Model/file checksum binding differs: ", row$model)
    }
  }
  list(models = models, files = files)
}

check_files <- function(base, files) {
  paths <- file.path(base, files$path)
  invisible(lapply(paths, regular_file))
  # Check every parent as well, including model/config directories.
  parents <- unique(dirname(paths))
  invisible(lapply(parents, directory))
  check_single_links(paths)
  info <- file.info(paths)
  require_true(identical(as.numeric(info$size), as.numeric(files$bytes)), "Native file size differs.")
  require_true(identical(as.integer(info$mode), as.integer(files$mode)), "Native file mode differs.")
  require_true(identical(sha256(paths), files$sha256), "Native file checksum differs.")
  invisible(TRUE)
}

check_models_tree <- function(root, inventory) {
  folder <- file.path(root, "models")
  directory(folder)
  require_true(setequal(list.files(folder, all.files = TRUE, no.. = TRUE), expected_models),
               "Extracted model directory set differs.")
  direct <- c(native_files[!grepl("/", native_files, fixed = TRUE)], "model-inputs", "selectivity-models")
  for (model in expected_models) {
    model_dir <- file.path(folder, model)
    directory(model_dir)
    require_true(setequal(list.files(model_dir, all.files = TRUE, no.. = TRUE), direct),
                 "Extracted model contains missing or extra files: ", model)
    for (relative in c("model-inputs/S0.90-F2.conf", "selectivity-models/F2.csv")) {
      nested <- file.path(model_dir, dirname(relative))
      directory(nested)
      require_true(identical(list.files(nested, all.files = TRUE, no.. = TRUE), basename(relative)),
                   "Extracted configuration directory differs: ", model)
    }
  }
  check_files(root, inventory$files)
  invisible(TRUE)
}

unpack_models <- function(root, inventory) {
  target <- file.path(root, "models")
  if (exists_path(target)) {
    check_models_tree(root, inventory)
    return(invisible(target))
  }
  archive <- file.path(root, "native.tar.xz")
  members <- checked_command("tar", c("-tf", shQuote(archive)))
  require_true(length(members) == nrow(inventory$files) && !anyDuplicated(members) &&
                 setequal(members, inventory$files$path), "Archive paths differ from FILES.csv.")
  types <- checked_command("tar", c("-tvf", shQuote(archive)))
  require_true(length(types) == length(members) && all(startsWith(types, "-")),
               "Archive contains a non-regular file or a link.")
  staging <- tempfile(".unpack-", tmpdir = root)
  require_true(dir.create(staging, mode = "0700"), "Cannot create archive staging directory.")
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  checked_command("tar", c("-xpf", shQuote(archive), "-C", shQuote(staging)))
  check_models_tree(staging, inventory)
  require_true(!exists_path(target) && file.rename(file.path(staging, "models"), target),
               "Models destination appeared or atomic publication failed.")
  invisible(target)
}

check_model <- function(root, row, files) {
  model_dir <- file.path(root, "models", row$model)
  original <- file.path(model_dir, "original-INPUTS.sha256")
  lines <- readLines(original, warn = FALSE)
  require_true(length(lines) > 0L && all(grepl("^[0-9a-f]{64}[[:space:]]+[^[:space:]]+$", lines)),
               "Invalid original input checksum manifest: ", row$model)
  names <- sub("^[0-9a-f]{64}[[:space:]]+", "", lines)
  sums <- substring(lines, 1L, 64L)
  require_true(!anyDuplicated(names) && all(native_inputs %in% names), "Original input manifest is incomplete: ", row$model)
  for (input in native_inputs) {
    index <- match(paste("models", row$model, input, sep = "/"), files$path)
    require_true(files$sha256[index] == sums[match(input, names)], "Original input binding differs: ", row$model, "/", input)
  }
  saved <- par_values(file.path(model_dir, "final.par"))
  require_true(abs(saved[["objective"]] - as.numeric(row$objective)) <= 1e-6,
               "Saved objective differs from model metadata: ", row$model)
  rep_values(file.path(model_dir, "reference.rep"))
  invisible(saved)
}

fresh_output <- function(root, raw) {
  root <- normalizePath(root, mustWork = TRUE)
  require_true(length(raw) == 1L && nzchar(raw) && startsWith(raw, "/"), "Set OUT to an absolute, fresh directory.")
  require_true(!exists_path(raw), "OUT already exists; choose a fresh directory.")
  parent <- normalizePath(dirname(raw), mustWork = TRUE)
  directory(parent)
  name <- basename(raw)
  require_true(nzchar(name) && !name %in% c(".", ".."), "Invalid OUT directory name.")
  output <- file.path(parent, name)
  require_true(output != root && !startsWith(output, paste0(root, "/")), "OUT must be outside the standalone package.")
  # When a package sits in a checkout, allow repository outputs/ only.
  candidate <- root
  while (candidate != dirname(candidate)) {
    if (exists_path(file.path(candidate, ".git"))) {
      require_true(output != candidate && (!startsWith(output, paste0(candidate, "/")) ||
                     startsWith(output, paste0(candidate, "/outputs/"))),
                   "OUT inside the checkout must be beneath outputs/.")
      break
    }
    candidate <- dirname(candidate)
  }
  output
}

run_repository_package <- function(script_dir, args) {
  repo <- normalizePath(dirname(script_dir), mustWork = TRUE)
  if (args[1L] %in% c("prepare", "rerun", "refit", "--prepare-only")) {
    output <- fresh_output(script_dir, args[3L])
    require_true(output != repo && (!startsWith(output, paste0(repo, "/")) ||
                   startsWith(output, paste0(repo, "/outputs/"))),
                 "OUT inside the checkout must be beneath outputs/.")
  }
  zip <- file.path(script_dir, "standalone.zip")
  regular_file(zip)
  temporary <- tempfile("bet-rr-standalone-")
  require_true(dir.create(temporary, mode = "0700"), "Cannot create temporary standalone directory.")
  on.exit(unlink(temporary, recursive = TRUE), add = TRUE)
  members <- utils::unzip(zip, list = TRUE)$Name
  prefix <- "bet-2026-rr-standalone/"
  names <- substring(members, nchar(prefix) + 1L)
  expected <- c("CONTENTS.sha256", "Makefile", "run-final.R", "run-final", "README.md", "mfclo64",
                "models.csv", "FILES.csv", "native.tar.xz", "source-manifest.json")
  require_true(length(names) == length(expected) && all(startsWith(members, prefix)) &&
                 !anyDuplicated(names) && setequal(names, expected), "Standalone ZIP file inventory differs.")
  utils::unzip(zip, exdir = temporary)
  runner <- file.path(temporary, "bet-2026-rr-standalone", "run-final.R")
  regular_file(runner)
  status <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                     c(shQuote(runner), shQuote(args))))
  require_true(status == 0L, "Standalone R command failed (exit ", status, ").")
  invisible(NULL)
}

prepare_model <- function(root, inventory, model, raw_out) {
  index <- match(model, inventory$models$model)
  require_true(!is.na(index), "Unknown saved case; failed fits have no saved PAR: ", model)
  output <- fresh_output(root, raw_out)
  unpack_models(root, inventory)
  row <- inventory$models[index, , drop = FALSE]
  selected <- inventory$files[startsWith(inventory$files$path, paste0("models/", model, "/")), , drop = FALSE]
  check_files(root, selected)
  check_model(root, row, inventory$files)
  require_true(dir.create(output, mode = "0700"), "Cannot create OUT.")
  # Failed preparation leaves its new working directory for inspection.
  for (i in seq_len(nrow(selected))) {
    relative <- substring(selected$path[i], nchar(paste0("models/", model, "/")) + 1L)
    target <- file.path(output, relative)
    if (!dir.exists(dirname(target))) require_true(dir.create(dirname(target), recursive = TRUE, mode = "0700"), "Cannot create native input directory.")
    require_true(file.copy(file.path(root, selected$path[i]), target, overwrite = FALSE,
                            copy.mode = TRUE, copy.date = TRUE), "Cannot copy native file: ", relative)
    require_true(Sys.chmod(target, mode = as.octmode(as.integer(selected$mode[i])), use_umask = FALSE), "Cannot preserve native mode: ", relative)
  }
  engine <- file.path(output, "mfclo64")
  require_true(file.copy(file.path(root, "mfclo64"), engine, overwrite = FALSE, copy.mode = TRUE), "Cannot copy native engine.")
  require_true(Sys.chmod(engine, mode = "0755", use_umask = FALSE), "Cannot make the copied native engine executable.")
  copied <- selected
  copied$path <- substring(copied$path, nchar(paste0("models/", model, "/")) + 1L)
  check_files(output, copied)
  require_true(sha256(engine) == native_engine_sha, "Copied native engine checksum differs.")
  list(output = output, row = row, files = copied)
}

require_linux <- function() {
  info <- Sys.info()
  require_true(info[["sysname"]] == "Linux" && tolower(info[["machine"]]) %in% c("x86_64", "amd64"),
               "Native execution requires Linux x86-64; list, unpack, verify and prepare work without execution.")
}

write_central <- function(path, model, values) {
  sb <- colMeans(matrix(rowSums(values[["Adult biomass"]]), nrow = 4L))
  sb0 <- colMeans(matrix(rowSums(values[["Adult biomass in absence of fishing"]]), nrow = 4L))
  years <- 1952:2024
  table <- data.frame(model = model, year = years, globalSB = sb, SBF0 = sb0,
                      depletion = sb / sb0)
  utils::write.csv(table, path, row.names = FALSE)
  recent <- mean(sb[years %in% 2021:2024])
  management <- data.frame(model = model,
                            SB_recent_SB0 = recent / mean(sb0[years %in% 2014:2023]),
                            SB_recent_SBMSY = recent / as.numeric(values[["Adult biomass at MSY"]]),
                            F_recent_FMSY = 1 / as.numeric(values[["F multiplier at MSY"]]))
  utils::write.csv(management, file.path(dirname(path), "management-quantities.csv"), row.names = FALSE)
}

evaluate_model <- function(root, prepared) {
  output <- prepared$output
  row <- prepared$row
  original <- file.path(root, "models", row$model, "final.par")
  final <- file.path(output, "final.par")
  require_true(file.copy(final, file.path(output, "input.par"), overwrite = FALSE, copy.mode = TRUE), "Cannot copy saved final PAR to input.par.")
  expected <- par_values(final)
  before <- sha256(c(original, final, file.path(output, "input.par")))
  require_true(all(before == row$final_par_sha256), "Saved/staged final PAR checksum differs.")
  control_file <- file.path(output, "evaluation-controls.txt")
  writeLines(controls, control_file, useBytes = TRUE)
  old <- setwd(output)
  on.exit(setwd(old), add = TRUE)
  status <- suppressWarnings(system2("./mfclo64", c("bet.frq", "input.par", "evaluated.par", "-file", "-"),
                                     stdin = "evaluation-controls.txt", stdout = "mfcl-evaluation.log", stderr = "mfcl-evaluation.log"))
  require_true(status %in% c(0L, 3L), "Native evaluation failed (exit ", status, "); inspect mfcl-evaluation.log.")
  check_files(output, prepared$files)
  require_true(identical(before, sha256(c(original, final, file.path(output, "input.par")))), "Saved or staged final PAR changed during evaluation.")
  observed <- par_values("evaluated.par")
  logged <- native_log("mfcl-evaluation.log")
  difference <- max(abs(c(expected[["objective"]], observed[["objective"]], logged$objective) - expected[["objective"]]))
  require_true(difference <= 1e-6, "Native objective differs from saved PAR by more than 1e-6.")
  rep <- compare_rep("plot-evaluated.par.rep", "reference.rep")
  receipt <- data.frame(model = row$model, objective_saved = expected[["objective"]],
                        objective_evaluated = observed[["objective"]], objective_logged = logged$objective,
                        objective_max_abs_diff = difference, parameters = observed[["parameters"]],
                        function_evaluation_ceiling = 1L, iteration = 0L, function_counter = 0L,
                        native_exit_code = status, central_rep_max_abs_diff = rep$max_abs_diff,
                        input_unchanged = TRUE)
  utils::write.csv(receipt, "evaluation-check.csv", row.names = FALSE)
  write_central("central-results.csv", row$model, rep$values)
  cat("Native outputs verified for ", row$model, "; objective difference ", format(difference),
      ", central REP maximum absolute difference ", format(rep$max_abs_diff), ".\n", sep = "")
}

refit_model <- function(prepared) {
  old <- setwd(prepared$output)
  on.exit(setwd(old), add = TRUE)
  environment <- c("MODEL_ID=S0.90-F2", "PROGRAM_PATH=./mfclo64", "BET_PHASE10_11_CONVERGENCE=-4")
  status <- suppressWarnings(system2("sh", "./doitall.sh", env = environment,
                                     stdout = "mfcl-refit.log", stderr = "mfcl-refit.log"))
  require_true(status == 0L, "Original refit script failed (exit ", status, "); inspect mfcl-refit.log.")
  check_files(prepared$output, prepared$files)
  regular_file("11.par")
  cat("Original refit script completed; fitted output: ", file.path(prepared$output, "11.par"), ".\n", sep = "")
}

run_final <- function(args = commandArgs(trailingOnly = TRUE)) {
  script_arg <- grep("^--file=", commandArgs(), value = TRUE)
  require_true(length(script_arg) == 1L, "Run this file with Rscript.")
  script <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
  root <- dirname(script)
  require_true(length(args) >= 1L, "Usage: Rscript run-final.R ACTION [CASE] [absolute-fresh-OUT]")
  action <- args[1L]
  aliases <- c("--list" = "list", "--verify" = "verify", "--prepare-only" = "prepare")
  if (action %in% names(aliases)) action <- aliases[[action]]
  if (action == "help") {
    cat("Rscript run-final.R list|unpack|verify\nRscript run-final.R prepare|rerun|refit CASE /absolute/fresh/OUT\n")
    return(invisible(NULL))
  }
  require_true(action %in% c("list", "unpack", "verify", "prepare", "rerun", "refit"), "Unknown action: ", action)
  require_true(length(args) == if (action %in% c("list", "unpack", "verify")) 1L else 3L,
               "Usage: Rscript run-final.R ACTION [CASE] [absolute-fresh-OUT]")
  if (!file.exists(file.path(root, "models.csv"))) {
    require_true(file.exists(file.path(root, "standalone.zip")), "Use run-final.R from the standalone package or its repository rr-test/ directory.")
    return(run_repository_package(root, args))
  }
  inventory <- read_inventory(root)
  if (action == "list") {
    cat(paste(sort(inventory$models$model), collapse = "\n"), "\n", sep = "")
  } else if (action == "unpack") {
    unpack_models(root, inventory)
    cat("Unpacked and verified exact native files for 60 saved cases.\n")
  } else if (action == "verify") {
    unpack_models(root, inventory)
    check_models_tree(root, inventory)
    for (i in seq_len(nrow(inventory$models))) check_model(root, inventory$models[i, , drop = FALSE], inventory$files)
    cat("Verified 60 saved cases, all original native inputs/PARs/references, and the offline engine.\n")
  } else {
    if (action %in% c("rerun", "refit")) require_linux()
    prepared <- prepare_model(root, inventory, args[2L], args[3L])
    if (action == "prepare") cat("Prepared exact native working copy: ", prepared$output, "\n", sep = "")
    else if (action == "rerun") evaluate_model(root, prepared)
    else refit_model(prepared)
  }
  invisible(NULL)
}

if (sys.nframe() == 0L) {
  tryCatch(run_final(), error = function(error) {
    cat(conditionMessage(error), "\n", file = stderr(), sep = "")
    quit(status = 1L)
  })
}
