#!/usr/bin/env Rscript
# Restore/check exact saved Hessian archives using base R; never execute MFCL.
options(stringsAsFactors = FALSE)
require_h <- function(x, message) if (!isTRUE(x)) stop(message, call. = FALSE)
linked_h <- function(p) { x <- Sys.readlink(p); !is.na(x) && nzchar(x) }
regular_h <- function(p) {
  i <- file.info(p)
  require_h(nrow(i) == 1L && !is.na(i$isdir) && !i$isdir && file_test("-f", p) && !linked_h(p), paste("Expected a regular file:", p))
  i
}
command_h <- function(program, args) {
  executable <- Sys.which(program)
  require_h(nzchar(executable), paste("Required command unavailable:", program))
  out <- suppressWarnings(system2(executable, shQuote(args), stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  require_h(is.null(status) || status == 0L, paste(program, "failed:", paste(out, collapse = "\n")))
  out
}
sha_h <- function(p) {
  regular_h(p)
  tool <- if (nzchar(Sys.which("sha256sum"))) "sha256sum" else "shasum"
  out <- command_h(tool, c(if (tool == "shasum") c("-a", "256"), "--", p))
  require_h(length(out) == 1L && grepl("^[0-9a-f]{64}[[:space:]]", out), "Invalid SHA256 output")
  substr(out, 1L, 64L)
}
# A bounded JSON data parser. It evaluates no code and requires unique keys.
json_h <- function(path) {
  require_h(regular_h(path)$size <= 2 * 1024^2, "Hessian manifest exceeds 2 MiB")
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  chars <- strsplit(text, "", fixed = TRUE)[[1L]]; at <- 1L; size <- length(chars)
  skip <- function() { while (at <= size && chars[at] %in% c(" ", "\n", "\r", "\t")) at <<- at + 1L }
  string <- function() {
    require_h(at <= size && chars[at] == '"', "Expected JSON string")
    at <<- at + 1L; result <- character()
    repeat {
      require_h(at <= size, "Truncated JSON string")
      ch <- chars[at]; at <<- at + 1L
      if (ch == '"') return(paste(result, collapse = ""))
      require_h(utf8ToInt(ch) >= 32L, "JSON string contains a control character")
      if (ch == "\\") {
        require_h(at <= size, "Truncated JSON escape")
        escape <- chars[at]; at <<- at + 1L
        if (escape == "u") {
          require_h(at + 3L <= size, "Truncated JSON Unicode escape")
          digits <- paste(chars[at:(at + 3L)], collapse = "")
          require_h(grepl("^[0-9A-Fa-f]{4}$", digits), "Invalid JSON Unicode escape")
          code <- strtoi(digits, 16L); at <<- at + 4L
          require_h(!(code >= 55296L && code <= 57343L), "Surrogate Unicode is not supported in Hessian metadata")
          ch <- intToUtf8(code)
        } else {
          translations <- c('"' = '"', "\\" = "\\", "/" = "/", b = "\b", f = "\f", n = "\n", r = "\r", t = "\t")
          require_h(escape %in% names(translations), "Invalid JSON escape")
          ch <- translations[[escape]]
        }
      }
      result <- c(result, ch)
    }
  }
  value <- function(depth = 0L) {
    require_h(depth <= 16L, "Excessive JSON nesting"); skip()
    require_h(at <= size, "Truncated JSON value")
    ch <- chars[at]
    if (ch == '"') return(string())
    if (ch %in% c("{", "[")) {
      object <- ch == "{"; closing <- if (object) "}" else "]"
      at <<- at + 1L; skip(); out <- list()
      if (at <= size && chars[at] == closing) { at <<- at + 1L; return(out) }
      repeat {
        skip()
        if (object) {
          key <- string(); require_h(!key %in% names(out), "Duplicate JSON key"); skip()
          require_h(at <= size && chars[at] == ":", "Missing JSON colon"); at <<- at + 1L
          out[key] <- list(value(depth + 1L))
        } else out[length(out) + 1L] <- list(value(depth + 1L))
        skip(); require_h(at <= size, "Truncated JSON container")
        ch <- chars[at]; at <<- at + 1L
        if (ch == closing) return(out)
        require_h(ch == ",", "Missing JSON comma")
      }
    }
    start <- at
    while (at <= size && !chars[at] %in% c(" ", "\n", "\r", "\t", ",", "}", "]")) at <<- at + 1L
    token <- paste(chars[start:(at - 1L)], collapse = "")
    if (token %in% c("true", "false", "null")) return(switch(token, true = TRUE, false = FALSE, null = NULL))
    require_h(grepl("^-?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][+-]?[0-9]+)?$", token), "Invalid JSON number")
    number <- as.numeric(token)
    require_h(is.finite(number), "Nonfinite JSON number"); number
  }
  result <- value(); skip(); require_h(at > size, "Extra JSON data"); result
}
safe_h <- function(p) {
  is.character(p) && length(p) == 1L && nchar(p) > 0L && nchar(p) <= 240L &&
    grepl("^[A-Za-z0-9._/-]+$", p) && !startsWith(p, "/") &&
    all(!strsplit(p, "/", fixed = TRUE)[[1L]] %in% c("", ".", ".."))
}
integer_h <- function(n, maximum = 512 * 1024^2) is.numeric(n) && length(n) == 1L && is.finite(n) && n >= 0 && n <= maximum && n == floor(n)
read_manifest_h <- function(path) {
  data <- json_h(path)
  require_h(identical(sort(names(data)), sort(c("cases", "classification", "repository", "schema_version"))) &&
              identical(data$schema_version, 1) && identical(data$classification, "PUBLIC") &&
              grepl("^PacificCommunity/ofp-sam-bet-2026-[a-z][a-z0-9-]*$", data$repository), "Invalid Hessian manifest schema")
  cases <- data$cases
  require_h(length(cases) >= 1L && length(cases) <= 512L && !is.null(names(cases)) && !anyDuplicated(names(cases)), "Invalid Hessian case index")
  for (case in names(cases)) {
    require_h(grepl("^[a-z0-9][a-z0-9._-]{0,63}$", case), "Invalid Hessian case name")
    row <- cases[[case]]
    required <- c("model_id", "pdh_status", "hessian_sha256", "final_par_sha256", "archive", "members")
    require_h(all(required %in% names(row)) && all(names(row) %in% c(required, "report_id", "report_label", "hessian_header")), "Invalid Hessian case fields")
    archive <- row$archive
    require_h(all(c("bytes", "sha256") %in% names(archive)) && xor("url" %in% names(archive), "relative_path" %in% names(archive)) &&
                length(archive) == 3L && integer_h(archive$bytes) && archive$bytes > 0 && grepl("^[0-9a-f]{64}$", archive$sha256), "Invalid Hessian archive pin")
    if (!is.null(archive$url)) {
      prefix <- paste0("https://github.com/", data$repository, "/releases/download/")
      require_h(startsWith(archive$url, prefix) && grepl("^[A-Za-z0-9._-]+/[A-Za-z0-9._-]+\\.tar\\.gz$", substring(archive$url, nchar(prefix) + 1L)), "Invalid fixed Hessian release URL")
    } else require_h(safe_h(archive$relative_path) && endsWith(archive$relative_path, ".tar.gz"), "Invalid relative Hessian archive path")
    members <- row$members
    require_h(length(members) >= 2L && length(members) <= 64L && all(c("bet.hes", "final.par") %in% names(members)) &&
                !anyDuplicated(tolower(names(members))) && all(vapply(names(members), safe_h, logical(1))), "Invalid Hessian members")
    for (name in names(members)) {
      pin <- members[[name]]
      require_h(identical(sort(names(pin)), c("bytes", "sha256")) && integer_h(pin$bytes) && grepl("^[0-9a-f]{64}$", pin$sha256), "Invalid Hessian member pin")
      require_h(!any(startsWith(setdiff(tolower(names(members)), tolower(name)), paste0(tolower(name), "/"))), "Hessian member path collision")
    }
    require_h(sum(vapply(members, function(x) x$bytes, numeric(1))) <= 1024^3 && row$hessian_sha256 == members$bet.hes$sha256 && row$final_par_sha256 == members$final.par$sha256, "Hessian scientific/member pins differ")
    if (!is.null(row$hessian_header)) {
      header <- row$hessian_header
      require_h(identical(sort(names(header)), sort(c("bytes_hex", "n_parameter", "row_bounds"))) && grepl("^([0-9a-f]{2}){1,64}$", header$bytes_hex) &&
                  integer_h(header$n_parameter, 100000L) && header$n_parameter > 0 && length(header$row_bounds) == 2L && all(vapply(header$row_bounds, integer_h, logical(1), maximum = 100000L)), "Invalid Hessian header pin")
    }
  }
  index <- file.path(dirname(path), "hessian-index.csv")
  if (file.exists(index)) {
    regular_h(index); table <- utils::read.csv(index, colClasses = "character", check.names = FALSE)
    required <- c("model", "display_model", "report_label", "hessian_status", "archive_bytes", "archive_sha256", "final_par_sha256", "hessian_sha256", "url")
    require_h(identical(names(table), required) && nrow(table) == length(cases) && !anyDuplicated(table$model) && setequal(table$model, names(cases)), "Hessian CSV index differs")
    for (i in seq_len(nrow(table))) {
      row <- cases[[table$model[i]]]
      require_h(table$display_model[i] == row$report_id && table$report_label[i] == row$report_label && table$hessian_status[i] == row$pdh_status &&
                  as.numeric(table$archive_bytes[i]) == row$archive$bytes && table$archive_sha256[i] == row$archive$sha256 &&
                  table$final_par_sha256[i] == row$final_par_sha256 && table$hessian_sha256[i] == row$hessian_sha256 && table$url[i] == row$archive$url, "Hessian CSV/source binding differs")
    }
  }
  data
}
validate_archive_h <- function(archive, entry, scratch) {
  require_h(regular_h(archive)$size == entry$archive$bytes && sha_h(archive) == entry$archive$sha256, "Hessian archive bytes/hash differ")
  names <- command_h("tar", c("-tzf", archive)); types <- command_h("tar", c("-tvzf", archive))
  expected <- names(entry$members)
  require_h(length(names) == length(expected) && !anyDuplicated(names) && setequal(names, expected) &&
              length(types) == length(expected) && all(startsWith(types, "-")), "Hessian archive member roster/type differs")
  command_h("tar", c("-xzf", archive, "-C", scratch))
  for (name in expected) {
    p <- file.path(scratch, name); pin <- entry$members[[name]]
    require_h(regular_h(p)$size == pin$bytes && sha_h(p) == pin$sha256, paste("Hessian member bytes/hash differ:", name))
  }
  if (!is.null(entry$hessian_header)) {
    expected <- entry$hessian_header$bytes_hex
    con <- file(file.path(scratch, "bet.hes"), "rb"); on.exit(close(con), add = TRUE)
    actual <- paste(sprintf("%02x", as.integer(readBin(con, "raw", n = nchar(expected) / 2L))), collapse = "")
    require_h(actual == expected, "Original Hessian header bytes differ")
  }
}
main_h <- function(args = commandArgs(trailingOnly = TRUE)) {
  script <- commandArgs()[grepl("^--file=", commandArgs())]
  require_h(length(script) == 1L, "Run hessian.R with Rscript")
  here <- dirname(normalizePath(sub("^--file=", "", script), mustWork = TRUE))
  verify <- "--verify" %in% args; args <- args[args != "--verify"]
  require_h(length(args) %% 2L == 0L, "Use --verify, --case CASE --out ABS [--archive ABS], or --verify --case CASE --archive ABS")
  keys <- if (length(args)) args[seq.int(1L, length(args), 2L)] else character()
  require_h(!anyDuplicated(keys) && all(keys %in% c("--manifest", "--case", "--out", "--archive")), "Invalid Hessian arguments")
  opts <- if (length(args)) setNames(args[seq.int(2L, length(args), 2L)], keys) else character()
  manifest <- if ("--manifest" %in% keys) opts[["--manifest"]] else file.path(here, "hessians.json")
  manifest <- normalizePath(manifest, mustWork = TRUE); data <- read_manifest_h(manifest)
  if (verify && !any(c("--case", "--out", "--archive") %in% keys)) { cat("Manifest verified: ", length(data$cases), " cases; no model run.\n", sep = ""); return(invisible(NULL)) }
  require_h("--case" %in% keys && opts[["--case"]] %in% names(data$cases), "Choose a case listed in hessians.json")
  entry <- data$cases[[opts[["--case"]]]]
  if (verify) require_h("--archive" %in% keys && !"--out" %in% keys, "Archive verification requires --case and local --archive, without OUT")
  else {
    require_h("--out" %in% keys && startsWith(opts[["--out"]], "/"), "Set OUT to an absolute fresh external directory")
    raw <- opts[["--out"]]
    require_h(!file.exists(raw) && !dir.exists(raw) && !linked_h(raw), "OUT already exists; choose a new directory")
    parent <- normalizePath(dirname(raw), mustWork = TRUE)
    require_h(dir.exists(parent) && !linked_h(parent) && !basename(raw) %in% c("", ".", ".."), "Invalid OUT parent/name")
    output <- file.path(parent, basename(raw))
    roots <- unique(c(dirname(here), dirname(dirname(manifest))))
    require_h(!any(vapply(roots, function(root) output == root || startsWith(output, paste0(root, "/")) || startsWith(root, paste0(output, "/")), logical(1))), "OUT must be outside the repository and its ancestors")
  }
  scratch <- tempfile("bet-hessian-"); require_h(dir.create(scratch, mode = "0700"), "Cannot create Hessian scratch directory")
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  if ("--archive" %in% keys) regular_h(opts[["--archive"]])
  archive <- if ("--archive" %in% keys) normalizePath(opts[["--archive"]], mustWork = TRUE) else if (!is.null(entry$archive$relative_path)) file.path(dirname(manifest), entry$archive$relative_path) else file.path(scratch, "archive.tar.gz")
  if (!"--archive" %in% keys && is.null(entry$archive$relative_path)) {
    command_h("curl", c("--fail", "--location", "--silent", "--show-error", "--proto", "=https", "--proto-redir", "=https", "--max-time", "600", "--max-filesize", as.character(entry$archive$bytes), "--output", archive, entry$archive$url))
  }
  extracted <- file.path(scratch, "members"); require_h(dir.create(extracted, mode = "0700"), "Cannot create Hessian extraction directory")
  validate_archive_h(archive, entry, extracted)
  if (!verify) {
    require_h(!file.exists(output) && !dir.exists(output) && !linked_h(output) && dir.create(output, mode = "0700"), "Cannot reserve fresh OUT")
    for (name in names(entry$members)) {
      target <- file.path(output, name)
      if (!dir.exists(dirname(target))) require_h(dir.create(dirname(target), recursive = TRUE, mode = "0700"), "Cannot create Hessian member directory")
      require_h(file.copy(file.path(extracted, name), target, overwrite = FALSE), "Cannot copy Hessian member")
      pin <- entry$members[[name]]
      require_h(regular_h(target)$size == pin$bytes && sha_h(target) == pin$sha256, "Copied Hessian member differs")
    }
  }
  cat(if (verify) "Saved Hessian archive verified; no model run.\n" else paste0("Restored exact original Hessian files to ", output, "; no model run.\n"))
}
if (sys.nframe() == 0L) tryCatch(main_h(), error = function(e) { cat(conditionMessage(e), "\n", file = stderr()); quit(status = 1L) })
