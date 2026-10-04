#!/usr/bin/env Rscript
# Restore one saved raw byte vector. Requires base R and Python 3 standard library.
# Selectors use slash-separated exact names (escape ~ as ~0 and / as ~1).
# Prefix names with n: when needed; i:N selects a one-based list index.

main <- function(args) {
  usage <- paste(
    "Usage: Rscript preservation/recover-native.R --rds FILE --rds-sha256 SHA256",
    "  --selector artifacts/files/par/bytes --compression gzip|none",
    "  --native-sha256 SHA256 --output FRESH_FILE",
    "Selectors: exact names, n:NAME, or i:N (one-based); ~0 means ~, ~1 means /.",
    "Only list traversal and raw byte payloads are accepted. No model is executed.",
    sep = "\n")
  if (identical(args, "--help")) { cat(usage, "\n"); return(invisible(NULL)) }
  keys <- c("--rds", "--rds-sha256", "--selector", "--compression",
            "--native-sha256", "--output")
  if (length(args) != 12L || anyDuplicated(args[seq(1, 12, 2)]) ||
      !setequal(args[seq(1, 12, 2)], keys)) stop(usage, call. = FALSE)
  opt <- setNames(as.list(args[seq(2, 12, 2)]), args[seq(1, 12, 2)])
  if (any(!nzchar(unlist(opt)))) stop("Empty arguments are not accepted.")
  for (k in c("--rds-sha256", "--native-sha256")) {
    if (!grepl("^[0-9a-fA-F]{64}$", opt[[k]])) stop("Expected a full SHA256.")
    opt[[k]] <- tolower(opt[[k]])
  }
  if (!opt[["--compression"]] %in% c("none", "gzip")) stop("Unsupported compression.")
  selector <- opt[["--selector"]]
  if (grepl("^/|/$|//", selector)) stop("Empty selector segments are not accepted.")
  segments <- strsplit(selector, "/", fixed = TRUE)[[1]]
  steps <- lapply(segments, function(s) {
    if (startsWith(s, "i:")) {
      n <- substring(s, 3L)
      if (!grepl("^[1-9][0-9]*$", n) || nchar(n) > 9L) stop("Invalid list index.")
      return(list(index = as.integer(n)))
    }
    if (startsWith(s, "n:")) s <- substring(s, 3L)
    if (!nzchar(s) || grepl("~([^01]|$)", s)) stop("Invalid named selector escape.")
    s <- gsub("~0", "~", gsub("~1", "/", s, fixed = TRUE), fixed = TRUE)
    list(name = s)
  })
  python <- unname(Sys.which("python3"))
  if (!nzchar(python)) stop("Python 3 is required for SHA256 and exclusive file writes.")
  work <- tempfile("native-recovery-", tmpdir = normalizePath(tempdir(), mustWork = TRUE))
  if (!dir.create(work, mode = "0700")) stop("Cannot create private temporary directory.")
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  checker <- file.path(work, "check.py")
  writeLines(c(
    "import hashlib, os, stat, sys",
    "def parent(path):",
    "    if not path or '\\x00' in path or '\\\\' in path or path.endswith('/'): raise ValueError('path')",
    "    parts = path.split('/')",
    "    if '..' in parts: raise ValueError('traversal')",
    "    if parts[-1] in ('', '.'): raise ValueError('filename')",
    "    parts = [p for p in parts if p not in ('', '.')]",
    "    if not parts: raise ValueError('path')",
    "    fd = os.open('/' if path.startswith('/') else '.', os.O_RDONLY | os.O_DIRECTORY)",
    "    try:",
    "        for p in parts[:-1]:",
    "            nxt = os.open(p, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW, dir_fd=fd)",
    "            os.close(fd); fd = nxt",
    "        return fd, parts[-1]",
    "    except BaseException: os.close(fd); raise",
    "def digest(fd):",
    "    os.lseek(fd, 0, os.SEEK_SET); h = hashlib.sha256(); n = 0",
    "    while True:",
    "        b = os.read(fd, 1048576)",
    "        if not b: break",
    "        h.update(b); n += len(b)",
    "    return n, h.hexdigest()",
    "mode, source, expected, target = sys.argv[1:]",
    "sp = tp = src = dst = None; made = False",
    "try:",
    "    sp, sn = parent(source); tp, tn = parent(target)",
    "    src = os.open(sn, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK, dir_fd=sp)",
    "    if not stat.S_ISREG(os.fstat(src).st_mode): raise ValueError('not regular')",
    "    before = os.fstat(src)",
    "    if mode == 'publish':",
    "        count, sha = digest(src)",
    "        if sha != expected: raise ValueError('native SHA256 mismatch')",
    "    elif mode != 'snapshot': raise ValueError('mode')",
    "    dst = os.open(tn, os.O_RDWR | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW, 0o600, dir_fd=tp)",
    "    made = True; created = os.fstat(dst); os.lseek(src, 0, os.SEEK_SET)",
    "    while True:",
    "        b = os.read(src, 1048576)",
    "        if not b: break",
    "        view = memoryview(b)",
    "        while view: view = view[os.write(dst, view):]",
    "    os.fsync(dst); count, sha = digest(dst); after = os.fstat(src)",
    "    if sha != expected: raise ValueError('SHA256 mismatch')",
    "    if (before.st_dev, before.st_ino, before.st_size, before.st_mtime_ns, before.st_ctime_ns) != (after.st_dev, after.st_ino, after.st_size, after.st_mtime_ns, after.st_ctime_ns): raise ValueError('source changed')",
    "    print(str(count) + ' ' + sha)",
    "except BaseException as e:",
    "    if made:",
    "        try:",
    "            current = os.stat(tn, dir_fd=tp, follow_symlinks=False)",
    "            if (current.st_dev, current.st_ino) == (created.st_dev, created.st_ino): os.unlink(tn, dir_fd=tp)",
    "        except OSError: pass",
    "    print('Recovery byte/file check failed: ' + type(e).__name__, file=sys.stderr)",
    "    sys.exit(1)",
    "finally:",
    "    for fd in (src, dst, sp, tp):",
    "        if fd is not None: os.close(fd)"
  ), checker, useBytes = TRUE)
  check <- function(mode, source, sha, target) {
    result <- suppressWarnings(system2(python, shQuote(c(checker, mode, source, sha, target)),
                                       stdout = TRUE, stderr = TRUE))
    if (!is.null(attr(result, "status")) || length(result) != 1L ||
        !grepl("^[0-9]+ [0-9a-f]{64}$", result)) stop("SHA256 or safe file check failed.")
    result
  }
  snapshot <- file.path(work, "source.rds")
  check("snapshot", opt[["--rds"]], opt[["--rds-sha256"]], snapshot)
  value <- readRDS(snapshot)
  for (step in steps) {
    if (typeof(value) != "list" || isS4(value)) stop("Selector requires an ordinary list.")
    if (!is.null(step$index)) {
      index <- step$index
      if (index > length(value)) stop("List index is out of range.")
    } else {
      index <- which(names(value) == step$name)
      if (length(index) != 1L) stop("Named selector is missing or duplicated.")
    }
    value <- .subset2(value, index)
  }
  if (!is.raw(value)) stop("Selected payload is not raw bytes; reconstruction is refused.")
  if (opt[["--compression"]] == "gzip") {
    value <- withCallingHandlers(memDecompress(value, type = "gzip"),
                                 warning = function(w) stop("Raw gzip/zlib decoding failed."))
  }
  if (!is.raw(value)) stop("Decoded payload is not raw bytes.")
  native <- file.path(work, "native.bytes")
  con <- file(native, "wb")
  tryCatch(writeBin(value, con), finally = close(con))
  verified <- check("publish", native, opt[["--native-sha256"]], opt[["--output"]])
  cat("Recovered exact native bytes:", verified, "\n")
}

tryCatch(main(commandArgs(trailingOnly = TRUE)), error = function(e) {
  message(conditionMessage(e)); quit(status = 1L)
})
