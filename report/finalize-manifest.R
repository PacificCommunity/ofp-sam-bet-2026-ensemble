options(stringsAsFactors = FALSE)

output_dir <- Sys.getenv("REPORT_OUTPUT_DIR", "results")
if (!dir.exists(output_dir)) stop("The report output directory does not exist.")

files <- list.files(output_dir, recursive = TRUE, full.names = FALSE)
files <- sort(files[
  files != "report-manifest.csv" &
    !grepl("[.]pre-rev1-root-owned$", files)
])
paths <- file.path(output_dir, files)
if (!length(files) || any(!file.exists(paths)) || any(file.info(paths)$isdir)) {
  stop("The report output set is incomplete.")
}

sha256 <- function(path) {
  output <- system2("sha256sum", path, stdout = TRUE, stderr = TRUE)
  if (!length(output)) stop("Could not hash report output: ", path)
  strsplit(output[[1L]], "[[:space:]]+")[[1L]][[1L]]
}

manifest <- data.frame(
  file = files,
  sha256 = vapply(paths, sha256, character(1L)),
  stringsAsFactors = FALSE
)
write.csv(manifest, file.path(output_dir, "report-manifest.csv"), row.names = FALSE)
cat("Finalized report manifest for ", nrow(manifest), " files.\n", sep = "")
