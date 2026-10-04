# Reuse the frozen ensemble preparation; select the paired design explicitly.
source(file.path(repo, "scripts", "ensemble-inputs.R"))
ensemble_load_row <- function(repo, model_id) {
  design_relative <- Sys.getenv("ENSEMBLE_DESIGN_FILE", "design/model-draws.csv")
  design_path <- if (grepl("^/", design_relative)) design_relative else file.path(repo, design_relative)
  design <- read.csv(design_path, stringsAsFactors = FALSE, check.names = FALSE)
  row <- design[design$ensemble_id == model_id, , drop = FALSE]
  if (nrow(row) != 1L) stop("Unknown ensemble model in ", design_relative, ": ", model_id, call. = FALSE)
  row
}
