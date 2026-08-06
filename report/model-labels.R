sequential_model_map <- function(source_ids) {
  source_ids <- sort(unique(as.character(source_ids)))
  if (!length(source_ids) || any(!nzchar(source_ids))) {
    stop("Sequential model labels require non-empty source identifiers.")
  }
  data.frame(
    ensemble_id = sprintf("ensemble-%03d", seq_along(source_ids)),
    source_ensemble_id = source_ids,
    stringsAsFactors = FALSE
  )
}

display_model_ids <- function(source_ids, model_map) {
  positions <- match(as.character(source_ids), model_map$source_ensemble_id)
  if (anyNA(positions)) {
    stop("A retained source model is missing from the sequential label map.")
  }
  model_map$ensemble_id[positions]
}
