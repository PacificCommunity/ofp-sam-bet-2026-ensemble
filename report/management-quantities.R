rolling_recent_depletion <- function(
    data, group_columns, target_years = NULL,
    spawning_column = "spawning_potential",
    nofishing_column = "spawning_potential_nofish",
    recent_window = 4L, nofishing_window = 10L) {
  required <- c(group_columns, "year", spawning_column, nofishing_column)
  absent <- setdiff(required, names(data))
  if (length(absent)) {
    stop("Missing rolling-depletion columns: ", paste(absent, collapse = ", "))
  }
  if (anyDuplicated(data[c(group_columns, "year")])) {
    stop("Rolling-depletion input contains duplicate group-year rows.")
  }

  key <- interaction(data[group_columns], drop = TRUE, lex.order = TRUE)
  pieces <- split(data, key)
  result <- lapply(pieces, function(value) {
    value <- value[order(value$year), , drop = FALSE]
    available_years <- as.integer(value$year)
    years <- if (is.null(target_years)) {
      available_years
    } else {
      intersect(as.integer(target_years), available_years)
    }
    spawning <- stats::setNames(value[[spawning_column]], available_years)
    nofishing <- stats::setNames(value[[nofishing_column]], available_years)
    rows <- lapply(years, function(year) {
      spawning_years <- seq.int(year - recent_window + 1L, year)
      nofishing_years <- seq.int(year - nofishing_window, year - 1L)
      if (!all(spawning_years %in% available_years) ||
          !all(nofishing_years %in% available_years)) {
        return(NULL)
      }
      recent_spawning <- mean(spawning[as.character(spawning_years)])
      recent_nofishing <- mean(nofishing[as.character(nofishing_years)])
      if (!is.finite(recent_spawning) || !is.finite(recent_nofishing) ||
          recent_nofishing <= 0) {
        stop("Invalid biomass in rolling management depletion.")
      }
      out <- value[1L, group_columns, drop = FALSE]
      out$year <- year
      out$sb_recent <- recent_spawning
      out$sb_f0_recent <- recent_nofishing
      out$sb_recent_sb0 <- recent_spawning / recent_nofishing
      out
    })
    rows <- Filter(Negate(is.null), rows)
    if (!length(rows)) return(NULL)
    do.call(rbind, rows)
  })
  result <- Filter(Negate(is.null), result)
  if (!length(result)) stop("No complete rolling management-depletion windows were found.")
  out <- do.call(rbind, result)
  row.names(out) <- NULL
  out[do.call(order, out[c(group_columns, "year")]), , drop = FALSE]
}

build_projection_management <- function(series, projection) {
  historical_input <- series[c(
    "ensemble_id", "year", "spawning_potential", "spawning_potential_nofish"
  )]
  historical <- rolling_recent_depletion(
    historical_input, "ensemble_id", target_years = sort(unique(series$year))
  )

  simulation_keys <- unique(projection$annual_stock[c("ensemble_id", "simulation")])
  historical_for_projection <- merge(
    simulation_keys, historical_input, by = "ensemble_id", sort = FALSE
  )
  projected_input <- projection$annual_stock[c(
    "ensemble_id", "simulation", "year",
    "spawning_biomass_mt", "spawning_biomass_noeff_mt"
  )]
  names(projected_input)[names(projected_input) == "spawning_biomass_mt"] <-
    "spawning_potential"
  names(projected_input)[names(projected_input) == "spawning_biomass_noeff_mt"] <-
    "spawning_potential_nofish"
  projected_input$spawning_potential <- projected_input$spawning_potential / 1000
  projected_input$spawning_potential_nofish <-
    projected_input$spawning_potential_nofish / 1000
  combined <- rbind(
    historical_for_projection[names(projected_input)],
    projected_input
  )
  projected <- rolling_recent_depletion(
    combined, c("ensemble_id", "simulation"),
    target_years = projection$projection_years
  )

  target <- stats::aggregate(
    depletion ~ ensemble_id,
    data = series[series$year %in% 2012:2015, ],
    FUN = mean
  )
  names(target)[names(target) == "depletion"] <- "historical_target_depletion"
  projected <- merge(projected, target, by = "ensemble_id", sort = FALSE)
  projected$recent_historical_target_ratio <- with(
    projected, sb_recent_sb0 / historical_target_depletion
  )
  projected$below_lrp_020 <- projected$sb_recent_sb0 < 0.20
  projected$below_historical_objective <-
    projected$recent_historical_target_ratio < 1
  projected <- projected[order(
    projected$ensemble_id, projected$simulation, projected$year
  ), ]
  row.names(projected) <- NULL

  list(historical = historical, projected = projected)
}
