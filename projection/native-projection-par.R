# Standalone native-MFCL projection PAR extension helpers.
#
# These functions preserve the fitted operating-model state while extending
# only time-dependent storage to the projection horizon supplied by -makepar.
# They are kept in this repository so a checksum-locked projection payload can
# be reproduced without downloading mfclkit or rebuilding the fitted models.

projection_overlay_array <- function(fitted, template, label) {
  fitted_dim <- dim(fitted)
  template_dim <- dim(template)
  if (is.null(fitted_dim) || is.null(template_dim) ||
      length(fitted_dim) != length(template_dim) || any(fitted_dim > template_dim)) {
    stop(label, " has incompatible fitted and projection-template dimensions.")
  }
  fitted_names <- dimnames(fitted)
  template_names <- dimnames(template)
  indices <- lapply(seq_along(fitted_dim), function(i) {
    from <- if (length(fitted_names) >= i) fitted_names[[i]] else NULL
    into <- if (length(template_names) >= i) template_names[[i]] else NULL
    if (!is.null(from) && !is.null(into) && length(from) == fitted_dim[[i]] &&
        !anyDuplicated(from) && !anyDuplicated(into) && all(from %in% into)) {
      match(from, into)
    } else {
      seq_len(fitted_dim[[i]])
    }
  })
  do.call(`[<-`, c(list(template), indices, list(value = fitted)))
}

projection_extend_value <- function(fitted, template, label) {
  if (is.null(fitted)) return(template)
  if (is.null(template)) stop(label, " is absent from the projection template.")
  if (!is.null(dim(fitted)) || !is.null(dim(template))) {
    return(projection_overlay_array(fitted, template, label))
  }
  if (length(fitted) > length(template)) {
    stop(label, " is longer than its projection template.")
  }
  out <- template
  if (length(fitted)) {
    fitted_names <- names(fitted)
    template_names <- names(template)
    if (!is.null(fitted_names) && !is.null(template_names) &&
        all(nzchar(fitted_names)) && !anyDuplicated(fitted_names) &&
        !anyDuplicated(template_names) && all(fitted_names %in% template_names)) {
      out[match(fitted_names, template_names)] <- fitted
    } else {
      out[seq_along(fitted)] <- fitted
    }
  }
  out
}

projection_extend_list <- function(fitted, template, label) {
  if (!is.list(fitted) || !is.list(template) || length(fitted) > length(template)) {
    stop(label, " has incompatible fitted and projection-template groups.")
  }
  out <- template
  for (i in seq_along(fitted)) {
    out[[i]] <- projection_extend_value(
      fitted[[i]], template[[i]], paste(label, "group", i)
    )
  }
  out
}

projection_extend_grouped_rows <- function(fitted, template,
                                            fitted_sizes, template_sizes,
                                            label) {
  fitted_sizes <- as.integer(fitted_sizes)
  template_sizes <- as.integer(template_sizes)
  if (!is.matrix(fitted) || !is.matrix(template) ||
      ncol(fitted) != ncol(template) || sum(fitted_sizes) != nrow(fitted) ||
      sum(template_sizes) != nrow(template) ||
      length(fitted_sizes) > length(template_sizes)) {
    stop(label, " has incompatible fitted and projection-template groups.")
  }
  take_rows <- function(x, starts, sizes, i) {
    if (!sizes[[i]]) return(x[FALSE, , drop = FALSE])
    x[starts[[i]] + seq_len(sizes[[i]]) - 1L, , drop = FALSE]
  }
  fitted_starts <- cumsum(c(1L, head(fitted_sizes, -1L)))
  template_starts <- cumsum(c(1L, head(template_sizes, -1L)))
  blocks <- lapply(seq_along(template_sizes), function(i) {
    template_block <- take_rows(template, template_starts, template_sizes, i)
    if (i > length(fitted_sizes)) return(template_block)
    projection_overlay_array(
      take_rows(fitted, fitted_starts, fitted_sizes, i),
      template_block,
      paste(label, "group", i)
    )
  })
  do.call(rbind, blocks)
}

projection_compilation_version <- function(path) {
  lines <- readLines(path, warn = FALSE)
  header <- grep(
    "^\\s*#\\s*MULTIFAN-CL compilation version number",
    lines,
    ignore.case = TRUE
  )
  if (!length(header) || header[[1L]] >= length(lines)) return(NA_integer_)
  for (i in seq.int(header[[1L]] + 1L, length(lines))) {
    text <- trimws(sub("#.*$", "", lines[[i]]))
    if (!nzchar(text)) next
    value <- suppressWarnings(as.integer(strsplit(text, "[[:space:]]+")[[1L]][[1L]]))
    if (is.finite(value)) return(value)
    break
  }
  NA_integer_
}

restore_projection_compilation_version <- function(template_file, target_file) {
  value <- projection_compilation_version(template_file)
  if (!is.finite(value)) return(NA_integer_)
  lines <- readLines(target_file, warn = FALSE)
  existing <- grep(
    "^\\s*#\\s*MULTIFAN-CL compilation version number",
    lines,
    ignore.case = TRUE
  )
  block <- c("# MULTIFAN-CL compilation version number", as.character(value))
  if (length(existing)) {
    i <- existing[[1L]]
    if (i < length(lines)) {
      lines[i:(i + 1L)] <- block
    } else {
      lines <- c(lines[seq_len(i - 1L)], block)
    }
  } else {
    insert_at <- grep(
      "^\\s*#\\s*The grouped_catch_dev_coffs flag",
      lines,
      ignore.case = TRUE
    )
    if (!length(insert_at)) {
      stop("Cannot restore MFCL compilation version in ", target_file, ".")
    }
    i <- insert_at[[1L]]
    lines <- c(lines[seq_len(i - 1L)], block, lines[i:length(lines)])
  }
  writeLines(lines, target_file, useBytes = TRUE)
  value
}

projection_static_slot_audit <- function(fitted, generated) {
  projection_slots <- c(
    "flags", "dimensions", "range", "rep_rate_dev_coffs", "fm_level_devs",
    "q_dev_coffs", "sel_dev_coffs", "sel_dev_coffs2", "growth_devs_cohort",
    "unused", "lagrangian", "effort_dev_coffs", "annual_rel_rec_coffs",
    "orth_coffs", "catch_dev_coffs", "region_rec_var", "rel_rec",
    "rec_standard", "rec_orthogonal", "logistic_normal_params",
    "historic_flags", "tag_fish_rep_rate", "tag_fish_rep_flags",
    "tag_fish_rep_grp", "tag_fish_rep_pen", "tag_fish_rep_target"
  )
  slots <- setdiff(intersect(slotNames(fitted), slotNames(generated)), projection_slots)
  data.frame(
    slot = slots,
    preserved = vapply(slots, function(name) {
      isTRUE(all.equal(slot(fitted, name), slot(generated, name),
                       check.attributes = TRUE))
    }, logical(1)),
    stringsAsFactors = FALSE
  )
}

extend_native_projection_par <- function(fitted_par, makepar_par, proj_frq) {
  x <- fitted_par
  y <- makepar_par
  projection_years <- seq.int(
    range(x)["maxyear"] + 1L,
    range(x)["maxyear"] +
      (dimensions(y)[2] - dimensions(x)[2]) / dimensions(x)[3]
  )

  if (flagval(x, 2, 199)$value == 0) {
    flagval(x, 2, 199) <- dimensions(x)["years"]
  }
  period <- recPeriod(
    x,
    af199 = flagval(x, 2, 199)$value,
    af200 = flagval(x, 2, 200)$value
  )
  if (flagval(x, 1, 232)$value == 0) flagval(x, 1, 232) <- period["pf232"]
  if (flagval(x, 1, 233)$value == 0) flagval(x, 1, 233) <- period["pf233"]

  fitted_q <- q_dev_coffs(x)
  template_q <- q_dev_coffs(y)
  rep_rate_dev_coffs(x) <- projection_extend_list(
    rep_rate_dev_coffs(x), rep_rate_dev_coffs(y), "reporting-rate deviations"
  )
  fm_level_devs(x) <- projection_extend_value(
    fm_level_devs(x), fm_level_devs(y), "implicit fishing-mortality levels"
  )
  q_dev_coffs(x) <- projection_extend_list(
    fitted_q, template_q, "catchability deviations"
  )
  sel_dev_coffs(x) <- projection_extend_grouped_rows(
    sel_dev_coffs(x), sel_dev_coffs(y),
    vapply(fitted_q, length, integer(1)),
    vapply(template_q, length, integer(1)),
    "selectivity deviations"
  )
  sel_dev_coffs2(x) <- projection_extend_list(
    sel_dev_coffs2(x), sel_dev_coffs2(y), "secondary selectivity deviations"
  )
  growth_devs_cohort(x) <- projection_extend_value(
    growth_devs_cohort(x), growth_devs_cohort(y), "cohort growth deviations"
  )
  unused(x) <- projection_extend_list(unused(x), unused(y), "year and season flags")
  # Augmented-Lagrangian rows are projection-layout structures. They are not
  # fitted operating-state inputs and must retain the exact -makepar layout.
  lagrangian(x) <- lagrangian(y)

  if (any(dim(tag_fish_rep_rate(x)) != dim(tag_fish_rep_rate(y)))) {
    flags(x) <- rbind(
      flags(x),
      flags(y)[!is.element(flags(y)$flagtype, flags(x)$flagtype), ]
    )
    for (name in c(
      "tag_fish_rep_rate", "tag_fish_rep_flags", "tag_fish_rep_grp",
      "tag_fish_rep_pen", "tag_fish_rep_target"
    )) {
      slot(x, name) <- projection_extend_value(slot(x, name), slot(y, name), name)
    }
  }
  if (flagval(x, 1, 200)$value < 1053 &&
      length(grep("# Other lambdas", lagrangian(x)))) {
    lagrangian(x) <- lagrangian(x)[seq_len(grep("Other lambdas", lagrangian(x)) - 1L)]
  }

  effort_increments <- vapply(effort_dev_coffs(y), length, integer(1)) -
    vapply(effort_dev_coffs(x), length, integer(1))
  for (i in seq_along(effort_increments)) {
    if (effort_increments[[i]] < 0L) {
      effort_dev_coffs(x)[[i]] <- effort_dev_coffs(x)[[i]][
        seq_len(length(effort_dev_coffs(x)[[i]]) + effort_increments[[i]])
      ]
      effort_increments[[i]] <- 0L
    }
  }
  effort_dev_coffs(x) <- lapply(seq_len(dimensions(x)["fisheries"]), function(i) {
    c(effort_dev_coffs(x)[[i]], rep(0, effort_increments[[i]]))
  })
  annual_rel_rec_coffs(x) <- cbind(
    annual_rel_rec_coffs(x),
    array(0, dim = c(nrow(annual_rel_rec_coffs(x)), length(projection_years)))
  )
  if (any(flagval(x, 1, 155)$value > 0, na.rm = TRUE)) {
    orth_coffs(x) <- cbind(
      orth_coffs(x),
      array(0, dim = c(nrow(annual_rel_rec_coffs(x)), length(projection_years)))
    )
  }

  if (flagval(x, 1, 373)$value == 0) {
    catch_dev_coffs(x) <- lapply(
      seq_along(catch_dev_coffs(x)),
      function(group) {
        c(
          catch_dev_coffs(x)[[group]],
          rep(0, length(projection_years) * dimensions(x)["seasons"])
        )
      }
    )
    fish_group <- flagval(x, -(seq_len(n_fisheries(x))), 29)$value
    incidents <- data.frame(
      period = with(realisations(proj_frq), paste(year, month, sep = "_")),
      fishery = realisations(proj_frq)$fishery
    )
    for (group in sort(unique(fish_group))) {
      required <- length(unique(incidents$period[
        incidents$fishery %in% which(fish_group == group)
      ])) - 1L
      catch_dev_coffs(x)[[group]] <- c(
        catch_dev_coffs(x)[[group]],
        rep(0, max(required - length(catch_dev_coffs(x)[[group]]), 0L))
      )
    }

    group_map <- data.frame(
      fishery = seq_len(n_fisheries(proj_frq)),
      group = fish_group,
      seasons = 0L
    )
    terminal_incidents <- table(subset(
      realisations(proj_frq),
      year == range(proj_frq)["maxyear"]
    )$fishery)
    group_map$seasons[group_map$fishery %in% names(terminal_incidents)] <-
      terminal_incidents
    seasons_by_group <- tapply(group_map$seasons, group_map$group, max)
    catch_dev_coffs(x) <- lapply(
      seq_along(catch_dev_coffs(x)),
      function(group) {
        c(
          catch_dev_coffs(x)[[group]],
          rep(0, length(projection_years) * seasons_by_group[[group]])
        )
      }
    )
    for (group in sort(unique(fish_group))) {
      required <- length(unique(incidents$period[
        incidents$fishery %in% which(fish_group == group)
      ])) - 1L
      catch_dev_coffs(x)[[group]] <- c(
        catch_dev_coffs(x)[[group]],
        rep(0, max(required - length(catch_dev_coffs(x)[[group]]), 0L))
      )
    }
  }

  region_rec_var(x) <- window(
    region_rec_var(x), start = range(x)["minyear"], end = range(y)["maxyear"]
  )
  region_rec_var(x)[is.na(region_rec_var(x))] <- 0
  region_rec_var(x)[, as.character(range(x)["maxyear"]:(range(y)["maxyear"] - 1L))] <-
    apply(
      region_rec_var(x)[, as.character(range(x)["minyear"]:(range(x)["maxyear"] - 1L))],
      c(1, 3, 4, 5, 6), mean
    )
  rel_rec(x) <- window(rel_rec(x), start = range(x)["minyear"], end = range(y)["maxyear"])
  rel_rec(x)[, as.character(projection_years)] <- rel_rec(y)[, as.character(projection_years)]

  dimensions(x) <- dimensions(y)
  range(x) <- range(y)
  audit <- projection_static_slot_audit(fitted_par, x)
  if (any(!audit$preserved)) {
    stop("Projection PAR reset fitted static slots: ",
         paste(audit$slot[!audit$preserved], collapse = ", "))
  }
  attr(x, "projection_state_audit") <- audit
  x
}
