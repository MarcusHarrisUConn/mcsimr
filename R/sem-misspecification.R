sem_misspecification_presets <- function() {
  data.frame(
    name = c(
      "none",
      "omit_residual_covariance",
      "omit_cross_loading",
      "omit_structural_path",
      "omit_factor_covariance",
      "omit_loading",
      "wrong_factor_structure"
    ),
    title = c(
      "No misspecification",
      "Omit residual covariance",
      "Omit cross-loading",
      "Omit structural path",
      "Omit factor covariance",
      "Omit loading",
      "Wrong factor structure"
    ),
    description = c(
      "Fit the analysis model as supplied.",
      "Remove the first observed-variable residual covariance from the fitted model.",
      "Remove the first secondary loading from the fitted model.",
      "Remove the first structural regression path from the fitted model.",
      "Remove the first latent covariance from the fitted model.",
      "Remove the last loading from the first measurement block in the fitted model.",
      "Move the last indicator from the first factor onto the second factor when possible."
    ),
    stringsAsFactors = FALSE
  )
}

apply_sem_misspecification <- function(fitted_model,
                                       preset = "none",
                                       population_model = fitted_model) {
  presets <- sem_misspecification_presets()
  preset <- match.arg(preset, presets$name)
  if (identical(preset, "none")) {
    return(fitted_model)
  }

  lines <- sem_model_lines(fitted_model)

  out <- switch(
    preset,
    omit_residual_covariance = remove_first_matching_line(
      lines,
      predicate = function(x) {
        grepl("~~", x, fixed = TRUE) &&
          all(!sem_line_lhs_rhs(x, "~~") %in% sem_latent_names(population_model)) &&
          !identical(sem_line_lhs_rhs(x, "~~")[[1L]], sem_line_lhs_rhs(x, "~~")[[2L]])
      }
    ),
    omit_cross_loading = remove_first_cross_loading(lines),
    omit_structural_path = remove_first_matching_line(
      lines,
      predicate = function(x) grepl("~", x, fixed = TRUE) && !grepl("=~|~~|~1", x)
    ),
    omit_factor_covariance = remove_first_matching_line(
      lines,
      predicate = function(x) {
        grepl("~~", x, fixed = TRUE) &&
          all(sem_line_lhs_rhs(x, "~~") %in% sem_latent_names(population_model)) &&
          !identical(sem_line_lhs_rhs(x, "~~")[[1L]], sem_line_lhs_rhs(x, "~~")[[2L]])
      }
    ),
    omit_loading = remove_last_loading(lines),
    wrong_factor_structure = wrong_factor_structure_lines(lines),
    lines
  )

  if (identical(out, lines)) {
    warning(
      "Misspecification preset did not change the fitted model: ",
      preset,
      call. = FALSE
    )
  }
  paste(out, collapse = "\n")
}

sem_model_lines <- function(model) {
  lines <- trimws(unlist(strsplit(model, "\n", fixed = TRUE)))
  lines[nzchar(lines) & !grepl("^#", lines)]
}

sem_latent_names <- function(model) {
  lines <- sem_model_lines(model)
  measurement <- lines[grepl("=~", lines, fixed = TRUE)]
  unique(trimws(vapply(strsplit(measurement, "=~", fixed = TRUE), `[[`, character(1L), 1L)))
}

sem_line_lhs_rhs <- function(line, operator) {
  parts <- trimws(strsplit(line, operator, fixed = TRUE)[[1L]])
  if (length(parts) < 2L) {
    return(c("", ""))
  }
  rhs <- trimws(gsub("^[^*]+[*]", "", parts[[2L]]))
  c(parts[[1L]], rhs)
}

remove_first_matching_line <- function(lines, predicate) {
  hit <- vapply(lines, predicate, logical(1L))
  if (!any(hit)) {
    return(lines)
  }
  lines[-which(hit)[[1L]]]
}

remove_first_cross_loading <- function(lines) {
  measurement <- lines[grepl("=~", lines, fixed = TRUE)]
  indicator_map <- list()
  for (line in measurement) {
    parts <- strsplit(line, "=~", fixed = TRUE)[[1L]]
    factor <- trimws(parts[[1L]])
    indicators <- trimws(unlist(strsplit(parts[[2L]], "+", fixed = TRUE)))
    indicators <- gsub("^[^*]+[*]", "", indicators)
    for (indicator in indicators) {
      indicator_map[[indicator]] <- c(indicator_map[[indicator]], factor)
    }
  }
  cross_loaded <- names(indicator_map)[vapply(indicator_map, length, integer(1L)) > 1L]
  if (!length(cross_loaded)) {
    return(lines)
  }
  hit <- which(grepl("=~", lines, fixed = TRUE) & grepl(cross_loaded[[1L]], lines, fixed = TRUE))
  if (length(hit) <= 1L) {
    return(lines)
  }
  remove_indicator_from_measurement_line(lines, hit[[length(hit)]], cross_loaded[[1L]])
}

remove_last_loading <- function(lines) {
  idx <- which(grepl("=~", lines, fixed = TRUE))
  if (!length(idx)) {
    return(lines)
  }
  i <- idx[[1L]]
  parts <- strsplit(lines[[i]], "=~", fixed = TRUE)[[1L]]
  indicators <- trimws(unlist(strsplit(parts[[2L]], "+", fixed = TRUE)))
  if (length(indicators) <= 2L) {
    return(lines)
  }
  indicators <- indicators[-length(indicators)]
  lines[[i]] <- paste(trimws(parts[[1L]]), "=~", paste(indicators, collapse = " + "))
  lines
}

remove_indicator_from_measurement_line <- function(lines, line_index, indicator) {
  parts <- strsplit(lines[[line_index]], "=~", fixed = TRUE)[[1L]]
  indicators <- trimws(unlist(strsplit(parts[[2L]], "+", fixed = TRUE)))
  clean <- trimws(gsub("^[^*]+[*]", "", indicators))
  keep <- clean != indicator
  if (sum(keep) < 2L) {
    return(lines)
  }
  lines[[line_index]] <- paste(trimws(parts[[1L]]), "=~", paste(indicators[keep], collapse = " + "))
  lines
}

wrong_factor_structure_lines <- function(lines) {
  idx <- which(grepl("=~", lines, fixed = TRUE))
  if (length(idx) < 2L) {
    return(lines)
  }
  first <- strsplit(lines[[idx[[1L]]]], "=~", fixed = TRUE)[[1L]]
  second <- strsplit(lines[[idx[[2L]]]], "=~", fixed = TRUE)[[1L]]
  first_indicators <- trimws(unlist(strsplit(first[[2L]], "+", fixed = TRUE)))
  second_indicators <- trimws(unlist(strsplit(second[[2L]], "+", fixed = TRUE)))
  if (length(first_indicators) <= 2L) {
    return(lines)
  }
  moved <- first_indicators[[length(first_indicators)]]
  first_indicators <- first_indicators[-length(first_indicators)]
  second_indicators <- c(second_indicators, moved)
  lines[[idx[[1L]]]] <- paste(trimws(first[[1L]]), "=~", paste(first_indicators, collapse = " + "))
  lines[[idx[[2L]]]] <- paste(trimws(second[[1L]]), "=~", paste(second_indicators, collapse = " + "))
  lines
}
