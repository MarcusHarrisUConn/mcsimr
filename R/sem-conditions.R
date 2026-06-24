sem_parameter_conditions <- function(lhs = character(),
                                     op = character(),
                                     rhs = character(),
                                     values = list(),
                                     label = NULL) {
  if (length(lhs) == 0L) {
    return(data.frame(
      lhs = character(),
      op = character(),
      rhs = character(),
      label = character(),
      values = I(list()),
      stringsAsFactors = FALSE
    ))
  }
  stopifnot(length(lhs) == length(op), length(op) == length(rhs))
  if (!is.list(values)) {
    values <- as.list(values)
  }
  if (length(values) != length(lhs)) {
    stop("`values` must contain one vector of condition values per parameter.", call. = FALSE)
  }
  if (is.null(label)) {
    label <- paste(lhs, op, rhs)
  }
  data.frame(
    lhs = as.character(lhs),
    op = as.character(op),
    rhs = as.character(rhs),
    label = as.character(label),
    values = I(lapply(values, as.numeric)),
    stringsAsFactors = FALSE
  )
}

normalize_sem_parameter_conditions <- function(parameter_conditions = NULL) {
  if (is.null(parameter_conditions)) {
    return(sem_parameter_conditions())
  }
  if (!is.data.frame(parameter_conditions)) {
    stop("`parameter_conditions` must be a data frame.", call. = FALSE)
  }
  required <- c("lhs", "op", "rhs", "values")
  missing <- setdiff(required, names(parameter_conditions))
  if (length(missing)) {
    stop("`parameter_conditions` is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  label <- parameter_conditions$label
  if (is.null(label)) {
    label <- paste(parameter_conditions$lhs, parameter_conditions$op, parameter_conditions$rhs)
  }
  values <- parameter_conditions$values
  if (!is.list(values)) {
    values <- lapply(values, function(x) {
      if (is.character(x)) {
        as.numeric(trimws(strsplit(x, ",", fixed = TRUE)[[1L]]))
      } else {
        as.numeric(x)
      }
    })
  }
  sem_parameter_conditions(
    lhs = parameter_conditions$lhs,
    op = parameter_conditions$op,
    rhs = parameter_conditions$rhs,
    values = values,
    label = label
  )
}

sem_condition_column_names <- function(parameter_conditions) {
  if (nrow(parameter_conditions) == 0L) {
    return(character())
  }
  paste0("param_", seq_len(nrow(parameter_conditions)))
}

apply_sem_parameter_conditions <- function(population_model, parameter_conditions, condition) {
  parameter_conditions <- normalize_sem_parameter_conditions(parameter_conditions)
  if (nrow(parameter_conditions) == 0L) {
    return(population_model)
  }

  tab <- lavaan::lavaanify(population_model, fixed.x = FALSE)
  cols <- sem_condition_column_names(parameter_conditions)

  for (i in seq_len(nrow(parameter_conditions))) {
    value <- condition[[cols[[i]]]]
    hit <- tab$lhs == parameter_conditions$lhs[[i]] &
      tab$op == parameter_conditions$op[[i]] &
      tab$rhs == parameter_conditions$rhs[[i]]
    if (!any(hit)) {
      stop(
        "Parameter condition not found in population model: ",
        parameter_conditions$lhs[[i]], " ",
        parameter_conditions$op[[i]], " ",
        parameter_conditions$rhs[[i]],
        call. = FALSE
      )
    }
    tab$ustart[hit] <- as.numeric(value)
    tab$free[hit] <- 0L
  }

  tab
}

sem_model_parameters <- function(model,
                                 operators = c("=~", "~", "~~", "~1"),
                                 include_fixed = TRUE) {
  stopifnot(length(model) == 1L, nzchar(model))
  tab <- tryCatch(
    lavaan::lavaanify(model, fixed.x = FALSE),
    error = function(e) {
      stop("Could not parse lavaan model syntax: ", conditionMessage(e), call. = FALSE)
    }
  )
  keep <- tab$op %in% operators
  if (!include_fixed && "free" %in% names(tab)) {
    keep <- keep & tab$free > 0L
  }
  tab <- tab[keep, , drop = FALSE]
  if (!nrow(tab)) {
    return(data.frame(
      lhs = character(),
      op = character(),
      rhs = character(),
      label = character(),
      value = numeric(),
      free = logical(),
      stringsAsFactors = FALSE
    ))
  }

  value <- tab$ustart
  value[is.na(value)] <- NA_real_
  out <- data.frame(
    lhs = tab$lhs,
    op = tab$op,
    rhs = tab$rhs,
    label = paste(tab$lhs, tab$op, tab$rhs),
    value = as.numeric(value),
    free = tab$free > 0L,
    stringsAsFactors = FALSE
  )
  unique(out)
}

sem_design_summary <- function(spec) {
  validate_sem_spec(spec)
  grid <- sem_condition_grid(spec)
  parameter_conditions <- normalize_sem_parameter_conditions(spec$parameter_conditions)
  parameter_grid_size <- if (nrow(parameter_conditions)) {
    prod(vapply(parameter_conditions$values, length, integer(1L)))
  } else {
    1L
  }

  data.frame(
    factor = c(
      "Sample sizes",
      "Estimators",
      "Missing rates",
      "Missing mechanisms",
      "Skewness values",
      "Kurtosis values",
      "Varied lavaan parameters",
      "Parameter combinations",
      "Total conditions",
      "Total model fits"
    ),
    levels = c(
      length(spec$n),
      length(spec$estimator),
      length(spec$missing_rate),
      length(spec$missing_mechanism),
      length(spec$skewness),
      length(spec$kurtosis),
      nrow(parameter_conditions),
      parameter_grid_size,
      nrow(grid),
      nrow(grid) * spec$reps
    ),
    values = c(
      sem_design_values(spec$n),
      sem_design_values(spec$estimator),
      sem_design_values(spec$missing_rate),
      sem_design_values(spec$missing_mechanism),
      sem_design_values(spec$skewness),
      sem_design_values(spec$kurtosis),
      if (nrow(parameter_conditions)) paste(parameter_conditions$label, collapse = "; ") else "none",
      if (nrow(parameter_conditions)) {
        paste(vapply(parameter_conditions$values, sem_design_values, character(1L)), collapse = " x ")
      } else {
        "none"
      },
      as.character(nrow(grid)),
      as.character(nrow(grid) * spec$reps)
    ),
    stringsAsFactors = FALSE
  )
}

sem_design_warnings <- function(spec) {
  validate_sem_spec(spec)
  grid <- sem_condition_grid(spec)
  parameter_conditions <- normalize_sem_parameter_conditions(spec$parameter_conditions)
  messages <- list()

  add_message <- function(level, message) {
    messages[[length(messages) + 1L]] <<- data.frame(
      level = level,
      message = message,
      stringsAsFactors = FALSE
    )
  }

  total_fits <- nrow(grid) * spec$reps
  if (total_fits >= 10000L) {
    add_message(
      "warning",
      paste0("This design schedules ", total_fits, " fitted models. Use checkpoints, parallel workers, and preferably targets or HPC sharding.")
    )
  }
  if (spec$reps < 100L) {
    add_message(
      "info",
      "Fewer than 100 replications is useful for a smoke test, but final Monte Carlo evidence usually needs many more replications."
    )
  }
  if (!nrow(parameter_conditions)) {
    add_message(
      "info",
      "No lavaan population parameters are varied yet; the design only crosses global factors such as N, estimator, missingness, and nonnormality."
    )
  }
  if (any(spec$missing_rate > 0) && identical(tolower(spec$missing), "listwise")) {
    add_message(
      "warning",
      "Missingness is enabled while lavaan uses listwise deletion. Consider FIML when the estimand and mechanism make that appropriate."
    )
  }
  if ("mar" %in% spec$missing_mechanism && is.null(spec$missing_driver)) {
    add_message(
      "info",
      "MAR missingness has no driver variable set; mcsimr will use the first generated variable that is not a missingness target."
    )
  }
  if (any(spec$missing_rate >= 0.40)) {
    add_message(
      "warning",
      "At least one missing-data condition removes 40% or more of targeted cells. Monitor convergence and admissibility closely."
    )
  }
  if (nrow(grid) >= 250L) {
    add_message(
      "warning",
      "The condition grid is large. Run a small pilot first, then use checkpoints or a targets pipeline for the full study."
    )
  }
  if (!length(messages)) {
    add_message("ok", "The design is ready for a pilot run.")
  }

  do.call(rbind, messages)
}

sem_design_values <- function(x, max_values = 6L) {
  x <- as.character(x)
  x <- x[nzchar(x)]
  if (!length(x)) {
    return("none")
  }
  if (length(x) > max_values) {
    return(paste0(paste(x[seq_len(max_values)], collapse = ", "), ", ..."))
  }
  paste(x, collapse = ", ")
}
