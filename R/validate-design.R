validate_simulation_design <- function(spec) {
  if (identical(spec$type, "sem") || inherits(spec, "mcsimr_sem_spec")) {
    return(validate_sem_design(spec))
  }
  validate_ols_design(spec)
}

validate_ols_design <- function(spec) {
  validate_ols_spec(spec)
  rows <- list()
  add <- function(level, check, message) {
    rows[[length(rows) + 1L]] <<- data.frame(level = level, check = check, message = message, stringsAsFactors = FALSE)
  }
  grid <- condition_grid(spec)
  add("ok", "condition grid", paste(nrow(grid), "condition(s) defined."))
  if (spec$reps < 100L) {
    add("info", "replications", "Fewer than 100 replications is best treated as a smoke test.")
  } else {
    add("ok", "replications", paste(spec$reps, "replications per condition."))
  }
  if (any(spec$error_sd <= 0)) {
    add("error", "residual variance", "All residual standard deviations must be positive.")
  }
  do.call(rbind, rows)
}

validate_sem_design <- function(spec) {
  validate_sem_spec(spec)
  rows <- list()
  add <- function(level, check, message) {
    rows[[length(rows) + 1L]] <<- data.frame(level = level, check = check, message = message, stringsAsFactors = FALSE)
  }

  population_parse <- tryCatch(lavaan::lavaanify(spec$population_model, fixed.x = FALSE), error = identity)
  fitted_parse <- tryCatch(lavaan::lavaanify(spec$fitted_model, fixed.x = FALSE), error = identity)
  if (inherits(population_parse, "error")) {
    add("error", "population model", conditionMessage(population_parse))
  } else {
    add("ok", "population model", paste(nrow(population_parse), "lavaan parameter rows parsed."))
  }
  if (inherits(fitted_parse, "error")) {
    add("error", "fitted model", conditionMessage(fitted_parse))
  } else {
    add("ok", "fitted model", paste(nrow(fitted_parse), "lavaan parameter rows parsed."))
  }

  grid <- tryCatch(sem_condition_grid(spec), error = identity)
  if (inherits(grid, "error")) {
    add("error", "condition grid", conditionMessage(grid))
  } else {
    add("ok", "condition grid", paste(nrow(grid), "condition(s) defined."))
    if (nrow(grid) * spec$reps >= 10000L) {
      add("warning", "run size", paste(nrow(grid) * spec$reps, "model fits scheduled. Use checkpoints and HPC/targets for full runs."))
    }
  }

  parameter_conditions <- normalize_sem_parameter_conditions(spec$parameter_conditions)
  if (nrow(parameter_conditions) && !inherits(population_parse, "error")) {
    keys <- sem_param_key(population_parse$lhs, population_parse$op, population_parse$rhs)
    requested <- sem_param_key(parameter_conditions$lhs, parameter_conditions$op, parameter_conditions$rhs)
    missing <- setdiff(requested, keys)
    if (length(missing)) {
      add("error", "parameter conditions", paste("Not found in population model:", paste(missing, collapse = ", ")))
    } else {
      add("ok", "parameter conditions", paste(nrow(parameter_conditions), "population parameter(s) varied."))
    }
  }

  if (any(spec$missing_rate > 0) && identical(tolower(spec$missing), "listwise")) {
    add("warning", "missing data", "Missingness is enabled while lavaan uses listwise deletion.")
  }
  if (!is.null(spec$group_variable)) {
    add("ok", "multiple groups", paste("Group variable", spec$group_variable, "with labels:", paste(spec$group_labels, collapse = ", ")))
  }
  if (!identical(spec$misspecification, "none")) {
    add("info", "misspecification", paste("Fitted model uses preset:", spec$misspecification))
  }
  if (!length(rows)) {
    add("ok", "design", "The design passed basic validation.")
  }
  do.call(rbind, rows)
}
