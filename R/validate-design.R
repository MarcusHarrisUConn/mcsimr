validate_simulation_design <- function(spec) {
  if (identical(spec$type, "sem") || inherits(spec, "mcsimr_sem_spec")) {
    return(validate_sem_design(spec))
  }
  validate_ols_design(spec)
}

simulation_readiness <- function(spec, mode = NULL) {
  if (identical(spec$type, "sem") || inherits(spec, "mcsimr_sem_spec")) {
    validate_sem_spec(spec)
    grid <- sem_condition_grid(spec)
    n_parameters <- tryCatch(nrow(sem_model_parameters(spec$fitted_model)), error = function(e) NA_integer_)
    missing_rate <- spec$missing_rate
    mechanisms <- spec$missing_mechanism
  } else {
    validate_ols_spec(spec)
    grid <- condition_grid(spec)
    n_parameters <- length(spec$betas) + 1L
    missing_rate <- 0
    mechanisms <- "none"
  }

  if (is.null(mode)) {
    mode <- if (!is.null(spec$readiness_mode)) spec$readiness_mode else "teaching"
  }
  mode <- match.arg(mode, c("teaching", "publication"))
  publication <- identical(mode, "publication")
  rows <- list()
  add <- function(level, check, message, action) {
    rows[[length(rows) + 1L]] <<- data.frame(
      mode = mode,
      level = level,
      check = check,
      message = message,
      action = action,
      stringsAsFactors = FALSE
    )
  }

  total_fits <- nrow(grid) * spec$reps
  min_reps <- if (publication) 500L else 100L
  if (spec$reps < min_reps) {
    add(
      if (publication) "warning" else "info",
      "replications",
      paste("The design uses", spec$reps, "replications per condition."),
      paste("Use at least", min_reps, "replications for", mode, "mode, or document why a smaller pilot is enough.")
    )
  } else {
    add("ok", "replications", paste(spec$reps, "replications per condition."), "Replication count meets the current mode threshold.")
  }

  if (total_fits >= if (publication) 25000L else 10000L) {
    add(
      "warning",
      "run size",
      paste(total_fits, "model fits are scheduled."),
      "Run a pilot first, keep checkpointing enabled, and consider sharding or targets for the full study."
    )
  } else {
    add("ok", "run size", paste(total_fits, "model fits are scheduled."), "Run size is reasonable for the current mode.")
  }

  min_n <- min(spec$n, na.rm = TRUE)
  if (is.finite(n_parameters) && min_n / max(1L, n_parameters) < if (publication) 10 else 5) {
    add(
      if (publication) "warning" else "info",
      "sample size per parameter",
      paste("Smallest N is", min_n, "with about", n_parameters, "model parameters."),
      "Consider increasing the smallest sample size or simplifying the model before a final run."
    )
  }

  missing_method <- if (is.null(spec$missing)) "none" else spec$missing
  if (any(missing_rate > 0) && identical(tolower(missing_method), "listwise")) {
    add("warning", "missing data", "Missingness is enabled with listwise deletion.", "Consider FIML/direct ML when appropriate for the estimand and mechanism.")
  }
  if ("mar" %in% mechanisms && is.null(spec$missing_driver)) {
    add(if (publication) "warning" else "info", "MAR driver", "MAR missingness has no explicit driver variable.", "Set a driver variable so the missing-data mechanism is transparent.")
  }
  if (any(missing_rate >= if (publication) 0.30 else 0.40)) {
    add("warning", "missing rate", "One or more missing-data rates are high.", "Monitor convergence, admissibility, and effective sample size by condition.")
  }

  rationale_fields <- c("research_question", "design_rationale", "metric_rationale", "interpretation_plan")
  missing_rationale <- rationale_fields[!vapply(rationale_fields, function(field) {
    !is.null(spec[[field]]) && nzchar(spec[[field]])
  }, logical(1L))]
  if (length(missing_rationale)) {
    add(
      if (publication) "warning" else "info",
      "rationale",
      paste("Missing rationale field(s):", paste(missing_rationale, collapse = ", ")),
      "Complete these fields so the exported report explains why the design was chosen."
    )
  } else {
    add("ok", "rationale", "Research question, design rationale, metric rationale, and interpretation plan are present.", "Keep these aligned with the final manuscript.")
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

readiness_decision <- function(readiness, diagnostics = NULL, checklist = NULL) {
  if (is.null(readiness) || !nrow(readiness)) {
    return(data.frame(
      decision = "run_review",
      decision_label = "Run the readiness review",
      level = "error",
      issues = 1L,
      message = "No readiness review was available.",
      next_step = "Create a simulation specification and run simulation_readiness() before interpreting results.",
      stringsAsFactors = FALSE
    ))
  }

  readiness_levels <- tolower(as.character(readiness$level))
  diagnostic_levels <- if (!is.null(diagnostics) && nrow(diagnostics)) {
    tolower(as.character(diagnostics$severity))
  } else {
    character()
  }
  checklist_levels <- if (!is.null(checklist) && nrow(checklist)) {
    tolower(as.character(checklist$status))
  } else {
    character()
  }

  issue_levels <- c(readiness_levels, diagnostic_levels, checklist_levels)
  high_issues <- sum(issue_levels %in% c("error", "warning"), na.rm = TRUE)
  review_issues <- sum(issue_levels %in% c("review", "missing", "info"), na.rm = TRUE)
  total_issues <- high_issues + review_issues

  if (high_issues > 0L) {
    decision <- "revise_before_claims"
    decision_label <- "Revise before making claims"
    level <- "warning"
    message <- paste(high_issues, "high-priority readiness or diagnostic issue(s) should be addressed before publication claims.")
    next_step <- "Revise the design or document a defensible rationale, then rerun the affected simulation conditions."
  } else if (review_issues > 0L) {
    decision <- "pilot_or_document"
    decision_label <- "Use as a pilot or document"
    level <- "review"
    message <- paste(review_issues, "review item(s) remain before treating this as a final study.")
    next_step <- "Use the run as a pilot or complete the rationale, reporting checklist, and interpretation notes."
  } else {
    decision <- "ready_for_publication_review"
    decision_label <- "Ready for publication review"
    level <- "ok"
    message <- "Automated readiness, diagnostics, and checklist items did not flag unresolved issues."
    next_step <- "Proceed to manuscript review, sensitivity checks, and substantive interpretation."
  }

  data.frame(
    decision = decision,
    decision_label = decision_label,
    level = level,
    issues = total_issues,
    message = message,
    next_step = next_step,
    stringsAsFactors = FALSE
  )
}

validate_ols_design <- function(spec) {
  validate_ols_spec(spec)
  rows <- list()
  add <- function(level, check, message) {
    rows[[length(rows) + 1L]] <<- data.frame(level = level, check = check, message = message, stringsAsFactors = FALSE)
  }
  grid <- condition_grid(spec)
  add("ok", "condition grid", paste(nrow(grid), "condition(s) defined."))
  readiness <- simulation_readiness(spec)
  for (i in seq_len(nrow(readiness))) {
    add(readiness$level[[i]], readiness$check[[i]], readiness$message[[i]])
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
  if (!inherits(grid, "error")) {
    readiness <- simulation_readiness(spec)
    for (i in seq_len(nrow(readiness))) {
      add(readiness$level[[i]], readiness$check[[i]], readiness$message[[i]])
    }
  }
  if (!length(rows)) {
    add("ok", "design", "The design passed basic validation.")
  }
  do.call(rbind, rows)
}
