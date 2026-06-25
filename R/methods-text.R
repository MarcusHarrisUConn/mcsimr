simulation_methods_text <- function(spec) {
  if (identical(spec$type, "sem") || inherits(spec, "mcsimr_sem_spec")) {
    return(sem_methods_text(spec))
  }
  ols_methods_text(spec)
}

sem_methods_text <- function(spec) {
  validate_sem_spec(spec)
  grid <- sem_condition_grid(spec)
  parameters <- normalize_sem_parameter_conditions(spec$parameter_conditions)
  varied <- if (nrow(parameters)) {
    paste(parameters$label, collapse = ", ")
  } else {
    "no lavaan population parameters"
  }
  group_text <- if (!is.null(spec$group_variable)) {
    paste0(
      " A multiple-group design was simulated using group variable `",
      spec$group_variable,
      "` with groups ",
      paste(spec$group_labels, collapse = ", "),
      "."
    )
  } else {
    ""
  }
  misspec_text <- if (!identical(spec$misspecification, "none")) {
    paste0(" The fitted model intentionally used the `", spec$misspecification, "` misspecification preset.")
  } else {
    ""
  }
  paste0(
    "A Monte Carlo simulation study was specified for a lavaan structural equation model. ",
    "The design crossed sample size values (", paste(spec$n, collapse = ", "), "), estimator values (",
    paste(spec$estimator, collapse = ", "), "), missing-data rates (", paste(spec$missing_rate, collapse = ", "),
    "), missing-data mechanisms (", paste(spec$missing_mechanism, collapse = ", "), "), skewness values (",
    paste(spec$skewness, collapse = ", "), "), excess kurtosis values (", paste(spec$kurtosis, collapse = ", "),
    "), and varied parameters: ", varied, ". ",
    "The design contained ", nrow(grid), " condition(s), with ", spec$reps,
    " replication(s) per condition. Missing data were handled in lavaan with `", spec$missing,
    "`, and latent variables were fit with `std.lv = ", spec$std_lv, "`.",
    group_text,
    misspec_text,
    " The base random seed was ", spec$seed, "."
  )
}

ols_methods_text <- function(spec) {
  validate_ols_spec(spec)
  grid <- condition_grid(spec)
  paste0(
    "A Monte Carlo simulation study was specified for ordinary least squares regression. ",
    "The design crossed sample size values (", paste(spec$n, collapse = ", "),
    "), predictor correlations (", paste(spec$predictor_correlation, collapse = ", "),
    "), and residual standard deviations (", paste(spec$error_sd, collapse = ", "),
    "). The design contained ", nrow(grid), " condition(s), with ", spec$reps,
    " replication(s) per condition. The base random seed was ", spec$seed, "."
  )
}
