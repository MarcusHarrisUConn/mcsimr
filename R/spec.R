available_cores <- function(reserve = 1L) {
  cores <- future::availableCores()
  max(1L, as.integer(cores[[1L]]) - as.integer(reserve))
}

ols_sim_spec <- function(n = c(100, 250, 500),
                         reps = 1000,
                         betas = c(0.20, 0.30, 0.00),
                         intercept = 0,
                         predictor_correlation = 0,
                         error_sd = 1,
                         alpha = 0.05,
                         seed = 20260608,
                         fitted_formula = NULL,
                         metrics = default_metrics("ols"),
                         study_name = "OLS Monte Carlo Simulation",
                         research_question = "How does OLS coefficient recovery vary across sample sizes?") {
  stopifnot(length(n) >= 1L, all(n > 1L))
  stopifnot(length(betas) >= 1L)
  stopifnot(reps >= 1L)
  stopifnot(all(error_sd > 0))
  stopifnot(alpha > 0, alpha < 1)
  stopifnot(all(predictor_correlation > -1), all(predictor_correlation < 1))
  if (length(betas) > 1L && any(predictor_correlation <= -1 / (length(betas) - 1L))) {
    stop(
      "`predictor_correlation` is too negative for an equicorrelation predictor matrix.",
      call. = FALSE
    )
  }
  if (is.null(fitted_formula)) {
    fitted_formula <- paste("y ~", paste(paste0("x", seq_along(betas)), collapse = " + "))
  }

  spec <- list(
    type = "ols",
    study_name = study_name,
    research_question = research_question,
    n = as.integer(n),
    reps = as.integer(reps),
    betas = as.numeric(betas),
    intercept = as.numeric(intercept),
    predictor_correlation = as.numeric(predictor_correlation),
    error_sd = as.numeric(error_sd),
    alpha = as.numeric(alpha),
    seed = as.integer(seed),
    fitted_formula = as.character(fitted_formula),
    metrics = as.character(metrics),
    created_at = as.character(Sys.time())
  )

  class(spec) <- c("mcsimr_ols_spec", "mcsimr_spec", "list")
  spec
}

condition_grid <- function(spec) {
  grid <- expand.grid(
    n = spec$n,
    predictor_correlation = spec$predictor_correlation,
    error_sd = spec$error_sd,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  grid$condition_id <- seq_len(nrow(grid))
  grid[c("condition_id", "n", "predictor_correlation", "error_sd")]
}

validate_ols_spec <- function(spec) {
  if (!inherits(spec, "mcsimr_ols_spec") && !identical(spec$type, "ols")) {
    stop("`spec` must be created by ols_sim_spec().", call. = FALSE)
  }
  invisible(spec)
}

sem_sim_spec <- function(population_model,
                         fitted_model = population_model,
                         n = c(100, 250, 500),
                         reps = 1000,
                         estimator = "ML",
                         parameter_conditions = NULL,
                         missing_rate = 0,
                         missing_mechanism = "mcar",
                         missing_targets = NULL,
                         missing_driver = NULL,
                         missing_slope = 1,
                         skewness = 0,
                         kurtosis = 0,
                         missing = "listwise",
                         std_lv = TRUE,
                         alpha = 0.05,
                         seed = 20260608,
                         metrics = default_metrics("sem"),
                         study_name = "lavaan Monte Carlo Simulation",
                         research_question = "How does SEM parameter recovery vary across sample sizes?") {
  stopifnot(length(population_model) == 1L, nzchar(population_model))
  stopifnot(length(fitted_model) == 1L, nzchar(fitted_model))
  stopifnot(length(n) >= 1L, all(n > 1L))
  stopifnot(reps >= 1L)
  stopifnot(alpha > 0, alpha < 1)
  stopifnot(length(missing_rate) >= 1L, all(missing_rate >= 0), all(missing_rate < 1))
  stopifnot(length(missing_mechanism) >= 1L)
  missing_mechanism <- match.arg(
    tolower(missing_mechanism),
    choices = c("none", "mcar", "mar", "mnar"),
    several.ok = TRUE
  )
  stopifnot(length(skewness) >= 1L)
  stopifnot(length(kurtosis) >= 1L)
  parameter_conditions <- normalize_sem_parameter_conditions(parameter_conditions)

  spec <- list(
    type = "sem",
    study_name = study_name,
    research_question = research_question,
    population_model = population_model,
    fitted_model = fitted_model,
    n = as.integer(n),
    reps = as.integer(reps),
    estimator = as.character(estimator),
    parameter_conditions = parameter_conditions,
    missing_rate = as.numeric(missing_rate),
    missing_mechanism = as.character(missing_mechanism),
    missing_targets = if (is.null(missing_targets)) NULL else as.character(missing_targets),
    missing_driver = if (is.null(missing_driver)) NULL else as.character(missing_driver)[1L],
    missing_slope = as.numeric(missing_slope)[1L],
    skewness = as.numeric(skewness),
    kurtosis = as.numeric(kurtosis),
    missing = as.character(missing),
    std_lv = isTRUE(std_lv),
    alpha = as.numeric(alpha),
    seed = as.integer(seed),
    metrics = as.character(metrics),
    equations_latex = sem_model_latex(fitted_model),
    created_at = as.character(Sys.time())
  )

  class(spec) <- c("mcsimr_sem_spec", "mcsimr_spec", "list")
  spec
}

validate_sem_spec <- function(spec) {
  if (!inherits(spec, "mcsimr_sem_spec") && !identical(spec$type, "sem")) {
    stop("`spec` must be created by sem_sim_spec().", call. = FALSE)
  }
  invisible(spec)
}

sem_condition_grid <- function(spec) {
  grid <- expand.grid(
    n = spec$n,
    estimator = spec$estimator,
    missing_rate = spec$missing_rate,
    missing_mechanism = spec$missing_mechanism,
    skewness = spec$skewness,
    kurtosis = spec$kurtosis,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  parameter_conditions <- normalize_sem_parameter_conditions(spec$parameter_conditions)
  if (nrow(parameter_conditions) > 0L) {
    value_grid <- expand.grid(
      stats::setNames(parameter_conditions$values, sem_condition_column_names(parameter_conditions)),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    grid <- cbind(
      grid[rep(seq_len(nrow(grid)), each = nrow(value_grid)), , drop = FALSE],
      value_grid[rep(seq_len(nrow(value_grid)), times = nrow(grid)), , drop = FALSE]
    )
    labels <- parameter_conditions$label
    grid$parameter_conditions <- apply(
      grid[sem_condition_column_names(parameter_conditions)],
      1L,
      function(row) paste(paste0(labels, " = ", row), collapse = "; ")
    )
  } else {
    grid$parameter_conditions <- "none"
  }

  grid$condition_id <- seq_len(nrow(grid))
  grid[c(
    "condition_id", "n", "estimator", "missing_rate", "missing_mechanism",
    "skewness", "kurtosis",
    "parameter_conditions", sem_condition_column_names(parameter_conditions)
  )]
}

spec_equations <- function(spec) {
  if (!is.null(spec$equations_latex)) {
    return(spec$equations_latex)
  }
  if (identical(spec$type, "ols")) {
    return(ols_formula_latex(spec$fitted_formula))
  }
  character()
}

metric_catalog <- function(model = c("ols", "sem")) {
  model <- match.arg(model)
  if (model == "ols") {
    return(data.frame(
      metric = c(
        "mean_estimate", "bias", "relative_bias", "mse", "rmse",
        "coverage", "rejection_rate", "power", "type_i_error",
        "mcse_rejection", "convergence_rate"
      ),
      description = c(
        "Mean parameter estimate across replications.",
        "Average estimate minus the population value.",
        "Bias divided by the population value when the population value is nonzero.",
        "Mean squared error of the estimate.",
        "Square root of MSE.",
        "Proportion of confidence intervals containing the population value.",
        "Proportion of p-values below alpha.",
        "Rejection rate for nonzero population effects.",
        "Rejection rate for zero population effects.",
        "Monte Carlo standard error of the rejection rate.",
        "Proportion of replications that produced analyzable results."
      ),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    metric = c(
      "mean_estimate", "bias", "relative_bias", "mse", "rmse",
      "coverage", "rejection_rate", "power", "type_i_error",
      "mcse_rejection", "convergence_rate", "improper_solution_rate",
      "mean_cfi", "mean_tli", "mean_rmsea", "mean_srmr"
    ),
    description = c(
      "Mean parameter estimate across replications.",
      "Average estimate minus the population value.",
      "Bias divided by the population value when the population value is nonzero.",
      "Mean squared error of the estimate.",
      "Square root of MSE.",
      "Proportion of confidence intervals containing the population value.",
      "Proportion of p-values below alpha.",
      "Rejection rate for nonzero population effects.",
      "Rejection rate for zero population effects.",
      "Monte Carlo standard error of the rejection rate.",
      "Proportion of replications that converged.",
      "Proportion of SEM replications with inadmissible estimates.",
      "Average comparative fit index.",
      "Average Tucker-Lewis index.",
      "Average RMSEA.",
      "Average SRMR."
    ),
    stringsAsFactors = FALSE
  )
}

default_metrics <- function(model = c("ols", "sem")) {
  model <- match.arg(model)
  if (model == "ols") {
    return(c(
      "mean_estimate", "bias", "relative_bias", "mse", "rmse",
      "coverage", "rejection_rate", "power", "type_i_error",
      "mcse_rejection", "convergence_rate"
    ))
  }
  c(
    "mean_estimate", "bias", "relative_bias", "mse", "rmse",
    "coverage", "rejection_rate", "power", "type_i_error",
    "mcse_rejection", "convergence_rate", "improper_solution_rate",
    "mean_cfi", "mean_tli", "mean_rmsea", "mean_srmr"
  )
}
