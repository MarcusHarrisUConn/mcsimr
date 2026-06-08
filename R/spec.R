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
      "fit_index_summary"
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
      "Condition-level summaries for fit indices such as CFI, TLI, RMSEA, and SRMR."
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
  metric_catalog("sem")$metric
}
