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
                         study_name = "OLS Monte Carlo Simulation",
                         research_question = "How does OLS coefficient recovery vary across sample sizes?") {
  stopifnot(length(n) >= 1L, all(n > 1L))
  stopifnot(length(betas) >= 1L)
  stopifnot(reps >= 1L)
  stopifnot(error_sd > 0)
  stopifnot(alpha > 0, alpha < 1)
  stopifnot(predictor_correlation > -1, predictor_correlation < 1)
  if (length(betas) > 1L && predictor_correlation <= -1 / (length(betas) - 1L)) {
    stop(
      "`predictor_correlation` is too negative for an equicorrelation predictor matrix.",
      call. = FALSE
    )
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
    created_at = as.character(Sys.time())
  )

  class(spec) <- c("mcsimr_ols_spec", "mcsimr_spec", "list")
  spec
}

condition_grid <- function(spec) {
  data.frame(
    condition_id = seq_along(spec$n),
    n = spec$n,
    stringsAsFactors = FALSE
  )
}

validate_ols_spec <- function(spec) {
  if (!inherits(spec, "mcsimr_ols_spec") && !identical(spec$type, "ols")) {
    stop("`spec` must be created by ols_sim_spec().", call. = FALSE)
  }
  invisible(spec)
}
