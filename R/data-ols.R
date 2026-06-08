make_predictor_cov <- function(p, rho) {
  mat <- matrix(rho, nrow = p, ncol = p)
  diag(mat) <- 1
  mat
}

generate_ols_data <- function(n, betas, intercept, predictor_correlation, error_sd) {
  p <- length(betas)
  sigma <- make_predictor_cov(p, predictor_correlation)
  x_raw <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)
  chol_sigma <- chol(sigma)
  x <- x_raw %*% chol_sigma
  colnames(x) <- paste0("x", seq_len(p))

  y <- as.numeric(intercept + x %*% betas + stats::rnorm(n, sd = error_sd))
  data.frame(y = y, x, check.names = FALSE)
}

fit_ols_model <- function(dat, fitted_formula = NULL) {
  if (is.null(fitted_formula)) {
    predictors <- setdiff(names(dat), "y")
    fitted_formula <- paste("y ~", paste(predictors, collapse = " + "))
  }
  form <- stats::as.formula(fitted_formula)
  stats::lm(form, data = dat)
}

extract_ols_estimates <- function(fit, true_betas, alpha) {
  coef_table <- summary(fit)$coefficients
  ci <- stats::confint(fit, level = 1 - alpha)
  terms <- rownames(coef_table)
  true_values <- c("(Intercept)" = NA_real_, stats::setNames(true_betas, paste0("x", seq_along(true_betas))))

  data.frame(
    term = terms,
    estimate = coef_table[, "Estimate"],
    std_error = coef_table[, "Std. Error"],
    statistic = coef_table[, "t value"],
    p_value = coef_table[, "Pr(>|t|)"],
    conf_low = ci[, 1],
    conf_high = ci[, 2],
    true_value = unname(true_values[terms]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
