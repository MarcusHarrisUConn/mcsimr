test_that("convergence rates count replications rather than parameter rows", {
  ols_results <- data.frame(
    condition_id = c(1, 1, 1),
    n = c(50, 50, 50),
    predictor_correlation = c(0, 0, 0),
    error_sd = c(1, 1, 1),
    rep_id = c(1, 1, 2),
    term = c("x1", "x2", NA),
    estimate = c(0.2, 0.3, NA),
    std_error = c(0.1, 0.1, NA),
    statistic = c(2, 3, NA),
    p_value = c(0.04, 0.01, NA),
    conf_low = c(0.01, 0.11, NA),
    conf_high = c(0.39, 0.49, NA),
    true_value = c(0.2, 0.3, NA),
    alpha = c(0.05, 0.05, 0.05),
    converged = c(TRUE, TRUE, FALSE),
    error = c(NA, NA, "failed"),
    stringsAsFactors = FALSE
  )

  summary <- summarize_ols_results(ols_results)

  expect_equal(unique(summary$convergence_rate), 0.5)
})

test_that("SEM truth maps expose fitted parameters without explicit population truth", {
  truth_map <- sem_truth_map(
    population_model = "f =~ y1 + y2 + y3",
    fitted_model = "f =~ y1 + y2 + y3"
  )

  expect_true(all(c("key", "truth_available", "true_value", "truth_note") %in% names(truth_map)))
  expect_true(any(!truth_map$truth_available))
})

test_that("missingness diagnostics summarize observed missingness by condition", {
  results <- data.frame(
    condition_id = c(1, 1, 2, 2),
    rep_id = c(1, 2, 1, 2),
    missing_rate = c(0.25, 0.25, 0, 0),
    missing_mechanism = c("mcar", "mcar", "none", "none"),
    observed_missing_rate = c(0.20, 0.30, 0, 0),
    stringsAsFactors = FALSE
  )

  diagnostics <- missingness_diagnostics(results)

  expect_equal(nrow(diagnostics), 2L)
  expect_equal(diagnostics$mean_observed_missing_rate[diagnostics$condition_id == 1], 0.25)
  expect_equal(diagnostics$replications[diagnostics$condition_id == 1], 2L)
})

test_that("missingness diagnostics prefer target-cell rates when available", {
  results <- data.frame(
    condition_id = c(1, 1),
    rep_id = c(1, 2),
    missing_rate = c(0.30, 0.30),
    missing_mechanism = c("mcar", "mcar"),
    observed_missing_rate = c(0.10, 0.12),
    observed_target_missing_rate = c(0.28, 0.32),
    stringsAsFactors = FALSE
  )

  diagnostics <- missingness_diagnostics(results)

  expect_equal(diagnostics$mean_observed_missing_rate, 0.11)
  expect_equal(diagnostics$mean_observed_target_missing_rate, 0.30)
})

test_that("OLS simulations are reproducible across worker counts", {
  cl <- tryCatch(parallel::makeCluster(2), error = identity)
  if (inherits(cl, "error")) {
    skip("This R session cannot create a local parallel cluster.")
  }
  parallel::stopCluster(cl)

  spec <- ols_sim_spec(
    n = c(30, 40),
    reps = 3,
    betas = c(0.20, -0.10),
    predictor_correlation = c(0, 0.20),
    error_sd = 1.5,
    seed = 8675309
  )

  one_worker <- run_ols_simulation(spec, workers = 1)
  two_workers <- run_ols_simulation(spec, workers = 2)

  rownames(one_worker) <- NULL
  rownames(two_workers) <- NULL
  expect_equal(two_workers, one_worker, tolerance = 1e-12)
})

test_that("SEM simulations are reproducible across worker counts", {
  skip_if_not(lavaan_runtime_available(), lavaan_runtime_error())
  cl <- tryCatch(parallel::makeCluster(2), error = identity)
  if (inherits(cl, "error")) {
    skip("This R session cannot create a local parallel cluster.")
  }
  parallel::stopCluster(cl)

  model <- paste(
    "f =~ 0.7*y1 + 0.8*y2 + 0.9*y3",
    "f ~~ 1*f",
    "y1 ~~ 0.51*y1",
    "y2 ~~ 0.36*y2",
    "y3 ~~ 0.19*y3",
    sep = "\n"
  )
  spec <- sem_sim_spec(
    model,
    n = 60,
    reps = 2,
    missing_rate = 0.10,
    missing_targets = c("y1", "y2"),
    seed = 2468
  )

  one_worker <- run_sem_simulation(spec, workers = 1)
  two_workers <- run_sem_simulation(spec, workers = 2)

  rownames(one_worker) <- NULL
  rownames(two_workers) <- NULL
  expect_equal(two_workers, one_worker, tolerance = 1e-12, ignore_attr = TRUE)
})
