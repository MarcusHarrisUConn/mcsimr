test_that("SEM parameter conditions expand the condition grid", {
  pc <- sem_parameter_conditions(
    lhs = c("f", "f"),
    op = c("=~", "~~"),
    rhs = c("y2", "f"),
    values = list(c(0.60, 0.80), c(0.80, 1.00))
  )

  spec <- sem_sim_spec(
    population_model = paste(
      "f =~ 0.70*y1 + 0.80*y2 + 0.90*y3",
      "f ~~ 1*f",
      "y1 ~~ 0.51*y1",
      "y2 ~~ 0.36*y2",
      "y3 ~~ 0.19*y3",
      sep = "\n"
    ),
    fitted_model = "f =~ y1 + y2 + y3",
    n = c(50, 100),
    reps = 1,
    parameter_conditions = pc,
    missing_rate = c(0, 0.10),
    skewness = c(0, 1),
    kurtosis = c(0, 2)
  )

  grid <- sem_condition_grid(spec)

  expect_equal(nrow(grid), 64L)
  expect_true(all(c(
    "param_1", "param_2", "missing_rate", "skewness", "kurtosis",
    "parameter_conditions"
  ) %in% names(grid)))
  expect_equal(sort(unique(grid$param_1)), c(0.60, 0.80))
  expect_equal(sort(unique(grid$param_2)), c(0.80, 1.00))
  expect_equal(sort(unique(grid$missing_rate)), c(0, 0.10))
  expect_equal(sort(unique(grid$skewness)), c(0, 1))
  expect_equal(sort(unique(grid$kurtosis)), c(0, 2))
})

test_that("SEM parameter conditions modify population true values", {
  pc <- sem_parameter_conditions(
    lhs = "f",
    op = "=~",
    rhs = "y2",
    values = list(c(0.60))
  )

  spec <- sem_sim_spec(
    population_model = paste(
      "f =~ 0.70*y1 + 0.80*y2 + 0.90*y3",
      "f ~~ 1*f",
      "y1 ~~ 0.51*y1",
      "y2 ~~ 0.36*y2",
      "y3 ~~ 0.19*y3",
      sep = "\n"
    ),
    fitted_model = "f =~ y1 + y2 + y3",
    n = 50,
    reps = 1,
    parameter_conditions = pc
  )

  grid <- sem_condition_grid(spec)
  conditioned <- mcsimr:::apply_sem_parameter_conditions(
    spec$population_model,
    spec$parameter_conditions,
    grid[1, , drop = FALSE]
  )
  truth <- mcsimr:::sem_true_values(conditioned)

  expect_equal(unname(truth[["f|=~|y2"]]), 0.60)
})

test_that("MCAR missingness helper leaves zero-rate data unchanged and can add missing data", {
  dat <- data.frame(y1 = 1:10, y2 = 11:20)

  expect_equal(mcsimr:::apply_mcar_missing(dat, 0), dat)

  set.seed(1)
  with_missing <- mcsimr:::apply_mcar_missing(dat, 0.50)
  expect_true(anyNA(with_missing))
  expect_equal(dim(with_missing), dim(dat))
})
