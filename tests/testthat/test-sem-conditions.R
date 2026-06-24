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
    missing_mechanism = c("mcar", "mar"),
    skewness = c(0, 1),
    kurtosis = c(0, 2)
  )

  grid <- sem_condition_grid(spec)

  expect_equal(nrow(grid), 128L)
  expect_true(all(c(
    "param_1", "param_2", "missing_rate", "missing_mechanism", "skewness", "kurtosis",
    "parameter_conditions"
  ) %in% names(grid)))
  expect_equal(sort(unique(grid$param_1)), c(0.60, 0.80))
  expect_equal(sort(unique(grid$param_2)), c(0.80, 1.00))
  expect_equal(sort(unique(grid$missing_rate)), c(0, 0.10))
  expect_equal(sort(unique(grid$missing_mechanism)), c("mar", "mcar"))
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

test_that("MAR and MNAR missingness helpers add missing values to requested targets", {
  dat <- data.frame(y1 = rnorm(50), y2 = rnorm(50), y3 = rnorm(50))

  set.seed(1)
  mar <- mcsimr:::apply_sem_missing(
    dat,
    rate = 0.30,
    mechanism = "mar",
    targets = c("y2", "y3"),
    driver = "y1"
  )
  expect_false(anyNA(mar$y1))
  expect_true(anyNA(mar$y2) || anyNA(mar$y3))

  set.seed(1)
  mnar <- mcsimr:::apply_sem_missing(
    dat,
    rate = 0.30,
    mechanism = "mnar",
    targets = "y2"
  )
  expect_true(anyNA(mnar$y2))
  expect_false(anyNA(mnar$y1))
  expect_false(anyNA(mnar$y3))
})

test_that("SEM presets provide runnable model syntax and default parameter conditions", {
  presets <- sem_model_presets()
  expect_true(all(c("name", "title", "description") %in% names(presets)))
  expect_true("bollen_political_democracy" %in% presets$name)

  preset <- sem_model_preset("latent_mediation")
  expect_s3_class(preset, "mcsimr_sem_preset")
  expect_true(nzchar(preset$population_model))
  expect_true(nzchar(preset$fitted_model))
  expect_true(nrow(preset$parameter_conditions) > 0)

  spec <- sem_sim_spec(
    population_model = preset$population_model,
    fitted_model = preset$fitted_model,
    n = 50,
    reps = 1,
    parameter_conditions = preset$parameter_conditions,
    missing_rate = c(0, 0.10),
    missing_mechanism = c("mcar", "mnar")
  )
  grid <- sem_condition_grid(spec)
  expect_true(nrow(grid) > 1)
  expect_true(all(c("missing_mechanism", "parameter_conditions") %in% names(grid)))
})

test_that("SEM design helpers expose parameters, counts, and warnings", {
  preset <- sem_model_preset("one_factor_cfa")
  spec <- sem_sim_spec(
    population_model = preset$population_model,
    fitted_model = preset$fitted_model,
    n = c(80, 160),
    reps = 50,
    parameter_conditions = preset$parameter_conditions,
    missing_rate = c(0, 0.20),
    missing_mechanism = c("mcar", "mar")
  )

  parameters <- sem_model_parameters(preset$population_model)
  expect_true(all(c("lhs", "op", "rhs", "label", "value", "free") %in% names(parameters)))
  expect_true("f =~ y2" %in% parameters$label)

  design <- sem_design_summary(spec)
  expect_true(all(c("factor", "levels", "values") %in% names(design)))
  expect_equal(
    design$levels[design$factor == "Total conditions"],
    nrow(sem_condition_grid(spec))
  )
  expect_equal(
    design$levels[design$factor == "Total model fits"],
    nrow(sem_condition_grid(spec)) * spec$reps
  )

  warnings <- sem_design_warnings(spec)
  expect_true(all(c("level", "message") %in% names(warnings)))
  expect_true(any(grepl("Fewer than 100 replications", warnings$message, fixed = TRUE)))
})
