test_that("SEM misspecification presets can change fitted syntax", {
  model <- paste(
    "f1 =~ y1 + y2 + y3",
    "f2 =~ y4 + y5 + y6 + y3",
    "f2 ~ f1",
    "f1 ~~ f2",
    "y1 ~~ y4",
    sep = "\n"
  )

  presets <- sem_misspecification_presets()
  expect_true(all(c("name", "title", "description") %in% names(presets)))

  omitted_path <- apply_sem_misspecification(model, "omit_structural_path")
  expect_false(grepl("f2 ~ f1", omitted_path, fixed = TRUE))

  omitted_cross_loading <- apply_sem_misspecification(model, "omit_cross_loading")
  expect_true(grepl("f1 =~ y1 + y2 + y3", omitted_cross_loading, fixed = TRUE))
  expect_true(grepl("f2 =~ y4 + y5 + y6", omitted_cross_loading, fixed = TRUE))
})

test_that("SEM specs support group labels and design validation", {
  preset <- sem_model_preset("one_factor_cfa")
  spec <- sem_sim_spec(
    population_model = preset$population_model,
    fitted_model = preset$fitted_model,
    n = 60,
    reps = 1,
    group_variable = "arm",
    group_labels = c("control", "treatment"),
    group_proportions = c(.40, .60)
  )

  expect_equal(spec$group_labels, c("control", "treatment"))
  expect_equal(sum(spec$group_proportions), 1)

  validation <- validate_simulation_design(spec)
  expect_true(all(c("level", "check", "message") %in% names(validation)))
  expect_true(any(validation$check == "multiple groups"))
})

test_that("SEM simulations tag parameter rows by group", {
  skip_if_not_installed("lavaan")
  preset <- sem_model_preset("one_factor_cfa")
  spec <- sem_sim_spec(
    population_model = preset$population_model,
    fitted_model = preset$fitted_model,
    n = 40,
    reps = 1,
    group_variable = "arm",
    group_labels = c("control", "treatment")
  )

  result <- run_sem_simulation(spec, workers = 1)
  expect_true("group" %in% names(result))
  expect_true(any(result$group %in% c("control", "treatment"), na.rm = TRUE))
  expect_true(any(grepl("[control]", result$term, fixed = TRUE), na.rm = TRUE))
})

test_that("APA HTML, Word-ready export, and methods text are written", {
  summary <- data.frame(
    model = "OLS",
    n = 30,
    predictor_correlation = 0,
    error_sd = 1,
    term = "x1",
    true_value = 0.2,
    mean_estimate = 0.21,
    bias = 0.01,
    mse = 0.02,
    coverage = 0.95,
    convergence_rate = 1,
    stringsAsFactors = FALSE
  )
  apa <- apa_metric_table(summary)
  html <- tempfile(fileext = ".html")
  word <- tempfile(fileext = ".doc")

  write_apa_html(apa, html)
  write_apa_word(apa, word)

  expect_true(file.exists(html))
  expect_true(file.exists(word))
  expect_true(grepl("<table", paste(readLines(html, warn = FALSE), collapse = "\n"), fixed = TRUE))

  spec <- ols_sim_spec(n = 30, reps = 2, betas = c(.2))
  text <- simulation_methods_text(spec)
  expect_true(grepl("Monte Carlo simulation", text, fixed = TRUE))
  expect_true(grepl("2 replications", text, fixed = TRUE))
})
