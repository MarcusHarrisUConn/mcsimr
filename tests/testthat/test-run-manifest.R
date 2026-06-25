test_that("condition checkpoints write and resume a run manifest", {
  checkpoint_dir <- tempfile("mcsimr-checkpoints")
  spec <- ols_sim_spec(
    n = c(30, 40),
    reps = 2,
    betas = c(0.20, 0.00),
    predictor_correlation = 0,
    error_sd = 1
  )

  first <- run_ols_simulation(spec, workers = 1, checkpoint_dir = checkpoint_dir, resume = TRUE)
  manifest <- read_run_manifest(checkpoint_dir)

  expect_equal(nrow(manifest), 2L)
  expect_true(all(manifest$status == "completed"))
  expect_true(all(manifest$attempts == 1L))
  expect_true(all(manifest$n_rows > 0L))
  expect_true(all(file.exists(manifest$checkpoint_file)))

  second <- run_ols_simulation(spec, workers = 1, checkpoint_dir = checkpoint_dir, resume = TRUE)
  resumed <- read_run_manifest(checkpoint_dir)

  expect_equal(nrow(second), nrow(first))
  expect_true(all(resumed$status == "resumed"))
  expect_true(all(resumed$resumed_from_checkpoint))
  expect_true(all(resumed$n_rows > 0L))
})

test_that("simulation study bundles and writes the run manifest", {
  checkpoint_dir <- tempfile("mcsimr-checkpoints")
  output_dir <- tempfile("mcsimr-output")
  spec <- ols_sim_spec(
    n = 30,
    reps = 2,
    betas = c(0.20, 0.00),
    predictor_correlation = 0,
    error_sd = 1
  )

  study <- run_simulation_study(
    spec,
    workers = 1,
    checkpoint_dir = checkpoint_dir,
    output_dir = output_dir
  )

  expect_s3_class(study, "mcsimr_study")
  expect_true(nrow(study$run_manifest) == 1L)
  expect_true(file.exists(file.path(output_dir, "run-manifest.csv")))
  expect_true(file.exists(run_manifest_path(checkpoint_dir)))
})
