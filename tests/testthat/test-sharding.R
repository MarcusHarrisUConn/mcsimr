test_that("condition sharding preserves original condition ids", {
  spec <- ols_sim_spec(
    n = c(30, 40, 50),
    reps = 1,
    betas = c(0.20),
    predictor_correlation = c(0, 0.30),
    error_sd = 1
  )

  shards <- condition_shards(spec, shards = 3)

  expect_equal(nrow(shards), nrow(condition_grid(spec)))
  expect_equal(sort(unique(shards$shard_id)), 1:3)
  expect_equal(sort(shards$condition_id), sort(condition_grid(spec)$condition_id))
})

test_that("simulation studies can run selected conditions", {
  checkpoint_dir <- tempfile("mcsimr-subset")
  spec <- ols_sim_spec(
    n = c(30, 40, 50),
    reps = 1,
    betas = c(0.20),
    predictor_correlation = c(0, 0.30),
    error_sd = 1
  )

  study <- run_simulation_study(
    spec,
    workers = 1,
    checkpoint_dir = checkpoint_dir,
    condition_ids = c(2, 4)
  )

  expect_equal(sort(unique(study$raw_results$condition_id)), c(2L, 4L))
  expect_equal(sort(study$run_manifest$condition_id), c(2L, 4L))
  expect_true(nrow(study$failure_summary) > 0)
  expect_true(nrow(study$runtime_estimate) == 1L)
})

test_that("sharded runs write shard folders and can be collected", {
  root <- tempfile("mcsimr-shards")
  spec <- ols_sim_spec(
    n = c(30, 40),
    reps = 1,
    betas = c(0.20),
    predictor_correlation = c(0, 0.30),
    error_sd = 1
  )

  shard <- run_simulation_shard(
    spec,
    shard_id = 1,
    shards = 2,
    workers = 1,
    checkpoint_dir = file.path(root, "checkpoints"),
    output_dir = file.path(root, "output")
  )

  expect_true(dir.exists(file.path(root, "checkpoints", "shard-001-of-002")))
  collected <- collect_checkpoint_results(file.path(root, "checkpoints"))
  expect_equal(sort(unique(collected$condition_id)), sort(unique(shard$raw_results$condition_id)))
})

test_that("failure and runtime summaries handle manifests", {
  manifest <- data.frame(
    condition_id = 1:3,
    status = c("completed", "failed", "queued"),
    attempts = c(1L, 2L, 0L),
    duration_seconds = c(10, 5, NA),
    stringsAsFactors = FALSE
  )

  failures <- run_failure_summary(manifest)
  estimate <- runtime_estimate_from_manifest(manifest)

  expect_true(all(c("status", "conditions", "attempts") %in% names(failures)))
  expect_equal(estimate$completed_conditions, 1L)
  expect_equal(estimate$failed_conditions, 1L)
  expect_equal(estimate$remaining_conditions, 1L)
})
