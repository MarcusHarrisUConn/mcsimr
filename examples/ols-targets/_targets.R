library(targets)
library(mcsimr)

tar_option_set(packages = c("mcsimr", "yaml"))

list(
  tar_target(
    spec,
    ols_sim_spec(
      n = c(100, 250, 500),
      reps = 1000,
      betas = c(0.20, 0.30, 0.00),
      predictor_correlation = 0.30,
      error_sd = 1,
      seed = 20260608
    )
  ),
  tar_target(
    raw_results,
    run_ols_simulation(
      spec,
      workers = 1,
      checkpoint_dir = "results/checkpoints/ols_targets",
      resume = TRUE
    )
  ),
  tar_target(
    summary,
    summarize_ols_results(raw_results)
  )
)
