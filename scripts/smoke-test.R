source("R/spec.R")
source("R/data-ols.R")
source("R/manifest.R")
source("R/run.R")
source("R/sharding.R")
source("R/summarize.R")

spec <- ols_sim_spec(
  n = c(40, 80),
  reps = 5,
  betas = c(0.20, 0.30, 0.00),
  predictor_correlation = 0.20,
  seed = 101
)

results <- run_ols_simulation(
  spec,
  workers = 1,
  checkpoint_dir = file.path(tempdir(), "mcsimr-smoke"),
  resume = FALSE
)

print(summarize_ols_results(results))
