library(targets)
library(mcsimr)

tar_option_set(
  packages = c("mcsimr", "lavaan")
)

list(
  tar_target(
    preset,
    sem_model_preset("latent_mediation")
  ),
  tar_target(
    spec,
    sem_sim_spec(
      population_model = preset$population_model,
      fitted_model = preset$fitted_model,
      n = c(100, 250, 500),
      reps = 1000,
      estimator = "ML",
      parameter_conditions = preset$parameter_conditions,
      missing = "fiml",
      missing_rate = c(0, 0.10),
      missing_mechanism = c("mcar", "mar", "mnar"),
      missing_targets = c("m1", "m2", "m3", "y1", "y2", "y3"),
      missing_driver = "x1",
      skewness = c(0, 1),
      kurtosis = c(0, 2),
      seed = 20260608
    )
  ),
  tar_target(
    study,
    run_simulation_study(
      spec,
      workers = mcsimr::available_cores(),
      checkpoint_dir = "results/checkpoints/sem-targets",
      resume = TRUE,
      output_dir = "results/sem-targets"
    )
  ),
  tar_target(summary, study$summary),
  tar_target(run_manifest, study$run_manifest),
  tar_target(apa_table, study$apa_tables),
  tar_target(equations_latex, study$equations_latex)
)
