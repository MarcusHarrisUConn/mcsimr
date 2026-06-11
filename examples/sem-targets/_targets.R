library(targets)
library(mcsimr)

tar_option_set(
  packages = c("mcsimr", "lavaan")
)

population_model <- "
f =~ 0.70*y1 + 0.80*y2 + 0.90*y3
f ~~ 1*f
y1 ~~ 0.51*y1
y2 ~~ 0.36*y2
y3 ~~ 0.19*y3
"

fitted_model <- "
f =~ y1 + y2 + y3
"

list(
  tar_target(
    parameter_conditions,
    sem_parameter_conditions(
      lhs = c("f", "f"),
      op = c("=~", "~~"),
      rhs = c("y2", "f"),
      values = list(c(0.60, 0.80), c(0.80, 1.00))
    )
  ),
  tar_target(
    spec,
    sem_sim_spec(
      population_model = population_model,
      fitted_model = fitted_model,
      n = c(100, 250, 500),
      reps = 1000,
      estimator = "ML",
      parameter_conditions = parameter_conditions,
      missing = "fiml",
      missing_rate = c(0, 0.10),
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
  tar_target(apa_table, study$apa_tables),
  tar_target(equations_latex, study$equations_latex)
)
