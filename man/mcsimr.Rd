\name{mcsimr}
\alias{mcsimr}
\alias{available_cores}
\alias{apa_metric_table}
\alias{collect_checkpoint_results}
\alias{condition_shards}
\alias{default_design_rationale}
\alias{default_interpretation_plan}
\alias{default_metric_rationale}
\alias{default_metrics}
\alias{export_quarto_project}
\alias{metric_catalog}
\alias{missingness_diagnostics}
\alias{ols_sim_spec}
\alias{plot_metric}
\alias{plot_diagnostics}
\alias{plot_metric_heatmap}
\alias{publication_checklist}
\alias{publication_recommendations}
\alias{publication_summary_text}
\alias{read_run_manifest}
\alias{readiness_decision}
\alias{reproducibility_manifest}
\alias{run_manifest_path}
\alias{apply_sem_misspecification}
\alias{sem_model_latex}
\alias{sem_model_parameters}
\alias{sem_model_preset}
\alias{sem_model_presets}
\alias{sem_misspecification_presets}
\alias{sem_sim_spec}
\alias{sem_truth_map}
\alias{run_condition}
\alias{run_ols_simulation}
\alias{run_replication}
\alias{run_sem_simulation}
\alias{run_failure_summary}
\alias{run_simulation_study}
\alias{run_simulation_shard}
\alias{retry_failed_conditions}
\alias{runtime_estimate_from_manifest}
\alias{save_metric_plots}
\alias{save_publication_plots}
\alias{sem_condition_grid}
\alias{sem_design_summary}
\alias{sem_design_warnings}
\alias{sem_parameter_conditions}
\alias{spec_equations}
\alias{summarize_ols_results}
\alias{summarize_sem_results}
\alias{use_mcsimr_app}
\alias{simulation_methods_text}
\alias{simulation_diagnostics}
\alias{simulation_readiness}
\alias{validate_simulation_design}
\alias{write_apa_html}
\alias{write_apa_tables}
\alias{write_apa_word}
\alias{write_publication_summary}
\alias{write_reproducibility_manifest}
\title{Local Monte Carlo simulation workbench}
\description{
Core functions for defining, running, checkpointing, summarizing, and exporting
Monte Carlo simulation studies. The package supports ordinary least squares
regression simulations and structural equation model simulations through
\pkg{lavaan}.
}
\usage{
available_cores(reserve = 1L)

metric_catalog(model = c("ols", "sem"))

default_metrics(model = c("ols", "sem"))

default_design_rationale(model = c("ols", "sem"))

default_metric_rationale(model = c("ols", "sem"))

default_interpretation_plan(model = c("ols", "sem"))

ols_sim_spec(n = c(100, 250, 500), reps = 1000,
  betas = c(0.20, 0.30, 0.00), intercept = 0,
  predictor_correlation = 0, error_sd = 1, alpha = 0.05,
  seed = 20260608, fitted_formula = NULL,
  metrics = default_metrics("ols"), study_name = "OLS Monte Carlo Simulation",
  research_question = "How does OLS coefficient recovery vary across sample sizes?",
  design_rationale = default_design_rationale("ols"),
  metric_rationale = default_metric_rationale("ols"),
  interpretation_plan = default_interpretation_plan("ols"),
  readiness_mode = c("teaching", "publication"))

sem_sim_spec(population_model, fitted_model = population_model,
  n = c(100, 250, 500), reps = 1000, estimator = "ML",
  parameter_conditions = NULL, missing_rate = 0,
  missing_mechanism = "mcar", missing_targets = NULL, missing_driver = NULL,
  missing_slope = 1, misspecification = "none", group_variable = NULL,
  group_labels = NULL, group_proportions = NULL,
  skewness = 0, kurtosis = 0, missing = "listwise",
  std_lv = TRUE, alpha = 0.05, seed = 20260608,
  metrics = default_metrics("sem"),
  study_name = "lavaan Monte Carlo Simulation",
  research_question = "How does SEM parameter recovery vary across sample sizes?",
  design_rationale = default_design_rationale("sem"),
  metric_rationale = default_metric_rationale("sem"),
  interpretation_plan = default_interpretation_plan("sem"),
  readiness_mode = c("teaching", "publication"))

sem_model_presets()

sem_model_preset(name = "one_factor_cfa")

sem_misspecification_presets()

apply_sem_misspecification(fitted_model, preset = "none",
  population_model = fitted_model)

sem_model_parameters(model, operators = c("=~", "~", "~~", "~1"),
  include_fixed = TRUE)

sem_truth_map(population_model, fitted_model = NULL)

run_manifest_path(checkpoint_dir)

read_run_manifest(checkpoint_dir)

sem_parameter_conditions(lhs = character(), op = character(),
  rhs = character(), values = list(), label = NULL)

sem_condition_grid(spec)

sem_design_summary(spec)

sem_design_warnings(spec)

validate_simulation_design(spec)

sem_model_latex(model)

spec_equations(spec)

run_replication(spec, n, rep_id, condition_id = 1L)

run_condition(spec, condition, workers = 1L)

run_ols_simulation(spec, workers = 1L, checkpoint_dir = NULL,
  resume = TRUE, condition_ids = NULL)

run_sem_simulation(spec, workers = 1L, checkpoint_dir = NULL,
  resume = TRUE, condition_ids = NULL)

run_simulation_study(spec, workers = 1L, checkpoint_dir = NULL,
  resume = TRUE, output_dir = NULL, condition_ids = NULL)

condition_shards(spec, shards, strategy = c("round_robin", "contiguous"))

run_simulation_shard(spec, shard_id, shards, workers = 1L,
  checkpoint_dir = NULL, resume = TRUE, output_dir = NULL,
  strategy = c("round_robin", "contiguous"))

collect_checkpoint_results(checkpoint_dir, recursive = TRUE)

retry_failed_conditions(spec, checkpoint_dir, workers = 1L,
  condition_ids = NULL)

run_failure_summary(manifest)

runtime_estimate_from_manifest(manifest, total_conditions = NULL)

summarize_ols_results(results, metrics = default_metrics("ols"))

summarize_sem_results(results, metrics = default_metrics("sem"))

apa_metric_table(summary, metrics = default_metrics("ols"), digits = 3L,
  caption = default_apa_caption())

write_apa_tables(apa_table, path)

write_apa_html(apa_table, path)

write_apa_word(apa_table, path)

simulation_methods_text(spec)

publication_checklist(spec)

publication_recommendations(diagnostics, checklist = NULL)

publication_summary_text(spec, diagnostics, checklist = NULL,
  recommendations = NULL)

write_publication_summary(text, path)

reproducibility_manifest(spec, raw_results = NULL, summary = NULL)

write_reproducibility_manifest(manifest, path)

simulation_diagnostics(results, summary, spec = NULL)

missingness_diagnostics(results)

simulation_readiness(spec, mode = NULL)

readiness_decision(readiness, diagnostics = NULL, checklist = NULL)

plot_metric(summary, metric = "bias", term = NULL, path = NULL,
  width = 900, height = 650)

plot_metric_heatmap(summary, metric = "bias", term = NULL, path = NULL,
  width = 900, height = 650)

plot_diagnostics(diagnostics, path = NULL, width = 900, height = 650)

save_metric_plots(summary, path, metrics = default_plot_metrics(summary))

save_publication_plots(summary, diagnostics, path,
  metrics = default_plot_metrics(summary))

export_quarto_project(spec, path, overwrite = FALSE, workers = 1L,
  checkpoint_dir = "results/checkpoints")

use_mcsimr_app(...)
}
\arguments{
\item{reserve}{Number of CPU cores to reserve for other work.}
\item{n}{Sample size condition or conditions.}
\item{reps}{Number of replications per condition.}
\item{betas}{True regression coefficients for predictors.}
\item{intercept}{Population intercept.}
\item{predictor_correlation}{Common correlation among predictors.}
\item{error_sd}{Residual standard deviation.}
\item{alpha}{Nominal alpha level.}
\item{seed}{Base random seed.}
\item{fitted_formula}{Formula string for the model fitted to each generated data set.}
\item{metrics}{Metrics to include in summaries and APA-style tables.}
\item{study_name}{Human-readable study name.}
\item{research_question}{Human-readable motivating question.}
\item{design_rationale}{Human-readable rationale for selected simulation conditions.}
\item{metric_rationale}{Human-readable rationale for selected performance metrics.}
\item{interpretation_plan}{Human-readable plan for interpreting results before inspecting them.}
\item{readiness_mode}{Readiness standard used for design checks. Use \code{"teaching"} for pilots and classroom use or \code{"publication"} for stricter manuscript-oriented review.}
\item{population_model}{lavaan syntax used to generate simulated data.}
\item{fitted_model}{lavaan syntax fitted to each simulated data set.}
\item{estimator}{lavaan estimator condition or conditions.}
\item{parameter_conditions}{Optional lavaan parameter values to vary across SEM simulation conditions.}
\item{checkpoint_dir}{Directory for checkpointed condition results and the run manifest.}
\item{missing_rate}{Missing-data rate condition or conditions applied after data generation.}
\item{missing_mechanism}{Missing-data mechanism condition or conditions. Supported values are \code{"none"}, \code{"mcar"}, \code{"mar"}, and \code{"mnar"}.}
\item{missing_targets}{Optional observed variable names that should receive missing values. When omitted, targets are chosen from generated data.}
\item{missing_driver}{Optional observed variable used to drive MAR missingness.}
\item{missing_slope}{Logistic slope controlling how strongly the driver or target variable affects MAR/MNAR missingness.}
\item{misspecification}{Fitted-model misspecification preset. Use \code{sem_misspecification_presets()} to list supported presets.}
\item{group_variable}{Optional grouping variable name to add to simulated SEM data and pass to \code{lavaan::sem()}.}
\item{group_labels}{Optional group labels for multiple-group SEM simulation.}
\item{group_proportions}{Optional positive group proportions; values are normalized to sum to one.}
\item{skewness}{Observed-variable skewness condition or conditions passed to \code{lavaan::simulateData()}.}
\item{kurtosis}{Observed-variable excess kurtosis condition or conditions passed to \code{lavaan::simulateData()}.}
\item{name}{SEM preset name. Use \code{sem_model_presets()} to list available presets.}
\item{operators}{lavaan operators to include when extracting model parameters.}
\item{include_fixed}{Whether fixed population parameters should be returned by \code{sem_model_parameters()}.}
\item{lhs}{Left-hand side parameter names for lavaan parameter conditions.}
\item{op}{lavaan operator for each parameter condition, such as \code{=~}, \code{~}, or \code{~~}.}
\item{rhs}{Right-hand side parameter names for lavaan parameter conditions.}
\item{values}{List of numeric values to cross across simulation conditions.}
\item{label}{Optional display labels for parameter conditions.}
\item{missing}{lavaan missing-data handling option.}
\item{std_lv}{Whether lavaan should identify latent variables with \code{std.lv = TRUE}.}
\item{model}{lavaan model syntax for equation conversion, or simulation model family for \code{metric_catalog()}.}
\item{spec}{Simulation specification created by \code{ols_sim_spec()} or \code{sem_sim_spec()}.}
\item{rep_id}{Replication identifier.}
\item{condition_id}{Condition identifier.}
\item{condition}{One-row condition data frame.}
\item{workers}{Number of local parallel workers.}
\item{resume}{Whether completed checkpoint files should be reused.}
\item{output_dir}{Optional directory for raw results, summaries, APA table markdown, plots, and the serialized study bundle.}
\item{results}{Raw simulation result data frame.}
\item{summary}{Simulation metric summary data frame.}
\item{digits}{Number of digits for APA-style numeric display.}
\item{caption}{APA-style table caption.}
\item{apa_table}{APA table object created by \code{apa_metric_table()}.}
\item{raw_results}{Optional raw simulation results for reproducibility metadata.}
\item{manifest}{Reproducibility manifest list or run manifest data frame.}
\item{diagnostics}{Diagnostics data frame created by \code{simulation_diagnostics()}.}
\item{checklist}{Optional checklist data frame created by \code{publication_checklist()}.}
\item{readiness}{Readiness review data frame created by \code{simulation_readiness()}.}
\item{recommendations}{Optional recommendations data frame created by \code{publication_recommendations()}.}
\item{text}{Publication summary text.}
\item{mode}{Optional readiness mode, either \code{"teaching"} or \code{"publication"}.}
\item{metric}{Metric to plot.}
\item{term}{Optional parameter term filter for plots.}
\item{path}{Export destination for a Quarto project, plot, table, or manifest.}
\item{preset}{Misspecification preset name.}
\item{condition_ids}{Optional condition identifiers to run or retry.}
\item{shards}{Number of shards used to split the condition grid.}
\item{shard_id}{One-based shard identifier to run.}
\item{strategy}{Condition-assignment strategy for sharding.}
\item{recursive}{Whether checkpoint collection should search nested shard directories.}
\item{total_conditions}{Optional total number of conditions for runtime estimation.}
\item{width}{PNG width in pixels.}
\item{height}{PNG height in pixels.}
\item{overwrite}{Whether template files may be overwritten.}
\item{...}{Arguments passed to \code{shiny::runApp()}.}
}
\value{
Simulation specifications, model parameter tables, condition summaries, run
manifest data frames, raw result data frames, summary data frames, or an
invisible project path depending on the function.
}
\examples{
spec <- ols_sim_spec(n = 50, reps = 3, betas = c(0.2, 0.3, 0))
results <- run_ols_simulation(spec, workers = 1)
summarize_ols_results(results)
}
