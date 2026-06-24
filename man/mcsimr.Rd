\name{mcsimr}
\alias{mcsimr}
\alias{available_cores}
\alias{apa_metric_table}
\alias{default_metrics}
\alias{export_quarto_project}
\alias{metric_catalog}
\alias{ols_sim_spec}
\alias{plot_metric}
\alias{sem_model_latex}
\alias{sem_model_parameters}
\alias{sem_model_preset}
\alias{sem_model_presets}
\alias{sem_sim_spec}
\alias{run_condition}
\alias{run_ols_simulation}
\alias{run_replication}
\alias{run_sem_simulation}
\alias{run_simulation_study}
\alias{save_metric_plots}
\alias{sem_condition_grid}
\alias{sem_design_summary}
\alias{sem_design_warnings}
\alias{sem_parameter_conditions}
\alias{spec_equations}
\alias{summarize_ols_results}
\alias{summarize_sem_results}
\alias{use_mcsimr_app}
\alias{write_apa_tables}
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

ols_sim_spec(n = c(100, 250, 500), reps = 1000,
  betas = c(0.20, 0.30, 0.00), intercept = 0,
  predictor_correlation = 0, error_sd = 1, alpha = 0.05,
  seed = 20260608, fitted_formula = NULL,
  metrics = default_metrics("ols"), study_name = "OLS Monte Carlo Simulation",
  research_question = "How does OLS coefficient recovery vary across sample sizes?")

sem_sim_spec(population_model, fitted_model = population_model,
  n = c(100, 250, 500), reps = 1000, estimator = "ML",
  parameter_conditions = NULL, missing_rate = 0,
  missing_mechanism = "mcar", missing_targets = NULL, missing_driver = NULL,
  missing_slope = 1, skewness = 0, kurtosis = 0, missing = "listwise",
  std_lv = TRUE, alpha = 0.05, seed = 20260608,
  metrics = default_metrics("sem"),
  study_name = "lavaan Monte Carlo Simulation",
  research_question = "How does SEM parameter recovery vary across sample sizes?")

sem_model_presets()

sem_model_preset(name = "one_factor_cfa")

sem_model_parameters(model, operators = c("=~", "~", "~~", "~1"),
  include_fixed = TRUE)

sem_parameter_conditions(lhs = character(), op = character(),
  rhs = character(), values = list(), label = NULL)

sem_condition_grid(spec)

sem_design_summary(spec)

sem_design_warnings(spec)

sem_model_latex(model)

spec_equations(spec)

run_replication(spec, n, rep_id, condition_id = 1L)

run_condition(spec, condition, workers = 1L)

run_ols_simulation(spec, workers = 1L, checkpoint_dir = NULL, resume = TRUE)

run_sem_simulation(spec, workers = 1L, checkpoint_dir = NULL, resume = TRUE)

run_simulation_study(spec, workers = 1L, checkpoint_dir = NULL,
  resume = TRUE, output_dir = NULL)

summarize_ols_results(results, metrics = default_metrics("ols"))

summarize_sem_results(results, metrics = default_metrics("sem"))

apa_metric_table(summary, metrics = default_metrics("ols"), digits = 3L,
  caption = "Table 1\\nMonte Carlo simulation performance metrics by condition and parameter.")

write_apa_tables(apa_table, path)

plot_metric(summary, metric = "bias", term = NULL, path = NULL,
  width = 900, height = 650)

save_metric_plots(summary, path,
  metrics = intersect(c("bias", "rmse", "coverage", "power", "type_i_error"), names(summary)))

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
\item{population_model}{lavaan syntax used to generate simulated data.}
\item{fitted_model}{lavaan syntax fitted to each simulated data set.}
\item{estimator}{lavaan estimator condition or conditions.}
\item{parameter_conditions}{Optional lavaan parameter values to vary across SEM simulation conditions.}
\item{missing_rate}{Missing-data rate condition or conditions applied after data generation.}
\item{missing_mechanism}{Missing-data mechanism condition or conditions. Supported values are \code{"none"}, \code{"mcar"}, \code{"mar"}, and \code{"mnar"}.}
\item{missing_targets}{Optional observed variable names that should receive missing values. When omitted, targets are chosen from generated data.}
\item{missing_driver}{Optional observed variable used to drive MAR missingness.}
\item{missing_slope}{Logistic slope controlling how strongly the driver or target variable affects MAR/MNAR missingness.}
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
\item{checkpoint_dir}{Directory for checkpointed condition results.}
\item{resume}{Whether completed checkpoint files should be reused.}
\item{output_dir}{Optional directory for raw results, summaries, APA table markdown, plots, and the serialized study bundle.}
\item{results}{Raw simulation result data frame.}
\item{summary}{Simulation metric summary data frame.}
\item{digits}{Number of digits for APA-style numeric display.}
\item{caption}{APA-style table caption.}
\item{apa_table}{APA table object created by \code{apa_metric_table()}.}
\item{metric}{Metric to plot.}
\item{term}{Optional parameter term filter for plots.}
\item{path}{Export destination for a Quarto project.}
\item{width}{PNG width in pixels.}
\item{height}{PNG height in pixels.}
\item{overwrite}{Whether template files may be overwritten.}
\item{...}{Arguments passed to \code{shiny::runApp()}.}
}
\value{
Simulation specifications, model parameter tables, condition summaries, raw
result data frames, summary data frames, or an invisible project path depending
on the function.
}
\examples{
spec <- ols_sim_spec(n = 50, reps = 3, betas = c(0.2, 0.3, 0))
results <- run_ols_simulation(spec, workers = 1)
summarize_ols_results(results)
}
