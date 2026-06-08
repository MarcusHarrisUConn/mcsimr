\name{mcsimr}
\alias{mcsimr}
\alias{available_cores}
\alias{export_quarto_project}
\alias{ols_sim_spec}
\alias{run_condition}
\alias{run_ols_simulation}
\alias{run_replication}
\alias{summarize_ols_results}
\alias{use_mcsimr_app}
\title{Local Monte Carlo simulation workbench}
\description{
Core functions for defining, running, checkpointing, summarizing, and exporting
Monte Carlo simulation studies. The initial implementation supports ordinary
least squares regression simulations.
}
\usage{
available_cores(reserve = 1L)

ols_sim_spec(n = c(100, 250, 500), reps = 1000,
  betas = c(0.20, 0.30, 0.00), intercept = 0,
  predictor_correlation = 0, error_sd = 1, alpha = 0.05,
  seed = 20260608, study_name = "OLS Monte Carlo Simulation",
  research_question = "How does OLS coefficient recovery vary across sample sizes?")

run_replication(spec, n, rep_id, condition_id = 1L)

run_condition(spec, condition, workers = 1L)

run_ols_simulation(spec, workers = 1L, checkpoint_dir = NULL, resume = TRUE)

summarize_ols_results(results)

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
\item{study_name}{Human-readable study name.}
\item{research_question}{Human-readable motivating question.}
\item{spec}{Simulation specification created by \code{ols_sim_spec()}.}
\item{rep_id}{Replication identifier.}
\item{condition_id}{Condition identifier.}
\item{condition}{One-row condition data frame.}
\item{workers}{Number of local parallel workers.}
\item{checkpoint_dir}{Directory for checkpointed condition results.}
\item{resume}{Whether completed checkpoint files should be reused.}
\item{results}{Raw simulation result data frame.}
\item{path}{Export destination for a Quarto project.}
\item{overwrite}{Whether template files may be overwritten.}
\item{...}{Arguments passed to \code{shiny::runApp()}.}
}
\value{
Simulation specifications, raw result data frames, summary data frames, or an
invisible project path depending on the function.
}
\examples{
spec <- ols_sim_spec(n = 50, reps = 3, betas = c(0.2, 0.3, 0))
results <- run_ols_simulation(spec, workers = 1)
summarize_ols_results(results)
}
