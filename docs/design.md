# mcsimr Design Plan

## Product vision

`mcsimr` should become a local desktop platform for Monte Carlo simulation work
that is friendly enough for applied researchers but transparent enough for
methodologists. The Shiny app helps users choose conditions and launch jobs.
The R package remains the source of truth for data generation, model fitting,
summaries, checkpointing, and reproducible exports.

## Architecture

- `R/`: simulation engine, result summaries, and project export helpers.
- `inst/shiny/`: local Shiny desktop app.
- `inst/templates/quarto/`: reproducible report template.
- `examples/ols-targets/`: `targets` workflow starter.
- `docs/`: design notes and methodological guidance.

## Monte Carlo study design checklist

The app and exported Quarto project should ask users to document:

1. The motivating research question.
2. Prior analytic theory or empirical findings motivating the study.
3. Hypotheses or performance expectations.
4. Why simulation is needed instead of an analytic solution.
5. Manipulated independent variables and justified levels.
6. Dependent variables or performance metrics.
7. The data-generating model and population values.
8. Exact data-generation procedures and code.
9. Exact fitted model and software options.
10. How performance metrics are analyzed.
11. Tables and figures for the most important results.
12. Conclusions tied back to the motivating question.

This checklist is adapted from the Monte Carlo Simulation Methods chapter by
McNeish, Lane, and Curran in *The Reviewer's Guide to Quantitative Methods in
the Social Sciences*.

## First module: OLS regression

The first simulation type supports:

- sample size conditions;
- predictor correlation conditions;
- residual variance conditions;
- number of replications;
- true regression coefficients;
- fitted model formula;
- selected output metrics;
- alpha level;
- reproducible RNG seed;
- local parallel execution;
- checkpoint files per condition;
- summary metrics including MSE, bias, relative bias, RMSE, coverage, rejection
  rate, power, Type I error, Monte Carlo standard error, and convergence/errors;
- APA-style tables;
- exported visualizations.

## Execution strategy

Small runs can execute directly from R or Shiny. Longer runs should use a
checkpoint directory where every condition is written independently as an `.rds`
file. This gives a simple resume path: completed condition files are read from
disk instead of rerun.

The initial local backend uses base R `parallel` workers because that works
well on a local Windows desktop without extra dependencies. For HPC use, the
exported `targets` pipeline can be run from a shell script, scheduler job, or
future batchtools plan in a later version.

## Reproducibility strategy

Every Shiny run should be serializable as a simulation specification object.
The Quarto export writes:

- `_quarto.yml`;
- `index.qmd`;
- `R/simulation.R`;
- `spec.yml`;
- `run.R`;
- `results/apa-table.md`;
- `results/figures/`.

The exported project should run outside Shiny and produce the same condition
grid, raw results, metric summary, APA-style tables, and plots.

## Later SEM/lavaan module

The SEM module should reuse the same high-level contract:

- define simulation specification;
- create condition grid;
- generate data from population model syntax;
- fit user-selected `lavaan` model syntax;
- extract convergence, improper solutions, parameter recovery, fit indices,
  coverage, power, and Type I error;
- summarize and export reproducible Quarto/targets projects.
