# mcsimr

<!-- badges: start -->
[![R-CMD-check](https://github.com/MarcusHarrisUConn/mcsimr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MarcusHarrisUConn/mcsimr/actions/workflows/R-CMD-check.yaml)
[![Deploy Shinylive app](https://github.com/MarcusHarrisUConn/mcsimr/actions/workflows/deploy-shinylive.yml/badge.svg)](https://github.com/MarcusHarrisUConn/mcsimr/actions/workflows/deploy-shinylive.yml)
<!-- badges: end -->

`mcsimr` is a local-first R package and Shiny workbench for Monte Carlo
simulation studies in the social and behavioral sciences, with `lavaan`/SEM as
the primary target audience.

The project now starts from `lavaan`: users can define a population model,
define a fitted model, set sample-size and estimator conditions, choose the
number of replications and seed, run across local cores, and export APA-style
tables, figures, model equations, raw LaTeX, and a reproducible Quarto project.
OLS regression remains available as a special case and as a simpler first
engine.

## Live demo

After GitHub Pages finishes deploying, the browser demo will be available at:

<http://marcusharrisphd.com/mcsimr/>

The live demo is powered by Shinylive, so it runs entirely in the browser. It is
meant for small demonstrations and teaching examples. Large simulations should
run locally or on an HPC system using the package engine, checkpoints, and
`targets` templates.

## Why this exists

Monte Carlo simulation studies are powerful, but they can easily become opaque:
undocumented condition choices, hidden data-generating mechanisms, unclear
performance metrics, and results that are hard to reproduce.

`mcsimr` is built around a different contract:

- the simulation design is an explicit R object;
- users specify the model, conditions, seed, replications, and desired metrics;
- each condition can be checkpointed to disk;
- local runs can use multiple workers;
- Shiny choices generate reproducible R code;
- results, APA-style tables, plots, and code can be exported into a Quarto project;
- the design vocabulary follows reviewer-facing Monte Carlo reporting guidance.

## Install locally

```r
# From a local checkout
install.packages("remotes")
remotes::install_local(".")
```

After the repository is public, you can install from GitHub:

```r
install.packages("remotes")
remotes::install_github("MarcusHarrisUConn/mcsimr")
```

## Run a lavaan simulation

```r
library(mcsimr)

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

spec <- sem_sim_spec(
  population_model = population_model,
  fitted_model = fitted_model,
  n = c(100, 250, 500),
  reps = 1000,
  estimator = c("ML"),
  std_lv = TRUE,
  alpha = 0.05,
  seed = 20260608
)

study <- run_simulation_study(
  spec,
  workers = 4,
  checkpoint_dir = "results/checkpoints/sem-demo",
  resume = TRUE,
  output_dir = "results/sem-demo"
)

study$summary
cat(paste(study$equations_latex, collapse = "\n"))
cat(paste(study$apa_tables$markdown, collapse = "\n"))
```

## Run an OLS simulation

```r
library(mcsimr)

spec <- ols_sim_spec(
  n = c(100, 250, 500),
  reps = 1000,
  betas = c(0.20, 0.30, 0.00),
  predictor_correlation = c(0.00, 0.30),
  error_sd = c(1, 2),
  alpha = 0.05,
  seed = 20260608,
  fitted_formula = "y ~ x1 + x2 + x3",
  research_question = "How does OLS coefficient recovery vary across sample sizes?"
)

study <- run_simulation_study(
  spec,
  workers = 4,
  checkpoint_dir = "results/checkpoints/ols-demo",
  resume = TRUE,
  output_dir = "results/ols-demo"
)

study$summary
cat(paste(study$apa_tables$markdown, collapse = "\n"))
```

The summary includes automatically generated parameter-level performance
metrics:

- mean estimate;
- bias;
- relative bias where defined;
- mean squared error;
- RMSE;
- confidence interval coverage;
- rejection rate;
- power for nonzero population effects;
- Type I error for zero population effects;
- Monte Carlo standard error for rejection rates.
- convergence rate;
- improper-solution rate for SEM;
- fit summaries for SEM, including CFI, TLI, RMSEA, and SRMR.

## Launch the local desktop app

```r
library(mcsimr)
use_mcsimr_app()
```

The local app is the right place for larger runs because it uses the package
engine and can write checkpoints to disk. It now uses a tabbed workflow:

- **Model Builder**: build lavaan SEM syntax from latent variables, indicators,
  loadings, covariances, and structural paths, or edit raw lavaan syntax.
- **Results**: inspect simulation summaries and APA-style tables.
- **Visualizations**: plot metrics such as bias, RMSE, coverage, power, Type I
  error, and SEM fit indices.
- **R Code**: copy the fully reproducible code generated from the current setup.
- **Quarto Export**: write a runnable Quarto project with code, tables, figures,
  rendered equations, and raw LaTeX.

For simulations that may run for days or weeks, use checkpoint directories and
rerun with `resume = TRUE`.

## Export a reproducible Quarto project

```r
export_quarto_project(
  spec,
  path = "ols-simulation-report",
  workers = 4,
  checkpoint_dir = "results/checkpoints"
)
```

The exported project contains:

- `_quarto.yml`;
- `index.qmd`;
- `spec.yml`;
- `run.R`;
- an `R/` helper folder;
- a `results/` folder for raw results, metric summaries, APA-style table
  markdown, raw LaTeX model equations, and figures.

The goal is that an app-launched simulation never stays trapped inside the app.
It should become a transparent, rerunnable research artifact.

## `targets` workflow

A starter pipeline lives in [`examples/ols-targets/_targets.R`](examples/ols-targets/_targets.R).

```r
install.packages("targets")
targets::tar_make()
```

This is the preferred direction for serious long-running studies because
`targets` gives dependency tracking, resumability, and a clean bridge to
scheduled HPC jobs.

## Design checklist

The project design follows the Monte Carlo simulation reporting logic described
by McNeish, Lane, and Curran in *The Reviewer's Guide to Quantitative Methods in
the Social Sciences*. A simulation should document:

1. motivating research question;
2. relevant analytic theory and prior findings;
3. hypotheses or performance expectations;
4. why simulation is needed;
5. manipulated conditions and justified levels;
6. dependent variables or performance metrics;
7. data-generating model and population values;
8. exact data-generation procedures and code;
9. fitted model and software details;
10. how performance metrics are analyzed;
11. concise tables and figures;
12. conclusions tied back to the motivating question.

See [`docs/design.md`](docs/design.md) for the platform plan.

## Roadmap

### Phase 1: OLS foundation

- [x] Package scaffold
- [x] OLS simulation specification
- [x] Data generation and model fitting
- [x] Condition-level checkpointing
- [x] Local parallel execution
- [x] Summary metrics
- [x] APA-style metric tables
- [x] Figure export
- [x] Local Shiny app
- [x] Browser demo with Shinylive
- [x] Quarto export template
- [x] `targets` starter workflow

### Phase 2: Better long-run ergonomics

- [ ] Background job launcher from Shiny
- [ ] Progress logs and run registry
- [ ] Safer resume/restart controls
- [ ] Batch-size controls for very large replication counts
- [ ] Condition editor with saved presets
- [ ] More APA table layouts for parameter-level and condition-level summaries
- [ ] R package tests
- [ ] pkgdown documentation site

### Phase 3: SEM/lavaan simulations

- [x] SEM simulation specification
- [x] Population model syntax
- [x] Fitted model syntax
- [x] `lavaan` fit extraction
- [x] convergence and improper-solution summaries
- [x] parameter bias, coverage, power, and fit-index behavior
- [x] model equation LaTeX export
- [x] SEM Quarto report template
- [ ] missing-data conditions
- [ ] model misspecification templates
- [ ] multi-group SEM templates

### Phase 4: HPC-ready execution

- [ ] `targets` plus scheduler examples
- [ ] SLURM template
- [ ] condition sharding
- [ ] checkpoint validation
- [ ] reproducible environment lockfiles

## Development status

This is an early research software scaffold. The OLS path works, but the API is
expected to evolve as the platform grows.

## License

MIT.
