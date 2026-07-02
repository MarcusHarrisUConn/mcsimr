# mcsimr 0.1.0

## First CRAN submission

- Added a local Shiny workbench for designing, running, and exporting Monte Carlo simulation studies.
- Added OLS simulation specifications with sample size, predictor correlation, residual SD, seed, replication count, checkpointing, parallel workers, and APA-style summaries.
- Added lavaan SEM simulation specifications with population and fitted model syntax, estimator conditions, convergence/improper-solution summaries, fit indices, model-equation LaTeX, and Quarto export.
- Added SEM parameter condition grids through `sem_parameter_conditions()`, allowing users to vary loadings, factor variances, residual variances/covariances, regressions, and intercepts across simulation conditions.
- Added `sem_model_presets()` and `sem_model_preset()` with CFA, structural regression, mediation, growth, and Bollen political democracy starting points.
- Added MCAR, MAR, and MNAR missing-data mechanisms with missing target, driver, and slope controls for SEM simulations.
- Added SEM design-inspection helpers for extracting lavaan parameters, summarizing crossed condition grids, and warning users about large or fragile designs before long runs.
- Added formal design validation with lavaan parse checks, parameter-condition checks, missing-data compatibility warnings, and large-run warnings.
- Added initial SEM misspecification presets for omitted residual covariances, cross-loadings, structural paths, factor covariances, loadings, and factor-structure changes.
- Added initial multiple-group SEM support with generated group variables, group labels, group proportions, and group-tagged parameter summaries.
- Added persistent run manifests for checkpointed simulations, including completed, resumed, running, and failed condition status plus checkpoint validation before reuse.
- Added retry support for failed checkpointed conditions.
- Added deterministic condition sharding for HPC/SLURM array workflows, plus checkpoint collection helpers.
- Added failed-condition summaries and pilot-run runtime estimates from run manifests.
- Added APA HTML and Word-ready table exports.
- Added generated methods text from the simulation specification for Quarto reports.
- Added reproducible Quarto project export with serialized simulation specs, runnable code, APA table markdown/HTML/Word files, generated methods text, model equations, raw results, summaries, and figures.
- Added publication-readiness helpers for reproducibility manifests, reviewer-facing reporting checklists, and automated simulation diagnostics.
- Added recommendation and publication-summary helpers that turn diagnostics into next steps for students.
- Added publication-focused plots, including metric heatmaps and diagnostic severity plots.
- Added a Shinylive browser demo and GitHub Pages deployment.
- Added an explicit lavaan runtime diagnostic for R sessions where CPU-core detection is unavailable.
