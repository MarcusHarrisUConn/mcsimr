# mcsimr 0.0.0.9000

## Development version

- Added a local Shiny workbench for designing, running, and exporting Monte Carlo simulation studies.
- Added OLS simulation specifications with sample size, predictor correlation, residual SD, seed, replication count, checkpointing, parallel workers, and APA-style summaries.
- Added lavaan SEM simulation specifications with population and fitted model syntax, estimator conditions, convergence/improper-solution summaries, fit indices, model-equation LaTeX, and Quarto export.
- Added SEM parameter condition grids through `sem_parameter_conditions()`, allowing users to vary loadings, factor variances, residual variances/covariances, regressions, and intercepts across simulation conditions.
- Added reproducible Quarto project export with serialized simulation specs, runnable code, APA table markdown, model equations, raw results, summaries, and figures.
- Added a Shinylive browser demo and GitHub Pages deployment.
