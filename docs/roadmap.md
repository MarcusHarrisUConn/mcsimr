# mcsimr SEM Workbench Roadmap

This roadmap prioritizes features that make `mcsimr` useful as a general SEM
simulation platform rather than a single-purpose example app.

## 1. Model Design Breadth

- [x] Add reusable SEM presets for common study families: CFA, two-factor CFA,
  latent structural regression, mediation, growth models, and the Bollen
  political democracy example.
- [x] Support direct lavaan syntax editing for advanced users.
- [x] Add an initial visual builder for loadings, residual variances, factor
  covariances, and structural regressions.
- [x] Add an initial parameter catalog so users can choose lavaan parameters to
  vary across conditions.
- [ ] Extend the visual builders for residual variances, factor covariances,
  structural regressions, intercepts, thresholds, and residual covariances.
- [x] Add initial model misspecification presets: omitted residual covariance, omitted
  cross-loading, omitted structural path, equality constraint mismatch, and
  wrong factor structure.
- [x] Add initial multiple-group SEM support with group labels and group-size
  proportions.
- [ ] Add longitudinal templates.

## 2. Condition Design Breadth

- [x] Cross sample size, estimator, missing-data rate, missing-data mechanism,
  nonnormality, and lavaan population parameters.
- [x] Support MCAR, MAR, and MNAR missingness with target variables, driver
  variables, and missingness slopes.
- [x] Add pre-run design summaries and warnings for fragile or large designs.
- [x] Add formal design validation with parse checks, parameter-condition checks,
  missing-data compatibility warnings, and large-run warnings.
- [ ] Add condition labels, condition sets, and saved presets.
- [x] Add expected runtime estimates from pilot runs.

## 3. Long-Run Execution

- [x] Write a run manifest that records queued, running, completed, failed, and
  resumed conditions.
- [x] Validate checkpoint files before reuse.
- [x] Add retry controls for failed conditions.
- [ ] Add background R job support for local desktop runs.
- [x] Add condition sharding for SLURM and other HPC schedulers.

## 4. Reporting And Reproducibility

- [x] Export Quarto projects with the complete spec, session info,
  model equations, raw lavaan syntax, condition grid, raw results, summaries,
  APA tables, and figures.
- [x] Add run manifests to Quarto exports.
- [x] Add Word-ready and HTML-ready APA tables.
- [x] Generate methods text from the simulation specification.
- [x] Include warnings and failed-condition summaries in reports.

## 5. Package Quality

- [x] Expand tests for presets, missingness, design summaries, and run manifests.
- [ ] Expand tests for nonnormality, parallel execution,
  checkpoint recovery, app-generated specs, and Quarto exports.
- [ ] Add full pkgdown reference and article site.
- [ ] Keep CRAN metadata, examples, and checks clean.
- [x] Keep the public Shinylive app as a browser-only demonstration while routing
  serious simulations to the local package app or HPC workflows.
