# CRAN Submission Notes

`mcsimr` is being prepared as a first CRAN submission for reproducible Monte Carlo simulation studies, with SEM/lavaan workflows as the primary use case.

## Submission Status

- Package version: `0.1.0`
- Submission type: first submission
- Expected note: `New submission`
- Downstream dependencies: none

## Release Rationale

The package provides a local-first simulation workflow with:

- explicit simulation specifications;
- lavaan SEM and OLS engines;
- checkpointed and shardable long-running simulations;
- Shiny design and teaching interface;
- reproducible Quarto export;
- APA-style tables and figures;
- publication-readiness diagnostics, reporting checklists, readiness decisions, and reproducibility manifests.

## Known Local Toolchain Notes

Vignettes require Pandoc. On the current macOS development machine, Pandoc is supplied by Quarto at:

```text
/Applications/quarto/bin/tools/aarch64
```

One lavaan-dependent test may skip if `parallel::detectCores()` returns `NA` in the local R session. The package includes a diagnostic for this host/runtime condition.
