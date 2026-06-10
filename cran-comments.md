## R CMD check results

Current development checks are run with:

```r
R CMD check --no-manual --no-vignettes mcsimr_0.0.0.9000.tar.gz
```

Before CRAN submission, run the full CRAN-facing check matrix:

```r
devtools::check(remote = TRUE, manual = TRUE, incoming = TRUE)
rhub::rhub_check()
urlchecker::url_check()
spelling::spell_check_package()
```

## Current status

This is a development version and has not yet been submitted to CRAN.

Known pre-submission items:

- Replace the placeholder maintainer email in `DESCRIPTION` with the final CRAN maintainer email.
- Confirm all examples run quickly enough for CRAN.
- Add unit tests for SEM parameter conditions, checkpoint resume behavior, and Quarto export.
- Run checks on Windows, macOS, and Linux.
- Review exported Shiny app files so CRAN package size remains reasonable.
- Confirm package title, description, and references do not overclaim beyond implemented behavior.

## Downstream dependencies

There are currently no downstream CRAN dependencies.
