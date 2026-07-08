# mcsimr Release Checklist

Use this checklist before tagging a CRAN or public release.

## Preflight

- Confirm `DESCRIPTION`, `NEWS.md`, `README.md`, and `cran-comments.md` describe the same release.
- Confirm the Shiny app starts locally with `use_mcsimr_app(host = "127.0.0.1")`.
- Confirm the Shinylive demo still mirrors the installed app for teaching and export workflows.
- Review exported Quarto projects for `readiness.csv`, `readiness-decision.csv`, `truth-map.csv`, `missingness-diagnostics.csv`, diagnostics, reporting checklist, reproducibility metadata, methods text, tables, and figures.

## Local Checks

On this Mac, expose Quarto's bundled Pandoc before building vignettes:

```sh
export PATH="/Applications/quarto/bin/tools/aarch64:$PATH"
Rscript dev/check-publication-readiness.R
```

If network access is restricted, run the local subset:

```sh
Rscript -e 'testthat::test_local()'
R CMD build .
R CMD check --no-manual mcsimr_0.1.0.tar.gz
Rscript -e 'spelling::spell_check_package()'
```

## Submission

- Run `urlchecker::url_check()` with network access.
- Confirm `R CMD check --as-cran --no-manual mcsimr_0.1.0.tar.gz` has no errors or warnings.
- Confirm the only expected CRAN note is `New submission` for the first release.
- Submit the source tarball through CRAN's incoming submission form.
- Watch CRAN incoming mail and update `cran-comments.md` if a resubmission is needed.
