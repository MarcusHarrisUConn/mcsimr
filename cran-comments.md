## R CMD check results

Local checks were run on macOS Tahoe 26.5.1, R 4.6.1:

```r
R CMD build .
R CMD check --as-cran --no-manual mcsimr_0.1.0.tar.gz
```

Result:

- 0 errors
- 0 warnings
- 1 note

The local check used pandoc from the installed Quarto distribution:
`/Applications/quarto/bin/tools/aarch64`.

The single note is expected for this first CRAN submission:

```text
New submission
```

Additional release checks:

- `urlchecker::url_check()`: all URLs are correct.
- `spelling::spell_check_package()`: no spelling errors found.

One lavaan-dependent test is skipped on this local machine because
`parallel::detectCores()` returns `NA`, which prevents lavaan 0.6-21 from
initializing. The package reports this condition explicitly during SEM runs.

## Test environments

- local macOS Tahoe 26.5.1, R 4.6.1
- GitHub Actions R-CMD-check on ubuntu-latest

## Downstream dependencies

There are currently no downstream CRAN dependencies.
