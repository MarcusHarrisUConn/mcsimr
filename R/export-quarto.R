export_quarto_project <- function(spec,
                                  path,
                                  overwrite = FALSE,
                                  workers = 1L,
                                  checkpoint_dir = "results/checkpoints") {
  validate_ols_spec(spec)
  if (dir.exists(path) && !overwrite) {
    stop("Export path already exists. Use overwrite = TRUE to replace template files.", call. = FALSE)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path, "R"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path, "results"), recursive = TRUE, showWarnings = FALSE)

  template_dir <- system.file("templates", "quarto", package = "mcsimr")
  if (nzchar(template_dir)) {
    file.copy(list.files(template_dir, full.names = TRUE, all.files = FALSE), path, recursive = TRUE, overwrite = TRUE)
  } else {
    writeLines(c("project:", "  type: website"), file.path(path, "_quarto.yml"))
    writeLines(default_index_qmd(), file.path(path, "index.qmd"))
    writeLines(default_simulation_script(), file.path(path, "R", "simulation.R"))
  }

  yaml::write_yaml(unclass(spec), file.path(path, "spec.yml"))
  writeLines(c(
    "library(mcsimr)",
    "",
    "spec <- yaml::read_yaml('spec.yml')",
    "class(spec) <- c('mcsimr_ols_spec', 'mcsimr_spec', 'list')",
    sprintf("results <- run_ols_simulation(spec, workers = %d, checkpoint_dir = '%s')", workers, checkpoint_dir),
    "summary <- summarize_ols_results(results)",
    "dir.create('results', showWarnings = FALSE)",
    "saveRDS(results, 'results/raw_results.rds')",
    "write.csv(summary, 'results/summary.csv', row.names = FALSE)"
  ), file.path(path, "run.R"))

  invisible(normalizePath(path, winslash = "/", mustWork = FALSE))
}

default_index_qmd <- function() {
  c(
    "---",
    "title: 'OLS Monte Carlo Simulation'",
    "format: html",
    "execute:",
    "  echo: true",
    "---",
    "",
    "```{r}",
    "source('run.R')",
    "summary",
    "```"
  )
}

default_simulation_script <- function() {
  c(
    "# Custom helper functions for exported simulations can be placed here.",
    "# The default project uses mcsimr directly."
  )
}

use_mcsimr_app <- function(...) {
  app_dir <- system.file("shiny", package = "mcsimr")
  if (!nzchar(app_dir)) {
    stop("Could not find the installed Shiny app directory.", call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}
