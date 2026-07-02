export_quarto_project <- function(spec,
                                  path,
                                  overwrite = FALSE,
                                  workers = 1L,
                                  checkpoint_dir = "results/checkpoints") {
  if (identical(spec$type, "sem") || inherits(spec, "mcsimr_sem_spec")) {
    validate_sem_spec(spec)
  } else {
    validate_ols_spec(spec)
  }
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
  saveRDS(spec, file.path(path, "spec.rds"))
  writeLines(c(
    "library(mcsimr)",
    "",
    "spec <- readRDS('spec.rds')",
    "dir.create('results', showWarnings = FALSE)",
    sprintf(
      "study <- run_simulation_study(spec, workers = %d, checkpoint_dir = '%s', output_dir = 'results')",
      workers,
      checkpoint_dir
    ),
    "results <- study$raw_results",
    "summary <- study$summary",
    "apa_table <- study$apa_tables",
    "run_manifest <- study$run_manifest",
    "failure_summary <- study$failure_summary",
    "runtime_estimate <- study$runtime_estimate",
    "diagnostics <- study$diagnostics",
    "reporting_checklist <- study$reporting_checklist",
    "reproducibility <- study$reproducibility",
    "publication_recommendations <- study$publication_recommendations",
    "publication_summary <- study$publication_summary",
    "methods_text <- study$methods_text",
    "equations_latex <- study$equations_latex",
    "writeLines(equations_latex, 'results/model-equations.tex')",
    "writeLines(apa_table$markdown, 'results/apa-table.md')",
    "write_apa_html(apa_table, 'results/apa-table.html')",
    "write_apa_word(apa_table, 'results/apa-table.doc')",
    "writeLines(methods_text, 'results/methods-text.md')",
    "if (nrow(run_manifest)) utils::write.csv(run_manifest, 'results/run-manifest.csv', row.names = FALSE)",
    "if (nrow(failure_summary)) utils::write.csv(failure_summary, 'results/failure-summary.csv', row.names = FALSE)",
    "if (nrow(runtime_estimate)) utils::write.csv(runtime_estimate, 'results/runtime-estimate.csv', row.names = FALSE)",
    "utils::write.csv(diagnostics, 'results/diagnostics.csv', row.names = FALSE)",
    "utils::write.csv(reporting_checklist, 'results/reporting-checklist.csv', row.names = FALSE)",
    "utils::write.csv(publication_recommendations, 'results/publication-recommendations.csv', row.names = FALSE)",
    "write_reproducibility_manifest(reproducibility, 'results/reproducibility.yml')",
    "write_publication_summary(publication_summary, 'results/publication-summary.md')",
    "save_publication_plots(summary, diagnostics, 'results/publication-figures')"
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
