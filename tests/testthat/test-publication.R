test_that("publication helpers create reviewer-facing artifacts", {
  spec <- ols_sim_spec(
    n = 30,
    reps = 2,
    betas = c(0.20, 0.00),
    predictor_correlation = 0,
    error_sd = 1,
    design_rationale = "Short pilot design for testing rationale capture.",
    metric_rationale = "Use core recovery metrics for package tests.",
    interpretation_plan = "Confirm artifacts are generated before interpreting estimates."
  )
  results <- run_ols_simulation(spec, workers = 1)
  summary <- summarize_ols_results(results)

  checklist <- publication_checklist(spec)
  manifest <- reproducibility_manifest(spec, raw_results = results, summary = summary)
  diagnostics <- simulation_diagnostics(results, summary, spec = spec)
  recommendations <- publication_recommendations(diagnostics, checklist)
  summary_text <- publication_summary_text(spec, diagnostics, checklist, recommendations)

  expect_true(all(c("section", "item", "status", "detail") %in% names(checklist)))
  expect_true(any(checklist$status == "review"))
  expect_equal(manifest$study_type, "ols")
  expect_true(nzchar(manifest$spec_checksum))
  expect_equal(manifest$raw_results_rows, nrow(results))
  expect_true(all(c("severity", "check", "value", "message") %in% names(diagnostics)))
  expect_true(any(diagnostics$check == "replications"))
  expect_true(all(c("priority", "area", "recommendation", "rationale") %in% names(recommendations)))
  expect_true(grepl("Recommended next steps", summary_text, fixed = TRUE))
  expect_true(grepl("Design rationale", summary_text, fixed = TRUE))
  expect_true(any(checklist$item == "State the interpretation plan before reviewing results."))

  path <- tempfile(fileext = ".yml")
  write_reproducibility_manifest(manifest, path)
  expect_true(file.exists(path))
  summary_path <- tempfile(fileext = ".md")
  write_publication_summary(summary_text, summary_path)
  expect_true(file.exists(summary_path))

  figure_dir <- tempfile("mcsimr-figures")
  files <- save_publication_plots(summary, diagnostics, figure_dir)
  expect_true(all(file.exists(files)))
})

test_that("simulation study writes publication artifacts", {
  output_dir <- tempfile("mcsimr-publication")
  spec <- ols_sim_spec(
    n = 30,
    reps = 2,
    betas = c(0.20),
    predictor_correlation = 0,
    error_sd = 1
  )

  study <- run_simulation_study(spec, workers = 1, output_dir = output_dir)

  expect_true(nrow(study$diagnostics) > 0)
  expect_true(nrow(study$reporting_checklist) > 0)
  expect_true(is.list(study$reproducibility))
  expect_true(nrow(study$publication_recommendations) > 0)
  expect_true(nzchar(study$publication_summary))
  expect_true(file.exists(file.path(output_dir, "diagnostics.csv")))
  expect_true(file.exists(file.path(output_dir, "reporting-checklist.csv")))
  expect_true(file.exists(file.path(output_dir, "reproducibility.yml")))
  expect_true(file.exists(file.path(output_dir, "publication-recommendations.csv")))
  expect_true(file.exists(file.path(output_dir, "publication-summary.md")))
  expect_true(file.exists(file.path(output_dir, "publication-figures", "diagnostics.png")))
})
