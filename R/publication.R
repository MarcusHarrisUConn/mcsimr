publication_checklist <- function(spec) {
  if (identical(spec$type, "sem") || inherits(spec, "mcsimr_sem_spec")) {
    validate_sem_spec(spec)
    model_detail <- "Population and fitted lavaan model syntax are stored in the simulation spec."
    condition_detail <- paste(
      "Crossed conditions include sample size, estimator, missingness, nonnormality,",
      "and any SEM parameter conditions."
    )
  } else {
    validate_ols_spec(spec)
    model_detail <- paste("Fitted formula:", spec$fitted_formula)
    condition_detail <- "Crossed conditions include sample size, predictor correlation, and residual SD."
  }

  reps_status <- if (spec$reps >= 100L) "complete" else "review"
  reps_detail <- if (identical(reps_status, "complete")) {
    paste(spec$reps, "replications per condition.")
  } else {
    paste(spec$reps, "replications per condition; use this as a pilot unless justified.")
  }

  data.frame(
    section = c(
      "Research question", "Design", "Design", "Reproducibility",
      "Reproducibility", "Evaluation", "Evaluation", "Reporting", "Reporting"
    ),
    item = c(
      "State the substantive or methodological question.",
      "Document the data-generating and fitted models.",
      "Justify all manipulated conditions and factor levels.",
      "Record the seed, software versions, and package versions.",
      "Archive raw results, summaries, checkpoints, and code.",
      "Define performance metrics before interpreting results.",
      "State the interpretation plan before reviewing results.",
      "Report convergence, failures, and inadmissible estimates.",
      "Tie conclusions to the research question and design limits."
    ),
    status = c(
      if (nzchar(spec$research_question)) "complete" else "review",
      "complete",
      if (!is.null(spec$design_rationale) && nzchar(spec$design_rationale)) "complete" else "review",
      if (!is.null(spec$seed) && is.finite(spec$seed)) "complete" else "review",
      "complete",
      if (length(spec$metrics) && !is.null(spec$metric_rationale) && nzchar(spec$metric_rationale)) "complete" else "review",
      if (!is.null(spec$interpretation_plan) && nzchar(spec$interpretation_plan)) "complete" else "review",
      "complete",
      "review"
    ),
    detail = c(
      spec$research_question,
      model_detail,
      paste(condition_detail, spec$design_rationale),
      paste("Base random seed:", spec$seed),
      "Exported studies write raw results, metric summaries, manifests, methods text, tables, figures, and reproducibility metadata.",
      paste("Requested metrics:", paste(spec$metrics, collapse = ", "), "|", spec$metric_rationale),
      spec$interpretation_plan,
      "Use simulation_diagnostics() and run manifests to identify fragile conditions.",
      reps_detail
    ),
    stringsAsFactors = FALSE
  )
}

reproducibility_manifest <- function(spec, raw_results = NULL, summary = NULL) {
  if (identical(spec$type, "sem") || inherits(spec, "mcsimr_sem_spec")) {
    validate_sem_spec(spec)
  } else {
    validate_ols_spec(spec)
  }

  package_names <- unique(c(
    "mcsimr",
    "lavaan",
    "future",
    "yaml",
    "shiny",
    "bslib",
    "knitr",
    "rmarkdown",
    "targets",
    "tarchetypes",
    "quarto",
    "testthat"
  ))
  installed <- utils::installed.packages()
  package_versions <- stats::setNames(
    lapply(package_names, function(pkg) {
      if (pkg %in% rownames(installed)) {
        as.character(utils::packageVersion(pkg))
      } else {
        NA_character_
      }
    }),
    package_names
  )

  list(
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    study_name = spec$study_name,
    study_type = spec$type,
    research_question = spec$research_question,
    design_rationale = spec$design_rationale,
    metric_rationale = spec$metric_rationale,
    interpretation_plan = spec$interpretation_plan,
    seed = spec$seed,
    spec_checksum = object_checksum(spec),
    raw_results_rows = if (is.null(raw_results)) NA_integer_ else nrow(raw_results),
    summary_rows = if (is.null(summary)) NA_integer_ else nrow(summary),
    r = list(
      version = paste(R.version$major, R.version$minor, sep = "."),
      platform = R.version$platform,
      os = paste(Sys.info()[["sysname"]], Sys.info()[["release"]])
    ),
    packages = package_versions,
    session_info = utils::capture.output(utils::sessionInfo())
  )
}

write_reproducibility_manifest <- function(manifest, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(manifest, path)
  invisible(path)
}

simulation_diagnostics <- function(results, summary, spec = NULL) {
  rows <- list()
  add <- function(severity, check, value, message) {
    rows[[length(rows) + 1L]] <<- data.frame(
      severity = severity,
      check = check,
      value = as.character(value),
      message = message,
      stringsAsFactors = FALSE
    )
  }

  if (is.null(results) || !nrow(results)) {
    add("error", "results", 0L, "No simulation results were available.")
    return(do.call(rbind, rows))
  }

  failed <- if ("error" %in% names(results)) sum(!is.na(results$error) & nzchar(results$error)) else 0L
  add(
    if (failed > 0L) "warning" else "ok",
    "failed replications",
    failed,
    if (failed > 0L) "Some replications returned errors; inspect raw results and run manifests." else "No replication errors were recorded."
  )

  if ("convergence_rate" %in% names(summary)) {
    min_convergence <- suppressWarnings(min(summary$convergence_rate, na.rm = TRUE))
    add(
      if (is.finite(min_convergence) && min_convergence >= 0.95) "ok" else "warning",
      "minimum convergence",
      round(min_convergence, 3),
      "Conditions with convergence below .95 usually need explanation or redesign."
    )
  }

  if ("coverage" %in% names(summary)) {
    nominal <- if (!is.null(spec) && !is.null(spec$alpha)) 1 - spec$alpha else 0.95
    max_deviation <- suppressWarnings(max(abs(summary$coverage - nominal), na.rm = TRUE))
    add(
      if (is.finite(max_deviation) && max_deviation <= 0.05) "ok" else "review",
      "coverage deviation",
      round(max_deviation, 3),
      paste("Largest absolute deviation from nominal coverage", nominal, ".")
    )
  }

  if ("mcse_rejection" %in% names(summary)) {
    max_mcse <- suppressWarnings(max(summary$mcse_rejection, na.rm = TRUE))
    add(
      if (is.finite(max_mcse) && max_mcse <= 0.025) "ok" else "review",
      "Monte Carlo standard error",
      round(max_mcse, 3),
      "Large MCSE suggests increasing replications for publication-grade estimates."
    )
  }

  if ("reps" %in% names(summary)) {
    min_reps <- suppressWarnings(min(summary$reps, na.rm = TRUE))
    add(
      if (is.finite(min_reps) && min_reps >= 100L) "ok" else "review",
      "replications",
      min_reps,
      "Fewer than 100 analyzed replications is best treated as a smoke test or pilot."
    )
  }

  if ("improper_solution_rate" %in% names(summary)) {
    max_improper <- suppressWarnings(max(summary$improper_solution_rate, na.rm = TRUE))
    add(
      if (is.finite(max_improper) && max_improper == 0) "ok" else "warning",
      "improper solutions",
      round(max_improper, 3),
      "SEM inadmissible solutions should be reported and interpreted by condition."
    )
  }

  missingness <- missingness_diagnostics(results)
  if (nrow(missingness)) {
    calibration_rate <- if ("mean_observed_target_missing_rate" %in% names(missingness)) {
      missingness$mean_observed_target_missing_rate
    } else {
      missingness$mean_observed_missing_rate
    }
    max_missing_deviation <- suppressWarnings(max(abs(
      calibration_rate - missingness$requested_missing_rate
    ), na.rm = TRUE))
    add(
      if (is.finite(max_missing_deviation) && max_missing_deviation <= 0.05) "ok" else "review",
      "missingness calibration",
      round(max_missing_deviation, 3),
      "Observed missingness should be checked against the requested missing-data rate by condition."
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

publication_recommendations <- function(diagnostics, checklist = NULL) {
  rows <- list()
  add <- function(priority, area, recommendation, rationale) {
    rows[[length(rows) + 1L]] <<- data.frame(
      priority = priority,
      area = area,
      recommendation = recommendation,
      rationale = rationale,
      stringsAsFactors = FALSE
    )
  }

  if (is.null(diagnostics) || !nrow(diagnostics)) {
    add("high", "Diagnostics", "Run the simulation before interpreting results.", "No diagnostic rows were available.")
  } else {
    flagged <- diagnostics[diagnostics$severity %in% c("review", "warning", "error"), , drop = FALSE]
    if (nrow(flagged)) {
      for (i in seq_len(nrow(flagged))) {
        priority <- switch(
          flagged$severity[[i]],
          error = "high",
          warning = "high",
          review = "medium",
          "medium"
        )
        add(
          priority,
          flagged$check[[i]],
          recommendation_for_check(flagged$check[[i]]),
          flagged$message[[i]]
        )
      }
    } else {
      add("low", "Diagnostics", "Document that automated diagnostics did not flag the pilot run.", "No review, warning, or error diagnostics were detected.")
    }
  }

  if (!is.null(checklist) && nrow(checklist)) {
    needs_review <- checklist[checklist$status %in% c("review", "missing"), , drop = FALSE]
    for (i in seq_len(nrow(needs_review))) {
      add(
        "medium",
        needs_review$section[[i]],
        needs_review$item[[i]],
        needs_review$detail[[i]]
      )
    }
  }

  out <- do.call(rbind, rows)
  out$priority <- factor(out$priority, levels = c("high", "medium", "low"), ordered = TRUE)
  out <- out[order(out$priority, out$area), , drop = FALSE]
  out$priority <- as.character(out$priority)
  rownames(out) <- NULL
  out
}

publication_summary_text <- function(spec, diagnostics, checklist = NULL, recommendations = NULL) {
  if (identical(spec$type, "sem") || inherits(spec, "mcsimr_sem_spec")) {
    validate_sem_spec(spec)
    model_family <- "structural equation model"
  } else {
    validate_ols_spec(spec)
    model_family <- "ordinary least squares regression model"
  }
  if (is.null(recommendations)) {
    recommendations <- publication_recommendations(diagnostics, checklist)
  }
  decision <- readiness_decision(
    simulation_readiness(spec),
    diagnostics = diagnostics,
    checklist = checklist
  )

  severity_counts <- if (!is.null(diagnostics) && nrow(diagnostics)) {
    counts <- table(diagnostics$severity, useNA = "ifany")
    paste(paste(names(counts), as.integer(counts), sep = ": "), collapse = "; ")
  } else {
    "no diagnostics available"
  }
  top_recommendations <- if (nrow(recommendations)) {
    paste(utils::head(recommendations$recommendation, 3L), collapse = " ")
  } else {
    "No additional recommendations were generated."
  }

  paste(
    paste0(
      "This simulation study evaluated a ", model_family, " design for the research question: ",
      spec$research_question
    ),
    paste0(
      "The design used ", length(spec$n), " sample-size level(s), ",
      spec$reps, " replication(s) per condition, alpha = ", spec$alpha,
      ", and seed = ", spec$seed, "."
    ),
    paste0("Design rationale: ", spec$design_rationale),
    paste0("Metric rationale: ", spec$metric_rationale),
    paste0("Interpretation plan: ", spec$interpretation_plan),
    paste0(
      "Readiness decision: ", decision$decision_label[[1L]], ". ",
      decision$message[[1L]], " Next step: ", decision$next_step[[1L]]
    ),
    paste0("Automated publication diagnostics were summarized as: ", severity_counts, "."),
    paste0("Recommended next steps: ", top_recommendations),
    sep = "\n\n"
  )
}

write_publication_summary <- function(text, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(text, path)
  invisible(path)
}

recommendation_for_check <- function(check) {
  switch(
    check,
    "failed replications" = "Inspect failed conditions, retry checkpointed failures, and report any persistent failure pattern.",
    "minimum convergence" = "Increase sample size, simplify fragile model features, or report convergence problems by condition.",
    "coverage deviation" = "Review confidence interval behavior and explain conditions with poor coverage.",
    "Monte Carlo standard error" = "Increase replications until Monte Carlo error is small enough for the intended claim.",
    "replications" = "Treat the current run as a pilot or increase replications for publication-grade estimates.",
    "improper solutions" = "Report inadmissible SEM solutions and consider design changes that reduce improper estimates.",
    "results" = "Run the simulation and verify that result files were created.",
    "Review this diagnostic before treating the simulation as publication-ready."
  )
}

object_checksum <- function(x) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(x, path)
  unname(tools::md5sum(path))
}
