run_replication <- function(spec, n, rep_id, condition_id = 1L) {
  validate_ols_spec(spec)
  if (is.null(spec$current_predictor_correlation)) {
    spec$current_predictor_correlation <- spec$predictor_correlation[1L]
  }
  if (is.null(spec$current_error_sd)) {
    spec$current_error_sd <- spec$error_sd[1L]
  }
  dat <- generate_ols_data(
    n = n,
    betas = spec$betas,
    intercept = spec$intercept,
    predictor_correlation = spec$current_predictor_correlation,
    error_sd = spec$current_error_sd
  )

  fit <- tryCatch(fit_ols_model(dat, spec$fitted_formula), error = identity)
  if (inherits(fit, "error")) {
    return(data.frame(
      condition_id = condition_id,
      n = n,
      predictor_correlation = spec$current_predictor_correlation,
      error_sd = spec$current_error_sd,
      rep_id = rep_id,
      term = NA_character_,
      estimate = NA_real_,
      std_error = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_,
      true_value = NA_real_,
      alpha = spec$alpha,
      converged = FALSE,
      error = conditionMessage(fit),
      stringsAsFactors = FALSE
    ))
  }

  estimates <- extract_ols_estimates(fit, spec$betas, spec$alpha)
  estimates$condition_id <- condition_id
  estimates$n <- n
  estimates$predictor_correlation <- spec$current_predictor_correlation
  estimates$error_sd <- spec$current_error_sd
  estimates$rep_id <- rep_id
  estimates$alpha <- spec$alpha
  estimates$converged <- TRUE
  estimates$error <- NA_character_
  estimates[c("condition_id", "n", "rep_id", "term", "estimate", "std_error",
              "predictor_correlation", "error_sd", "statistic", "p_value",
              "conf_low", "conf_high", "true_value", "alpha", "converged",
              "error")]
}

run_condition <- function(spec, condition, workers = 1L) {
  validate_ols_spec(spec)
  spec$current_predictor_correlation <- condition$predictor_correlation
  spec$current_error_sd <- condition$error_sd
  reps <- seq_len(spec$reps)

  if (workers > 1L) {
    cl <- parallel::makeCluster(workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterSetRNGStream(cl, spec$seed + condition$condition_id)
    parallel::clusterExport(
      cl,
      varlist = c(
        "spec", "condition", "run_replication", "generate_ols_data",
        "fit_ols_model", "extract_ols_estimates", "make_predictor_cov",
        "validate_ols_spec"
      ),
      envir = environment()
    )
    pieces <- parallel::parLapply(
      cl,
      reps,
      function(rep_id) run_replication(spec, condition$n, rep_id, condition$condition_id)
    )
  } else {
    set.seed(spec$seed + condition$condition_id)
    pieces <- lapply(
      reps,
      function(rep_id) run_replication(spec, condition$n, rep_id, condition$condition_id)
    )
  }

  do.call(rbind, pieces)
}

run_ols_simulation <- function(spec,
                               workers = 1L,
                               checkpoint_dir = NULL,
                               resume = TRUE,
                               condition_ids = NULL) {
  validate_ols_spec(spec)
  grid <- subset_condition_grid(condition_grid(spec), condition_ids = condition_ids)

  if (!is.null(checkpoint_dir)) {
    dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
    yaml::write_yaml(spec, file.path(checkpoint_dir, "spec.yml"))
    initialize_run_manifest(grid, checkpoint_dir)
  }

  results <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    condition <- grid[i, , drop = FALSE]
    checkpoint_file <- if (!is.null(checkpoint_dir)) {
      condition_checkpoint_path(checkpoint_dir, condition$condition_id)
    } else {
      NULL
    }

    if (resume && !is.null(checkpoint_file) && file.exists(checkpoint_file)) {
      checkpoint_result <- tryCatch(
        read_condition_checkpoint(checkpoint_file, condition_id = condition$condition_id),
        error = identity
      )
      if (!inherits(checkpoint_result, "error")) {
        update_run_manifest(
          checkpoint_dir = checkpoint_dir,
          condition_id = condition$condition_id,
          status = "resumed",
          finished_at = current_manifest_time(),
          n_rows = nrow(checkpoint_result),
          error = NA_character_,
          resumed_from_checkpoint = TRUE
        )
        results[[i]] <- checkpoint_result
        next
      }
      update_run_manifest(
        checkpoint_dir = checkpoint_dir,
        condition_id = condition$condition_id,
        status = "queued",
        error = paste("Invalid checkpoint, rerunning:", conditionMessage(checkpoint_result)),
        resumed_from_checkpoint = FALSE
      )
    }

    started <- Sys.time()
    update_run_manifest(
      checkpoint_dir = checkpoint_dir,
      condition_id = condition$condition_id,
      status = "running",
      started_at = format(started, "%Y-%m-%d %H:%M:%S"),
      error = NA_character_,
      resumed_from_checkpoint = FALSE,
      increment_attempt = TRUE
    )
    condition_result <- tryCatch(
      run_condition(spec, condition, workers = workers),
      error = identity
    )
    if (inherits(condition_result, "error")) {
      update_run_manifest(
        checkpoint_dir = checkpoint_dir,
        condition_id = condition$condition_id,
        status = "failed",
        finished_at = current_manifest_time(),
        duration_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
        error = conditionMessage(condition_result),
        resumed_from_checkpoint = FALSE
      )
      stop(conditionMessage(condition_result), call. = FALSE)
    }
    if (!is.null(checkpoint_file)) {
      saveRDS(condition_result, checkpoint_file)
    }
    update_run_manifest(
      checkpoint_dir = checkpoint_dir,
      condition_id = condition$condition_id,
      status = "completed",
      finished_at = current_manifest_time(),
      duration_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
      n_rows = nrow(condition_result),
      error = NA_character_,
      resumed_from_checkpoint = FALSE
    )
    results[[i]] <- condition_result
  }

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  out
}

run_simulation_study <- function(spec,
                                 workers = 1L,
                                 checkpoint_dir = NULL,
                                 resume = TRUE,
                                 output_dir = NULL,
                                 condition_ids = NULL) {
  if (identical(spec$type, "sem") || inherits(spec, "mcsimr_sem_spec")) {
    validate_sem_spec(spec)
    raw <- run_sem_simulation(
      spec = spec,
      workers = workers,
      checkpoint_dir = checkpoint_dir,
      resume = resume,
      condition_ids = condition_ids
    )
    summary <- summarize_sem_results(raw, metrics = spec$metrics)
  } else {
    validate_ols_spec(spec)
    raw <- run_ols_simulation(
      spec = spec,
      workers = workers,
      checkpoint_dir = checkpoint_dir,
      resume = resume,
      condition_ids = condition_ids
    )
    summary <- summarize_ols_results(raw, metrics = spec$metrics)
  }
  apa <- apa_metric_table(summary, metrics = spec$metrics)

  manifest <- if (!is.null(checkpoint_dir)) read_run_manifest(checkpoint_dir) else data.frame()
  failure_summary <- run_failure_summary(manifest)
  runtime_estimate <- runtime_estimate_from_manifest(
    manifest,
    total_conditions = if (is.null(condition_ids)) nrow(simulation_condition_grid(spec)) else length(condition_ids)
  )
  diagnostics <- simulation_diagnostics(raw, summary, spec = spec)
  reporting_checklist <- publication_checklist(spec)
  readiness <- simulation_readiness(spec)
  readiness_summary <- readiness_decision(readiness, diagnostics, reporting_checklist)
  reproducibility <- reproducibility_manifest(spec, raw_results = raw, summary = summary)
  publication_recommendations_tbl <- publication_recommendations(diagnostics, reporting_checklist)
  publication_summary <- publication_summary_text(
    spec,
    diagnostics,
    reporting_checklist,
    publication_recommendations_tbl
  )

  bundle <- list(
    spec = spec,
    equations_latex = spec_equations(spec),
    raw_results = raw,
    summary = summary,
    apa_tables = apa,
    run_manifest = manifest,
    failure_summary = failure_summary,
    runtime_estimate = runtime_estimate,
    diagnostics = diagnostics,
    reporting_checklist = reporting_checklist,
    readiness = readiness,
    readiness_decision = readiness_summary,
    reproducibility = reproducibility,
    publication_recommendations = publication_recommendations_tbl,
    publication_summary = publication_summary,
    methods_text = simulation_methods_text(spec),
    metric_catalog = metric_catalog(spec$type),
    created_at = as.character(Sys.time())
  )
  class(bundle) <- c("mcsimr_study", "list")

  if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(bundle, file.path(output_dir, "simulation-study.rds"))
    utils::write.csv(raw, file.path(output_dir, "raw-results.csv"), row.names = FALSE)
    utils::write.csv(summary, file.path(output_dir, "metric-summary.csv"), row.names = FALSE)
    if (nrow(bundle$run_manifest)) {
      utils::write.csv(bundle$run_manifest, file.path(output_dir, "run-manifest.csv"), row.names = FALSE)
    }
    if (nrow(bundle$failure_summary)) {
      utils::write.csv(bundle$failure_summary, file.path(output_dir, "failure-summary.csv"), row.names = FALSE)
    }
    if (nrow(bundle$runtime_estimate)) {
      utils::write.csv(bundle$runtime_estimate, file.path(output_dir, "runtime-estimate.csv"), row.names = FALSE)
    }
    utils::write.csv(diagnostics, file.path(output_dir, "diagnostics.csv"), row.names = FALSE)
    utils::write.csv(reporting_checklist, file.path(output_dir, "reporting-checklist.csv"), row.names = FALSE)
    utils::write.csv(readiness, file.path(output_dir, "readiness.csv"), row.names = FALSE)
    utils::write.csv(readiness_summary, file.path(output_dir, "readiness-decision.csv"), row.names = FALSE)
    utils::write.csv(publication_recommendations_tbl, file.path(output_dir, "publication-recommendations.csv"), row.names = FALSE)
    write_reproducibility_manifest(reproducibility, file.path(output_dir, "reproducibility.yml"))
    write_publication_summary(publication_summary, file.path(output_dir, "publication-summary.md"))
    write_apa_tables(apa, file.path(output_dir, "apa-tables.md"))
    write_apa_html(apa, file.path(output_dir, "apa-tables.html"))
    write_apa_word(apa, file.path(output_dir, "apa-tables.doc"))
    writeLines(bundle$methods_text, file.path(output_dir, "methods-text.md"))
    save_metric_plots(summary, file.path(output_dir, "figures"))
    save_publication_plots(summary, diagnostics, file.path(output_dir, "publication-figures"))
  }

  bundle
}
