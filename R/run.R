run_replication <- function(spec, n, rep_id, condition_id = 1L) {
  validate_ols_spec(spec)
  dat <- generate_ols_data(
    n = n,
    betas = spec$betas,
    intercept = spec$intercept,
    predictor_correlation = spec$predictor_correlation,
    error_sd = spec$error_sd
  )

  fit <- tryCatch(fit_ols_model(dat), error = identity)
  if (inherits(fit, "error")) {
    return(data.frame(
      condition_id = condition_id,
      n = n,
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
  estimates$rep_id <- rep_id
  estimates$alpha <- spec$alpha
  estimates$converged <- TRUE
  estimates$error <- NA_character_
  estimates[c("condition_id", "n", "rep_id", "term", "estimate", "std_error",
              "statistic", "p_value", "conf_low", "conf_high", "true_value",
              "alpha", "converged", "error")]
}

run_condition <- function(spec, condition, workers = 1L) {
  validate_ols_spec(spec)
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
                               resume = TRUE) {
  validate_ols_spec(spec)
  grid <- condition_grid(spec)

  if (!is.null(checkpoint_dir)) {
    dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
    yaml::write_yaml(spec, file.path(checkpoint_dir, "spec.yml"))
  }

  results <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    condition <- grid[i, , drop = FALSE]
    checkpoint_file <- if (!is.null(checkpoint_dir)) {
      file.path(checkpoint_dir, sprintf("condition_%03d.rds", condition$condition_id))
    } else {
      NULL
    }

    if (resume && !is.null(checkpoint_file) && file.exists(checkpoint_file)) {
      results[[i]] <- readRDS(checkpoint_file)
      next
    }

    condition_result <- run_condition(spec, condition, workers = workers)
    if (!is.null(checkpoint_file)) {
      saveRDS(condition_result, checkpoint_file)
    }
    results[[i]] <- condition_result
  }

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  out
}
