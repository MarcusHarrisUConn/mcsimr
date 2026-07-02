lavaan_runtime_available <- function() {
  cores <- tryCatch(parallel::detectCores(), error = function(e) NA_integer_)
  is.finite(cores) && cores >= 1L
}

lavaan_runtime_error <- function() {
  paste(
    "lavaan could not initialize because parallel::detectCores() did not return",
    "a finite core count in this R session. Try running in an R session where",
    "core detection is available, or update lavaan if a fix is available."
  )
}

run_sem_replication <- function(spec, condition, rep_id) {
  validate_sem_spec(spec)
  if (!lavaan_runtime_available()) {
    return(sem_error_row(spec, condition, rep_id, lavaan_runtime_error()))
  }
  conditioned_population <- tryCatch(
    apply_sem_parameter_conditions(
      population_model = spec$population_model,
      parameter_conditions = spec$parameter_conditions,
      condition = condition
    ),
    error = identity
  )
  if (inherits(conditioned_population, "error")) {
    return(sem_error_row(spec, condition, rep_id, conditionMessage(conditioned_population)))
  }

  dat <- tryCatch(
    simulate_sem_data(
      spec = spec,
      model = conditioned_population,
      n = condition$n,
      skewness = condition$skewness,
      kurtosis = condition$kurtosis
    ),
    error = identity
  )
  if (inherits(dat, "error")) {
    return(sem_error_row(spec, condition, rep_id, conditionMessage(dat)))
  }
  dat <- apply_sem_missing(
    dat = dat,
    rate = condition$missing_rate,
    mechanism = condition$missing_mechanism,
    targets = spec$missing_targets,
    driver = spec$missing_driver,
    slope = spec$missing_slope
  )

  fit_args <- list(
    model = spec$fitted_model,
    data = dat,
    estimator = condition$estimator,
    missing = spec$missing,
    std.lv = spec$std_lv
  )
  if (!is.null(spec$group_variable)) {
    fit_args$group <- spec$group_variable
  }
  fit <- tryCatch(
    do.call(lavaan::sem, fit_args),
    error = identity
  )
  if (inherits(fit, "error")) {
    return(sem_error_row(spec, condition, rep_id, conditionMessage(fit)))
  }

  converged <- isTRUE(lavaan::lavInspect(fit, "converged"))
  pe <- tryCatch(lavaan::parameterEstimates(fit, ci = TRUE), error = identity)
  if (inherits(pe, "error")) {
    return(sem_error_row(spec, condition, rep_id, conditionMessage(pe), converged = converged))
  }

  true_values <- sem_true_values(conditioned_population)
  if ("group" %in% names(pe) && !is.null(spec$group_labels)) {
    pe$group_label <- spec$group_labels[pe$group]
  } else {
    pe$group_label <- NA_character_
  }
  pe$term <- paste(pe$lhs, pe$op, pe$rhs)
  pe$term <- ifelse(
    is.na(pe$group_label),
    pe$term,
    paste0(pe$term, " [", pe$group_label, "]")
  )
  pe$key <- sem_param_key(pe$lhs, pe$op, pe$rhs)
  pe$true_value <- true_values[pe$key]

  fit_indices <- sem_fit_indices(fit)
  improper <- sem_improper_solution(pe)

  data.frame(
    condition_id = condition$condition_id,
    n = condition$n,
    estimator = condition$estimator,
    missing_rate = condition$missing_rate,
    missing_mechanism = condition$missing_mechanism,
    skewness = condition$skewness,
    kurtosis = condition$kurtosis,
    parameter_conditions = condition$parameter_conditions,
    rep_id = rep_id,
    group = pe$group_label,
    term = pe$term,
    lhs = pe$lhs,
    op = pe$op,
    rhs = pe$rhs,
    estimate = pe$est,
    std_error = pe$se,
    statistic = pe$z,
    p_value = pe$pvalue,
    conf_low = pe$ci.lower,
    conf_high = pe$ci.upper,
    true_value = unname(pe$true_value),
    alpha = spec$alpha,
    converged = converged,
    improper_solution = improper,
    cfi = fit_indices[["cfi"]],
    tli = fit_indices[["tli"]],
    rmsea = fit_indices[["rmsea"]],
    srmr = fit_indices[["srmr"]],
    error = NA_character_,
    stringsAsFactors = FALSE
  )
}

run_sem_condition <- function(spec, condition, workers = 1L) {
  validate_sem_spec(spec)
  reps <- seq_len(spec$reps)

  if (workers > 1L) {
    cl <- parallel::makeCluster(workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterSetRNGStream(cl, spec$seed + condition$condition_id)
    parallel::clusterEvalQ(cl, library(lavaan))
    parallel::clusterExport(
      cl,
      varlist = c(
        "spec", "condition", "run_sem_replication", "sem_error_row",
        "sem_true_values", "sem_param_key", "sem_fit_indices",
        "sem_improper_solution", "validate_sem_spec",
        "apply_sem_parameter_conditions", "normalize_sem_parameter_conditions",
        "sem_parameter_conditions", "sem_condition_column_names",
        "apply_mcar_missing", "apply_sem_missing", "resolve_missing_targets",
        "missing_probabilities", "standardize_for_missing", "sem_moment_arg",
        "simulate_sem_data", "sem_group_sample_sizes",
        "lavaan_runtime_available", "lavaan_runtime_error"
      ),
      envir = environment()
    )
    pieces <- parallel::parLapply(
      cl,
      reps,
      function(rep_id) run_sem_replication(spec, condition, rep_id)
    )
  } else {
    set.seed(spec$seed + condition$condition_id)
    pieces <- lapply(reps, function(rep_id) run_sem_replication(spec, condition, rep_id))
  }

  do.call(rbind, pieces)
}

simulate_sem_data <- function(spec, model, n, skewness = 0, kurtosis = 0) {
  if (is.null(spec$group_variable)) {
    return(lavaan::simulateData(
      model = model,
      sample.nobs = n,
      skewness = sem_moment_arg(skewness),
      kurtosis = sem_moment_arg(kurtosis)
    ))
  }

  group_labels <- spec$group_labels
  group_n <- sem_group_sample_sizes(n, group_labels, spec$group_proportions)
  pieces <- lapply(seq_along(group_labels), function(i) {
    dat <- lavaan::simulateData(
      model = model,
      sample.nobs = group_n[[i]],
      skewness = sem_moment_arg(skewness),
      kurtosis = sem_moment_arg(kurtosis)
    )
    dat[[spec$group_variable]] <- group_labels[[i]]
    dat
  })
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

sem_group_sample_sizes <- function(n, group_labels, group_proportions = NULL) {
  n <- as.integer(n)[1L]
  if (is.null(group_proportions)) {
    group_proportions <- rep(1 / length(group_labels), length(group_labels))
  }
  counts <- floor(n * group_proportions)
  remainder <- n - sum(counts)
  if (remainder > 0L) {
    counts[seq_len(remainder)] <- counts[seq_len(remainder)] + 1L
  }
  pmax(1L, counts)
}

run_sem_simulation <- function(spec,
                               workers = 1L,
                               checkpoint_dir = NULL,
                               resume = TRUE,
                               condition_ids = NULL) {
  validate_sem_spec(spec)
  grid <- subset_condition_grid(sem_condition_grid(spec), condition_ids = condition_ids)

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
      run_sem_condition(spec, condition, workers = workers),
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

sem_true_values <- function(population_model) {
  if (is.data.frame(population_model)) {
    tab <- population_model
  } else {
    tab <- lavaan::lavaanify(population_model, fixed.x = FALSE)
  }
  keep <- !is.na(tab$ustart) & tab$op %in% c("=~", "~", "~~", "~1")
  vals <- tab$ustart[keep]
  names(vals) <- sem_param_key(tab$lhs[keep], tab$op[keep], tab$rhs[keep])
  vals
}

sem_param_key <- function(lhs, op, rhs) {
  paste(lhs, op, rhs, sep = "|")
}

sem_fit_indices <- function(fit) {
  wanted <- c("cfi", "tli", "rmsea", "srmr")
  out <- stats::setNames(rep(NA_real_, length(wanted)), wanted)
  vals <- tryCatch(lavaan::fitMeasures(fit, wanted), error = function(e) out)
  out[names(vals)] <- vals
  out
}

sem_improper_solution <- function(pe) {
  variances <- pe[pe$op == "~~" & pe$lhs == pe$rhs, , drop = FALSE]
  any(is.finite(variances$est) & variances$est < 0)
}

sem_moment_arg <- function(x) {
  if (length(x) == 0L || all(is.na(x)) || isTRUE(all.equal(as.numeric(x), 0))) {
    return(NULL)
  }
  as.numeric(x)
}

apply_mcar_missing <- function(dat, rate) {
  apply_sem_missing(dat = dat, rate = rate, mechanism = "mcar")
}

apply_sem_missing <- function(dat,
                              rate,
                              mechanism = "mcar",
                              targets = NULL,
                              driver = NULL,
                              slope = 1) {
  rate <- as.numeric(rate)[1L]
  if (is.na(rate) || rate <= 0) {
    return(dat)
  }
  mechanism <- match.arg(tolower(mechanism), c("none", "mcar", "mar", "mnar"))
  if (identical(mechanism, "none")) {
    return(dat)
  }

  resolved <- resolve_missing_targets(dat, targets = targets, driver = driver, mechanism = mechanism)
  target_names <- resolved$targets
  if (!length(target_names)) {
    return(dat)
  }

  out <- dat
  if (identical(mechanism, "mcar")) {
    mask <- matrix(
      stats::runif(nrow(out) * length(target_names)) < rate,
      nrow = nrow(out),
      ncol = length(target_names)
    )
    for (j in seq_along(target_names)) {
      out[[target_names[[j]]]][mask[, j]] <- NA
    }
    return(out)
  }

  if (identical(mechanism, "mar")) {
    z <- standardize_for_missing(out[[resolved$driver]])
    probs <- missing_probabilities(z, rate = rate, slope = slope)
    for (target in target_names) {
      out[[target]][stats::runif(nrow(out)) < probs] <- NA
    }
    return(out)
  }

  for (target in target_names) {
    z <- standardize_for_missing(out[[target]])
    probs <- missing_probabilities(z, rate = rate, slope = slope)
    out[[target]][stats::runif(nrow(out)) < probs] <- NA
  }
  out
}

resolve_missing_targets <- function(dat, targets = NULL, driver = NULL, mechanism = "mcar") {
  vars <- names(dat)
  if (is.null(vars)) {
    stop("Generated data must have column names for missing-data mechanisms.", call. = FALSE)
  }

  if (is.null(targets) || !length(targets) || identical(targets, "")) {
    targets <- if (identical(mechanism, "mar") && length(vars) > 1L) vars[-1L] else vars
  }
  targets <- intersect(as.character(targets), vars)

  if (is.null(driver) || !nzchar(driver)) {
    driver <- setdiff(vars, targets)[1L]
    if (is.na(driver)) {
      driver <- vars[1L]
    }
  }
  if (!driver %in% vars) {
    stop("Missing-data driver not found in generated data: ", driver, call. = FALSE)
  }

  list(targets = targets, driver = driver)
}

standardize_for_missing <- function(x) {
  x <- as.numeric(x)
  s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) {
    return(rep(0, length(x)))
  }
  as.numeric((x - mean(x, na.rm = TRUE)) / s)
}

missing_probabilities <- function(z, rate, slope = 1) {
  rate <- min(max(as.numeric(rate)[1L], 1e-6), 1 - 1e-6)
  slope <- as.numeric(slope)[1L]
  if (!is.finite(slope)) {
    slope <- 1
  }
  objective <- function(intercept) mean(stats::plogis(intercept + slope * z), na.rm = TRUE) - rate
  intercept <- tryCatch(
    stats::uniroot(objective, lower = -30, upper = 30)$root,
    error = function(e) stats::qlogis(rate)
  )
  stats::plogis(intercept + slope * z)
}

sem_error_row <- function(spec, condition, rep_id, error, converged = FALSE) {
  data.frame(
    condition_id = condition$condition_id,
    n = condition$n,
    estimator = condition$estimator,
    missing_rate = condition$missing_rate,
    missing_mechanism = condition$missing_mechanism,
    skewness = condition$skewness,
    kurtosis = condition$kurtosis,
    parameter_conditions = condition$parameter_conditions,
    rep_id = rep_id,
    group = NA_character_,
    term = NA_character_,
    lhs = NA_character_,
    op = NA_character_,
    rhs = NA_character_,
    estimate = NA_real_,
    std_error = NA_real_,
    statistic = NA_real_,
    p_value = NA_real_,
    conf_low = NA_real_,
    conf_high = NA_real_,
    true_value = NA_real_,
    alpha = spec$alpha,
    converged = converged,
    improper_solution = NA,
    cfi = NA_real_,
    tli = NA_real_,
    rmsea = NA_real_,
    srmr = NA_real_,
    error = error,
    stringsAsFactors = FALSE
  )
}
