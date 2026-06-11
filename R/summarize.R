summarize_ols_results <- function(results, metrics = default_metrics("ols")) {
  parameter_rows <- !is.na(results$true_value)
  x <- results[parameter_rows & results$converged, , drop = FALSE]
  if (nrow(x) == 0L) {
    stop("No converged parameter-level results were available to summarize.", call. = FALSE)
  }

  split_key <- interaction(
    x$condition_id, x$n, x$predictor_correlation, x$error_sd, x$term,
    drop = TRUE,
    sep = "|"
  )
  groups <- split(x, split_key)

  summaries <- lapply(groups, function(dat) {
    true <- dat$true_value[1L]
    estimate <- dat$estimate
    rejection <- dat$p_value < dat$alpha[1L]
    coverage <- dat$conf_low <= true & dat$conf_high >= true
    reps <- length(estimate)

    data.frame(
      condition_id = dat$condition_id[1L],
      n = dat$n[1L],
      predictor_correlation = dat$predictor_correlation[1L],
      error_sd = dat$error_sd[1L],
      term = dat$term[1L],
      true_value = true,
      reps = reps,
      convergence_rate = mean(results$converged[results$condition_id == dat$condition_id[1L]], na.rm = TRUE),
      mean_estimate = mean(estimate, na.rm = TRUE),
      bias = mean(estimate - true, na.rm = TRUE),
      relative_bias = if (isTRUE(all.equal(true, 0))) NA_real_ else mean((estimate - true) / true, na.rm = TRUE),
      mse = mean((estimate - true)^2, na.rm = TRUE),
      rmse = sqrt(mean((estimate - true)^2, na.rm = TRUE)),
      coverage = mean(coverage, na.rm = TRUE),
      rejection_rate = mean(rejection, na.rm = TRUE),
      power = if (isTRUE(all.equal(true, 0))) NA_real_ else mean(rejection, na.rm = TRUE),
      type_i_error = if (isTRUE(all.equal(true, 0))) mean(rejection, na.rm = TRUE) else NA_real_,
      mcse_rejection = sqrt(mean(rejection, na.rm = TRUE) * (1 - mean(rejection, na.rm = TRUE)) / reps),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, summaries)
  rownames(out) <- NULL
  out <- out[order(out$condition_id, out$term), ]
  keep <- unique(c(
    "condition_id", "n", "predictor_correlation", "error_sd", "term",
    "true_value", "reps", metrics
  ))
  out[, intersect(keep, names(out)), drop = FALSE]
}

format_apa_number <- function(x, digits = 3L) {
  ifelse(
    is.na(x),
    "",
    formatC(x, format = "f", digits = digits)
  )
}

apa_metric_table <- function(summary,
                             metrics = default_metrics("ols"),
                             digits = 3L,
                             caption = "Table 1\nMonte Carlo simulation performance metrics by condition and parameter.") {
  base_cols <- c("condition_id", "n", "predictor_correlation", "error_sd", "term", "true_value", "reps")
  base_cols <- c(base_cols, "estimator", "missing_rate", "skewness", "kurtosis", "parameter_conditions")
  cols <- intersect(c(base_cols, metrics), names(summary))
  tab <- summary[, cols, drop = FALSE]
  labels <- c(
    condition_id = "Condition",
    n = "N",
    predictor_correlation = "Predictor r",
    error_sd = "Residual SD",
    estimator = "Estimator",
    missing_rate = "Missing rate",
    skewness = "Skewness",
    kurtosis = "Kurtosis",
    parameter_conditions = "Varied parameter(s)",
    term = "Parameter",
    true_value = "Population",
    reps = "Replications",
    mean_estimate = "Mean estimate",
    bias = "Bias",
    relative_bias = "Relative bias",
    mse = "MSE",
    rmse = "RMSE",
    coverage = "Coverage",
    rejection_rate = "Rejection rate",
    power = "Power",
    type_i_error = "Type I error",
    mcse_rejection = "MCSE",
    convergence_rate = "Convergence",
    improper_solution_rate = "Improper solution",
    mean_cfi = "Mean CFI",
    mean_tli = "Mean TLI",
    mean_rmsea = "Mean RMSEA",
    mean_srmr = "Mean SRMR"
  )
  names(tab) <- unname(labels[cols])

  for (nm in names(tab)) {
    if (is.numeric(tab[[nm]]) && !nm %in% c("Condition", "N", "Replications")) {
      tab[[nm]] <- format_apa_number(tab[[nm]], digits = digits)
    }
  }

  list(caption = caption, table = tab, markdown = markdown_table(tab, caption))
}

summarize_sem_results <- function(results, metrics = default_metrics("sem")) {
  parameter_rows <- !is.na(results$true_value)
  x <- results[parameter_rows & results$converged, , drop = FALSE]
  if (nrow(x) == 0L) {
    stop("No converged SEM parameter-level results were available to summarize.", call. = FALSE)
  }

  split_key <- interaction(
    x$condition_id, x$n, x$estimator, x$missing_rate, x$skewness, x$kurtosis,
    x$parameter_conditions, x$term,
    drop = TRUE,
    sep = "|"
  )
  groups <- split(x, split_key)

  summaries <- lapply(groups, function(dat) {
    true <- dat$true_value[1L]
    estimate <- dat$estimate
    rejection <- dat$p_value < dat$alpha[1L]
    coverage <- dat$conf_low <= true & dat$conf_high >= true
    reps <- length(estimate)
    condition_rows <- results[results$condition_id == dat$condition_id[1L], , drop = FALSE]

    data.frame(
      condition_id = dat$condition_id[1L],
      n = dat$n[1L],
      estimator = dat$estimator[1L],
      missing_rate = dat$missing_rate[1L],
      skewness = dat$skewness[1L],
      kurtosis = dat$kurtosis[1L],
      parameter_conditions = dat$parameter_conditions[1L],
      term = dat$term[1L],
      true_value = true,
      reps = reps,
      convergence_rate = mean(condition_rows$converged, na.rm = TRUE),
      improper_solution_rate = mean(unique(condition_rows[c("rep_id", "improper_solution")])$improper_solution, na.rm = TRUE),
      mean_estimate = mean(estimate, na.rm = TRUE),
      bias = mean(estimate - true, na.rm = TRUE),
      relative_bias = if (isTRUE(all.equal(true, 0))) NA_real_ else mean((estimate - true) / true, na.rm = TRUE),
      mse = mean((estimate - true)^2, na.rm = TRUE),
      rmse = sqrt(mean((estimate - true)^2, na.rm = TRUE)),
      coverage = mean(coverage, na.rm = TRUE),
      rejection_rate = mean(rejection, na.rm = TRUE),
      power = if (isTRUE(all.equal(true, 0))) NA_real_ else mean(rejection, na.rm = TRUE),
      type_i_error = if (isTRUE(all.equal(true, 0))) mean(rejection, na.rm = TRUE) else NA_real_,
      mcse_rejection = sqrt(mean(rejection, na.rm = TRUE) * (1 - mean(rejection, na.rm = TRUE)) / reps),
      mean_cfi = mean(unique(condition_rows[c("rep_id", "cfi")])$cfi, na.rm = TRUE),
      mean_tli = mean(unique(condition_rows[c("rep_id", "tli")])$tli, na.rm = TRUE),
      mean_rmsea = mean(unique(condition_rows[c("rep_id", "rmsea")])$rmsea, na.rm = TRUE),
      mean_srmr = mean(unique(condition_rows[c("rep_id", "srmr")])$srmr, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, summaries)
  rownames(out) <- NULL
  out <- out[order(out$condition_id, out$term), ]
  keep <- unique(c(
    "condition_id", "n", "estimator", "missing_rate", "skewness", "kurtosis",
    "parameter_conditions", "term", "true_value", "reps", metrics
  ))
  out[, intersect(keep, names(out)), drop = FALSE]
}

markdown_table <- function(tab, caption = NULL) {
  header <- paste0("| ", paste(names(tab), collapse = " | "), " |")
  divider <- paste0("| ", paste(rep("---", ncol(tab)), collapse = " | "), " |")
  rows <- apply(tab, 1L, function(row) paste0("| ", paste(row, collapse = " | "), " |"))
  c(if (!is.null(caption)) c(caption, "") else character(), header, divider, rows)
}

write_apa_tables <- function(apa_table, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(apa_table$markdown, path)
  invisible(path)
}

plot_metric <- function(summary,
                        metric = "bias",
                        term = NULL,
                        path = NULL,
                        width = 900,
                        height = 650) {
  if (!metric %in% names(summary)) {
    stop("Metric not found in summary: ", metric, call. = FALSE)
  }
  dat <- summary
  if (!is.null(term)) {
    dat <- dat[dat$term %in% term, , drop = FALSE]
  }
  if (!is.null(path)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    grDevices::png(path, width = width, height = height)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  y <- dat[[metric]]
  graphics::plot(
    dat$n, y,
    type = "n",
    xlab = "Sample size",
    ylab = metric,
    main = paste("Monte Carlo", metric, "by sample size")
  )
  terms <- unique(dat$term)
  cols <- stats::setNames(seq_along(terms), terms)
  for (tm in terms) {
    piece <- dat[dat$term == tm, , drop = FALSE]
    piece <- piece[order(piece$n), , drop = FALSE]
    graphics::lines(piece$n, piece[[metric]], type = "b", col = cols[[tm]], pch = cols[[tm]])
  }
  graphics::legend("topright", legend = terms, col = cols, pch = cols, lty = 1, bty = "n")
  invisible(path)
}

save_metric_plots <- function(summary,
                              path,
                              metrics = intersect(c("bias", "rmse", "coverage", "power", "type_i_error"), names(summary))) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  files <- stats::setNames(file.path(path, paste0(metrics, ".png")), metrics)
  for (metric in metrics) {
    plot_metric(summary, metric = metric, path = files[[metric]])
  }
  invisible(files)
}
