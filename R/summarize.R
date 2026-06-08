summarize_ols_results <- function(results) {
  parameter_rows <- !is.na(results$true_value)
  x <- results[parameter_rows & results$converged, , drop = FALSE]
  if (nrow(x) == 0L) {
    stop("No converged parameter-level results were available to summarize.", call. = FALSE)
  }

  split_key <- interaction(x$condition_id, x$n, x$term, drop = TRUE, sep = "|")
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
      term = dat$term[1L],
      true_value = true,
      reps = reps,
      mean_estimate = mean(estimate, na.rm = TRUE),
      bias = mean(estimate - true, na.rm = TRUE),
      relative_bias = if (isTRUE(all.equal(true, 0))) NA_real_ else mean((estimate - true) / true, na.rm = TRUE),
      rmse = sqrt(mean((estimate - true)^2, na.rm = TRUE)),
      coverage = mean(coverage, na.rm = TRUE),
      rejection_rate = mean(rejection, na.rm = TRUE),
      rejection_mcse = sqrt(mean(rejection, na.rm = TRUE) * (1 - mean(rejection, na.rm = TRUE)) / reps),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, summaries)
  rownames(out) <- NULL
  out[order(out$condition_id, out$term), ]
}
