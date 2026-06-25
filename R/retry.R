retry_failed_conditions <- function(spec,
                                    checkpoint_dir,
                                    workers = 1L,
                                    condition_ids = NULL) {
  if (is.null(checkpoint_dir) || !dir.exists(checkpoint_dir)) {
    stop("Checkpoint directory does not exist.", call. = FALSE)
  }
  manifest <- read_run_manifest(checkpoint_dir)
  if (!nrow(manifest)) {
    stop("No run manifest found in checkpoint directory.", call. = FALSE)
  }
  failed <- manifest[manifest$status == "failed", , drop = FALSE]
  if (!is.null(condition_ids)) {
    failed <- failed[failed$condition_id %in% condition_ids, , drop = FALSE]
  }
  if (!nrow(failed)) {
    return(invisible(data.frame()))
  }

  grid <- if (identical(spec$type, "sem") || inherits(spec, "mcsimr_sem_spec")) {
    sem_condition_grid(spec)
  } else {
    condition_grid(spec)
  }

  results <- vector("list", nrow(failed))
  for (i in seq_len(nrow(failed))) {
    condition_id <- failed$condition_id[[i]]
    condition <- grid[grid$condition_id == condition_id, , drop = FALSE]
    if (!nrow(condition)) {
      next
    }
    checkpoint_file <- condition_checkpoint_path(checkpoint_dir, condition_id)
    if (file.exists(checkpoint_file)) {
      unlink(checkpoint_file)
    }
    started <- Sys.time()
    update_run_manifest(
      checkpoint_dir,
      condition_id,
      status = "running",
      started_at = format(started, "%Y-%m-%d %H:%M:%S"),
      error = NA_character_,
      resumed_from_checkpoint = FALSE,
      increment_attempt = TRUE
    )
    result <- tryCatch(
      if (identical(spec$type, "sem") || inherits(spec, "mcsimr_sem_spec")) {
        run_sem_condition(spec, condition, workers = workers)
      } else {
        run_condition(spec, condition, workers = workers)
      },
      error = identity
    )
    if (inherits(result, "error")) {
      update_run_manifest(
        checkpoint_dir,
        condition_id,
        status = "failed",
        finished_at = current_manifest_time(),
        duration_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
        error = conditionMessage(result)
      )
      next
    }
    saveRDS(result, checkpoint_file)
    update_run_manifest(
      checkpoint_dir,
      condition_id,
      status = "completed",
      finished_at = current_manifest_time(),
      duration_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
      n_rows = nrow(result),
      error = NA_character_,
      resumed_from_checkpoint = FALSE
    )
    results[[i]] <- result
  }

  out <- do.call(rbind, results[!vapply(results, is.null, logical(1L))])
  if (is.null(out)) {
    return(invisible(data.frame()))
  }
  rownames(out) <- NULL
  out
}
