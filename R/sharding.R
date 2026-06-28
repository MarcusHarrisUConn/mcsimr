condition_shards <- function(spec,
                             shards,
                             strategy = c("round_robin", "contiguous")) {
  shards <- as.integer(shards)[1L]
  if (is.na(shards) || shards < 1L) {
    stop("`shards` must be a positive integer.", call. = FALSE)
  }
  strategy <- match.arg(strategy)
  grid <- simulation_condition_grid(spec)
  n_conditions <- nrow(grid)
  if (!n_conditions) {
    stop("The simulation design has no conditions.", call. = FALSE)
  }

  if (identical(strategy, "round_robin")) {
    shard_id <- ((seq_len(n_conditions) - 1L) %% shards) + 1L
  } else {
    shard_id <- ceiling(seq_len(n_conditions) / ceiling(n_conditions / shards))
    shard_id <- pmin(shard_id, shards)
  }

  out <- data.frame(
    shard_id = shard_id,
    shard_label = sprintf("shard-%03d-of-%03d", shard_id, shards),
    total_shards = shards,
    stringsAsFactors = FALSE
  )
  cbind(out, grid)
}

run_simulation_shard <- function(spec,
                                 shard_id,
                                 shards,
                                 workers = 1L,
                                 checkpoint_dir = NULL,
                                 resume = TRUE,
                                 output_dir = NULL,
                                 strategy = c("round_robin", "contiguous")) {
  shard_id <- as.integer(shard_id)[1L]
  shards <- as.integer(shards)[1L]
  if (is.na(shard_id) || is.na(shards) || shard_id < 1L || shard_id > shards) {
    stop("`shard_id` must be between 1 and `shards`.", call. = FALSE)
  }
  strategy <- match.arg(strategy)
  shard_map <- condition_shards(spec, shards = shards, strategy = strategy)
  ids <- shard_map$condition_id[shard_map$shard_id == shard_id]
  label <- sprintf("shard-%03d-of-%03d", shard_id, shards)

  run_simulation_study(
    spec = spec,
    workers = workers,
    checkpoint_dir = if (is.null(checkpoint_dir)) NULL else file.path(checkpoint_dir, label),
    resume = resume,
    output_dir = if (is.null(output_dir)) NULL else file.path(output_dir, label),
    condition_ids = ids
  )
}

collect_checkpoint_results <- function(checkpoint_dir,
                                       recursive = TRUE) {
  if (is.null(checkpoint_dir) || !dir.exists(checkpoint_dir)) {
    stop("Checkpoint directory does not exist.", call. = FALSE)
  }
  files <- list.files(
    checkpoint_dir,
    pattern = "^condition_[0-9]+[.]rds$",
    recursive = recursive,
    full.names = TRUE
  )
  if (!length(files)) {
    return(data.frame())
  }
  pieces <- lapply(files, function(path) {
    tryCatch(read_condition_checkpoint(path), error = function(e) NULL)
  })
  pieces <- pieces[!vapply(pieces, is.null, logical(1L))]
  if (!length(pieces)) {
    return(data.frame())
  }
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

simulation_condition_grid <- function(spec) {
  if (identical(spec$type, "sem") || inherits(spec, "mcsimr_sem_spec")) {
    validate_sem_spec(spec)
    return(sem_condition_grid(spec))
  }
  validate_ols_spec(spec)
  condition_grid(spec)
}

subset_condition_grid <- function(grid, condition_ids = NULL) {
  if (is.null(condition_ids)) {
    return(grid)
  }
  condition_ids <- as.integer(condition_ids)
  keep <- grid$condition_id %in% condition_ids
  if (!any(keep)) {
    stop("No condition rows matched `condition_ids`.", call. = FALSE)
  }
  grid[keep, , drop = FALSE]
}

run_failure_summary <- function(manifest) {
  if (is.character(manifest) && length(manifest) == 1L) {
    manifest <- read_run_manifest(manifest)
  }
  if (is.null(manifest) || !nrow(manifest)) {
    return(data.frame())
  }
  tab <- as.data.frame(table(manifest$status), stringsAsFactors = FALSE)
  names(tab) <- c("status", "conditions")
  tab$conditions <- as.integer(tab$conditions)
  tab$attempts <- vapply(tab$status, function(status) {
    rows <- manifest$status == status
    sum(manifest$attempts[rows], na.rm = TRUE)
  }, numeric(1L))
  tab$median_duration_seconds <- vapply(tab$status, function(status) {
    rows <- manifest$status == status
    vals <- manifest$duration_seconds[rows]
    vals <- vals[is.finite(vals)]
    if (!length(vals)) NA_real_ else stats::median(vals)
  }, numeric(1L))
  tab$total_duration_seconds <- vapply(tab$status, function(status) {
    rows <- manifest$status == status
    sum(manifest$duration_seconds[rows], na.rm = TRUE)
  }, numeric(1L))
  tab
}

runtime_estimate_from_manifest <- function(manifest,
                                           total_conditions = NULL) {
  if (is.character(manifest) && length(manifest) == 1L) {
    manifest <- read_run_manifest(manifest)
  }
  if (is.null(manifest) || !nrow(manifest)) {
    return(data.frame())
  }
  completed <- manifest$status %in% c("completed", "resumed")
  durations <- manifest$duration_seconds[completed]
  durations <- durations[is.finite(durations) & durations > 0]
  if (is.null(total_conditions)) {
    total_conditions <- nrow(manifest)
  }
  total_conditions <- as.integer(total_conditions)[1L]
  completed_conditions <- sum(completed, na.rm = TRUE)
  failed_conditions <- sum(manifest$status == "failed", na.rm = TRUE)
  remaining_conditions <- max(0L, total_conditions - completed_conditions - failed_conditions)
  mean_seconds <- if (!length(durations)) NA_real_ else mean(durations)
  remaining_seconds <- if (is.na(mean_seconds)) NA_real_ else remaining_conditions * mean_seconds
  data.frame(
    total_conditions = total_conditions,
    completed_conditions = completed_conditions,
    failed_conditions = failed_conditions,
    remaining_conditions = remaining_conditions,
    mean_seconds_per_condition = mean_seconds,
    estimated_seconds_remaining = remaining_seconds,
    estimated_finish_at = if (is.na(remaining_seconds)) NA_character_ else as.character(Sys.time() + remaining_seconds),
    stringsAsFactors = FALSE
  )
}
