run_manifest_path <- function(checkpoint_dir) {
  if (is.null(checkpoint_dir) || !nzchar(checkpoint_dir)) {
    return(NULL)
  }
  file.path(checkpoint_dir, "run-manifest.csv")
}

read_run_manifest <- function(checkpoint_dir) {
  path <- run_manifest_path(checkpoint_dir)
  if (is.null(path) || !file.exists(path)) {
    return(data.frame())
  }
  manifest <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
  manifest <- normalize_run_manifest(manifest)
  manifest
}

initialize_run_manifest <- function(grid, checkpoint_dir) {
  if (is.null(checkpoint_dir)) {
    return(NULL)
  }
  checkpoint_files <- condition_checkpoint_paths(checkpoint_dir, grid$condition_id)
  manifest <- cbind(
    data.frame(
      condition_id = grid$condition_id,
      status = "queued",
      checkpoint_file = checkpoint_files,
      started_at = NA_character_,
      finished_at = NA_character_,
      duration_seconds = NA_real_,
      attempts = 0L,
      n_rows = NA_integer_,
      error = NA_character_,
      resumed_from_checkpoint = FALSE,
      updated_at = current_manifest_time(),
      stringsAsFactors = FALSE
    ),
    grid[setdiff(names(grid), "condition_id")]
  )
  write_run_manifest(manifest, checkpoint_dir)
  manifest
}

write_run_manifest <- function(manifest, checkpoint_dir) {
  path <- run_manifest_path(checkpoint_dir)
  if (is.null(path)) {
    return(invisible(manifest))
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(manifest, path, row.names = FALSE, na = "")
  invisible(manifest)
}

update_run_manifest <- function(checkpoint_dir,
                                condition_id,
                                status,
                                started_at = NULL,
                                finished_at = NULL,
                                duration_seconds = NULL,
                                n_rows = NULL,
                                error = NULL,
                                resumed_from_checkpoint = NULL,
                                increment_attempt = FALSE) {
  if (is.null(checkpoint_dir)) {
    return(invisible(NULL))
  }
  manifest <- read_run_manifest(checkpoint_dir)
  if (!nrow(manifest)) {
    return(invisible(NULL))
  }
  row <- manifest$condition_id == condition_id
  if (!any(row)) {
    return(invisible(manifest))
  }

  manifest$status[row] <- status
  if (!is.null(started_at)) {
    manifest$started_at[row] <- started_at
  }
  if (!is.null(finished_at)) {
    manifest$finished_at[row] <- finished_at
  }
  if (!is.null(duration_seconds)) {
    manifest$duration_seconds[row] <- duration_seconds
  }
  if (!is.null(n_rows)) {
    manifest$n_rows[row] <- n_rows
  }
  if (!is.null(error)) {
    manifest$error[row] <- error
  }
  if (!is.null(resumed_from_checkpoint)) {
    manifest$resumed_from_checkpoint[row] <- resumed_from_checkpoint
  }
  if (isTRUE(increment_attempt)) {
    attempts <- suppressWarnings(as.integer(manifest$attempts[row]))
    attempts[is.na(attempts)] <- 0L
    manifest$attempts[row] <- attempts + 1L
  }
  manifest$updated_at[row] <- current_manifest_time()

  write_run_manifest(manifest, checkpoint_dir)
  invisible(manifest)
}

condition_checkpoint_path <- function(checkpoint_dir, condition_id) {
  condition_checkpoint_paths(checkpoint_dir, condition_id)
}

condition_checkpoint_paths <- function(checkpoint_dir, condition_id) {
  file.path(checkpoint_dir, sprintf("condition_%03d.rds", as.integer(condition_id)))
}

read_condition_checkpoint <- function(path, condition_id = NULL) {
  checkpoint <- tryCatch(readRDS(path), error = identity)
  if (inherits(checkpoint, "error")) {
    stop("Could not read checkpoint: ", conditionMessage(checkpoint), call. = FALSE)
  }
  validate_condition_checkpoint(checkpoint, condition_id = condition_id)
  checkpoint
}

validate_condition_checkpoint <- function(checkpoint, condition_id = NULL) {
  if (!is.data.frame(checkpoint)) {
    stop("Checkpoint is not a data frame.", call. = FALSE)
  }
  if (!nrow(checkpoint)) {
    stop("Checkpoint has zero rows.", call. = FALSE)
  }
  if (!"condition_id" %in% names(checkpoint)) {
    stop("Checkpoint has no condition_id column.", call. = FALSE)
  }
  if (!is.null(condition_id) && any(checkpoint$condition_id != condition_id, na.rm = TRUE)) {
    stop("Checkpoint condition_id does not match the requested condition.", call. = FALSE)
  }
  invisible(TRUE)
}

current_manifest_time <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

normalize_run_manifest <- function(manifest) {
  if (!nrow(manifest)) {
    return(manifest)
  }
  integer_cols <- intersect(c("condition_id", "attempts", "n_rows"), names(manifest))
  for (col in integer_cols) {
    manifest[[col]] <- suppressWarnings(as.integer(manifest[[col]]))
  }
  numeric_cols <- intersect("duration_seconds", names(manifest))
  for (col in numeric_cols) {
    manifest[[col]] <- suppressWarnings(as.numeric(manifest[[col]]))
  }
  logical_cols <- intersect("resumed_from_checkpoint", names(manifest))
  for (col in logical_cols) {
    manifest[[col]] <- as.logical(manifest[[col]])
    manifest[[col]][is.na(manifest[[col]])] <- FALSE
  }
  character_cols <- intersect(
    c("status", "checkpoint_file", "started_at", "finished_at", "error", "updated_at"),
    names(manifest)
  )
  for (col in character_cols) {
    manifest[[col]] <- as.character(manifest[[col]])
  }
  manifest
}
