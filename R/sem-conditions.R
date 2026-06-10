sem_parameter_conditions <- function(lhs = character(),
                                     op = character(),
                                     rhs = character(),
                                     values = list(),
                                     label = NULL) {
  if (length(lhs) == 0L) {
    return(data.frame(
      lhs = character(),
      op = character(),
      rhs = character(),
      label = character(),
      values = I(list()),
      stringsAsFactors = FALSE
    ))
  }
  stopifnot(length(lhs) == length(op), length(op) == length(rhs))
  if (!is.list(values)) {
    values <- as.list(values)
  }
  if (length(values) != length(lhs)) {
    stop("`values` must contain one vector of condition values per parameter.", call. = FALSE)
  }
  if (is.null(label)) {
    label <- paste(lhs, op, rhs)
  }
  data.frame(
    lhs = as.character(lhs),
    op = as.character(op),
    rhs = as.character(rhs),
    label = as.character(label),
    values = I(lapply(values, as.numeric)),
    stringsAsFactors = FALSE
  )
}

normalize_sem_parameter_conditions <- function(parameter_conditions = NULL) {
  if (is.null(parameter_conditions)) {
    return(sem_parameter_conditions())
  }
  if (!is.data.frame(parameter_conditions)) {
    stop("`parameter_conditions` must be a data frame.", call. = FALSE)
  }
  required <- c("lhs", "op", "rhs", "values")
  missing <- setdiff(required, names(parameter_conditions))
  if (length(missing)) {
    stop("`parameter_conditions` is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  label <- parameter_conditions$label
  if (is.null(label)) {
    label <- paste(parameter_conditions$lhs, parameter_conditions$op, parameter_conditions$rhs)
  }
  values <- parameter_conditions$values
  if (!is.list(values)) {
    values <- lapply(values, function(x) {
      if (is.character(x)) {
        as.numeric(trimws(strsplit(x, ",", fixed = TRUE)[[1L]]))
      } else {
        as.numeric(x)
      }
    })
  }
  sem_parameter_conditions(
    lhs = parameter_conditions$lhs,
    op = parameter_conditions$op,
    rhs = parameter_conditions$rhs,
    values = values,
    label = label
  )
}

sem_condition_column_names <- function(parameter_conditions) {
  if (nrow(parameter_conditions) == 0L) {
    return(character())
  }
  paste0("param_", seq_len(nrow(parameter_conditions)))
}

apply_sem_parameter_conditions <- function(population_model, parameter_conditions, condition) {
  parameter_conditions <- normalize_sem_parameter_conditions(parameter_conditions)
  if (nrow(parameter_conditions) == 0L) {
    return(population_model)
  }

  tab <- lavaan::lavaanify(population_model, fixed.x = FALSE)
  cols <- sem_condition_column_names(parameter_conditions)

  for (i in seq_len(nrow(parameter_conditions))) {
    value <- condition[[cols[[i]]]]
    hit <- tab$lhs == parameter_conditions$lhs[[i]] &
      tab$op == parameter_conditions$op[[i]] &
      tab$rhs == parameter_conditions$rhs[[i]]
    if (!any(hit)) {
      stop(
        "Parameter condition not found in population model: ",
        parameter_conditions$lhs[[i]], " ",
        parameter_conditions$op[[i]], " ",
        parameter_conditions$rhs[[i]],
        call. = FALSE
      )
    }
    tab$ustart[hit] <- as.numeric(value)
    tab$free[hit] <- 0L
  }

  tab
}
