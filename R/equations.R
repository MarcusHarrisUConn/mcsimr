escape_latex <- function(x) {
  x <- gsub("_", "\\\\_", x, fixed = TRUE)
  x
}

ols_formula_latex <- function(formula_text) {
  form <- stats::as.formula(formula_text)
  response <- escape_latex(as.character(form[[2L]]))
  predictors <- attr(stats::terms(form), "term.labels")
  if (length(predictors) == 0L) {
    return(paste0(response, " = \\beta_0 + \\varepsilon"))
  }
  rhs <- paste0("\\beta_{", seq_along(predictors), "}", escape_latex(predictors), collapse = " + ")
  paste0(response, " = \\beta_0 + ", rhs, " + \\varepsilon")
}

sem_model_latex <- function(model) {
  lines <- trimws(unlist(strsplit(model, "\n", fixed = TRUE)))
  lines <- lines[nzchar(lines)]
  lines <- lines[!grepl("^#", lines)]
  out <- character()

  for (line in lines) {
    if (grepl("=~", line, fixed = TRUE)) {
      parts <- strsplit(line, "=~", fixed = TRUE)[[1L]]
      lhs <- escape_latex(trimws(parts[1L]))
      rhs <- latex_terms(parts[2L], loading = "\\lambda")
      out <- c(out, paste0(lhs, " = ", rhs))
    } else if (grepl("~~", line, fixed = TRUE)) {
      parts <- strsplit(line, "~~", fixed = TRUE)[[1L]]
      lhs <- escape_latex(trimws(parts[1L]))
      rhs <- escape_latex(trimws(parts[2L]))
      out <- c(out, paste0("\\mathrm{Cov}(", lhs, ", ", rhs, ")"))
    } else if (grepl("~", line, fixed = TRUE)) {
      parts <- strsplit(line, "~", fixed = TRUE)[[1L]]
      lhs <- escape_latex(trimws(parts[1L]))
      rhs <- latex_terms(parts[2L], loading = "\\beta")
      out <- c(out, paste0(lhs, " = ", rhs, " + \\varepsilon"))
    }
  }

  out
}

latex_terms <- function(rhs, loading = "\\beta") {
  terms <- trimws(unlist(strsplit(rhs, "+", fixed = TRUE)))
  terms <- terms[nzchar(terms)]
  pieces <- vapply(seq_along(terms), function(i) {
    term <- terms[[i]]
    if (grepl("*", term, fixed = TRUE)) {
      item <- trimws(strsplit(term, "*", fixed = TRUE)[[1L]])
      coef <- item[1L]
      var <- item[length(item)]
      paste0(coef, escape_latex(var))
    } else {
      paste0(loading, "_{", i, "}", escape_latex(term))
    }
  }, character(1L))
  paste(pieces, collapse = " + ")
}
