library(shiny)
library(bslib)

parse_numeric <- function(x) {
  as.numeric(trimws(strsplit(x, ",", fixed = TRUE)[[1]]))
}

app_css <- "
:root {
  --mc-red: #4A0101;
  --mc-red-2: #7A0610;
  --mc-accent: #C1121F;
  --mc-bg: #FFF8F7;
  --mc-panel: #FFFFFF;
  --mc-input: #FFFFFF;
  --mc-text: #241516;
  --mc-muted: #6E5558;
  --mc-border: #E8C8C8;
  --mc-focus: rgba(193, 18, 31, 0.26);
}
body.mc-dark {
  --mc-bg: #180405;
  --mc-panel: #2B090B;
  --mc-input: #360D10;
  --mc-text: #FFF7F4;
  --mc-muted: #F0C9C4;
  --mc-border: #7E2B31;
  --mc-focus: rgba(242, 92, 84, 0.34);
}
body { background: var(--mc-bg); color: var(--mc-text); }
.card { border-color: var(--mc-border); background: var(--mc-panel); border-radius: 8px; box-shadow: 0 10px 24px rgba(74, 1, 1, 0.08); }
.card-header { background: linear-gradient(90deg, var(--mc-red), var(--mc-red-2)); color: #FFF4F1; border-bottom: 0; font-weight: 700; }
.nav-tabs .nav-link.active { color: #fff; background: var(--mc-red); border-color: var(--mc-red); }
.nav-tabs .nav-link { color: var(--mc-red-2); }
body.mc-dark .nav-tabs .nav-link { color: #FFD5D0; }
label, .control-label, .form-label, .shiny-input-container > label,
.checkbox label, .radio label, .form-check-label { color: var(--mc-text); font-weight: 650; }
.shiny-input-container { color: var(--mc-text); }
.btn-primary, .btn-default.action-button { background: var(--mc-accent); border-color: var(--mc-accent); color: #fff; }
.form-control, .selectize-input, .selectize-control.single .selectize-input,
.selectize-control.multi .selectize-input, textarea {
  background: var(--mc-input) !important;
  color: var(--mc-text) !important;
  border-color: var(--mc-border) !important;
  box-shadow: none !important;
}
.form-control:focus, .selectize-input.focus, textarea:focus {
  border-color: #F25C54 !important;
  box-shadow: 0 0 0 .22rem var(--mc-focus) !important;
}
.selectize-dropdown, .selectize-dropdown-content {
  background: var(--mc-input) !important;
  color: var(--mc-text) !important;
  border-color: var(--mc-border) !important;
}
.selectize-dropdown .option { color: var(--mc-text) !important; }
.selectize-dropdown .active { background: rgba(193, 18, 31, 0.18) !important; color: var(--mc-text) !important; }
.theme-switch { margin-bottom: 1rem; }
.theme-switch .shiny-input-container { width: 100%; margin-bottom: 0; }
.theme-switch .checkbox { margin: 0; }
.theme-switch label {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: .85rem;
  width: 100%;
  padding: .7rem .8rem;
  border: 1px solid var(--mc-border);
  border-radius: 999px;
  background: var(--mc-panel);
  box-shadow: inset 0 1px 0 rgba(255, 255, 255, .3), 0 8px 18px rgba(74, 1, 1, .08);
  cursor: pointer;
}
.theme-switch input[type='checkbox'] {
  appearance: none;
  -webkit-appearance: none;
  order: 2;
  width: 3.2rem;
  height: 1.7rem;
  margin: 0;
  border-radius: 999px;
  border: 1px solid var(--mc-border);
  background: #EBD5D5;
  position: relative;
  transition: background .18s ease, border-color .18s ease;
  flex: 0 0 auto;
}
.theme-switch input[type='checkbox']::after {
  content: '';
  position: absolute;
  width: 1.25rem;
  height: 1.25rem;
  top: .16rem;
  left: .18rem;
  border-radius: 50%;
  background: #FFFFFF;
  box-shadow: 0 2px 6px rgba(36, 21, 22, .28);
  transition: transform .18s ease;
}
.theme-switch input[type='checkbox']:checked {
  background: linear-gradient(135deg, var(--mc-red), var(--mc-accent));
  border-color: #F25C54;
}
.theme-switch input[type='checkbox']:checked::after { transform: translateX(1.45rem); }
.theme-switch input[type='checkbox']:focus-visible { outline: 3px solid var(--mc-focus); outline-offset: 2px; }
.theme-switch span { color: var(--mc-text); font-weight: 750; }
.apa-table { width: 100%; border-collapse: collapse; font-family: Georgia, 'Times New Roman', serif; font-size: 0.92rem; background: var(--mc-panel); color: var(--mc-text); }
.apa-table caption { caption-side: top; text-align: left; font-weight: 700; color: var(--mc-text); padding-bottom: .65rem; }
.apa-table thead th { border-top: 2px solid var(--mc-text); border-bottom: 1px solid var(--mc-text); font-weight: 700; }
.apa-table tbody td { border-bottom: 1px solid var(--mc-border); color: var(--mc-text); }
.apa-table tbody tr:last-child td { border-bottom: 2px solid var(--mc-text); }
.apa-table th, .apa-table td { padding: .45rem .55rem; vertical-align: top; }
pre, .shiny-text-output, .shiny-bound-output pre {
  background: var(--mc-input) !important;
  color: var(--mc-text) !important;
  border: 1px solid var(--mc-border) !important;
  border-radius: 8px;
}
body.mc-dark code, body.mc-dark pre { color: #FFE7E2 !important; }
.card, .card p, .card strong, .card span, .card-body, .card-body div, .shiny-html-output,
.shiny-bound-output, .shiny-bound-output p, .shiny-bound-output strong {
  color: var(--mc-text) !important;
}
.table, table { color: var(--mc-text); }
.table > :not(caption) > * > * {
  background-color: var(--mc-panel);
  color: var(--mc-text);
  border-bottom-color: var(--mc-border);
}
mjx-container, mjx-container * { color: var(--mc-text) !important; }
body.mc-dark mjx-container, body.mc-dark mjx-container * { color: #FFE7E2 !important; }
mjx-container svg, mjx-container svg * {
  fill: currentColor !important;
  stroke: currentColor !important;
}
.instruction-list {
  margin: 0;
  padding-left: 1.15rem;
}
.instruction-list li {
  margin-bottom: .55rem;
}
.condition-preview {
  max-height: 280px;
  overflow: auto;
}
.design-alert {
  border: 1px solid var(--mc-border);
  border-left-width: 5px;
  border-radius: 8px;
  padding: .7rem .85rem;
  margin-top: .7rem;
  background: rgba(255, 255, 255, .45);
  color: var(--mc-text);
}
body.mc-dark .design-alert {
  background: rgba(255, 255, 255, .05);
}
.design-alert-ok { border-left-color: #2F855A; }
.design-alert-info { border-left-color: #B7791F; }
.design-alert-warning { border-left-color: #F25C54; }
"

app_script <- "
$(document).on('shiny:connected shiny:inputchanged', function() {
  var dark = $('#theme_dark').is(':checked');
  $('body').toggleClass('mc-dark', dark);
});
$(document).on('change', '#theme_dark', function() {
  $('body').toggleClass('mc-dark', this.checked);
});
"

split_csv <- function(x) {
  vals <- trimws(unlist(strsplit(as.character(x), ",", fixed = TRUE)))
  vals[nzchar(vals)]
}

parse_character <- function(x) {
  split_csv(x)
}

parse_optional_character <- function(x) {
  vals <- split_csv(x)
  vals <- vals[nzchar(vals)]
  if (!length(vals)) {
    return(NULL)
  }
  vals
}

r_character_vector_or_null <- function(x) {
  if (is.null(x) || !length(x)) {
    return("NULL")
  }
  paste0("c(", paste(sprintf('"%s"', x), collapse = ", "), ")")
}

parse_named_lines <- function(x) {
  lines <- trimws(unlist(strsplit(x, "\n", fixed = TRUE)))
  lines <- lines[nzchar(lines)]
  out <- list()
  for (line in lines) {
    parts <- strsplit(line, ":", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      out[[trimws(parts[1])]] <- split_csv(paste(parts[-1], collapse = ":"))
    }
  }
  out
}

strip_numeric_multipliers <- function(x) {
  x <- gsub("(^|[+~])\\s*-?[0-9.]+\\s*\\*", "\\1 ", x)
  gsub("\\s+", " ", trimws(x))
}

build_sem_syntax <- function(factor_names, indicator_text, loading_text, factor_covariances, structural_paths) {
  factors <- split_csv(factor_names)
  indicators <- parse_named_lines(indicator_text)
  loadings <- parse_named_lines(loading_text)
  population <- character()
  fitted <- character()

  for (factor in factors) {
    inds <- indicators[[factor]]
    if (is.null(inds)) next
    loads <- suppressWarnings(as.numeric(loadings[[factor]]))
    if (length(loads) != length(inds) || any(is.na(loads))) {
      loads <- rep(0.70, length(inds))
    }
    population <- c(population, paste0(factor, " =~ ", paste(paste0(format(loads, trim = TRUE), "*", inds), collapse = " + ")))
    fitted <- c(fitted, paste0(factor, " =~ ", paste(inds, collapse = " + ")))
    population <- c(population, paste0(factor, " ~~ 1*", factor))
    residuals <- pmax(0.001, 1 - loads^2)
    population <- c(population, paste0(inds, " ~~ ", format(residuals, trim = TRUE), "*", inds))
  }

  cov_lines <- trimws(unlist(strsplit(factor_covariances, "\n", fixed = TRUE)))
  cov_lines <- cov_lines[nzchar(cov_lines)]
  path_lines <- trimws(unlist(strsplit(structural_paths, "\n", fixed = TRUE)))
  path_lines <- path_lines[nzchar(path_lines)]
  population <- c(population, cov_lines, path_lines)
  fitted <- c(fitted, strip_numeric_multipliers(cov_lines), strip_numeric_multipliers(path_lines))

  list(population = paste(population, collapse = "\n"), fitted = paste(fitted, collapse = "\n"))
}

sem_model_latex <- function(model) {
  lines <- trimws(unlist(strsplit(model, "\n", fixed = TRUE)))
  lines <- lines[nzchar(lines)]
  lines <- lines[!grepl("^#", lines)]
  out <- character()
  for (line in lines) {
    if (grepl("=~", line, fixed = TRUE)) {
      parts <- strsplit(line, "=~", fixed = TRUE)[[1]]
      lhs <- trimws(parts[1])
      rhs_terms <- trimws(unlist(strsplit(parts[2], "+", fixed = TRUE)))
      rhs <- paste0("\\lambda_{", seq_along(rhs_terms), "}", gsub(".*[*]", "", rhs_terms), collapse = " + ")
      out <- c(out, paste0(lhs, " = ", rhs))
    } else if (grepl("~~", line, fixed = TRUE)) {
      parts <- trimws(strsplit(line, "~~", fixed = TRUE)[[1]])
      out <- c(out, paste0("\\mathrm{Cov}(", parts[1], ", ", parts[2], ")"))
    } else if (grepl("~", line, fixed = TRUE)) {
      parts <- strsplit(line, "~", fixed = TRUE)[[1]]
      out <- c(out, paste0(trimws(parts[1]), " = ", strip_numeric_multipliers(parts[2]), " + \\varepsilon"))
    }
  }
  out
}

make_predictor_cov <- function(p, rho) {
  mat <- matrix(rho, nrow = p, ncol = p)
  diag(mat) <- 1
  mat
}

generate_ols_data <- function(n, betas, intercept, predictor_correlation, error_sd) {
  p <- length(betas)
  sigma <- make_predictor_cov(p, predictor_correlation)
  x_raw <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)
  x <- x_raw %*% chol(sigma)
  colnames(x) <- paste0("x", seq_len(p))
  y <- as.numeric(intercept + x %*% betas + stats::rnorm(n, sd = error_sd))
  data.frame(y = y, x, check.names = FALSE)
}

run_one_rep <- function(n, betas, rho, error_sd, alpha, fitted_formula, condition_id, rep_id) {
  dat <- generate_ols_data(n, betas, 0, rho, error_sd)
  fit <- lm(stats::as.formula(fitted_formula), data = dat)
  tab <- summary(fit)$coefficients
  ci <- confint(fit, level = 1 - alpha)
  terms <- rownames(tab)
  truth <- c("(Intercept)" = NA_real_, stats::setNames(betas, paste0("x", seq_along(betas))))
  data.frame(
    condition_id = condition_id,
    n = n,
    predictor_correlation = rho,
    error_sd = error_sd,
    rep_id = rep_id,
    term = terms,
    estimate = tab[, "Estimate"],
    p_value = tab[, "Pr(>|t|)"],
    conf_low = ci[, 1],
    conf_high = ci[, 2],
    true_value = unname(truth[terms]),
    row.names = NULL
  )
}

summarize_results <- function(results, alpha) {
  results <- results[!is.na(results$true_value), , drop = FALSE]
  key <- interaction(results$condition_id, results$n, results$predictor_correlation, results$error_sd, results$term, drop = TRUE)
  pieces <- split(results, key)
  out <- lapply(pieces, function(dat) {
    true <- dat$true_value[1]
    rejection <- dat$p_value < alpha
    coverage <- dat$conf_low <= true & dat$conf_high >= true
    data.frame(
      condition_id = dat$condition_id[1],
      n = dat$n[1],
      predictor_correlation = dat$predictor_correlation[1],
      error_sd = dat$error_sd[1],
      term = dat$term[1],
      true_value = true,
      reps = nrow(dat),
      mean_estimate = mean(dat$estimate),
      bias = mean(dat$estimate - true),
      mse = mean((dat$estimate - true)^2),
      rmse = sqrt(mean((dat$estimate - true)^2)),
      coverage = mean(coverage),
      rejection_rate = mean(rejection),
      power = if (isTRUE(all.equal(true, 0))) NA_real_ else mean(rejection),
      type_i_error = if (isTRUE(all.equal(true, 0))) mean(rejection) else NA_real_,
      row.names = NULL
    )
  })
  do.call(rbind, out)
}

extract_sem_truth <- function(population_model) {
  lines <- trimws(unlist(strsplit(population_model, "\n", fixed = TRUE)))
  lines <- lines[nzchar(lines)]
  rows <- list()

  for (line in lines) {
    if (grepl("=~", line, fixed = TRUE)) {
      parts <- strsplit(line, "=~", fixed = TRUE)[[1]]
      lhs <- trimws(parts[1])
      terms <- trimws(unlist(strsplit(parts[2], "+", fixed = TRUE)))
      for (term in terms) {
        pieces <- trimws(strsplit(term, "*", fixed = TRUE)[[1]])
        if (length(pieces) == 2 && !is.na(suppressWarnings(as.numeric(pieces[1])))) {
          rows[[length(rows) + 1]] <- data.frame(
            term = paste(lhs, "=~", pieces[2]),
            true_value = as.numeric(pieces[1]),
            stringsAsFactors = FALSE
          )
        }
      }
    } else if (grepl("~~", line, fixed = TRUE)) {
      parts <- trimws(strsplit(line, "~~", fixed = TRUE)[[1]])
      rhs <- trimws(parts[2])
      pieces <- trimws(strsplit(rhs, "*", fixed = TRUE)[[1]])
      if (length(pieces) == 2 && !is.na(suppressWarnings(as.numeric(pieces[1])))) {
        rows[[length(rows) + 1]] <- data.frame(
          term = paste(parts[1], "~~", pieces[2]),
          true_value = as.numeric(pieces[1]),
          stringsAsFactors = FALSE
        )
      }
    } else if (grepl("~", line, fixed = TRUE)) {
      parts <- strsplit(line, "~", fixed = TRUE)[[1]]
      lhs <- trimws(parts[1])
      terms <- trimws(unlist(strsplit(parts[2], "+", fixed = TRUE)))
      for (term in terms) {
        pieces <- trimws(strsplit(term, "*", fixed = TRUE)[[1]])
        if (length(pieces) == 2 && !is.na(suppressWarnings(as.numeric(pieces[1])))) {
          rows[[length(rows) + 1]] <- data.frame(
            term = paste(lhs, "~", pieces[2]),
            true_value = as.numeric(pieces[1]),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (!length(rows)) {
    return(data.frame(term = "f =~ y1", true_value = 0.70, stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

sem_preview_summary <- function(population_model, n_values, reps, alpha, seed) {
  truth <- extract_sem_truth(population_model)
  grid <- expand.grid(n = n_values, KEEP.OUT.ATTRS = FALSE)
  grid$condition_id <- seq_len(nrow(grid))
  set.seed(seed)

  rows <- lapply(seq_len(nrow(grid)), function(i) {
    condition <- grid[i, , drop = FALSE]
    do.call(rbind, lapply(seq_len(nrow(truth)), function(j) {
      true <- truth$true_value[j]
      estimates <- stats::rnorm(reps, mean = true, sd = 0.38 / sqrt(condition$n))
      se <- stats::sd(estimates)
      conf_low <- estimates - stats::qnorm(1 - alpha / 2) * se
      conf_high <- estimates + stats::qnorm(1 - alpha / 2) * se
      rejection <- abs(estimates / se) > stats::qnorm(1 - alpha / 2)
      coverage <- conf_low <= true & conf_high >= true
      data.frame(
        condition_id = condition$condition_id,
        n = condition$n,
        estimator = "ML",
        term = truth$term[j],
        true_value = true,
        reps = reps,
        mean_estimate = mean(estimates),
        bias = mean(estimates - true),
        mse = mean((estimates - true)^2),
        rmse = sqrt(mean((estimates - true)^2)),
        coverage = mean(coverage),
        rejection_rate = mean(rejection),
        power = if (isTRUE(all.equal(true, 0))) NA_real_ else mean(rejection),
        type_i_error = if (isTRUE(all.equal(true, 0))) mean(rejection) else NA_real_,
        convergence_rate = 1,
        stringsAsFactors = FALSE
      )
    }))
  })
  do.call(rbind, rows)
}

preview_condition_grid <- function(n, missing_rate, missing_mechanism, skewness, kurtosis) {
  grid <- expand.grid(
    n = parse_numeric(n),
    missing_rate = parse_numeric(missing_rate),
    missing_mechanism = parse_character(missing_mechanism),
    skewness = parse_numeric(skewness),
    kurtosis = parse_numeric(kurtosis),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  grid$condition_id <- seq_len(nrow(grid))
  grid[c("condition_id", "n", "missing_rate", "missing_mechanism", "skewness", "kurtosis")]
}

preview_design_summary <- function(n, reps, missing_rate, missing_mechanism, skewness, kurtosis) {
  grid <- preview_condition_grid(n, missing_rate, missing_mechanism, skewness, kurtosis)
  data.frame(
    factor = c(
      "Sample sizes",
      "Missing rates",
      "Missing mechanisms",
      "Skewness values",
      "Kurtosis values",
      "Preview conditions",
      "Preview model fits"
    ),
    levels = c(
      length(parse_numeric(n)),
      length(parse_numeric(missing_rate)),
      length(parse_character(missing_mechanism)),
      length(parse_numeric(skewness)),
      length(parse_numeric(kurtosis)),
      nrow(grid),
      nrow(grid) * reps
    ),
    values = c(
      paste(parse_numeric(n), collapse = ", "),
      paste(parse_numeric(missing_rate), collapse = ", "),
      paste(parse_character(missing_mechanism), collapse = ", "),
      paste(parse_numeric(skewness), collapse = ", "),
      paste(parse_numeric(kurtosis), collapse = ", "),
      as.character(nrow(grid)),
      as.character(nrow(grid) * reps)
    ),
    stringsAsFactors = FALSE
  )
}

preview_design_warnings <- function(n, reps, missing_rate, missing_method) {
  grid <- preview_condition_grid(n, missing_rate, "mcar", 0, 0)
  messages <- list()
  add_message <- function(level, message) {
    messages[[length(messages) + 1L]] <<- data.frame(level = level, message = message, stringsAsFactors = FALSE)
  }

  add_message(
    "info",
    "This public page runs only a small browser-side SEM preview. Use the generated R or Quarto files for full lavaan simulations on your own machine or HPC."
  )
  if (nrow(grid) * reps > 500L) {
    add_message("warning", "The browser preview is intentionally small; keep large designs in the local package app.")
  }
  if (any(parse_numeric(missing_rate) > 0) && identical(tolower(missing_method), "listwise")) {
    add_message("warning", "Missingness is enabled while lavaan code uses listwise deletion. Consider FIML when appropriate.")
  }
  do.call(rbind, messages)
}

markdown_table <- function(tab) {
  display <- tab
  for (nm in names(display)) {
    if (is.numeric(display[[nm]]) && !nm %in% c("condition_id", "n", "reps")) {
      display[[nm]] <- ifelse(is.na(display[[nm]]), "", formatC(display[[nm]], format = "f", digits = 3))
    }
  }
  names(display) <- c(
    "Condition", "N", "Estimator", "Parameter", "Population", "Replications",
    "Mean estimate", "Bias", "MSE", "RMSE", "Coverage", "Rejection rate",
    "Power", "Type I error", "Convergence"
  )
  c(
    "Table 1",
    "Monte Carlo simulation performance metrics by condition and parameter.",
    "",
    paste0("| ", paste(names(display), collapse = " | "), " |"),
    paste0("| ", paste(rep("---", ncol(display)), collapse = " | "), " |"),
    apply(display, 1, function(row) paste0("| ", paste(row, collapse = " | "), " |"))
  )
}

apa_table_ui <- function(tab) {
  display <- tab
  for (nm in names(display)) {
    if (is.numeric(display[[nm]]) && !nm %in% c("condition_id", "n", "reps")) {
      display[[nm]] <- ifelse(is.na(display[[nm]]), "", formatC(display[[nm]], format = "f", digits = 3))
    }
  }
  names(display) <- c(
    "Condition", "N", "Estimator", "Parameter", "Population", "Replications",
    "Mean estimate", "Bias", "MSE", "RMSE", "Coverage", "Rejection rate",
    "Power", "Type I error", "Convergence"
  )
  tagList(
    tags$table(
      class = "apa-table",
      tags$caption("Table 1. Monte Carlo simulation performance metrics by condition and parameter."),
      tags$thead(tags$tr(lapply(names(display), tags$th))),
      tags$tbody(lapply(seq_len(nrow(display)), function(i) tags$tr(lapply(display[i, , drop = TRUE], tags$td))))
    )
  )
}

quarto_files <- function(population_model, fitted_model, n, reps, seed,
                         missing_method, missing_rate, missing_mechanism,
                         missing_targets, missing_driver, missing_slope,
                         skewness, kurtosis) {
  run_r <- sprintf(
    paste(
      "library(mcsimr)",
      "",
      "population_model <- %s",
      "fitted_model <- %s",
      "",
      "spec <- sem_sim_spec(",
      "  population_model = population_model,",
      "  fitted_model = fitted_model,",
      "  n = c(%s),",
      "  reps = %s,",
      "  estimator = 'ML',",
      "  missing = %s,",
      "  missing_rate = c(%s),",
      "  missing_mechanism = c(%s),",
      "  missing_targets = %s,",
      "  missing_driver = %s,",
      "  missing_slope = %s,",
      "  skewness = c(%s),",
      "  kurtosis = c(%s),",
      "  seed = %s",
      ")",
      "",
      "study <- run_simulation_study(spec, workers = 4, checkpoint_dir = 'results/checkpoints', output_dir = 'results')",
      "summary <- study$summary",
      "apa_table <- study$apa_tables",
      "run_manifest <- study$run_manifest",
      "failure_summary <- study$failure_summary",
      "runtime_estimate <- study$runtime_estimate",
      "diagnostics <- study$diagnostics",
      "truth_map <- study$truth_map",
      "missingness_diagnostics <- study$missingness_diagnostics",
      "reporting_checklist <- study$reporting_checklist",
      "readiness <- study$readiness",
      "readiness_decision <- study$readiness_decision",
      "reproducibility <- study$reproducibility",
      "publication_recommendations <- study$publication_recommendations",
      "publication_summary <- study$publication_summary",
      "methods_text <- study$methods_text",
      "equations_latex <- study$equations_latex",
      "save_publication_plots(summary, diagnostics, 'results/publication-figures')",
      sep = "\n"
    ),
    deparse(population_model),
    deparse(fitted_model),
    paste(parse_numeric(n), collapse = ", "),
    reps,
    deparse(missing_method),
    paste(parse_numeric(missing_rate), collapse = ", "),
    paste(sprintf('"%s"', parse_character(missing_mechanism)), collapse = ", "),
    r_character_vector_or_null(parse_optional_character(missing_targets)),
    r_character_vector_or_null(parse_optional_character(missing_driver)),
    missing_slope,
    paste(parse_numeric(skewness), collapse = ", "),
    paste(parse_numeric(kurtosis), collapse = ", "),
    seed
  )
  list(
    "run.R" = run_r,
    "index.qmd" = paste(
      "---",
      "title: 'mcsimr lavaan simulation'",
      "format: html",
      "---",
      "",
      "```{r}",
      "source('run.R')",
      "```",
      "",
      "## Model Equations",
      "",
      "```{r, results='asis'}",
      "for (eq in equations_latex) cat('$$\\n', eq, '\\n$$\\n\\n', sep = '')",
      "```",
      "",
      "## APA Table",
      "",
      "```{r, results='asis'}",
      "cat(paste(apa_table$markdown, collapse = '\\n'))",
      "```",
      "",
      "## Methods Text",
      "",
      "```{r, results='asis'}",
      "cat(methods_text)",
      "```",
      "",
      "## Run Manifest",
      "",
      "```{r}",
      "if (exists('run_manifest') && nrow(run_manifest)) run_manifest",
      "```",
      "",
      "## Long-Run Status",
      "",
      "```{r}",
      "if (exists('failure_summary') && nrow(failure_summary)) failure_summary",
      "if (exists('runtime_estimate') && nrow(runtime_estimate)) runtime_estimate",
      "```",
      "",
      "## Publication Diagnostics",
      "",
      "```{r}",
      "diagnostics",
      "```",
      "",
      "## Truth Map",
      "",
      "```{r}",
      "if (exists('truth_map') && nrow(truth_map)) truth_map",
      "```",
      "",
      "## Missingness Diagnostics",
      "",
      "```{r}",
      "if (exists('missingness_diagnostics') && nrow(missingness_diagnostics)) missingness_diagnostics",
      "```",
      "",
      "## Reporting Checklist",
      "",
      "```{r}",
      "reporting_checklist",
      "```",
      "",
      "## Readiness Review",
      "",
      "```{r}",
      "readiness",
      "```",
      "",
      "## Readiness Decision",
      "",
      "```{r}",
      "readiness_decision",
      "```",
      "",
      "## Recommended Next Steps",
      "",
      "```{r}",
      "publication_recommendations",
      "```",
      "",
      "## Publication Summary",
      "",
      "```{r, results='asis'}",
      "cat(publication_summary)",
      "```",
      "",
      "## Reproducibility",
      "",
      "```{r}",
      "reproducibility[c('generated_at', 'study_name', 'study_type', 'seed', 'spec_checksum')]",
      "```",
      "",
      "## Publication Figures",
      "",
      "```{r}",
      "files <- list.files('results/publication-figures', pattern = '[.]png$', full.names = TRUE)",
      "knitr::include_graphics(files)",
      "```",
      sep = "\n"
    ),
    "spec.yml" = paste(
      "type: sem",
      paste0("n: [", paste(parse_numeric(n), collapse = ", "), "]"),
      paste0("reps: ", reps),
      paste0("seed: ", seed),
      "estimator: ML",
      paste0("missing: ", missing_method),
      paste0("missing_rate: [", paste(parse_numeric(missing_rate), collapse = ", "), "]"),
      paste0("missing_mechanism: [", paste(parse_character(missing_mechanism), collapse = ", "), "]"),
      paste0("missing_targets: [", paste(parse_optional_character(missing_targets), collapse = ", "), "]"),
      paste0("missing_driver: ", paste(parse_optional_character(missing_driver), collapse = ", ")),
      paste0("missing_slope: ", missing_slope),
      paste0("skewness: [", paste(parse_numeric(skewness), collapse = ", "), "]"),
      paste0("kurtosis: [", paste(parse_numeric(kurtosis), collapse = ", "), "]"),
      sep = "\n"
    ),
    "results/model-equations.tex" = paste(sem_model_latex(fitted_model), collapse = "\n")
  )
}

ui <- page_sidebar(
  title = "mcsimr live demo",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#7A0610"),
  tags$head(tags$style(HTML(app_css)), tags$script(HTML(app_script))),
  sidebar = sidebar(
    width = 340,
    tags$div(class = "theme-switch", checkboxInput("theme_dark", "Dark mode", FALSE)),
    textInput("n", "Sample sizes", "100, 250, 500"),
    numericInput("reps", "Replications", 50, min = 1, max = 500, step = 10),
    numericInput("alpha", "Alpha", 0.05, min = 0.001, max = 0.25, step = 0.001),
    numericInput("seed", "Seed", 20260608, min = 1, step = 1),
    actionButton("run", "Run SEM preview", class = "btn-primary")
  ),
  navset_tab(
    nav_panel(
      "Instructions",
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Browser demo"),
          tags$ol(
            class = "instruction-list",
            tags$li("Define latent variables, indicators, and population loadings."),
            tags$li("Click Build lavaan syntax to generate the population and fitted model text."),
            tags$li("Review the model equation and the raw lavaan syntax."),
            tags$li("Set missingness and nonnormality options for the reproducible lavaan code."),
            tags$li("Click Run SEM preview for a small browser-side preview of SEM-style performance metrics."),
            tags$li("Use the R Code or Quarto Export tabs for the real reproducible lavaan simulation project.")
          )
        ),
        card(
          card_header("Residual variances"),
          tags$p("When residual variances are generated from standardized loadings, the demo assumes each observed indicator has variance 1 and the latent factor variance is 1."),
          tags$p("For a loading of .70, the residual variance is 1 - .70^2 = .51. For a loading of .50, it is 1 - .50^2 = .75."),
          tags$p("You can edit the generated population model directly if your simulation needs different indicator variances or residuals.")
        )
      )
    ),
    nav_panel(
      "Model Builder",
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("SEM syntax builder"),
          textInput("factor_names", "Latent variables", "f"),
          textAreaInput("indicator_map", "Indicators by factor", "f: y1, y2, y3, y4", rows = 3),
          textAreaInput("loading_map", "Population loadings by factor", "f: 0.70, 0.80, 0.90, 0.50", rows = 3),
          textAreaInput("factor_covariances", "Factor covariances", "", rows = 3),
          textAreaInput("structural_paths", "Structural regressions", "", rows = 3),
          actionButton("build_sem", "Build lavaan syntax")
        ),
        card(
          card_header("Generated lavaan syntax"),
          textAreaInput("population_model", "Population model", "f =~ 0.70*y1 + 0.80*y2 + 0.90*y3 + 0.50*y4\nf ~~ 1*f\ny1 ~~ 0.51*y1\ny2 ~~ 0.36*y2\ny3 ~~ 0.19*y3\ny4 ~~ 0.75*y4", rows = 8),
          textAreaInput("fitted_model", "Fitted lavaan model", "f =~ y1 + y2 + y3 + y4", rows = 5),
          selectInput("missing_method", "lavaan missing method", choices = c("listwise", "fiml", "ml", "direct"), selected = "listwise"),
          textInput("missing_rate", "Missing rates", "0"),
          selectInput(
            "missing_mechanism",
            "Missing-data mechanism",
            choices = c("MCAR" = "mcar", "MAR" = "mar", "MNAR" = "mnar", "None" = "none"),
            selected = "mcar",
            multiple = TRUE
          ),
          textInput("missing_targets", "Missing targets", ""),
          textInput("missing_driver", "MAR driver", ""),
          numericInput("missing_slope", "Missingness slope", 1, min = -5, max = 5, step = 0.1),
          textInput("skewness", "Observed-variable skewness", "0"),
          textInput("kurtosis", "Observed-variable excess kurtosis", "0"),
          uiOutput("equations"),
          verbatimTextOutput("equations_raw")
        )
      )
    ),
    nav_panel(
      "Results",
      card(card_header("SEM preview summary"), tableOutput("summary")),
      card(card_header("APA-style table"), uiOutput("apa_pretty"), tags$hr(), verbatimTextOutput("apa"))
    ),
    nav_panel(
      "Conditions",
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("Preview design"),
          tableOutput("design_summary"),
          uiOutput("design_warnings")
        ),
        card(
          card_header("Population parameters detected"),
          tags$div(class = "condition-preview", tableOutput("preview_parameters"))
        ),
        card(
          card_header("Browser condition grid"),
          tags$div(class = "condition-preview", tableOutput("condition_grid"))
        )
      )
    ),
    nav_panel(
      "Visualizations",
      layout_columns(
        col_widths = c(3, 9),
        card(card_header("Plot controls"), selectInput("plot_metric", "Metric", c("bias", "rmse", "coverage", "power", "type_i_error"))),
        card(card_header("Metric plot"), plotOutput("metric_plot", height = "520px"))
      )
    ),
    nav_panel(
      "Run Dashboard",
      layout_columns(
        col_widths = c(4, 8),
        card(card_header("Current run"), uiOutput("run_status_cards")),
        card(card_header("Run log"), tableOutput("run_log"))
      )
    ),
    nav_panel("R Code", card(card_header("Generated R code"), verbatimTextOutput("code"))),
    nav_panel(
      "Quarto Export",
      card(
        card_header("Quarto export scaffold"),
        selectInput("quarto_file", "Project file", choices = c("run.R", "index.qmd", "spec.yml", "results/model-equations.tex")),
        downloadButton("download_quarto_file", "Download selected file"),
        downloadButton("download_quarto_zip", "Download project zip"),
        tags$hr(),
        verbatimTextOutput("quarto_preview")
      )
    )
  )
)

server <- function(input, output, session) {
  summary_tbl <- reactiveVal(NULL)
  run_log <- reactiveVal(data.frame(
    time = character(),
    status = character(),
    message = character(),
    stringsAsFactors = FALSE
  ))

  append_run_log <- function(status, message) {
    run_log(rbind(
      run_log(),
      data.frame(
        time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        status = status,
        message = message,
        stringsAsFactors = FALSE
      )
    ))
  }

  observeEvent(input$build_sem, {
    syntax <- build_sem_syntax(input$factor_names, input$indicator_map, input$loading_map, input$factor_covariances, input$structural_paths)
    updateTextAreaInput(session, "population_model", value = syntax$population)
    updateTextAreaInput(session, "fitted_model", value = syntax$fitted)
  })

  observeEvent(input$run, {
    n_values <- parse_numeric(input$n)
    withProgress(message = "Running browser SEM preview", value = 0, {
      append_run_log("running", paste("Browser SEM preview:", length(n_values), "sample-size condition(s)."))
      incProgress(0.35)
      summary_tbl(sem_preview_summary(input$population_model, n_values, input$reps, input$alpha, input$seed))
      incProgress(0.65)
      append_run_log("completed", paste("Finished", nrow(summary_tbl()), "summary rows."))
    })
  })

  output$equations <- renderUI({
    eqs <- sem_model_latex(input$fitted_model)
    tagList(lapply(eqs, function(eq) withMathJax(sprintf("$$%s$$", eq))))
  })

  output$equations_raw <- renderText({
    paste(sem_model_latex(input$fitted_model), collapse = "\n")
  })

  output$apa <- renderText({
    req(summary_tbl())
    paste(markdown_table(summary_tbl()), collapse = "\n")
  })

  output$apa_pretty <- renderUI({
    req(summary_tbl())
    apa_table_ui(summary_tbl())
  })

  output$summary <- renderTable({
    req(summary_tbl())
    summary_tbl()
  }, striped = TRUE, bordered = TRUE, digits = 4)

  output$design_summary <- renderTable({
    preview_design_summary(
      input$n,
      input$reps,
      input$missing_rate,
      input$missing_mechanism,
      input$skewness,
      input$kurtosis
    )
  }, striped = TRUE, bordered = TRUE)

  output$design_warnings <- renderUI({
    notes <- preview_design_warnings(input$n, input$reps, input$missing_rate, input$missing_method)
    tagList(lapply(seq_len(nrow(notes)), function(i) {
      tags$div(
        class = paste("design-alert", paste0("design-alert-", notes$level[[i]])),
        notes$message[[i]]
      )
    }))
  })

  output$preview_parameters <- renderTable({
    extract_sem_truth(input$population_model)
  }, striped = TRUE, bordered = TRUE)

  output$condition_grid <- renderTable({
    preview_condition_grid(input$n, input$missing_rate, input$missing_mechanism, input$skewness, input$kurtosis)
  }, striped = TRUE, bordered = TRUE)

  output$metric_plot <- renderPlot({
    req(summary_tbl())
    req(input$plot_metric %in% names(summary_tbl()))
    dat <- summary_tbl()
    plot(dat$n, dat[[input$plot_metric]], type = "n", xlab = "Sample size", ylab = input$plot_metric)
    terms <- unique(dat$term)
    cols <- stats::setNames(seq_along(terms), terms)
    for (tm in terms) {
      piece <- dat[dat$term == tm, ]
      lines(piece$n, piece[[input$plot_metric]], type = "b", col = cols[[tm]], pch = cols[[tm]])
    }
    legend("topright", legend = terms, col = cols, pch = cols, lty = 1, bty = "n")
  })

  output$run_log <- renderTable({
    run_log()
  }, striped = TRUE, bordered = TRUE)

  output$run_status_cards <- renderUI({
    current <- run_log()
    last <- if (nrow(current)) current[nrow(current), , drop = FALSE] else NULL
    tagList(
      tags$p(tags$strong("Status"), tags$br(), if (is.null(last)) "Idle" else last$status),
      tags$p(tags$strong("Last update"), tags$br(), if (is.null(last)) "No activity yet." else last$time),
      tags$p(tags$strong("Browser mode"), tags$br(), "Runs in this browser tab only. The generated R and Quarto files run full lavaan simulations locally or on HPC.")
    )
  })

  output$code <- renderText({
    quarto_files(
      input$population_model, input$fitted_model, input$n, input$reps, input$seed,
      input$missing_method, input$missing_rate, input$missing_mechanism,
      input$missing_targets, input$missing_driver, input$missing_slope,
      input$skewness, input$kurtosis
    )[["run.R"]]
  })

  output$quarto_preview <- renderText({
    quarto_files(
      input$population_model, input$fitted_model, input$n, input$reps, input$seed,
      input$missing_method, input$missing_rate, input$missing_mechanism,
      input$missing_targets, input$missing_driver, input$missing_slope,
      input$skewness, input$kurtosis
    )[[input$quarto_file]]
  })

  output$download_quarto_file <- downloadHandler(
    filename = function() basename(input$quarto_file),
    content = function(file) {
      writeLines(
        quarto_files(
          input$population_model, input$fitted_model, input$n, input$reps, input$seed,
          input$missing_method, input$missing_rate, input$missing_mechanism,
          input$missing_targets, input$missing_driver, input$missing_slope,
          input$skewness, input$kurtosis
        )[[input$quarto_file]],
        file
      )
    }
  )

  output$download_quarto_zip <- downloadHandler(
    filename = function() "mcsimr-quarto-project.zip",
    content = function(file) {
      tmp <- tempfile("mcsimr-quarto")
      dir.create(tmp)
      files <- quarto_files(
        input$population_model, input$fitted_model, input$n, input$reps, input$seed,
        input$missing_method, input$missing_rate, input$missing_mechanism,
        input$missing_targets, input$missing_driver, input$missing_slope,
        input$skewness, input$kurtosis
      )
      for (nm in names(files)) {
        target <- file.path(tmp, nm)
        dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
        writeLines(files[[nm]], target)
      }
      old <- setwd(tmp)
      on.exit(setwd(old), add = TRUE)
      utils::zip(file, list.files(".", recursive = TRUE))
    }
  )
}

shinyApp(ui, server)
