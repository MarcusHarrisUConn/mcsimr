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
.table, table { color: var(--mc-text); }
.table > :not(caption) > * > * {
  background-color: var(--mc-panel);
  color: var(--mc-text);
  border-bottom-color: var(--mc-border);
}
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
  trimws(strsplit(x, ",", fixed = TRUE)[[1]])
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

markdown_table <- function(tab) {
  display <- tab
  for (nm in names(display)) {
    if (is.numeric(display[[nm]]) && !nm %in% c("condition_id", "n", "reps")) {
      display[[nm]] <- ifelse(is.na(display[[nm]]), "", formatC(display[[nm]], format = "f", digits = 3))
    }
  }
  names(display) <- c(
    "Condition", "N", "Predictor r", "Residual SD", "Parameter", "Population",
    "Replications", "Mean estimate", "Bias", "MSE", "RMSE", "Coverage",
    "Rejection rate", "Power", "Type I error"
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
    "Condition", "N", "Predictor r", "Residual SD", "Parameter", "Population",
    "Replications", "Mean estimate", "Bias", "MSE", "RMSE", "Coverage",
    "Rejection rate", "Power", "Type I error"
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

quarto_files <- function(population_model, fitted_model, n, reps, seed) {
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
      "  seed = %s",
      ")",
      "",
      "study <- run_simulation_study(spec, workers = 4, checkpoint_dir = 'results/checkpoints', output_dir = 'results')",
      "summary <- study$summary",
      "apa_table <- study$apa_tables",
      "equations_latex <- study$equations_latex",
      sep = "\n"
    ),
    deparse(population_model),
    deparse(fitted_model),
    paste(parse_numeric(n), collapse = ", "),
    reps,
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
      sep = "\n"
    ),
    "spec.yml" = paste(
      "type: sem",
      paste0("n: [", paste(parse_numeric(n), collapse = ", "), "]"),
      paste0("reps: ", reps),
      paste0("seed: ", seed),
      "estimator: ML",
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
    actionButton("run", "Run OLS demo", class = "btn-primary")
  ),
  navset_tab(
    nav_panel(
      "Model Builder",
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("SEM syntax builder"),
          textInput("factor_names", "Latent variables", "f"),
          textAreaInput("indicator_map", "Indicators by factor", "f: y1, y2, y3", rows = 3),
          textAreaInput("loading_map", "Population loadings by factor", "f: 0.70, 0.80, 0.90", rows = 3),
          textAreaInput("factor_covariances", "Factor covariances", "", rows = 3),
          textAreaInput("structural_paths", "Structural regressions", "", rows = 3),
          actionButton("build_sem", "Build lavaan syntax")
        ),
        card(
          card_header("Generated lavaan syntax"),
          textAreaInput("population_model", "Population model", "f =~ 0.70*y1 + 0.80*y2 + 0.90*y3\nf ~~ 1*f\ny1 ~~ 0.51*y1\ny2 ~~ 0.36*y2\ny3 ~~ 0.19*y3", rows = 8),
          textAreaInput("fitted_model", "Fitted lavaan model", "f =~ y1 + y2 + y3", rows = 5),
          uiOutput("equations"),
          verbatimTextOutput("equations_raw")
        )
      )
    ),
    nav_panel(
      "Results",
      card(card_header("OLS demo summary"), tableOutput("summary")),
      card(card_header("APA-style table"), uiOutput("apa_pretty"), tags$hr(), verbatimTextOutput("apa"))
    ),
    nav_panel(
      "Visualizations",
      layout_columns(
        col_widths = c(3, 9),
        card(card_header("Plot controls"), selectInput("plot_metric", "Metric", c("bias", "rmse", "coverage", "power", "type_i_error"))),
        card(card_header("Metric plot"), plotOutput("metric_plot", height = "520px"))
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

  observeEvent(input$build_sem, {
    syntax <- build_sem_syntax(input$factor_names, input$indicator_map, input$loading_map, input$factor_covariances, input$structural_paths)
    updateTextAreaInput(session, "population_model", value = syntax$population)
    updateTextAreaInput(session, "fitted_model", value = syntax$fitted)
  })

  observeEvent(input$run, {
    n_values <- parse_numeric(input$n)
    betas <- c(0.20, 0.30, 0.00)
    rho_values <- 0.30
    error_values <- 1
    withProgress(message = "Running browser OLS demo", value = 0, {
      set.seed(input$seed)
      grid <- expand.grid(n = n_values, predictor_correlation = rho_values, error_sd = error_values)
      grid$condition_id <- seq_len(nrow(grid))
      total <- nrow(grid) * input$reps
      reps <- unlist(lapply(seq_len(nrow(grid)), function(row_id) {
        condition <- grid[row_id, ]
        lapply(seq_len(input$reps), function(i) {
          incProgress(1 / total)
          run_one_rep(condition$n, betas, condition$predictor_correlation, condition$error_sd, input$alpha, "y ~ x1 + x2 + x3", condition$condition_id, i)
        })
      }), recursive = FALSE)
      summary_tbl(summarize_results(do.call(rbind, reps), input$alpha))
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

  output$code <- renderText({
    quarto_files(input$population_model, input$fitted_model, input$n, input$reps, input$seed)[["run.R"]]
  })

  output$quarto_preview <- renderText({
    quarto_files(input$population_model, input$fitted_model, input$n, input$reps, input$seed)[[input$quarto_file]]
  })

  output$download_quarto_file <- downloadHandler(
    filename = function() basename(input$quarto_file),
    content = function(file) {
      writeLines(quarto_files(input$population_model, input$fitted_model, input$n, input$reps, input$seed)[[input$quarto_file]], file)
    }
  )

  output$download_quarto_zip <- downloadHandler(
    filename = function() "mcsimr-quarto-project.zip",
    content = function(file) {
      tmp <- tempfile("mcsimr-quarto")
      dir.create(tmp)
      files <- quarto_files(input$population_model, input$fitted_model, input$n, input$reps, input$seed)
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
