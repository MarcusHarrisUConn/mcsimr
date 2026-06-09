library(shiny)
library(bslib)

parse_numeric <- function(x) {
  as.numeric(trimws(strsplit(x, ",", fixed = TRUE)[[1]]))
}

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

ui <- page_sidebar(
  title = "mcsimr live demo",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    width = 340,
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
      card(card_header("APA-style table"), verbatimTextOutput("apa"))
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
    nav_panel("Quarto Export", card(card_header("Quarto export scaffold"), verbatimTextOutput("quarto_note")))
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
    sprintf(
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
        "study <- run_simulation_study(spec, workers = 4, checkpoint_dir = 'results/checkpoints')",
        "summary <- study$summary",
        "equations_latex <- study$equations_latex",
        sep = "\n"
      ),
      deparse(input$population_model),
      deparse(input$fitted_model),
      paste(parse_numeric(input$n), collapse = ", "),
      input$reps,
      input$seed
    )
  })

  output$quarto_note <- renderText({
    "The full local app can export a runnable Quarto project with spec.yml, run.R, APA tables, figures, rendered model equations, and raw LaTeX equation files."
  })
}

shinyApp(ui, server)
