library(shiny)
library(bslib)

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
    textInput("betas", "True betas", "0.20, 0.30, 0.00"),
    textInput("rho", "Predictor correlations", "0.30"),
    textInput("error_sd", "Residual SD conditions", "1"),
    textInput("fitted_formula", "Fitted model", "y ~ x1 + x2 + x3"),
    numericInput("alpha", "Alpha", 0.05, min = 0.001, max = 0.25, step = 0.001),
    numericInput("seed", "Seed", 20260608, min = 1, step = 1),
    actionButton("run", "Run demo", class = "btn-primary")
  ),
  layout_column_wrap(
    width = 1,
    card(
      card_header("OLS simulation summary"),
      tableOutput("summary")
    ),
    card(
      card_header("APA-style table"),
      verbatimTextOutput("apa")
    ),
    card(
      card_header("Reproducible R code"),
      verbatimTextOutput("code")
    )
  )
)

parse_numeric <- function(x) {
  as.numeric(trimws(strsplit(x, ",", fixed = TRUE)[[1]]))
}

server <- function(input, output, session) {
  summary_tbl <- reactiveVal(NULL)

  observeEvent(input$run, {
    n_values <- parse_numeric(input$n)
    betas <- parse_numeric(input$betas)
    rho_values <- parse_numeric(input$rho)
    error_values <- parse_numeric(input$error_sd)
    validate(
      need(all(!is.na(n_values)), "Sample sizes must be comma-separated numbers."),
      need(all(!is.na(betas)), "True betas must be comma-separated numbers."),
      need(all(!is.na(rho_values)), "Predictor correlations must be comma-separated numbers."),
      need(all(!is.na(error_values)), "Residual SD conditions must be comma-separated numbers."),
      need(all(rho_values > -1 / (length(betas) - 1)), "A predictor correlation is too negative for this number of predictors.")
    )

    withProgress(message = "Running browser demo", value = 0, {
      set.seed(input$seed)
      grid <- expand.grid(n = n_values, predictor_correlation = rho_values, error_sd = error_values)
      grid$condition_id <- seq_len(nrow(grid))
      total <- nrow(grid) * input$reps
      reps <- unlist(lapply(seq_len(nrow(grid)), function(row_id) {
        condition <- grid[row_id, ]
        lapply(seq_len(input$reps), function(i) {
          incProgress(1 / total)
          run_one_rep(
            condition$n, betas, condition$predictor_correlation, condition$error_sd,
            input$alpha, input$fitted_formula, condition$condition_id, i
          )
        })
      }), recursive = FALSE)
      summary <- summarize_results(do.call(rbind, reps), input$alpha)
      summary_tbl(summary)
    })
  })

  output$apa <- renderText({
    req(summary_tbl())
    paste(markdown_table(summary_tbl()), collapse = "\n")
  })

  output$summary <- renderTable({
    req(summary_tbl())
    summary_tbl()
  }, striped = TRUE, bordered = TRUE, digits = 4)

  output$code <- renderText({
    sprintf(
      paste(
        "library(mcsimr)",
        "",
        "spec <- ols_sim_spec(",
        "  n = c(%s),",
        "  reps = %s,",
        "  betas = c(%s),",
        "  predictor_correlation = c(%s),",
        "  error_sd = c(%s),",
        "  alpha = %s,",
        "  seed = %s,",
        "  fitted_formula = %s",
        ")",
        "",
        "study <- run_simulation_study(spec, workers = 1, checkpoint_dir = 'results/checkpoints')",
        "summary <- study$summary",
        "apa_table <- study$apa_tables",
        sep = "\n"
      ),
      paste(parse_numeric(input$n), collapse = ", "),
      input$reps,
      paste(parse_numeric(input$betas), collapse = ", "),
      paste(parse_numeric(input$rho), collapse = ", "),
      paste(parse_numeric(input$error_sd), collapse = ", "),
      input$alpha,
      input$seed,
      deparse(input$fitted_formula)
    )
  })
}

shinyApp(ui, server)
