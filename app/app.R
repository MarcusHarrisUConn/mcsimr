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

run_one_rep <- function(n, betas, rho, error_sd, alpha, rep_id) {
  dat <- generate_ols_data(n, betas, 0, rho, error_sd)
  fit <- lm(y ~ ., data = dat)
  tab <- summary(fit)$coefficients
  ci <- confint(fit, level = 1 - alpha)
  terms <- rownames(tab)
  truth <- c("(Intercept)" = NA_real_, stats::setNames(betas, paste0("x", seq_along(betas))))
  data.frame(
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

summarize_results <- function(results, n, alpha) {
  results <- results[!is.na(results$true_value), , drop = FALSE]
  pieces <- split(results, results$term)
  out <- lapply(pieces, function(dat) {
    true <- dat$true_value[1]
    rejection <- dat$p_value < alpha
    coverage <- dat$conf_low <= true & dat$conf_high >= true
    data.frame(
      n = n,
      term = dat$term[1],
      true_value = true,
      reps = nrow(dat),
      mean_estimate = mean(dat$estimate),
      bias = mean(dat$estimate - true),
      rmse = sqrt(mean((dat$estimate - true)^2)),
      coverage = mean(coverage),
      rejection_rate = mean(rejection),
      row.names = NULL
    )
  })
  do.call(rbind, out)
}

ui <- page_sidebar(
  title = "mcsimr live demo",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    width = 340,
    numericInput("n", "Sample size", 250, min = 20, max = 2000, step = 10),
    numericInput("reps", "Replications", 50, min = 1, max = 500, step = 10),
    textInput("betas", "True betas", "0.20, 0.30, 0.00"),
    sliderInput("rho", "Common predictor correlation", min = -0.3, max = 0.9, value = 0.3, step = 0.05),
    numericInput("error_sd", "Residual SD", 1, min = 0.05, step = 0.05),
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
    betas <- parse_numeric(input$betas)
    validate(
      need(all(!is.na(betas)), "True betas must be comma-separated numbers."),
      need(input$rho > -1 / (length(betas) - 1), "The predictor correlation is too negative for this number of predictors.")
    )

    withProgress(message = "Running browser demo", value = 0, {
      set.seed(input$seed)
      reps <- lapply(seq_len(input$reps), function(i) {
        incProgress(1 / input$reps)
        run_one_rep(input$n, betas, input$rho, input$error_sd, input$alpha, i)
      })
      summary_tbl(summarize_results(do.call(rbind, reps), input$n, input$alpha))
    })
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
        "  n = %s,",
        "  reps = %s,",
        "  betas = c(%s),",
        "  predictor_correlation = %s,",
        "  error_sd = %s,",
        "  alpha = %s,",
        "  seed = %s",
        ")",
        "",
        "results <- run_ols_simulation(spec, workers = 1, checkpoint_dir = 'results/checkpoints')",
        "summary <- summarize_ols_results(results)",
        sep = "\n"
      ),
      input$n,
      input$reps,
      paste(parse_numeric(input$betas), collapse = ", "),
      input$rho,
      input$error_sd,
      input$alpha,
      input$seed
    )
  })
}

shinyApp(ui, server)
