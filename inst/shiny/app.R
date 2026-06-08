library(shiny)
library(bslib)
library(mcsimr)

ui <- page_sidebar(
  title = "mcsimr",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    textInput("study_name", "Study name", "OLS Monte Carlo Simulation"),
    textAreaInput("research_question", "Research question", "How does OLS coefficient recovery vary across sample sizes?", rows = 3),
    textInput("n", "Sample sizes", "100, 250, 500"),
    numericInput("reps", "Replications per condition", 100, min = 1, step = 10),
    textInput("betas", "True betas", "0.20, 0.30, 0.00"),
    numericInput("rho", "Common predictor correlation", 0.30, min = -0.95, max = 0.95, step = 0.05),
    numericInput("error_sd", "Residual SD", 1, min = 0.01, step = 0.10),
    numericInput("seed", "Seed", 20260608, min = 1, step = 1),
    numericInput("workers", "Workers", max(1, available_cores()), min = 1, step = 1),
    textInput("checkpoint_dir", "Checkpoint directory", "output/checkpoints/ols_app"),
    actionButton("run", "Run simulation", class = "btn-primary"),
    downloadButton("download_summary", "Download summary")
  ),
  layout_column_wrap(
    width = 1,
    card(
      card_header("Simulation summary"),
      tableOutput("summary")
    ),
    card(
      card_header("Generated R code"),
      verbatimTextOutput("code")
    )
  )
)

parse_numeric <- function(x) {
  as.numeric(trimws(strsplit(x, ",", fixed = TRUE)[[1L]]))
}

server <- function(input, output, session) {
  result <- reactiveVal(NULL)

  spec <- reactive({
    ols_sim_spec(
      n = parse_numeric(input$n),
      reps = input$reps,
      betas = parse_numeric(input$betas),
      predictor_correlation = input$rho,
      error_sd = input$error_sd,
      seed = input$seed,
      study_name = input$study_name,
      research_question = input$research_question
    )
  })

  observeEvent(input$run, {
    withProgress(message = "Running OLS simulation", value = 0, {
      res <- run_ols_simulation(
        spec(),
        workers = input$workers,
        checkpoint_dir = input$checkpoint_dir,
        resume = TRUE
      )
      incProgress(0.8)
      result(summarize_ols_results(res))
      incProgress(0.2)
    })
  })

  output$summary <- renderTable({
    req(result())
    result()
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
        "  predictor_correlation = %s,",
        "  error_sd = %s,",
        "  seed = %s,",
        "  study_name = %s,",
        "  research_question = %s",
        ")",
        "",
        "results <- run_ols_simulation(",
        "  spec,",
        "  workers = %s,",
        "  checkpoint_dir = %s,",
        "  resume = TRUE",
        ")",
        "summary <- summarize_ols_results(results)",
        sep = "\n"
      ),
      paste(parse_numeric(input$n), collapse = ", "),
      input$reps,
      paste(parse_numeric(input$betas), collapse = ", "),
      input$rho,
      input$error_sd,
      input$seed,
      deparse(input$study_name),
      deparse(input$research_question),
      input$workers,
      deparse(input$checkpoint_dir)
    )
  })

  output$download_summary <- downloadHandler(
    filename = function() "mcsimr-summary.csv",
    content = function(file) {
      write.csv(result(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
