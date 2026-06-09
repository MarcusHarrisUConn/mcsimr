library(shiny)
library(bslib)
library(mcsimr)

parse_numeric <- function(x) {
  as.numeric(trimws(strsplit(x, ",", fixed = TRUE)[[1L]]))
}

parse_character <- function(x) {
  trimws(strsplit(x, ",", fixed = TRUE)[[1L]])
}

ui <- page_sidebar(
  title = "mcsimr",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    selectInput("simulation_type", "Simulation family", choices = c("lavaan SEM" = "sem", "OLS regression" = "ols")),
    textInput("study_name", "Study name", "lavaan Monte Carlo Simulation"),
    textAreaInput("research_question", "Research question", "How does SEM parameter recovery vary across sample sizes?", rows = 3),
    textInput("n", "Sample sizes", "100, 250, 500"),
    numericInput("reps", "Replications per condition", 100, min = 1, step = 10),
    conditionalPanel(
      "input.simulation_type == 'ols'",
      textInput("condition_rho", "Predictor correlations", "0.30"),
      textInput("condition_error_sd", "Residual SD conditions", "1"),
      textInput("betas", "True betas", "0.20, 0.30, 0.00"),
      textInput("fitted_formula", "Fitted model", "y ~ x1 + x2 + x3")
    ),
    conditionalPanel(
      "input.simulation_type == 'sem'",
      textAreaInput(
        "population_model",
        "Population model",
        "f =~ 0.70*y1 + 0.80*y2 + 0.90*y3\nf ~~ 1*f\ny1 ~~ 0.51*y1\ny2 ~~ 0.36*y2\ny3 ~~ 0.19*y3",
        rows = 6
      ),
      textAreaInput(
        "fitted_model",
        "Fitted lavaan model",
        "f =~ y1 + y2 + y3",
        rows = 4
      ),
      textInput("estimator", "Estimators", "ML"),
      checkboxInput("std_lv", "Use std.lv = TRUE", TRUE)
    ),
    numericInput("alpha", "Alpha", 0.05, min = 0.001, max = 0.25, step = 0.001),
    checkboxGroupInput(
      "metrics",
      "Output metrics",
      choices = stats::setNames(metric_catalog("sem")$metric, metric_catalog("sem")$metric),
      selected = default_metrics("sem")
    ),
    numericInput("seed", "Seed", 20260608, min = 1, step = 1),
    numericInput("workers", "Workers", max(1, available_cores()), min = 1, step = 1),
    textInput("checkpoint_dir", "Checkpoint directory", "output/checkpoints/sem_app"),
    textInput("export_path", "Quarto export path", "output/quarto/sem_simulation_project"),
    actionButton("run", "Run simulation", class = "btn-primary"),
    actionButton("export_quarto", "Export Quarto project"),
    downloadButton("download_summary", "Download summary")
  ),
  layout_column_wrap(
    width = 1,
    card(
      card_header("Simulation summary"),
      tableOutput("summary")
    ),
    card(
      card_header("APA-style table"),
      verbatimTextOutput("apa")
    ),
    card(
      card_header("Model equations"),
      uiOutput("equations"),
      verbatimTextOutput("equations_raw")
    ),
    card(
      card_header("Generated R code"),
      verbatimTextOutput("code")
    ),
    card(
      card_header("Export status"),
      verbatimTextOutput("export_status")
    )
  )
)

server <- function(input, output, session) {
  study <- reactiveVal(NULL)
  export_status <- reactiveVal("No project exported yet.")

  observeEvent(input$simulation_type, {
    model <- if (identical(input$simulation_type, "sem")) "sem" else "ols"
    updateCheckboxGroupInput(
      session,
      "metrics",
      choices = stats::setNames(metric_catalog(model)$metric, metric_catalog(model)$metric),
      selected = default_metrics(model)
    )
  }, ignoreInit = FALSE)

  spec <- reactive({
    if (identical(input$simulation_type, "sem")) {
      sem_sim_spec(
        population_model = input$population_model,
        fitted_model = input$fitted_model,
        n = parse_numeric(input$n),
        reps = input$reps,
        estimator = parse_character(input$estimator),
        std_lv = input$std_lv,
        alpha = input$alpha,
        seed = input$seed,
        metrics = input$metrics,
        study_name = input$study_name,
        research_question = input$research_question
      )
    } else {
      ols_sim_spec(
        n = parse_numeric(input$n),
        reps = input$reps,
        betas = parse_numeric(input$betas),
        predictor_correlation = parse_numeric(input$condition_rho),
        error_sd = parse_numeric(input$condition_error_sd),
        alpha = input$alpha,
        seed = input$seed,
        fitted_formula = input$fitted_formula,
        metrics = input$metrics,
        study_name = input$study_name,
        research_question = input$research_question
      )
    }
  })

  observeEvent(input$run, {
    withProgress(message = "Running simulation", value = 0, {
      out <- run_simulation_study(
        spec(),
        workers = input$workers,
        checkpoint_dir = input$checkpoint_dir,
        resume = TRUE
      )
      incProgress(0.8)
      study(out)
      incProgress(0.2)
    })
  })

  observeEvent(input$export_quarto, {
    path <- export_quarto_project(
      spec(),
      path = input$export_path,
      overwrite = TRUE,
      workers = input$workers,
      checkpoint_dir = "results/checkpoints"
    )
    export_status(paste("Exported reproducible Quarto project to:", path))
  })

  output$summary <- renderTable({
    req(study())
    study()$summary
  }, striped = TRUE, bordered = TRUE, digits = 4)

  output$apa <- renderText({
    req(study())
    paste(study()$apa_tables$markdown, collapse = "\n")
  })

  output$equations <- renderUI({
    eqs <- spec_equations(spec())
    tagList(lapply(eqs, function(eq) withMathJax(sprintf("$$%s$$", eq))))
  })

  output$equations_raw <- renderText({
    paste(spec_equations(spec()), collapse = "\n")
  })

  output$code <- renderText({
    if (identical(input$simulation_type, "sem")) {
      return(sprintf(
        paste(
          "library(mcsimr)",
          "",
          "spec <- sem_sim_spec(",
          "  population_model = %s,",
          "  fitted_model = %s,",
          "  n = c(%s),",
          "  reps = %s,",
          "  estimator = c(%s),",
          "  std_lv = %s,",
          "  alpha = %s,",
          "  seed = %s,",
          "  metrics = c(%s),",
          "  study_name = %s,",
          "  research_question = %s",
          ")",
          "",
          "study <- run_simulation_study(",
          "  spec,",
          "  workers = %s,",
          "  checkpoint_dir = %s,",
          "  resume = TRUE",
          ")",
          "summary <- study$summary",
          "apa_table <- study$apa_tables",
          "equations_latex <- study$equations_latex",
          sep = "\n"
        ),
        deparse(input$population_model),
        deparse(input$fitted_model),
        paste(parse_numeric(input$n), collapse = ", "),
        input$reps,
        paste(sprintf('"%s"', parse_character(input$estimator)), collapse = ", "),
        if (isTRUE(input$std_lv)) "TRUE" else "FALSE",
        input$alpha,
        input$seed,
        paste(sprintf('"%s"', input$metrics), collapse = ", "),
        deparse(input$study_name),
        deparse(input$research_question),
        input$workers,
        deparse(input$checkpoint_dir)
      ))
    }

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
        "  fitted_formula = %s,",
        "  metrics = c(%s),",
        "  study_name = %s,",
        "  research_question = %s",
        ")",
        "",
        "study <- run_simulation_study(",
        "  spec,",
        "  workers = %s,",
        "  checkpoint_dir = %s,",
        "  resume = TRUE",
        ")",
        "summary <- study$summary",
        "apa_table <- study$apa_tables",
        "equations_latex <- study$equations_latex",
        sep = "\n"
      ),
      paste(parse_numeric(input$n), collapse = ", "),
      input$reps,
      paste(parse_numeric(input$betas), collapse = ", "),
      paste(parse_numeric(input$condition_rho), collapse = ", "),
      paste(parse_numeric(input$condition_error_sd), collapse = ", "),
      input$alpha,
      input$seed,
      deparse(input$fitted_formula),
      paste(sprintf('"%s"', input$metrics), collapse = ", "),
      deparse(input$study_name),
      deparse(input$research_question),
      input$workers,
      deparse(input$checkpoint_dir)
    )
  })

  output$export_status <- renderText({
    export_status()
  })

  output$download_summary <- downloadHandler(
    filename = function() "mcsimr-summary.csv",
    content = function(file) {
      utils::write.csv(study()$summary, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
