library(shiny)
library(bslib)
library(mcsimr)

parse_numeric <- function(x) {
  as.numeric(trimws(strsplit(x, ",", fixed = TRUE)[[1L]]))
}

parse_character <- function(x) {
  trimws(strsplit(x, ",", fixed = TRUE)[[1L]])
}

split_csv <- function(x) {
  trimws(strsplit(x, ",", fixed = TRUE)[[1L]])
}

parse_named_lines <- function(x) {
  lines <- trimws(unlist(strsplit(x, "\n", fixed = TRUE)))
  lines <- lines[nzchar(lines)]
  out <- list()
  for (line in lines) {
    parts <- strsplit(line, ":", fixed = TRUE)[[1L]]
    if (length(parts) >= 2L) {
      out[[trimws(parts[1L])]] <- split_csv(paste(parts[-1L], collapse = ":"))
    }
  }
  out
}

strip_numeric_multipliers <- function(x) {
  x <- gsub("(^|[+~])\\s*-?[0-9.]+\\s*\\*", "\\1 ", x)
  gsub("\\s+", " ", trimws(x))
}

build_sem_syntax <- function(factor_names,
                             indicator_text,
                             loading_text,
                             factor_covariances,
                             structural_paths,
                             include_residuals = TRUE) {
  factors <- split_csv(factor_names)
  indicators <- parse_named_lines(indicator_text)
  loadings <- parse_named_lines(loading_text)

  population <- character()
  fitted <- character()

  for (factor in factors) {
    inds <- indicators[[factor]]
    if (is.null(inds) || !length(inds)) {
      next
    }
    loads <- suppressWarnings(as.numeric(loadings[[factor]]))
    if (length(loads) != length(inds) || any(is.na(loads))) {
      loads <- rep(0.70, length(inds))
    }

    population <- c(population, paste0(factor, " =~ ", paste(paste0(format(loads, trim = TRUE), "*", inds), collapse = " + ")))
    fitted <- c(fitted, paste0(factor, " =~ ", paste(inds, collapse = " + ")))
    population <- c(population, paste0(factor, " ~~ 1*", factor))

    if (include_residuals) {
      residuals <- pmax(0.001, 1 - loads^2)
      population <- c(population, paste0(inds, " ~~ ", format(residuals, trim = TRUE), "*", inds))
    }
  }

  cov_lines <- trimws(unlist(strsplit(factor_covariances, "\n", fixed = TRUE)))
  cov_lines <- cov_lines[nzchar(cov_lines)]
  population <- c(population, cov_lines)
  fitted <- c(fitted, strip_numeric_multipliers(cov_lines))

  path_lines <- trimws(unlist(strsplit(structural_paths, "\n", fixed = TRUE)))
  path_lines <- path_lines[nzchar(path_lines)]
  population <- c(population, path_lines)
  fitted <- c(fitted, strip_numeric_multipliers(path_lines))

  list(
    population = paste(population, collapse = "\n"),
    fitted = paste(fitted, collapse = "\n")
  )
}

default_population <- "f =~ 0.70*y1 + 0.80*y2 + 0.90*y3\nf ~~ 1*f\ny1 ~~ 0.51*y1\ny2 ~~ 0.36*y2\ny3 ~~ 0.19*y3"
default_fitted <- "f =~ y1 + y2 + y3"

ui <- page_sidebar(
  title = "mcsimr",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    selectInput("simulation_type", "Simulation family", choices = c("lavaan SEM" = "sem", "OLS regression" = "ols")),
    textInput("study_name", "Study name", "lavaan Monte Carlo Simulation"),
    textAreaInput("research_question", "Research question", "How does SEM parameter recovery vary across sample sizes?", rows = 3),
    textInput("n", "Sample sizes", "100, 250, 500"),
    numericInput("reps", "Replications per condition", 100, min = 1, step = 10),
    numericInput("alpha", "Alpha", 0.05, min = 0.001, max = 0.25, step = 0.001),
    numericInput("seed", "Seed", 20260608, min = 1, step = 1),
    numericInput("workers", "Workers", max(1, available_cores()), min = 1, step = 1),
    textInput("checkpoint_dir", "Checkpoint directory", "output/checkpoints/sem_app"),
    actionButton("run", "Run simulation", class = "btn-primary")
  ),
  navset_tab(
    nav_panel(
      "Model Builder",
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("SEM builder"),
          conditionalPanel(
            "input.simulation_type == 'sem'",
            textInput("factor_names", "Latent variables", "f"),
            textAreaInput("indicator_map", "Indicators by factor", "f: y1, y2, y3", rows = 3),
            textAreaInput("loading_map", "Population loadings by factor", "f: 0.70, 0.80, 0.90", rows = 3),
            textAreaInput("factor_covariances", "Factor covariances", "", rows = 3),
            textAreaInput("structural_paths", "Structural regressions", "", rows = 3),
            checkboxInput("include_residuals", "Compute residual variances from standardized loadings", TRUE),
            actionButton("build_sem", "Build lavaan syntax")
          ),
          conditionalPanel(
            "input.simulation_type == 'ols'",
            textInput("condition_rho", "Predictor correlations", "0.30"),
            textInput("condition_error_sd", "Residual SD conditions", "1"),
            textInput("betas", "True betas", "0.20, 0.30, 0.00"),
            textInput("fitted_formula", "Fitted model", "y ~ x1 + x2 + x3")
          )
        ),
        card(
          card_header("Model syntax"),
          conditionalPanel(
            "input.simulation_type == 'sem'",
            textAreaInput("population_model", "Population model", default_population, rows = 9),
            textAreaInput("fitted_model", "Fitted lavaan model", default_fitted, rows = 6),
            textInput("estimator", "Estimators", "ML"),
            checkboxInput("std_lv", "Use std.lv = TRUE", TRUE)
          ),
          card_body(
            uiOutput("equations"),
            tags$hr(),
            verbatimTextOutput("equations_raw")
          )
        )
      )
    ),
    nav_panel(
      "Results",
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Simulation summary"),
          tableOutput("summary")
        ),
        card(
          card_header("APA-style table"),
          verbatimTextOutput("apa")
        )
      )
    ),
    nav_panel(
      "Visualizations",
      layout_columns(
        col_widths = c(3, 9),
        card(
          card_header("Plot controls"),
          selectInput("plot_metric", "Metric", choices = c("bias", "rmse", "coverage", "power", "type_i_error", "mean_cfi", "mean_rmsea")),
          uiOutput("plot_term_ui")
        ),
        card(
          card_header("Metric plot"),
          plotOutput("metric_plot", height = "520px")
        )
      )
    ),
    nav_panel(
      "R Code",
      card(
        card_header("Generated reproducible R code"),
        verbatimTextOutput("code")
      )
    ),
    nav_panel(
      "Quarto Export",
      card(
        card_header("Reproducible Quarto project"),
        textInput("export_path", "Quarto export path", "output/quarto/sem_simulation_project"),
        actionButton("export_quarto", "Export Quarto project"),
        verbatimTextOutput("export_status"),
        downloadButton("download_summary", "Download summary")
      )
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

  observeEvent(input$build_sem, {
    syntax <- build_sem_syntax(
      factor_names = input$factor_names,
      indicator_text = input$indicator_map,
      loading_text = input$loading_map,
      factor_covariances = input$factor_covariances,
      structural_paths = input$structural_paths,
      include_residuals = input$include_residuals
    )
    updateTextAreaInput(session, "population_model", value = syntax$population)
    updateTextAreaInput(session, "fitted_model", value = syntax$fitted)
  })

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
        metrics = default_metrics("sem"),
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
        metrics = default_metrics("ols"),
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

  output$plot_term_ui <- renderUI({
    req(study())
    selectInput("plot_terms", "Parameters", choices = unique(study()$summary$term), selected = unique(study()$summary$term), multiple = TRUE)
  })

  output$metric_plot <- renderPlot({
    req(study())
    req(input$plot_metric %in% names(study()$summary))
    plot_metric(study()$summary, metric = input$plot_metric, term = input$plot_terms)
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
