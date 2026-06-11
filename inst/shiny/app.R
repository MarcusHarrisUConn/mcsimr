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

parse_parameter_condition_lines <- function(x) {
  lines <- trimws(unlist(strsplit(x, "\n", fixed = TRUE)))
  lines <- lines[nzchar(lines)]
  if (!length(lines)) {
    return(NULL)
  }
  parsed <- lapply(lines, function(line) {
    pieces <- strsplit(line, ":", fixed = TRUE)[[1L]]
    if (length(pieces) < 2L) {
      stop("Condition lines must look like `lhs op rhs: value1, value2`.", call. = FALSE)
    }
    term <- trimws(pieces[1L])
    values <- as.numeric(trimws(strsplit(paste(pieces[-1L], collapse = ":"), ",", fixed = TRUE)[[1L]]))
    if (any(is.na(values))) {
      stop("Condition values must be numeric: ", line, call. = FALSE)
    }
    op <- c("=~", "~~", "~1", "~")[vapply(c("=~", "~~", "~1", "~"), function(o) grepl(o, term, fixed = TRUE), logical(1L))]
    if (!length(op)) {
      stop("Could not find a lavaan operator in: ", line, call. = FALSE)
    }
    op <- op[[1L]]
    parts <- trimws(strsplit(term, op, fixed = TRUE)[[1L]])
    if (length(parts) != 2L) {
      stop("Could not parse parameter condition: ", line, call. = FALSE)
    }
    list(lhs = parts[[1L]], op = op, rhs = parts[[2L]], values = values, label = paste(parts[[1L]], op, parts[[2L]]))
  })
  sem_parameter_conditions(
    lhs = vapply(parsed, `[[`, character(1L), "lhs"),
    op = vapply(parsed, `[[`, character(1L), "op"),
    rhs = vapply(parsed, `[[`, character(1L), "rhs"),
    values = lapply(parsed, `[[`, "values"),
    label = vapply(parsed, `[[`, character(1L), "label")
  )
}

format_parameter_condition_line <- function(lhs, op, rhs, values) {
  paste0(trimws(lhs), " ", trimws(op), " ", trimws(rhs), ": ", paste(values, collapse = ", "))
}

strip_numeric_multipliers <- function(x) {
  x <- gsub("(^|[+~])\\s*-?[0-9.]+\\s*\\*", "\\1 ", x)
  gsub("\\s+", " ", trimws(x))
}

app_css <- "
:root {
  --mc-red: #4A0101;
  --mc-red-2: #7A0610;
  --mc-accent: #C1121F;
  --mc-accent-2: #F25C54;
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
body {
  background: var(--mc-bg);
  color: var(--mc-text);
}
.bslib-sidebar-layout, .card, .tab-content {
  background: transparent;
}
.card {
  border-color: var(--mc-border);
  background: var(--mc-panel);
  border-radius: 8px;
  box-shadow: 0 10px 24px rgba(74, 1, 1, 0.08);
}
.card-header {
  background: linear-gradient(90deg, var(--mc-red), var(--mc-red-2));
  color: #FFF4F1;
  border-bottom: 0;
  font-weight: 700;
}
.nav-tabs .nav-link.active {
  color: #fff;
  background: var(--mc-red);
  border-color: var(--mc-red);
}
.nav-tabs .nav-link {
  color: var(--mc-red-2);
}
body.mc-dark .nav-tabs .nav-link {
  color: #FFD5D0;
}
label, .control-label, .form-label, .shiny-input-container > label,
.checkbox label, .radio label, .form-check-label {
  color: var(--mc-text);
  font-weight: 650;
}
.shiny-input-container {
  color: var(--mc-text);
}
.btn-primary, .btn-default.action-button {
  background: var(--mc-accent);
  border-color: var(--mc-accent);
  color: #fff;
}
.btn-primary:hover, .btn-default.action-button:hover {
  background: var(--mc-red-2);
  border-color: var(--mc-red-2);
}
.form-control, .selectize-input, .selectize-control.single .selectize-input,
.selectize-control.multi .selectize-input, textarea {
  background: var(--mc-input) !important;
  color: var(--mc-text) !important;
  border-color: var(--mc-border) !important;
  box-shadow: none !important;
}
.form-control:focus, .selectize-input.focus, textarea:focus {
  border-color: var(--mc-accent-2) !important;
  box-shadow: 0 0 0 .22rem var(--mc-focus) !important;
}
.selectize-dropdown, .selectize-dropdown-content {
  background: var(--mc-input) !important;
  color: var(--mc-text) !important;
  border-color: var(--mc-border) !important;
}
.selectize-dropdown .option {
  color: var(--mc-text) !important;
}
.selectize-dropdown .active {
  background: rgba(193, 18, 31, 0.18) !important;
  color: var(--mc-text) !important;
}
.help-block, .form-text, .text-muted {
  color: var(--mc-muted) !important;
}
.theme-switch {
  margin-bottom: 1rem;
}
.theme-switch .shiny-input-container {
  width: 100%;
  margin-bottom: 0;
}
.theme-switch .checkbox {
  margin: 0;
}
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
  border-color: var(--mc-accent-2);
}
.theme-switch input[type='checkbox']:checked::after {
  transform: translateX(1.45rem);
}
.theme-switch input[type='checkbox']:focus-visible {
  outline: 3px solid var(--mc-focus);
  outline-offset: 2px;
}
.theme-switch span {
  color: var(--mc-text);
  font-weight: 750;
}
.apa-table {
  width: 100%;
  border-collapse: collapse;
  font-family: Georgia, 'Times New Roman', serif;
  font-size: 0.92rem;
  background: var(--mc-panel);
  color: var(--mc-text);
}
.apa-table caption {
  caption-side: top;
  text-align: left;
  font-weight: 700;
  color: var(--mc-text);
  padding-bottom: .65rem;
}
.apa-table thead th {
  border-top: 2px solid var(--mc-text);
  border-bottom: 1px solid var(--mc-text);
  font-weight: 700;
}
.apa-table tbody td {
  border-bottom: 1px solid var(--mc-border);
  color: var(--mc-text);
}
.apa-table tbody tr:last-child td {
  border-bottom: 2px solid var(--mc-text);
}
.apa-table th, .apa-table td {
  padding: .45rem .55rem;
  vertical-align: top;
}
.apa-note {
  font-family: Georgia, 'Times New Roman', serif;
  color: var(--mc-muted);
  margin-top: .5rem;
}
.code-preview {
  max-height: 520px;
  overflow: auto;
}
pre, .shiny-text-output, .shiny-bound-output pre {
  background: var(--mc-input) !important;
  color: var(--mc-text) !important;
  border: 1px solid var(--mc-border) !important;
  border-radius: 8px;
}
body.mc-dark code, body.mc-dark pre {
  color: #FFE7E2 !important;
}
.table, table {
  color: var(--mc-text);
}
.table > :not(caption) > * > * {
  background-color: var(--mc-panel);
  color: var(--mc-text);
  border-bottom-color: var(--mc-border);
}
.condition-preview {
  max-height: 260px;
  overflow: auto;
}
mjx-container, mjx-container * {
  color: var(--mc-text) !important;
}
body.mc-dark mjx-container, body.mc-dark mjx-container * {
  color: #FFE7E2 !important;
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

apa_table_ui <- function(apa) {
  tab <- apa$table
  header <- tags$tr(lapply(names(tab), tags$th))
  rows <- lapply(seq_len(nrow(tab)), function(i) {
    tags$tr(lapply(tab[i, , drop = TRUE], tags$td))
  })
  tagList(
    tags$table(
      class = "apa-table",
      tags$caption(strsplit(apa$caption, "\n", fixed = TRUE)[[1L]][1L]),
      tags$thead(header),
      tags$tbody(rows)
    ),
    tags$div(class = "apa-note", "Note. Values are rounded for display; downloadable files retain the generated summary output.")
  )
}

exported_files <- function(path) {
  if (is.null(path) || !dir.exists(path)) {
    return(character())
  }
  files <- list.files(path, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  files[file.info(file.path(path, files))$isdir %in% FALSE]
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
default_parameter_conditions <- "f =~ y2: 0.60, 0.80\nf ~~ f: 0.80, 1.00"
bollen_population <- "
ind60 =~ 1*x1 + 2.180*x2 + 1.819*x3
dem60 =~ 1*y1 + 1.257*y2 + 1.058*y3 + 1.265*y4
dem65 =~ 1*y5 + 1.186*y6 + 1.280*y7 + 1.266*y8
dem60 ~ 1.483*ind60
dem65 ~ 0.572*ind60 + 0.837*dem60
ind60 ~~ 0.448*ind60
dem60 ~~ 3.956*dem60
dem65 ~~ 0.172*dem65
y1 ~~ 1.892*y5
y2 ~~ 7.373*y4
y2 ~~ 2.488*y6
y3 ~~ 5.067*y7
y4 ~~ 1.706*y8
x1 ~~ 0.082*x1
x2 ~~ 0.120*x2
x3 ~~ 0.467*x3
y1 ~~ 1.891*y1
y2 ~~ 7.373*y2
y3 ~~ 5.067*y3
y4 ~~ 3.148*y4
y5 ~~ 2.351*y5
y6 ~~ 4.954*y6
y7 ~~ 3.431*y7
y8 ~~ 3.254*y8
"
bollen_fitted <- "
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
dem60 ~ ind60
dem65 ~ ind60 + dem60
y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
"
bollen_conditions <- "dem65 ~ dem60: 0.65, 0.85\nind60 =~ x2: 1.80, 2.20\ndem60 ~~ dem60: 3.00, 4.00"

ui <- page_sidebar(
  title = "mcsimr",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#7A0610"),
  tags$head(tags$style(HTML(app_css)), tags$script(HTML(app_script))),
  sidebar = sidebar(
    tags$div(class = "theme-switch", checkboxInput("theme_dark", "Dark mode", FALSE)),
    selectInput("simulation_type", "Simulation family", choices = c("lavaan SEM" = "sem", "OLS regression" = "ols")),
    textInput("study_name", "Study name", "lavaan Monte Carlo Simulation"),
    textAreaInput("research_question", "Research question", "How does SEM parameter recovery vary across sample sizes?", rows = 3),
    textInput("n", "Sample sizes", "100, 250, 500"),
    numericInput("reps", "Replications per condition", 100, min = 1, step = 10),
    numericInput("alpha", "Alpha", 0.05, min = 0.001, max = 0.25, step = 0.001),
    numericInput("seed", "Seed", 20260608, min = 1, step = 1),
    checkboxInput("use_parallel", "Use multiple cores", TRUE),
    numericInput("workers", "Workers", max(1, available_cores()), min = 1, step = 1),
    helpText(textOutput("parallel_status", inline = TRUE)),
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
            actionButton("build_sem", "Build lavaan syntax"),
            actionButton("load_bollen", "Load Political Democracy example"),
            tags$hr(),
            textInput("condition_lhs", "Condition lhs", "f"),
            selectInput("condition_op", "Condition operator", choices = c("Loading (=~)" = "=~", "Regression (~)" = "~", "Variance/covariance (~~)" = "~~", "Intercept (~1)" = "~1")),
            textInput("condition_rhs", "Condition rhs", "y2"),
            textInput("condition_values", "Condition values", "0.60, 0.80"),
            layout_columns(
              col_widths = c(6, 6),
              actionButton("add_condition", "Add condition"),
              actionButton("clear_conditions", "Clear conditions")
            ),
            tableOutput("visual_conditions")
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
            selectInput("missing", "lavaan missing method", choices = c("listwise", "fiml", "ml", "direct"), selected = "listwise"),
            textInput("missing_rate", "MCAR missing rates", "0"),
            textInput("skewness", "Observed-variable skewness", "0"),
            textInput("kurtosis", "Observed-variable excess kurtosis", "0"),
            checkboxInput("std_lv", "Use std.lv = TRUE", TRUE),
            textAreaInput("parameter_conditions", "Parameter conditions", default_parameter_conditions, rows = 5),
            tags$div(class = "condition-preview", tableOutput("condition_grid"))
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
          uiOutput("apa_pretty"),
          tags$hr(),
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
      "Run Dashboard",
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("Current run"),
          uiOutput("run_status_cards")
        ),
        card(
          card_header("Run log"),
          tableOutput("run_log")
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
        selectInput("export_file", "Project file", choices = character()),
        layout_columns(
          col_widths = c(6, 6),
          downloadButton("download_export_file", "Download selected file"),
          downloadButton("download_export_zip", "Download project zip")
        ),
        tags$hr(),
        verbatimTextOutput("export_file_preview"),
        tags$hr(),
        downloadButton("download_summary", "Download summary")
      )
    )
  )
)

server <- function(input, output, session) {
  study <- reactiveVal(NULL)
  export_status <- reactiveVal("No project exported yet.")
  exported_path <- reactiveVal(NULL)
  run_log <- reactiveVal(data.frame(
    time = character(),
    status = character(),
    message = character(),
    stringsAsFactors = FALSE
  ))
  visual_conditions <- reactiveVal(data.frame(
    lhs = character(),
    op = character(),
    rhs = character(),
    values = character(),
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

  active_workers <- reactive({
    if (isTRUE(input$use_parallel)) {
      max(1L, as.integer(input$workers))
    } else {
      1L
    }
  })

  output$parallel_status <- renderText({
    paste0(
      "Detected ", future::availableCores()[[1L]], " cores; this run will use ",
      active_workers(), " worker", if (active_workers() == 1L) "." else "s."
    )
  })

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

  observeEvent(input$load_bollen, {
    updateTextAreaInput(session, "population_model", value = trimws(bollen_population))
    updateTextAreaInput(session, "fitted_model", value = trimws(bollen_fitted))
    updateTextAreaInput(session, "parameter_conditions", value = bollen_conditions)
    updateTextInput(session, "factor_names", value = "ind60, dem60, dem65")
    updateTextAreaInput(session, "indicator_map", value = "ind60: x1, x2, x3\ndem60: y1, y2, y3, y4\ndem65: y5, y6, y7, y8")
    updateTextAreaInput(session, "loading_map", value = "ind60: 1, 2.180, 1.819\ndem60: 1, 1.257, 1.058, 1.265\ndem65: 1, 1.186, 1.280, 1.266")
    updateTextAreaInput(session, "factor_covariances", value = "y1 ~~ 1.892*y5\ny2 ~~ 7.373*y4\ny2 ~~ 2.488*y6\ny3 ~~ 5.067*y7\ny4 ~~ 1.706*y8")
    updateTextAreaInput(session, "structural_paths", value = "dem60 ~ 1.483*ind60\ndem65 ~ 0.572*ind60 + 0.837*dem60")
    visual_conditions(data.frame(
      lhs = c("dem65", "ind60", "dem60"),
      op = c("~", "=~", "~~"),
      rhs = c("dem60", "x2", "dem60"),
      values = c("0.65, 0.85", "1.80, 2.20", "3.00, 4.00"),
      stringsAsFactors = FALSE
    ))
  })

  observeEvent(input$add_condition, {
    vals <- parse_numeric(input$condition_values)
    line <- format_parameter_condition_line(input$condition_lhs, input$condition_op, input$condition_rhs, vals)
    current <- trimws(input$parameter_conditions)
    updated <- if (nzchar(current)) paste(current, line, sep = "\n") else line
    updateTextAreaInput(session, "parameter_conditions", value = updated)
    visual_conditions(rbind(
      visual_conditions(),
      data.frame(
        lhs = input$condition_lhs,
        op = input$condition_op,
        rhs = input$condition_rhs,
        values = paste(vals, collapse = ", "),
        stringsAsFactors = FALSE
      )
    ))
  })

  observeEvent(input$clear_conditions, {
    updateTextAreaInput(session, "parameter_conditions", value = "")
    visual_conditions(visual_conditions()[0, , drop = FALSE])
  })

  output$visual_conditions <- renderTable({
    visual_conditions()
  }, striped = TRUE, bordered = TRUE)

  parameter_conditions <- reactive({
    if (!identical(input$simulation_type, "sem")) {
      return(NULL)
    }
    parse_parameter_condition_lines(input$parameter_conditions)
  })

  spec <- reactive({
    if (identical(input$simulation_type, "sem")) {
      sem_sim_spec(
        population_model = input$population_model,
        fitted_model = input$fitted_model,
        n = parse_numeric(input$n),
        reps = input$reps,
        estimator = parse_character(input$estimator),
        parameter_conditions = parameter_conditions(),
        missing_rate = parse_numeric(input$missing_rate),
        skewness = parse_numeric(input$skewness),
        kurtosis = parse_numeric(input$kurtosis),
        missing = input$missing,
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

  output$condition_grid <- renderTable({
    req(identical(input$simulation_type, "sem"))
    sem_condition_grid(spec())[c(
      "condition_id", "n", "estimator", "missing_rate", "skewness", "kurtosis",
      "parameter_conditions"
    )]
  }, striped = TRUE, bordered = TRUE)

  observeEvent(input$run, {
    grid <- if (identical(spec()$type, "sem")) sem_condition_grid(spec()) else NULL
    append_run_log(
      "queued",
      paste0(
        "Prepared ", if (is.null(grid)) "OLS" else nrow(grid),
        " condition", if (!is.null(grid) && nrow(grid) == 1L) "" else "s",
        " x ", input$reps, " replications on ", active_workers(), " worker",
        if (active_workers() == 1L) "." else "s."
      )
    )
    withProgress(message = "Running simulation", value = 0, {
      append_run_log("running", paste("Checkpoint directory:", input$checkpoint_dir))
      out <- run_simulation_study(
        spec(),
        workers = active_workers(),
        checkpoint_dir = input$checkpoint_dir,
        resume = TRUE
      )
      incProgress(0.8)
      study(out)
      append_run_log("completed", paste("Finished", nrow(out$summary), "summary rows."))
      incProgress(0.2)
    })
  })

  observeEvent(input$export_quarto, {
    path <- export_quarto_project(
      spec(),
      path = input$export_path,
      overwrite = TRUE,
      workers = active_workers(),
      checkpoint_dir = "results/checkpoints"
    )
    exported_path(path)
    updateSelectInput(session, "export_file", choices = exported_files(path))
    export_status(paste("Exported reproducible Quarto project to:", path))
    append_run_log("exported", paste("Quarto project:", path))
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
      tags$p(tags$strong("Workers"), tags$br(), active_workers()),
      tags$p(tags$strong("Checkpointing"), tags$br(), input$checkpoint_dir)
    )
  })

  output$summary <- renderTable({
    req(study())
    study()$summary
  }, striped = TRUE, bordered = TRUE, digits = 4)

  output$apa <- renderText({
    req(study())
    paste(study()$apa_tables$markdown, collapse = "\n")
  })

  output$apa_pretty <- renderUI({
    req(study())
    apa_table_ui(study()$apa_tables)
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
      pc <- parameter_conditions()
      if (is.null(pc)) {
        pc <- sem_parameter_conditions()
      }
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
          "  missing = %s,",
          "  missing_rate = c(%s),",
          "  skewness = c(%s),",
          "  kurtosis = c(%s),",
          "  parameter_conditions = sem_parameter_conditions(",
          "    lhs = c(%s),",
          "    op = c(%s),",
          "    rhs = c(%s),",
          "    values = list(%s)",
          "  ),",
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
        deparse(input$missing),
        paste(parse_numeric(input$missing_rate), collapse = ", "),
        paste(parse_numeric(input$skewness), collapse = ", "),
        paste(parse_numeric(input$kurtosis), collapse = ", "),
        paste(sprintf('"%s"', pc$lhs), collapse = ", "),
        paste(sprintf('"%s"', pc$op), collapse = ", "),
        paste(sprintf('"%s"', pc$rhs), collapse = ", "),
        paste(vapply(pc$values, function(x) paste0("c(", paste(x, collapse = ", "), ")"), character(1L)), collapse = ", "),
        if (isTRUE(input$std_lv)) "TRUE" else "FALSE",
        input$alpha,
        input$seed,
        deparse(input$study_name),
        deparse(input$research_question),
        active_workers(),
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
      active_workers(),
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

  output$export_file_preview <- renderText({
    path <- exported_path()
    req(path, input$export_file)
    file <- file.path(path, input$export_file)
    if (!file.exists(file)) {
      return("")
    }
    ext <- tolower(tools::file_ext(file))
    if (!ext %in% c("r", "qmd", "yml", "yaml", "md", "tex", "txt", "csv")) {
      return("Preview is available for text files only.")
    }
    paste(readLines(file, warn = FALSE, n = 250L), collapse = "\n")
  })

  output$download_export_file <- downloadHandler(
    filename = function() {
      req(input$export_file)
      basename(input$export_file)
    },
    content = function(file) {
      path <- exported_path()
      req(path, input$export_file)
      file.copy(file.path(path, input$export_file), file, overwrite = TRUE)
    }
  )

  output$download_export_zip <- downloadHandler(
    filename = function() "mcsimr-quarto-project.zip",
    content = function(file) {
      path <- exported_path()
      req(path)
      old <- setwd(path)
      on.exit(setwd(old), add = TRUE)
      files <- list.files(".", recursive = TRUE, all.files = TRUE, no.. = TRUE)
      utils::zip(zipfile = file, files = files)
    }
  )
}

shinyApp(ui, server)
