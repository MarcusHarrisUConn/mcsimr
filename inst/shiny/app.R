library(shiny)
library(bslib)
library(mcsimr)

split_csv <- function(x) {
  vals <- trimws(unlist(strsplit(as.character(x), ",", fixed = TRUE)))
  vals[nzchar(vals)]
}

parse_numeric <- function(x) {
  suppressWarnings(as.numeric(split_csv(x)))
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

numeric_input_issue <- function(label, value, required = TRUE, min = NULL, max = NULL,
                                lower_open = FALSE, upper_open = FALSE) {
  tokens <- split_csv(value)
  if (!length(tokens)) {
    if (required) {
      return(data.frame(level = "error", field = label, message = "Enter at least one numeric value.", stringsAsFactors = FALSE))
    }
    return(NULL)
  }
  numbers <- suppressWarnings(as.numeric(tokens))
  if (any(is.na(numbers))) {
    bad <- tokens[is.na(numbers)]
    return(data.frame(
      level = "error",
      field = label,
      message = paste("Could not read numeric value(s):", paste(bad, collapse = ", ")),
      stringsAsFactors = FALSE
    ))
  }
  if (!is.null(min)) {
    too_low <- if (lower_open) numbers <= min else numbers < min
    if (any(too_low)) {
      relation <- if (lower_open) "greater than" else "at least"
      return(data.frame(
        level = "error",
        field = label,
        message = paste("Values must be", relation, min, "."),
        stringsAsFactors = FALSE
      ))
    }
  }
  if (!is.null(max)) {
    too_high <- if (upper_open) numbers >= max else numbers > max
    if (any(too_high)) {
      relation <- if (upper_open) "less than" else "at most"
      return(data.frame(
        level = "error",
        field = label,
        message = paste("Values must be", relation, max, "."),
        stringsAsFactors = FALSE
      ))
    }
  }
  NULL
}

combine_issue_rows <- function(rows) {
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(data.frame(level = "ok", field = "Inputs", message = "No blocking input issues detected.", stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
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

format_parameter_conditions_for_app <- function(parameter_conditions) {
  if (is.null(parameter_conditions) || !nrow(parameter_conditions)) {
    return("")
  }
  paste(
    vapply(seq_len(nrow(parameter_conditions)), function(i) {
      format_parameter_condition_line(
        parameter_conditions$lhs[[i]],
        parameter_conditions$op[[i]],
        parameter_conditions$rhs[[i]],
        parameter_conditions$values[[i]]
      )
    }, character(1L)),
    collapse = "\n"
  )
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
.card, .card p, .card strong, .card span, .card-body, .card-body div, .shiny-html-output,
.shiny-bound-output, .shiny-bound-output p, .shiny-bound-output strong {
  color: var(--mc-text) !important;
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
.design-alert-ok {
  border-left-color: #2F855A;
}
.design-alert-info {
  border-left-color: #B7791F;
}
.design-alert-warning {
  border-left-color: var(--mc-accent-2);
}
.parameter-picker {
  margin-bottom: .85rem;
}
mjx-container, mjx-container * {
  color: var(--mc-text) !important;
}
body.mc-dark mjx-container, body.mc-dark mjx-container * {
  color: #FFE7E2 !important;
}
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

default_population <- "f =~ 0.70*y1 + 0.80*y2 + 0.90*y3 + 0.50*y4\nf ~~ 1*f\ny1 ~~ 0.51*y1\ny2 ~~ 0.36*y2\ny3 ~~ 0.19*y3\ny4 ~~ 0.75*y4"
default_fitted <- "f =~ y1 + y2 + y3 + y4"
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
    selectInput("readiness_mode", "Readiness mode", choices = c("Teaching / pilot" = "teaching", "Publication" = "publication")),
    textInput("study_name", "Study name", "lavaan Monte Carlo Simulation"),
    textAreaInput("research_question", "Research question", "How does SEM parameter recovery vary across sample sizes?", rows = 3),
    textAreaInput("design_rationale", "Design rationale", "Sample size, estimator, missingness, nonnormality, and parameter conditions were selected to evaluate SEM performance across theoretically relevant design scenarios.", rows = 3),
    textAreaInput("metric_rationale", "Metric rationale", "Bias, precision, coverage, convergence, improper solutions, and fit indices summarize parameter recovery and model performance.", rows = 3),
    textAreaInput("interpretation_plan", "Interpretation plan", "Interpret results by condition, emphasizing whether conclusions are robust across sample size, missingness, nonnormality, and parameter values.", rows = 3),
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
      "Instructions",
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("SEM workflow"),
          tags$ol(
            class = "instruction-list",
            tags$li("Choose lavaan SEM in the sidebar."),
            tags$li("Enter latent variables, indicators, and population loadings, or load the Political Democracy example."),
            tags$li("Click Build lavaan syntax to generate editable population and fitted lavaan syntax."),
            tags$li("Set sample sizes, replications, seed, missingness, nonnormality, and parameter conditions."),
            tags$li("Choose workers and a checkpoint directory, then click Run simulation."),
            tags$li("Review results, figures, generated R code, and the Quarto export.")
          )
        ),
        card(
          card_header("Residual variances"),
          tags$p("When residual variances are generated from standardized loadings, the app assumes each observed indicator has variance 1 and the latent factor variance is 1."),
          tags$p("For a loading of .70, the residual variance is 1 - .70^2 = .51. For a loading of .50, it is 1 - .50^2 = .75."),
          tags$p("Edit the population model directly when your simulation requires different residual variances, correlated residuals, or nonstandardized indicators.")
        )
      )
    ),
    nav_panel(
      "Model Builder",
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("SEM builder"),
          conditionalPanel(
            "input.simulation_type == 'sem'",
            selectInput(
              "sem_preset",
              "SEM preset",
              choices = stats::setNames(sem_model_presets()$name, sem_model_presets()$title),
              selected = "one_factor_cfa"
            ),
            actionButton("load_preset", "Load preset"),
            tags$hr(),
            textInput("factor_names", "Latent variables", "f"),
            textAreaInput("indicator_map", "Indicators by factor", "f: y1, y2, y3, y4", rows = 3),
            textAreaInput("loading_map", "Population loadings by factor", "f: 0.70, 0.80, 0.90, 0.50", rows = 3),
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
            selectInput(
              "misspecification",
              "Fitted-model misspecification",
              choices = stats::setNames(sem_misspecification_presets()$name, sem_misspecification_presets()$title),
              selected = "none"
            ),
            textInput("estimator", "Estimators", "ML"),
            selectInput("missing", "lavaan missing method", choices = c("listwise", "fiml", "ml", "direct"), selected = "listwise"),
            textInput("missing_rate", "Missing-data rates", "0"),
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
            textInput("group_variable", "Group variable", ""),
            textInput("group_labels", "Group labels", ""),
            textInput("group_proportions", "Group proportions", ""),
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
      "Conditions",
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("Design summary"),
          tableOutput("design_summary"),
          uiOutput("design_warnings")
        ),
        card(
          card_header("Input audit"),
          tableOutput("input_audit")
        ),
        card(
          card_header("Design size"),
          tableOutput("design_size")
        ),
        card(
          card_header("Validation"),
          tableOutput("design_validation")
        ),
        card(
          card_header("Readiness review"),
          tableOutput("readiness_review")
        ),
        card(
          card_header("Readiness decision"),
          tableOutput("pre_run_readiness_decision")
        ),
        card(
          card_header("Model parameters to vary"),
          tags$div(class = "parameter-picker", uiOutput("candidate_parameter_ui")),
          layout_columns(
            col_widths = c(6, 6),
            actionButton("use_candidate_parameter", "Use selected parameter"),
            actionButton("add_candidate_condition", "Add selected condition")
          ),
          textInput("candidate_values", "Condition values for selected parameter", "0.50, 0.70, 0.90"),
          tags$div(class = "condition-preview", tableOutput("parameter_catalog"))
        ),
        card(
          card_header("Full condition grid"),
          tags$div(class = "condition-preview", tableOutput("condition_grid_full"))
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
          selectInput("plot_type", "Plot type", choices = c("Line plot" = "line", "Heatmap" = "heatmap")),
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
        col_widths = c(4, 8, 6, 6, 12),
        card(
          card_header("Current run"),
          uiOutput("run_status_cards"),
          actionButton("refresh_manifest", "Refresh manifest"),
          actionButton("retry_failed", "Retry failed conditions")
        ),
        card(
          card_header("Run log"),
          tableOutput("run_log")
        ),
        card(
          card_header("Failure summary"),
          tableOutput("failure_summary")
        ),
        card(
          card_header("Runtime estimate"),
          tableOutput("runtime_estimate")
        ),
        card(
          card_header("Condition manifest"),
          tags$div(class = "condition-preview", tableOutput("run_manifest"))
        )
      )
    ),
    nav_panel(
      "Publication Readiness",
      layout_columns(
        col_widths = c(4, 8, 6, 6, 12),
        card(
          card_header("Readiness snapshot"),
          uiOutput("publication_status_cards")
        ),
        card(
          card_header("Automated diagnostics"),
          tableOutput("diagnostics")
        ),
        card(
          card_header("Readiness review"),
          tableOutput("publication_readiness")
        ),
        card(
          card_header("Readiness decision"),
          tableOutput("publication_readiness_decision")
        ),
        card(
          card_header("Diagnostic plot"),
          plotOutput("diagnostics_plot", height = "360px")
        ),
        card(
          card_header("Recommended next steps"),
          tableOutput("publication_recommendations")
        ),
        card(
          card_header("Publication summary"),
          verbatimTextOutput("publication_summary")
        ),
        card(
          card_header("Reporting checklist"),
          tableOutput("reporting_checklist")
        ),
        card(
          card_header("Reproducibility"),
          tableOutput("reproducibility_summary")
        ),
        card(
          card_header("Download publication artifacts"),
          layout_columns(
            col_widths = c(4, 4, 4, 4, 4, 4, 6),
            downloadButton("download_diagnostics", "Download diagnostics"),
            downloadButton("download_readiness", "Download readiness"),
            downloadButton("download_readiness_decision", "Download decision"),
            downloadButton("download_checklist", "Download checklist"),
            downloadButton("download_recommendations", "Download recommendations"),
            downloadButton("download_summary_text", "Download summary"),
            downloadButton("download_reproducibility", "Download reproducibility YAML")
          )
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
  manifest <- reactiveVal(data.frame())
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

  input_issues <- reactive({
    rows <- list(
      numeric_input_issue("Sample sizes", input$n, min = 1, lower_open = TRUE),
      numeric_input_issue("Replications per condition", input$reps, min = 1),
      numeric_input_issue("Alpha", input$alpha, min = 0, max = 1, lower_open = TRUE, upper_open = TRUE),
      numeric_input_issue("Seed", input$seed, min = 1),
      numeric_input_issue("Workers", active_workers(), min = 1)
    )

    if (identical(input$simulation_type, "sem")) {
      rows <- c(rows, list(
        numeric_input_issue("Missing-data rates", input$missing_rate, min = 0, max = 1, upper_open = TRUE),
        numeric_input_issue("Missingness slope", input$missing_slope, required = TRUE),
        numeric_input_issue("Observed-variable skewness", input$skewness, required = TRUE),
        numeric_input_issue("Observed-variable excess kurtosis", input$kurtosis, required = TRUE),
        numeric_input_issue("Group proportions", input$group_proportions, required = FALSE, min = 0, lower_open = TRUE)
      ))
      parsed_conditions <- tryCatch(parameter_conditions(), error = identity)
      if (inherits(parsed_conditions, "error")) {
        rows <- c(rows, list(data.frame(
          level = "error",
          field = "Parameter conditions",
          message = conditionMessage(parsed_conditions),
          stringsAsFactors = FALSE
        )))
      }
      missing_rates <- parse_numeric(input$missing_rate)
      if ("mar" %in% parse_character(input$missing_mechanism) &&
          any(!is.na(missing_rates) & missing_rates > 0) &&
          is.null(parse_optional_character(input$missing_driver))) {
        rows <- c(rows, list(data.frame(
          level = "review",
          field = "MAR driver",
          message = "MAR missingness is selected without a driver variable.",
          stringsAsFactors = FALSE
        )))
      }
    } else {
      rows <- c(rows, list(
        numeric_input_issue("True betas", input$betas, required = TRUE),
        numeric_input_issue("Predictor correlations", input$condition_rho, min = -1, max = 1, lower_open = TRUE, upper_open = TRUE),
        numeric_input_issue("Residual SD conditions", input$condition_error_sd, min = 0, lower_open = TRUE)
      ))
    }
    combine_issue_rows(rows)
  })

  blocking_input_issues <- reactive({
    issues <- input_issues()
    issues[issues$level == "error", , drop = FALSE]
  })

  load_sem_preset <- function(name) {
    preset <- sem_model_preset(name)
    updateTextAreaInput(session, "population_model", value = preset$population_model)
    updateTextAreaInput(session, "fitted_model", value = preset$fitted_model)
    updateTextAreaInput(session, "parameter_conditions", value = format_parameter_conditions_for_app(preset$parameter_conditions))
    updateTextInput(session, "factor_names", value = preset$builder$factor_names)
    updateTextAreaInput(session, "indicator_map", value = preset$builder$indicator_map)
    updateTextAreaInput(session, "loading_map", value = preset$builder$loading_map)
    updateTextAreaInput(session, "factor_covariances", value = preset$builder$factor_covariances)
    updateTextAreaInput(session, "structural_paths", value = preset$builder$structural_paths)
    if (!is.null(preset$parameter_conditions) && nrow(preset$parameter_conditions)) {
      visual_conditions(data.frame(
        lhs = preset$parameter_conditions$lhs,
        op = preset$parameter_conditions$op,
        rhs = preset$parameter_conditions$rhs,
        values = vapply(preset$parameter_conditions$values, function(x) paste(x, collapse = ", "), character(1L)),
        stringsAsFactors = FALSE
      ))
    } else {
      visual_conditions(visual_conditions()[0, , drop = FALSE])
    }
  }

  observeEvent(input$load_preset, {
    load_sem_preset(input$sem_preset)
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
    updateSelectInput(session, "sem_preset", selected = "bollen_political_democracy")
    load_sem_preset("bollen_political_democracy")
  })

  observeEvent(input$add_condition, {
    issue <- numeric_input_issue("Condition values", input$condition_values, required = TRUE)
    if (!is.null(issue)) {
      showNotification(paste(issue$field[[1L]], issue$message[[1L]], sep = ": "), type = "error")
      return(invisible(FALSE))
    }
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

  candidate_parameters <- reactive({
    if (!identical(input$simulation_type, "sem")) {
      return(NULL)
    }
    tryCatch(
      sem_model_parameters(input$population_model),
      error = function(e) {
        attr(e, "mcsimr_message") <- conditionMessage(e)
        e
      }
    )
  })

  output$candidate_parameter_ui <- renderUI({
    pars <- candidate_parameters()
    if (inherits(pars, "error")) {
      return(tags$p(attr(pars, "mcsimr_message")))
    }
    if (is.null(pars) || !nrow(pars)) {
      return(tags$p("No lavaan parameters were parsed from the population model."))
    }
    keys <- paste(pars$lhs, pars$op, pars$rhs, sep = "|")
    labels <- paste0(
      pars$label,
      ifelse(is.na(pars$value), "", paste0(" [", format(pars$value, trim = TRUE), "]"))
    )
    selectInput(
      "candidate_parameter",
      "Parameter",
      choices = stats::setNames(keys, labels),
      selected = keys[[1L]]
    )
  })

  output$parameter_catalog <- renderTable({
    pars <- candidate_parameters()
    if (inherits(pars, "error")) {
      return(data.frame(message = attr(pars, "mcsimr_message"), stringsAsFactors = FALSE))
    }
    if (is.null(pars) || !nrow(pars)) {
      return(data.frame(message = "No parameters available.", stringsAsFactors = FALSE))
    }
    out <- pars
    out$value <- ifelse(is.na(out$value), "", format(out$value, trim = TRUE))
    out$free <- ifelse(out$free, "estimated", "fixed")
    out[, c("label", "value", "free"), drop = FALSE]
  }, striped = TRUE, bordered = TRUE)

  use_selected_parameter <- function(values = NULL, add = FALSE) {
    req(input$candidate_parameter)
    parts <- strsplit(input$candidate_parameter, "|", fixed = TRUE)[[1L]]
    if (length(parts) != 3L) {
      return(invisible(FALSE))
    }
    updateTextInput(session, "condition_lhs", value = parts[[1L]])
    updateSelectInput(session, "condition_op", selected = parts[[2L]])
    updateTextInput(session, "condition_rhs", value = parts[[3L]])
    if (!is.null(values)) {
      updateTextInput(session, "condition_values", value = values)
    }
    if (isTRUE(add)) {
      issue <- numeric_input_issue("Selected-parameter condition values", values, required = TRUE)
      if (!is.null(issue)) {
        showNotification(paste(issue$field[[1L]], issue$message[[1L]], sep = ": "), type = "error")
        return(invisible(FALSE))
      }
      vals <- parse_numeric(values)
      line <- format_parameter_condition_line(parts[[1L]], parts[[2L]], parts[[3L]], vals)
      current <- trimws(input$parameter_conditions)
      updated <- if (nzchar(current)) paste(current, line, sep = "\n") else line
      updateTextAreaInput(session, "parameter_conditions", value = updated)
      visual_conditions(rbind(
        visual_conditions(),
        data.frame(
          lhs = parts[[1L]],
          op = parts[[2L]],
          rhs = parts[[3L]],
          values = paste(vals, collapse = ", "),
          stringsAsFactors = FALSE
        )
      ))
    }
    invisible(TRUE)
  }

  observeEvent(input$use_candidate_parameter, {
    use_selected_parameter(values = input$candidate_values, add = FALSE)
  })

  observeEvent(input$add_candidate_condition, {
    use_selected_parameter(values = input$candidate_values, add = TRUE)
  })

  parameter_conditions <- reactive({
    if (!identical(input$simulation_type, "sem")) {
      return(NULL)
    }
    parse_parameter_condition_lines(input$parameter_conditions)
  })

  spec <- reactive({
    issues <- blocking_input_issues()
    validate(need(!nrow(issues), paste(issues$field[[1L]], issues$message[[1L]], sep = ": ")))
    if (identical(input$simulation_type, "sem")) {
    sem_sim_spec(
        population_model = input$population_model,
        fitted_model = input$fitted_model,
        n = parse_numeric(input$n),
        reps = input$reps,
        estimator = parse_character(input$estimator),
        parameter_conditions = parameter_conditions(),
        missing_rate = parse_numeric(input$missing_rate),
        missing_mechanism = parse_character(input$missing_mechanism),
        missing_targets = parse_optional_character(input$missing_targets),
        missing_driver = parse_optional_character(input$missing_driver),
        missing_slope = input$missing_slope,
        misspecification = input$misspecification,
        group_variable = parse_optional_character(input$group_variable),
        group_labels = parse_optional_character(input$group_labels),
        group_proportions = if (is.null(parse_optional_character(input$group_proportions))) NULL else parse_numeric(input$group_proportions),
        skewness = parse_numeric(input$skewness),
        kurtosis = parse_numeric(input$kurtosis),
        missing = input$missing,
        std_lv = input$std_lv,
        alpha = input$alpha,
        seed = input$seed,
        metrics = default_metrics("sem"),
        study_name = input$study_name,
        research_question = input$research_question,
        design_rationale = input$design_rationale,
        metric_rationale = input$metric_rationale,
        interpretation_plan = input$interpretation_plan,
        readiness_mode = input$readiness_mode
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
        research_question = input$research_question,
        design_rationale = input$design_rationale,
        metric_rationale = input$metric_rationale,
        interpretation_plan = input$interpretation_plan,
        readiness_mode = input$readiness_mode
      )
    }
  })

  output$condition_grid <- renderTable({
    req(identical(input$simulation_type, "sem"))
    sem_condition_grid(spec())[c(
      "condition_id", "n", "estimator", "missing_rate", "missing_mechanism",
      "skewness", "kurtosis",
      "parameter_conditions"
    )]
  }, striped = TRUE, bordered = TRUE)

  output$design_summary <- renderTable({
    current_spec <- spec()
    if (identical(current_spec$type, "sem")) {
      sem_design_summary(current_spec)
    } else {
      data.frame(
        item = c("Model family", "Sample sizes", "Condition factors", "Replications"),
        value = c(
          "OLS regression",
          paste(current_spec$n, collapse = ", "),
          "sample size, predictor correlation, residual SD",
          current_spec$reps
        ),
        stringsAsFactors = FALSE
      )
    }
  }, striped = TRUE, bordered = TRUE)

  output$design_warnings <- renderUI({
    if (!identical(input$simulation_type, "sem")) {
      return(NULL)
    }
    notes <- sem_design_warnings(spec())
    tagList(lapply(seq_len(nrow(notes)), function(i) {
      tags$div(
        class = paste("design-alert", paste0("design-alert-", notes$level[[i]])),
        notes$message[[i]]
      )
    }))
  })

  output$design_validation <- renderTable({
    validate_simulation_design(spec())
  }, striped = TRUE, bordered = TRUE)

  output$input_audit <- renderTable({
    input_issues()
  }, striped = TRUE, bordered = TRUE)

  output$design_size <- renderTable({
    current_spec <- spec()
    grid <- if (identical(current_spec$type, "sem")) {
      sem_condition_grid(current_spec)
    } else {
      condition_grid(current_spec)
    }
    total_fits <- nrow(grid) * current_spec$reps
    data.frame(
      item = c("Conditions", "Replications per condition", "Total model fits", "Workers", "Readiness mode"),
      value = c(nrow(grid), current_spec$reps, total_fits, active_workers(), current_spec$readiness_mode),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE)

  output$readiness_review <- renderTable({
    simulation_readiness(spec(), mode = input$readiness_mode)
  }, striped = TRUE, bordered = TRUE)

  output$pre_run_readiness_decision <- renderTable({
    readiness_decision(simulation_readiness(spec(), mode = input$readiness_mode))
  }, striped = TRUE, bordered = TRUE)

  output$condition_grid_full <- renderTable({
    current_spec <- spec()
    if (identical(current_spec$type, "sem")) {
      sem_condition_grid(current_spec)
    } else {
      condition_grid(current_spec)
    }
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
      manifest(out$run_manifest)
      append_run_log("completed", paste("Finished", nrow(out$summary), "summary rows."))
      incProgress(0.2)
    })
  })

  observeEvent(input$refresh_manifest, {
    manifest(read_run_manifest(input$checkpoint_dir))
    append_run_log("manifest", paste("Refreshed manifest from:", input$checkpoint_dir))
  })

  observeEvent(input$retry_failed, {
    append_run_log("retry", "Retrying failed conditions from manifest.")
    out <- retry_failed_conditions(
      spec(),
      checkpoint_dir = input$checkpoint_dir,
      workers = active_workers()
    )
    manifest(read_run_manifest(input$checkpoint_dir))
    append_run_log("retry", paste("Retry produced", nrow(out), "raw result rows."))
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

  output$run_manifest <- renderTable({
    current <- manifest()
    if (!nrow(current)) {
      current <- read_run_manifest(input$checkpoint_dir)
    }
    if (!nrow(current)) {
      return(data.frame(message = "No run manifest has been written yet.", stringsAsFactors = FALSE))
    }
    keep <- intersect(
      c(
        "condition_id", "status", "attempts", "n_rows", "started_at", "finished_at",
        "duration_seconds", "resumed_from_checkpoint", "error"
      ),
      names(current)
    )
    current[, keep, drop = FALSE]
  }, striped = TRUE, bordered = TRUE)

  output$failure_summary <- renderTable({
    current <- manifest()
    if (!nrow(current)) {
      current <- read_run_manifest(input$checkpoint_dir)
    }
    out <- run_failure_summary(current)
    if (!nrow(out)) {
      return(data.frame(message = "No manifest has been written yet.", stringsAsFactors = FALSE))
    }
    out
  }, striped = TRUE, bordered = TRUE, digits = 2)

  output$runtime_estimate <- renderTable({
    current <- manifest()
    if (!nrow(current)) {
      current <- read_run_manifest(input$checkpoint_dir)
    }
    out <- runtime_estimate_from_manifest(current)
    if (!nrow(out)) {
      return(data.frame(message = "Run a pilot or refresh the manifest to estimate time.", stringsAsFactors = FALSE))
    }
    out
  }, striped = TRUE, bordered = TRUE, digits = 2)

  output$run_status_cards <- renderUI({
    current <- run_log()
    last <- if (nrow(current)) current[nrow(current), , drop = FALSE] else NULL
    current_manifest <- manifest()
    if (!nrow(current_manifest)) {
      current_manifest <- read_run_manifest(input$checkpoint_dir)
    }
    manifest_summary <- if (nrow(current_manifest)) {
      counts <- table(current_manifest$status, useNA = "ifany")
      paste(paste(names(counts), as.integer(counts), sep = ": "), collapse = "; ")
    } else {
      "No manifest yet."
    }
    tagList(
      tags$p(tags$strong("Status"), tags$br(), if (is.null(last)) "Idle" else last$status),
      tags$p(tags$strong("Last update"), tags$br(), if (is.null(last)) "No activity yet." else last$time),
      tags$p(tags$strong("Workers"), tags$br(), active_workers()),
      tags$p(tags$strong("Checkpointing"), tags$br(), input$checkpoint_dir),
      tags$p(tags$strong("Manifest"), tags$br(), manifest_summary)
    )
  })

  current_diagnostics <- reactive({
    req(study())
    study()$diagnostics
  })

  current_checklist <- reactive({
    req(study())
    study()$reporting_checklist
  })

  current_reproducibility <- reactive({
    req(study())
    study()$reproducibility
  })

  current_readiness <- reactive({
    req(study())
    study()$readiness
  })

  current_readiness_decision <- reactive({
    req(study())
    study()$readiness_decision
  })

  current_recommendations <- reactive({
    req(study())
    study()$publication_recommendations
  })

  current_publication_summary <- reactive({
    req(study())
    study()$publication_summary
  })

  output$summary <- renderTable({
    req(study())
    study()$summary
  }, striped = TRUE, bordered = TRUE, digits = 4)

  output$publication_status_cards <- renderUI({
    req(study())
    diagnostics <- current_diagnostics()
    checklist <- current_checklist()
    severity_counts <- table(diagnostics$severity, useNA = "ifany")
    checklist_counts <- table(checklist$status, useNA = "ifany")
    high_priority <- sum(current_recommendations()$priority == "high", na.rm = TRUE)
    readiness_counts <- table(current_readiness()$level, useNA = "ifany")
    decision <- current_readiness_decision()
    tagList(
      tags$p(tags$strong("Decision"), tags$br(), decision$decision[[1]]),
      tags$p(tags$strong("Diagnostics"), tags$br(), paste(paste(names(severity_counts), as.integer(severity_counts), sep = ": "), collapse = "; ")),
      tags$p(tags$strong("Checklist"), tags$br(), paste(paste(names(checklist_counts), as.integer(checklist_counts), sep = ": "), collapse = "; ")),
      tags$p(tags$strong("Readiness"), tags$br(), paste(paste(names(readiness_counts), as.integer(readiness_counts), sep = ": "), collapse = "; ")),
      tags$p(tags$strong("High-priority next steps"), tags$br(), high_priority),
      tags$p(tags$strong("Spec checksum"), tags$br(), study()$reproducibility$spec_checksum),
      tags$p(tags$strong("Generated"), tags$br(), study()$reproducibility$generated_at)
    )
  })

  output$diagnostics <- renderTable({
    current_diagnostics()
  }, striped = TRUE, bordered = TRUE)

  output$publication_readiness <- renderTable({
    current_readiness()
  }, striped = TRUE, bordered = TRUE)

  output$publication_readiness_decision <- renderTable({
    current_readiness_decision()
  }, striped = TRUE, bordered = TRUE)

  output$diagnostics_plot <- renderPlot({
    plot_diagnostics(current_diagnostics())
  })

  output$publication_recommendations <- renderTable({
    current_recommendations()
  }, striped = TRUE, bordered = TRUE)

  output$publication_summary <- renderText({
    current_publication_summary()
  })

  output$reporting_checklist <- renderTable({
    current_checklist()
  }, striped = TRUE, bordered = TRUE)

  output$reproducibility_summary <- renderTable({
    repro <- current_reproducibility()
    data.frame(
      field = c("Study", "Type", "Seed", "Spec checksum", "R", "Platform", "Raw rows", "Summary rows"),
      value = c(
        repro$study_name,
        repro$study_type,
        repro$seed,
        repro$spec_checksum,
        repro$r$version,
        repro$r$platform,
        repro$raw_results_rows,
        repro$summary_rows
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE)

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
    if (identical(input$plot_type, "heatmap")) {
      plot_metric_heatmap(study()$summary, metric = input$plot_metric, term = input$plot_terms)
    } else {
      plot_metric(study()$summary, metric = input$plot_metric, term = input$plot_terms)
    }
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
          "  missing_mechanism = c(%s),",
          "  missing_targets = %s,",
          "  missing_driver = %s,",
          "  missing_slope = %s,",
          "  misspecification = %s,",
          "  group_variable = %s,",
          "  group_labels = %s,",
          "  group_proportions = %s,",
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
          "  research_question = %s,",
          "  design_rationale = %s,",
          "  metric_rationale = %s,",
          "  interpretation_plan = %s,",
          "  readiness_mode = %s",
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
          "run_manifest <- study$run_manifest",
          "failure_summary <- study$failure_summary",
          "runtime_estimate <- study$runtime_estimate",
          "diagnostics <- study$diagnostics",
          "readiness <- study$readiness",
          "readiness_decision <- study$readiness_decision",
          "reporting_checklist <- study$reporting_checklist",
          "reproducibility <- study$reproducibility",
          "publication_recommendations <- study$publication_recommendations",
          "publication_summary <- study$publication_summary",
          "methods_text <- study$methods_text",
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
        paste(sprintf('"%s"', parse_character(input$missing_mechanism)), collapse = ", "),
        r_character_vector_or_null(parse_optional_character(input$missing_targets)),
        r_character_vector_or_null(parse_optional_character(input$missing_driver)),
        input$missing_slope,
        deparse(input$misspecification),
        r_character_vector_or_null(parse_optional_character(input$group_variable)),
        r_character_vector_or_null(parse_optional_character(input$group_labels)),
        if (is.null(parse_optional_character(input$group_proportions))) "NULL" else paste0("c(", paste(parse_numeric(input$group_proportions), collapse = ", "), ")"),
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
        deparse(input$design_rationale),
        deparse(input$metric_rationale),
        deparse(input$interpretation_plan),
        deparse(input$readiness_mode),
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
        "  research_question = %s,",
        "  design_rationale = %s,",
        "  metric_rationale = %s,",
        "  interpretation_plan = %s,",
        "  readiness_mode = %s",
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
        "run_manifest <- study$run_manifest",
        "failure_summary <- study$failure_summary",
        "runtime_estimate <- study$runtime_estimate",
        "diagnostics <- study$diagnostics",
        "readiness <- study$readiness",
        "readiness_decision <- study$readiness_decision",
        "reporting_checklist <- study$reporting_checklist",
        "reproducibility <- study$reproducibility",
        "publication_recommendations <- study$publication_recommendations",
        "publication_summary <- study$publication_summary",
        "methods_text <- study$methods_text",
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
      deparse(input$design_rationale),
      deparse(input$metric_rationale),
      deparse(input$interpretation_plan),
      deparse(input$readiness_mode),
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

  output$download_diagnostics <- downloadHandler(
    filename = function() "mcsimr-diagnostics.csv",
    content = function(file) {
      utils::write.csv(current_diagnostics(), file, row.names = FALSE)
    }
  )

  output$download_readiness <- downloadHandler(
    filename = function() "mcsimr-readiness.csv",
    content = function(file) {
      utils::write.csv(current_readiness(), file, row.names = FALSE)
    }
  )

  output$download_readiness_decision <- downloadHandler(
    filename = function() "mcsimr-readiness-decision.csv",
    content = function(file) {
      utils::write.csv(current_readiness_decision(), file, row.names = FALSE)
    }
  )

  output$download_checklist <- downloadHandler(
    filename = function() "mcsimr-reporting-checklist.csv",
    content = function(file) {
      utils::write.csv(current_checklist(), file, row.names = FALSE)
    }
  )

  output$download_recommendations <- downloadHandler(
    filename = function() "mcsimr-publication-recommendations.csv",
    content = function(file) {
      utils::write.csv(current_recommendations(), file, row.names = FALSE)
    }
  )

  output$download_summary_text <- downloadHandler(
    filename = function() "mcsimr-publication-summary.md",
    content = function(file) {
      write_publication_summary(current_publication_summary(), file)
    }
  )

  output$download_reproducibility <- downloadHandler(
    filename = function() "mcsimr-reproducibility.yml",
    content = function(file) {
      write_reproducibility_manifest(current_reproducibility(), file)
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
