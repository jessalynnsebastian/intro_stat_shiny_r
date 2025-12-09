library(shiny)

# =========================================
# Preload all CSVs from app/data into a list
# =========================================

data_files <- list.files("data", pattern = "\\.csv$", full.names = TRUE)

data_list <- list()
if (length(data_files) > 0) {
  data_list <- lapply(data_files, read.csv)
  names(data_list) <- sub("\\.csv$", "", basename(data_files))
}

# =========================================
# Simple help text for supported functions
# =========================================

help_texts <- list(
  hist = "
hist(x, breaks = ...)

Creates a histogram of a numeric vector x.
- x: numeric data (e.g. data$height)
- breaks: number of bins (or a vector of break points)

Example:
  hist(survey$height, breaks = 10)
",
  qnorm = "
qnorm(p, mean = 0, sd = 1)

Gives the quantile (cutoff) of a Normal(mean, sd) distribution.
- p: probability (e.g. 0.975 for a 97.5% cutoff)
- mean, sd: mean and standard deviation

Example:
  qnorm(0.975)       # z* for a 95% CI
",
  t_test = "
t.test(x, y = NULL, mu = 0, conf.level = 0.95)

Performs a t-test.
- One-sample: t.test(x, mu = mean_under_H0)
- Two-sample: t.test(x ~ group, data = ...)

Example:
  t.test(survey$height, mu = 170)
",
  plot = "
plot(y ~ x, data = ...)

Creates a scatterplot of y versus x from a data frame.
- x: numeric predictor
- y: numeric response

Example:
  plot(height ~ weight, data = survey)
"
)

ui <- fluidPage(
  titlePanel("Intro Stats: Interactive R Code Helper"),

  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = "helper_tabs",

        # ---- Data tab: choose dataset and see preview ----
        tabPanel(
          "Data",
          uiOutput("data_ui"),
          br(),
          h5("Preview of selected dataset:"),
          tableOutput("data_preview")
        ),

        # ---- Graphics tab: histogram & scatterplot ----
        tabPanel(
          "Graphics",
          uiOutput("graphics_ui")
        )

        # Later you can add:
        # tabPanel("Statistics", ...)
      )
    ),

    mainPanel(
      h4("R code (you can edit this):"),
      textAreaInput(
        "code",
        label = NULL,
        value = "x <- rnorm(10)\nmean(x)\nsd(x)",
        rows = 10,
        width = "100%"
      ),
      br(),
      actionButton("run", "Run code"),
      hr(),
      h4("Text / numeric output:"),
      verbatimTextOutput("text_out"),
      h4("Plot output:"),
      plotOutput("plot_out", height = "300px"),
      hr(),
      h4("Function help:"),
      verbatimTextOutput("fun_help")
    )
  )
)

server <- function(input, output, session) {

  # Environment that persists across runs in this browser session
  user_env <- new.env(parent = globalenv())

  # Put all preloaded datasets into the user environment
  if (length(data_list) > 0) {
    for (nm in names(data_list)) {
      assign(nm, data_list[[nm]], envir = user_env)
    }
  }

  # Reactive value for which function's help to show
  current_fun <- reactiveVal(NULL)

  # =====================
  # Data tab
  # =====================

  output$data_ui <- renderUI({
    if (length(data_list) == 0) {
      return(helpText("No CSV files found in 'app/data'."))
    }

    tagList(
      selectInput(
        "dataset_name",
        "Choose a dataset (preloaded from app/data):",
        choices = names(data_list)
      )
    )
  })

  output$data_preview <- renderTable({
    req(input$dataset_name)
    df <- data_list[[input$dataset_name]]
    head(df)
  }, rownames = TRUE)

  # =====================
  # Graphics tab
  # =====================

  output$graphics_ui <- renderUI({
    if (length(data_list) == 0) {
      return(helpText("No CSV files found in 'app/data'."))
    }

    tagList(
      selectInput(
        "g_dataset",
        "Choose a dataset (preloaded):",
        choices = names(data_list)
      ),
      tabsetPanel(
        id = "graphics_tabs",

        # Histogram sub-tab
        tabPanel(
          "Histogram",
          uiOutput("g_hist_var_ui"),
          numericInput(
            "g_breaks",
            "Number of breaks (bins):",
            value = 10, min = 1, step = 1
          ),
          actionButton("insert_hist", "Insert hist() code into editor")
        ),

        # Scatterplot sub-tab
        tabPanel(
          "Scatterplot",
          uiOutput("g_scatter_ui"),
          actionButton("insert_scatter", "Insert scatterplot code into editor")
        )
      )
    )
  })

  # ---- Histogram variable selector ----
  output$g_hist_var_ui <- renderUI({
    req(input$g_dataset)

    df <- data_list[[input$g_dataset]]

    numeric_cols <- names(df)[sapply(df, is.numeric)]
    if (length(numeric_cols) == 0) {
      return(helpText("No numeric variables found in this dataset."))
    }

    selectInput(
      "g_hist_var",
      "Choose a numeric variable for the histogram:",
      choices = numeric_cols
    )
  })

  # ---- Scatterplot variable selectors (x and y) ----
  output$g_scatter_ui <- renderUI({
    req(input$g_dataset)

    df <- data_list[[input$g_dataset]]

    numeric_cols <- names(df)[sapply(df, is.numeric)]
    if (length(numeric_cols) < 2) {
      return(helpText("Need at least two numeric variables for a scatterplot."))
    }

    tagList(
      selectInput(
        "g_scatter_x",
        "X variable (horizontal axis):",
        choices = numeric_cols
      ),
      selectInput(
        "g_scatter_y",
        "Y variable (vertical axis):",
        choices = numeric_cols,
        selected = numeric_cols[min(2, length(numeric_cols))]
      )
    )
  })

  # ---- Insert hist() code into the editor ----
  observeEvent(input$insert_hist, {
    req(input$g_dataset, input$g_hist_var, input$g_breaks)

    dataset_name <- input$g_dataset
    line <- sprintf(
      "hist(%s$%s, breaks = %s)",
      dataset_name, input$g_hist_var, input$g_breaks
    )

    old_code <- input$code
    if (is.null(old_code)) old_code <- ""

    new_code <- if (nzchar(old_code)) {
      paste(old_code, line, sep = "\n")
    } else {
      line
    }

    updateTextAreaInput(session, "code", value = new_code)
    current_fun("hist")
  })

  # ---- Insert scatterplot code into the editor ----
  observeEvent(input$insert_scatter, {
    req(input$g_dataset, input$g_scatter_x, input$g_scatter_y)

    dataset_name <- input$g_dataset
    line <- sprintf(
      "plot(%s ~ %s, data = %s)",
      input$g_scatter_y,
      input$g_scatter_x,
      dataset_name
    )

    old_code <- input$code
    if (is.null(old_code)) old_code <- ""

    new_code <- if (nzchar(old_code)) {
      paste(old_code, line, sep = "\n")
    } else {
      line
    }

    updateTextAreaInput(session, "code", value = new_code)
    current_fun("plot")
  })

  # =====================
  # Run code + outputs
  # =====================

  observeEvent(input$run, {
    code_text <- input$code

    # Clear previous outputs
    output$text_out  <- renderText({ "" })
    output$plot_out  <- renderPlot({ })

    # Parse into multiple expressions
    exprs <- try(parse(text = code_text), silent = TRUE)

    if (inherits(exprs, "try-error")) {
      output$text_out <- renderText({
        paste("Parse error:\n", attr(exprs, "condition")$message)
      })
      return(NULL)
    }

    # Evaluate each expression, capture printed output
    all_out <- character()

    for (expr in exprs) {
      this_out <- try(
        {
          res <- withVisible(eval(expr, envir = user_env))
          capture.output(if (res$visible) print(res$value))
        },
        silent = TRUE
      )

      if (inherits(this_out, "try-error")) {
        all_out <- c(
          all_out,
          paste("Error during evaluation:",
                attr(this_out, "condition")$message)
        )
      } else if (length(this_out) > 0) {
        all_out <- c(all_out, this_out)
      }
    }

    if (length(all_out) == 0) {
      all_out <- "[No printed output.]"
    }

    output$text_out <- renderText({
      paste(all_out, collapse = "\n")
    })

    # Re-run code in a plotting context: any plotting calls will draw here
    output$plot_out <- renderPlot({
      eval(parse(text = code_text), envir = user_env)
    })
  })

  # =====================
  # Function help panel
  # =====================

  output$fun_help <- renderText({
    fun <- current_fun()
    if (is.null(fun)) {
      return("Select a template (e.g. insert hist() or scatterplot code) to see help.")
    }

    txt <- help_texts[[fun]]
    if (is.null(txt)) {
      return(paste("No help text defined for function:", fun))
    }

    txt
  })
}

shinyApp(ui, server)
