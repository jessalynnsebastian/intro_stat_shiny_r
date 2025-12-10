library(shiny)

# =========================================
# Preload all CSVs from URLs into a list
# =========================================

# ---- Define data sources by URL ----
data_sources <- list(
  ex1 = "https://raw.githubusercontent.com/jessalynnsebastian/intro_stat_shiny_r/main/app/data/ex1.csv",
  ex2 = "https://raw.githubusercontent.com/jessalynnsebastian/intro_stat_shiny_r/main/app/data/ex2.csv"
  # add more here
)

# ---- Download + read them into the webR filesystem ----
data_list <- list()
if (length(data_sources) > 0) {
  for (nm in names(data_sources)) {
    url <- data_sources[[nm]]
    local_name <- paste0(nm, ".csv")
    download.file(url, local_name, quiet = TRUE)
    data_list[[nm]] <- read.csv(local_name)
  }
}

# =========================================
# Safety: Blocklist dangerous functions
# =========================================

dangerous_functions <- c(
  "system", "system2", "shell", "exec",
  "write.csv", "write.table", "saveRDS", "save",
  "download.file", "url",
  "source", "eval", "parse",
  "library", "require", "install.packages",
  "unlink", "file.remove", "file.create",
  ".Internal", ".External", ".Call",
  "Sys.system", "Sys.command"
)

check_code_safety <- function(code_text) {
  # Check if any dangerous functions are mentioned in the code
  for (func in dangerous_functions) {
    # Simple pattern: function name followed by (
    pattern <- paste0("\\b", func, "\\s*\\(")
    if (grepl(pattern, code_text, ignore.case = TRUE)) {
      return(paste("Error: Function '", func, "' is not allowed for security reasons.", sep = ""))
    }
  }
  return(NULL)  # Safe
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
  rnorm = "
rnorm(n, mean = 0, sd = 1)

Generates n random draws from a Normal(mean, sd).
- n: number of draws
- mean, sd: mean and standard deviation

Example:
  rnorm(5, mean = 10, sd = 2)
",
  dnorm = "
dnorm(x, mean = 0, sd = 1, log = FALSE)

Returns the density f(x) for a Normal(mean, sd).
- x: numeric vector of points
- mean, sd: mean and standard deviation
- log: TRUE to return log-density

Example:
  dnorm(0)  # ~0.3989
",
  pnorm = "
pnorm(q, mean = 0, sd = 1, lower.tail = TRUE)

Returns P(X <= q) for a Normal(mean, sd).
- q: cutoff value
- mean, sd: mean and standard deviation
- lower.tail: FALSE gives P(X > q)

Example:
  pnorm(1.96)       # ~0.975
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
  
  tags$head(
    tags$style(HTML("
      #data_preview {
        max-height: 400px;
        overflow-y: auto;
        overflow-x: auto;
        display: block;
      }
    "))
  ),

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
        ),

        # ---- Distributions tab: Normal helpers ----
        tabPanel(
          "Distributions",
          uiOutput("dist_ui")
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
  
  # Reactive value to store uploaded dataset
  uploaded_data <- reactiveVal(NULL)

  # =====================
  # Data tab
  # =====================

  output$data_ui <- renderUI({
    preloaded_choices <- if (length(data_list) > 0) names(data_list) else c()
    
    # Add uploaded dataset to choices if available
    all_choices <- c(preloaded_choices, if (!is.null(uploaded_data())) "(uploaded)" else c())

    tagList(
      h5("Upload a CSV file:"),
      fileInput(
        "upload_file",
        "Choose CSV file:",
        accept = c(".csv", "text/csv")
      ),
      hr(),
      h5("Or select a preloaded dataset:"),
      if (length(all_choices) > 0) {
        selectInput(
          "dataset_name",
          "Select dataset to preview:",
          choices = all_choices
        )
      } else {
        helpText("No datasets available. Upload a CSV file to get started.")
      }
    )
  })

  # Handle file upload
  observeEvent(input$upload_file, {
    file_path <- input$upload_file$datapath
    if (!is.null(file_path)) {
      tryCatch(
        {
          df <- read.csv(file_path)
          uploaded_data(df)
          # Assign to user environment for code execution
          assign("uploaded", df, envir = user_env)
        },
        error = function(e) {
          showNotification(
            paste("Error reading file:", e$message),
            type = "error",
            duration = 5
          )
        }
      )
    }
  })

  output$data_preview <- renderTable({
    # Use uploaded data if available and selected, otherwise use preloaded
    dataset_choice <- input$dataset_name
    
    if (!is.null(dataset_choice)) {
      if (dataset_choice == "(uploaded)") {
        df <- uploaded_data()
      } else {
        df <- data_list[[dataset_choice]]
      }
      
      if (!is.null(df)) {
        head(df)
      }
    }
  }, rownames = TRUE)

  # =====================
  # Graphics tab
  # =====================

  output$graphics_ui <- renderUI({
    # Dataset choice is optional: "" means "none"
    choices <- c("(none, I'll type my own)" = "", names(data_list))

    tagList(
      selectInput(
        "g_dataset",
        "Choose a preloaded dataset (optional):",
        choices = choices
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

  # =====================
  # Distributions tab
  # =====================

  output$dist_ui <- renderUI({
    tagList(
      selectInput(
        "dist_family",
        "Choose a distribution:",
        choices = c("Normal", "Student's t", "Binomial", "Uniform")
      ),
      selectInput(
        "dist_type",
        "Choose what to compute:",
        choices = c("Samples", "Density/Mass", "CDF", "Quantiles")
      ),
      uiOutput("dist_params_ui"),
      actionButton("insert_dist", "Insert code")
    )
  })

  # Render parameter inputs based on distribution + type selection
  output$dist_params_ui <- renderUI({
    family <- input$dist_family
    dtype <- input$dist_type
    
    if (is.null(family) || is.null(dtype)) return(NULL)

    # Normal distribution
    if (family == "Normal") {
      if (dtype == "Samples") {
        return(tagList(
          numericInput("d_n", "Number of samples:", value = 100, min = 1, step = 1),
          numericInput("d_mean", "Mean:", value = 0),
          numericInput("d_sd", "SD:", value = 1, min = 0.01, step = 0.1)
        ))
      } else if (dtype == "Density/Mass") {
        return(tagList(
          numericInput("d_x", "x value:", value = 0),
          numericInput("d_mean", "Mean:", value = 0),
          numericInput("d_sd", "SD:", value = 1, min = 0.01, step = 0.1)
        ))
      } else if (dtype == "CDF") {
        return(tagList(
          numericInput("d_q", "Quantile:", value = 0),
          numericInput("d_mean", "Mean:", value = 0),
          numericInput("d_sd", "SD:", value = 1, min = 0.01, step = 0.1)
        ))
      } else if (dtype == "Quantiles") {
        return(tagList(
          numericInput("d_p", "Probability:", value = 0.5, min = 0, max = 1, step = 0.01),
          numericInput("d_mean", "Mean:", value = 0),
          numericInput("d_sd", "SD:", value = 1, min = 0.01, step = 0.1)
        ))
      }
    }

    # Student's t distribution
    if (family == "Student's t") {
      if (dtype == "Samples") {
        return(tagList(
          numericInput("d_n", "Number of samples:", value = 100, min = 1, step = 1),
          numericInput("d_df", "Degrees of freedom:", value = 10, min = 1, step = 1)
        ))
      } else if (dtype == "Density/Mass") {
        return(tagList(
          numericInput("d_x", "x value:", value = 0),
          numericInput("d_df", "Degrees of freedom:", value = 10, min = 1, step = 1)
        ))
      } else if (dtype == "CDF") {
        return(tagList(
          numericInput("d_q", "Quantile:", value = 0),
          numericInput("d_df", "Degrees of freedom:", value = 10, min = 1, step = 1)
        ))
      } else if (dtype == "Quantiles") {
        return(tagList(
          numericInput("d_p", "Probability:", value = 0.5, min = 0, max = 1, step = 0.01),
          numericInput("d_df", "Degrees of freedom:", value = 10, min = 1, step = 1)
        ))
      }
    }

    # Binomial distribution
    if (family == "Binomial") {
      if (dtype == "Samples") {
        return(tagList(
          numericInput("d_n", "Number of samples:", value = 100, min = 1, step = 1),
          numericInput("d_size", "Number of trials:", value = 10, min = 1, step = 1),
          numericInput("d_prob", "Probability of success:", value = 0.5, min = 0, max = 1, step = 0.1)
        ))
      } else if (dtype == "Density/Mass") {
        return(tagList(
          numericInput("d_k", "Number of successes:", value = 5, min = 0, step = 1),
          numericInput("d_size", "Number of trials:", value = 10, min = 1, step = 1),
          numericInput("d_prob", "Probability of success:", value = 0.5, min = 0, max = 1, step = 0.1)
        ))
      } else if (dtype == "CDF") {
        return(tagList(
          numericInput("d_q", "Quantile:", value = 5, min = 0, step = 1),
          numericInput("d_size", "Number of trials:", value = 10, min = 1, step = 1),
          numericInput("d_prob", "Probability of success:", value = 0.5, min = 0, max = 1, step = 0.1)
        ))
      } else if (dtype == "Quantiles") {
        return(tagList(
          numericInput("d_p", "Probability:", value = 0.5, min = 0, max = 1, step = 0.01),
          numericInput("d_size", "Number of trials:", value = 10, min = 1, step = 1),
          numericInput("d_prob", "Probability of success:", value = 0.5, min = 0, max = 1, step = 0.1)
        ))
      }
    }

    # Uniform distribution
    if (family == "Uniform") {
      if (dtype == "Samples") {
        return(tagList(
          numericInput("d_n", "Number of samples:", value = 100, min = 1, step = 1),
          numericInput("d_min", "Min:", value = 0),
          numericInput("d_max", "Max:", value = 1)
        ))
      } else if (dtype == "Density/Mass") {
        return(tagList(
          numericInput("d_x", "x value:", value = 0.5),
          numericInput("d_min", "Min:", value = 0),
          numericInput("d_max", "Max:", value = 1)
        ))
      } else if (dtype == "CDF") {
        return(tagList(
          numericInput("d_q", "Quantile:", value = 0.5),
          numericInput("d_min", "Min:", value = 0),
          numericInput("d_max", "Max:", value = 1)
        ))
      } else if (dtype == "Quantiles") {
        return(tagList(
          numericInput("d_p", "Probability:", value = 0.5, min = 0, max = 1, step = 0.01),
          numericInput("d_min", "Min:", value = 0),
          numericInput("d_max", "Max:", value = 1)
        ))
      }
    }
  })

  # Handle insert for distributions
  observeEvent(input$insert_dist, {
    family <- input$dist_family
    dtype <- input$dist_type

    line <- ""

    # Construct function call based on family and type
    if (family == "Normal") {
      if (dtype == "Samples") {
        line <- sprintf("rnorm(%s, mean = %s, sd = %s)", input$d_n, input$d_mean, input$d_sd)
      } else if (dtype == "Density/Mass") {
        line <- sprintf("dnorm(%s, mean = %s, sd = %s)", input$d_x, input$d_mean, input$d_sd)
      } else if (dtype == "CDF") {
        line <- sprintf("pnorm(%s, mean = %s, sd = %s)", input$d_q, input$d_mean, input$d_sd)
      } else if (dtype == "Quantiles") {
        line <- sprintf("qnorm(%s, mean = %s, sd = %s)", input$d_p, input$d_mean, input$d_sd)
      }
    } else if (family == "Student's t") {
      if (dtype == "Samples") {
        line <- sprintf("rt(%s, df = %s)", input$d_n, input$d_df)
      } else if (dtype == "Density/Mass") {
        line <- sprintf("dt(%s, df = %s)", input$d_x, input$d_df)
      } else if (dtype == "CDF") {
        line <- sprintf("pt(%s, df = %s)", input$d_q, input$d_df)
      } else if (dtype == "Quantiles") {
        line <- sprintf("qt(%s, df = %s)", input$d_p, input$d_df)
      }
    } else if (family == "Binomial") {
      if (dtype == "Samples") {
        line <- sprintf("rbinom(%s, size = %s, prob = %s)", input$d_n, input$d_size, input$d_prob)
      } else if (dtype == "Density/Mass") {
        line <- sprintf("dbinom(%s, size = %s, prob = %s)", input$d_k, input$d_size, input$d_prob)
      } else if (dtype == "CDF") {
        line <- sprintf("pbinom(%s, size = %s, prob = %s)", input$d_q, input$d_size, input$d_prob)
      } else if (dtype == "Quantiles") {
        line <- sprintf("qbinom(%s, size = %s, prob = %s)", input$d_p, input$d_size, input$d_prob)
      }
    } else if (family == "Uniform") {
      if (dtype == "Samples") {
        line <- sprintf("runif(%s, min = %s, max = %s)", input$d_n, input$d_min, input$d_max)
      } else if (dtype == "Density/Mass") {
        line <- sprintf("dunif(%s, min = %s, max = %s)", input$d_x, input$d_min, input$d_max)
      } else if (dtype == "CDF") {
        line <- sprintf("punif(%s, min = %s, max = %s)", input$d_q, input$d_min, input$d_max)
      } else if (dtype == "Quantiles") {
        line <- sprintf("qunif(%s, min = %s, max = %s)", input$d_p, input$d_min, input$d_max)
      }
    }

    old_code <- input$code
    if (is.null(old_code)) old_code <- ""

    new_code <- if (nzchar(old_code)) paste(old_code, line, sep = "\n") else line
    updateTextAreaInput(session, "code", value = new_code)
    current_fun(tolower(gsub("\\(.*", "", line)))
  })

  # ---- Histogram variable selector ----
  output$g_hist_var_ui <- renderUI({
    # No dataset selected: generic template
    if (is.null(input$g_dataset) || input$g_dataset == "") {
      return(helpText(
        "No dataset selected.",
        "We'll insert a generic template like:",
        "  hist(x, breaks = 10)",
        "You can define x yourself in the code box (e.g. x <- rnorm(50))."
      ))
    }

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
    # No dataset selected: generic template
    if (is.null(input$g_dataset) || input$g_dataset == "") {
      return(helpText(
        "No dataset selected.",
        "We'll insert a generic template like:",
        "  plot(y ~ x)",
        "You can define x and y yourself in the code box."
      ))
    }

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
    # Branch on whether a dataset is selected
    if (is.null(input$g_dataset) || input$g_dataset == "") {
      # Generic template: student supplies x
      line <- sprintf("hist(x, breaks = %s)", input$g_breaks)
    } else {
      req(input$g_hist_var, input$g_breaks)
      dataset_name <- input$g_dataset
      line <- sprintf(
        "hist(%s$%s, breaks = %s)",
        dataset_name, input$g_hist_var, input$g_breaks
      )
    }

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
    # Branch on whether a dataset is selected
    if (is.null(input$g_dataset) || input$g_dataset == "") {
      # Generic template: student defines x and y
      line <- "plot(y ~ x)"
    } else {
      req(input$g_scatter_x, input$g_scatter_y)
      dataset_name <- input$g_dataset
      line <- sprintf(
        "plot(%s ~ %s, data = %s)",
        input$g_scatter_y,
        input$g_scatter_x,
        dataset_name
      )
    }

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

    # ---- Safety check: blocklist dangerous functions ----
    safety_error <- check_code_safety(code_text)
    if (!is.null(safety_error)) {
      output$text_out <- renderText({ safety_error })
      return(NULL)
    }

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
          # Set timeout to 5 seconds per expression
          setTimeLimit(elapsed = 5, transient = TRUE)
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
      setTimeLimit(elapsed = 5, transient = TRUE)
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
