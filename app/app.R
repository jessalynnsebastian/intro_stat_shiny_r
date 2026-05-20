library(shiny)

# Allow larger classroom datasets than Shiny's default 5 MB upload limit.
options(shiny.maxRequestSize = 25 * 1024^2)

# =========================================
# Preload CSV data (embedded as strings)
# =========================================

# ---- Plant Growth Data ----
plantgrowth_csv <- "\"weight\",\"group\"
4.17,\"ctrl\"
5.58,\"ctrl\"
5.18,\"ctrl\"
6.11,\"ctrl\"
4.5,\"ctrl\"
4.61,\"ctrl\"
5.17,\"ctrl\"
4.53,\"ctrl\"
5.33,\"ctrl\"
5.14,\"ctrl\"
4.81,\"trt1\"
4.17,\"trt1\"
4.41,\"trt1\"
3.59,\"trt1\"
5.87,\"trt1\"
3.83,\"trt1\"
6.03,\"trt1\"
4.89,\"trt1\"
4.32,\"trt1\"
4.69,\"trt1\"
6.31,\"trt2\"
5.12,\"trt2\"
5.54,\"trt2\"
5.5,\"trt2\"
5.37,\"trt2\"
5.29,\"trt2\"
4.92,\"trt2\"
6.15,\"trt2\"
5.8,\"trt2\"
5.26,\"trt2\""

# ---- Cherry Trees Data ----
cherrytrees_csv <- "\"Girth\",\"Height\",\"Volume\"
8.3,70,10.3
8.6,65,10.3
8.8,63,10.2
10.5,72,16.4
10.7,81,18.8
10.8,83,19.7
11,66,15.6
11,75,18.2
11.1,80,22.6
11.2,75,19.9
11.3,79,24.2
11.4,76,21
11.4,76,21.4
11.7,69,21.3
12,75,19.1
12.9,74,22.2
12.9,85,33.8
13.3,86,27.4
13.7,71,25.7
13.8,64,24.9
14,78,34.5
14.2,80,31.7
14.5,74,36.3
16,72,38.3
16.3,77,42.6
17.3,81,55.4
17.5,82,55.7
17.9,80,58.3
18,80,51.5
18,80,51
20.6,87,77"

# ---- Load data into list ----
data_list <- list(
  plantgrowth = read.csv(text = plantgrowth_csv),
  cherrytrees = read.csv(text = cherrytrees_csv)
)

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
",
  boxplot = "
boxplot(x ~ group, data = ...)

Creates a boxplot to compare distributions.
- x: numeric variable (the data to visualize)
- group: categorical variable (grouping factor, optional)
- data: data frame

Example:
  boxplot(height ~ gender, data = survey)  # Side-by-side
  boxplot(survey$height)                   # Single boxplot
",
  barplot = "
barplot(table(x))

Creates a bar chart for categorical data.
- x: categorical variable or a table of frequencies

Example:
  barplot(table(survey$gender))
",
  pie = "
pie(x, labels = names(x))

Creates a pie chart for categorical data.
- x: a vector of frequencies/counts (often from table())
- labels: category names

Example:
  pie(table(survey$gender))
",
  mean = "
mean(x, na.rm = FALSE)

Calculates the arithmetic mean (average) of a numeric vector.
- x: numeric vector
- na.rm: if TRUE, removes missing values before computing

Example:
  mean(survey$height)
",
  sd = "
sd(x, na.rm = FALSE)

Calculates the standard deviation of a numeric vector.
- x: numeric vector
- na.rm: if TRUE, removes missing values before computing

Example:
  sd(survey$height)
",
  var = "
var(x, na.rm = FALSE)

Calculates the variance of a numeric vector.
- x: numeric vector
- na.rm: if TRUE, removes missing values before computing

Example:
  var(survey$height)
",
  median = "
median(x, na.rm = FALSE)

Calculates the median (middle value) of a numeric vector.
- x: numeric vector
- na.rm: if TRUE, removes missing values before computing

Example:
  median(survey$height)
",
  summary = "
summary(x)

Produces a summary of a variable or data frame.
For numeric: min, Q1, median, mean, Q3, max
For factors: frequency counts

Example:
  summary(survey$height)
  summary(survey)  # Summary of all variables
",
  length = "
length(x)

Returns the number of elements in a vector.
Useful for sample size (n).

Example:
  length(survey$height)  # Sample size
",
  rexp = "
rexp(n, rate = 1)

Generates n random draws from an Exponential(rate) distribution.
- n: number of draws
- rate: rate parameter (for Exponential(1), rate = 1)

Example:
  rexp(5, rate = 1)
",
  dexp = "
dexp(x, rate = 1, log = FALSE)

Returns the density f(x) for an Exponential(rate) distribution.
- x: numeric vector of points
- rate: rate parameter (for Exponential(1), rate = 1)
- log: TRUE to return log-density

Example:
  dexp(1, rate = 1)
",
  pexp = "
pexp(q, rate = 1, lower.tail = TRUE)

Returns P(X <= q) for an Exponential(rate) distribution.
- q: cutoff value
- rate: rate parameter (for Exponential(1), rate = 1)
- lower.tail: FALSE gives P(X > q)

Example:
  pexp(1, rate = 1)
",
  qexp = "
qexp(p, rate = 1)

Gives the quantile (cutoff) of an Exponential(rate) distribution.
- p: probability
- rate: rate parameter (for Exponential(1), rate = 1)

Example:
  qexp(0.975, rate = 1)
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
          h5("Upload a CSV file:"),
          fileInput(
            "upload_file",
            "Choose CSV file:",
            accept = c(".csv", "text/csv")
          ),
          hr(),
          h5("Or select a dataset:"),
          uiOutput("dataset_picker_ui"),
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
        ),

        tabPanel(
          "CLT",
          h5("Central Limit Theorem simulator"),
          
          selectInput(
            "clt_dist",
            "Choose a distribution:",
            choices = c("Normal", "Uniform", "Bernoulli", "Exponential", "Custom pdf")
          ),
          
          numericInput("clt_n", "Sample size (n):", value = 30, min = 1),
          numericInput("clt_reps", "Number of repetitions:", value = 10000, min = 1),
          
          # Parameters for Normal
          conditionalPanel(
            condition = "input.clt_dist == 'Normal'",
            numericInput("clt_mean", "Mean:", value = 0),
            numericInput("clt_sd", "SD:", value = 1, min = 0.01)
          ),
          
          # Parameters for Uniform
          conditionalPanel(
            condition = "input.clt_dist == 'Uniform'",
            numericInput("clt_min", "Min:", value = 0),
            numericInput("clt_max", "Max:", value = 1)
          ),

          # Parameters for Bernoulli
          conditionalPanel(
            condition = "input.clt_dist == 'Bernoulli'",
            numericInput("clt_p", "Probability of success (p):", value = 0.5, min = 0, max = 1)
          ),

          # Parameters for Exponential
          conditionalPanel(
            condition = "input.clt_dist == 'Exponential'",
            numericInput("clt_rate", "Rate parameter:", value = 1, min = 0.01, step = 0.1)
          ),

          # Parameters for a custom pdf
          conditionalPanel(
            condition = "input.clt_dist == 'Custom pdf'",
            textInput("clt_pdf_expr", "Custom density f(x):", value = "dbeta(x, shape1 = 2, shape2 = 2)"),
            numericInput("clt_pdf_min", "Support min:", value = 0),
            numericInput("clt_pdf_max", "Support max:", value = 1),
            numericInput("clt_pdf_m", "Envelope height M:", value = 1.5, min = 0.01)
            ,helpText("Choose M so the density stays at or below M on the support interval. This is the rejection-sampling envelope height.")
          ),
          
          actionButton("insert_clt", "Insert CLT simulation code")
        ),

        # ---- Statistics tab: summaries, tests, regression ----
        tabPanel(
          "Statistics",
          uiOutput("stats_ui")
        )
      ),
      hr(),
      h4("Function help:"),
      verbatimTextOutput("fun_help")
    ),

    mainPanel(
      h4("R code:"),
      textAreaInput(
        "code",
        label = NULL,
        value = "# You can type your R code here.\n# Use the helper tabs to insert code snippets.",
        rows = 10,
        width = "100%"
      ),
      br(),
      actionButton("run", "Run code"),
      hr(),
      h4("Output:"),
      uiOutput("combined_output")
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
  uploaded_name <- reactiveVal(NULL)

  # Quote non-syntactic column names so generated code stays valid.
  safe_col <- function(col_name) {
    if (identical(col_name, make.names(col_name)) && !grepl("^\\.[0-9]", col_name)) {
      return(col_name)
    }
    paste0("`", gsub("`", "\\\\`", col_name), "`")
  }

  # =====================
  # Data tab
  # =====================

  output$dataset_picker_ui <- renderUI({
    preloaded_choices <- if (length(data_list) > 0) names(data_list) else c()
    
    # Add uploaded dataset to choices if available
    uploaded_choice <- if (!is.null(uploaded_data())) {
      name <- uploaded_name()
      if (!is.null(name)) name else "uploaded"
    } else {
      c()
    }
    all_choices <- c(preloaded_choices, uploaded_choice)

    if (length(all_choices) > 0) {
      selectInput(
        "dataset_name",
        "Select dataset to preview:",
        choices = all_choices
      )
    } else {
      helpText("No datasets available. Upload a CSV file to get started.")
    }
  })

  # Handle file upload
  observeEvent(input$upload_file, {
    req(input$upload_file)

    file_path <- input$upload_file$datapath[[1]]
    file_name <- input$upload_file$name[[1]]

    if (is.null(file_path) || !nzchar(file_path)) {
      return(NULL)
    }

    tryCatch(
      {
        df <- read.csv(
          file_path,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )

        uploaded_data(df)

        # Get clean variable name from filename (remove .csv, replace invalid chars)
        clean_name <- gsub("\\.[cC][sS][vV]$", "", basename(file_name))
        clean_name <- gsub("[^a-zA-Z0-9_]", "_", clean_name)

        if (!nzchar(clean_name)) clean_name <- "uploaded_data"
        if (grepl("^[0-9]", clean_name)) clean_name <- paste0("data_", clean_name)

        uploaded_name(clean_name)

        # Assign to user environment for code execution
        assign(clean_name, df, envir = user_env)

        showNotification(
          paste("Uploaded", file_name, "as", clean_name),
          type = "message",
          duration = 3
        )
      },
      error = function(e) {
        showNotification(
          paste("Error reading file:", e$message),
          type = "error",
          duration = 5
        )
      }
    )
  })

  output$data_preview <- renderTable({
    # Use uploaded data if available and selected, otherwise use preloaded
    dataset_choice <- input$dataset_name
    
    if (!is.null(dataset_choice)) {
      if (!is.null(uploaded_name()) && dataset_choice == uploaded_name()) {
        df <- uploaded_data()
      } else {
        df <- data_list[[dataset_choice]]
      }
      
      if (!is.null(df)) {
        head(df)
      }
    }
  })

  # =====================
  # Graphics tab
  # =====================

  output$graphics_ui <- renderUI({
    # Dataset choice is optional: "" means "none"
    choices <- c("(none, I'll type my own)" = "", names(data_list))
    
    # Also include uploaded dataset if available
    if (!is.null(uploaded_data())) {
      uname <- uploaded_name()
      if (!is.null(uname)) {
        choices <- c(choices, setNames(uname, uname))
      }
    }

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
          actionButton("insert_hist", "Insert histogram code into editor")
        ),

        # Scatterplot sub-tab
        tabPanel(
          "Scatterplot",
          uiOutput("g_scatter_ui"),
          actionButton("insert_scatter", "Insert scatterplot code into editor")
        ),

        # Boxplot sub-tab
        tabPanel(
          "Boxplot",
          uiOutput("g_boxplot_ui"),
          actionButton("insert_boxplot", "Insert boxplot code into editor")
        ),

        # Barplot sub-tab
        tabPanel(
          "Bar Plot",
          uiOutput("g_barplot_ui"),
          actionButton("insert_barplot", "Insert barplot code into editor")
        ),

        # Pie chart sub-tab
        tabPanel(
          "Pie Chart",
          uiOutput("g_piechart_ui"),
          actionButton("insert_piechart", "Insert pie chart code into editor")
        )
      )
    )
  })

  # =====================
  # Statistics tab
  # =====================

  output$stats_ui <- renderUI({
    # Dataset choice
    choices <- c("(none, I'll type my own)" = "", names(data_list))
    if (!is.null(uploaded_data())) {
      uname <- uploaded_name()
      if (!is.null(uname)) {
        choices <- c(choices, setNames(uname, uname))
      }
    }

    tagList(
      selectInput(
        "stats_dataset",
        "Choose a dataset (optional):",
        choices = choices
      ),
      selectInput(
        "stats_function",
        "Choose a function:",
        choices = c("mean", "sd", "var", "median", "summary", "length")
      ),
      uiOutput("stats_var_ui"),
      actionButton("insert_stats", "Insert code")
    )
  })

  # Render variable selector for statistics
  output$stats_var_ui <- renderUI({
    if (is.null(input$stats_dataset) || input$stats_dataset == "") {
      return(helpText(
        "No dataset selected.",
        "We'll insert a generic template like: mean(x)"
      ))
    }

    dataset_name <- input$stats_dataset
    if (!is.null(uploaded_name()) && dataset_name == uploaded_name()) {
      df <- uploaded_data()
    } else {
      df <- data_list[[dataset_name]]
    }

    numeric_cols <- names(df)[sapply(df, is.numeric)]
    if (length(numeric_cols) == 0) {
      return(helpText("No numeric variables found in this dataset."))
    }

    selectInput(
      "stats_var",
      "Choose a numeric variable:",
      choices = numeric_cols
    )
  })

  # Handle insert for statistics
  observeEvent(input$insert_stats, {
    func <- input$stats_function
    
    if (is.null(input$stats_dataset) || input$stats_dataset == "") {
      # Generic template
      line <- sprintf("%s(x)", func)
    } else {
      req(input$stats_var)
      dataset_name <- input$stats_dataset
      var <- safe_col(input$stats_var)
      line <- sprintf("%s(%s$%s)", func, dataset_name, var)
    }
    
    old_code <- input$code
    if (is.null(old_code)) old_code <- ""
    new_code <- if (nzchar(old_code)) paste(old_code, line, sep = "\n") else line
    updateTextAreaInput(session, "code", value = new_code)
    current_fun(func)
  })

  # =====================
  # Distributions tab
  # =====================

  output$dist_ui <- renderUI({
    tagList(
      selectInput(
        "dist_family",
        "Choose a distribution:",
        choices = c("Normal", "Student's t", "Binomial", "Uniform", "Exponential")
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

    # Exponential distribution (fixed rate = 1)
    if (family == "Exponential") {
      return(helpText("This helper uses Exponential(1), so the rate is fixed at 1."))
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
    } else if (family == "Exponential") {
      if (dtype == "Samples") {
        line <- sprintf("rexp(%s, rate = 1)", input$d_n)
      } else if (dtype == "Density/Mass") {
        line <- sprintf("dexp(%s, rate = 1)", input$d_x)
      } else if (dtype == "CDF") {
        line <- sprintf("pexp(%s, rate = 1)", input$d_q)
      } else if (dtype == "Quantiles") {
        line <- sprintf("qexp(%s, rate = 1)", input$d_p)
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

    dataset_name <- input$g_dataset
    if (!is.null(uploaded_name()) && dataset_name == uploaded_name()) {
      df <- uploaded_data()
    } else {
      df <- data_list[[dataset_name]]
    }

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

  # ---- Boxplot variable selector ----
  output$g_boxplot_ui <- renderUI({
    if (is.null(input$g_dataset) || input$g_dataset == "") {
      return(helpText(
        "No dataset selected.",
        "We'll insert a generic template like:",
        "  boxplot(x ~ group)"
      ))
    }

    dataset_name <- input$g_dataset
    if (!is.null(uploaded_name()) && dataset_name == uploaded_name()) {
      df <- uploaded_data()
    } else {
      df <- data_list[[dataset_name]]
    }

    numeric_cols <- names(df)[sapply(df, is.numeric)]
    other_cols <- names(df)[!sapply(df, is.numeric)]

    if (length(numeric_cols) == 0) {
      return(helpText("No numeric variables found in this dataset."))
    }

    tagList(
      selectInput(
        "g_boxplot_var",
        "Choose a numeric variable:",
        choices = numeric_cols
      ),
      if (length(other_cols) > 0) {
        selectInput(
          "g_boxplot_group",
          "Group by (optional, for side-by-side boxplots):",
          choices = c("(none)" = "", other_cols)
        )
      }
    )
  })

  # ---- Barplot variable selector ----
  output$g_barplot_ui <- renderUI({
    if (is.null(input$g_dataset) || input$g_dataset == "") {
      return(helpText(
        "No dataset selected.",
        "We'll insert a generic template like:",
        "  barplot(table(x))"
      ))
    }

    dataset_name <- input$g_dataset
    if (!is.null(uploaded_name()) && dataset_name == uploaded_name()) {
      df <- uploaded_data()
    } else {
      df <- data_list[[dataset_name]]
    }

    cat_cols <- names(df)[!sapply(df, is.numeric)]

    if (length(cat_cols) == 0) {
      return(helpText("No categorical variables found in this dataset."))
    }

    selectInput(
      "g_barplot_var",
      "Choose a categorical variable:",
      choices = cat_cols
    )
  })

  # ---- Pie chart variable selector ----
  output$g_piechart_ui <- renderUI({
    if (is.null(input$g_dataset) || input$g_dataset == "") {
      return(helpText(
        "No dataset selected.",
        "We'll insert a generic template like:",
        "  pie(table(x))"
      ))
    }

    dataset_name <- input$g_dataset
    if (!is.null(uploaded_name()) && dataset_name == uploaded_name()) {
      df <- uploaded_data()
    } else {
      df <- data_list[[dataset_name]]
    }

    cat_cols <- names(df)[!sapply(df, is.numeric)]

    if (length(cat_cols) == 0) {
      return(helpText("No categorical variables found in this dataset."))
    }

    selectInput(
      "g_piechart_var",
      "Choose a categorical variable:",
      choices = cat_cols
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

    dataset_name <- input$g_dataset
    if (!is.null(uploaded_name()) && dataset_name == uploaded_name()) {
      df <- uploaded_data()
    } else {
      df <- data_list[[dataset_name]]
    }

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
      line <- paste(
        "x_clean <- x[is.finite(x)]",
        "x_mean <- mean(x_clean, na.rm = TRUE)",
        "x_sd <- sd(x_clean, na.rm = TRUE)",
        sprintf(
          "hist(x_clean, breaks = %s, probability = TRUE, col = 'gray85', border = 'white',",
          input$g_breaks
        ),
        "     main = sprintf('Histogram + KDE (mean = %.2f, sd = %.2f)', x_mean, x_sd),",
        "     xlab = 'x')",
        "if (length(x_clean) > 1) lines(density(x_clean, na.rm = TRUE), col = 'steelblue', lwd = 2)",
        sep = "\n"
      )
    } else {
      req(input$g_hist_var, input$g_breaks)
      dataset_name <- input$g_dataset
      hist_var <- safe_col(input$g_hist_var)
      line <- sprintf(
        paste(
          "x_clean <- %s$%s[is.finite(%s$%s)]",
          "x_mean <- mean(x_clean, na.rm = TRUE)",
          "x_sd <- sd(x_clean, na.rm = TRUE)",
          "hist(x_clean, breaks = %s, probability = TRUE, col = 'gray85', border = 'white',",
          "     main = sprintf('Histogram + KDE (mean = %%.2f, sd = %%.2f)', x_mean, x_sd),",
          "     xlab = '%s')",
          "if (length(x_clean) > 1) lines(density(x_clean, na.rm = TRUE), col = 'steelblue', lwd = 2)",
          sep = "\n"
        ),
        dataset_name, hist_var, dataset_name, hist_var,
        input$g_breaks,
        input$g_hist_var
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
      x_var <- safe_col(input$g_scatter_x)
      y_var <- safe_col(input$g_scatter_y)
      line <- sprintf(
        "plot(%s ~ %s, data = %s)",
        y_var,
        x_var,
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

  # ---- Insert boxplot code ----
  observeEvent(input$insert_boxplot, {
    if (is.null(input$g_dataset) || input$g_dataset == "") {
      line <- "boxplot(x ~ group)"
    } else {
      req(input$g_boxplot_var)
      dataset_name <- input$g_dataset
      var <- safe_col(input$g_boxplot_var)
      group <- input$g_boxplot_group
      
      if (is.null(group) || group == "") {
        # Single boxplot
        line <- sprintf("boxplot(%s$%s)", dataset_name, var)
      } else {
        # Side-by-side boxplots
        group_safe <- safe_col(group)
        line <- sprintf("boxplot(%s$%s ~ %s$%s)", dataset_name, var, dataset_name, group_safe)
      }
    }

    old_code <- input$code
    if (is.null(old_code)) old_code <- ""
    new_code <- if (nzchar(old_code)) paste(old_code, line, sep = "\n") else line
    updateTextAreaInput(session, "code", value = new_code)
    current_fun("boxplot")
  })

  # ---- Insert barplot code ----
  observeEvent(input$insert_barplot, {
    if (is.null(input$g_dataset) || input$g_dataset == "") {
      line <- "barplot(table(x))"
    } else {
      req(input$g_barplot_var)
      dataset_name <- input$g_dataset
      var <- safe_col(input$g_barplot_var)
      line <- sprintf("barplot(table(%s$%s))", dataset_name, var)
    }

    old_code <- input$code
    if (is.null(old_code)) old_code <- ""
    new_code <- if (nzchar(old_code)) paste(old_code, line, sep = "\n") else line
    updateTextAreaInput(session, "code", value = new_code)
    current_fun("barplot")
  })

  # ---- Insert pie chart code ----
  observeEvent(input$insert_piechart, {
    if (is.null(input$g_dataset) || input$g_dataset == "") {
      line <- "pie(table(x))"
    } else {
      req(input$g_piechart_var)
      dataset_name <- input$g_dataset
      var <- safe_col(input$g_piechart_var)
      line <- sprintf("pie(table(%s$%s))", dataset_name, var)
    }

    old_code <- input$code
    if (is.null(old_code)) old_code <- ""
    new_code <- if (nzchar(old_code)) paste(old_code, line, sep = "\n") else line
    updateTextAreaInput(session, "code", value = new_code)
    current_fun("pie")
  })

  observeEvent(input$insert_clt, {
    dist <- input$clt_dist
    n <- input$clt_n
    reps <- input$clt_reps

    if (dist == "Normal") {
      line <- sprintf(
        "means <- replicate(%s, mean(rnorm(%s, mean = %s, sd = %s)))\nhist(means)",
        reps, n, input$clt_mean, input$clt_sd
      )
    } else if (dist == "Uniform") {
      line <- sprintf(
        "means <- replicate(%s, mean(runif(%s, min = %s, max = %s)))\nhist(means)",
        reps, n, input$clt_min, input$clt_max
      )
    } else if (dist == "Bernoulli") {
      line <- sprintf(
        "means <- replicate(%s, mean(rbinom(%s, size = 1, prob = %s)))\nhist(means)",
        reps, n, input$clt_p
      )
    } else if (dist == "Exponential") {
      line <- sprintf(
        "means <- replicate(%s, mean(rexp(%s, rate = %s)))\nhist(means)",
        reps, n, input$clt_rate
      )
    } else if (dist == "Custom pdf") {
      line <- sprintf(
        paste(
          "f <- function(x) %s",
          "sample_custom <- function(n, min = %s, max = %s, M = %s) {",
          "  out <- numeric(0)",
          "  while (length(out) < n) {",
          "    x <- runif(1, min = min, max = max)",
          "    u <- runif(1, 0, M)",
          "    if (u <= f(x)) out <- c(out, x)",
          "  }",
          "  out",
          "}",
          "means <- replicate(%s, mean(sample_custom(%s, min = %s, max = %s, M = %s)))",
          "hist(means)",
          sep = "\n"
        ),
        input$clt_pdf_expr,
        input$clt_pdf_min,
        input$clt_pdf_max,
        input$clt_pdf_m,
        reps,
        n,
        input$clt_pdf_min,
        input$clt_pdf_max,
        input$clt_pdf_m
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
    current_fun("mean")
  })

  # =====================
  # Run code + outputs (combined)
  # =====================

  observeEvent(input$run, {
    code_text <- input$code

    # ---- Safety check: blocklist dangerous functions ----
    safety_error <- check_code_safety(code_text)
    if (!is.null(safety_error)) {
      output$combined_output <- renderUI({
        tagList(
          verbatimTextOutput("error_msg")
        )
      })
      output$error_msg <- renderText({ safety_error })
      return(NULL)
    }

    # Parse into multiple expressions
    exprs <- try(parse(text = code_text), silent = TRUE)

    if (inherits(exprs, "try-error")) {
      output$combined_output <- renderUI({
        verbatimTextOutput("error_msg")
      })
      output$error_msg <- renderText({
        paste("Parse error:\n", attr(exprs, "condition")$message)
      })
      return(NULL)
    }

    # Evaluate each expression, capture printed output
    all_out <- character()

    for (expr in exprs) {
      # Capture text output
      this_out <- try(
        {
          setTimeLimit(elapsed = 5, transient = TRUE)
          captured <- capture.output({
            res <- withVisible(eval(expr, envir = user_env))
            if (res$visible) print(res$value)
          })
          captured
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

    # Build combined output - always show both text and plot areas
    output$combined_output <- renderUI({
      # Set up text output
      output$text_result <- renderText({
        if (length(all_out) > 0) {
          paste(all_out, collapse = "\n")
        } else {
          ""
        }
      })
      
      # Set up plot output (will only display if plot is created)
      output$plot_result <- renderPlot({
        setTimeLimit(elapsed = 5, transient = TRUE)
        eval(parse(text = code_text), envir = user_env)
      }, height = 400)
      
      tagList(
        verbatimTextOutput("text_result"),
        plotOutput("plot_result")
      )
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
