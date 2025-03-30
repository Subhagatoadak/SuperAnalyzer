library(shiny)
library(bs4Dash)
library(shinyjs)
library(DT)
library(sortable)      # For drag-and-drop interface
library(httr)          # For HTTP requests
library(jsonlite)      # For JSON handling
library(rpivotTable)
library(moments)
library(plotly)
library(randomForest)  # For Random Forest
library(rpart)         # For Decision Tree
library(pls)           # For Partial Least Squares
library(e1071)         # For SVM & Naive Bayes
library(forecast)      # For forecasting models

#--------------------------
# Helper Functions
#--------------------------
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#--------------------------
# Data Exploration Module
#--------------------------
dataExplorationUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Missing Value Analysis
    bs4Accordion(
      id = ns("missing_accordion"),
      bs4AccordionItem(
        id = ns("missing_values"),
        title = "Missing Value Analysis",
        collapsed = TRUE,
        tableOutput(ns("missingTable")),
        fluidRow(
          column(4, actionButton(ns("impute_mean"), "Impute Using Mean")),
          column(4, actionButton(ns("impute_median"), "Impute Using Median")),
          column(4, actionButton(ns("impute_custom_btn"), "Custom Impute Code"))
        )
      )
    ),
    h4("Pivot Table"),
    div(style = "max-height:600px; overflow-y:auto;", 
        rpivotTableOutput(ns("pivotTable"))
    ),
    # Correlation/Moments & Distribution Plots
    bs4Accordion(
      id = ns("accordion1"),
      bs4AccordionItem(
        id = ns("corr_moments"),
        title = "Correlation Matrix and Moments",
        collapsed = TRUE,
        h4("Correlation Matrix"),
        tableOutput(ns("correlationTable")),
        h4("Moments (Mean, Median, Mode, Variance, Skewness, Kurtosis)"),
        tableOutput(ns("statsTable"))
      ),
      bs4AccordionItem(
        id = ns("dist_plots"),
        title = "Distribution Plots",
        collapsed = TRUE,
        h4("Continuous Variable Plots"),
        selectInput(ns("cont_var"), "Select Continuous Variable", choices = NULL),
        fluidRow(
          column(4, plotlyOutput(ns("densityPlot"))),
          column(4, plotlyOutput(ns("violinPlot"))),
          column(4, plotlyOutput(ns("boxPlot")))
        ),
        hr(),
        h4("Discrete Variable Plot"),
        selectInput(ns("disc_var"), "Select Discrete Variable", choices = NULL),
        fluidRow(
          column(12, plotlyOutput(ns("barPlot")))
        ),
        hr(),
        h4("Outlier Analysis"),
        verbatimTextOutput(ns("outlierAnalysis")),
        actionButton(ns("handle_outliers"), "Handle Outliers")
      )
    )
  )
}

dataExplorationServer <- function(id, dataset, varTypes = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      # Missing Value Analysis
      output$missingTable <- renderTable({
        df <- dataset()
        req(df)
        missing_percent <- sapply(df, function(x) round(sum(is.na(x)) / length(x) * 100, 2))
        data.frame(Variable = names(missing_percent), Missing_Percentage = missing_percent)
      })
      
      # Imputation Observers (handled here or in main app)
      observeEvent(input$impute_mean, {
        df <- dataset()
        req(df)
        for(col in names(df)) {
          if(is.numeric(df[[col]]) && any(is.na(df[[col]]))) {
            df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
          }
        }
        dataset(df)
        showNotification("Missing values imputed using mean.", type = "message")
      })
      
      observeEvent(input$impute_median, {
        df <- dataset()
        req(df)
        for(col in names(df)) {
          if(is.numeric(df[[col]]) && any(is.na(df[[col]]))) {
            df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
          }
        }
        dataset(df)
        showNotification("Missing values imputed using median.", type = "message")
      })
      
      observeEvent(input$impute_custom_btn, {
        req(dataset())
        df <- dataset()
        schema_text <- paste(capture.output(str(df)), collapse = "\n")
        showModal(modalDialog(
          title = "Custom Missing Value Imputation",
          tagList(
            h4("Dataset Schema:"),
            pre(schema_text),
            p("Note: Your dataset is stored as 'df'. Write R code to impute missing values."),
            textAreaInput(session$ns("impute_code"), "Enter Imputation Code", value = "", rows = 5, width = "100%")
          ),
          footer = tagList(
            actionButton(session$ns("run_impute"), "Run Imputation"),
            modalButton("Close")
          ),
          size = "l"
        ))
      })
      
      observeEvent(input$run_impute, {
        req(dataset())
        df <- dataset()
        tryCatch({
          df_imputed <- eval(parse(text = input$impute_code), envir = list(df = df))
          if (!is.null(df_imputed)) {
            dataset(df_imputed)
            showNotification("Custom imputation applied.", type = "message")
            removeModal()
          }
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
        })
      })
      
      # Pivot Table
      output$pivotTable <- renderRpivotTable({
        df <- dataset()
        req(df)
        rpivotTable(df)
      })
      
      # Correlation Matrix (only continuous variables)
      output$correlationTable <- renderTable({
        df <- dataset()
        req(df)
        if (!is.null(varTypes)) {
          vt <- varTypes()
          cont_vars <- names(vt)[vt == "Continuous"]
        } else {
          cont_vars <- names(df)[sapply(df, is.numeric)]
        }
        if (length(cont_vars) > 1) {
          round(cor(df[, cont_vars, drop = FALSE], use = "complete.obs"), 2)
        } else {
          data.frame(Message = "Not enough continuous variables for correlation matrix")
        }
      })
      
      # Moments Table (only for continuous variables)
      output$statsTable <- renderTable({
        df <- dataset()
        req(df)
        if (!is.null(varTypes)) {
          vt <- varTypes()
          cont_vars <- names(vt)[vt == "Continuous"]
        } else {
          cont_vars <- names(df)[sapply(df, is.numeric)]
        }
        if (length(cont_vars) > 0) {
          stats <- data.frame(
            Variable = cont_vars,
            Mean = sapply(df[, cont_vars, drop = FALSE], function(x) round(mean(x, na.rm = TRUE), 2)),
            Median = sapply(df[, cont_vars, drop = FALSE], function(x) round(median(x, na.rm = TRUE), 2)),
            Mode = sapply(df[, cont_vars, drop = FALSE], function(x) get_mode(x)),
            Variance = sapply(df[, cont_vars, drop = FALSE], function(x) round(var(x, na.rm = TRUE), 2)),
            Skewness = sapply(df[, cont_vars, drop = FALSE], function(x) round(skewness(x, na.rm = TRUE), 2)),
            Kurtosis = sapply(df[, cont_vars, drop = FALSE], function(x) round(kurtosis(x, na.rm = TRUE), 2))
          )
          stats
        } else {
          data.frame(Message = "No continuous variables available for statistics")
        }
      })
      
      # Update continuous and discrete variable select inputs
      observe({
        df <- dataset()
        req(df)
        if (!is.null(varTypes)) {
          vt <- varTypes()
          continuous_vars <- names(vt)[vt == "Continuous"]
          discrete_vars <- names(vt)[vt == "Discrete"]
        } else {
          continuous_vars <- names(df)[sapply(df, is.numeric)]
          discrete_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
        }
        updateSelectInput(session, "cont_var", choices = continuous_vars)
        updateSelectInput(session, "disc_var", choices = discrete_vars)
      })
      
      ### Continuous Variable Plots using plotly ###
      output$densityPlot <- renderPlotly({
        df <- dataset()
        req(df, input$cont_var)
        x <- df[[input$cont_var]]
        dens <- density(x, na.rm = TRUE)
        plot_ly(x = dens$x, y = dens$y, type = 'scatter', mode = 'lines') %>%
          layout(title = paste("Density Plot of", input$cont_var),
                 xaxis = list(title = input$cont_var),
                 yaxis = list(title = "Density"))
      })
      
      output$violinPlot <- renderPlotly({
        df <- dataset()
        req(df, input$cont_var)
        plot_ly(df, y = ~get(input$cont_var), type = 'violin',
                box = list(visible = TRUE),
                meanline = list(visible = TRUE)) %>%
          layout(title = paste("Violin Plot of", input$cont_var),
                 yaxis = list(title = input$cont_var))
      })
      
      output$boxPlot <- renderPlotly({
        df <- dataset()
        req(df, input$cont_var)
        plot_ly(df, y = ~get(input$cont_var), type = 'box') %>%
          layout(title = paste("Box Plot of", input$cont_var),
                 yaxis = list(title = input$cont_var))
      })
      
      ### Outlier Analysis for selected continuous variable ###
      output$outlierAnalysis <- renderPrint({
        df <- dataset()
        req(df, input$cont_var)
        x <- df[[input$cont_var]]
        x <- x[!is.na(x)]
        Q1 <- quantile(x, 0.25)
        Q3 <- quantile(x, 0.75)
        IQR_val <- IQR(x)
        lower_bound <- Q1 - 1.5 * IQR_val
        upper_bound <- Q3 + 1.5 * IQR_val
        outliers <- x[x < lower_bound | x > upper_bound]
        cat("Outlier Analysis for", input$cont_var, "\n")
        cat("Lower Bound:", lower_bound, "\n")
        cat("Upper Bound:", upper_bound, "\n")
        cat("Number of Outliers:", length(outliers), "\n")
        if(length(outliers) > 0) {
          cat("Outlier Values:", paste(round(outliers, 2), collapse = ", "), "\n")
        }
      })
      
      ### Discrete Variable Plot (Bar plot) using plotly ###
      output$barPlot <- renderPlotly({
        df <- dataset()
        req(df, input$disc_var)
        freq <- as.data.frame(table(df[[input$disc_var]]))
        colnames(freq) <- c("Value", "Count")
        plot_ly(freq, x = ~Value, y = ~Count, type = 'bar') %>%
          layout(title = paste("Bar Plot of", input$disc_var),
                 xaxis = list(title = input$disc_var),
                 yaxis = list(title = "Count"))
      })
      
      ### Outlier Handling Modal ###
      observeEvent(input$handle_outliers, {
        req(dataset())
        df <- dataset()
        schema_text <- paste(capture.output(str(df)), collapse = "\n")
        showModal(modalDialog(
          title = "Outlier Handling",
          tagList(
            h4("Dataset Schema:"),
            pre(schema_text),
            p("Note: Your dataset is stored as 'df'. Write R code to handle outliers."),
            textAreaInput(session$ns("outlier_code"), "Enter Outlier Handling Code", value = "", rows = 5, width = "100%")
          ),
          footer = tagList(
            actionButton(session$ns("run_outlier"), "Run Outlier Handling"),
            modalButton("Close")
          ),
          size = "l"
        ))
      })
      
      observeEvent(input$run_outlier, {
        req(dataset())
        df <- dataset()
        tryCatch({
          df_out <- eval(parse(text = input$outlier_code), envir = list(df = df))
          if(!is.null(df_out)) {
            dataset(df_out)
            showNotification("Outlier handling applied.", type = "message")
            removeModal()
          }
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
        })
      })
    }
  )
}

#--------------------------
# Main App UI
#--------------------------
ui_main <- bs4DashPage(
  title = "Modern Bayesian Tool",
  header = bs4DashNavbar(
    title = "Modern Bayesian Tool",
    skin = "light"
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    brandColor = "primary",
    bs4SidebarMenu(
      bs4SidebarMenuItem("Data Transformation", tabName = "data_transform", icon = icon("table")),
      bs4SidebarMenuItem("Data Exploration", tabName = "data_exploration", icon = icon("search")),
      bs4SidebarMenuItem("Modeling Methods", tabName = "modeling_methods", icon = icon("chart-line")),
      bs4SidebarMenuItem("OpenAI Chat", tabName = "openai_chat", icon = icon("robot"))
    )
  ),
  body = bs4DashBody(
    useShinyjs(),
    bs4TabItems(
      # Data Transformation Tab
      bs4TabItem(
        tabName = "data_transform",
        fluidRow(
          bs4Card(
            title = "Upload and Transform Data",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            fluidRow(
              column(12,
                     fileInput("file", "Upload CSV Data", accept = ".csv"),
                     actionButton("show_code", "Show Transformation Editor"),
                     br(), br(),
                     actionButton("set_var_types", "Set Variable Types")
              ),
              br(),
              column(12,
                     DTOutput("data_table"),
                     br(),
                     actionButton("go_to_initial", "Revert to Initial Data")
              )
            )
          )
        )
      ),
      
      # Data Exploration Tab
      bs4TabItem(
        tabName = "data_exploration",
        fluidRow(
          bs4Card(
            title = "Data Exploration",
            status = "info",
            width = 12,
            solidHeader = TRUE,
            dataExplorationUI("exploration")
          )
        )
      ),
      
      # Modeling Methods Tab (Card View)
      bs4TabItem(
        tabName = "modeling_methods",
        fluidRow(
          # Row 1: Regression Models
          column(4,
                 bs4Card(
                   title = "Linear Regression",
                   status = "info",
                   solidHeader = TRUE,
                   footer = actionButton("open_lr", "Open")
                 )
          ),
          column(4,
                 bs4Card(
                   title = "Logistic Regression",
                   status = "info",
                   solidHeader = TRUE,
                   footer = actionButton("open_logr", "Open")
                 )
          ),
          column(4,
                 bs4Card(
                   title = "Bayesian Regression",
                   status = "info",
                   solidHeader = TRUE,
                   footer = actionButton("open_bayes", "Open")
                 )
          )
        ),
        fluidRow(
          # Row 2: Classification Models
          column(4,
                 bs4Card(
                   title = "Random Forest",
                   status = "info",
                   solidHeader = TRUE,
                   footer = actionButton("open_rf", "Open")
                 )
          ),
          column(4,
                 bs4Card(
                   title = "Decision Tree",
                   status = "info",
                   solidHeader = TRUE,
                   footer = actionButton("open_dt", "Open")
                 )
          ),
          column(4,
                 bs4Card(
                   title = "SVM / Naive Bayes",
                   status = "info",
                   solidHeader = TRUE,
                   footer = actionButton("open_class", "Open")
                 )
          )
        ),
        fluidRow(
          # Row 3: Forecasting & Hypothesis Testing & Clustering
          column(4,
                 bs4Card(
                   title = "Forecasting Models",
                   status = "info",
                   solidHeader = TRUE,
                   footer = actionButton("open_forecast", "Open")
                 )
          ),
          column(4,
                 bs4Card(
                   title = "Hypothesis Testing",
                   status = "info",
                   solidHeader = TRUE,
                   footer = actionButton("open_ht", "Open")
                 )
          ),
          column(4,
                 bs4Card(
                   title = "Clustering",
                   status = "info",
                   solidHeader = TRUE,
                   footer = actionButton("open_cluster", "Open")
                 )
          )
        ),
        fluidRow(
          # Row 4: Custom Model Options
          column(6,
                 bs4Card(
                   title = "Custom Model Code",
                   status = "warning",
                   solidHeader = TRUE,
                   footer = actionButton("open_custom_code", "Open")
                 )
          ),
          column(6,
                 bs4Card(
                   title = "Create New Model Card",
                   status = "primary",
                   solidHeader = TRUE,
                   footer = actionButton("create_new_model", "Create New Card")
                 )
          )
        ),
        fluidRow(
          # Dynamically created custom model cards will be rendered here
          uiOutput("new_model_cards")
        )
      ),
      
      # OpenAI Chat Tab
      bs4TabItem(
        tabName = "openai_chat",
        fluidRow(
          bs4Card(
            title = "OpenAI Chat",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            selectInput("openai_model", "Select Model", 
                        choices = c("gpt-4o", "o3-mini-2025-01-31", "gpt-3.5-turbo-16k", "gpt-4o-mini", "chatgpt-4o-latest"),
                        selected = "gpt-4o"),
            textAreaInput("openai_input", "Enter your question:", "", rows = 4, width = "100%"),
            actionButton("openai_send", "Send"),
            br(), br(),
            uiOutput("openai_output")
          )
        )
      )
    )
  )
)

#--------------------------
# Server
#--------------------------
server <- function(input, output, session) {
  ## Global Reactive Values ##
  data_reactive <- reactiveVal(NULL)
  initial_df <- reactiveVal(NULL)
  previous_df <- reactiveVal(NULL)
  variable_types <- reactiveVal(NULL)
  saved_models <- reactiveValues(
    lr = NULL, logr = NULL, bayes = NULL,
    rf = NULL, dt = NULL, class = NULL,
    forecast = NULL, ht = NULL, cluster = NULL,
    custom = list()
  )
  custom_model_cards <- reactiveValues(cards = list())
  
  #### Data Loading and Transformation ####
  observeEvent(input$file, {
    req(input$file)
    df <- tryCatch(
      read.csv(input$file$datapath, stringsAsFactors = FALSE),
      error = function(e) {
        showNotification("Error reading file", type = "error")
        NULL
      }
    )
    data_reactive(df)
    initial_df(df)
    if (!is.null(df)) {
      default_types <- sapply(df, function(x) if (is.numeric(x)) "Continuous" else "Discrete")
      variable_types(default_types)
    }
  })
  
  output$data_table <- renderDT({
    req(data_reactive())
    datatable(data_reactive(), editable = TRUE, options = list(pageLength = 5))
  })
  
  observeEvent(input$data_table_cell_edit, {
    info <- input$data_table_cell_edit
    df <- data_reactive()
    colName <- names(df)[as.numeric(info$col)]
    newValue <- if(is.numeric(df[[colName]])) as.numeric(info$value) else info$value
    df[as.numeric(info$row), as.numeric(info$col)] <- newValue
    data_reactive(df)
  })
  
  # Transformation Modal Window
  observeEvent(input$show_code, {
    req(data_reactive())
    df <- data_reactive()
    datasetName <- if (!is.null(input$file)) input$file$name else "Dataset"
    schema_text <- paste(capture.output(str(df)), collapse = "\n")
    showModal(modalDialog(
      title = "Transformation Editor",
      tagList(
        h4(paste("Dataset:", datasetName)),
        div(style = "max-height:500px; overflow-y:auto;", DTOutput("data_table")),
        pre(schema_text),
        p("Note: The dataset is stored in the variable 'df'."),
        textAreaInput("transformation_code", "Enter Transformation Code", value = "", rows = 5, width = "100%")
      ),
      footer = tagList(
        actionButton("run_transformation_modal", "Run Transformation"),
        actionButton("revert_previous", "Revert to Previous"),
        actionButton("revert_initial", "Revert to Initial"),
        modalButton("Close")
      ),
      size = "l"
    ))
  })
  
  observeEvent(input$run_transformation_modal, {
    req(data_reactive())
    df <- data_reactive()
    previous_df(df)
    tryCatch({
      df_transformed <- eval(parse(text = input$transformation_code), envir = list(df = df))
      if (!is.null(df_transformed)) {
        data_reactive(df_transformed)
        showNotification("Transformation applied.", type = "message")
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  observeEvent(input$revert_previous, {
    req(previous_df())
    data_reactive(previous_df())
    showNotification("Reverted to previous state.", type = "message")
  })
  
  observeEvent(input$revert_initial, {
    req(initial_df())
    data_reactive(initial_df())
    showNotification("Reverted to initial state.", type = "message")
  })
  
  observeEvent(input$go_to_initial, {
    req(initial_df())
    data_reactive(initial_df())
    showNotification("Reverted to initial state.", type = "message")
  })
  
  #### Variable Type Setting (Drag and Drop) ####
  observeEvent(input$set_var_types, {
    req(data_reactive())
    df <- data_reactive()
    var_names <- names(df)
    current_types <- variable_types()
    if (is.null(current_types)) {
      current_types <- sapply(df, function(x) if (is.numeric(x)) "Continuous" else "Discrete")
    }
    initial_cont <- var_names[current_types == "Continuous"]
    initial_disc <- var_names[current_types == "Discrete"]
    showModal(modalDialog(
      title = "Set Variable Types",
      bucket_list(
        header = "Drag and drop variables to assign types",
        group_name = "variable_types_group",
        add_rank_list(
          text = "Continuous Variables",
          labels = initial_cont,
          input_id = "cont_list"
        ),
        add_rank_list(
          text = "Discrete Variables",
          labels = initial_disc,
          input_id = "disc_list"
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_var_types", "Save")
      ),
      size = "l"
    ))
  })
  
  observeEvent(input$save_var_types, {
    req(data_reactive())
    cont_vars <- input$cont_list
    disc_vars <- input$disc_list
    df <- data_reactive()
    var_names <- names(df)
    new_types <- sapply(var_names, function(var) {
      if (!is.null(cont_vars) && var %in% cont_vars) "Continuous" else if (!is.null(disc_vars) && var %in% disc_vars) "Discrete" else if (is.numeric(df[[var]])) "Continuous" else "Discrete"
    })
    variable_types(new_types)
    removeModal()
    showNotification("Variable types updated.", type = "message")
  })
  
  #### Navigation ####
  observeEvent(input$go_exploration, {
    updateTabItems(session, "navbar", "data_exploration")
  })
  
  observeEvent(input$go_modeling, {
    updateTabItems(session, "navbar", "modeling_methods")
  })
  
  #### Data Exploration Module ####
  dataExplorationServer("exploration", dataset = data_reactive, varTypes = variable_types)
  
  #### Modeling Methods Observers ####
  # For simplicity, each model observer opens a modal with inputs for dependent and independent variables (if needed),
  # a text area with example code in a tooltip, and buttons to run and save the model.
  
  # Linear Regression
  observeEvent(input$open_lr, {
    req(data_reactive())
    df <- data_reactive()
    showModal(modalDialog(
      title = "Linear Regression",
      tagList(
        selectInput("lr_dep_var", "Dependent Variable", choices = names(df)),
        selectInput("lr_indep_vars", "Independent Variables", choices = names(df), multiple = TRUE),
        textAreaInput("lr_model_code", "Model Code", value = "lm(dep ~ indep, data = df)", rows = 3, width = "100%"),
        p("Example: lm(Y ~ X1 + X2, data = df)"),
        p("Note: The dataset is stored as 'df'.")
      ),
      footer = tagList(
        actionButton("run_lr", "Run Model"),
        actionButton("save_lr", "Save Model"),
        modalButton("Close")
      ),
      size = "l"
    ))
  })
  
  observeEvent(input$run_lr, {
    req(data_reactive())
    df <- data_reactive()
    req(input$lr_dep_var, input$lr_indep_vars)
    formula_lr <- as.formula(paste(input$lr_dep_var, "~", paste(input$lr_indep_vars, collapse = "+")))
    model_lr <- tryCatch({
      lm(formula_lr, data = df)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    output$lr_model_output <- renderPrint({ summary(model_lr) })
  })
  
  observeEvent(input$save_lr, {
    req(data_reactive())
    df <- data_reactive()
    req(input$lr_dep_var, input$lr_indep_vars)
    formula_lr <- as.formula(paste(input$lr_dep_var, "~", paste(input$lr_indep_vars, collapse = "+")))
    model_lr <- tryCatch({
      lm(formula_lr, data = df)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    saved_models$lr <- model_lr
    showNotification("Linear model saved.", type = "message")
  })
  
  # Logistic Regression
  observeEvent(input$open_logr, {
    req(data_reactive())
    df <- data_reactive()
    showModal(modalDialog(
      title = "Logistic Regression",
      tagList(
        selectInput("logr_dep_var", "Dependent Variable", choices = names(df)),
        selectInput("logr_indep_vars", "Independent Variables", choices = names(df), multiple = TRUE),
        textAreaInput("logr_model_code", "Model Code", value = "glm(dep ~ indep, data = df, family = binomial)", rows = 3, width = "100%"),
        p("Example: glm(Y ~ X1 + X2, data = df, family = binomial)"),
        p("Note: The dataset is stored as 'df'.")
      ),
      footer = tagList(
        actionButton("run_logr", "Run Model"),
        actionButton("save_logr", "Save Model"),
        modalButton("Close")
      ),
      size = "l"
    ))
  })
  
  observeEvent(input$run_logr, {
    req(data_reactive())
    df <- data_reactive()
    req(input$logr_dep_var, input$logr_indep_vars)
    formula_logr <- as.formula(paste(input$logr_dep_var, "~", paste(input$logr_indep_vars, collapse = "+")))
    model_logr <- tryCatch({
      glm(formula_logr, data = df, family = binomial)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    output$logr_model_output <- renderPrint({ summary(model_logr) })
  })
  
  observeEvent(input$save_logr, {
    req(data_reactive())
    df <- data_reactive()
    req(input$logr_dep_var, input$logr_indep_vars)
    formula_logr <- as.formula(paste(input$logr_dep_var, "~", paste(input$logr_indep_vars, collapse = "+")))
    model_logr <- tryCatch({
      glm(formula_logr, data = df, family = binomial)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    saved_models$logr <- model_logr
    showNotification("Logistic model saved.", type = "message")
  })
  
  # Bayesian Regression (simplified using rstan demo code)
  observeEvent(input$open_bayes, {
    req(data_reactive())
    df <- data_reactive()
    showModal(modalDialog(
      title = "Bayesian Regression",
      tagList(
        selectInput("bayes_dep_var", "Dependent Variable", choices = names(df)),
        selectInput("bayes_indep_vars", "Independent Variables", choices = names(df), multiple = TRUE),
        textAreaInput("bayes_model_code", "Model Code", 
                      value = "bayes_model_code <- 'data { int<lower=0> N; vector[N] y; } parameters { real mu; real<lower=0> sigma; } model { y ~ normal(mu, sigma); }';\n y <- df[[dep]];\n stan_data <- list(N = length(y), y = y);\n rstan::stan(model_code = bayes_model_code, data = stan_data, iter = 2000, chains = 4)",
                      rows = 5, width = "100%"),
        p("Example: Use rstan to fit a normal model on the dependent variable."),
        p("Note: The dataset is stored as 'df'. Replace 'dep' with your dependent variable.")
      ),
      footer = tagList(
        actionButton("run_bayes", "Run Model"),
        actionButton("save_bayes", "Save Model"),
        modalButton("Close")
      ),
      size = "l"
    ))
  })
  
  observeEvent(input$run_bayes, {
    req(data_reactive())
    df <- data_reactive()
    req(input$bayes_dep_var)
    # For simplicity, we use the code provided by the user directly
    custom_bayes_env <- list(df = df, dep = input$bayes_dep_var, indep = input$bayes_indep_vars)
    model_bayes <- tryCatch({
      eval(parse(text = input$bayes_model_code), envir = custom_bayes_env)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    output$bayes_model_output <- renderPrint({ model_bayes })
  })
  
  observeEvent(input$save_bayes, {
    req(data_reactive())
    df <- data_reactive()
    req(input$bayes_dep_var)
    custom_bayes_env <- list(df = df, dep = input$bayes_dep_var, indep = input$bayes_indep_vars)
    model_bayes <- tryCatch({
      eval(parse(text = input$bayes_model_code), envir = custom_bayes_env)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    saved_models$bayes <- model_bayes
    showNotification("Bayesian model saved.", type = "message")
  })
  
  # Random Forest
  observeEvent(input$open_rf, {
    req(data_reactive())
    df <- data_reactive()
    showModal(modalDialog(
      title = "Random Forest",
      tagList(
        selectInput("rf_dep_var", "Dependent Variable", choices = names(df)),
        selectInput("rf_indep_vars", "Independent Variables", choices = names(df), multiple = TRUE),
        textAreaInput("rf_model_code", "Model Code", value = "randomForest(dep ~ ., data = df)", rows = 3, width = "100%"),
        p("Example: randomForest(Y ~ X1 + X2, data = df)"),
        p("Note: The dataset is stored as 'df'.")
      ),
      footer = tagList(
        actionButton("run_rf", "Run Model"),
        actionButton("save_rf", "Save Model"),
        modalButton("Close")
      ),
      size = "l"
    ))
  })
  
  observeEvent(input$run_rf, {
    req(data_reactive())
    df <- data_reactive()
    req(input$rf_dep_var, input$rf_indep_vars)
    formula_rf <- as.formula(paste(input$rf_dep_var, "~", paste(input$rf_indep_vars, collapse = "+")))
    model_rf <- tryCatch({
      randomForest(formula_rf, data = df)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    output$rf_model_output <- renderPrint({ model_rf })
  })
  
  observeEvent(input$save_rf, {
    req(data_reactive())
    df <- data_reactive()
    req(input$rf_dep_var, input$rf_indep_vars)
    formula_rf <- as.formula(paste(input$rf_dep_var, "~", paste(input$rf_indep_vars, collapse = "+")))
    model_rf <- tryCatch({
      randomForest(formula_rf, data = df)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    saved_models$rf <- model_rf
    showNotification("Random Forest model saved.", type = "message")
  })
  
  # Decision Tree
  observeEvent(input$open_dt, {
    req(data_reactive())
    df <- data_reactive()
    showModal(modalDialog(
      title = "Decision Tree",
      tagList(
        selectInput("dt_dep_var", "Dependent Variable", choices = names(df)),
        selectInput("dt_indep_vars", "Independent Variables", choices = names(df), multiple = TRUE),
        textAreaInput("dt_model_code", "Model Code", value = "rpart(dep ~ ., data = df)", rows = 3, width = "100%"),
        p("Example: rpart(Y ~ X1 + X2, data = df)"),
        p("Note: The dataset is stored as 'df'.")
      ),
      footer = tagList(
        actionButton("run_dt", "Run Model"),
        actionButton("save_dt", "Save Model"),
        modalButton("Close")
      ),
      size = "l"
    ))
  })
  
  observeEvent(input$run_dt, {
    req(data_reactive())
    df <- data_reactive()
    req(input$dt_dep_var, input$dt_indep_vars)
    formula_dt <- as.formula(paste(input$dt_dep_var, "~", paste(input$dt_indep_vars, collapse = "+")))
    model_dt <- tryCatch({
      rpart(formula_dt, data = df)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    output$dt_model_output <- renderPrint({ model_dt })
  })
  
  observeEvent(input$save_dt, {
    req(data_reactive())
    df <- data_reactive()
    req(input$dt_dep_var, input$dt_indep_vars)
    formula_dt <- as.formula(paste(input$dt_dep_var, "~", paste(input$dt_indep_vars, collapse = "+")))
    model_dt <- tryCatch({
      rpart(formula_dt, data = df)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    saved_models$dt <- model_dt
    showNotification("Decision Tree model saved.", type = "message")
  })
  
  # SVM / Naive Bayes (as one card for simplicity)
  observeEvent(input$open_class, {
    req(data_reactive())
    df <- data_reactive()
    showModal(modalDialog(
      title = "SVM / Naive Bayes",
      tagList(
        selectInput("class_dep_var", "Dependent Variable", choices = names(df)),
        selectInput("class_indep_vars", "Independent Variables", choices = names(df), multiple = TRUE),
        textAreaInput("class_model_code", "Model Code", value = "svm(dep ~ ., data = df)", rows = 3, width = "100%"),
        p("Example (SVM): svm(Y ~ X1 + X2, data = df)"),
        p("Or for Naive Bayes: naiveBayes(dep ~ ., data = df)"),
        p("Note: The dataset is stored as 'df'.")
      ),
      footer = tagList(
        actionButton("run_class", "Run Model"),
        actionButton("save_class", "Save Model"),
        modalButton("Close")
      ),
      size = "l"
    ))
  })
  
  observeEvent(input$run_class, {
    req(data_reactive())
    df <- data_reactive()
    req(input$class_dep_var, input$class_indep_vars)
    # Here we try to evaluate the code provided by the user in the class card.
    class_env <- list(df = df, dep = input$class_dep_var, indep = input$class_indep_vars)
    model_class <- tryCatch({
      eval(parse(text = input$class_model_code), envir = class_env)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    output$class_model_output <- renderPrint({ model_class })
  })
  
  observeEvent(input$save_class, {
    req(data_reactive())
    df <- data_reactive()
    req(input$class_dep_var, input$class_indep_vars)
    class_env <- list(df = df, dep = input$class_dep_var, indep = input$class_indep_vars)
    model_class <- tryCatch({
      eval(parse(text = input$class_model_code), envir = class_env)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    saved_models$class <- model_class
    showNotification("SVM/Naive Bayes model saved.", type = "message")
  })
  
  # Forecasting Models (ARIMA, SARIMA, Exponential Smoothing)
  observeEvent(input$open_forecast, {
    req(data_reactive())
    df <- data_reactive()
    showModal(modalDialog(
      title = "Forecasting Models",
      tagList(
        selectInput("forecast_var", "Time Series Variable", choices = names(df)),
        textAreaInput("forecast_model_code", "Model Code", 
                      value = "forecast::auto.arima(df[[var]])", rows = 3, width = "100%"),
        p("Example: forecast::auto.arima(df[[TimeVar]])"),
        p("Note: 'df' holds the transformed data. Replace 'var' with your time series variable.")
      ),
      footer = tagList(
        actionButton("run_forecast", "Run Model"),
        actionButton("save_forecast", "Save Model"),
        modalButton("Close")
      ),
      size = "l"
    ))
  })
  
  observeEvent(input$run_forecast, {
    req(data_reactive())
    df <- data_reactive()
    req(input$forecast_var)
    forecast_env <- list(df = df, var = input$forecast_var)
    model_forecast <- tryCatch({
      eval(parse(text = input$forecast_model_code), envir = forecast_env)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    output$forecast_model_output <- renderPrint({ model_forecast })
  })
  
  observeEvent(input$save_forecast, {
    req(data_reactive())
    df <- data_reactive()
    req(input$forecast_var)
    forecast_env <- list(df = df, var = input$forecast_var)
    model_forecast <- tryCatch({
      eval(parse(text = input$forecast_model_code), envir = forecast_env)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    saved_models$forecast <- model_forecast
    showNotification("Forecasting model saved.", type = "message")
  })
  
  # Hypothesis Testing (Two Sample t-test, One Sample t-test, ANOVA)
  observeEvent(input$open_ht, {
    req(data_reactive())
    df <- data_reactive()
    showModal(modalDialog(
      title = "Hypothesis Testing",
      tagList(
        selectInput("ht_test_type", "Select Test Type", choices = c("Two Sample t-test", "One Sample t-test", "ANOVA")),
        selectInput("ht_dep_var", "Dependent Variable", choices = names(df)),
        selectInput("ht_group_var", "Grouping Variable (if applicable)", choices = c("None", names(df))),
        textAreaInput("ht_model_code", "Test Code", value = "", rows = 3, width = "100%"),
        p("Example for Two Sample t-test: t.test(df[[dep]] ~ as.factor(df[[group]]))"),
        p("Example for One Sample t-test: t.test(df[[dep]], mu=0)"),
        p("Example for ANOVA: aov(df[[dep]] ~ as.factor(df[[group]]))"),
        p("Note: The dataset is stored as 'df'.")
      ),
      footer = tagList(
        actionButton("run_ht", "Run Test"),
        actionButton("save_ht", "Save Test Result"),
        modalButton("Close")
      ),
      size = "l"
    ))
  })
  
  observeEvent(input$run_ht, {
    req(data_reactive())
    df <- data_reactive()
    test_env <- list(df = df, dep = input$ht_dep_var, group = if(input$ht_group_var != "None") input$ht_group_var else NULL)
    model_ht <- tryCatch({
      eval(parse(text = input$ht_model_code), envir = test_env)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    output$ht_model_output <- renderPrint({ model_ht })
  })
  
  observeEvent(input$save_ht, {
    req(data_reactive())
    df <- data_reactive()
    test_env <- list(df = df, dep = input$ht_dep_var, group = if(input$ht_group_var != "None") input$ht_group_var else NULL)
    model_ht <- tryCatch({
      eval(parse(text = input$ht_model_code), envir = test_env)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    saved_models$ht <- model_ht
    showNotification("Hypothesis test result saved.", type = "message")
  })
  
  # Clustering (e.g., k-means)
  observeEvent(input$open_cluster, {
    req(data_reactive())
    df <- data_reactive()
    showModal(modalDialog(
      title = "Clustering",
      tagList(
        selectInput("cluster_vars", "Variables for Clustering", choices = names(df), multiple = TRUE),
        numericInput("cluster_k", "Number of Clusters", value = 3, min = 1),
        textAreaInput("cluster_model_code", "Model Code", value = "kmeans(df[, vars], centers = k)", rows = 3, width = "100%"),
        p("Example: kmeans(df[, cluster_vars], centers = 3)"),
        p("Note: 'df' is your dataset, 'vars' will be replaced by the selected variables, and 'k' by the number of clusters.")
      ),
      footer = tagList(
        actionButton("run_cluster", "Run Clustering"),
        actionButton("save_cluster", "Save Model"),
        modalButton("Close")
      ),
      size = "l"
    ))
  })
  
  observeEvent(input$run_cluster, {
    req(data_reactive())
    df <- data_reactive()
    req(input$cluster_vars, input$cluster_k)
    cluster_env <- list(df = df, vars = input$cluster_vars, k = input$cluster_k)
    model_cluster <- tryCatch({
      eval(parse(text = input$cluster_model_code), envir = cluster_env)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    output$cluster_model_output <- renderPrint({ model_cluster })
  })
  
  observeEvent(input$save_cluster, {
    req(data_reactive())
    df <- data_reactive()
    req(input$cluster_vars, input$cluster_k)
    cluster_env <- list(df = df, vars = input$cluster_vars, k = input$cluster_k)
    model_cluster <- tryCatch({
      eval(parse(text = input$cluster_model_code), envir = cluster_env)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    saved_models$cluster <- model_cluster
    showNotification("Clustering model saved.", type = "message")
  })
  
  # Custom Model Code Card
  observeEvent(input$open_custom_code, {
    req(data_reactive())
    df <- data_reactive()
    showModal(modalDialog(
      title = "Custom Model Code",
      tagList(
        selectInput("custom_dep", "Dependent Variable", choices = names(df)),
        selectInput("custom_indep", "Independent Variables", choices = names(df), multiple = TRUE),
        textAreaInput("custom_model_code", "Enter Custom Model Code", value = "", rows = 5, width = "100%"),
        p("Example: lm(custom_dep ~ ., data = df)"),
        p("Note: 'df' holds the transformed dataset.")
      ),
      footer = tagList(
        actionButton("run_custom_model", "Run Model"),
        actionButton("save_custom_model", "Save Model"),
        modalButton("Close")
      ),
      size = "l"
    ))
  })
  
  observeEvent(input$run_custom_model, {
    req(data_reactive())
    df <- data_reactive()
    custom_env <- list(df = df, custom_dep = input$custom_dep, custom_indep = input$custom_indep)
    tryCatch({
      custom_result <- eval(parse(text = input$custom_model_code), envir = custom_env)
      output$custom_model_output <- renderPrint({ custom_result })
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      output$custom_model_output <- renderPrint({ e$message })
    })
  })
  
  observeEvent(input$save_custom_model, {
    req(data_reactive())
    df <- data_reactive()
    custom_env <- list(df = df, custom_dep = input$custom_dep, custom_indep = input$custom_indep)
    tryCatch({
      custom_result <- eval(parse(text = input$custom_model_code), envir = custom_env)
      saved_models$custom <- c(saved_models$custom, list(custom_result))
      showNotification("Custom model saved.", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Create New Model Card (Dynamic)
  custom_model_cards <- reactiveValues(cards = list())
  
  observeEvent(input$create_new_model, {
    showModal(modalDialog(
      title = "Create New Model Card",
      tagList(
        textInput("new_model_name", "Model Card Name", value = ""),
        selectInput("new_model_dep", "Dependent Variable", choices = names(data_reactive())),
        selectInput("new_model_indep", "Independent Variables", choices = names(data_reactive()), multiple = TRUE),
        textAreaInput("new_model_code", "Enter Model Code", value = "", rows = 5, width = "100%"),
        p("Example: lm(new_model_dep ~ new_model_indep, data = df)"),
        p("Note: 'df' holds the dataset.")
      ),
      footer = tagList(
        actionButton("save_new_model", "Create Card"),
        modalButton("Cancel")
      ),
      size = "l"
    ))
  })
  
  observeEvent(input$save_new_model, {
    req(input$new_model_name, data_reactive())
    new_card <- list(
      name = input$new_model_name,
      dep = input$new_model_dep,
      indep = input$new_model_indep,
      code = input$new_model_code
    )
    custom_model_cards$cards <- c(custom_model_cards$cards, list(new_card))
    removeModal()
    showNotification("New model card created.", type = "message")
  })
  
  output$new_model_cards <- renderUI({
    req(custom_model_cards$cards)
    card_list <- lapply(custom_model_cards$cards, function(card) {
      bs4Card(
        title = card$name,
        status = "primary",
        solidHeader = TRUE,
        footer = actionButton(paste0("open_custom_", card$name), "Open"),
        width = 4
      )
    })
    fluidRow(card_list)
  })
  
  # Dynamic observer for each new custom card
  observe({
    req(custom_model_cards$cards)
    for(card in custom_model_cards$cards) {
      local({
        this_card <- card
        observeEvent(input[[paste0("open_custom_", this_card$name)]], {
          showModal(modalDialog(
            title = paste("Custom Model -", this_card$name),
            tagList(
              selectInput("custom_dep_dynamic", "Dependent Variable", choices = names(data_reactive())),
              selectInput("custom_indep_dynamic", "Independent Variables", choices = names(data_reactive()), multiple = TRUE),
              textAreaInput("custom_model_code_dynamic", "Enter Model Code", value = this_card$code, rows = 5, width = "100%"),
              p("Note: The dataset is available as 'df'.")
            ),
            footer = tagList(
              actionButton("run_custom_model_dynamic", "Run Model"),
              actionButton("save_custom_model_dynamic", "Save Model"),
              modalButton("Close")
            ),
            size = "l"
          ))
        })
      })
    }
  })
  
  #### OpenAI Integration ####
  rv_chat <- reactiveValues(messages = list())
  
  output$openai_output <- renderUI({
    req(rv_chat$messages)
    message_tags <- lapply(rv_chat$messages, function(msg) {
      if (msg$role == "user") {
        tags$div(
          style = "background-color: #DCF8C6; padding: 10px; margin: 5px; border-radius: 10px; text-align: right; max-width:70%; align-self: flex-end;",
          tags$p(msg$text, style = "margin: 0;")
        )
      } else {
        tags$div(
          style = "background-color: #F1F0F0; padding: 10px; margin: 5px; border-radius: 10px; text-align: left; max-width:70%; align-self: flex-start;",
          tags$p(msg$text, style = "margin: 0;")
        )
      }
    })
    tags$div(style = "display: flex; flex-direction: column;", message_tags)
  })
  
  observeEvent(input$openai_send, {
    req(input$openai_input, input$openai_model)
    query <- input$openai_input
    model_choice <- input$openai_model
    showNotification("Sending query to OpenAI...", type = "message")
    
    api_key <- Sys.getenv("OPENAI_API_KEY")
    if(api_key == "") {
      showNotification("OPENAI_API_KEY not set", type = "error")
      return(NULL)
    }
    
    rv_chat$messages <- c(rv_chat$messages, list(list(role = "user", text = query)))
    
    res <- tryCatch({
      POST(
        url = "https://api.openai.com/v1/chat/completions",
        add_headers(
          "Authorization" = paste("Bearer", api_key),
          "Content-Type" = "application/json"
        ),
        body = toJSON(list(
          model = model_choice,
          messages = list(list(role = "user", content = query))
        ), auto_unbox = TRUE)
      )
    }, error = function(e) {
      showNotification(paste("POST request failed:", e$message), type = "error")
      return(NULL)
    })
    
    if(is.null(res)) return(NULL)
    
    if(http_error(res)) {
      showNotification(paste("HTTP error:", status_code(res)), type = "error")
      return(NULL)
    }
    
    response_content <- tryCatch({
      content(res, as = "parsed")
    }, error = function(e) {
      showNotification(paste("Error parsing response:", e$message), type = "error")
      return(NULL)
    })
    
    if(is.null(response_content) || is.null(response_content$choices)) {
      showNotification("No valid response from OpenAI", type = "error")
      return(NULL)
    }
    
    answer <- response_content$choices[[1]]$message$content
    if(is.null(answer)) {
      answer <- "No answer returned."
    }
    
    rv_chat$messages <- c(rv_chat$messages, list(list(role = "assistant", text = answer)))
    showNotification("Response received from OpenAI.", type = "message")
  })
}

shinyApp(ui = ui_main, server = server)
