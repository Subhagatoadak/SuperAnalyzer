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
library(shinyBS)

# Source the modules (data_exploration remains in a separate file)
source("modules/data_exploration.R")
# (For modeling methods, we are now defining UI and server directly below)

ui <- bs4DashPage(
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
      bs4SidebarMenuItem(
        "Data Transformation",
        tabName = "data_transform",
        icon = icon("table")
      ),
      bs4SidebarMenuItem(
        "Data Exploration",
        tabName = "data_exploration",
        icon = icon("search")
      ),
      bs4SidebarMenuItem(
        "Modeling Methods",
        tabName = "modeling_methods",
        icon = icon("chart-line")
      ),
      bs4SidebarMenuItem(
        "OpenAI Chat",
        tabName = "openai_chat",
        icon = icon("robot")
      )
    )
  ),
  body = bs4DashBody(
    useShinyjs(),
    bs4TabItems(
      # ---- Data Transformation Tab ----
      bs4TabItem(
        tabName = "data_transform",
        fluidRow(
          bs4Card(
            title = "Upload and Transform Data",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            fluidRow(
              column(
                width = 12,
                fileInput("file", "Upload CSV Data", accept = ".csv"),
                actionButton("show_code", "Show Transformation Editor"),
                br(), br(),
                actionButton("set_var_types", "Set Variable Types")
              ),
              br(),
              column(
                width = 12,
                DTOutput("data_table"),
                br(),
                actionButton("go_to_initial", "Revert to Initial Data")
              )
            )
          )
        )
      ),
      
      # ---- Data Exploration Tab ----
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
      
      # ---- Modeling Methods Tab ----
      # ---- Modeling Methods Tab ----
      bs4TabItem(
        tabName = "modeling_methods",
        fluidRow(
          # First row: Linear Regression and Logistic Regression cards
          column(
            width = 6,
            bs4Card(
              title = "Linear Regression",
              status = "info",
              solidHeader = TRUE,
              selectInput("lr_dep", "Dependent Variable", choices = NULL),
              selectInput("lr_indep", "Independent Variables", choices = NULL, multiple = TRUE),
              actionButton("run_lr", "Run Linear Regression"),
              actionButton("save_lr", "Save Model"),
              verbatimTextOutput("lr_summary")
            )
          ),
          column(
            width = 6,
            bs4Card(
              title = "Logistic Regression",
              status = "info",
              solidHeader = TRUE,
              selectInput("logr_dep", "Dependent Variable", choices = NULL),
              selectInput("logr_indep", "Independent Variables", choices = NULL, multiple = TRUE),
              actionButton("run_logr", "Run Logistic Regression"),
              actionButton("save_logr", "Save Model"),
              verbatimTextOutput("logr_summary")
            )
          )
        ),
        fluidRow(
          # Second row: Bayesian Regression and Custom Model Cards
          column(
            width = 6,
            bs4Card(
              title = "Bayesian Regression",
              status = "info",
              solidHeader = TRUE,
              selectInput("bayes_dep", "Dependent Variable", choices = NULL),
              selectInput("bayes_indep", "Independent Variables", choices = NULL, multiple = TRUE),
              actionButton("run_bayes", "Run Bayesian Regression"),
              actionButton("save_bayes", "Save Model"),
              verbatimTextOutput("bayes_summary")
            )
          ),
          column(
            width = 6,
            bs4Card(
              title = "Code Your Model",
              status = "warning",
              solidHeader = TRUE,
              # Pre-populated dropdowns for custom model card
              selectInput("custom_dep", "Dependent Variable", choices = NULL),
              selectInput("custom_indep", "Independent Variables", choices = NULL, multiple = TRUE),
              textAreaInput("custom_model_code", "Enter Custom Model Code", value = "", rows = 5, width = "100%"),
              # Tooltip with example code
              bsPopover("custom_model_code", 
                        title = "Example Code", 
                        content = "lm(custom_dep ~ ., data = df) \n# OR\nglm(custom_dep ~ ., data = df, family = binomial)",
                        placement = "right", trigger = "hover"),
              actionButton("run_custom_model", "Run Custom Model"),
              actionButton("save_custom_model", "Save Model"),
              verbatimTextOutput("custom_model_output")
            )
          )
        )
      ),
      
      # ---- OpenAI Chat Tab ----
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

server <- function(input, output, session) {
  
  ## Data & Transformation Reactive Values ##
  data_reactive <- reactiveVal(NULL)
  initial_df <- reactiveVal(NULL)
  previous_df <- reactiveVal(NULL)
  variable_types <- reactiveVal(NULL)
  
  ## Saved Models Reactive Values ##
  saved_models <- reactiveValues(lr = NULL, logr = NULL, bayes = NULL, custom = NULL)
  
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
        # Place the data table (with scrolling)
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
  
  #### Modeling Methods: Card View ####
  # Update model UI select inputs when data changes
  observe({
    df <- data_reactive()
    req(df)
    num_vars <- names(df)[sapply(df, is.numeric)]
    char_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    
    updateSelectInput(session, "lr_dep", choices = names(df))
    updateSelectInput(session, "lr_indep", choices = names(df))
    
    updateSelectInput(session, "logr_dep", choices = names(df))
    updateSelectInput(session, "logr_indep", choices = names(df))
    
    updateSelectInput(session, "bayes_dep", choices = names(df))
    updateSelectInput(session, "bayes_indep", choices = names(df))
  })
  
  # Reactive values to store saved models
  saved_models <- reactiveValues(lr = NULL, logr = NULL, bayes = NULL, custom = NULL)
  
  # Linear Regression
  observeEvent(input$run_lr, {
    req(data_reactive())
    df <- data_reactive()
    req(input$lr_dep, input$lr_indep)
    formula_lr <- as.formula(paste(input$lr_dep, "~", paste(input$lr_indep, collapse = "+")))
    fit_lr <- tryCatch({
      lm(formula_lr, data = df)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    output$lr_summary <- renderPrint({ summary(fit_lr) })
  })
  
  observeEvent(input$save_lr, {
    req(data_reactive())
    df <- data_reactive()
    req(input$lr_dep, input$lr_indep)
    formula_lr <- as.formula(paste(input$lr_dep, "~", paste(input$lr_indep, collapse = "+")))
    fit_lr <- tryCatch({
      lm(formula_lr, data = df)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    saved_models$lr <- fit_lr
    showNotification("Linear model saved.", type = "message")
  })
  
  # Logistic Regression
  observeEvent(input$run_logr, {
    req(data_reactive())
    df <- data_reactive()
    req(input$logr_dep, input$logr_indep)
    formula_logr <- as.formula(paste(input$logr_dep, "~", paste(input$logr_indep, collapse = "+")))
    fit_logr <- tryCatch({
      glm(formula_logr, data = df, family = binomial)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    output$logr_summary <- renderPrint({ summary(fit_logr) })
  })
  
  observeEvent(input$save_logr, {
    req(data_reactive())
    df <- data_reactive()
    req(input$logr_dep, input$logr_indep)
    formula_logr <- as.formula(paste(input$logr_dep, "~", paste(input$logr_indep, collapse = "+")))
    fit_logr <- tryCatch({
      glm(formula_logr, data = df, family = binomial)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    saved_models$logr <- fit_logr
    showNotification("Logistic model saved.", type = "message")
  })
  
  # Bayesian Regression (simplified demo using rstan)
  observeEvent(input$run_bayes, {
    req(data_reactive())
    df <- data_reactive()
    req(input$bayes_dep, input$bayes_indep)
    bayes_model_code <- "
data {
  int<lower=0> N;
  vector[N] y;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  y ~ normal(mu, sigma);
}
"
y <- df[[input$bayes_dep]]
stan_data <- list(N = length(y), y = y)
fit_bayes <- tryCatch({
  rstan::stan(model_code = bayes_model_code, data = stan_data, iter = 2000, chains = 4, refresh = 0)
}, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
output$bayes_summary <- renderPrint({ fit_bayes })
  })
  
  observeEvent(input$save_bayes, {
    req(data_reactive())
    df <- data_reactive()
    req(input$bayes_dep, input$bayes_indep)
    bayes_model_code <- "
data {
  int<lower=0> N;
  vector[N] y;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  y ~ normal(mu, sigma);
}
"
y <- df[[input$bayes_dep]]
stan_data <- list(N = length(y), y = y)
fit_bayes <- tryCatch({
  rstan::stan(model_code = bayes_model_code, data = stan_data, iter = 2000, chains = 4, refresh = 0)
}, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
saved_models$bayes <- fit_bayes
showNotification("Bayesian model saved.", type = "message")
  })
  
  # Custom R Code for Modeling
  observeEvent(input$run_custom_model, {
    req(data_reactive())
    df <- data_reactive()
    tryCatch({
      model_result <- eval(parse(text = input$custom_model_code), envir = list(df = df))
      output$custom_model_output <- renderPrint({ model_result })
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      output$custom_model_output <- renderPrint({ e$message })
    })
  })
  
  observeEvent(input$save_custom_model, {
    req(data_reactive())
    df <- data_reactive()
    tryCatch({
      model_result <- eval(parse(text = input$custom_model_code), envir = list(df = df))
      saved_models$custom <- model_result
      showNotification("Custom model saved.", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
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
    
    # Retrieve API key from environment
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

shinyApp(ui, server)
