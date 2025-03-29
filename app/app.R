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

# Source the data exploration module (assumed implemented as before)
source("modules/data_exploration.R")

# ---- UI ----
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
      bs4SidebarMenuItem("Data Transformation", tabName = "data_transform", icon = icon("table")),
      bs4SidebarMenuItem("Data Exploration", tabName = "data_exploration", icon = icon("search")),
      bs4SidebarMenuItem("Modeling Methods", tabName = "modeling_methods", icon = icon("chart-line")),
      bs4SidebarMenuItem("OpenAI Chat", tabName = "openai_chat", icon = icon("robot"))
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
      
      # ---- Modeling Methods Tab (Card View) ----
      bs4TabItem(
        tabName = "modeling_methods",
        fluidRow(
          # Example: Linear Regression Card
          column(
            width = 6,
            bs4Card(
              title = "Linear Regression",
              status = "info",
              solidHeader = TRUE,
              footer = tagList(
                actionButton("lr_configure", "Configure & Run"),
                actionButton("lr_export", "Export Model")
              ),
              verbatimTextOutput("lr_output")
            )
          ),
          # Example: Custom Model Card (Quick Code)
          column(
            width = 6,
            bs4Card(
              title = "Code Your Model",
              status = "warning",
              solidHeader = TRUE,
              footer = tagList(
                actionButton("custom_configure", "Configure & Run"),
                actionButton("custom_export", "Export Model")
              ),
              verbatimTextOutput("custom_output")
            )
          )
        ),
        fluidRow(
          # Dynamic UI output for user-created model cards
          uiOutput("custom_model_cards_ui"),
          actionButton("add_new_model_card", "Create New Model Card")
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

# ---- Server ----
server <- function(input, output, session) {
  
  ## Data & Transformation Reactive Values ##
  data_reactive <- reactiveVal(NULL)
  initial_df <- reactiveVal(NULL)
  previous_df <- reactiveVal(NULL)
  variable_types <- reactiveVal(NULL)
  
  ## Saved Models ##
  saved_models <- reactiveValues()  # To store model objects
  
  ## Dynamic Custom Model Cards ##
  custom_cards <- reactiveVal(list())
  
  #### Data Loading and Transformation ####
  observeEvent(input$file, {
    req(input$file)
    df <- tryCatch({
      read.csv(input$file$datapath, stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification("Error reading file", type = "error")
      NULL
    })
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
    newValue <- if (is.numeric(df[[colName]])) as.numeric(info$value) else info$value
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
  
  #### Call Data Exploration Module ####
  dataExplorationServer("exploration", dataset = data_reactive, varTypes = variable_types)
  
  #### Modeling Methods: Card-Based UI ####
  
  # Predefined model cards (for demonstration, we'll implement Linear Regression and add placeholders for others)
  output$modelCards <- renderUI({
    tagList(
      fluidRow(
        column(
          width = 6,
          bs4Card(
            title = "Linear Regression",
            status = "info",
            solidHeader = TRUE,
            footer = tagList(
              actionButton("lr_configure", "Configure & Run"),
              actionButton("lr_export", "Export Model")
            ),
            verbatimTextOutput("lr_output")
          )
        ),
        column(
          width = 6,
          bs4Card(
            title = "Logistic Regression",
            status = "info",
            solidHeader = TRUE,
            footer = tagList(
              actionButton("logr_configure", "Configure & Run"),
              actionButton("logr_export", "Export Model")
            ),
            verbatimTextOutput("logr_output")
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          bs4Card(
            title = "Bayesian Regression",
            status = "info",
            solidHeader = TRUE,
            footer = tagList(
              actionButton("bayes_configure", "Configure & Run"),
              actionButton("bayes_export", "Export Model")
            ),
            verbatimTextOutput("bayes_output")
          )
        ),
        column(
          width = 6,
          bs4Card(
            title = "Code Your Model",
            status = "warning",
            solidHeader = TRUE,
            footer = tagList(
              actionButton("custom_configure", "Configure & Run"),
              actionButton("custom_export", "Export Model")
            ),
            verbatimTextOutput("custom_output")
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          bs4Card(
            title = "Create New Model Card",
            status = "primary",
            solidHeader = TRUE,
            actionButton("add_new_model_card", "Create New Model Card")
          )
        )
      ),
      # UI for dynamically created custom model cards
      uiOutput("custom_model_cards_ui")
    )
  })
  
  # Render the Modeling Methods tab using modelCards output
  observe({
    updateTabItems(session, "navbar", "modeling_methods")
  })
  
  #### Modal for Predefined Model Cards ####
  # Example: Linear Regression Configuration
  observeEvent(input$lr_configure, {
    req(data_reactive())
    df <- data_reactive()
    showModal(modalDialog(
      title = "Configure Linear Regression",
      tagList(
        selectInput("lr_dep", "Dependent Variable", choices = names(df)),
        selectInput("lr_indep", "Independent Variables", choices = names(df), multiple = TRUE)
      ),
      footer = tagList(
        actionButton("run_lr", "Run Model"),
        modalButton("Close")
      ),
      size = "m"
    ))
  })
  
  observeEvent(input$run_lr, {
    req(data_reactive(), input$lr_dep, input$lr_indep)
    df <- data_reactive()
    formula_lr <- as.formula(paste(input$lr_dep, "~", paste(input$lr_indep, collapse = "+")))
    fit_lr <- tryCatch({
      lm(formula_lr, data = df)
    }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
    output$lr_output <- renderPrint({ summary(fit_lr) })
    saved_models$lr <- fit_lr
    removeModal()
    showNotification("Linear Regression model executed.", type = "message")
  })
  
  # (Similar modals and observers would be created for Logistic, Bayesian, etc.)
  # For demonstration, we add a placeholder for the custom model configuration:
  
  observeEvent(input$custom_configure, {
    req(data_reactive())
    df <- data_reactive()
    showModal(modalDialog(
      title = "Configure Custom Model",
      tagList(
        selectInput("custom_dep", "Dependent Variable", choices = names(df)),
        selectInput("custom_indep", "Independent Variables", choices = names(df), multiple = TRUE),
        textAreaInput("custom_model_code", "Enter Model Code", value = "", rows = 5, width = "100%"),
        p("Example: lm(custom_dep ~ ., data = df)"),
        p("Note: 'df' is your dataset, and the chosen variables are available as input values.")
      ),
      footer = tagList(
        actionButton("run_custom_model", "Run Model"),
        modalButton("Close")
      ),
      size = "l"
    ))
  })
  
  observeEvent(input$run_custom_model, {
    req(data_reactive(), input$custom_dep, input$custom_indep)
    df <- data_reactive()
    custom_env <- list(df = df, dep = input$custom_dep, indep = input$custom_indep)
    tryCatch({
      result <- eval(parse(text = input$custom_model_code), envir = custom_env)
      output$custom_output <- renderPrint({ result })
      saved_models$custom <- result
      removeModal()
      showNotification("Custom model executed.", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      output$custom_output <- renderPrint({ e$message })
    })
  })
  
  #### Modal for Creating New Custom Model Cards ####
  observeEvent(input$add_new_model_card, {
    showModal(modalDialog(
      title = "Create New Model Card",
      tagList(
        textInput("new_model_name", "Model Name", value = ""),
        selectInput("new_model_dep", "Dependent Variable", choices = names(data_reactive())),
        selectInput("new_model_indep", "Independent Variables", choices = names(data_reactive()), multiple = TRUE),
        textAreaInput("new_model_code", "Enter Model Code", value = "", rows = 5, width = "100%"),
        p("Example: lm(new_model_dep ~ new_model_indep, data = df)")
      ),
      footer = tagList(
        actionButton("create_new_model_card", "Create Card"),
        modalButton("Close")
      ),
      size = "l"
    ))
  })
  
  observeEvent(input$create_new_model_card, {
    req(input$new_model_name, input$new_model_dep, input$new_model_code)
    new_card <- tags$div(
      bs4Card(
        title = input$new_model_name,
        status = "secondary",
        solidHeader = TRUE,
        footer = tagList(
          actionButton(paste0("new_configure_", input$new_model_name), "Configure & Run"),
          actionButton(paste0("new_export_", input$new_model_name), "Export Model")
        ),
        verbatimTextOutput(paste0("new_output_", input$new_model_name))
      ),
      br()
    )
    # Append the new card to our dynamic UI list
    current_cards <- custom_cards()
    custom_cards(c(current_cards, list(new_card)))
    removeModal()
  })
  
  # Render dynamic custom model cards
  output$custom_model_cards_ui <- renderUI({
    custom_cards()
  })
  
  # (For each dynamically created custom model card, you would set up observers dynamically using, for example, 
  # callModule or a loop using local() to capture the input IDs. This example leaves that as an exercise for extension.)
  
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

shinyApp(ui, server)
