# app.R
library(shiny)
library(bs4Dash)
library(shinyjs)
library(DT)
library(sortable)
library(httr)
library(jsonlite)
library(rpivotTable)
library(moments)
library(plotly)
library(randomForest)
library(rpart)
library(pls)
library(e1071)
library(forecast)

# Source modular files
source("modules/data_exploration.R")
source("modules/modelling_ui.R")
source("modules/openai_chat.R")

# Define UI
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
      # Data Transformation Tab (with log below the data table)
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
              br(),br(),
              column(12,
                     br(),
                     DTOutput("data_table"),
                     br(),
                     actionButton("revert_initial", "Revert to Initial Data"),
                     #actionButton("revert_previous", "Revert to Previous Data"),
                     #br(), br(),
                     h4("Transformation Log"),
                     verbatimTextOutput("transformation_log"),
                     downloadButton("download_log", "Download Log")
              )
            )
          )
        )
      ),
      # Data Exploration Tab using module
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
      # Modeling Methods Tab using module
      modelingMethodsUI("modeling"),
      # OpenAI Chat Tab using module
      bs4TabItem(
        tabName = "openai_chat",
        fluidRow(
          openaiChatUI("openai")
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Global reactive values for dataset and variable types
  data_reactive <- reactiveVal(NULL)
  initial_df <- reactiveVal(NULL)
  variable_types <- reactiveVal(NULL)
  # Reactive value to store previous state before a transformation is applied
  previous_df <- reactiveVal(NULL)
  
  # Create a reactive value to store transformation log
  transformation_log <- reactiveVal("")
  
  # Helper function to append a log message with timestamp
  log_message <- function(msg) {
    current <- transformation_log()
    time_stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    new_log <- paste0("[", time_stamp, "] ", msg)
    transformation_log(paste(current, new_log, sep = "\n"))
  }
  
  # Data Loading
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
      log_message(paste("File loaded:", input$file$name))
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
    log_message(paste("Cell edited at row", info$row, "column", colName, "new value:", info$value))
  })
  
  observeEvent(input$show_code, {
    req(data_reactive())
    df <- data_reactive()
    previous_df(df)
    datasetName <- if (!is.null(input$file)) input$file$name else "Dataset"
    schema_text <- paste(capture.output(str(df)), collapse = "\n")
    showModal(modalDialog(
      title = "Transformation Editor",
      tagList(
        h4(paste("Dataset:", datasetName)),
        div(style = "max-height:500px; overflow-y:auto;", DTOutput("data_table")),
        pre(schema_text),
        p("Note: The dataset is stored as 'df'."),
        textAreaInput("transformation_code", "Enter Transformation Code", value = "", rows = 5, width = "100%")
      ),
      footer = tagList(
        actionButton("run_transformation_modal", "Run Transformation"),
        actionButton("revert_initial", "Revert to Initial"),
        #actionButton("revert_previous", "Revert to Previous Data"),
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
        log_message(paste("Transformation applied:", input$transformation_code))
        showNotification("Transformation applied.", type = "message")
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  observeEvent(input$revert_initial, {
    req(initial_df())
    data_reactive(initial_df())
    log_message("Reverted to initial state.")
    showNotification("Reverted to initial state.", type = "message")
  })
  observeEvent(input$revert_previous, {
    req(previous_df())
    data_reactive(previous_df())
    log_message("Reverted to previous state.")
    showNotification("Reverted to previous state.", type = "message")
  })
  # Variable Type Setting (using drag-and-drop via sortable)
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
    log_message("Variable types updated.")
    showNotification("Variable types updated.", type = "message")
  })
  
  # Reactive value to store saved models
  saved_models <- reactiveValues()
  
  # Call our modules (if you want to log events from modules, you could modify those modules to call log_message as needed)
  dataExplorationServer("exploration", dataset = data_reactive, varTypes = variable_types)
  modelingMethodsServer("modeling", dataset = data_reactive, saved_models = saved_models)
  openaiChatServer("openai")
  
  # Render the transformation log in the UI
  output$transformation_log <- renderText({
    transformation_log()
  })
  
  # Download handler to export the log as a text file
  output$download_log <- downloadHandler(
    filename = function() {
      paste("transformation_log_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      writeLines(transformation_log(), con = file)
    }
  )
}

shinyApp(ui = ui, server = server)
