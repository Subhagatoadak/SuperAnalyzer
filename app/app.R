library(shiny)
library(bs4Dash)
library(shinyjs)
library(DT)
library(sortable)      # For drag-and-drop interface
library(httr)          # For HTTP requests
library(jsonlite)      # For JSON handling

# Source the modules
source("modules/data_exploration.R")
source("modules/modelling_linear.R")
source("modules/modelling_logistic.R")
source("modules/modelling_bayesian.R")

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
      bs4TabItem(
        tabName = "modeling_methods",
        fluidRow(
          bs4Card(
            title = "Modeling Methods",
            status = "info",
            width = 12,
            solidHeader = TRUE,
            tabsetPanel(
              tabPanel("Linear Regression", linearModelUI("linear")),
              tabPanel("Logistic Regression", logisticModelUI("logistic")),
              tabPanel("Bayesian Regression", bayesianModelUI("bayesian")),
              tabPanel("Custom R Code",
                       textAreaInput("custom_code", "R Code", "", rows = 5, width = "100%"),
                       actionButton("run_custom_code", "Run Code"),
                       br(), br(),
                       verbatimTextOutput("custom_code_output")
              )
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
                        choices = c("gpt-4o", "o3-mini-2025-01-31", "gpt-3.5-turbo-16k", "gpt-4o-mini","chatgpt-4o-latest"),
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
  # Reactive value to store the current data
  data_reactive <- reactiveVal(NULL)
  # Reactive value to store the initial state (once a file is uploaded)
  initial_df <- reactiveVal(NULL)
  # Reactive value to store the previous state (before a transformation)
  previous_df <- reactiveVal(NULL)
  
  # Reactive value to store user-specified variable types
  variable_types <- reactiveVal(NULL)
  
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
    # Determine the column name from the cell edit info
    colName <- names(df)[as.numeric(info$col)]
    # If the column is numeric, convert the new value to numeric.
    if(is.numeric(df[[colName]])) {
      newValue <- as.numeric(info$value)
    } else {
      newValue <- info$value
    }
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
   
        # Wrap the data table in a div that scrolls if it overflows
        div(style = "max-height:500px; overflow-y:auto;",
            DTOutput("data_table")
        )
        ,
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
      if (!is.null(cont_vars) && var %in% cont_vars) {
        "Continuous"
      } else if (!is.null(disc_vars) && var %in% disc_vars) {
        "Discrete"
      } else {
        if (is.numeric(df[[var]])) "Continuous" else "Discrete"
      }
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
  
  #### Call Module Server Functions ####
  dataExplorationServer("exploration", dataset = data_reactive, varTypes = variable_types)
  linearModelServer("linear", dataset = data_reactive)
  logisticModelServer("logistic", dataset = data_reactive)
  bayesianModelServer("bayesian", dataset = data_reactive)
  
  #### Custom R Code Execution ####
  observeEvent(input$run_custom_code, {
    code <- input$custom_code
    result <- tryCatch({
      capture.output(eval(parse(text = code)))
    }, error = function(e) {
      paste("Error:", e$message)
    })
    output$custom_code_output <- renderText({
      if (length(result) == 0) {
        "No output returned."
      } else {
        paste(result, collapse = "\n")
      }
    })
  })
  

  #### OpenAI Integration ####
  # Create a reactiveValues object to store chat history
  rv <- reactiveValues(messages = list())
  
  # Render chat history as styled HTML chat bubbles
  output$openai_output <- renderUI({
    req(rv$messages)
    message_tags <- lapply(rv$messages, function(msg) {
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
    # Wrap messages in a flex container to mimic chat layout
    tags$div(style = "display: flex; flex-direction: column;", message_tags)
  })
  
  observeEvent(input$openai_send, {
    req(input$openai_input,input$openai_model)
    query <- input$openai_input
    model_choice <- input$openai_model
    showNotification("Sending query to OpenAI...", type = "message")
    
    #api_key <- Sys.getenv("OPENAI_API_KEY")
    api_key<- Sys.getenv("OPENAI_API_KEY")

    if(api_key == "") {
      showNotification("OPENAI_API_KEY not set", type = "error")
      return(NULL)
    }
    
    # Send the POST request
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
    
    # Check for HTTP errors
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
   
    rv$messages <- c(rv$messages, list(list(role = "assistant", text = answer)))
    showNotification("Response received from OpenAI.", type = "message")
  }) 
}



shinyApp(ui, server)
