# app.R

# —— Libraries —— 
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
library(shinyAce)

# —— Source your modules —— 
source("modules/data_exploration.R")
source("modules/modelling_ui.R")
source("modules/openai_chat.R")

# —— UI Definition —— 
ui <- bs4DashPage(
  title   = "SuperAnalyzer",
  header  = bs4DashNavbar(title = "SuperAnalyzer", skin = "light"),
  sidebar = bs4DashSidebar(
    skin       = "light",
    status     = "primary",
    brandColor = "primary",
    bs4SidebarMenu(
      id = "sidebarMenu",
      bs4SidebarMenuItem("Data Transformation", tabName = "data_transform", icon = icon("table")),
      bs4SidebarMenuItem("Data Exploration",   tabName = "data_exploration", icon = icon("search")),
      bs4SidebarMenuItem("Modeling Methods",   tabName = "modeling_methods",  icon = icon("chart-line")),
      bs4SidebarMenuItem("OpenAI Chat",        tabName = "openai_chat",       icon = icon("robot"))
    )
  ),
  body = bs4DashBody(
    useShinyjs(),
    bs4TabItems(
      # — Data Transformation —
      bs4TabItem(
        tabName = "data_transform",
        fluidRow(
          bs4Card(
            title = "Upload & Transform Data",
            status = "primary",
            width  = 12,
            solidHeader = TRUE,
            fluidRow(
              # Controls
              column(
                width = 4,
                fileInput("file",          "Upload CSV",            accept = ".csv"),
                actionButton("show_code",     "Edit via Code",       icon = icon("code")),
                actionButton("set_var_types", "Set Variable Types",  icon = icon("tags")),
                br(), br(),
                actionButton("revert_initial",  "Revert to Initial",  icon = icon("undo-alt")),
                actionButton("revert_previous", "Revert Previous",    icon = icon("history"))
              ),
              # Data table
              column(
                width = 8,
                DTOutput("data_table")
              )
            ),
            hr(),
            fluidRow(
              column(
                width = 12,
                h4("Transformation Log"),
                verbatimTextOutput("transformation_log"),
                downloadButton("download_log", "Download Log")
              )
            )
          )
        )
      ),
      
      # — Data Exploration —
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
      
      # — Modeling Methods —
      modelingMethodsUI("modeling"),
      
      # — OpenAI Chat —
      bs4TabItem(
        tabName = "openai_chat",
        fluidRow(
          openaiChatUI("openai")
        )
      )
    )
  )
)

# —— Server Definition —— 
server <- function(input, output, session) {
  # central state
  rv <- reactiveValues(
    data           = NULL,   # current DF
    initial_data   = NULL,   # as uploaded
    history         = list(),# for undo
    variable_types = NULL,   # named list
    log            = ""      # text log
  )
  
  # — Log helper —
  log_message <- function(msg) {
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    rv$log <- paste0("[", ts, "] ", msg, "\n", rv$log)
  }
  
  # — Push to history (before any change) —
  add_history <- function() {
    req(rv$data)
    rv$history <- c(rv$history, list(rv$data))
    shinyjs::enable("revert_previous")
  }
  
  # — Update variable_types if columns change —
  update_var_types <- function(df) {
    old <- isolate(rv$variable_types)
    new <- setNames(
      lapply(df, function(col) if (is.numeric(col)) "Continuous" else "Discrete"),
      names(df)
    )
    if (!identical(old, new)) rv$variable_types <- new
  }
  
  # —— 1) Load CSV —— 
  observeEvent(input$file, {
    req(input$file)
    df <- tryCatch(
      read.csv(input$file$datapath, stringsAsFactors = FALSE),
      error = function(e) { showNotification(e$message, type = "error"); NULL }
    )
    req(df)
    rv$data         <- df
    rv$initial_data <- df
    rv$history      <- list()
    rv$variable_types <- setNames(
      lapply(df, function(col) if (is.numeric(col)) "Continuous" else "Discrete"),
      names(df)
    )
    log_message(paste("Loaded file:", input$file$name))
    shinyjs::disable(c("revert_initial", "revert_previous"))
  })
  
  # —— 2) Render table —— 
  output$data_table <- renderDT({
    req(rv$data)
    datatable(
      rv$data,
      editable = "cell",
      options  = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # —— 3) Cell edits —— 
  observeEvent(input$data_table_cell_edit, {
    info <- input$data_table_cell_edit
    add_history()
    df <- isolate(rv$data)
    row <- info$row + 1
    col <- info$col + 1
    name <- names(df)[col]
    old  <- df[row, col]
    new  <- if (is.numeric(old)) as.numeric(info$value) else as.character(info$value)
    df[row, col] <- new
    rv$data <- df
    log_message(sprintf("Cell [%d, %s]: '%s' → '%s'", row, name, old, info$value))
    shinyjs::enable("revert_initial")
  })
  
  # —— 4) Code editor modal —— 
  observeEvent(input$show_code, {
    req(rv$data)
    add_history()
    schema <- paste(capture.output(str(rv$data)), collapse = "\n")
    showModal(modalDialog(
      title = "Transformation Editor",
      shinyAce::aceEditor(
        "code_editor",
        value = "# e.g.\n# df <- dplyr::filter(df, col > 10)\n# return(df)\ndf",
        mode  = "r", theme = "chrome", height = "200px"
      ),
      br(),
      h5("Current schema:"),
      pre(schema),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("apply_code", "Apply")
      ),
      size = "l", easyClose = FALSE
    ))
  })
  
  # —— 5) Apply code —— 
  observeEvent(input$apply_code, {
    req(rv$data, input$code_editor)
    df_old <- isolate(rv$data)
    code   <- input$code_editor
    
    # evaluate in private env with df
    env <- list2env(list(df = df_old), parent = globalenv())
    result <- tryCatch(
      eval(parse(text = code), envir = env),
      error = function(e) e
    )
    
    if (inherits(result, "error")) {
      showNotification(result$message, type = "error", duration = 10)
    } else if (!is.data.frame(result)) {
      showNotification("Code must return a data.frame", type = "error", duration = 10)
    } else {
      rv$data <- result
      update_var_types(result)
      log_message("Applied code transformation")
      removeModal()
      showNotification("Transformation applied", type = "message")
    }
  })
  
  # —— 6) Revert to initial —— 
  observeEvent(input$revert_initial, {
    req(rv$initial_data)
    rv$data      <- rv$initial_data
    rv$history   <- list()
    log_message("Reverted to initial upload")
    showNotification("Reverted to initial data", type = "message")
    shinyjs::disable(c("revert_initial", "revert_previous"))
  })
  
  # —— 7) Undo previous —— 
  observeEvent(input$revert_previous, {
    req(length(rv$history) > 0)
    last <- tail(rv$history, 1)[[1]]
    rv$history   <- head(rv$history, -1)
    rv$data      <- last
    update_var_types(last)
    log_message("Reverted one step back")
    showNotification("Reverted previous change", type = "message")
    if (length(rv$history) == 0) shinyjs::disable("revert_previous")
  })
  
  # —— 8) Set variable types —— 
  observeEvent(input$set_var_types, {
    req(rv$data)
    types <- rv$variable_types
    names <- names(rv$data)
    cont  <- names[types == "Continuous"]
    disc  <- names[types == "Discrete"]
    
    showModal(modalDialog(
      title = "Assign Variable Types",
      bucket_list(
        header      = "Drag variables into types",
        group_name  = "vt_grp",
        add_rank_list(text = "Continuous", labels = cont, input_id = "cont_vars"),
        add_rank_list(text = "Discrete",   labels = disc, input_id = "disc_vars")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_types", "Save")
      ),
      size = "l", easyClose = FALSE
    ))
  })
  
  # —— 9) Save variable types —— 
  observeEvent(input$save_types, {
    req(rv$data)
    names <- names(rv$data)
    cont  <- input$cont_vars %||% character(0)
    new   <- setNames(
      lapply(names, function(nm) if (nm %in% cont) "Continuous" else "Discrete"),
      names
    )
    rv$variable_types <- new
    log_message("Variable types updated")
    removeModal()
    showNotification("Variable types saved", type = "message")
  })
  
  # —— 10) Transformation log & download —— 
  output$transformation_log <- renderText({ rv$log })
  output$download_log <- downloadHandler(
    filename = function() paste0("log_", Sys.Date(), ".txt"),
    content  = function(f) writeLines(rv$log, con = f)
  )
  
  # —— 11) Modules —— 
  dataExplorationServer("exploration",
                        dataset  = reactive(rv$data),
                        varTypes = reactive(rv$variable_types))
  modelingMethodsServer("modeling",
                         dataset      = reactive(rv$data),
                         saved_models = reactiveValues())
  openaiChatServer("openai")
  
  # —— 12) Button enable/disable —— 
  observe({
    loaded <- !is.null(rv$data)
    shinyjs::toggleState("show_code",      loaded)
    shinyjs::toggleState("set_var_types",  loaded)
    shinyjs::toggleState("revert_initial", loaded)
    shinyjs::toggleState("revert_previous", length(rv$history) > 0)
    shinyjs::toggleState("download_log",   nzchar(rv$log))
  })
}

# —— Launch App —— 
shinyApp(ui = ui, server = server)

