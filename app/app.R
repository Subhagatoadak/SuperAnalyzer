library(shiny)
library(bs4Dash)
library(shinyjs)
library(DT)
library(sortable)  # For drag-and-drop interface

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
                width = 4,
                fileInput("file", "Upload CSV Data", accept = ".csv"),
                actionButton("show_code", "Show Transformation Editor"),
                shinyjs::hidden(
                  textAreaInput("code_editor", "Enter R Code", "", rows = 5, width = "100%")
                ),
                actionButton("run_transformation", "Run Transformation"),
                br(), br(),
                actionButton("set_var_types", "Set Variable Types")
              ),
              column(
                width = 8,
                DTOutput("data_table")
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
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive value to store the uploaded/transformed data
  data_reactive <- reactiveVal(NULL)
  
  # Reactive value to store user-specified variable types
  # A named vector: names are variable names; values are "Continuous" or "Discrete"
  variable_types <- reactiveVal(NULL)
  
  # Load CSV file when uploaded
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
    # Initialize variable_types when data is loaded
    if (!is.null(df)) {
      default_types <- sapply(df, function(x) if (is.numeric(x)) "Continuous" else "Discrete")
      variable_types(default_types)
    }
  })
  
  # Render an editable data table in the Data Transformation tab
  output$data_table <- renderDT({
    req(data_reactive())
    datatable(data_reactive(), editable = TRUE, options = list(pageLength = 5))
  })
  
  # Update reactive data when a cell is edited
  observeEvent(input$data_table_cell_edit, {
    info <- input$data_table_cell_edit
    df <- data_reactive()
    df[info$row, info$col] <- info$value
    data_reactive(df)
  })
  
  # Toggle the transformation editor
  observeEvent(input$show_code, {
    shinyjs::toggle("code_editor")
  })
  
  # Execute transformation code and update the data
  observeEvent(input$run_transformation, {
    req(data_reactive())
    code <- input$code_editor
    df <- data_reactive()
    tryCatch({
      df_transformed <- eval(parse(text = code), envir = list(df = df))
      if (!is.null(df_transformed)) {
        data_reactive(df_transformed)
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Show modal dialog for drag-and-drop variable type selection
  observeEvent(input$set_var_types, {
    req(data_reactive())
    df <- data_reactive()
    var_names <- names(df)
    # Get current variable types or set defaults
    current_types <- variable_types()
    if (is.null(current_types)) {
      current_types <- sapply(df, function(x) if (is.numeric(x)) "Continuous" else "Discrete")
    }
    # Determine initial buckets
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
  
  # When the modal Save button is pressed, update variable_types
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
  
  # Navigation: Go to Data Exploration tab
  observeEvent(input$go_exploration, {
    updateTabItems(session, "navbar", "data_exploration")
  })
  
  # Navigation: Go to Modeling Methods tab
  observeEvent(input$go_modeling, {
    updateTabItems(session, "navbar", "modeling_methods")
  })
  
  # Call module server functions; pass variable_types to data exploration
  dataExplorationServer("exploration", dataset = data_reactive, varTypes = variable_types)
  linearModelServer("linear", dataset = data_reactive)
  logisticModelServer("logistic", dataset = data_reactive)
  bayesianModelServer("bayesian", dataset = data_reactive)
  
  # Execute custom R code from the Modeling Methods tab
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
}

shinyApp(ui, server)
