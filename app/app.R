# app/app.R
library(shiny)
library(bslib)
library(shinyjs)
library(DT)

# Define a custom modern theme using bslib
my_theme <- bs_theme(
  bootswatch = "flatly",           # A modern Bootswatch theme
  primary = "#2C3E50",             # Custom primary color
  secondary = "#18BC9C",           # Custom secondary color
  base_font = font_google("Roboto")# Use a modern Google font
)

ui <- navbarPage(
  title = "Modern Bayesian Tool",
  id = "navbar",
  theme = my_theme,
  tabPanel("Data Transformation",
           sidebarLayout(
             sidebarPanel(
               fileInput("file", "Upload CSV Data", accept = ".csv"),
               actionButton("show_code", "Show Transformation Editor"),
               shinyjs::hidden(
                 textAreaInput("code_editor", "Enter R Code", "", rows = 5, width = "100%")
               ),
               actionButton("run_transformation", "Run Transformation"),
               br(), br(),
               actionButton("go_modeling", "Go to Modeling Methods")
             ),
             mainPanel(
               DTOutput("data_table")
             )
           )
  ),
  tabPanel("Modeling Methods",
           fluidRow(
             column(4,
                    div(class = "card",
                        div(class = "card-body",
                            h4(class = "card-title", "Linear Regression"),
                            p("Perform linear regression analysis."),
                            actionButton("linear_reg", "Select")
                        )
                    )
             ),
             column(4,
                    div(class = "card",
                        div(class = "card-body",
                            h4(class = "card-title", "Logistic Regression"),
                            p("Perform logistic regression analysis."),
                            actionButton("logistic_reg", "Select")
                        )
                    )
             ),
             column(4,
                    div(class = "card",
                        div(class = "card-body",
                            h4(class = "card-title", "Bayesian Regression"),
                            p("Perform Bayesian regression analysis."),
                            actionButton("bayesian_reg", "Select")
                        )
                    )
             )
           )
  )
)

server <- function(input, output, session) {
  # Enable shinyjs functions
  shinyjs::useShinyjs()
  
  # Reactive value to store uploaded/transformed data
  data_reactive <- reactiveVal(NULL)
  
  # When a file is uploaded, read and store the CSV data
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
  })
  
  # Render an editable data table using DT
  output$data_table <- renderDT({
    req(data_reactive())
    datatable(data_reactive(), editable = TRUE, options = list(pageLength = 5))
  })
  
  # Capture cell edits and update the reactive data
  observeEvent(input$data_table_cell_edit, {
    info <- input$data_table_cell_edit
    df <- data_reactive()
    df[info$row, info$col] <- info$value
    data_reactive(df)
  })
  
  # Toggle (show/hide) the code editor when the button is clicked
  observeEvent(input$show_code, {
    shinyjs::toggle("code_editor")
  })
  
  # When "Run Transformation" is clicked, evaluate the R code from the editor
  observeEvent(input$run_transformation, {
    req(data_reactive())
    code <- input$code_editor
    df <- data_reactive()
    tryCatch({
      # Evaluate the code in an environment where 'df' is available
      df_transformed <- eval(parse(text = code), envir = list(df = df))
      # If the transformation returns a value, update the data
      if (!is.null(df_transformed)) {
        data_reactive(df_transformed)
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Navigate to the Modeling Methods tab when the button is clicked
  observeEvent(input$go_modeling, {
    updateNavbarPage(session, "navbar", selected = "Modeling Methods")
  })
  
  # Sample handlers for modeling method buttons (add your modeling logic here)
  observeEvent(input$linear_reg, {
    showNotification("Linear Regression selected.")
  })
  observeEvent(input$logistic_reg, {
    showNotification("Logistic Regression selected.")
  })
  observeEvent(input$bayesian_reg, {
    showNotification("Bayesian Regression selected.")
  })
}

shinyApp(ui, server)
