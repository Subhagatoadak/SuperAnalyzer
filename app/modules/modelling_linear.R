# app/modules/modelling_linear.R
library(shiny)
library(DT)

# UI for Linear Regression
linearModelUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("dep"), "Select Dependent Variable", choices = NULL),
    selectInput(ns("indep"), "Select Independent Variable(s)", choices = NULL, multiple = TRUE),
    actionButton(ns("run_linear"), "Run Linear Regression"),
    verbatimTextOutput(ns("lm_summary")),
    DTOutput(ns("lm_table"))
  )
}

# Server for Linear Regression
linearModelServer <- function(id, dataset) {
  moduleServer(
    id,
    function(input, output, session) {
      # Update variable choices when data is available
      observe({
        df <- dataset()
        if (!is.null(df)) {
          numeric_vars <- names(df)[sapply(df, is.numeric)]
          updateSelectInput(session, "dep", choices = numeric_vars)
          updateSelectInput(session, "indep", choices = numeric_vars)
        }
      })
      
      lm_fit <- eventReactive(input$run_linear, {
        df <- dataset()
        req(df, input$dep, input$indep)
        formula <- as.formula(paste(input$dep, "~", paste(input$indep, collapse = "+")))
        lm(formula, data = df)
      })
      
      output$lm_summary <- renderPrint({
        fit <- lm_fit()
        req(fit)
        summary(fit)
      })
      
      output$lm_table <- renderDT({
        fit <- lm_fit()
        req(fit)
        datatable(as.data.frame(coef(summary(fit))))
      })
    }
  )
}
