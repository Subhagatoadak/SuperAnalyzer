# app/modules/modelling_logistic.R
library(shiny)
library(DT)

# UI for Logistic Regression
logisticModelUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("dep"), "Select Dependent Variable (binary)", choices = NULL),
    selectInput(ns("indep"), "Select Independent Variable(s)", choices = NULL, multiple = TRUE),
    actionButton(ns("run_logistic"), "Run Logistic Regression"),
    verbatimTextOutput(ns("logistic_summary")),
    DTOutput(ns("logistic_table"))
  )
}

# Server for Logistic Regression
logisticModelServer <- function(id, dataset) {
  moduleServer(
    id,
    function(input, output, session) {
      observe({
        df <- dataset()
        if (!is.null(df)) {
          # For dependent variable, try to use numeric variables that may represent binary data
          binary_vars <- names(df)[sapply(df, function(x) all(x %in% c(0,1)))]
          numeric_vars <- names(df)[sapply(df, is.numeric)]
          updateSelectInput(session, "dep", choices = unique(c(binary_vars, numeric_vars)))
          updateSelectInput(session, "indep", choices = names(df))
        }
      })
      
      logistic_fit <- eventReactive(input$run_logistic, {
        df <- dataset()
        req(df, input$dep, input$indep)
        df[[input$dep]] <- as.factor(df[[input$dep]])
        formula <- as.formula(paste(input$dep, "~", paste(input$indep, collapse = "+")))
        glm(formula, data = df, family = binomial)
      })
      
      output$logistic_summary <- renderPrint({
        fit <- logistic_fit()
        req(fit)
        summary(fit)
      })
      
      output$logistic_table <- renderDT({
        fit <- logistic_fit()
        req(fit)
        datatable(as.data.frame(coef(summary(fit))))
      })
    }
  )
}
