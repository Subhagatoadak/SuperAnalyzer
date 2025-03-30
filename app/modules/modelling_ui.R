# modeling_ui.R
library(shiny)
library(bs4Dash)
library(randomForest)
library(rpart)
library(e1071)

# UI for Modeling Methods (example with three model cards)
modelingMethodsUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "modeling_methods",
    fluidRow(
      column(4,
             bs4Card(
               title = "Linear Regression",
               status = "info",
               solidHeader = TRUE,
               footer = actionButton(ns("open_lr"), "Open")
             )
      ),
      column(4,
             bs4Card(
               title = "Logistic Regression",
               status = "info",
               solidHeader = TRUE,
               footer = actionButton(ns("open_logr"), "Open")
             )
      ),
      column(4,
             bs4Card(
               title = "Random Forest",
               status = "info",
               solidHeader = TRUE,
               footer = actionButton(ns("open_rf"), "Open")
             )
      )
    )
    # Additional cards can be added as needed.
  )
}

# Server logic for Modeling Methods (example with three models)
modelingMethodsServer <- function(id, dataset, saved_models) {
  moduleServer(
    id,
    function(input, output, session) {
      # Linear Regression
      observeEvent(input$open_lr, {
        req(dataset())
        df <- dataset()
        showModal(modalDialog(
          title = "Linear Regression",
          tagList(
            selectInput(session$ns("lr_dep_var"), "Dependent Variable", choices = names(df)),
            selectInput(session$ns("lr_indep_vars"), "Independent Variables", choices = names(df), multiple = TRUE),
            textAreaInput(session$ns("lr_model_code"), "Model Code", 
                          value = "lm(dep ~ indep, data = df)", rows = 3, width = "100%"),
            p("Example: lm(Y ~ X1 + X2, data = df)"),
            p("Note: The dataset is stored as 'df'.")
          ),
          footer = tagList(
            actionButton(session$ns("run_lr"), "Run Model"),
            actionButton(session$ns("save_lr"), "Save Model"),
            modalButton("Close")
          ),
          size = "l"
        ))
      })
      
      observeEvent(input$run_lr, {
        req(dataset())
        df <- dataset()
        req(input$lr_dep_var, input$lr_indep_vars)
        formula_lr <- as.formula(paste(input$lr_dep_var, "~", paste(input$lr_indep_vars, collapse = "+")))
        model_lr <- tryCatch({
          lm(formula_lr, data = df)
        }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
        output$lr_model_output <- renderPrint({ summary(model_lr) })
      })
      
      observeEvent(input$save_lr, {
        req(dataset())
        df <- dataset()
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
        req(dataset())
        df <- dataset()
        showModal(modalDialog(
          title = "Logistic Regression",
          tagList(
            selectInput(session$ns("logr_dep_var"), "Dependent Variable", choices = names(df)),
            selectInput(session$ns("logr_indep_vars"), "Independent Variables", choices = names(df), multiple = TRUE),
            textAreaInput(session$ns("logr_model_code"), "Model Code", 
                          value = "glm(dep ~ indep, data = df, family = binomial)", rows = 3, width = "100%"),
            p("Example: glm(Y ~ X1 + X2, data = df, family = binomial)"),
            p("Note: The dataset is stored as 'df'.")
          ),
          footer = tagList(
            actionButton(session$ns("run_logr"), "Run Model"),
            actionButton(session$ns("save_logr"), "Save Model"),
            modalButton("Close")
          ),
          size = "l"
        ))
      })
      
      observeEvent(input$run_logr, {
        req(dataset())
        df <- dataset()
        req(input$logr_dep_var, input$logr_indep_vars)
        formula_logr <- as.formula(paste(input$logr_dep_var, "~", paste(input$logr_indep_vars, collapse = "+")))
        model_logr <- tryCatch({
          glm(formula_logr, data = df, family = binomial)
        }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
        output$logr_model_output <- renderPrint({ summary(model_logr) })
      })
      
      observeEvent(input$save_logr, {
        req(dataset())
        df <- dataset()
        req(input$logr_dep_var, input$logr_indep_vars)
        formula_logr <- as.formula(paste(input$logr_dep_var, "~", paste(input$logr_indep_vars, collapse = "+")))
        model_logr <- tryCatch({
          glm(formula_logr, data = df, family = binomial)
        }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
        saved_models$logr <- model_logr
        showNotification("Logistic model saved.", type = "message")
      })
      
      # Random Forest
      observeEvent(input$open_rf, {
        req(dataset())
        df <- dataset()
        showModal(modalDialog(
          title = "Random Forest",
          tagList(
            selectInput(session$ns("rf_dep_var"), "Dependent Variable", choices = names(df)),
            selectInput(session$ns("rf_indep_vars"), "Independent Variables", choices = names(df), multiple = TRUE),
            textAreaInput(session$ns("rf_model_code"), "Model Code", 
                          value = "randomForest(dep ~ ., data = df)", rows = 3, width = "100%"),
            p("Example: randomForest(Y ~ X1 + X2, data = df)"),
            p("Note: The dataset is stored as 'df'.")
          ),
          footer = tagList(
            actionButton(session$ns("run_rf"), "Run Model"),
            actionButton(session$ns("save_rf"), "Save Model"),
            modalButton("Close")
          ),
          size = "l"
        ))
      })
      
      observeEvent(input$run_rf, {
        req(dataset())
        df <- dataset()
        req(input$rf_dep_var, input$rf_indep_vars)
        formula_rf <- as.formula(paste(input$rf_dep_var, "~", paste(input$rf_indep_vars, collapse = "+")))
        model_rf <- tryCatch({
          randomForest(formula_rf, data = df)
        }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
        output$rf_model_output <- renderPrint({ model_rf })
      })
      
      observeEvent(input$save_rf, {
        req(dataset())
        df <- dataset()
        req(input$rf_dep_var, input$rf_indep_vars)
        formula_rf <- as.formula(paste(input$rf_dep_var, "~", paste(input$rf_indep_vars, collapse = "+")))
        model_rf <- tryCatch({
          randomForest(formula_rf, data = df)
        }, error = function(e) { showNotification(e$message, type = "error"); return(NULL) })
        saved_models$rf <- model_rf
        showNotification("Random Forest model saved.", type = "message")
      })
    }
  )
}

