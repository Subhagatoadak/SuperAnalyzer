# modeling_ui.R
# Complete Modeling Methods UI and Server modules

library(shiny)
library(bs4Dash)
library(randomForest)
library(rpart)
library(e1071)
library(MCMCpack)
library(cluster)
library(dbscan)
library(pls)
library(evaluate) # For Custom Code Execution

# UI Module --------------------------------------------------------------
modelingMethodsUI <- function(id) {
  ns <- NS(id)
  bs4TabItem(
    tabName = "modeling_methods",
    # Regression Section
    bs4Card(
      title = "Regression Methods",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      fluidRow(
        column(4,
               bs4Card(
                 title = "Linear Regression",
                 status = "info",
                 solidHeader = TRUE,
                 p("Ordinary least squares regression."),
                 footer = actionButton(ns("open_lr"), "Open")
               )
        ),
        column(4,
               bs4Card(
                 title = "Bayesian Linear Regression",
                 status = "info",
                 solidHeader = TRUE,
                 p("Bayesian regression via MCMC."),
                 footer = actionButton(ns("open_blr"), "Open")
               )
        ),
        column(4,
               bs4Card(
                 title = "Non-linear Regression",
                 status = "info",
                 solidHeader = TRUE,
                 p("Non-linear least squares fitting."),
                 footer = actionButton(ns("open_nlr"), "Open")
               )
        )
      )
    ),
    # Classification Section
    bs4Card(
      title = "Classification Methods",
      status = "warning",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      fluidRow(
        column(3,
               bs4Card(
                 title = "Logistic Regression",
                 status = "info",
                 solidHeader = TRUE,
                 p("Binary outcome modeling."),
                 footer = actionButton(ns("open_logr"), "Open")
               )
        ),
        column(3,
               bs4Card(
                 title = "Decision Tree",
                 status = "info",
                 solidHeader = TRUE,
                 p("Tree-based classification/regression."),
                 footer = actionButton(ns("open_dt"), "Open")
               )
        ),
        column(3,
               bs4Card(
                 title = "Random Forest",
                 status = "info",
                 solidHeader = TRUE,
                 p("Ensemble of decision trees."),
                 footer = actionButton(ns("open_rf"), "Open")
               )
        ),
        column(3,
               bs4Card(
                 title = "Naive Bayes",
                 status = "info",
                 solidHeader = TRUE,
                 p("Probabilistic classifier."),
                 footer = actionButton(ns("open_nb"), "Open")
               )
        )
      )
    ),
    # Clustering Section
    bs4Card(
      title = "Clustering Methods",
      status = "success",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      fluidRow(
        column(4,
               bs4Card(
                 title = "K-means",
                 status = "info",
                 solidHeader = TRUE,
                 p("Partition data into k clusters."),
                 footer = actionButton(ns("open_km"), "Open")
               )
        ),
        column(4,
               bs4Card(
                 title = "Hierarchical",
                 status = "info",
                 solidHeader = TRUE,
                 p("Agglomerative hierarchical clustering."),
                 footer = actionButton(ns("open_hc"), "Open")
               )
        ),
        column(4,
               bs4Card(
                 title = "DBSCAN",
                 status = "info",
                 solidHeader = TRUE,
                 p("Density-based clustering."),
                 footer = actionButton(ns("open_dbscan"), "Open")
               )
        )
      )
    ),
    # Dimensionality Reduction Section
    bs4Card(
      title = "Dimensionality Reduction",
      status = "secondary",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      fluidRow(
        column(6,
               bs4Card(
                 title = "PCA",
                 status = "info",
                 solidHeader = TRUE,
                 p("Principal Component Analysis."),
                 footer = actionButton(ns("open_pca"), "Open")
               )
        ),
        column(6,
               bs4Card(
                 title = "PLS",
                 status = "info",
                 solidHeader = TRUE,
                 p("Partial Least Squares Regression."),
                 footer = actionButton(ns("open_pls"), "Open")
               )
        )
      )
    ),
    # Custom Code Section
    bs4Card(
      title = "Custom Code",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      textAreaInput(ns("custom_code"), "Enter R code to run:", value = "", rows = 5),
      actionButton(ns("run_custom"), "Run Code")
    )
  )
}

# Server Module ----------------------------------------------------------
modelingMethodsServer <- function(id, dataset, saved_models) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- reactive(dataset()) # Assuming dataset is a reactive expression
    
    # Ensure saved_models is a reactiveValues object...
    
    models_dir <- "saved_models"
    if (!dir.exists(models_dir)) dir.create(models_dir)
    
    # Load saved models on startup
    # CORRECTED: Code to run once on session startup goes directly in moduleServer body
    # The original 'observe' with 'once = TRUE' was incorrect syntax.
    files <- list.files(models_dir, pattern = "\\.rds$")
    if (inherits(saved_models, "reactivevalues")) {
      for (f in files) {
        name <- sub("\\.rds$", "", f)
        tryCatch({
          saved_models[[name]] <- readRDS(file.path(models_dir, f))
        }, error = function(e) {
          warning(paste("Failed to load model", f, ":", e$message))
        })
      }
    } else {
      # Handle case where saved_models is not reactiveValues if necessary
      warning("saved_models is not a reactiveValues object. Saved models will not be loaded or saved correctly.")
    }
    # End of startup code
    
    
    # --- Helper Function to check variable types ---
    # ... rest of your helper functions and observers ...
    check_numeric_vars <- function(data, vars) {
      if (is.null(vars) || length(vars) == 0) return(FALSE)
      all(sapply(data[, vars, drop = FALSE], is.numeric))
    }
    
    check_factor_vars <- function(data, vars) {
      if (is.null(vars) || length(vars) == 0) return(FALSE)
      all(sapply(data[, vars, drop = FALSE], is.factor))
    }
    
    check_binary_dep_var <- function(data, var) {
      if (is.null(var) || length(var) == 0) return(FALSE)
      data_col <- data[[var]]
      (is.factor(data_col) && nlevels(data_col) == 2) || (is.numeric(data_col) && all(unique(data_col) %in% c(0, 1, NA), na.rm = TRUE))
    }
    
    ## Linear Regression
    observeEvent(input$open_lr, {
      # Allow modal to open even if df() is not ready, req will handle when running
      showModal(modalDialog(
        title = "Linear Regression",
        selectInput(ns("lr_dep"), "Dependent Variable", choices = names(df())),
        selectInput(ns("lr_indep"), "Independent Variables", choices = names(df()), multiple = TRUE),
        footer = tagList(actionButton(ns("run_lr"), "Run"), modalButton("Close")),
        size = "m"
      ))
    })
    
    observeEvent(input$run_lr, {
      # Require data and selections to run
      req(df(), input$lr_dep, input$lr_indep)
      
      # Basic check for variable types (numeric dependent is typical)
      if (!is.numeric(df()[[input$lr_dep]])) {
        showModal(modalDialog(title = "Input Error", "Dependent variable for Linear Regression should be numeric.", easyClose = TRUE))
        return()
      }
      if (!check_numeric_vars(df(), input$lr_indep)) {
        showModal(modalDialog(title = "Input Error", "Independent variables for Linear Regression should ideally be numeric or factors. Non-numeric inputs may cause errors.", easyClose = TRUE))
        # Don't return, let lm try to handle, but warn user
      }
      
      
      f <- as.formula(paste(input$lr_dep, "~", paste(input$lr_indep, collapse = "+")))
      model <- tryCatch(lm(f, data = df()), error = identity)
      
      output$lr_out <- renderPrint({
        if (inherits(model, "error")) model$message else summary(model)
      })
      
      # Render plot for diagnostics
      output$lr_plot <- renderPlot({
        if (!inherits(model, "error")) {
          # Standard diagnostic plots for lm
          par(mfrow = c(2, 2)) # Arrange plots in a 2x2 grid
          plot(model)
          par(mfrow = c(1, 1)) # Reset plot layout
        }
      })
      
      # Generate data with predictions
      df_with_predictions <- reactive({
        req(df(), !inherits(model, "error"))
        tryCatch({
          data <- df()
          data$Predicted <- predict(model, newdata = data)
          data
        }, error = function(e) {
          warning("Failed to generate predictions:", e$message)
          df() # Return original data if prediction fails
        })
      })
      
      
      showModal(modalDialog(
        title = "Linear Regression Results",
        tagList(
          h4("Model Summary"),
          verbatimTextOutput(ns("lr_out")),
          h4("Diagnostic Plots"),
          plotOutput(ns("lr_plot")),
          br(), # Add some space
          downloadButton(ns("dl_lr_pdf"), "Download Report (PDF)"),
          downloadButton(ns("dl_lr_data"), "Download Data with Predictions (CSV)") # New download button for data
        ),
        size = "l",
        easyClose = TRUE
      ))
      
      # Download Handler for PDF
      output$dl_lr_pdf <- downloadHandler(
        filename = function() paste0("linear_regression_report_", Sys.Date(), ".pdf"),
        content = function(file) {
          pdf(file, width = 8, height = 10) # Adjust size as needed
          # Print summary
          if (!inherits(model, "error")) print(summary(model)) else cat("Error:", model$message)
          # Print plots
          if (!inherits(model, "error")) {
            par(mfrow = c(2, 2))
            plot(model)
            par(mfrow = c(1, 1))
          } else {
            plot.new() # Create a blank plot page
            text(0.5, 0.5, "Plots not available due to model error.")
          }
          dev.off()
        }
      )
      
      # Download Handler for Data with Predictions
      output$dl_lr_data <- downloadHandler(
        filename = function() paste0("linear_regression_data_", Sys.Date(), ".csv"),
        content = function(file) {
          req(df_with_predictions()) # Ensure data is ready
          write.csv(df_with_predictions(), file, row.names = FALSE)
        }
      )
      
      # Save model
      if (inherits(saved_models, "reactivevalues") && !inherits(model, "error")) {
        saved_models$linear_regression <- model
        tryCatch({
          saveRDS(model, file.path(models_dir, "linear_regression.rds"))
        }, error = function(e) {
          warning(paste("Failed to save model linear_regression.rds:", e$message))
        })
      }
    })
    
    ## Bayesian Linear Regression (MCMCpack requires specific inputs, error handling crucial)
    observeEvent(input$open_blr, {
      showModal(modalDialog(
        title = "Bayesian Linear Regression",
        selectInput(ns("blr_dep"), "Dependent Variable", choices = names(df())),
        selectInput(ns("blr_indep"), "Independent Variables", choices = names(df()), multiple = TRUE),
        numericInput(ns("blr_iter"), "Number of Iterations", value = 10000, min = 1000, step = 1000),
        numericInput(ns("blr_burnin"), "Burn-in", value = 1000, min = 0, step = 100),
        footer = tagList(actionButton(ns("run_blr"), "Run"), modalButton("Close")),
        size = "m"
      ))
    })
    
    observeEvent(input$run_blr, {
      req(df(), input$blr_dep, input$blr_indep, input$blr_iter, input$blr_burnin)
      
      # Basic check for variable types (numeric dependent is typical)
      if (!is.numeric(df()[[input$blr_dep]])) {
        showModal(modalDialog(title = "Input Error", "Dependent variable for Bayesian Linear Regression should be numeric.", easyClose = TRUE))
        return()
      }
      if (!check_numeric_vars(df(), input$blr_indep)) {
        showModal(modalDialog(title = "Input Error", "Independent variables for Bayesian Linear Regression should ideally be numeric or factors. Non-numeric inputs may cause errors.", easyClose = TRUE))
        # Don't return, let MCMCpack try, but warn
      }
      
      f <- as.formula(paste(input$blr_dep, "~", paste(input$blr_indep, collapse = "+")))
      # Added iter and burnin inputs
      model <- tryCatch(MCMCpack::MCMCregress(f, data = df(), mcmc = input$blr_iter, burnin = input$blr_burnin), error = identity)
      
      output$blr_out <- renderPrint({
        if (inherits(model, "error")) model$message else summary(model)
      })
      
      # Render plot for diagnostics (MCMCpack provides trace plots etc.)
      output$blr_plot <- renderPlot({
        if (!inherits(model, "error")) {
          # Example plot - trace plots
          plot(model)
          # Can add others like autocorr.plot(model) or densplot(model)
        }
      })
      
      # Generate data with predicted values (posterior mean)
      df_with_predictions <- reactive({
        req(df(), !inherits(model, "error"))
        tryCatch({
          data <- df()
          # Predict using the posterior mean of coefficients
          # This is a simplified approach; full posterior predictive would be more complex
          X <- model.matrix(f, data = data) # Design matrix
          beta_mean <- colMeans(model[, 1:ncol(X)]) # Mean of posterior coefficients
          data$Predicted <- as.numeric(X %*% beta_mean)
          data
        }, error = function(e) {
          warning("Failed to generate predictions:", e$message)
          df() # Return original data if prediction fails
        })
      })
      
      
      showModal(modalDialog(
        title = "Bayesian LR Results",
        tagList(
          h4("Model Summary"),
          verbatimTextOutput(ns("blr_out")),
          h4("Diagnostic Plots (Trace Plots)"),
          plotOutput(ns("blr_plot")),
          br(),
          downloadButton(ns("dl_blr_pdf"), "Download Report (PDF)"),
          downloadButton(ns("dl_blr_data"), "Download Data with Predictions (CSV)") # New download button
        ),
        size = "l",
        easyClose = TRUE
      ))
      
      # Download Handler for PDF
      output$dl_blr_pdf <- downloadHandler(
        filename = function() paste0("bayesian_lr_report_", Sys.Date(), ".pdf"),
        content = function(file) {
          pdf(file, width = 8, height = 10)
          if (!inherits(model, "error")) print(summary(model)) else cat("Error:", model$message)
          if (!inherits(model, "error")) {
            plot(model) # Trace plots
            # Optional: autocorr.plot(model); densplot(model)
          } else {
            plot.new()
            text(0.5, 0.5, "Plots not available due to model error.")
          }
          dev.off()
        }
      )
      
      # Download Handler for Data with Predictions
      output$dl_blr_data <- downloadHandler(
        filename = function() paste0("bayesian_lr_data_", Sys.Date(), ".csv"),
        content = function(file) {
          req(df_with_predictions())
          write.csv(df_with_predictions(), file, row.names = FALSE)
        }
      )
      
      # Save model
      if (inherits(saved_models, "reactivevalues") && !inherits(model, "error")) {
        saved_models$bayesian_linear_regression <- model
        tryCatch({
          saveRDS(model, file.path(models_dir, "bayesian_linear_regression.rds"))
        }, error = function(e) {
          warning(paste("Failed to save model bayesian_linear_regression.rds:", e$message))
        })
      }
    })
    
    ## Non-linear Regression (NOTE: Still needs proper start value UI)
    observeEvent(input$open_nlr, {
      showModal(modalDialog(
        title = "Non-linear Regression",
        # Note: nls requires initial parameter guesses. This UI doesn't provide them easily.
        textInput(ns("nlr_formula"), "Formula (e.g., y ~ a * x / (b + x)). Define start values below.", "y ~ a * x / (b + x)"),
        # Add inputs for start values - CRITICAL for nls. This needs dynamic UI based on formula.
        # For simplicity, let's add a single text input for a list of start values.
        textInput(ns("nlr_start"), "Start Values (e.g., list(a=1, b=1))", "list()"),
        footer = tagList(actionButton(ns("run_nlr"), "Run"), modalButton("Close")),
        size = "m"
      ))
    })
    observeEvent(input$run_nlr, {
      req(df(), input$nlr_formula, input$nlr_start)
      
      f_text <- input$nlr_formula
      start_text <- input$nlr_start
      
      start_values <- tryCatch(eval(parse(text = start_text)), error = identity)
      
      if (inherits(start_values, "error") || !is.list(start_values)) {
        showModal(modalDialog(title = "Input Error", paste("Invalid start values format:", start_values$message), easyClose = TRUE))
        return()
      }
      
      # Attempt to run nls
      # The current nls(f, data=df()) approach is likely to fail without proper setup.
      # Need to ensure variables in formula exist in df and parameters have start values.
      f <- tryCatch(as.formula(f_text), error = identity)
      if (inherits(f, "error")) {
        showModal(modalDialog(title = "Input Error", paste("Invalid formula:", f$message), easyClose = TRUE))
        return()
      }
      
      model <- tryCatch(nls(f, data = df(), start = start_values), error = identity)
      
      output$nlr_out <- renderPrint({
        if (inherits(model, "error")) {
          paste("Error running NLS:", model$message, "\nNote: nls requires initial parameter guesses and formula variables/parameters must be defined in the dataset.")
        } else summary(model)
      })
      
      # Render plot (Fitted curve vs data)
      output$nlr_plot <- renderPlot({
        if (!inherits(model, "error")) {
          # Need data points and fitted curve
          plot_data <- df() # Use the original data frame for plotting
          # Need to identify the dependent variable from the formula
          dep_var_name <- all.vars(f)[1]
          # Need to identify the independent variable(s) from the formula
          indep_var_names <- all.vars(f)[-1]
          # This plotting assumes a single independent variable for simplicity
          if (length(indep_var_names) == 1) {
            indep_var_name <- indep_var_names[1]
            # Generate points for the fitted curve across the range of the independent variable
            x_range <- range(plot_data[[indep_var_name]], na.rm = TRUE)
            new_data <- data.frame(x = seq(x_range[1], x_range[2], length.out = 100))
            names(new_data) <- indep_var_name # Name the column correctly for predict
            new_data$Predicted <- predict(model, newdata = new_data)
            
            # Plot original data
            plot(plot_data[[indep_var_name]], plot_data[[dep_var_name]],
                 xlab = indep_var_name, ylab = dep_var_name,
                 main = "NLS Fit")
            # Add fitted curve
            lines(new_data[[indep_var_name]], new_data$Predicted, col = "blue", lwd = 2)
          } else {
            plot.new()
            text(0.5, 0.5, "Plotting for NLS currently supports only one independent variable.")
          }
        }
      })
      
      # Generate data with predictions
      df_with_predictions <- reactive({
        req(df(), !inherits(model, "error"))
        tryCatch({
          data <- df()
          data$Predicted <- predict(model, newdata = data)
          data
        }, error = function(e) {
          warning("Failed to generate predictions:", e$message)
          df() # Return original data if prediction fails
        })
      })
      
      
      showModal(modalDialog(
        title = "Non-linear Regression Results",
        tagList(
          h4("Model Summary"),
          verbatimTextOutput(ns("nlr_out")),
          h4("Fitted Curve"),
          plotOutput(ns("nlr_plot")),
          br(),
          downloadButton(ns("dl_nlr_pdf"), "Download Report (PDF)"),
          downloadButton(ns("dl_nlr_data"), "Download Data with Predictions (CSV)") # New download button
        ),
        size = "l",
        easyClose = TRUE
      ))
      
      # Download Handler for PDF
      output$dl_nlr_pdf <- downloadHandler(
        filename = function() paste0("nonlinear_regression_report_", Sys.Date(), ".pdf"),
        content = function(file) {
          pdf(file, width = 8, height = 10)
          if (!inherits(model, "error")) print(summary(model)) else cat("Error:", model$message)
          
          if (!inherits(model, "error") && length(all.vars(f)[-1]) == 1) {
            # Re-generate plot data for PDF
            plot_data <- df()
            dep_var_name <- all.vars(f)[1]
            indep_var_name <- all.vars(f)[-1][1]
            x_range <- range(plot_data[[indep_var_name]], na.rm = TRUE)
            new_data <- data.frame(x = seq(x_range[1], x_range[2], length.out = 100))
            names(new_data) <- indep_var_name
            new_data$Predicted <- predict(model, newdata = new_data)
            
            plot(plot_data[[indep_var_name]], plot_data[[dep_var_name]],
                 xlab = indep_var_name, ylab = dep_var_name,
                 main = "NLS Fit")
            lines(new_data[[indep_var_name]], new_data$Predicted, col = "blue", lwd = 2)
          } else {
            plot.new()
            text(0.5, 0.5, "Plot not available due to model error or multiple independent variables.")
          }
          
          dev.off()
        }
      )
      
      # Download Handler for Data with Predictions
      output$dl_nlr_data <- downloadHandler(
        filename = function() paste0("nonlinear_regression_data_", Sys.Date(), ".csv"),
        content = function(file) {
          req(df_with_predictions())
          write.csv(df_with_predictions(), file, row.names = FALSE)
        }
      )
      
      
      # Save model
      if (inherits(saved_models, "reactivevalues") && !inherits(model, "error")) {
        saved_models$nonlinear_regression <- model
        tryCatch({
          saveRDS(model, file.path(models_dir, "nonlinear_regression.rds"))
        }, error = function(e) {
          warning(paste("Failed to save model nonlinear_regression.rds:", e$message))
        })
      }
    })
    
    
    ## Logistic Regression
    observeEvent(input$open_logr, {
      showModal(modalDialog(
        title = "Logistic Regression",
        selectInput(ns("logr_dep"), "Dependent Variable", choices = names(df())), # Should ideally filter for binary/factor
        selectInput(ns("logr_indep"), "Independent Variables", choices = names(df()), multiple = TRUE),
        footer = tagList(actionButton(ns("run_logr"), "Run"), modalButton("Close")),
        size = "m"
      ))
    })
    
    observeEvent(input$run_logr, {
      req(df(), input$logr_dep, input$logr_indep)
      
      # Check if dependent variable is suitable for logistic regression (binary/factor)
      if (!check_binary_dep_var(df(), input$logr_dep)) {
        showModal(modalDialog(
          title = "Error",
          "Dependent variable must be a binary factor or 0/1 numeric for Logistic Regression.",
          easyClose = TRUE
        ))
        return() # Stop execution
      }
      
      # Basic check for independent variable types
      if (!check_numeric_vars(df(), input$logr_indep) && !check_factor_vars(df(), input$logr_indep)) {
        showModal(modalDialog(title = "Input Error", "Independent variables for Logistic Regression should ideally be numeric or factors.", easyClose = TRUE))
        # Don't return, let glm try, but warn
      }
      
      
      f <- as.formula(paste(input$logr_dep, "~", paste(input$logr_indep, collapse = "+")))
      model <- tryCatch(glm(f, data = df(), family = binomial), error = identity)
      
      output$logr_out <- renderPrint({
        if (inherits(model, "error")) model$message else summary(model)
      })
      
      # Render plot for diagnostics (similar to lm)
      output$logr_plot <- renderPlot({
        if (!inherits(model, "error")) {
          par(mfrow = c(2, 2))
          plot(model)
          par(mfrow = c(1, 1))
        }
      })
      
      # Generate data with predictions (probabilities and predicted class)
      df_with_predictions <- reactive({
        req(df(), !inherits(model, "error"))
        tryCatch({
          data <- df()
          data$Predicted_Prob <- predict(model, newdata = data, type = "response")
          # Get predicted class (assuming binary outcome)
          # Need to know which level is the positive outcome or use 0.5 threshold
          # For simplicity, let's use 0.5 threshold or factor levels if available
          if (is.factor(df()[[input$logr_dep]])) {
            data$Predicted_Class <- factor(ifelse(data$Predicted_Prob > 0.5, levels(df()[[input$logr_dep]])[2], levels(df()[[input$logr_dep]])[1]),
                                           levels = levels(df()[[input$logr_dep]]))
          } else { # Assuming 0/1 numeric
            data$Predicted_Class <- ifelse(data$Predicted_Prob > 0.5, 1, 0)
          }
          data
        }, error = function(e) {
          warning("Failed to generate predictions:", e$message)
          df() # Return original data if prediction fails
        })
      })
      
      
      showModal(modalDialog(
        title = "Logistic Regression Results",
        tagList(
          h4("Model Summary"),
          verbatimTextOutput(ns("logr_out")),
          h4("Diagnostic Plots"),
          plotOutput(ns("logr_plot")),
          br(),
          downloadButton(ns("dl_logr_pdf"), "Download Report (PDF)"),
          downloadButton(ns("dl_logr_data"), "Download Data with Predictions (CSV)") # New download button
        ),
        size = "l",
        easyClose = TRUE
      ))
      
      # Download Handler for PDF
      output$dl_logr_pdf <- downloadHandler(
        filename = function() paste0("logistic_regression_report_", Sys.Date(), ".pdf"),
        content = function(file) {
          pdf(file, width = 8, height = 10)
          if (!inherits(model, "error")) print(summary(model)) else cat("Error:", model$message)
          
          if (!inherits(model, "error")) {
            par(mfrow = c(2, 2))
            plot(model)
            par(mfrow = c(1, 1))
          } else {
            plot.new()
            text(0.5, 0.5, "Plots not available due to model error.")
          }
          dev.off()
        }
      )
      
      # Download Handler for Data with Predictions
      output$dl_logr_data <- downloadHandler(
        filename = function() paste0("logistic_regression_data_", Sys.Date(), ".csv"),
        content = function(file) {
          req(df_with_predictions())
          write.csv(df_with_predictions(), file, row.names = FALSE)
        }
      )
      
      # Save model
      if (inherits(saved_models, "reactivevalues") && !inherits(model, "error")) {
        saved_models$logistic_regression <- model
        tryCatch({
          saveRDS(model, file.path(models_dir, "logistic_regression.rds"))
        }, error = function(e) {
          warning(paste("Failed to save model logistic_regression.rds:", e$message))
        })
      }
    })
    
    ## Decision Tree
    observeEvent(input$open_dt, {
      showModal(modalDialog(
        title = "Decision Tree",
        selectInput(ns("dt_dep"), "Dependent Variable", choices = names(df())),
        selectInput(ns("dt_indep"), "Independent Variables", choices = names(df()), multiple = TRUE),
        # Add control options? e.g., complexity parameter (cp)
        numericInput(ns("dt_cp"), "Complexity Parameter (cp)", value = 0.01, min = 0, step = 0.001),
        footer = tagList(actionButton(ns("run_dt"), "Run"), modalButton("Close")),
        size = "m"
      ))
    })
    observeEvent(input$run_dt, {
      req(df(), input$dt_dep, input$dt_indep, input$dt_cp)
      
      # rpart can handle various types, but factor dependent is common for classification trees
      dep_var_data <- df()[[input$dt_dep]]
      if (is.numeric(dep_var_data) && length(unique(dep_var_data)) > 10) { # Arbitrary threshold
        # It's a regression tree case
      } else if (!is.factor(dep_var_data) && length(unique(dep_var_data)) <= 10) {
        # Suggest converting to factor if few unique values
        showModal(modalDialog(title = "Input Warning", "Dependent variable has few unique values. Consider converting to factor for classification tree.", easyClose = TRUE))
      }
      
      
      f <- as.formula(paste(input$dt_dep, "~", paste(input$dt_indep, collapse = "+")))
      # Added cp control
      model <- tryCatch(rpart::rpart(f, data = df(), control = rpart::rpart.control(cp = input$dt_cp)), error = identity)
      
      output$dt_out <- renderPrint({
        if (inherits(model, "error")) model$message else print(model) # print gives the tree structure
      })
      
      # Render plot (Decision tree plot)
      output$dt_plot <- renderPlot({
        if (!inherits(model, "error")) {
          plot(model, uniform = TRUE, main = "Decision Tree")
          text(model, use.n = TRUE, all = TRUE, cex = .8) # Add labels
        }
      })
      
      # Generate data with predictions
      df_with_predictions <- reactive({
        req(df(), !inherits(model, "error"))
        tryCatch({
          data <- df()
          # Predict based on model type (regression or classification)
          if (is.numeric(df()[[input$dt_dep]])) { # Regression tree
            data$Predicted <- predict(model, newdata = data)
          } else { # Classification tree
            data$Predicted <- predict(model, newdata = data, type = "class")
          }
          data
        }, error = function(e) {
          warning("Failed to generate predictions:", e$message)
          df() # Return original data if prediction fails
        })
      })
      
      
      showModal(modalDialog(
        title = "Decision Tree Results",
        tagList(
          h4("Model Summary"),
          verbatimTextOutput(ns("dt_out")),
          h4("Tree Plot"),
          plotOutput(ns("dt_plot")),
          br(),
          downloadButton(ns("dl_dt_pdf"), "Download Report (PDF)"),
          downloadButton(ns("dl_dt_data"), "Download Data with Predictions (CSV)") # New download button
        ),
        size = "l",
        easyClose = TRUE
      ))
      
      # Download Handler for PDF
      output$dl_dt_pdf <- downloadHandler(
        filename = function() paste0("decision_tree_report_", Sys.Date(), ".pdf"),
        content = function(file) {
          pdf(file, width = 10, height = 12) # Adjust size for tree plot
          if (!inherits(model, "error")) print(model) else cat("Error:", model$message)
          
          if (!inherits(model, "error")) {
            plot(model, uniform = TRUE, main = "Decision Tree")
            text(model, use.n = TRUE, all = TRUE, cex = .8)
          } else {
            plot.new()
            text(0.5, 0.5, "Plot not available due to model error.")
          }
          dev.off()
        }
      )
      
      # Download Handler for Data with Predictions
      output$dl_dt_data <- downloadHandler(
        filename = function() paste0("decision_tree_data_", Sys.Date(), ".csv"),
        content = function(file) {
          req(df_with_predictions())
          write.csv(df_with_predictions(), file, row.names = FALSE)
        }
      )
      
      # Save model
      if (inherits(saved_models, "reactivevalues") && !inherits(model, "error")) {
        saved_models$decision_tree <- model
        tryCatch({
          saveRDS(model, file.path(models_dir, "decision_tree.rds"))
        }, error = function(e) {
          warning(paste("Failed to save model decision_tree.rds:", e$message))
        })
      }
    })
    
    ## Random Forest
    observeEvent(input$open_rf, {
      showModal(modalDialog(
        title = "Random Forest",
        selectInput(ns("rf_dep"), "Dependent Variable", choices = names(df())),
        selectInput(ns("rf_indep"), "Independent Variables", choices = names(df()), multiple = TRUE),
        numericInput(ns("rf_ntree"), "Number of trees (ntree)", value = 500, min = 1),
        # mtry default depends on classification vs regression
        # Can make this reactive based on dependent variable type selection
        uiOutput(ns("rf_mtry_ui")),
        footer = tagList(actionButton(ns("run_rf"), "Run"), modalButton("Close")),
        size = "m"
      ))
    })
    
    # Dynamic UI for mtry based on dependent variable selection
    output$rf_mtry_ui <- renderUI({
      req(df(), input$rf_dep)
      dep_var_data <- df()[[input$rf_dep]]
      num_indep <- length(input$rf_indep)
      
      default_mtry <- if (is.factor(dep_var_data)) {
        floor(sqrt(num_indep)) # Classification
      } else {
        max(1, floor(num_indep / 3)) # Regression
      }
      # Ensure mtry is at least 1 and no more than the number of independent variables
      default_mtry <- max(1, min(default_mtry, num_indep))
      
      numericInput(ns("rf_mtry"), "Number of variables per split (mtry)",
                   value = default_mtry, min = 1, max = num_indep)
    })
    
    
    observeEvent(input$run_rf, {
      req(df(), input$rf_dep, input$rf_indep, input$rf_ntree, input$rf_mtry)
      
      # randomForest handles various types, factor dependent for classification
      dep_var_data <- df()[[input$rf_dep]]
      if (is.numeric(dep_var_data) && length(unique(dep_var_data)) > 10) { # Arbitrary threshold
        # Regression forest case
      } else if (!is.factor(dep_var_data) && length(unique(dep_var_data)) <= 10) {
        # Suggest converting to factor
        showModal(modalDialog(title = "Input Warning", "Dependent variable has few unique values. Consider converting to factor for classification forest.", easyClose = TRUE))
      }
      
      
      f <- as.formula(paste(input$rf_dep, "~", paste(input$rf_indep, collapse = "+")))
      # Added ntree and mtry inputs
      model <- tryCatch(randomForest::randomForest(f, data = df(), ntree = input$rf_ntree, mtry = input$rf_mtry, importance = TRUE), error = identity) # importance for varImpPlot
      
      output$rf_out <- renderPrint({
        if (inherits(model, "error")) model$message else print(model)
      })
      
      # Render plot (Variable importance)
      output$rf_plot <- renderPlot({
        if (!inherits(model, "error")) {
          randomForest::varImpPlot(model)
        }
      })
      
      # Generate data with predictions
      df_with_predictions <- reactive({
        req(df(), !inherits(model, "error"))
        tryCatch({
          data <- df()
          # Predict based on model type
          if (is.numeric(df()[[input$rf_dep]])) { # Regression forest
            data$Predicted <- predict(model, newdata = data)
          } else { # Classification forest
            data$Predicted <- predict(model, newdata = data, type = "class")
          }
          data
        }, error = function(e) {
          warning("Failed to generate predictions:", e$message)
          df() # Return original data if prediction fails
        })
      })
      
      
      showModal(modalDialog(
        title = "Random Forest Results",
        tagList(
          h4("Model Summary"),
          verbatimTextOutput(ns("rf_out")),
          h4("Variable Importance Plot"),
          plotOutput(ns("rf_plot")),
          br(),
          downloadButton(ns("dl_rf_pdf"), "Download Report (PDF)"),
          downloadButton(ns("dl_rf_data"), "Download Data with Predictions (CSV)") # New download button
        ),
        size = "l",
        easyClose = TRUE
      ))
      
      # Download Handler for PDF
      output$dl_rf_pdf <- downloadHandler(
        filename = function() paste0("random_forest_report_", Sys.Date(), ".pdf"),
        content = function(file) {
          pdf(file, width = 8, height = 10)
          if (!inherits(model, "error")) print(model) else cat("Error:", model$message)
          
          if (!inherits(model, "error")) {
            randomForest::varImpPlot(model)
          } else {
            plot.new()
            text(0.5, 0.5, "Plot not available due to model error.")
          }
          dev.off()
        }
      )
      
      # Download Handler for Data with Predictions
      output$dl_rf_data <- downloadHandler(
        filename = function() paste0("random_forest_data_", Sys.Date(), ".csv"),
        content = function(file) {
          req(df_with_predictions())
          write.csv(df_with_predictions(), file, row.names = FALSE)
        }
      )
      
      
      # Save model
      if (inherits(saved_models, "reactivevalues") && !inherits(model, "error")) {
        saved_models$random_forest <- model
        tryCatch({
          saveRDS(model, file.path(models_dir, "random_forest.rds"))
        }, error = function(e) {
          warning(paste("Failed to save model random_forest.rds:", e$message))
        })
      }
    })
    
    ## Naive Bayes
    observeEvent(input$open_nb, {
      showModal(modalDialog(
        title = "Naive Bayes",
        selectInput(ns("nb_dep"), "Dependent Variable", choices = names(df())), # Should ideally filter for factor
        selectInput(ns("nb_indep"), "Independent Variables", choices = names(df()), multiple = TRUE),
        footer = tagList(actionButton(ns("run_nb"), "Run"), modalButton("Close")),
        size = "m"
      ))
    })
    
    observeEvent(input$run_nb, {
      req(df(), input$nb_dep, input$nb_indep)
      
      # Check if dependent variable is suitable for classification (factor)
      if (!is.factor(df()[[input$nb_dep]])) {
        showModal(modalDialog(
          title = "Error",
          "Dependent variable must be a factor for Naive Bayes.",
          easyClose = TRUE
        ))
        return() # Stop execution
      }
      # Naive Bayes handles numeric and factors for independent vars
      
      
      f <- as.formula(paste(input$nb_dep, "~", paste(input$nb_indep, collapse = "+")))
      model <- tryCatch(e1071::naiveBayes(f, data = df()), error = identity)
      
      output$nb_out <- renderPrint({
        if (inherits(model, "error")) model$message else print(model)
      })
      
      # Render plot (No standard diagnostic plots, perhaps conditional distributions?)
      # Keeping it simple: no plot output for Naive Bayes for now unless a specific one is desired.
      # output$nb_plot <- renderPlot({...})
      
      # Generate data with predictions
      df_with_predictions <- reactive({
        req(df(), !inherits(model, "error"))
        tryCatch({
          data <- df()
          data$Predicted_Class <- predict(model, newdata = data, type = "class")
          # Optional: add posterior probabilities
          # data_probs <- predict(model, newdata = data, type = "raw")
          # data <- cbind(data, data_probs)
          data
        }, error = function(e) {
          warning("Failed to generate predictions:", e$message)
          df() # Return original data if prediction fails
        })
      })
      
      
      showModal(modalDialog(
        title = "Naive Bayes Results",
        tagList(
          h4("Model Summary"),
          verbatimTextOutput(ns("nb_out")),
          # plotOutput(ns("nb_plot")), # Hide plot output if not used
          br(),
          downloadButton(ns("dl_nb_pdf"), "Download Report (PDF)"),
          downloadButton(ns("dl_nb_data"), "Download Data with Predictions (CSV)") # New download button
        ),
        size = "l",
        easyClose = TRUE
      ))
      
      # Download Handler for PDF
      output$dl_nb_pdf <- downloadHandler(
        filename = function() paste0("naive_bayes_report_", Sys.Date(), ".pdf"),
        content = function(file) {
          pdf(file, width = 8, height = 10)
          if (!inherits(model, "error")) print(model) else cat("Error:", model$message)
          # Add placeholder if no plot is rendered
          plot.new()
          text(0.5, 0.5, "No standard diagnostic plots for Naive Bayes.")
          dev.off()
        }
      )
      
      # Download Handler for Data with Predictions
      output$dl_nb_data <- downloadHandler(
        filename = function() paste0("naive_bayes_data_", Sys.Date(), ".csv"),
        content = function(file) {
          req(df_with_predictions())
          write.csv(df_with_predictions(), file, row.names = FALSE)
        }
      )
      
      # Save model
      if (inherits(saved_models, "reactivevalues") && !inherits(model, "error")) {
        saved_models$naive_bayes <- model
        tryCatch({
          saveRDS(model, file.path(models_dir, "naive_bayes.rds"))
        }, error = function(e) {
          warning(paste("Failed to save model naive_bayes.rds:", e$message))
        })
      }
    })
    
    ## K-means Clustering
    observeEvent(input$open_km, {
      showModal(modalDialog(
        title = "K-means Clustering",
        selectInput(ns("km_vars"), "Variables", choices = names(df()), multiple = TRUE), # Should ideally filter for numeric
        numericInput(ns("km_centers"), "Number of Clusters", value = 3, min = 2),
        numericInput(ns("km_nstart"), "Number of random starts (nstart)", value = 10, min = 1), # Recommended for better results
        footer = tagList(actionButton(ns("run_km"), "Run"), modalButton("Close")),
        size = "m"
      ))
    })
    observeEvent(input$run_km, {
      req(df(), input$km_vars, input$km_centers, input$km_nstart)
      
      # Check if selected columns are numeric
      if (!check_numeric_vars(df(), input$km_vars)) {
        showModal(modalDialog(
          title = "Error",
          "K-means requires numeric variables. Please select only numeric columns.",
          easyClose = TRUE
        ))
        return() # Stop execution
      }
      
      data_for_clustering <- df()[, input$km_vars, drop = FALSE]
      
      model <- tryCatch(stats::kmeans(data_for_clustering, centers = input$km_centers, nstart = input$km_nstart), error = identity) # Added nstart
      
      output$km_out <- renderPrint({
        if (inherits(model, "error")) model$message else print(model)
      })
      
      # Render plot (Data points colored by cluster) - works best for 2 variables
      output$km_plot <- renderPlot({
        if (!inherits(model, "error")) {
          if (length(input$km_vars) >= 2) {
            # Plot first two variables
            plot(data_for_clustering[, 1:2], col = model$cluster,
                 main = paste("K-means Clustering (k=", input$km_centers, ")"),
                 xlab = input$km_vars[1], ylab = input$km_vars[2],
                 pch = 16) # Solid circles
            points(model$centers[, 1:2], col = 1:input$km_centers, pch = 8, cex = 2) # Plot centers
          } else if (length(input$km_vars) == 1) {
            plot.new()
            text(0.5, 0.5, "Plotting for K-means requires at least two variables.")
          } else {
            plot.new() # Should not happen due to req and numeric check
          }
        }
      })
      
      # Generate data with cluster assignments
      df_with_clusters <- reactive({
        req(df(), !inherits(model, "error"))
        tryCatch({
          data <- df()
          data$Cluster <- as.factor(model$cluster) # Store cluster as factor
          data
        }, error = function(e) {
          warning("Failed to add clusters:", e$message)
          df() # Return original data if error
        })
      })
      
      
      showModal(modalDialog(
        title = "K-means Results",
        tagList(
          h4("Model Summary"),
          verbatimTextOutput(ns("km_out")),
          h4("Cluster Plot (First 2 Variables)"),
          plotOutput(ns("km_plot")),
          br(),
          downloadButton(ns("dl_km_pdf"), "Download Report (PDF)"),
          downloadButton(ns("dl_km_data"), "Download Data with Clusters (CSV)") # New download button
        ),
        size = "l",
        easyClose = TRUE
      ))
      
      # Download Handler for PDF
      output$dl_km_pdf <- downloadHandler(
        filename = function() paste0("kmeans_report_", Sys.Date(), ".pdf"),
        content = function(file) {
          pdf(file, width = 8, height = 10)
          if (!inherits(model, "error")) print(model) else cat("Error:", model$message)
          
          if (!inherits(model, "error") && length(input$km_vars) >= 2) {
            # Re-generate plot data for PDF
            data_for_clustering_pdf <- df()[, input$km_vars, drop = FALSE]
            plot(data_for_clustering_pdf[, 1:2], col = model$cluster,
                 main = paste("K-means Clustering (k=", input$km_centers, ")"),
                 xlab = input$km_vars[1], ylab = input$km_vars[2],
                 pch = 16)
            points(model$centers[, 1:2], col = 1:input$km_centers, pch = 8, cex = 2)
          } else if (!inherits(model, "error") && length(input$km_vars) == 1) {
            plot.new()
            text(0.5, 0.5, "Plotting for K-means requires at least two variables.")
          } else {
            plot.new()
            text(0.5, 0.5, "Plot not available due to model error or insufficient variables.")
          }
          dev.off()
        }
      )
      
      # Download Handler for Data with Clusters
      output$dl_km_data <- downloadHandler(
        filename = function() paste0("kmeans_data_", Sys.Date(), ".csv"),
        content = function(file) {
          req(df_with_clusters())
          write.csv(df_with_clusters(), file, row.names = FALSE)
        }
      )
      
      # Save model
      if (inherits(saved_models, "reactivevalues") && !inherits(model, "error")) {
        saved_models$kmeans <- model
        tryCatch({
          saveRDS(model, file.path(models_dir, "kmeans.rds"))
        }, error = function(e) {
          warning(paste("Failed to save model kmeans.rds:", e$message))
        })
      }
    })
    
    ## Hierarchical Clustering
    observeEvent(input$open_hc, {
      showModal(modalDialog(
        title = "Hierarchical Clustering",
        selectInput(ns("hc_vars"), "Variables", choices = names(df()), multiple = TRUE), # Should ideally filter for numeric
        selectInput(ns("hc_method"), "Linkage Method", c("complete", "average", "single", "ward.D", "ward.D2", "mcquitty", "median", "centroid"), selected = "complete"),
        footer = tagList(actionButton(ns("run_hc"), "Run"), modalButton("Close")),
        size = "m"
      ))
    })
    observeEvent(input$run_hc, {
      req(df(), input$hc_vars, input$hc_method)
      
      # Check if selected columns are numeric
      if (!check_numeric_vars(df(), input$hc_vars)) {
        showModal(modalDialog(
          title = "Error",
          "Hierarchical Clustering requires numeric variables. Please select only numeric columns.",
          easyClose = TRUE
        ))
        return() # Stop execution
      }
      if (length(input$hc_vars) == 0) {
        showModal(modalDialog(title = "Input Error", "Please select variables for Hierarchical Clustering.", easyClose = TRUE))
        return()
      }
      
      
      data_for_clustering <- df()[, input$hc_vars, drop = FALSE]
      
      # Calculate distance matrix
      dist_mat <- tryCatch(stats::dist(data_for_clustering), error = identity)
      
      model <- NULL # Initialize model
      if (inherits(dist_mat, "error")) {
        output$hc_out <- renderPrint(paste("Error calculating distance matrix:", dist_mat$message))
      } else {
        model <- tryCatch(stats::hclust(dist_mat, method = input$hc_method), error = identity) # Added method
        output$hc_out <- renderPrint(if (inherits(model, "error")) model$message else print(model))
      }
      
      # Render plot (Dendrogram)
      output$hc_plot <- renderPlot({
        if (!inherits(dist_mat, "error") && !inherits(model, "error")) {
          plot(model, main = paste("Hierarchical Clustering Dendrogram (Method:", input$hc_method, ")"))
        } else {
          plot.new()
          text(0.5, 0.5, "Dendrogram not available due to calculation or model error.")
        }
      })
      
      # Generate data with cluster assignments (needs cutting the tree)
      # Note: Requires user input for number of clusters or height to cut
      # For simplicity, let's add an input for k (number of clusters)
      # This reactive updates when the plot is shown or PDF is downloaded
      df_with_clusters <- reactive({
        req(df(), !inherits(model, "error"), input$hc_k_cut) # Require the k input from the modal
        tryCatch({
          data <- df()
          # Cut tree into k groups
          data$Cluster <- as.factor(cutree(model, k = input$hc_k_cut))
          data
        }, error = function(e) {
          warning("Failed to add clusters:", e$message)
          df() # Return original data if error
        })
      })
      
      
      showModal(modalDialog(
        title = "Hierarchical Clustering Results",
        tagList(
          h4("Model Output"),
          verbatimTextOutput(ns("hc_out")),
          h4("Dendrogram"),
          plotOutput(ns("hc_plot")),
          # Add input for cutting the tree
          numericInput(ns("hc_k_cut"), "Number of Clusters to Cut Tree", value = 2, min = 1),
          br(),
          downloadButton(ns("dl_hc_pdf"), "Download Report (PDF)"),
          downloadButton(ns("dl_hc_data"), "Download Data with Clusters (CSV)") # New download button
        ),
        size = "l",
        easyClose = TRUE
      ))
      
      # Download Handler for PDF
      output$dl_hc_pdf <- downloadHandler(
        filename = function() paste0("hierarchical_clustering_report_", Sys.Date(), ".pdf"),
        content = function(file) {
          pdf(file, width = 10, height = 8) # Adjust size for dendrogram
          if (!inherits(model, "error")) print(model) else cat("Error:", model$message)
          
          if (!inherits(dist_mat, "error") && !inherits(model, "error")) {
            plot(model, main = paste("Hierarchical Clustering Dendrogram (Method:", input$hc_method, ")"))
            # Optionally show the cut
            # rect.hclust(model, k = input$hc_k_cut, border = "red")
          } else {
            plot.new()
            text(0.5, 0.5, "Dendrogram not available due to calculation or model error.")
          }
          dev.off()
        }
      )
      
      # Download Handler for Data with Clusters
      output$dl_hc_data <- downloadHandler(
        filename = function() paste0("hierarchical_clustering_data_", Sys.Date(), ".csv"),
        content = function(file) {
          # Need to ensure df_with_clusters reactive has run using the k value from the modal
          req(df_with_clusters())
          write.csv(df_with_clusters(), file, row.names = FALSE)
        }
      )
      
      
      # Save model (only save hclust object if successful)
      if (inherits(saved_models, "reactivevalues") && !inherits(model, "error")) {
        saved_models$hierarchical_clustering <- model
        tryCatch({
          saveRDS(model, file.path(models_dir, "hierarchical_clustering.rds"))
        }, error = function(e) {
          warning(paste("Failed to save model hierarchical_clustering.rds:", e$message))
        })
      }
    })
    
    ## DBSCAN Clustering
    observeEvent(input$open_dbscan, {
      showModal(modalDialog(
        title = "DBSCAN Clustering",
        selectInput(ns("db_vars"), "Variables", choices = names(df()), multiple = TRUE), # Should ideally filter for numeric
        numericInput(ns("db_eps"), "Epsilon (eps)", value = 0.5, step = 0.1),
        numericInput(ns("db_minpts"), "Minimum Points (minPts)", value = 5, min = 1),
        footer = tagList(actionButton(ns("run_dbscan"), "Run"), modalButton("Close")),
        size = "m"
      ))
    })
    observeEvent(input$run_dbscan, {
      req(df(), input$db_vars, input$db_eps, input$db_minpts)
      
      # Check if selected columns are numeric
      if (!check_numeric_vars(df(), input$db_vars)) {
        showModal(modalDialog(
          title = "Error",
          "DBSCAN requires numeric variables. Please select only numeric columns.",
          easyClose = TRUE
        ))
        return() # Stop execution
      }
      if (length(input$db_vars) == 0) {
        showModal(modalDialog(title = "Input Error", "Please select variables for DBSCAN Clustering.", easyClose = TRUE))
        return()
      }
      
      
      data_for_clustering <- df()[, input$db_vars, drop = FALSE]
      
      # DBSCAN often works better with scaled data, but let's stick to current structure
      # data_scaled <- scale(data_for_clustering)
      
      model <- tryCatch(dbscan::dbscan(data_for_clustering, eps = input$db_eps, minPts = input$db_minpts), error = identity) # Use dbscan::
      
      output$dbscan_out <- renderPrint({
        if (inherits(model, "error")) model$message else print(model)
      })
      
      # Render plot (Data points colored by cluster) - works best for 2 variables
      output$dbscan_plot <- renderPlot({
        if (!inherits(model, "error")) {
          if (length(input$db_vars) >= 2) {
            # Plot first two variables
            plot(data_for_clustering[, 1:2], col = model$cluster + 1, # Add 1 because cluster 0 is noise
                 main = paste("DBSCAN Clustering (eps=", input$db_eps, ", minPts=", input$db_minpts, ")"),
                 xlab = input$db_vars[1], ylab = input$db_vars[2],
                 pch = 16)
            # Add legend manually if needed
          } else if (length(input$db_vars) == 1) {
            plot.new()
            text(0.5, 0.5, "Plotting for DBSCAN requires at least two variables.")
          } else {
            plot.new() # Should not happen
          }
        }
      })
      
      
      # Generate data with cluster assignments
      df_with_clusters <- reactive({
        req(df(), !inherits(model, "error"))
        tryCatch({
          data <- df()
          # Cluster 0 is noise. Convert to factor, maybe label 0 as "Noise".
          data$Cluster <- as.factor(model$cluster)
          levels(data$Cluster)[levels(data$Cluster) == "0"] <- "Noise"
          data
        }, error = function(e) {
          warning("Failed to add clusters:", e$message)
          df() # Return original data if error
        })
      })
      
      
      showModal(modalDialog(
        title = "DBSCAN Results",
        tagList(
          h4("Model Summary"),
          verbatimTextOutput(ns("dbscan_out")),
          h4("Cluster Plot (First 2 Variables)"),
          plotOutput(ns("dbscan_plot")),
          br(),
          downloadButton(ns("dl_dbscan_pdf"), "Download Report (PDF)"),
          downloadButton(ns("dl_dbscan_data"), "Download Data with Clusters (CSV)") # New download button
        ),
        size = "l",
        easyClose = TRUE
      ))
      
      # Download Handler for PDF
      output$dl_dbscan_pdf <- downloadHandler(
        filename = function() paste0("dbscan_report_", Sys.Date(), ".pdf"),
        content = function(file) {
          pdf(file, width = 8, height = 10)
          if (!inherits(model, "error")) print(model) else cat("Error:", model$message)
          
          if (!inherits(model, "error") && length(input$db_vars) >= 2) {
            data_for_clustering_pdf <- df()[, input$db_vars, drop = FALSE]
            plot(data_for_clustering_pdf[, 1:2], col = model$cluster + 1,
                 main = paste("DBSCAN Clustering (eps=", input$db_eps, ", minPts=", input$db_minpts, ")"),
                 xlab = input$db_vars[1], ylab = input$db_vars[2],
                 pch = 16)
          } else if (!inherits(model, "error") && length(input$db_vars) == 1) {
            plot.new()
            text(0.5, 0.5, "Plotting for DBSCAN requires at least two variables.")
          } else {
            plot.new()
            text(0.5, 0.5, "Plot not available due to model error or insufficient variables.")
          }
          dev.off()
        }
      )
      
      # Download Handler for Data with Clusters
      output$dl_dbscan_data <- downloadHandler(
        filename = function() paste0("dbscan_data_", Sys.Date(), ".csv"),
        content = function(file) {
          req(df_with_clusters())
          write.csv(df_with_clusters(), file, row.names = FALSE)
        }
      )
      
      
      # Save model
      if (inherits(saved_models, "reactivevalues") && !inherits(model, "error")) {
        saved_models$dbscan <- model
        tryCatch({
          saveRDS(model, file.path(models_dir, "dbscan.rds"))
        }, error = function(e) {
          warning(paste("Failed to save model dbscan.rds:", e$message))
        })
      }
    })
    
    ## PCA
    observeEvent(input$open_pca, {
      showModal(modalDialog(
        title = "PCA",
        selectInput(ns("pca_vars"), "Variables", choices = names(df()), multiple = TRUE), # Should ideally filter for numeric
        footer = tagList(actionButton(ns("run_pca"), "Run"), modalButton("Close")),
        size = "m"
      ))
    })
    observeEvent(input$run_pca, {
      req(df(), input$pca_vars)
      
      # Check if selected columns are numeric
      if (!check_numeric_vars(df(), input$pca_vars)) {
        showModal(modalDialog(
          title = "Error",
          "PCA requires numeric variables. Please select only numeric columns.",
          easyClose = TRUE
        ))
        return() # Stop execution
      }
      if (length(input$pca_vars) == 0) {
        showModal(modalDialog(title = "Input Error", "Please select variables for PCA.", easyClose = TRUE))
        return()
      }
      
      data_for_pca <- df()[, input$pca_vars, drop = FALSE]
      
      # prcomp handles NA by default (na.action = na.omit), but good to be aware
      # Let's add a check/warning for NAs
      if (anyNA(data_for_pca)) {
        showModal(modalDialog(title = "Data Warning", "Selected variables contain missing values (NA). Rows with NAs will be omitted for PCA.", easyClose = TRUE))
        # The prcomp function handles this, so no need to return
      }
      
      model <- tryCatch(stats::prcomp(data_for_pca, center = TRUE, scale. = TRUE), error = identity) # Use stats::
      
      output$pca_out <- renderPrint({
        if (inherits(model, "error")) model$message else summary(model)
      })
      
      # Render plots (Scree plot and Biplot)
      output$pca_plot1 <- renderPlot({
        if (!inherits(model, "error")) {
          screeplot(model, type = "lines", main = "Scree Plot")
        }
      })
      output$pca_plot2 <- renderPlot({
        if (!inherits(model, "error")) {
          # Biplot requires at least 2 PCs and sufficient variables
          if (ncol(model$x) >= 2 && ncol(model$rotation) >= 2) {
            biplot(model, main = "Biplot (PC1 vs PC2)")
          } else {
            plot.new()
            text(0.5, 0.5, "Biplot requires at least 2 principal components.")
          }
        }
      })
      
      
      # Generate data with PCA scores
      df_with_scores <- reactive({
        req(df(), !inherits(model, "error"))
        tryCatch({
          data <- df()
          # Get PCA scores (principal components)
          pca_scores <- as.data.frame(model$x)
          # Need to handle rows omitted due to NA. Join by row names if original df has them.
          # Or more simply, if no NAs, just add the columns.
          # If NAs were present, the number of rows in model$x might be less than df().
          # A robust way involves merging or aligning rows if possible.
          # For simplicity now, assume no NAs or accept reduced row count if NAs were dropped.
          # A better approach for NAs would be imputation or explicitly handling omitted rows.
          
          # Simple approach: Add scores to the *original* df, NAs will align if prcomp removed rows
          # This might not be correct if df has NAs in *other* columns not used for PCA
          # A more robust approach is needed for general NA handling.
          # Let's add them by rownames, assuming original df has row names that match the subset used by prcomp
          if (is.null(rownames(data))) rownames(data) <- 1:nrow(data)
          data_with_scores <- merge(data, pca_scores, by = 0, all.x = TRUE) # Merge using row names as key
          rownames(data_with_scores) <- data_with_scores$X0 # Restore original row names
          data_with_scores$X0 <- NULL # Remove the merge key column
          data_with_scores
        }, error = function(e) {
          warning("Failed to add PCA scores:", e$message)
          df() # Return original data if error
        })
      })
      
      
      showModal(modalDialog(
        title = "PCA Results",
        tagList(
          h4("Model Summary"),
          verbatimTextOutput(ns("pca_out")),
          h4("Scree Plot"),
          plotOutput(ns("pca_plot1")),
          h4("Biplot"),
          plotOutput(ns("pca_plot2")),
          br(),
          downloadButton(ns("dl_pca_pdf"), "Download Report (PDF)"),
          downloadButton(ns("dl_pca_data"), "Download Data with PCA Scores (CSV)") # New download button
        ),
        size = "l",
        easyClose = TRUE
      ))
      
      # Download Handler for PDF
      output$dl_pca_pdf <- downloadHandler(
        filename = function() paste0("pca_report_", Sys.Date(), ".pdf"),
        content = function(file) {
          pdf(file, width = 8, height = 10)
          if (!inherits(model, "error")) print(summary(model)) else cat("Error:", model$message)
          
          if (!inherits(model, "error")) {
            screeplot(model, type = "lines", main = "Scree Plot")
            # Need to check dimensions again for biplot in PDF
            if (ncol(model$x) >= 2 && ncol(model$rotation) >= 2) {
              biplot(model, main = "Biplot (PC1 vs PC2)")
            } else {
              plot.new()
              text(0.5, 0.5, "Biplot requires at least 2 principal components.")
            }
          } else {
            plot.new()
            text(0.5, 0.5, "Plots not available due to model error.")
          }
          dev.off()
        }
      )
      
      # Download Handler for Data with PCA Scores
      output$dl_pca_data <- downloadHandler(
        filename = function() paste0("pca_data_", Sys.Date(), ".csv"),
        content = function(file) {
          req(df_with_scores())
          write.csv(df_with_scores(), file, row.names = FALSE)
        }
      )
      
      
      # Save model
      if (inherits(saved_models, "reactivevalues") && !inherits(model, "error")) {
        saved_models$pca <- model
        tryCatch({
          saveRDS(model, file.path(models_dir, "pca.rds"))
        }, error = function(e) {
          warning(paste("Failed to save model pca.rds:", e$message))
        })
      }
    })
    
    ## PLS (Partial Least Squares Regression)
    observeEvent(input$open_pls, {
      showModal(modalDialog(
        title = "PLS",
        selectInput(ns("pls_dep"), "Dependent Variable", choices = names(df())), # Should be numeric
        selectInput(ns("pls_indep"), "Independent Variables", choices = names(df()), multiple = TRUE), # Should be numeric
        numericInput(ns("pls_ncomp"), "Maximum Number of Components", value = 5, min = 1),
        footer = tagList(actionButton(ns("run_pls"), "Run"), modalButton("Close")),
        size = "m"
      ))
    })
    observeEvent(input$run_pls, {
      req(df(), input$pls_dep, input$pls_indep, input$pls_ncomp)
      
      # Check if selected columns are numeric
      if (!is.numeric(df()[[input$pls_dep]])) {
        showModal(modalDialog(title = "Input Error", "Dependent variable for PLS should be numeric.", easyClose = TRUE))
        return()
      }
      if (!check_numeric_vars(df(), input$pls_indep)) {
        showModal(modalDialog(title = "Input Error", "Independent variables for PLS should be numeric.", easyClose = TRUE))
        return()
      }
      if (length(input$pls_indep) == 0) {
        showModal(modalDialog(title = "Input Error", "Please select independent variables for PLS.", easyClose = TRUE))
        return()
      }
      
      # Adjust ncomp if greater than number of independent variables
      n_indep_vars <- length(input$pls_indep)
      ncomp_val <- min(input$pls_ncomp, n_indep_vars)
      if (ncomp_val < input$pls_ncomp) {
        warning("Number of components adjusted to ", ncomp_val, " as it cannot exceed the number of independent variables.")
      }
      if (ncomp_val == 0) {
        showModal(modalDialog(title = "Input Error", "Number of components must be at least 1.", easyClose = TRUE))
        return()
      }
      
      
      f <- as.formula(paste(input$pls_dep, "~", paste(input$pls_indep, collapse = "+")))
      # Added validation = "CV" and ncomp
      model <- tryCatch(pls::plsr(f, data = df(), validation = "CV", ncomp = ncomp_val), error = identity) # Use pls::
      
      output$pls_out <- renderPrint({
        if (inherits(model, "error")) model$message else summary(model)
      })
      
      # Render plot (RMSEP plot from cross-validation)
      output$pls_plot <- renderPlot({
        if (!inherits(model, "error")) {
          # Check if cross-validation was performed
          if (!is.null(model$validation)) {
            plot(pls::RMSEP(model), main = "RMSEP from Cross-Validation")
            # Optional: add a line for optimal number of components if identified
            # optim_ncomp <- selectNcomp(model, method = "onesigma") # or "auto"
            # abline(v = optim_ncomp, col = "red", lty = 2)
          } else {
            plot.new()
            text(0.5, 0.5, "Cross-validation results not available for plotting RMSEP.")
          }
        }
      })
      
      # Generate data with predictions
      df_with_predictions <- reactive({
        req(df(), !inherits(model, "error"))
        tryCatch({
          data <- df()
          # Predict using the optimal or selected number of components
          # Using the maximum fitted components for simplicity
          data$Predicted <- predict(model, newdata = data, ncomp = model$ncomp)
          # The predict output is a matrix, needs conversion if only one dependent variable
          if (is.matrix(data$Predicted) && ncol(data$Predicted) == 1) {
            data$Predicted <- as.numeric(data$Predicted)
          } else if (!is.numeric(data$Predicted)) {
            # Handle multivariate PLS prediction output if necessary
          }
          data
        }, error = function(e) {
          warning("Failed to generate predictions:", e$message)
          df() # Return original data if prediction fails
        })
      })
      
      
      showModal(modalDialog(
        title = "PLS Results",
        tagList(
          h4("Model Summary"),
          verbatimTextOutput(ns("pls_out")),
          h4("RMSEP Plot (Cross-Validation)"),
          plotOutput(ns("pls_plot")),
          br(),
          downloadButton(ns("dl_pls_pdf"), "Download Report (PDF)"),
          downloadButton(ns("dl_pls_data"), "Download Data with Predictions (CSV)") # New download button
        ),
        size = "l",
        easyClose = TRUE
      ))
      
      # Download Handler for PDF
      output$dl_pls_pdf <- downloadHandler(
        filename = function() paste0("pls_report_", Sys.Date(), ".pdf"),
        content = function(file) {
          pdf(file, width = 8, height = 10)
          if (!inherits(model, "error")) print(summary(model)) else cat("Error:", model$message)
          
          if (!inherits(model, "error") && !is.null(model$validation)) {
            plot(pls::RMSEP(model), main = "RMSEP from Cross-Validation")
          } else {
            plot.new()
            text(0.5, 0.5, "RMSEP plot not available.")
          }
          dev.off()
        }
      )
      
      # Download Handler for Data with Predictions
      output$dl_pls_data <- downloadHandler(
        filename = function() paste0("pls_data_", Sys.Date(), ".csv"),
        content = function(file) {
          req(df_with_predictions())
          write.csv(df_with_predictions(), file, row.names = FALSE)
        }
      )
      
      # Save model
      if (inherits(saved_models, "reactivevalues") && !inherits(model, "error")) {
        saved_models$pls <- model
        tryCatch({
          saveRDS(model, file.path(models_dir, "pls.rds"))
        }, error = function(e) {
          warning(paste("Failed to save model pls.rds:", e$message))
        })
      }
    })
    
    
    ## Custom Code Execution
    observeEvent(input$run_custom, {
      req(df(), input$custom_code) # Require data and code to run
      code_to_run <- input$custom_code
      
      # Create a new environment and make df and potentially saved_models available
      env <- new.env(parent = globalenv())
      env$df <- df() # Make the dataset available as 'df'
      
      # Make saved models available as a list in the custom environment
      if (inherits(saved_models, "reactivevalues")) {
        tryCatch({
          saved_models_list <- reactiveValuesToList(saved_models)
          list2env(saved_models_list, envir = env)
          # Provide a message in the output that saved_models are available
          message("Saved models are available in the execution environment.")
        }, error = function(e) {
          warning("Failed to expose saved_models to custom code environment:", e$message)
          message("Could not make saved_models available in the execution environment.")
        })
      }
      
      
      # Capture both output, messages, warnings, and errors
      capture <- evaluate::evaluate(code_to_run, envir = env)
      
      output$custom_out <- renderPrint({
        # Print the captured output, including messages, warnings, errors, and results
        evaluate::replay(capture)
      })
      
      # No standard plot output for arbitrary custom code unless the code explicitly generates plots.
      # If the code *does* generate a plot, evaluate::evaluate captures it.
      # Need to figure out how to display captured plots in a Shiny renderPlot.
      # This is non-trivial for arbitrary code. A common approach is to save plots to temp files.
      
      # For now, let's rely on evaluate::replay to *attempt* to show captured plots inline if the output type allows (e.g., knitr chunk output)
      # A more robust solution for displaying arbitrary plots requires more complex handling of the evaluate results.
      output$custom_plot <- renderPlot({
        # evaluate::replay captures plots, but rendering them requires inspecting the output list
        # This is complex for a generic renderPlot. Let's skip showing arbitrary plots inline for now.
        # We will add plot capturing to the PDF download.
        plot.new()
        text(0.5, 0.5, "Inline plotting for arbitrary custom code is not supported. Check PDF for plots.")
      })
      
      
      showModal(modalDialog(
        title = "Custom Code Results",
        tagList(
          h4("Execution Output"),
          verbatimTextOutput(ns("custom_out")),
          # plotOutput(ns("custom_plot")), # Hide inline plot output for now
          br(),
          downloadButton(ns("dl_custom_txt"), "Download Output (Text)"), # Changed to .txt
          downloadButton(ns("dl_custom_pdf"), "Download Output (PDF) - Includes Plots") # New PDF download for custom code
        ),
        size = "l",
        easyClose = TRUE
      ))
      
      # Download Handler for Text Output
      output$dl_custom_txt <- downloadHandler(
        filename = function() paste0("custom_output_", Sys.Date(), ".txt"),
        content = function(file) {
          # evaluate::replay captures print/cat output
          text_output <- paste(sapply(capture, function(x) {
            if ("src" %in% class(x)) return(paste0(">", x$src)) # Show source lines
            if ("error" %in% class(x)) return(paste("Error:", x$message))
            if ("warning" %in% class(x)) return(paste("Warning:", x$message))
            if ("message" %in% class(x)) return(paste("Message:", x$message))
            if ("source" %in% class(x)) return(x$src) # Literal source
            if (!is.null(x$src) && !is.null(x$value)) return(paste0(">", x$src, "\n", paste(capture.output(print(x$value)), collapse = "\n"))) # Output + source
            if (!is.null(x$value)) return(paste(capture.output(print(x$value)), collapse = "\n")) # Only output value
            return("") # Other types (plots handled in PDF)
          }), collapse = "\n")
          writeLines(text_output, con = file)
        }
      )
      
      # Download Handler for PDF Output (including plots)
      output$dl_custom_pdf <- downloadHandler(
        filename = function() paste0("custom_output_", Sys.Date(), ".pdf"),
        content = function(file) {
          pdf(file, width = 8, height = 10)
          # Use evaluate::replay to send captured output (text and plots) to the PDF device
          tryCatch({
            evaluate::replay(capture)
          }, error = function(e) {
            plot.new()
            text(0.5, 0.5, paste("Error rendering custom output to PDF:", e$message))
          })
          dev.off()
        }
      )
      
      # No saving for arbitrary custom code results currently
    })
    
    
    # --- Return values from the module if needed by the parent app ---
    # For example, if the parent app needs access to the saved models reactiveValues:
    # return(list(saved_models = saved_models))
  })
}
