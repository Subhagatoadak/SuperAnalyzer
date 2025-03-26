# app/modules/modelling.R
library(shiny)
library(rstan)
library(bayesplot)

# Define the Stan model as a string (simple normal model)
stan_model_code <- "
data {
  int<lower=0> N;
  vector[N] y;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  y ~ normal(mu, sigma);
}
"

# UI function for Bayesian modelling
modellingUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("varSelectUI")),
    actionButton(ns("runModel"), "Run Bayesian Model"),
    plotOutput(ns("modelPlot")),
    verbatimTextOutput(ns("modelSummary"))
  )
}

# Server function for Bayesian modelling
modellingServer <- function(id, dataset) {
  moduleServer(
    id,
    function(input, output, session) {
      # Create variable selection UI if data exists
      output$varSelectUI <- renderUI({
        df <- dataset()
        if (!is.null(df)) {
          numeric_cols <- names(df)[sapply(df, is.numeric)]
          if (length(numeric_cols) > 0) {
            selectInput(session$ns("var"), "Select Numeric Variable for Bayesian Model", choices = numeric_cols)
          } else {
            helpText("No numeric columns available in the uploaded data.")
          }
        }
      })
      
      stan_fit <- reactiveVal(NULL)
      
      observeEvent(input$runModel, {
        df <- dataset()
        if (is.null(df)) {
          showNotification("No data uploaded. Using dummy data.", type = "warning")
          y <- rnorm(100)
        } else {
          req(input$var)
          y <- df[[input$var]]
        }
        
        stan_data <- list(
          N = length(y),
          y = y
        )
        
        # Fit the model using rstan
        fit <- stan(model_code = stan_model_code, 
                    data = stan_data, 
                    iter = 2000, 
                    warmup = 1000, 
                    chains = 4, 
                    refresh = 0)
        stan_fit(fit)
      })
      
      output$modelPlot <- renderPlot({
        fit <- stan_fit()
        req(fit)
        mcmc_trace(as.array(fit), pars = c("mu", "sigma"))
      })
      
      output$modelSummary <- renderPrint({
        fit <- stan_fit()
        req(fit)
        print(fit)
      })
    }
  )
}
