# app/modules/modelling_bayesian.R
library(shiny)
library(rstan)
library(bayesplot)
library(DT)

# UI for Bayesian Regression
bayesianModelUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("dep"), "Select Dependent Variable", choices = NULL),
    selectInput(ns("indep"), "Select Independent Variable(s)", choices = NULL, multiple = TRUE),
    actionButton(ns("run_bayesian"), "Run Bayesian Regression"),
    plotOutput(ns("bayesian_plot")),
    verbatimTextOutput(ns("bayesian_summary")),
    DTOutput(ns("bayesian_table"))
  )
}

# Server for Bayesian Regression
bayesianModelServer <- function(id, dataset) {
  moduleServer(
    id,
    function(input, output, session) {
      stan_model_code <- "
data {
  int<lower=0> N;
  int<lower=0> K;
  matrix[N, K] X;
  vector[N] y;
}
parameters {
  vector[K] beta;
  real<lower=0> sigma;
}
model {
  y ~ normal(X * beta, sigma);
}
"
observe({
  df <- dataset()
  if (!is.null(df)) {
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    updateSelectInput(session, "dep", choices = numeric_vars)
    updateSelectInput(session, "indep", choices = numeric_vars)
  }
})

bayesian_fit <- eventReactive(input$run_bayesian, {
  df <- dataset()
  req(df, input$dep, input$indep)
  X <- as.matrix(df[, input$indep, drop = FALSE])
  y <- df[[input$dep]]
  stan_data <- list(
    N = nrow(X),
    K = ncol(X),
    X = X,
    y = y
  )
  stan(model_code = stan_model_code,
       data = stan_data,
       iter = 2000,
       warmup = 1000,
       chains = 4,
       refresh = 0)
})

output$bayesian_plot <- renderPlot({
  fit <- bayesian_fit()
  req(fit)
  mcmc_trace(as.array(fit), pars = "beta")
})

output$bayesian_summary <- renderPrint({
  fit <- bayesian_fit()
  req(fit)
  print(fit)
})

output$bayesian_table <- renderDT({
  fit <- bayesian_fit()
  req(fit)
  summary_fit <- summary(fit)$summary
  datatable(as.data.frame(summary_fit))
})
    }
  )
}
