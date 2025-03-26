# app/modules/data_exploration.R
library(shiny)
library(rpivotTable)
library(moments)

# UI function for data exploration
dataExplorationUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Correlation Matrix"),
    tableOutput(ns("correlationTable")),
    h4("Skewness and Kurtosis"),
    tableOutput(ns("statsTable")),
    h4("Pivot Table"),
    rpivotTableOutput(ns("pivotTable"))
  )
}

# Server function for data exploration
dataExplorationServer <- function(id, dataset) {
  moduleServer(
    id,
    function(input, output, session) {
      output$correlationTable <- renderTable({
        df <- dataset()
        req(df)
        numeric_cols <- sapply(df, is.numeric)
        if (sum(numeric_cols) > 1) {
          round(cor(df[, numeric_cols], use = "complete.obs"), 2)
        } else {
          data.frame(Message = "Not enough numeric columns for correlation matrix")
        }
      })
      
      output$statsTable <- renderTable({
        df <- dataset()
        req(df)
        numeric_cols <- sapply(df, is.numeric)
        if (any(numeric_cols)) {
          stats <- data.frame(
            Variable = names(df)[numeric_cols],
            Skewness = sapply(df[, numeric_cols, drop = FALSE], skewness),
            Kurtosis = sapply(df[, numeric_cols, drop = FALSE], kurtosis)
          )
          stats
        } else {
          data.frame(Message = "No numeric columns available for statistics")
        }
      })
      
      output$pivotTable <- renderRpivotTable({
        df <- dataset()
        req(df)
        rpivotTable(df)
      })
    }
  )
}
