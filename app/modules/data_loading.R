# app/modules/data_loading.R
library(shiny)

# UI function for data loading
dataLoaderUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("data"), "Upload CSV Data", accept = ".csv")
  )
}

# Server function for data loading
dataLoaderServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      reactive({
        req(input$data)
        tryCatch({
          read.csv(input$data$datapath)
        }, error = function(e) {
          NULL
        })
      })
    }
  )
}