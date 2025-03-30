# openai_chat.R
library(shiny)
library(bs4Dash)
library(httr)
library(jsonlite)

# UI for the OpenAI Chat module
openaiChatUI <- function(id) {
  ns <- NS(id)
  bs4Card(
    title = "OpenAI Chat",
    status = "primary",
    width = 12,
    solidHeader = TRUE,
    selectInput(ns("openai_model"), "Select Model", 
                choices = c("gpt-4o", "o3-mini-2025-01-31", "gpt-3.5-turbo-16k", "gpt-4o-mini", "chatgpt-4o-latest"),
                selected = "gpt-4o"),
    textAreaInput(ns("openai_input"), "Enter your question:", "", rows = 4, width = "100%"),
    actionButton(ns("openai_send"), "Send"),
    br(), br(),
    uiOutput(ns("openai_output"))
  )
}

# Server logic for the OpenAI Chat module
openaiChatServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      rv_chat <- reactiveValues(messages = list())
      
      output$openai_output <- renderUI({
        req(rv_chat$messages)
        message_tags <- lapply(rv_chat$messages, function(msg) {
          if (msg$role == "user") {
            tags$div(
              style = "background-color: #DCF8C6; padding: 10px; margin: 5px; border-radius: 10px; text-align: right; max-width:70%; align-self: flex-end;",
              tags$p(msg$text, style = "margin: 0;")
            )
          } else {
            tags$div(
              style = "background-color: #F1F0F0; padding: 10px; margin: 5px; border-radius: 10px; text-align: left; max-width:70%; align-self: flex-start;",
              tags$p(msg$text, style = "margin: 0;")
            )
          }
        })
        tags$div(style = "display: flex; flex-direction: column;", message_tags)
      })
      
      observeEvent(input$openai_send, {
        req(input$openai_input, input$openai_model)
        query <- input$openai_input
        model_choice <- input$openai_model
        showNotification("Sending query to OpenAI...", type = "message")
        
        api_key <- Sys.getenv("OPENAI_API_KEY")
        if (api_key == "") {
          showNotification("OPENAI_API_KEY not set", type = "error")
          return(NULL)
        }
        
        rv_chat$messages <- c(rv_chat$messages, list(list(role = "user", text = query)))
        
        res <- tryCatch({
          POST(
            url = "https://api.openai.com/v1/chat/completions",
            add_headers(
              "Authorization" = paste("Bearer", api_key),
              "Content-Type" = "application/json"
            ),
            body = toJSON(list(
              model = model_choice,
              messages = list(list(role = "user", content = query))
            ), auto_unbox = TRUE)
          )
        }, error = function(e) {
          showNotification(paste("POST request failed:", e$message), type = "error")
          return(NULL)
        })
        
        if (is.null(res)) return(NULL)
        if (http_error(res)) {
          showNotification(paste("HTTP error:", status_code(res)), type = "error")
          return(NULL)
        }
        
        response_content <- tryCatch({
          content(res, as = "parsed")
        }, error = function(e) {
          showNotification(paste("Error parsing response:", e$message), type = "error")
          return(NULL)
        })
        
        if (is.null(response_content) || is.null(response_content$choices)) {
          showNotification("No valid response from OpenAI", type = "error")
          return(NULL)
        }
        
        answer <- response_content$choices[[1]]$message$content
        if (is.null(answer)) {
          answer <- "No answer returned."
        }
        
        rv_chat$messages <- c(rv_chat$messages, list(list(role = "assistant", text = answer)))
        showNotification("Response received from OpenAI.", type = "message")
      })
    }
  )
}
