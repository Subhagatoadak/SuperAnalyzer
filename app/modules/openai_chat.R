# openai_chat.R
library(shiny)
library(bs4Dash)
library(httr)
library(jsonlite)
library(dotenv)

# --- Debug helper: print working directory and .env status
message("[DEBUG] Shiny app working dir: ", getwd())
message("[DEBUG] .env exists: ", file.exists(".env"))
if (file.exists(".env")) {
  message("[DEBUG] .env contents:\n", paste(readLines(".env"), collapse = "\n"))
  # Load .env
  load_dot_env(file = ".env")
  # Fallback manual parse if dotenv didn't set it
  if (identical(Sys.getenv("OPENAI_API_KEY"), "") || is.na(Sys.getenv("OPENAI_API_KEY"))) {
    lines <- readLines(".env")
    key_line <- lines[grepl("^OPENAI_API_KEY=", lines)]
    if (length(key_line) >= 1) {
      # Extract value after = and trim whitespace/newlines
      raw_key <- sub("^OPENAI_API_KEY=", "", key_line[1])
      trimmed_key <- trimws(raw_key, whitespace = "[ \t\r\n]")
      Sys.setenv(OPENAI_API_KEY = trimmed_key)
      message("[DEBUG] Manual parse set OPENAI_API_KEY (first 5 chars): ", substr(trimmed_key,1,5), "... length=", nchar(trimmed_key))
    }
  }
  # Always trim any existing env key
  env_key <- Sys.getenv("OPENAI_API_KEY")
  env_key_trimmed <- trimws(env_key, whitespace = "[ \t\r\n]")
  if (!identical(env_key, env_key_trimmed)) Sys.setenv(OPENAI_API_KEY = env_key_trimmed)
  message("[DEBUG] Final OPENAI_API_KEY (first 5 chars): ", substr(env_key_trimmed,1,5), "... length=", nchar(env_key_trimmed))
} else {
  warning(".env file not found in app root. Make sure your OPENAI_API_KEY is available.")
}

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
        system_prompt <- input$system_prompt %||% "You are a helpful assistant."
        showNotification("Sending query to OpenAI...", type = "message")
        
        # Retrieve and debug API key
        api_key <- Sys.getenv("OPENAI_API_KEY")
        api_key <- trimws(api_key, whitespace = "[ \t\r\n]")
        
        if (identical(api_key, "") || is.na(api_key)) {
          showNotification("OPENAI_API_KEY not set or empty (HTTP 401)", type = "error")
          return(NULL)
        }
        
        rv_chat$messages <- c(rv_chat$messages, list(list(role = "user", text = query)))
        
        # Perform POST request
        res <- tryCatch({
          POST(
            url = "https://api.openai.com/v1/chat/completions",
            add_headers(
              Authorization = paste0("Bearer ", api_key),
              `Content-Type` = "application/json"
            ),
            body = toJSON(
              list(
                model = model_choice,
                messages = list(list(role = "system", content = system_prompt),
                                list(role = "user", content = query))
              ),
              auto_unbox = TRUE
            )
          )
        }, error = function(e) {
          showNotification(paste("POST request failed:", e$message), type = "error")
          return(NULL)
        })
        
        # Check for HTTP errors
        if (is.null(res)) return(NULL)
        if (http_error(res)) {
          code <- status_code(res)
          if (code == 401) {
            showNotification("Unauthorized (401): Key appears invalid. Did you rotate or revoke it?", type = "error")
          } else {
            showNotification(paste("HTTP error:", code), type = "error")
          }
          return(NULL)
        }
        
        # Parse and display response
        response_content <- tryCatch({
          content(res, as = "parsed")
        }, error = function(e) {
          showNotification(paste("Error parsing response:", e$message), type = "error")
          return(NULL)
        })
        
        answer <- response_content$choices[[1]]$message$content %||% "No answer returned."
        rv_chat$messages <- c(rv_chat$messages, list(list(role = "assistant", text = answer)))
        showNotification("Response received from OpenAI.", type = "message")
      })
    }
  )
}
