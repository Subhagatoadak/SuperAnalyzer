library(shiny)
library(rpivotTable)
library(moments)
library(bs4Dash)   # For accordion components
library(plotly)    # For interactive plotting

# A helper function to compute mode
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# UI function for data exploration
dataExplorationUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Accordion for Missing Value Analysis
    bs4Accordion(
      id = ns("missing_accordion"),
      bs4AccordionItem(
        id = ns("missing_values"),
        title = "Missing Value Analysis",
        collapsed = TRUE,
        tableOutput(ns("missingTable")),
        fluidRow(
          column(4, actionButton(ns("impute_mean"), "Impute Using Mean")),
          column(4, actionButton(ns("impute_median"), "Impute Using Median")),
          column(4, actionButton(ns("impute_custom_btn"), "Custom Impute Code"))
        )
      )
    ),
    h4("Pivot Table"),
    # Fixed container for pivot table
    div(style = "max-height:600px; overflow-y:auto;", 
        rpivotTableOutput(ns("pivotTable"))
    ),
    # Accordion for Correlation/Moments & Distribution Plots
    bs4Accordion(
      id = ns("accordion1"),
      bs4AccordionItem(
        id = ns("corr_moments"),
        title = "Correlation Matrix and Moments",
        collapsed = TRUE,
        h4("Correlation Matrix"),
        tableOutput(ns("correlationTable")),
        h4("Moments (Mean, Median, Mode, Variance, Skewness, Kurtosis)"),
        tableOutput(ns("statsTable"))
      ),
      bs4AccordionItem(
        id = ns("dist_plots"),
        title = "Distribution Plots",
        collapsed = TRUE,
        h4("Continuous Variable Plots"),
        selectInput(ns("cont_var"), "Select Continuous Variable", choices = NULL),
        fluidRow(
          column(4, plotlyOutput(ns("densityPlot"))),
          column(4, plotlyOutput(ns("violinPlot"))),
          column(4, plotlyOutput(ns("boxPlot")))
        ),
        hr(),
        h4("Discrete Variable Plot"),
        selectInput(ns("disc_var"), "Select Discrete Variable", choices = NULL),
        fluidRow(
          column(12, plotlyOutput(ns("barPlot")))
        ),
        hr(),
        h4("Outlier Analysis"),
        verbatimTextOutput(ns("outlierAnalysis")),
        actionButton(ns("handle_outliers"), "Handle Outliers")
      )
    )
  )
}

# Server function for data exploration
# 'varTypes' is an optional reactive expression returning a named vector of variable types.
dataExplorationServer <- function(id, dataset, varTypes = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ### Missing Value Analysis ###
      output$missingTable <- renderTable({
        df <- dataset()
        req(df)
        missing_percent <- sapply(df, function(x) round(sum(is.na(x)) / length(x) * 100, 2))
        data.frame(Variable = names(missing_percent), Missing_Percentage = missing_percent)
      })
      
      # Impute Using Mean
      observeEvent(input$impute_mean, {
        df <- dataset()
        req(df)
        for(col in names(df)) {
          if(is.numeric(df[[col]]) && any(is.na(df[[col]]))) {
            df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
          }
        }
        dataset(df)
        showNotification("Missing values imputed using mean.", type = "message")
      })
      
      # Impute Using Median
      observeEvent(input$impute_median, {
        df <- dataset()
        req(df)
        for(col in names(df)) {
          if(is.numeric(df[[col]]) && any(is.na(df[[col]]))) {
            df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
          }
        }
        dataset(df)
        showNotification("Missing values imputed using median.", type = "message")
      })
      
      # Custom Imputation: Open Modal
      observeEvent(input$impute_custom_btn, {
        req(dataset())
        df <- dataset()
        schema_text <- paste(capture.output(str(df)), collapse = "\n")
        showModal(modalDialog(
          title = "Custom Missing Value Imputation",
          tagList(
            h4("Dataset Schema:"),
            pre(schema_text),
            p("Note: Your dataset is stored as 'df'. Write R code to impute missing values."),
            textAreaInput(session$ns("impute_code"), "Enter Imputation Code", value = "", rows = 5, width = "100%")
          ),
          footer = tagList(
            actionButton(session$ns("run_impute"), "Run Imputation"),
            modalButton("Close")
          ),
          size = "l"
        ))
      })
      
      observeEvent(input$run_impute, {
        req(dataset())
        df <- dataset()
        tryCatch({
          df_imputed <- eval(parse(text = input$impute_code), envir = list(df = df))
          if (!is.null(df_imputed)) {
            dataset(df_imputed)
            showNotification("Custom imputation applied.", type = "message")
            removeModal()
          }
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
        })
      })
      
      ### Pivot Table ###
      output$pivotTable <- renderRpivotTable({
        df <- dataset()
        req(df)
        rpivotTable(df)
      })
      
      ### Correlation Matrix (only continuous variables) ###
      output$correlationTable <- renderTable({
        df <- dataset()
        req(df)
        if (!is.null(varTypes)) {
          vt <- varTypes()
          cont_vars <- names(vt)[vt == "Continuous"]
        } else {
          cont_vars <- names(df)[sapply(df, is.numeric)]
        }
        if (length(cont_vars) > 1) {
          round(cor(df[, cont_vars, drop = FALSE], use = "complete.obs"), 2)
        } else {
          data.frame(Message = "Not enough continuous variables for correlation matrix")
        }
      })
      
      ### Moments Table (only for continuous variables) ###
      output$statsTable <- renderTable({
        df <- dataset()
        req(df)
        if (!is.null(varTypes)) {
          vt <- varTypes()
          cont_vars <- names(vt)[vt == "Continuous"]
        } else {
          cont_vars <- names(df)[sapply(df, is.numeric)]
        }
        if (length(cont_vars) > 0) {
          stats <- data.frame(
            Variable = cont_vars,
            Mean = sapply(df[, cont_vars, drop = FALSE], function(x) round(mean(x, na.rm = TRUE), 2)),
            Median = sapply(df[, cont_vars, drop = FALSE], function(x) round(median(x, na.rm = TRUE), 2)),
            Mode = sapply(df[, cont_vars, drop = FALSE], function(x) get_mode(x)),
            Variance = sapply(df[, cont_vars, drop = FALSE], function(x) round(var(x, na.rm = TRUE), 2)),
            Skewness = sapply(df[, cont_vars, drop = FALSE], function(x) round(skewness(x, na.rm = TRUE), 2)),
            Kurtosis = sapply(df[, cont_vars, drop = FALSE], function(x) round(kurtosis(x, na.rm = TRUE), 2))
          )
          stats
        } else {
          data.frame(Message = "No continuous variables available for statistics")
        }
      })
      
      ### Update continuous and discrete variable select inputs ###
      observe({
        df <- dataset()
        req(df)
        if (!is.null(varTypes)) {
          vt <- varTypes()
          continuous_vars <- names(vt)[vt == "Continuous"]
          discrete_vars <- names(vt)[vt == "Discrete"]
        } else {
          continuous_vars <- names(df)[sapply(df, is.numeric)]
          discrete_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
        }
        updateSelectInput(session, "cont_var", choices = continuous_vars)
        updateSelectInput(session, "disc_var", choices = discrete_vars)
      })
      
      ### Continuous Variable Plots using plotly ###
      
      # Density plot using plotly
      output$densityPlot <- renderPlotly({
        df <- dataset()
        req(df, input$cont_var)
        x <- df[[input$cont_var]]
        dens <- density(x, na.rm = TRUE)
        plot_ly(x = dens$x, y = dens$y, type = 'scatter', mode = 'lines') %>%
          layout(title = paste("Density Plot of", input$cont_var),
                 xaxis = list(title = input$cont_var),
                 yaxis = list(title = "Density"))
      })
      
      # Violin plot using plotly
      output$violinPlot <- renderPlotly({
        df <- dataset()
        req(df, input$cont_var)
        plot_ly(df, y = ~get(input$cont_var), type = 'violin',
                box = list(visible = TRUE),
                meanline = list(visible = TRUE)) %>%
          layout(title = paste("Violin Plot of", input$cont_var),
                 yaxis = list(title = input$cont_var))
      })
      
      # Box plot using plotly
      output$boxPlot <- renderPlotly({
        df <- dataset()
        req(df, input$cont_var)
        plot_ly(df, y = ~get(input$cont_var), type = 'box') %>%
          layout(title = paste("Box Plot of", input$cont_var),
                 yaxis = list(title = input$cont_var))
      })
      
      ### Outlier Analysis for selected continuous variable ###
      output$outlierAnalysis <- renderPrint({
        df <- dataset()
        req(df, input$cont_var)
        x <- df[[input$cont_var]]
        x <- x[!is.na(x)]
        Q1 <- quantile(x, 0.25)
        Q3 <- quantile(x, 0.75)
        IQR_val <- IQR(x)
        lower_bound <- Q1 - 1.5 * IQR_val
        upper_bound <- Q3 + 1.5 * IQR_val
        outliers <- x[x < lower_bound | x > upper_bound]
        cat("Outlier Analysis for", input$cont_var, "\n")
        cat("Lower Bound:", lower_bound, "\n")
        cat("Upper Bound:", upper_bound, "\n")
        cat("Number of Outliers:", length(outliers), "\n")
        if(length(outliers) > 0) {
          cat("Outlier Values:", paste(round(outliers, 2), collapse = ", "), "\n")
        }
      })
      
      ### Discrete Variable Plot (Bar plot) using plotly ###
      output$barPlot <- renderPlotly({
        df <- dataset()
        req(df, input$disc_var)
        freq <- as.data.frame(table(df[[input$disc_var]]))
        colnames(freq) <- c("Value", "Count")
        plot_ly(freq, x = ~Value, y = ~Count, type = 'bar') %>%
          layout(title = paste("Bar Plot of", input$disc_var),
                 xaxis = list(title = input$disc_var),
                 yaxis = list(title = "Count"))
      })
      
      ### Outlier Handling Modal ###
      observeEvent(input$handle_outliers, {
        req(dataset())
        df <- dataset()
        schema_text <- paste(capture.output(str(df)), collapse = "\n")
        showModal(modalDialog(
          title = "Outlier Handling",
          tagList(
            h4("Dataset Schema:"),
            pre(schema_text),
            p("Note: Your dataset is stored as 'df'. Write R code to handle outliers."),
            textAreaInput(session$ns("outlier_code"), "Enter Outlier Handling Code", value = "", rows = 5, width = "100%")
          ),
          footer = tagList(
            actionButton(session$ns("run_outlier"), "Run Outlier Handling"),
            modalButton("Close")
          ),
          size = "l"
        ))
      })
      
      observeEvent(input$run_outlier, {
        req(dataset())
        df <- dataset()
        tryCatch({
          df_out <- eval(parse(text = input$outlier_code), envir = list(df = df))
          if(!is.null(df_out)) {
            dataset(df_out)
            showNotification("Outlier handling applied.", type = "message")
            removeModal()
          }
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
        })
      })
      
    }
  )
}

