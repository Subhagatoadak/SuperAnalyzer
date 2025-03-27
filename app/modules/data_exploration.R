library(shiny)
library(rpivotTable)
library(moments)
library(bs4Dash)   # For accordion components
library(ggplot2)   # For plotting

# UI function for data exploration
dataExplorationUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Pivot Table"),
    # Fixed container to prevent overflowing
    div(style = "max-height:600px; overflow-y:auto;", 
        rpivotTableOutput(ns("pivotTable"))
    ),
    # Accordion with two items: one for correlation/moments and one for distribution plots
    bs4Accordion(
      id = ns("accordion1"),
      bs4AccordionItem(
        id = ns("corr_moments"),
        title = "Correlation Matrix and Moments",
        collapsed = TRUE,
        h4("Correlation Matrix"),
        tableOutput(ns("correlationTable")),
        h4("Moments (Mean, Variance, Skewness, Kurtosis)"),
        tableOutput(ns("statsTable"))
      ),
      bs4AccordionItem(
        id = ns("dist_plots"),
        title = "Distribution Plots",
        collapsed = TRUE,
        h4("Continuous Variable Plots"),
        selectInput(ns("cont_var"), "Select Continuous Variable", choices = NULL),
        fluidRow(
          column(4, plotOutput(ns("densityPlot"))),
          column(4, plotOutput(ns("violinPlot"))),
          column(4, plotOutput(ns("boxPlot")))
        ),
        hr(),
        h4("Discrete Variable Plot"),
        selectInput(ns("disc_var"), "Select Discrete Variable", choices = NULL),
        fluidRow(
          column(12, plotOutput(ns("barPlot")))
        )
      )
    )
  )
}

# Server function for data exploration
# 'varTypes' is an additional reactive expression returning a named vector of variable types.
dataExplorationServer <- function(id, dataset, varTypes = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Render the pivot table
      output$pivotTable <- renderRpivotTable({
        df <- dataset()
        req(df)
        rpivotTable(df)
      })
      
      # Render the correlation matrix
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
      
      # Render the moments table (mean, variance, skewness, kurtosis)
      output$statsTable <- renderTable({
        df <- dataset()
        req(df)
        numeric_cols <- sapply(df, is.numeric)
        if (any(numeric_cols)) {
          stats <- data.frame(
            Variable = names(df)[numeric_cols],
            Mean = sapply(df[, numeric_cols, drop = FALSE], function(x) round(mean(x, na.rm = TRUE), 2)),
            Variance = sapply(df[, numeric_cols, drop = FALSE], function(x) round(var(x, na.rm = TRUE), 2)),
            Skewness = sapply(df[, numeric_cols, drop = FALSE], function(x) round(skewness(x, na.rm = TRUE), 2)),
            Kurtosis = sapply(df[, numeric_cols, drop = FALSE], function(x) round(kurtosis(x, na.rm = TRUE), 2))
          )
          stats
        } else {
          data.frame(Message = "No numeric columns available for statistics")
        }
      })
      
      # Update continuous and discrete variable select inputs
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
      
      # Density plot for selected continuous variable
      output$densityPlot <- renderPlot({
        df <- dataset()
        req(df, input$cont_var)
        ggplot(df, aes_string(x = input$cont_var)) +
          geom_density(fill = "lightblue", alpha = 0.5) +
          labs(title = paste("Density Plot of", input$cont_var))
      })
      
      # Violin plot for selected continuous variable
      output$violinPlot <- renderPlot({
        df <- dataset()
        req(df, input$cont_var)
        df$dummy <- ""
        ggplot(df, aes_string(x = "dummy", y = input$cont_var)) +
          geom_violin(fill = "lightgreen", color = "black") +
          labs(title = paste("Violin Plot of", input$cont_var), x = "") +
          theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      })
      
      # Box plot for selected continuous variable
      output$boxPlot <- renderPlot({
        df <- dataset()
        req(df, input$cont_var)
        df$dummy <- ""
        ggplot(df, aes_string(x = "dummy", y = input$cont_var)) +
          geom_boxplot(fill = "lightcoral", color = "black") +
          labs(title = paste("Box Plot of", input$cont_var), x = "") +
          theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      })
      
      # Bar plot for selected discrete variable
      output$barPlot <- renderPlot({
        df <- dataset()
        req(df, input$disc_var)
        df[[input$disc_var]] <- as.factor(df[[input$disc_var]])
        ggplot(df, aes_string(x = input$disc_var)) +
          geom_bar(fill = "steelblue") +
          labs(title = paste("Bar Plot of", input$disc_var),
               x = input$disc_var, y = "Count") +
          theme_minimal()
      })
    }
  )
}
